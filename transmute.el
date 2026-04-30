;;; transmute.el --- Media management utilities  -*- lexical-binding: t; -*-

;; Author: James Dyer <james@dyerdwelling.family>
;; Keywords: media, image, video, automation
;; Package-Requires: ((emacs "27.1") (cl-lib "0.5") (transient "0.3.0"))
;; Version: 0.1.0

;;; Commentary:
;; This package provides Emacs Lisp implementations for media processing
;; (Images, Video, Audio). It relies on external tools: exiftool,
;; imagemagick (magick), ffmpeg, sox, tesseract, and realesrgan-ncnn-vulkan.

;;; Code:

(require 'cl-lib)
(require 'dired)
(require 'transient)

(defgroup transmute nil
  "Media management and conversion utilities."
  :group 'media)

(defcustom transmute-trash-command "trash-put"
  "Command to use for trashing files."
  :type 'string
  :group 'transmute)

;;; Core Variables & Parsing

(defun transmute--parse-filename (file)
  "Parse media filename YYYYMMDDHHMMSS--description__tag1@tag2.ext.
Returns an alist with keys: directory, filename, extension, no-ext,
no-tag, timestamp, label, tags-raw, tags, keywords."
  (let* ((file (expand-file-name file))
         (directory (file-name-directory file))
         (filename (file-name-nondirectory file))
         (extension (file-name-extension filename))
         (no-ext (file-name-sans-extension filename))
         ;; Split by __ to get description and tags
         (parts (split-string no-ext "__"))
         (desc-part (car parts))
         (tags-raw (cadr parts))
         ;; Split description by -- to get timestamp and label
         (desc-subparts (split-string desc-part "--"))
         (has-timestamp (and (> (length desc-subparts) 1)
                            (string-match-p "^[0-9]\\{14\\}$" (car desc-subparts))))
         (timestamp (if has-timestamp (car desc-subparts) ""))
         (label (if has-timestamp (cadr desc-subparts) (car desc-subparts)))
         (no-tag (if has-timestamp 
                     (format "%s--%s.%s" timestamp label extension)
                   (format "%s.%s" label extension)))
         ;; Parse tags
         (tags (when tags-raw
                 (replace-regexp-in-string "[-_]" " " (replace-regexp-in-string "@" " " tags-raw))))
         (keywords (when tags
                     (delete-dups (sort (split-string tags " " t) #'string<)))))
    `((directory . ,directory)
      (filename . ,filename)
      (extension . ,extension)
      (no-ext . ,no-ext)
      (no-tag . ,no-tag)
      (timestamp . ,timestamp)
      (label . ,label)
      (tags-raw . ,tags-raw)
      (tags . ,tags)
      (keywords . ,keywords))))

(defun transmute-get-date (file)
  "Get creation date from FILE using exiftool.
Tries CreateDate, DateTimeOriginal, ModifyDate, and FileModifyDate."
  (let ((props '("CreateDate" "DateTimeOriginal" "ModifyDate" "FileModifyDate"))
        (result nil))
    (cl-loop for prop in props
             until result
             do (let ((val (shell-command-to-string
                            (format "exiftool -s3 -%s %s"
                                    prop (shell-quote-argument (expand-file-name file))))))
                  (setq val (string-trim val))
                  (unless (string-empty-p val)
                    (setq result (list prop val)))))
    result))

(defun transmute-format-date (date-string)
  "Format exiftool date (YYYY:MM:DD HH:MM:SS) to YYYYMMDDHHMMSS."
  (let ((cleaned (replace-regexp-in-string "[: ]" "" date-string)))
    (if (string-match "^\\([0-9]\\{14\\}\\)" cleaned)
        (match-string 1 cleaned)
      cleaned)))

;;; Internal Process Wrappers

(defun transmute--run-command (cmd &rest args)
  "Run CMD with ARGS and return exit code."
  (let ((full-cmd (mapconcat #'shell-quote-argument (cons cmd args) " ")))
    (message "Running: %s" full-cmd)
    (shell-command full-cmd)))

(defun transmute--preserve-metadata (src dst)
  "Copy metadata from SRC to DST and preserve timestamp."
  (transmute--run-command "exiftool" "-overwrite_original_in_place" "-TagsFromFile" src dst)
  (set-file-times dst (file-attribute-modification-time (file-attributes src))))

(defun transmute--trash (file)
  "Move FILE to trash."
  (if (executable-find transmute-trash-command)
      (transmute--run-command transmute-trash-command file)
    (move-file-to-trash file)))

;;; High-level Conversion Helpers

(defun transmute-convert-image (src dst &rest magick-args)
  "Convert image SRC to DST using MAGICK-ARGS.
Preserves metadata and moves SRC to trash."
  (let ((tmp (make-temp-file "transmute-" nil (concat "." (file-name-extension dst)))))
    (unwind-protect
        (progn
          (apply #'transmute--run-command "magick" src (append magick-args (list tmp)))
          (transmute--preserve-metadata src tmp)
          (copy-file tmp dst t t t t)
          (unless (string= (expand-file-name src) (expand-file-name dst))
            (transmute--trash src)))
      (when (file-exists-p tmp) (delete-file tmp)))))

(defun transmute-convert-image-copy (src dst &rest magick-args)
  "Convert image SRC to DST using MAGICK-ARGS.
Preserves metadata, keeps SRC."
  (let ((tmp (make-temp-file "transmute-" nil (concat "." (file-name-extension dst)))))
    (unwind-protect
        (progn
          (apply #'transmute--run-command "magick" src (append magick-args (list tmp)))
          (transmute--preserve-metadata src tmp)
          (copy-file tmp dst t t t t))
      (when (file-exists-p tmp) (delete-file tmp)))))

(defun transmute-convert-video (src dst &rest ffmpeg-args)
  "Convert video SRC to DST using FFMPEG-ARGS.
Preserves metadata."
  (let ((tmp (make-temp-file "transmute-" nil (concat "." (file-name-extension dst)))))
    (unwind-protect
        (progn
          (let ((cmd-args (append (list "-hide_banner" "-loglevel" "warning" "-stats" "-y" "-i" src "-map_metadata" "0" "-threads" "8")
                                  ffmpeg-args
                                  (list tmp))))
            (apply #'transmute--run-command "ffmpeg" cmd-args))
          (set-file-times tmp (file-attribute-modification-time (file-attributes src)))
          (copy-file tmp dst t t t t))
      (when (file-exists-p tmp) (delete-file tmp)))))

(defun transmute-convert-gan (src dst &rest gan-args)
  "Upscale image SRC to DST using realesrgan-ncnn-vulkan with GAN-ARGS.
Preserves metadata and moves SRC to trash."
  (let ((tmp (make-temp-file "transmute-" nil (concat "." (file-name-extension dst)))))
    (unwind-protect
        (progn
          (let ((cmd-args (append gan-args (list "-i" src "-o" tmp))))
            (apply #'transmute--run-command "realesrgan-ncnn-vulkan" cmd-args))
          (transmute--preserve-metadata src tmp)
          (copy-file tmp dst t t t t)
          (unless (string= (expand-file-name src) (expand-file-name dst))
            (transmute--trash src)))
      (when (file-exists-p tmp) (delete-file tmp)))))

;;; Batch / Dired Integration

(defun transmute-get-targets ()
  "Get list of files to process. 
If in dired, use marked files or file at point. Otherwise ask for file."
  (if (derived-mode-p 'dired-mode)
      (dired-get-marked-files)
    (list (read-file-name "Process file: "))))

(defmacro transmute-do-batch (files &rest body)
  "Run BODY for each file in FILES, binding 'file' to current file."
  (declare (indent 1))
  `(dolist (file ,files)
     (let ((default-directory (file-name-directory file)))
       ,@body)))

;;; Specific Commands

;;;###autoload
(defun transmute-picture-convert ()
  "Convert images to JPG."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".jpg")))
      (if (string-suffix-p ".svg" file t)
          (transmute-convert-image-copy file dst "-density" "300" "-auto-orient" "-strip")
        (transmute-convert-image file dst "-auto-orient" "-strip")))))

;;;###autoload
(defun transmute-picture-crush ()
  "Resize images to 640px."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute-convert-image file file "-auto-orient" "-strip" "-quality" "50%" "-resize" "640x>" "-resize" "x640>")))

;;;###autoload
(defun transmute-picture-scale ()
  "Resize images to 1920px."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute-convert-image file file "-auto-orient" "-strip" "-quality" "50%" "-resize" "1920x>" "-resize" "x1920>")))

;;;###autoload
(defun transmute-picture-rotate-right ()
  "Rotate images 90 degrees clockwise."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute-convert-image file file "-rotate" "90")))

;;;###autoload
(defun transmute-picture-rotate-left ()
  "Rotate images 90 degrees counter-clockwise."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute-convert-image file file "-rotate" "-90")))

;;;###autoload
(defun transmute-picture-correct ()
  "Brighten images (120% modulate)."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute-convert-image file file "-modulate" "120,100,100")))

;;;###autoload
(defun transmute-video-convert ()
  "Convert videos to MP4 (h264/aac)."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".mp4")))
      (transmute-convert-video file dst "-c:a" "aac" "-c:v" "libx264" "-crf" "23"))))

;;;###autoload
(defun transmute-video-shrink ()
  "Resize videos to fit 960x960."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".mp4")))
      (transmute-convert-video file dst 
                                 "-vf" "scale='min(960,iw)':'min(960,ih)':force_original_aspect_ratio=decrease"
                                 "-vcodec" "libx264" "-crf" "28" "-preset" "medium" "-movflags" "+faststart"))))

;;;###autoload
(defun transmute-video-extract-audio ()
  "Extract audio from videos as WAV."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (timestamp (format-time-string "%Y%m%d%H%M%S"))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-" timestamp ".wav")))
      (transmute-convert-video file dst "-vn" "-q:a" "0" "-map" "a"))))

;;;###autoload
(defun transmute-video-toptail (trim-start trim-end)
  "Trim video from start and end."
  (interactive "nTrim from start (sec): \nnTrim from end (sec): ")
  (transmute-do-batch (transmute-get-targets)
    (let* ((duration-str (shell-command-to-string
                          (format "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 %s"
                                  (shell-quote-argument file))))
           (duration (floor (string-to-number duration-str)))
           (end-time (- duration trim-end))
           (parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-trimmed." (cdr (assoc 'extension parsed)))))
      (if (<= end-time trim-start)
          (message "Error: Trim amount exceeds duration for %s" file)
        (transmute-convert-video file dst "-ss" (number-to-string trim-start) "-t" (number-to-string end-time) "-c" "copy")))))

;;;###autoload
(defun transmute-picture-upscale ()
  "Upscale images using realesrgan-x4plus."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".jpg")))
      (transmute-convert-gan file dst "-n" "realesrgan-x4plus" "-j" "8:8:8" "-f" "jpg"))))

;;;###autoload
(defun transmute-picture-get-text ()
  "Extract text from images using tesseract OCR."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (out-base (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)))))
      (transmute--run-command "tesseract" "-l" "eng" file out-base))))

;;;###autoload
(defun transmute-picture-autocolour ()
  "Auto-level image colors."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute-convert-image file file "-auto-level")))

;;;###autoload
(defun transmute-video-remove-audio ()
  "Remove audio from videos."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute-convert-video file file "-an" "-c:v" "copy")))

;;;###autoload
(defun transmute-video-reverse ()
  "Reverse video and audio."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-reversed." (cdr (assoc 'extension parsed)))))
      (transmute-convert-video file dst "-vf" "reverse" "-af" "areverse"))))

;;;###autoload
(defun transmute-video-rotate-right ()
  "Rotate video 90 degrees clockwise."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute-convert-video file file "-vf" "transpose=1")))

;;;###autoload
(defun transmute-video-rotate-left ()
  "Rotate video 90 degrees counter-clockwise."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute-convert-video file file "-vf" "transpose=2")))

;;;###autoload
(defun transmute-picture-to-pdf ()
  "Convert images to PDF."
  (interactive)
  (let ((targets (transmute-get-targets)))
    (if (= (length targets) 1)
        (let* ((file (car targets))
               (parsed (transmute--parse-filename file))
               (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".pdf")))
          (transmute--run-command "magick" file dst))
      (let ((dst (read-file-name "Output PDF: ")))
        (apply #'transmute--run-command "magick" (append targets (list dst)))))))

;;;###autoload
(defun transmute-tag-interactive (tags)
  "Interactively tag selected media files.
TAGS is a comma-separated string or list of tags."
  (interactive (list (completing-read-multiple "Tags (comma separated): " nil)))
  (let* ((tag-list (if (stringp tags) (split-string tags "," t) tags))
         (tag-str (mapconcat #'identity tag-list ","))
         (hier-tag-str (replace-regexp-in-string "@" "/" tag-str))
         (keywords (delete-dups (sort (mapcan (lambda (t) (split-string (replace-regexp-in-string "@" " " t) " " t)) tag-list) #'string<)))
         (keyword-str (mapconcat #'identity keywords ",")))
    (transmute-do-batch (transmute-get-targets)
      (message "Tagging %s with %s" file tag-str)
      (transmute--run-command "exiftool" "-overwrite_original_in_place"
                                (format "-TagsList=%s" hier-tag-str)
                                (format "-XMP-microsoft:LastKeywordXMP=%s" hier-tag-str)
                                (format "-HierarchicalSubject=%s" (replace-regexp-in-string "/" "|" hier-tag-str))
                                (format "-XPKeywords=%s" keyword-str)
                                (format "-Subject=%s" keyword-str)
                                (format "-Keywords=%s" keyword-str)
                                file))))

;;;###autoload
(defun transmute-retag-by-date ()
  "Rename images based on EXIF creation date."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((date-info (transmute-get-date file))
           (prop (car date-info))
           (val (cadr date-info)))
      (when date-info
        (when (member prop '("FileModifyDate" "ModifyDate"))
          (message "Writing %s to CreateDate and DateTimeOriginal for %s" prop file)
          (shell-command (format "exiftool -all= -overwrite_original_in_place \"-CreateDate<%s\" \"-DateTimeOriginal<%s\" %s"
                                 prop prop (shell-quote-argument file))))
        (let* ((formatted-date (transmute-format-date val))
               (parsed (transmute--parse-filename file))
               (current-ts (cdr (assoc 'timestamp parsed)))
               (label (cdr (assoc 'label parsed)))
               (tags-raw (cdr (assoc 'tags-raw parsed)))
               (ext (cdr (assoc 'extension parsed)))
               (basedir (cdr (assoc 'directory parsed))))
          (if (not (string= formatted-date current-ts))
              (let* ((new-base (format "%s--%s" formatted-date label))
                     (new-name (if tags-raw
                                   (format "%s/%s__%s.%s" basedir new-base tags-raw ext)
                                 (format "%s/%s.%s" basedir new-base ext)))
                     (final-name new-name)
                     (counter 1))
                (while (file-exists-p final-name)
                  (setq final-name (if tags-raw
                                       (format "%s/%s%d__%s.%s" basedir new-base counter tags-raw ext)
                                     (format "%s/%s%d.%s" basedir new-base counter ext)))
                  (cl-incf counter))
                (unless (string= (expand-file-name final-name) (expand-file-name file))
                  (message "%s -> %s" file final-name)
                  (rename-file file final-name)))
            (message "#### %s : NO CHANGE" file)))))))

;;;###autoload
(defun transmute-audio-convert ()
  "Convert audio to MP3."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".mp3")))
      (transmute--run-command "ffmpeg" "-hide_banner" "-loglevel" "warning" "-stats" "-y" "-i" file "-b:a" "192k" dst))))

;;;###autoload
(defun transmute-audio-info ()
  "Show audio ID3 tags."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute--run-command "id3v2" "-l" file)))

;;;###autoload
(defun transmute-picture-info ()
  "Show image metadata."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute--run-command "exiftool" "-g" file)))

;;;###autoload
(defun transmute-video-info ()
  "Show video info using ffprobe and exiftool."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute--run-command "ffprobe" file)
    (transmute--run-command "exiftool" file)))

;;;###autoload
(defun transmute-video-to-gif ()
  "Convert video to GIF."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".gif")))
      (transmute-convert-video file dst "-vf" "fps=20,scale=800:-1:flags=lanczos" "-loop" "0"))))

;;;###autoload
(defun transmute-picture-update-from-create-date ()
  "Update FileModifyDate and DateTimeOriginal from CreateDate."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (transmute--run-command "exiftool" "-overwrite_original" "-FileModifyDate<CreateDate" "-DateTimeOriginal<CreateDate" file)))

;;;###autoload
(defun transmute-picture-tag-rename ()
  "Rename file based on its tags and creation date."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((date-info (transmute-get-date file))
           (prop (car date-info))
           (val (cadr date-info))
           (tags-out (shell-command-to-string (format "exiftool -s3 -TagsList %s" (shell-quote-argument file))))
           (tags (string-trim tags-out)))
      (when (and date-info (not (string-empty-p tags)))
        (let* ((formatted-tags (replace-regexp-in-string "/" "@" (replace-regexp-in-string "," "-" tags)))
               (formatted-date (transmute-format-date val))
               (parsed (transmute--parse-filename file))
               (label (cdr (assoc 'label parsed)))
               (ext (cdr (assoc 'extension parsed)))
               (basedir (cdr (assoc 'directory parsed)))
               (new-base (format "%s--%s" formatted-date label))
               (new-name (format "%s/%s__%s.%s" basedir new-base formatted-tags ext))
               (final-name new-name)
               (counter 1))
          (while (file-exists-p final-name)
            (setq final-name (format "%s/%s%d__%s.%s" basedir new-base counter formatted-tags ext))
            (cl-incf counter))
          (unless (string= (expand-file-name final-name) (expand-file-name file))
            (message "%s -> %s" file final-name)
            (rename-file file final-name)))))))

;;;###autoload
(defun transmute-video-speed-up ()
  "Speed up video 2x (removes audio)."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (timestamp (format-time-string "%Y%m%d%H%M%S"))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-sped-" timestamp ".mp4")))
      (transmute-convert-video file dst "-threads" "8" "-an" "-filter:v" "setpts=0.5*PTS" "-r" "30"))))

;;;###autoload
(defun transmute-video-slow-down ()
  "Slow down video 5x (removes audio)."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (timestamp (format-time-string "%Y%m%d%H%M%S"))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-slow-" timestamp ".mp4")))
      (transmute-convert-video file dst "-threads" "2" "-an" "-filter:v" "setpts=5*PTS" "-r" "30"))))

;;;###autoload
(defun transmute-audio-normalise ()
  "Normalise audio using sox."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-norm." (cdr (assoc 'extension parsed)))))
      (transmute--run-command "sox" "--norm=0" file dst))))

;;;###autoload
(defun transmute-audio-trim-silence ()
  "Trim silence from start and end of audio."
  (interactive)
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-trim.mp3")))
      (transmute--run-command "ffmpeg" "-hide_banner" "-loglevel" "warning" "-stats" "-y" "-i" file
                                "-af" "silenceremove=start_periods=1:start_duration=1:start_threshold=-60dB:detection=peak,aformat=dblp,areverse,silenceremove=start_periods=1:start_duration=1:start_threshold=-60dB:detection=peak,aformat=dblp,areverse"
                                dst))))

;;;###autoload
(defun transmute-picture-crop (width height)
  "Crop image to WIDTHxHEIGHT centered."
  (interactive "nWidth: \nnHeight: ")
  (let ((dim (format "%dx%d" width height)))
    (transmute-do-batch (transmute-get-targets)
      (transmute-convert-image file file "-resize" (concat dim "^") "-gravity" "center" "-extent" dim))))

;;;###autoload
(defun transmute-video-cut (start duration)
  "Cut video from START for DURATION seconds."
  (interactive "sStart (HH:MM:SS or sec): \nsDuration (sec): ")
  (transmute-do-batch (transmute-get-targets)
    (let* ((parsed (transmute--parse-filename file))
           (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-cut." (cdr (assoc 'extension parsed)))))
      (transmute-convert-video file dst "-ss" start "-t" duration "-c" "copy"))))

;;;###autoload
(defun transmute-video-to-mp4 ()
  "Alias for video convert."
  (interactive)
  (call-interactively #'transmute-video-convert))

;;;###autoload
(defun transmute-completing-read-menu ()
  "Execute media command using completing-read."
  (interactive)
  (let* ((commands '(("Picture Convert" . transmute-picture-convert)
                     ("Picture Crush (640px)" . transmute-picture-crush)
                     ("Picture Scale (1920px)" . transmute-picture-scale)
                     ("Picture Correct (Brighten)" . transmute-picture-correct)
                     ("Picture Auto Colour" . transmute-picture-autocolour)
                     ("Picture Crop" . transmute-picture-crop)
                     ("Picture Upscale (GAN)" . transmute-picture-upscale)
                     ("Picture Get Text (OCR)" . transmute-picture-get-text)
                     ("Picture To PDF" . transmute-picture-to-pdf)
                     ("Picture Tag (Interactive)" . transmute-tag-interactive)
                     ("Picture Tag Rename" . transmute-picture-tag-rename)
                     ("Picture Retag by Date" . transmute-retag-by-date)
                     ("Picture Info" . transmute-picture-info)
                     ("Video Convert" . transmute-video-convert)
                     ("Video Shrink" . transmute-video-shrink)
                     ("Video To GIF" . transmute-video-to-gif)
                     ("Video Trim (Top/Tail)" . transmute-video-toptail)
                     ("Video Cut" . transmute-video-cut)
                     ("Video Reverse" . transmute-video-reverse)
                     ("Video Speed Up" . transmute-video-speed-up)
                     ("Video Slow Down" . transmute-video-slow-down)
                     ("Video Extract Audio" . transmute-video-extract-audio)
                     ("Video Remove Audio" . transmute-video-remove-audio)
                     ("Video Info" . transmute-video-info)
                     ("Audio Convert" . transmute-audio-convert)
                     ("Audio Normalise" . transmute-audio-normalise)
                     ("Audio Trim Silence" . transmute-audio-trim-silence)
                     ("Audio Info" . transmute-audio-info)))
         (choice (completing-read "Media Command: " (mapcar #'car commands) nil t))
         (func (cdr (assoc choice commands))))
    (when func
      (call-interactively func))))

;;; Transient Menu

;;;###autoload
(transient-define-prefix transmute-menu ()
  "Main menu for media management utilities."
  ["Image Commands"
   [("ic" "Convert" transmute-picture-convert)
    ("iz" "Crush (640px)" transmute-picture-crush)
    ("is" "Scale (1920px)" transmute-picture-scale)
    ("iu" "Upscale (GAN)" transmute-picture-upscale)]
   [("ir" "Rotate Right" transmute-picture-rotate-right)
    ("il" "Rotate Left" transmute-picture-rotate-left)
    ("ib" "Brighten" transmute-picture-correct)
    ("ia" "Auto Colour" transmute-picture-autocolour)]
   [("it" "Tag (Interactive)" transmute-tag-interactive)
    ("in" "Tag Rename" transmute-picture-tag-rename)
    ("id" "Retag by Date" transmute-retag-by-date)
    ("if" "Update from CreateDate" transmute-picture-update-from-create-date)]
   [("ip" "To PDF" transmute-picture-to-pdf)
    ("io" "OCR (Get Text)" transmute-picture-get-text)
    ("iC" "Crop" transmute-picture-crop)
    ("ii" "Info" transmute-picture-info)]]
  ["Video Commands"
   [("vc" "Convert" transmute-video-convert)
    ("vs" "Shrink" transmute-video-shrink)
    ("vg" "To GIF" transmute-video-to-gif)
    ("va" "Extract Audio" transmute-video-extract-audio)]
   [("vt" "Top/Tail (Trim)" transmute-video-toptail)
    ("vk" "Cut" transmute-video-cut)
    ("vr" "Reverse" transmute-video-reverse)
    ("vR" "Rotate Right" transmute-video-rotate-right)
    ("vL" "Rotate Left" transmute-video-rotate-left)]
   [("v+" "Speed Up" transmute-video-speed-up)
    ("v-" "Slow Down" transmute-video-slow-down)
    ("vx" "Remove Audio" transmute-video-remove-audio)
    ("vi" "Info" transmute-video-info)]]
  ["Audio Commands"
   [("ac" "Convert" transmute-audio-convert)
    ("an" "Normalise" transmute-audio-normalise)
    ("at" "Trim Silence" transmute-audio-trim-silence)
    ("ai" "Info" transmute-audio-info)]]
  ["Menus"
   [("m" "Completing Read Menu" transmute-completing-read-menu)]])

(provide 'transmute)

;;; transmute.el ends here
