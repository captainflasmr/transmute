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

(declare-function dired-image-thumbnail-get-marked "dired-image-thumbnail")
(declare-function dired-image-thumbnail--nearest-image-original-file-name "dired-image-thumbnail")
(declare-function dired-image-thumbnail-clear-preview-cache "dired-image-thumbnail")
(declare-function dired-image-thumbnail-refresh-current-display "dired-image-thumbnail")
(defvar image-dired-thumbnail-mode-map)

(defgroup transmute nil
  "Media management and conversion utilities."
  :group 'media)

(defvar transmute-active-processes nil
  "List of active transmute processes.")

(defvar transmute-batch-files nil
  "List of files processed in the current batch.")

(defvar transmute--last-renames nil
  "Alist mapping old filenames to new filenames from the last batch.")

(defvar transmute--inhibit-display-refresh-once nil
  "Internal flag to skip image display refresh once.")

(defvar transmute-after-batch-hook nil
  "Hook run after each transmute batch operation completes.
Useful for refreshing external buffers like image-dired thumbnail views.")

(defvar transmute-total-tasks 0
  "Total number of tasks in the current batch.")

(defvar transmute-completed-tasks 0
  "Number of completed tasks in the current batch.")

(defcustom transmute-tag-list-file "/home/jdyer/bin/category-list-uniq.txt"
  "Path to a file of known tags, one per line, for `transmute-tag-from-list'.
When nil, fallback completion is used."
  :type '(choice (file :tag "Tag list file") (const :tag "None" nil))
  :group 'transmute)

(defvar transmute-log-buffer-name "*transmute-log*"
  "Name of the buffer for transmute conversion output.")

(defvar transmute-info-buffer-name "*transmute-info*"
  "Name of the buffer for transmute metadata output.")

(define-derived-mode transmute-info-mode special-mode "Transmute-Info"
  "Major mode for viewing media metadata."
  (setq-local font-lock-defaults
              '((("^File:.*" . 'bold)
                 ("^----.*----$" . 'font-lock-function-name-face)
                 ("^\\s-*\\(.+?\\)\\s-*:" . (1 'font-lock-variable-name-face))))))

(defun transmute--display-info (targets &rest commands)
  "Run COMMANDS for each of TARGETS and display in a dedicated buffer.
COMMANDS can be a list of strings or (label . cmd) pairs."
  (let ((buf (get-buffer-create transmute-info-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (transmute-info-mode)
        (dolist (file targets)
          (insert (propertize (format "File: %s\n" file) 'face 'bold))
          (dolist (cmd-spec commands)
            (let ((label (if (listp cmd-spec) (car cmd-spec) nil))
                  (cmd (if (listp cmd-spec) (cdr cmd-spec) cmd-spec)))
              (when label
                (insert (propertize (format "[%s]\n" label) 'face 'font-lock-comment-face)))
              (insert (shell-command-to-string (format "%s %s 2>&1" cmd (shell-quote-argument file))))))
          (insert "\n" (make-string (window-width) ?=) "\n\n"))
        (goto-char (point-min))))
    (pop-to-buffer buf)))

(defun transmute-show-log ()
  "Show the transmute log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create transmute-log-buffer-name)))

(defun transmute-stop-conversions ()
  "Stop all currently active transmute conversions."
  (interactive)
  (when transmute-active-processes
    (let ((count (length transmute-active-processes)))
      (dolist (proc transmute-active-processes)
        (when (process-live-p proc)
          (interrupt-process proc)))
      (setq transmute-active-processes nil
            transmute-total-tasks 0
            transmute-completed-tasks 0)
      (transmute--update-progress-display)
      (transmute--log "[ABORT] Stopped %d active conversions." count)
      (message "Stopped %d active conversions." count))))

(defun transmute--log (msg &rest args)
  "Log MSG with ARGS to the transmute log buffer."
  (let ((buf (get-buffer-create transmute-log-buffer-name))
        (inhibit-read-only t))
    (with-current-buffer buf
      (unless (eq major-mode 'compilation-mode)
        (compilation-mode))
      (save-excursion
        (goto-char (point-max))
        (insert (apply #'format msg args) "\n")))))

(defun transmute-header-line-format ()
  "Return the header line format for transmute progress."
  (when (> transmute-total-tasks 0)
    (let* ((pct (if (> transmute-total-tasks 0)
                    (/ (* 100 transmute-completed-tasks) transmute-total-tasks)
                  0))
           (active (length transmute-active-processes)))
      (concat
       (propertize (format " [Transmuting: %d/%d (%d%%)] " 
                           transmute-completed-tasks 
                           transmute-total-tasks
                           pct)
                   'face '(:inherit mode-line-highlight :weight bold))
       (when (> active 0)
         (format " (%d active) " active))))))

(defvar transmute--header-line-entry '(:eval (transmute-header-line-format))
  "The entry added to `header-line-format`.")

(defun transmute--update-progress-display ()
  "Update the global header line based on active tasks."
  (when (bound-and-true-p transmute-progress-mode)
    (let ((current (default-value 'header-line-format)))
      (if transmute-active-processes
          (unless (member transmute--header-line-entry current)
            (setq-default header-line-format 
                          (if current 
                              (cons transmute--header-line-entry current)
                            (list transmute--header-line-entry))))
        (let ((new-fmt (delete transmute--header-line-entry current)))
          (setq-default header-line-format (if (or (null new-fmt) (equal new-fmt '(nil)))
                                               nil
                                             new-fmt)))
        (setq transmute-total-tasks 0
              transmute-completed-tasks 0)))
    (force-mode-line-update t)
    (redraw-display)))

;;;###autoload
(define-minor-mode transmute-progress-mode
  "Show transmute progress in the header line."
  :global t
  :group 'transmute
  (transmute--update-progress-display))

(defcustom transmute-trash-command "trash-put"
  "Command to use for trashing files."
  :type 'string
  :group 'transmute)

(defvar transmute-image-extensions
  '("jpg" "jpeg" "png" "gif" "bmp" "tiff" "webp" "svg" "heic" "heif")
  "List of image file extensions.")

(defvar transmute-video-extensions
  '("mp4" "mkv" "avi" "mov" "wmv" "flv" "webm" "m4v" "mpg" "mpeg")
  "List of video file extensions.")

(defvar transmute-audio-extensions
  '("mp3" "wav" "flac" "ogg" "m4a" "aac" "wma")
  "List of audio file extensions.")

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
Tries CreateDate, DateTimeOriginal, ModifyDate, and FileModifyDate.
Filters out exiftool warning lines from the output."
  (let ((props '("CreateDate" "DateTimeOriginal" "ModifyDate" "FileModifyDate"))
        (result nil))
    (cl-loop for prop in props
             until result
             do (let ((val (shell-command-to-string
                            (format "exiftool -s3 -%s %s"
                                    prop (shell-quote-argument (expand-file-name file))))))
                  (setq val (string-trim
                              (replace-regexp-in-string
                               "^Warning:.*\n\\|Warning:.*$" "" val)))
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

(defun transmute--process-sentinel (process _event)
  "Sentinel for transmute processes."
  (when (memq (process-status process) '(exit signal))
    (setq transmute-active-processes (delq process transmute-active-processes))
    (setq transmute-completed-tasks (1+ transmute-completed-tasks))
    (let ((buf (process-buffer process))
          (name (process-name process))
          (exit-code (process-exit-status process)))
      (if (zerop exit-code)
          (progn
            (transmute--log "[SUCCESS] %s" name)
            (message "Transmute task finished: %s" name)
            (when (buffer-live-p buf) (kill-buffer buf)))
        (transmute--log "[FAILED] %s (exit code %d)" name exit-code)
        (message "Transmute task FAILED: %s" name)
        (when (buffer-live-p buf)
          (transmute--log "--- Output for %s ---" name)
          (transmute--log "%s" (with-current-buffer buf (buffer-string)))
          (transmute--log "---------------------" name)
          (kill-buffer buf))))
    (when (null transmute-active-processes)
      (setq transmute-total-tasks 0
            transmute-completed-tasks 0)
      (run-hooks 'transmute-after-batch-hook))
    (transmute--update-progress-display)))

(defun transmute--run-command-async (name cmd)
  "Run CMD string asynchronously as NAME."
  (transmute--log "[START] %s: %s" name cmd)
  (let ((process (start-process-shell-command name 
                                              (generate-new-buffer (format " *transmute-%s*" name))
                                              cmd)))
    (push process transmute-active-processes)
    (set-process-sentinel process #'transmute--process-sentinel)
    (set-process-query-on-exit-flag process nil)
    (transmute--update-progress-display)
    process))

(defun transmute--run-command (cmd &rest args)
  "Run CMD with ARGS and return exit code."
  (let ((full-cmd (mapconcat #'shell-quote-argument (cons cmd args) " ")))
    (transmute--log "[EXEC] %s" full-cmd)
    (shell-command full-cmd)))

(defun transmute--preserve-metadata (src dst)
  "Copy metadata from SRC to DST, excluding image dimensions and orientation.
Sets Orientation to Normal."
  (transmute--run-command "exiftool" "-overwrite_original_in_place" "-Orientation=1" "-n"
                          "-TagsFromFile" src 
                          "--ExifImageWidth" "--ExifImageHeight" "--Orientation"
                          dst))

(defun transmute--trash (file)
  "Move FILE to trash."
  (if (executable-find transmute-trash-command)
      (transmute--run-command transmute-trash-command file)
    (move-file-to-trash file)))

(defun transmute--exif-cmd (src dst)
  "Return exiftool command string to copy tags from SRC to DST, excluding image dimensions.
Sets Orientation to Normal."
  (format "exiftool -overwrite_original_in_place -Orientation=1 -n -TagsFromFile %s --ExifImageWidth --ExifImageHeight --Orientation %s"
          (shell-quote-argument src) (shell-quote-argument dst)))

(defun transmute--set-processed-xattr (file)
  "Set user.do_backup.processed xattr on FILE.
This tells `do_backup` that the file has already been processed
and should not be rotated or re-processed on the next backup run."
  (when (executable-find "setfattr")
    (format "setfattr -n user.do_backup.processed -v 1 %s"
            (shell-quote-argument (expand-file-name file)))))

;;;###autoload
(defun transmute-picture-show-xattr ()
  "Show user.do_backup.processed xattr for selected files.
Displayed in a dedicated info buffer."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute--display-info targets
      '("Backup Xattr" . "sh -c 'getfattr -n user.do_backup.processed \"$1\" 2>&1 || echo \"(not set)\"' xattr"))))

;;;###autoload
(defun transmute-picture-set-xattr (name value)
  "Set xattr NAME to VALUE on selected files.
The NAME is completed against known xattr names."
  (interactive
   (let ((name (completing-read "Xattr name: "
                                '("user.do_backup.processed"
                                  "user.do_backup.skip"
                                  "user.transmute.processed")
                                nil nil "user.do_backup.processed")))
     (list name (read-string (format "Value for %s: " name) "1"))))
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (let* ((abs-file (expand-file-name file))
             (cmd (format "setfattr -n %s -v %s %s"
                          (shell-quote-argument name)
                          (shell-quote-argument value)
                          (shell-quote-argument abs-file))))
        (transmute--run-command-async (file-name-nondirectory file) cmd)))))

(defun transmute--rotate-image (file degrees)
  "Pixel-rotate FILE by DEGREES using ImageMagick.
Uses -auto-orient to bake any existing EXIF orientation into the pixels
first, then applies the rotation.  No EXIF orientation metadata is
written — the result is a pure pixel rotation compatible with all viewers."
  (let* ((file (expand-file-name file))
         (tmp (make-temp-file "transmute-" nil (concat "." (file-name-extension file))))
         (magick-cmd (format "magick %s -auto-orient -rotate %s %s"
                              (shell-quote-argument file) degrees (shell-quote-argument tmp)))
         (strip-orient (format "exiftool -overwrite_original_in_place -Orientation= %s"
                                (shell-quote-argument tmp)))
         (touch-cmd (format "touch -r %s %s" (shell-quote-argument file) (shell-quote-argument tmp)))
         (cp-cmd (format "cp -p %s %s" (shell-quote-argument tmp) (shell-quote-argument file)))
         (rm-tmp (format "rm %s" (shell-quote-argument tmp)))
         (xattr-cmd (transmute--set-processed-xattr file))
         (full-cmd (mapconcat #'identity (delq nil (list magick-cmd strip-orient touch-cmd cp-cmd rm-tmp xattr-cmd)) " && ")))
    (transmute--run-command-async (file-name-nondirectory file) full-cmd)))

;;; High-level Conversion Helpers

(defun transmute-convert-image (src dst &rest magick-args)
  "Convert image SRC to DST using MAGICK-ARGS.
Preserves metadata and moves SRC to trash."
  (let* ((src (expand-file-name src))
         (dst (expand-file-name dst))
         (tmp (make-temp-file "transmute-" nil (concat "." (file-name-extension dst))))
         (magick-cmd (mapconcat #'shell-quote-argument (append (list "magick" src) magick-args (list tmp)) " "))
         (exif-cmd (transmute--exif-cmd src tmp))
         (touch-cmd (format "touch -r %s %s" (shell-quote-argument src) (shell-quote-argument tmp)))
         (cp-cmd (format "cp -p %s %s" (shell-quote-argument tmp) (shell-quote-argument dst)))
         (rm-tmp (format "rm %s" (shell-quote-argument tmp)))
          (trash-cmd (unless (string= src dst)
                       (format "%s %s" transmute-trash-command (shell-quote-argument src))))
          (xattr-cmd (transmute--set-processed-xattr src))
          (full-cmd (mapconcat #'identity (delq nil (list magick-cmd exif-cmd touch-cmd cp-cmd rm-tmp trash-cmd xattr-cmd)) " && ")))
    (transmute--run-command-async (file-name-nondirectory src) full-cmd)))

(defun transmute-convert-image-copy (src dst &rest magick-args)
  "Convert image SRC to DST using MAGICK-ARGS.
Preserves metadata, keeps SRC."
  (let* ((src (expand-file-name src))
         (dst (expand-file-name dst))
         (tmp (make-temp-file "transmute-" nil (concat "." (file-name-extension dst))))
         (magick-cmd (mapconcat #'shell-quote-argument (append (list "magick" src) magick-args (list tmp)) " "))
         (exif-cmd (transmute--exif-cmd src tmp))
         (touch-cmd (format "touch -r %s %s" (shell-quote-argument src) (shell-quote-argument tmp)))
         (cp-cmd (format "cp -p %s %s" (shell-quote-argument tmp) (shell-quote-argument dst)))
         (rm-tmp (format "rm %s" (shell-quote-argument tmp)))
          (xattr-cmd (transmute--set-processed-xattr dst))
          (full-cmd (mapconcat #'identity (delq nil (list magick-cmd exif-cmd touch-cmd cp-cmd rm-tmp xattr-cmd)) " && ")))
    (transmute--run-command-async (file-name-nondirectory src) full-cmd)))

(defun transmute-convert-video (src dst &rest ffmpeg-args)
  "Convert video SRC to DST using FFMPEG-ARGS.
Preserves metadata."
  (let* ((src (expand-file-name src))
         (dst (expand-file-name dst))
         (tmp (make-temp-file "transmute-" nil (concat "." (file-name-extension dst))))
         (ffmpeg-cmd (mapconcat #'shell-quote-argument 
                                (append (list "ffmpeg" "-hide_banner" "-loglevel" "warning" "-stats" "-y" "-i" src "-map_metadata" "0" "-threads" "8")
                                        ffmpeg-args
                                        (list tmp)) " "))
         (touch-cmd (format "touch -r %s %s" (shell-quote-argument src) (shell-quote-argument tmp)))
         (cp-cmd (format "cp -p %s %s" (shell-quote-argument tmp) (shell-quote-argument dst)))
         (rm-tmp (format "rm %s" (shell-quote-argument tmp)))
         (full-cmd (mapconcat #'identity (list ffmpeg-cmd touch-cmd cp-cmd rm-tmp) " && ")))
    (transmute--run-command-async (file-name-nondirectory src) full-cmd)))

(defun transmute-convert-gan (src dst &rest gan-args)
  "Upscale image SRC to DST using realesrgan-ncnn-vulkan with GAN-ARGS.
Preserves metadata and moves SRC to trash."
  (let* ((src (expand-file-name src))
         (dst (expand-file-name dst))
         (tmp (make-temp-file "transmute-" nil (concat "." (file-name-extension dst))))
         (gan-cmd (mapconcat #'shell-quote-argument (append (list "realesrgan-ncnn-vulkan") gan-args (list "-i" src "-o" tmp)) " "))
         (exif-cmd (transmute--exif-cmd src tmp))
         (touch-cmd (format "touch -r %s %s" (shell-quote-argument src) (shell-quote-argument tmp)))
         (cp-cmd (format "cp -p %s %s" (shell-quote-argument tmp) (shell-quote-argument dst)))
         (rm-tmp (format "rm %s" (shell-quote-argument tmp)))
          (trash-cmd (unless (string= src dst)
                       (format "%s %s" transmute-trash-command (shell-quote-argument src))))
          (xattr-cmd (transmute--set-processed-xattr src))
          (full-cmd (mapconcat #'identity (delq nil (list gan-cmd exif-cmd touch-cmd cp-cmd rm-tmp trash-cmd xattr-cmd)) " && ")))
    (transmute--run-command-async (file-name-nondirectory src) full-cmd)))

;;; Batch / Dired Integration

(defun transmute-get-targets ()
  "Get list of files to process. 
If in dired, use marked files or file at point.
If in image-dired-thumbnail-mode, use marked or file at point.
Otherwise ask for file."
  (cond
   ((derived-mode-p 'dired-mode)
    (dired-get-marked-files))
   ((and (derived-mode-p 'image-dired-thumbnail-mode)
         (fboundp 'dired-image-thumbnail-get-marked))
    (or (dired-image-thumbnail-get-marked)
        (and (fboundp 'dired-image-thumbnail--nearest-image-original-file-name)
             (let ((file (dired-image-thumbnail--nearest-image-original-file-name)))
               (when file (list file))))))
   (t (list (read-file-name "Process file: ")))))

(defun transmute-get-filtered-targets (type)
  "Get marked files filtered by TYPE ('image, 'video, 'audio, or 'any)."
  (let* ((files (transmute-get-targets))
         (extensions (cl-case type
                       (image transmute-image-extensions)
                       (video transmute-video-extensions)
                       (audio transmute-audio-extensions)
                       (any (append transmute-image-extensions 
                                    transmute-video-extensions 
                                    transmute-audio-extensions))
                       (t nil))))
    (if extensions
        (let ((filtered (cl-remove-if-not
                         (lambda (f)
                           (let ((ext (file-name-extension f)))
                             (and ext (member (downcase ext) extensions))))
                         files)))
          (unless filtered
            (message "No %s files found in selection." (symbol-name type)))
          filtered)
      files)))

(defmacro transmute-do-batch (files &rest body)
  "Run BODY for each file in FILES, binding 'file' to current file.
After processing all files, run `transmute-after-batch-hook' if no
asynchronous processes are active."
  (declare (indent 1))
  `(let ((batch-files ,files))
     (setq transmute--last-renames nil)
     (setq transmute-total-tasks (+ transmute-total-tasks (length batch-files))
           transmute-batch-files batch-files)
     (dolist (file batch-files)
       (let ((default-directory (file-name-directory file)))
         ,@body
         ;; If this task was purely synchronous, increment completed count now
         (when (null transmute-active-processes)
           (setq transmute-completed-tasks (1+ transmute-completed-tasks))
           (transmute--update-progress-display))))
     ;; If after processing all files no asynchronous tasks are pending,
     ;; run the refresh hook immediately.
     (when (null transmute-active-processes)
       (setq transmute-total-tasks 0
             transmute-completed-tasks 0)
       (run-hooks 'transmute-after-batch-hook))))

;;; Specific Commands

;;;###autoload
(defun transmute-picture-convert ()
  "Convert images to JPG."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".jpg")))
        (if (string-suffix-p ".svg" file t)
            (transmute-convert-image-copy file dst "-density" "300" "-auto-orient" "-strip")
          (transmute-convert-image file dst "-auto-orient" "-strip"))))))

;;;###autoload
(defun transmute-picture-crush ()
  "Resize images to 640px."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (transmute-convert-image file file "-auto-orient" "-strip" "-quality" "50%" "-resize" "640x>" "-resize" "x640>"))))

;;;###autoload
(defun transmute-picture-scale ()
  "Resize images to 1920px."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (transmute-convert-image file file "-auto-orient" "-strip" "-quality" "50%" "-resize" "1920x>" "-resize" "x1920>"))))

;;;###autoload
(defun transmute-picture-rotate-right ()
  "Rotate images 90 degrees clockwise (pixel-only, no EXIF orientation)."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (transmute--rotate-image file "90"))))

;;;###autoload
(defun transmute-picture-rotate-left ()
  "Rotate images 90 degrees counter-clockwise (pixel-only, no EXIF orientation)."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (transmute--rotate-image file "-90"))))

;;;###autoload
(defun transmute-picture-correct ()
  "Brighten images (120% modulate)."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (transmute-convert-image file file "-modulate" "120,100,100"))))

;;;###autoload
(defun transmute-video-convert ()
  "Convert videos to MP4 (h264/aac)."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".mp4")))
        (transmute-convert-video file dst "-c:a" "aac" "-c:v" "libx264" "-crf" "23")))))

;;;###autoload
(defun transmute-video-shrink ()
  "Resize videos to fit 960x960."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".mp4")))
        (transmute-convert-video file dst 
                                 "-vf" "scale='min(960,iw)':'min(960,ih)':force_original_aspect_ratio=decrease"
                                 "-vcodec" "libx264" "-crf" "28" "-preset" "medium" "-movflags" "+faststart")))))

;;;###autoload
(defun transmute-video-extract-audio ()
  "Extract audio from videos as WAV."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (timestamp (format-time-string "%Y%m%d%H%M%S"))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-" timestamp ".wav")))
        (transmute-convert-video file dst "-vn" "-q:a" "0" "-map" "a")))))

;;;###autoload
(defun transmute-video-toptail (trim-start trim-end)
  "Trim video from start and end."
  (interactive "nTrim from start (sec): \nnTrim from end (sec): ")
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((duration-str (shell-command-to-string
                            (format "ffprobe -v error -show_entries format=duration -of default=noprint_wrappers=1:nokey=1 %s"
                                    (shell-quote-argument file))))
             (duration (floor (string-to-number duration-str)))
             (end-time (- duration trim-end))
             (parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-trimmed." (cdr (assoc 'extension parsed)))))
        (if (<= end-time trim-start)
            (message "Error: Trim amount exceeds duration for %s" file)
          (transmute-convert-video file dst "-ss" (number-to-string trim-start) "-t" (number-to-string end-time) "-c" "copy"))))))

;;;###autoload
(defun transmute-picture-upscale ()
  "Upscale images using realesrgan-x4plus."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".jpg")))
        (transmute-convert-gan file dst "-n" "realesrgan-x4plus" "-j" "8:8:8" "-f" "jpg")))))

;;;###autoload
(defun transmute-picture-get-text ()
  "Extract text from images using tesseract OCR."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (out-base (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)))))
        (transmute--run-command "tesseract" "-l" "eng" file out-base)))))

;;;###autoload
(defun transmute-picture-autocolour ()
  "Auto-level image colors."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (transmute-convert-image file file "-auto-level"))))

;;;###autoload
(defun transmute-video-remove-audio ()
  "Remove audio from videos."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (transmute-convert-video file file "-an" "-c:v" "copy"))))

;;;###autoload
(defun transmute-video-reverse ()
  "Reverse video and audio."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-reversed." (cdr (assoc 'extension parsed)))))
        (transmute-convert-video file dst "-vf" "reverse" "-af" "areverse")))))

;;;###autoload
(defun transmute-video-rotate-right ()
  "Rotate video 90 degrees clockwise."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (transmute-convert-video file file "-vf" "transpose=1"))))

;;;###autoload
(defun transmute-video-rotate-left ()
  "Rotate video 90 degrees counter-clockwise."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (transmute-convert-video file file "-vf" "transpose=2"))))

;;;###autoload
(defun transmute-picture-to-pdf ()
  "Convert images to PDF."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (if (= (length targets) 1)
        (let* ((file (car targets))
               (parsed (transmute--parse-filename file))
               (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".pdf")))
          (transmute--run-command-async (file-name-nondirectory file)
                                          (format "magick %s %s" 
                                                  (shell-quote-argument file)
                                                  (shell-quote-argument dst))))
      (let ((dst (read-file-name "Output PDF: ")))
        (transmute--run-command-async "Batch PDF"
                                        (format "magick %s %s"
                                                (mapconcat #'shell-quote-argument targets " ")
                                                (shell-quote-argument dst)))))))

;;;###autoload
(defun transmute-picture-montage (output-file)
  "Create a 2x4 montage of selected images at 1024px width.
Images are resized to temporary files to avoid modifying originals."
  (interactive (list (read-file-name "Output montage: " nil "montage_001.jpg")))
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (let* ((tmp-dir (make-temp-file "transmute-montage-" t))
           (resized-files nil)
           (width 1024)
           (resize-cmds nil))
      (dolist (img targets)
        (let* ((base (file-name-nondirectory img))
               (tmp-img (expand-file-name base tmp-dir)))
          (push tmp-img resized-files)
          (push (format "magick %s -resize %dx %s"
                        (shell-quote-argument (expand-file-name img))
                        width
                        (shell-quote-argument tmp-img))
                resize-cmds)))
      (let* ((montage-cmd (format "magick montage -tile 2x4 -mode Concatenate -gravity NorthWest -alpha off -background darkgrey %s %s"
                                 (mapconcat #'shell-quote-argument (reverse resized-files) " ")
                                 (shell-quote-argument (expand-file-name output-file))))
             (cleanup-cmd (format "rm -rf %s" (shell-quote-argument tmp-dir)))
             (full-cmd (mapconcat #'identity (append (reverse resize-cmds) (list montage-cmd cleanup-cmd)) " && ")))
        (transmute--run-command-async "Montage" full-cmd)))))

;;;###autoload
(defun transmute-tag-interactive (tags)
  "Interactively tag selected media files.
TAGS is a comma-separated string or list of tags."
  (interactive (list (completing-read-multiple "Tags (comma separated): " nil)))
  (when-let ((targets (transmute-get-filtered-targets 'any)))
    (let* ((tag-list (if (stringp tags) (split-string tags "," t) tags))
           (tag-str (mapconcat #'identity tag-list ","))
           (hier-tag-str (replace-regexp-in-string "@" "/" tag-str))
           (keywords (delete-dups (sort (mapcan (lambda (tag) (split-string (replace-regexp-in-string "@" " " tag) " " t)) tag-list) #'string<)))
           (keyword-str (mapconcat #'identity keywords ",")))
      (transmute-do-batch targets
        (message "Tagging %s with %s" file tag-str)
        (transmute--run-command "exiftool" "-P" "-overwrite_original_in_place"
                                  (format "-TagsList=%s" hier-tag-str)
                                  (format "-XMP-microsoft:LastKeywordXMP=%s" hier-tag-str)
                                  (format "-HierarchicalSubject=%s" (replace-regexp-in-string "/" "|" hier-tag-str))
                                  (format "-XPKeywords=%s" keyword-str)
                                  (format "-Subject=%s" keyword-str)
                                  file)))))

;;;###autoload
(defun transmute-picture-email ()
  "Prepare images for email sending by resizing and stripping metadata.
Saves to ~/Pictures/YYYYMMDDHH/."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (let* ((date (format-time-string "%Y%m%d%H"))
           (dest-dir (expand-file-name (format "~/Pictures/%s" date))))
      (unless (file-directory-p dest-dir)
        (make-directory dest-dir t))
      (transmute-do-batch targets
        (let* ((filename (file-name-nondirectory file))
               (dst (expand-file-name filename dest-dir)))
          (transmute-convert-image-copy file dst "-auto-orient" "-strip" "-quality" "50%" "-resize" "1920x>" "-resize" "x1920>"))))))

;;;###autoload
(defun transmute-picture-from-pdf ()
  "Extract JPEGs from PDF files using pdftoppm."
  (interactive)
  (let ((targets (seq-filter (lambda (f) (string-suffix-p ".pdf" f t)) (transmute-get-targets))))
    (if (null targets)
        (message "No PDF files selected.")
      (transmute-do-batch targets
        (let* ((parsed (transmute--parse-filename file))
               (basedir (cdr (assoc 'directory parsed)))
               (no-ext (cdr (assoc 'no-ext parsed)))
               (out-prefix (if (> (length no-ext) 20) (substring no-ext 0 20) no-ext))
               (dst-prefix (expand-file-name out-prefix basedir)))
          (transmute--run-command-async (file-name-nondirectory file)
                                          (format "pdftoppm -r 300 -jpeg %s %s"
                                                  (shell-quote-argument file)
                                                  (shell-quote-argument dst-prefix))))))))

;;;###autoload
(defun transmute-picture-orientation-reset ()
  "Reset EXIF orientation to normal and optimization image."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (let* ((cmd (format "exiftool -overwrite_original -orientation#=1 %s && magick %s -auto-orient -strip %s"
                           (shell-quote-argument file)
                           (shell-quote-argument file)
                           (shell-quote-argument file)))
             (xattr-cmd (transmute--set-processed-xattr file))
             (full-cmd (if xattr-cmd (format "%s && %s" cmd xattr-cmd) cmd)))
        (transmute--run-command-async (file-name-nondirectory file) full-cmd)))))

;;;###autoload
(defun transmute-picture-split (num-splits)
  "Split image horizontally into NUM-SPLITS parts."
  (interactive "nNumber of horizontal splits: ")
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute-do-batch targets
      (let* ((filename (file-name-nondirectory file))
             (no-ext (file-name-sans-extension filename))
             (dims (shell-command-to-string (format "identify -format \"%%w %%h\" %s" (shell-quote-argument file))))
             (dim-list (split-string dims " "))
             (width (string-to-number (car dim-list)))
             (height (string-to-number (cadr dim-list)))
             (slice-width (/ width num-splits))
             (cmds nil))
        (dotimes (i num-splits)
          (let* ((offset (* i slice-width))
                 (out-file (format "%s_%d.png" no-ext i)))
            (push (format "magick %s -crop %dx%d+%d+0 %s"
                          (shell-quote-argument file)
                          slice-width height offset
                          (shell-quote-argument out-file))
                  cmds)))
        (transmute--run-command-async (file-name-nondirectory file)
                                        (mapconcat #'identity (reverse cmds) " && "))))))

;;;###autoload
(defun transmute-picture-to-capture ()
  "Trigger Org capture gallery with selected files."
  (interactive)
  (when-let ((targets (transmute-get-targets)))
    (let ((files-str (mapconcat #'expand-file-name targets ";")))
      (when (fboundp 'my/external-org-capture-blog-with-gallery)
        (my/external-org-capture-blog-with-gallery files-str)))))

(defun transmute--known-tags ()
  "Return a list of known tags from `transmute-tag-list-file'."
  (when (and transmute-tag-list-file
             (file-readable-p transmute-tag-list-file))
    (with-temp-buffer
      (insert-file-contents transmute-tag-list-file)
      (split-string (buffer-string) "\n" t))))

;;;###autoload
(defun transmute-tag-from-list (tags)
  "Interactively tag selected media files from a list of known tags.
TAGS is a comma-separated string or list of tags selected via
`completing-read-multiple' against the tag list file."
  (interactive (list (completing-read-multiple "Tags: " (transmute--known-tags))))
  (when-let ((targets (transmute-get-filtered-targets 'any)))
    (let* ((tag-list (if (stringp tags) (split-string tags "," t) tags))
           (tag-str (mapconcat #'identity tag-list ","))
           (hier-tag-str (replace-regexp-in-string "@" "/" tag-str))
           (keywords (delete-dups (sort (mapcan (lambda (tag) (split-string (replace-regexp-in-string "@" " " tag) " " t)) tag-list) #'string<)))
           (keyword-str (mapconcat #'identity keywords ",")))
      (transmute-do-batch targets
        (message "Tagging %s with %s" file tag-str)
        (transmute--run-command "exiftool" "-P" "-overwrite_original_in_place"
                                  (format "-TagsList=%s" hier-tag-str)
                                  (format "-XMP-microsoft:LastKeywordXMP=%s" hier-tag-str)
                                  (format "-HierarchicalSubject=%s" (replace-regexp-in-string "/" "|" hier-tag-str))
                                  (format "-XPKeywords=%s" keyword-str)
                                  (format "-Subject=%s" keyword-str)
                                  (format "-Keywords=%s" keyword-str)
                                  file)))))

;;;###autoload
(defun transmute-retag-by-date ()
  "Rename images based on EXIF creation date."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'any)))
    (transmute-do-batch targets
      (let* ((date-info (transmute-get-date file))
             (prop (car date-info))
             (val (cadr date-info)))
        (when date-info
          (when (member prop '("FileModifyDate" "ModifyDate"))
            (message "Writing %s to CreateDate and DateTimeOriginal for %s" prop file)
            (shell-command (format "exiftool -P -all= -overwrite_original_in_place \"-CreateDate<%s\" \"-DateTimeOriginal<%s\" %s"
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
              (message "#### %s : NO CHANGE" file))))))))

;;;###autoload
(defun transmute-audio-convert ()
  "Convert audio to MP3."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'audio)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".mp3")))
        (transmute--run-command-async (file-name-nondirectory file)
                                        (format "ffmpeg -hide_banner -loglevel warning -stats -y -i %s -b:a 192k %s"
                                                (shell-quote-argument file)
                                                (shell-quote-argument (expand-file-name dst))))))))

;;;###autoload
(defun transmute-audio-info ()
  "Show audio ID3 tags."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'audio)))
    (transmute--display-info targets "id3v2 -l")))

;;;###autoload
(defun transmute-picture-info ()
  "Show image metadata."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    (transmute--display-info targets "exiftool -g")))

;;;###autoload
(defun transmute-video-info ()
  "Show video info using ffprobe and exiftool."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute--display-info targets 
                             '("FFProbe" . "ffprobe -hide_banner")
                             '("ExifTool" . "exiftool"))))

;;;###autoload
(defun transmute-video-to-gif ()
  "Convert video to GIF."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) ".gif")))
        (transmute-convert-video file dst "-vf" "fps=20,scale=800:-1:flags=lanczos" "-loop" "0")))))

;;;###autoload
(defun transmute-picture-update-from-create-date ()
  "Update FileModifyDate and DateTimeOriginal from CreateDate."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'any)))
    (transmute-do-batch targets
      (transmute--run-command-async (file-name-nondirectory file)
                                      (format "exiftool -P -overwrite_original \"-FileModifyDate<CreateDate\" \"-DateTimeOriginal<CreateDate\" %s"
                                              (shell-quote-argument file))))))

;;;###autoload
(defun transmute-picture-update-to-create-date ()
  "Update CreateDate and DateTimeOriginal from FileModifyDate."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'any)))
    (transmute-do-batch targets
      (transmute--run-command-async (file-name-nondirectory file)
                                      (format "exiftool -overwrite_original \"-CreateDate<FileModifyDate\" \"-DateTimeOriginal<FileModifyDate\" %s"
                                              (shell-quote-argument file))))))

;;;###autoload
(defun transmute-picture-tag-rename ()
  "Rename file based on its tags and creation date."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'any)))
    (transmute-do-batch targets
      (let* ((date-info (transmute-get-date file))
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
            (let ((abs-final (expand-file-name final-name basedir))
                  (abs-orig (expand-file-name file)))
              (unless (string= abs-final abs-orig)
                (message "%s -> %s" abs-orig abs-final)
                (setq transmute--last-renames (cons (cons abs-orig abs-final) transmute--last-renames))
                (rename-file abs-orig abs-final)))))))))

;;;###autoload
(defun transmute-tag-and-rename (tags)
  "Tag selected media files from known tags, then rename based on tags and date.
TAGS is selected via `completing-read-multiple' against the tag list file.
After writing metadata, each file is renamed to the
YYYYMMDDHHMMSS--label__tag1@tag2.ext pattern."
  (interactive (list (completing-read-multiple "Tags: " (transmute--known-tags))))
  (when-let ((targets (transmute-get-filtered-targets 'any)))
    (let* ((tag-list (if (stringp tags) (split-string tags "," t) tags))
           (tag-str (mapconcat #'identity tag-list ","))
           (hier-tag-str (replace-regexp-in-string "@" "/" tag-str))
           (keywords (delete-dups (sort (mapcan (lambda (tag) (split-string (replace-regexp-in-string "@" " " tag) " " t)) tag-list) #'string<)))
           (keyword-str (mapconcat #'identity keywords ","))
           (formatted-tags (replace-regexp-in-string "/" "@" (replace-regexp-in-string "," "-" hier-tag-str))))
      (transmute-do-batch targets
        (message "Tagging %s with %s" file tag-str)
        (transmute--run-command "exiftool" "-P" "-overwrite_original_in_place"
                                  (format "-TagsList=%s" hier-tag-str)
                                  (format "-XMP-microsoft:LastKeywordXMP=%s" hier-tag-str)
                                  (format "-HierarchicalSubject=%s" (replace-regexp-in-string "/" "|" hier-tag-str))
                                  (format "-XPKeywords=%s" keyword-str)
                                  (format "-Subject=%s" keyword-str)
                                  (format "-Keywords=%s" keyword-str)
                                  file)
        (let* ((date-info (transmute-get-date file))
               (val (cadr date-info)))
          (when date-info
            (let* ((formatted-date (transmute-format-date val))
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
              (let ((abs-final (expand-file-name final-name basedir))
                    (abs-orig (expand-file-name file)))
                (unless (string= abs-final abs-orig)
                  (message "%s -> %s" abs-orig abs-final)
                  (setq transmute--last-renames (cons (cons abs-orig abs-final) transmute--last-renames))
                  (rename-file abs-orig abs-final))))))))))

;;;###autoload
(defun transmute-clear-tags ()
  "Remove all tags from selected media files."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'any)))
    (transmute-do-batch targets
      (message "Clearing tags from %s" file)
      (transmute--run-command "exiftool" "-P" "-overwrite_original_in_place"
                                "-TagsList=" "-XMP-microsoft:LastKeywordXMP="
                                "-HierarchicalSubject=" "-XPKeywords="
                                "-Subject=" "-Keywords=" file))))

(defun transmute--exif-field (file field)
  "Return exiftool FIELD value for FILE, or nil if empty."
  (let ((val (string-trim
              (shell-command-to-string
               (format "exiftool -s3 -%s %s" field (shell-quote-argument (expand-file-name file)))))))
    (unless (or (string-empty-p val) (string-prefix-p "Warning" val)) val)))

(defun transmute--get-sidecar-files (file)
  "Return list of sidecar files for FILE (e.g. file.xmp, file.jpg.xmp)."
  (let* ((abs-file (expand-file-name file))
         (dir (file-name-directory abs-file))
         (name (file-name-nondirectory abs-file))
         (sidecars nil))
    ;; case 1: file.xmp
    (let ((s1 (concat (file-name-sans-extension abs-file) ".xmp")))
      (when (and (file-exists-p s1) (not (string= s1 abs-file)))
        (push s1 sidecars)))
    ;; case 2: file.ext.xmp
    (let ((s2 (concat abs-file ".xmp")))
      (when (file-exists-p s2)
        (push s2 sidecars)))
    sidecars))

;;;###autoload
(defun transmute-picture-organise ()
  "Move selected files into YYYYMM subdirectories and move sidecars (.xmp)."
  (interactive)
  (when-let ((targets (transmute-get-targets)))
    ;; Pop to log buffer so user sees progress
    (let ((buf (get-buffer-create transmute-log-buffer-name)))
      (pop-to-buffer buf)
      (transmute--log "[START] Organizing %d files..." (length targets)))
    (setq transmute--inhibit-display-refresh-once t)
    (transmute-do-batch targets
      (let* ((date-info (transmute-get-date file))
             (val (cadr date-info)))
        (if (not date-info)
            (transmute--log "[WARN] No date found for %s" file)
          (let* ((formatted-date (transmute-format-date val))
                 (year-month (substring formatted-date 0 6))
                 (basedir (file-name-directory (expand-file-name file)))
                 (dest-dir (expand-file-name year-month basedir))
                 (main-name (file-name-nondirectory file))
                 (new-main (expand-file-name main-name dest-dir))
                 (sidecars (transmute--get-sidecar-files file)))
            (unless (file-directory-p dest-dir)
              (make-directory dest-dir t))
            ;; Update rename record for main file
            (let ((abs-orig (expand-file-name file))
                  (abs-new (expand-file-name new-main)))
              (unless (string= abs-orig abs-new)
                (transmute--log "[MOVE] %s -> %s" (file-name-nondirectory abs-orig) 
                                (concat year-month "/" (file-name-nondirectory abs-new)))
                (setq transmute--last-renames (cons (cons abs-orig abs-new) transmute--last-renames))
                (rename-file abs-orig abs-new t))
              ;; Move sidecars
              (dolist (s sidecars)
                (let* ((s-abs (expand-file-name s))
                       (new-s (expand-file-name (file-name-nondirectory s) dest-dir))
                       (new-s-abs (expand-file-name new-s)))
                  (unless (string= s-abs new-s-abs)
                    (transmute--log "[SIDE] %s -> %s" (file-name-nondirectory s-abs)
                                    (concat year-month "/" (file-name-nondirectory new-s-abs)))
                    (setq transmute--last-renames (cons (cons s-abs new-s-abs) transmute--last-renames))
                    (rename-file s-abs new-s-abs t)))))))))
    ;; After organization, hide the stale image display
    (when (fboundp 'dired-image-thumbnail-hide-display)
      (dired-image-thumbnail-hide-display))
    ;; Ensure log buffer remains visible and selected
    (pop-to-buffer (get-buffer-create transmute-log-buffer-name))))

;;;###autoload
(defun transmute-tag-info ()
  "Show tags and key metadata for selected media files in a summary buffer."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'any)))
    (transmute--display-info targets "exiftool -TagsList -HierarchicalSubject -Keywords -Subject -CreateDate -DateTimeOriginal -FileModifyDate -ImageSize -MIMEType")))

;;;###autoload
(defun transmute-video-speed-up ()
  "Speed up video 2x (removes audio)."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (timestamp (format-time-string "%Y%m%d%H%M%S"))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-sped-" timestamp ".mp4")))
        (transmute-convert-video file dst "-threads" "8" "-an" "-filter:v" "setpts=0.5*PTS" "-r" "30")))))

;;;###autoload
(defun transmute-picture-fix-whatsapp ()
  "Fix WhatsApp images by extracting date from filename and setting metadata.
Renames file to YYYYMMDD120000--IMG-YYYYMMDD-WA... pattern and sets EXIF dates."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'image)))
    ;; Pop to log buffer
    (let ((buf (get-buffer-create transmute-log-buffer-name)))
      (display-buffer buf)
      (transmute--log "[START] Fixing WhatsApp for %d files..." (length targets)))
    (transmute-do-batch targets
      (let* ((filename (file-name-nondirectory file))
             (basedir (file-name-directory (expand-file-name file))))
        (if (string-match "IMG-\\([0-9]\\{8\\}\\)-WA" filename)
            (let* ((imgdate (match-string 1 filename))
                   (newtimestamp (concat imgdate "120000"))
                   ;; Construct 'rest' similar to the bash script logic
                   (rest (cond
                          ((string-match "--IMG-[0-9]\\{8\\}\\(.*\\)$" filename)
                           (concat "--IMG-" imgdate (match-string 1 filename)))
                          ((string-match "IMG-[0-9]\\{8\\}\\(.*\\)$" filename)
                           (concat "--IMG-" imgdate (match-string 1 filename)))
                          (t (concat "--" filename))))
                   (newname (concat newtimestamp rest))
                   (abs-newname (expand-file-name newname basedir))
                   (date-fmt (format "%s:%s:%s 12:00:00"
                                     (substring imgdate 0 4)
                                     (substring imgdate 4 6)
                                     (substring imgdate 6 8))))
              (transmute--log "[FIX] %s -> %s" filename newname)
              ;; Set EXIF metadata
              (transmute--run-command "exiftool" "-overwrite_original"
                                      (format "-DateTimeOriginal=%s" date-fmt)
                                      (format "-CreateDate=%s" date-fmt)
                                      (format "-ModifyDate=%s" date-fmt)
                                      file)
              ;; Update filesystem time
              (transmute--run-command "touch" "-t" (concat imgdate "1200") file)
              ;; Rename the file
              (let ((abs-orig (expand-file-name file)))
                (unless (string= abs-orig abs-newname)
                  (setq transmute--last-renames (cons (cons abs-orig abs-newname) transmute--last-renames))
                  (rename-file abs-orig abs-newname t))))
          (transmute--log "[SKIP] %s: Does not match WhatsApp pattern" filename))))))

;;;###autoload
(defun transmute-video-slow-down ()
  "Slow down video 5x (removes audio)."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (timestamp (format-time-string "%Y%m%d%H%M%S"))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-slow-" timestamp ".mp4")))
        (transmute-convert-video file dst "-threads" "2" "-an" "-filter:v" "setpts=5*PTS" "-r" "30")))))

;;;###autoload
(defun transmute-video-concat ()
  "Concatenate selected video files in sorted order."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (let* ((tmp-file (make-temp-file "transmute-concat-" nil ".txt"))
           (sorted-targets (sort targets #'string<))
           (basedir (file-name-directory (expand-file-name (car targets))))
           (output-file (expand-file-name (format "concat-%s.mp4" (format-time-string "%Y%m%d%H%M%S")) basedir)))
      (with-temp-file tmp-file
        (dolist (f sorted-targets)
          (insert (format "file '%s'\n" (expand-file-name f)))))
      (let ((cmd (format "ffmpeg -f concat -safe 0 -i %s -c copy %s && rm %s"
                         (shell-quote-argument tmp-file)
                         (shell-quote-argument output-file)
                         (shell-quote-argument tmp-file))))
        (transmute--run-command-async "Concatenate" cmd)))))

;;;###autoload
(defun transmute-video-double ()
  "Double the length of each selected video by repeating it."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((tmp-list (make-temp-file "transmute-double-" nil ".txt"))
             (base (file-name-nondirectory file))
             (no-ext (file-name-sans-extension base))
             (ext (file-name-extension base))
             (dst (expand-file-name (format "%s-double.%s" no-ext ext) (file-name-directory (expand-file-name file)))))
        (with-temp-file tmp-list
          (insert (format "file '%s'\n" (expand-file-name file)))
          (insert (format "file '%s'\n" (expand-file-name file))))
        (let ((cmd (format "ffmpeg -f concat -safe 0 -i %s -c copy %s && rm %s"
                           (shell-quote-argument tmp-list)
                           (shell-quote-argument dst)
                           (shell-quote-argument tmp-list))))
          (transmute--run-command-async (format "Double %s" base) cmd))))))

;;;###autoload
(defun transmute-video-extract-frames (fps)
  "Extract frames from video at FPS (default 10)."
  (interactive (list (read-number "FPS: " 10)))
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((basedir (file-name-directory (expand-file-name file)))
             (frames-dir (expand-file-name "frames" basedir)))
        (unless (file-directory-p frames-dir)
          (make-directory frames-dir t))
        (let ((cmd (format "ffmpeg -y -i %s -vf fps=%d %s/%%07d.jpg"
                           (shell-quote-argument file)
                           fps
                           (shell-quote-argument frames-dir))))
          (transmute--run-command-async (format "Extract Frames %s" (file-name-nondirectory file)) cmd))))))

;;;###autoload
(defun transmute-video-set-fps (fps)
  "Change video framerate to FPS."
  (interactive "nTarget FPS: ")
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((base (file-name-nondirectory file))
             (no-ext (file-name-sans-extension base))
             (ext (file-name-extension base))
             (dst (expand-file-name (format "%s-fps%d.%s" no-ext fps ext) (file-name-directory (expand-file-name file)))))
        (transmute-convert-video file dst "-filter:v" (format "fps=fps=%d" fps))))))

;;;###autoload
(defun transmute-video-from-frames (pattern fps)
  "Create video from image sequence PATTERN (e.g. img%%03d.jpg) at FPS."
  (interactive (list (read-string "Frame pattern: " "img%03d.jpg")
                     (read-number "FPS: " 25)))
  (let* ((basedir default-directory)
         (output-file (expand-file-name "out.mp4" basedir)))
    (let ((cmd (format "ffmpeg -y -start_number 0 -i %s -c:v libx264 -vf \"fps=%d,format=yuv420p\" %s"
                       (shell-quote-argument pattern)
                       fps
                       (shell-quote-argument output-file))))
      (transmute--run-command-async "Video from Frames" cmd))))

;;;###autoload
(defun transmute-video-remove-flips ()
  "Remove rapid camera flips/glitches using scene filter."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((base (file-name-nondirectory file))
             (no-ext (file-name-sans-extension base))
             (ext (file-name-extension base))
             (tmp1 (expand-file-name (format "%s-pass1.%s" no-ext ext) (file-name-directory (expand-file-name file))))
             (dst (expand-file-name (format "%s-filtered.%s" no-ext ext) (file-name-directory (expand-file-name file)))))
        (let ((cmd (format "ffmpeg -i %s -vf \"select='lt(scene,0.005)',setpts=N/FRAME_RATE/TB\" -vsync vfr %s && ffmpeg -i %s -vf \"select='lt(scene,0.005)',setpts=N/FRAME_RATE/TB\" -vsync vfr %s && rm %s"
                           (shell-quote-argument file)
                           (shell-quote-argument tmp1)
                           (shell-quote-argument tmp1)
                           (shell-quote-argument dst)
                           (shell-quote-argument tmp1))))
          (transmute--run-command-async (format "Remove Flips %s" base) cmd))))))

;;;###autoload
(defun transmute-video-replace-audio (audio-file)
  "Replace video audio with AUDIO-FILE (default newaudio.wav)."
  (interactive (list (read-file-name "Audio file: " nil "newaudio.wav" t)))
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((base (file-name-nondirectory file))
             (no-ext (file-name-sans-extension base))
             (ext (file-name-extension base))
             (dst (expand-file-name (format "%s-newaudio.%s" no-ext ext) (file-name-directory (expand-file-name file)))))
        (let ((cmd (format "ffmpeg -y -i %s -i %s -map 0:v:0 -map 1:a:0 -c:v copy %s"
                           (shell-quote-argument file)
                           (shell-quote-argument audio-file)
                           (shell-quote-argument dst))))
          (transmute--run-command-async (format "Replace Audio %s" base) cmd))))))

;;;###autoload
(defun transmute-video-background-music (music-dir)
  "Add random background music from MUSIC-DIR to selected videos."
  (interactive "DMusic directory: ")
  (when-let ((targets (transmute-get-filtered-targets 'video))
             (music-files (directory-files music-dir t "\\.mp3$\\|\\.wav$")))
    (transmute-do-batch targets
      (let* ((music (nth (random (length music-files)) music-files))
             (base (file-name-nondirectory file))
             (no-ext (file-name-sans-extension base))
             (ext (file-name-extension base))
             (dst (expand-file-name (format "%s-bgm.%s" no-ext ext) (file-name-directory (expand-file-name file))))
             (duration (string-to-number (shell-command-to-string (format "ffprobe -v quiet -of csv=p=0 -show_entries format=duration %s" (shell-quote-argument file)))))
             (fade-duration 5)
             (fade-start (- duration fade-duration))
             (cmd (format "ffmpeg -y -i %s -stream_loop -1 -i %s -map 0:v:0 -map 1:a:0 -t %f -af \"afade=t=out:st=%f:d=%d\" -c:v copy %s"
                          (shell-quote-argument file)
                          (shell-quote-argument music)
                          duration
                          fade-start fade-duration
                          (shell-quote-argument dst))))
        (transmute--run-command-async (format "BG Music %s" base) cmd)))))

;;;###autoload
(defun transmute-video-rescale (width height)
  "Rescale video to WIDTH x HEIGHT (default 1800x1080)."
  (interactive (list (read-number "Width: " 1800)
                     (read-number "Height: " 1080)))
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((base (file-name-nondirectory file))
             (no-ext (file-name-sans-extension base))
             (ext (file-name-extension base))
             (dst (expand-file-name (format "%s-rescaled.%s" no-ext ext) (file-name-directory (expand-file-name file)))))
        (transmute-convert-video file dst "-vf" (format "scale=%d:%d" width height) "-vcodec" "libx264" "-crf" "23")))))

;;;###autoload
(defun transmute-video-rotate-ccw ()
  "Rotate video 90 degrees counter-clockwise."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((base (file-name-nondirectory file))
             (no-ext (file-name-sans-extension base))
             (ext (file-name-extension base))
             (dst (expand-file-name (format "%s-rotated-ccw.%s" no-ext ext) (file-name-directory (expand-file-name file)))))
        (transmute-convert-video file dst "-vf" "transpose=2")))))

;;;###autoload
(defun transmute-audio-normalise ()
  "Normalise audio using sox."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'audio)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-norm." (cdr (assoc 'extension parsed)))))
        (transmute--run-command-async (file-name-nondirectory file)
                                        (format "sox --norm=0 %s %s"
                                                (shell-quote-argument file)
                                                (shell-quote-argument (expand-file-name dst))))))))

;;;###autoload
(defun transmute-audio-trim-silence ()
  "Trim silence from start and end of audio."
  (interactive)
  (when-let ((targets (transmute-get-filtered-targets 'audio)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-trim.mp3")))
        (transmute--run-command-async (file-name-nondirectory file)
                                        (format "ffmpeg -hide_banner -loglevel warning -stats -y -i %s -af silenceremove=start_periods=1:start_duration=1:start_threshold=-60dB:detection=peak,aformat=dblp,areverse,silenceremove=start_periods=1:start_duration=1:start_threshold=-60dB:detection=peak,aformat=dblp,areverse %s"
                                                (shell-quote-argument file)
                                                (shell-quote-argument (expand-file-name dst))))))))

;;;###autoload
(defun transmute-picture-crop (width height)
  "Crop image to WIDTHxHEIGHT centered."
  (interactive "nWidth: \nnHeight: ")
  (let ((dim (format "%dx%d" width height)))
    (when-let ((targets (transmute-get-filtered-targets 'image)))
      (transmute-do-batch targets
        (transmute-convert-image file file "-resize" (concat dim "^") "-gravity" "center" "-extent" dim)))))

;;;###autoload
(defun transmute-video-cut (start duration)
  "Cut video from START for DURATION seconds."
  (interactive "sStart (HH:MM:SS or sec): \nsDuration (sec): ")
  (when-let ((targets (transmute-get-filtered-targets 'video)))
    (transmute-do-batch targets
      (let* ((parsed (transmute--parse-filename file))
             (dst (concat (cdr (assoc 'directory parsed)) (cdr (assoc 'no-ext parsed)) "-cut." (cdr (assoc 'extension parsed)))))
        (transmute-convert-video file dst "-ss" start "-t" duration "-c" "copy")))))

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
                     ("Picture Tag from List" . transmute-tag-from-list)
                     ("Picture Add Tags & Rename" . transmute-tag-and-rename)
                     ("Picture Rename from Tags" . transmute-picture-tag-rename)
                     ("Picture Retag by Date" . transmute-retag-by-date)
                     ("Picture Clear Tags" . transmute-clear-tags)
                     ("Picture Tag Info" . transmute-tag-info)
                     ("Picture Info" . transmute-picture-info)
                     ("Picture Montage" . transmute-picture-montage)
                     ("Picture Organise" . transmute-picture-organise)
                     ("Picture Fix WhatsApp" . transmute-picture-fix-whatsapp)
                     ("Picture Email" . transmute-picture-email)
                     ("Picture From PDF" . transmute-picture-from-pdf)
                     ("Picture Orientation Reset" . transmute-picture-orientation-reset)
                     ("Picture Split" . transmute-picture-split)
                     ("Picture To Capture" . transmute-picture-to-capture)
                     ("Picture Update to CreateDate" . transmute-picture-update-to-create-date)
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
                     ("Video Replace Audio" . transmute-video-replace-audio)
                     ("Video Background Music" . transmute-video-background-music)
                     ("Video Concat" . transmute-video-concat)
                     ("Video Double" . transmute-video-double)
                     ("Video Extract Frames" . transmute-video-extract-frames)
                     ("Video From Frames" . transmute-video-from-frames)
                     ("Video Remove Flips" . transmute-video-remove-flips)
                     ("Video Set FPS" . transmute-video-set-fps)
                     ("Video Rescale" . transmute-video-rescale)
                     ("Video Rotate CCW" . transmute-video-rotate-ccw)
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
  ["Image Management"
   ["Basic Ops"
    ("ic" "Convert" transmute-picture-convert)
    ("iC" "Crop" transmute-picture-crop)
    ("iS" "Split" transmute-picture-split)
    ("im" "Montage" transmute-picture-montage)
    ("ip" "To PDF" transmute-picture-to-pdf)
    ("id" "From PDF" transmute-picture-from-pdf)
    ("ii" "Info" transmute-picture-info)
    ("ix" "Show Backup Xattr" transmute-picture-show-xattr)
    ("iX" "Set Xattr" transmute-picture-set-xattr)]
   ["Enhance & Process"
    ("iz" "Crush (640px)" transmute-picture-crush)
    ("is" "Scale (1920px)" transmute-picture-scale)
    ("iu" "Upscale (GAN)" transmute-picture-upscale)
    ("ir" "Rotate Right" transmute-picture-rotate-right)
    ("il" "Rotate Left" transmute-picture-rotate-left)
    ("ib" "Brighten" transmute-picture-correct)
    ("ia" "Auto Colour" transmute-picture-autocolour)
    ("iO" "Orientation Reset" transmute-picture-orientation-reset)]
   ["Organisation"
    ("io" "Organise" transmute-picture-organise)
    ("iw" "WhatsApp Fix" transmute-picture-fix-whatsapp)
    ("iF" "Update from CreateDate" transmute-picture-update-from-create-date)
    ("iU" "Update to CreateDate" transmute-picture-update-to-create-date)]
   ["Publish"
    ("ie" "Email" transmute-picture-email)
    ("ig" "To Capture" transmute-picture-to-capture)]]
  ["Video & Audio"
   ["Video Transform"
    ("vc" "Convert" transmute-video-convert)
    ("vs" "Shrink" transmute-video-shrink)
    ("ve" "Rescale" transmute-video-rescale)
    ("vR" "Rotate Right" transmute-video-rotate-right)
    ("vL" "Rotate Left" transmute-video-rotate-left)
    ("vC" "Rotate CCW" transmute-video-rotate-ccw)
    ("vr" "Reverse" transmute-video-reverse)
    ("vt" "Top/Tail (Trim)" transmute-video-toptail)
    ("vk" "Cut" transmute-video-cut)]
   ["Video Effects"
    ("vg" "To GIF" transmute-video-to-gif)
    ("v+" "Speed Up" transmute-video-speed-up)
    ("v-" "Slow Down" transmute-video-slow-down)
    ("vf" "Remove Flips" transmute-video-remove-flips)
    ("vF" "Set FPS" transmute-video-set-fps)]
   ["Video Sequence"
    ("vj" "Concat" transmute-video-concat)
    ("v2" "Double" transmute-video-double)
    ("vE" "Extract Frames" transmute-video-extract-frames)
    ("vI" "From Frames" transmute-video-from-frames)]
   ["Video Audio"
    ("va" "Extract Audio" transmute-video-extract-audio)
    ("vx" "Remove Audio" transmute-video-remove-audio)
    ("vm" "Replace Audio" transmute-video-replace-audio)
    ("vB" "Background Music" transmute-video-background-music)]
   ["Audio"
    ("ac" "Convert" transmute-audio-convert)
    ("an" "Normalise" transmute-audio-normalise)
    ("at" "Trim Silence" transmute-audio-trim-silence)
    ("ai" "Info" transmute-audio-info)
    ("vi" "Video Info" transmute-video-info)]]
  ["Metadata & Global"
   ["Tags"
    ("tt" "Tag (Interactive)" transmute-tag-interactive)
    ("tk" "Tag from List" transmute-tag-from-list)
    ("tr" "Add Tags & Rename" transmute-tag-and-rename)
    ("tR" "Rename from Tags" transmute-picture-tag-rename)
    ("td" "Retag by Date" transmute-retag-by-date)
    ("tc" "Clear Tags" transmute-clear-tags)
    ("ti" "Tag Info" transmute-tag-info)]
   ["Utilities"
    ("m" "Completing Read Menu" transmute-completing-read-menu)
    ("L" "Show Log" transmute-show-log)
    ("S" "Stop Conversions" transmute-stop-conversions)]])

;;;###autoload
(transmute-progress-mode 1)

(defun transmute--image-dired-kill-buffer-query ()
  "Allow killing *image-dired-display-image* buffers without confirmation.
Transmute modifies image files externally (rotate, tag, etc.) which
marks the display buffer as modified.  image-dired-display-image calls
kill-buffer on it when switching images, triggering a \"Buffer modified;
kill anyway?\" prompt.  This handler silently clears the modified flag
so the kill proceeds without prompting.  Always returns t to allow the kill."
  (when (and (string-match-p "\\`\\*image-dired-display-image\\*"
                              (buffer-name))
             (buffer-modified-p))
    (set-buffer-modified-p nil))
  t)

(defun transmute--clean-orphaned-locks (files &optional directory)
  "Delete orphaned Emacs file-lock symlinks for FILES or DIRECTORY.
If DIRECTORY is provided, scan it for all orphaned media locks
matching the current user's PID pattern."
  (let ((user-pattern (format "%s@%s" (user-login-name) (system-name))))
    (if directory
        (let ((lock-files (directory-files directory t "\\`\\.#" t))
              (media-exts (append transmute-image-extensions 
                                  transmute-video-extensions 
                                  transmute-audio-extensions)))
          (dolist (lock lock-files)
            (let* ((base (substring (file-name-nondirectory lock) 2))
                   (ext (file-name-extension base)))
              (when (and (member (downcase (or ext "")) media-exts)
                         (let ((target (file-symlink-p lock)))
                           (and target (string-prefix-p user-pattern target))))
                (delete-file lock)))))
      ;; Individual files mode
      (dolist (f files)
        (let* ((f (expand-file-name f))
               (dir (file-name-directory f))
               (base (file-name-nondirectory f))
               (lock (expand-file-name (concat ".#" base) dir)))
          (when (file-symlink-p lock)
            (delete-file lock)))))))

(defun transmute-refresh-thumbnail ()
  "Refresh dired and thumbnail buffers after a transmute batch operation.
Handles renamed files by updating their visiting buffers and reverts
affected buffers silently to prevent file-supersession prompts.
Clears modified flags and orphaned lock files immediately to avoid
\"Buffer modified; kill anyway?\" prompts; defers cache refresh."
  (let ((files (mapcar #'expand-file-name transmute-batch-files))
        (renames transmute--last-renames)
        (inhibit-display transmute--inhibit-display-refresh-once))

    ;; Clear the flag immediately
    (setq transmute--inhibit-display-refresh-once nil)

    ;; -- Immediate cleanup: must happen before user can interact --

    ;; 1. Clear modified flag on image-dired display buffers so they
    ;;    can be reused without "kill anyway?" prompts.
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf)
                 (string-match-p "\\`\\*image-dired-display-image\\*"
                                 (buffer-name buf)))
        (with-current-buffer buf
          (set-buffer-modified-p nil)
          (unlock-buffer))))

    ;; -- Deferred refresh: cache/thumb/dired can wait --
    (run-at-time 0.5 nil
      (lambda ()
        (clear-image-cache)
        (when (fboundp 'dired-image-thumbnail-clear-preview-cache)
          (dired-image-thumbnail-clear-preview-cache))

        ;; 3. Delete associated thumbnails to force regeneration
        (dolist (f files)
          (when (and (boundp 'image-dired-dir)
                     (fboundp 'image-dired-thumb-name))
            (let ((thumb (image-dired-thumb-name f)))
              (when (file-exists-p thumb)
                (delete-file thumb)))))

        ;; 4. Synchronize all buffers visiting affected files
        ;;    Skip image-dired display buffers — clearing their modified
        ;;    flag is enough; refreshing them is handled by image-dired itself.
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (unless (string-match-p "\\`\\*image-dired-display-image\\*"
                                    (buffer-name buf))
              (let ((bfn (and buffer-file-name (expand-file-name buffer-file-name))))
                (when bfn
                  (let ((new-name (cdr (assoc bfn renames))))
                    (cond
                     (new-name
                      (set-buffer-modified-p nil)
                      (set-visited-file-name new-name nil t)
                      (unlock-buffer)
                      (revert-buffer nil t))
                     ((member bfn files)
                      (set-buffer-modified-p nil)
                      (unlock-buffer)
                      (revert-buffer nil t))))))
              (when (derived-mode-p 'dired-mode)
                (revert-buffer nil t)))))

        ;; 5. Refresh the thumbnail view
        (when (fboundp 'dired-image-thumbnail-invalidate-files)
          (dired-image-thumbnail-invalidate-files files))
        (if (fboundp 'dired-image-thumbnail-refresh-all)
            (dired-image-thumbnail-refresh-all renames)
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (derived-mode-p 'image-dired-thumbnail-mode)
                (when (and (boundp 'dired-image-thumbnail--dimension-cache)
                           dired-image-thumbnail--dimension-cache)
                  (clrhash dired-image-thumbnail--dimension-cache))
                (when (fboundp 'dired-image-thumbnail-refresh)
                  (dired-image-thumbnail-refresh))))))

        ;; 6. Finally, refresh the full-size display if active
        (when (and (not inhibit-display)
                   (fboundp 'dired-image-thumbnail-refresh-current-display))
          (dired-image-thumbnail-refresh-current-display))

        ;; 7. FINAL SWEEP: Clean orphaned Emacs file-lock symlinks
        (transmute--clean-orphaned-locks files)
        (transmute--clean-orphaned-locks (mapcar #'cdr renames))
        (when-let ((batch-dir (and (car files) (file-name-directory (car files)))))
          (transmute--clean-orphaned-locks nil batch-dir))
        (transmute--log "[FINISH] Batch refresh complete.")))))

(add-hook 'transmute-after-batch-hook #'transmute-refresh-thumbnail)

(add-hook 'kill-buffer-query-functions
          #'transmute--image-dired-kill-buffer-query)

;;;###autoload
(defun transmute-setup-thumbnail-keys ()
  "Bind `transmute-menu' to C-c M in `image-dired-thumbnail-mode-map'.
Call this in your init file after loading both packages."
  (when (boundp 'image-dired-thumbnail-mode-map)
    (define-key image-dired-thumbnail-mode-map (kbd "C-c M") #'transmute-menu)))

(provide 'transmute)

;;; transmute.el ends here
