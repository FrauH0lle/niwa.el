;;; niwa-view-thumbnail.el --- Private inline image thumbnails -*- lexical-binding: t -*-

;;; Commentary:

;; Shrinks large local raster previews (PNG, JPEG, WebP) once, off the
;; interactive path, so
;; redisplay scales a small source instead of a full-resolution one.
;;
;; Derivatives live in a session-private directory under XDG_RUNTIME_DIR
;; (directories 0700, files 0600, named by digest only) and are deleted when
;; the session ends.  ImageMagick's `magick' runs asynchronously from argv,
;; one job at a time, reading only the sniffed format's coder and writing
;; PNG, with delegates disabled and resource limits set.  Until a thumbnail
;; exists, and whenever the source is remote, of an unknown, unavailable or
;; multi-frame format, larger than `max-image-size' allows, or anything
;; fails, the caller keeps its ordinary full-size preview.
;;
;; Emacs's native JPEG loader ignores EXIF orientation, so thumbnails do too
;; (no -auto-orient): the preview keeps today's orientation and size.

;;; Code:

(require 'cl-lib)
(require 'text-property-search)

(defvar niwa--session)
(defvar niwa--data-buffer)
(defvar niwa-session-end-hook)
(declare-function niwa-view-render-mutate "niwa-view-render"
                  (key function &optional replacement cleanup))

(defconst niwa-view-thumbnail--formats
  '((png "PNG" "\\`\211PNG\r\n\032\n")
    (jpeg "JPEG" "\\`\377\330\377")
    (webp "WEBP" "\\`RIFF\\(?:.\\|\n\\)\\{4\\}WEBP"))
  ;; GIF is excluded: a safe frame count needs a whole-file scan, and
  ;; offset frames need canvas compositing.  GIFs keep today's preview.
  "Thumbnailed raster formats as (TYPE CODER SIGNATURE).
TYPE is the Emacs image type, CODER the ImageMagick coder and SIGNATURE a
regexp matching the file's first bytes.")

(defun niwa-view-thumbnail--policy ()
  "Return the ImageMagick policy: table coders only, no delegates."
  (concat "<policymap>
  <policy domain=\"delegate\" rights=\"none\" pattern=\"*\"/>
  <policy domain=\"coder\" rights=\"none\" pattern=\"*\"/>
"
          (mapconcat (lambda (format)
                       (format "  <policy domain=\"coder\" rights=\"%s\" pattern=\"%s\"/>\n"
                               (if (eq 'png (car format)) "read|write" "read")
                               (nth 1 format)))
                     niwa-view-thumbnail--formats "")
          "  <policy domain=\"path\" rights=\"none\" pattern=\"@*\"/>
</policymap>
"))

(defconst niwa-view-thumbnail--header-bytes 65536
  "Bytes read to sniff a source and find its dimensions.")

(defconst niwa-view-thumbnail--limits
  '("-limit" "memory" "256MiB" "-limit" "map" "512MiB" "-limit" "disk" "1GiB"
    "-limit" "area" "128MP" "-limit" "time" "30")
  "ImageMagick resource limits for one conversion.")

(defconst niwa-view-thumbnail--max-pixels 128000000
  "Largest source area converted, matching the `area' limit.")

(defconst niwa-view-thumbnail--max-queue 16
  "Most conversions waiting at once; further requests keep the original.")

(defconst niwa-view-thumbnail--max-entries 256
  "Most thumbnails one session keeps.")

(defconst niwa-view-thumbnail--timeout 30
  "Seconds before a conversion is killed.")

(defvar niwa-view-thumbnail--converter 'unknown
  "`magick' as (PROGRAM . VERSION), nil when absent, or `unknown'.")

(defvar niwa-view-thumbnail--coders 'unknown
  "Table coders `magick' can read, or `unknown' until its listing arrives.")

(defvar niwa-view-thumbnail--root nil
  "This Emacs's private thumbnail root, holding the policy and session dirs.")

(defvar niwa-view-thumbnail--sessions (make-hash-table :test #'eq)
  "Map of session to (DIRECTORY . THUMBNAIL-COUNT).")

(defvar niwa-view-thumbnail--jobs (make-hash-table :test #'equal)
  "Map of output file to its queued or running job, coalescing requests.")

(defvar niwa-view-thumbnail--queue nil
  "Jobs waiting to run, oldest first.")

(defvar niwa-view-thumbnail--process nil
  "The running conversion, or nil.")

(defvar niwa-view-thumbnail--misses (make-hash-table :test #'equal)
  "Output files whose unchanged source the converter rejected.
Keys sit in a session directory and encode source and converter identity,
so changed metadata or a new converter misses this table.")

(defconst niwa-view-thumbnail--max-misses 256
  "Most remembered rejections; a full table starts over.")

(cl-defstruct (niwa-view-thumbnail--job
               (:constructor niwa-view-thumbnail--job-create)
               (:copier nil))
  file source cap session views timer)

(defun niwa-view-thumbnail--converter ()
  "Return `magick' as (PROGRAM . VERSION), or nil when unavailable."
  (when (eq niwa-view-thumbnail--converter 'unknown)
    (setq niwa-view-thumbnail--converter
          (let ((default-directory "/"))
            (when-let* ((program (executable-find "magick"))
                        (truename (file-truename program))
                        (attributes (file-attributes truename 'integer)))
              (niwa-view-thumbnail--probe-coders program)
              ;; The binary's identity versions the output without a subprocess.
              (cons program
                    (list truename (file-attribute-size attributes)
                          (file-attribute-modification-time attributes)))))))
  niwa-view-thumbnail--converter)

(defun niwa-view-thumbnail--probe-coders (program)
  "Record which table coders PROGRAM can read, asynchronously.
Until the listing arrives every coder is tried; a failed job only misses."
  (let ((default-directory "/")
        (output nil))
    (condition-case nil
        (make-process
         :name "niwa-thumbnail-formats" :buffer nil :noquery t
         :connection-type 'pipe :command (list program "-list" "format")
         :filter (lambda (_process string) (push string output))
         :sentinel
         (lambda (process _event)
           (unless (process-live-p process)
             (let ((listing (apply #'concat (nreverse output))))
               (setq niwa-view-thumbnail--coders
                     (cl-loop for (_ coder) in niwa-view-thumbnail--formats
                              when (string-match-p
                                    (format "^ *%s\\*? +%s +r" coder coder)
                                    listing)
                              collect coder))))))
      (error (setq niwa-view-thumbnail--coders nil)))))

(defun niwa-view-thumbnail--private-directory-p (directory)
  "Return non-nil when DIRECTORY is ours and closed to other users."
  (let ((attributes (file-attributes directory 'integer)))
    (and (eq t (file-attribute-type attributes))
         (eql (user-uid) (file-attribute-user-id attributes))
         (zerop (logand #o077 (file-modes directory 'nofollow))))))

(defun niwa-view-thumbnail--root ()
  "Return this Emacs's private thumbnail root, or nil when none is safe."
  (unless (and niwa-view-thumbnail--root
               (file-directory-p niwa-view-thumbnail--root))
    (clrhash niwa-view-thumbnail--sessions)
    (setq niwa-view-thumbnail--root
          (when-let* ((runtime (getenv "XDG_RUNTIME_DIR"))
                      ((file-name-absolute-p runtime))
                      ((niwa-view-thumbnail--private-directory-p runtime)))
            (with-file-modes #o700
              (let ((root (make-temp-file
                           (file-name-concat runtime "niwa-thumbnails-") t)))
                (with-file-modes #o600
                  (write-region (niwa-view-thumbnail--policy) nil
                                (file-name-concat root "policy.xml")
                                nil 'silent))
                root)))))
  niwa-view-thumbnail--root)

(defun niwa-view-thumbnail--session-entry (session)
  "Return SESSION's (DIRECTORY . COUNT), creating its directory.
COUNT counts admitted conversions, finished or not.  Creating the entry
ties its cleanup to the owning data buffer: inspections and side chats
never run `niwa-session-end-hook'."
  (when-let* ((root (niwa-view-thumbnail--root)))
    (or (gethash session niwa-view-thumbnail--sessions)
        (let ((owner (bound-and-true-p niwa--data-buffer)))
          (when (buffer-live-p owner)
            (with-current-buffer owner
              (add-hook 'kill-buffer-hook
                        #'niwa-view-thumbnail--end-session nil t)))
          (puthash session
                   (cons (with-file-modes #o700
                           (make-temp-file (file-name-concat root "s") t))
                         0)
                   niwa-view-thumbnail--sessions)))))

(defun niwa-view-thumbnail--read (file bytes)
  "Return the first BYTES of FILE as a unibyte string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file nil 0 bytes)
    (buffer-string)))

(defun niwa-view-thumbnail--uint (bytes offset count &optional little)
  "Return the COUNT-byte unsigned integer at OFFSET in BYTES.
Big-endian unless LITTLE."
  (let ((value 0))
    (dotimes (i count)
      (setq value (logior value
                          (ash (aref bytes (+ offset i))
                               (* 8 (if little i (- count i 1)))))))
    value))

(defun niwa-view-thumbnail--dimensions (type bytes)
  "Return (WIDTH . HEIGHT) of a single-frame TYPE image from BYTES, or nil."
  (cl-flet ((be (offset count) (niwa-view-thumbnail--uint bytes offset count))
            (le (offset count) (niwa-view-thumbnail--uint bytes offset count t)))
    (pcase type
      ('png (and (equal "IHDR" (substring bytes 12 16))
                 (cons (be 16 4) (be 20 4))))
      ('webp (pcase (substring bytes 12 16)
               ("VP8 " (cons (logand (le 26 2) #x3fff) (logand (le 28 2) #x3fff)))
               ("VP8L" (let ((bits (le 21 4)))
                         (cons (1+ (logand bits #x3fff))
                               (1+ (logand (ash bits -14) #x3fff)))))
               ;; Bit 1 of the VP8X flags marks an animation.
               ("VP8X" (and (zerop (logand (aref bytes 20) 2))
                            (cons (1+ (le 24 3)) (1+ (le 27 3)))))))
      ('jpeg
       ;; Walk marker segments to the first start-of-frame.
       (let ((pos 2) dimensions)
         (while (not dimensions)
           (unless (= #xff (aref bytes pos)) (error "Malformed JPEG"))
           (let ((marker (aref bytes (1+ pos))))
             (cond ((= marker #xff) (cl-incf pos))
                   ((or (= marker 1) (<= #xd0 marker #xd7)) (cl-incf pos 2))
                   ((and (<= #xc0 marker #xcf) (not (memq marker '(#xc4 #xc8 #xcc))))
                    (setq dimensions (cons (be (+ pos 7) 2) (be (+ pos 5) 2))))
                   (t (cl-incf pos (+ 2 (be (+ pos 2) 2)))))))
         dimensions)))))

(defun niwa-view-thumbnail--source (file)
  "Return FILE as (TRUENAME SIZE MTIME WIDTH HEIGHT FORMAT), or nil.
FORMAT is the `niwa-view-thumbnail--formats' entry its first bytes match.
Nil for an unknown, malformed or multi-frame source."
  (when-let* ((truename (file-truename file))
              ;; A FIFO or device named *.png must never be opened here.
              ((file-regular-p truename))
              (attributes (file-attributes truename 'integer))
              (size (file-attribute-size attributes))
              (bytes (niwa-view-thumbnail--read
                      truename (min size niwa-view-thumbnail--header-bytes)))
              (format (seq-find (lambda (format)
                                  (string-match-p (nth 2 format) bytes))
                                niwa-view-thumbnail--formats))
              ;; shortcut: a JPEG frame header past the first 64 KiB, or any
              ;; truncated header, misses; read further if real files need it.
              (dimensions (ignore-errors
                            (niwa-view-thumbnail--dimensions (car format) bytes))))
    (list truename size (file-attribute-modification-time attributes)
          (car dimensions) (cdr dimensions) format)))

(defun niwa-view-thumbnail--size-allowed-p (width height frame)
  "Return non-nil when Emacs would load a WIDTH by HEIGHT image on FRAME.
Mirrors `check_image_size' in image.c, which guards the original."
  (let ((limit max-image-size))
    (cond ((integerp limit) (and (<= width limit) (<= height limit)))
          ((floatp limit) (and (<= width (* limit (frame-pixel-width frame)))
                               (<= height (* limit (frame-pixel-height frame)))))
          (t t))))

(defun niwa-view-thumbnail--scale (frame)
  "Return the default image `:scale' Emacs applies on FRAME.
Mirrors `compute_image_size' in image.c."
  (if (numberp image-scaling-factor)
      image-scaling-factor
    (let ((width (frame-char-width frame)))
      (if (> width 10) (/ width 10.0) 1))))

(defun niwa-view-thumbnail--key (source cap converter)
  "Return the cache key of SOURCE at width CAP made by CONVERTER."
  (secure-hash 'sha256 (prin1-to-string
                        (list (butlast source 3) cap (cdr converter)))))

(defun niwa-view-thumbnail-image (path max-width)
  "Return a thumbnail image spec showing PATH as its MAX-WIDTH preview does.
Return nil to keep the ordinary preview.  When PATH is eligible but its
thumbnail is missing, queue it for the current view buffer."
  (when-let* ((session (bound-and-true-p niwa--session))
              ((not (file-remote-p path)))
              (converter (niwa-view-thumbnail--converter))
              (source (niwa-view-thumbnail--source path))
              (format (nth 5 source))
              ((image-type-available-p (car format)))
              ((or (eq niwa-view-thumbnail--coders 'unknown)
                   (member (nth 1 format) niwa-view-thumbnail--coders)))
              (width (nth 3 source))
              (height (nth 4 source))
              ;; One compact size serves nearby preview widths.
              (cap (max 256 (expt 2 (ceiling (log max-width 2)))))
              ((< cap width))
              ((<= (* width height) niwa-view-thumbnail--max-pixels))
              (frame (window-frame (or (get-buffer-window nil t)
                                       (selected-window))))
              ((< max-width (* width (niwa-view-thumbnail--scale frame))))
              ((niwa-view-thumbnail--size-allowed-p width height frame))
              (entry (niwa-view-thumbnail--session-entry session)))
    (let ((file (file-name-concat
                 (car entry)
                 (concat (niwa-view-thumbnail--key source cap converter)
                         ".png"))))
      (if (file-exists-p file)
          ;; The original shrinks to MAX-WIDTH and a ceiled proportional
          ;; height (`compute_image_size'); pin that size exactly.
          (create-image file 'png nil
                        :width max-width
                        :height (ceiling (/ (* max-width (float height)) width))
                        :scale 1)
        ;; A source the converter already rejected keeps its original.
        (unless (gethash file niwa-view-thumbnail--misses)
          (niwa-view-thumbnail--request file source cap session entry path))
        nil))))

(defun niwa-view-thumbnail--request (file source cap session entry path)
  "Queue FILE from SOURCE at CAP for SESSION's ENTRY, shown as PATH here."
  (let ((view (cons (current-buffer) path))
        (job (gethash file niwa-view-thumbnail--jobs)))
    (cond
     (job (cl-pushnew view (niwa-view-thumbnail--job-views job)
                      :test #'equal))
     ((or (<= niwa-view-thumbnail--max-entries (cdr entry))
          (<= niwa-view-thumbnail--max-queue
              (length niwa-view-thumbnail--queue))))
     (t
      ;; Reserve the slot now; queued jobs must not overshoot the cap.
      (cl-incf (cdr entry))
      (setq job (niwa-view-thumbnail--job-create
                 :file file :source source :cap cap :session session
                 :views (list view)))
      (puthash file job niwa-view-thumbnail--jobs)
      (setq niwa-view-thumbnail--queue
            (nconc niwa-view-thumbnail--queue (list job)))
      (niwa-view-thumbnail--start)))))

(defun niwa-view-thumbnail--start ()
  "Start the next queued conversion unless one is running."
  (while (and niwa-view-thumbnail--queue
              (not (process-live-p niwa-view-thumbnail--process)))
    (let* ((job (pop niwa-view-thumbnail--queue))
           (file (niwa-view-thumbnail--job-file job))
           (directory (file-name-directory file))
           (default-directory directory)
           (process-environment
            (append (list (concat "MAGICK_CONFIGURE_PATH="
                                  niwa-view-thumbnail--root)
                          (concat "MAGICK_TEMPORARY_PATH=" directory)
                          (concat "TMPDIR=" directory))
                    process-environment)))
      (let (process)
        (condition-case nil
            (with-file-modes #o600
              (progn
                (setq process
                      (make-process
                       :name "niwa-thumbnail" :buffer nil :noquery t
                       :connection-type 'pipe :filter #'ignore
                       :sentinel #'niwa-view-thumbnail--sentinel
                       :command
                       `(,(car niwa-view-thumbnail--converter)
                         ,@niwa-view-thumbnail--limits
                         ,(let ((source (niwa-view-thumbnail--job-source job)))
                            (concat (nth 1 (nth 5 source)) ":" (car source)))
                         "-strip" "-define" "png:exclude-chunks=date,time"
                         "-thumbnail"
                         ,(format "%dx" (niwa-view-thumbnail--job-cap job))
                         ,(concat "PNG:" file ".part"))))
                (process-put process 'niwa-view-thumbnail-job job)
                (setf (niwa-view-thumbnail--job-timer job)
                      (run-at-time niwa-view-thumbnail--timeout nil
                                   #'delete-process process))
                (setq niwa-view-thumbnail--process process)))
          (error
           (if process
               ;; Its sentinel returns the reserved slot.
               (delete-process process)
             (setq niwa-view-thumbnail--converter 'unknown)
             (remhash file niwa-view-thumbnail--jobs)
             (when-let* ((entry (gethash (niwa-view-thumbnail--job-session job)
                                         niwa-view-thumbnail--sessions)))
               (cl-decf (cdr entry))))))))))

(defun niwa-view-thumbnail--sentinel (process _event)
  "Commit PROCESS's thumbnail if its source and session are unchanged."
  (unless (process-live-p process)
    (let* ((job (process-get process 'niwa-view-thumbnail-job))
           (file (niwa-view-thumbnail--job-file job))
           (part (concat file ".part"))
           (entry (gethash (niwa-view-thumbnail--job-session job)
                           niwa-view-thumbnail--sessions)))
      (when (niwa-view-thumbnail--job-timer job)
        (cancel-timer (niwa-view-thumbnail--job-timer job)))
      (when (eq process niwa-view-thumbnail--process)
        (setq niwa-view-thumbnail--process nil))
      (when (eq job (gethash file niwa-view-thumbnail--jobs))
        (remhash file niwa-view-thumbnail--jobs))
      (let (committed)
        (unwind-protect
            (progn
              ;; A failing recheck or publication is a cache miss.
              (setq committed
                    (ignore-errors
                      (and (eq (process-status process) 'exit)
                           (zerop (process-exit-status process))
                           entry
                           (string-prefix-p (car entry) file)
                           (file-regular-p part)
                           (equal (niwa-view-thumbnail--job-source job)
                                  (niwa-view-thumbnail--source
                                   (car (niwa-view-thumbnail--job-source job))))
                           (progn (set-file-modes part #o600)
                                  (rename-file part file t)
                                  t))))
              (when committed
                (pcase-dolist (`(,buffer . ,path)
                               (niwa-view-thumbnail--job-views job))
                  (when (buffer-live-p buffer)
                    ;; A failed swap leaves that view's full-size preview.
                    (ignore-errors
                      (with-current-buffer buffer
                        (niwa-view-thumbnail--refresh path)))))))
          (unless committed
            ;; Return the reserved slot; a miss may be retried.
            (when entry (cl-decf (cdr entry)))
            ;; Only a decode failure of the unchanged source is final;
            ;; kills, launch and publication failures stay retryable.
            (when (and entry
                       (eq (process-status process) 'exit)
                       (/= 0 (process-exit-status process))
                       (ignore-errors
                         (equal (niwa-view-thumbnail--job-source job)
                                (niwa-view-thumbnail--source
                                 (car (niwa-view-thumbnail--job-source job))))))
              (when (<= niwa-view-thumbnail--max-misses
                        (hash-table-count niwa-view-thumbnail--misses))
                (clrhash niwa-view-thumbnail--misses))
              (puthash file t niwa-view-thumbnail--misses))
            (ignore-errors (delete-file part)))
          (niwa-view-thumbnail--start))))))

(defun niwa-view-thumbnail--refresh (path)
  "Show thumbnails for this view's still-current full-size PATH previews."
  (niwa-view-render-mutate
   (list 'image-thumbnail path)
   (lambda ()
     (let ((inhibit-read-only t)
           (buffer-undo-list t)
           (modified (buffer-modified-p)))
       (unwind-protect
           (save-excursion
             (goto-char (point-min))
             (let (match)
               (while (setq match
                            (text-property-search-forward
                             'display path
                             (lambda (path spec)
                               (and (eq 'image (car-safe spec))
                                    (equal path (plist-get (cdr spec) :file))
                                    (plist-get (cdr spec) :max-width)))))
                 (when-let* ((image (ignore-errors
                                      (niwa-view-thumbnail-image
                                       path (plist-get (cdr (prop-match-value match))
                                                       :max-width)))))
                   ;; Same displayed size: no reflow, point or window motion.
                   (put-text-property (prop-match-beginning match)
                                      (prop-match-end match)
                                      'display image)))))
         (restore-buffer-modified-p modified))))))

(defun niwa-view-thumbnail--end-session ()
  "Delete the ending session's thumbnails and drop its conversions.
Runs from `niwa-session-end-hook' and from the owning data buffer's
`kill-buffer-hook'; the second call finds nothing left to do."
  (when-let* ((session (bound-and-true-p niwa--session))
              (entry (gethash session niwa-view-thumbnail--sessions)))
    (remhash session niwa-view-thumbnail--sessions)
    (setq niwa-view-thumbnail--queue
          (cl-remove session niwa-view-thumbnail--queue
                     :key #'niwa-view-thumbnail--job-session))
    (maphash (lambda (file job)
               (when (eq session (niwa-view-thumbnail--job-session job))
                 (remhash file niwa-view-thumbnail--jobs)))
             niwa-view-thumbnail--jobs)
    (maphash (lambda (file _)
               (when (string-prefix-p (car entry) file)
                 (remhash file niwa-view-thumbnail--misses)))
             niwa-view-thumbnail--misses)
    (when-let* ((process niwa-view-thumbnail--process)
                ((eq session (niwa-view-thumbnail--job-session
                              (process-get process 'niwa-view-thumbnail-job)))))
      (delete-process process))
    (delete-directory (car entry) t)))

(defun niwa-view-thumbnail--delete-root ()
  "Delete this Emacs's thumbnail root."
  (when niwa-view-thumbnail--root
    (ignore-errors (delete-directory niwa-view-thumbnail--root t))))

(add-hook 'niwa-session-end-hook #'niwa-view-thumbnail--end-session)
;; Side chats change major mode after the first preview can render.
(put 'niwa-view-thumbnail--end-session 'permanent-local-hook t)
(add-hook 'kill-emacs-hook #'niwa-view-thumbnail--delete-root)

(provide 'niwa-view-thumbnail)

;;; niwa-view-thumbnail.el ends here
