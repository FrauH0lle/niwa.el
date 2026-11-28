;;; test-niwa-view-thumbnail.el --- Inline image thumbnail tests -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-markdown)
(require 'niwa-view-thumbnail)
(require 'niwa-chat)
(require 'niwa-workspace)
(require 'niwa-reminders)
(require 'niwa-presets)
(require 'niwa-session-artifacts)
(require 'niwa-skills-core)
(require 'niwa-skills-prompt)
(require 'niwa-permission-persistence)
(require 'niwa-plugin-ui)

(defmacro niwa-thumbnail-test--with-state (&rest body)
  "Run BODY with fresh thumbnail state under a private runtime directory.
Binds `runtime', a scratch `dir' for sources and a `session'."
  (declare (indent 0) (debug t))
  `(let* ((session (niwa-session--create :name "thumbnail-test"))
          (runtime (make-temp-file "niwa-thumb-runtime-" t))
          (dir (make-temp-file "niwa-thumb-src-" t))
          (process-environment (cons (concat "XDG_RUNTIME_DIR=" runtime)
                                     process-environment))
          (niwa-view-thumbnail--converter 'unknown)
          (niwa-view-thumbnail--coders 'unknown)
          (niwa-view-thumbnail--root nil)
          (niwa-view-thumbnail--sessions (make-hash-table :test #'eq))
          (niwa-view-thumbnail--jobs (make-hash-table :test #'equal))
          (niwa-view-thumbnail--queue nil)
          (niwa-view-thumbnail--process nil)
          (niwa-view-thumbnail--misses (make-hash-table :test #'equal))
          (max-image-size nil)
          (image-scaling-factor 1))
     (set-file-modes runtime #o700)
     (unwind-protect (progn ,@body)
       (when (process-live-p niwa-view-thumbnail--process)
         (delete-process niwa-view-thumbnail--process))
       (delete-directory runtime t)
       (delete-directory dir t))))

(defun niwa-thumbnail-test--header-png (file width height)
  "Write FILE as a PNG signature and IHDR claiming WIDTH by HEIGHT."
  (let ((coding-system-for-write 'binary))
    (with-temp-file file
      (set-buffer-multibyte nil)
      (insert "\211PNG\r\n\032\n\0\0\0\rIHDR")
      (dolist (n (list width height))
        (insert (logand (ash n -24) 255) (logand (ash n -16) 255)
                (logand (ash n -8) 255) (logand n 255)))
      (insert "\b\2\0\0\0")))
  file)

(defun niwa-thumbnail-test--real-png (file width height &optional coder)
  "Make FILE a real WIDTH by HEIGHT image whose comment names FILE.
CODER is the ImageMagick output coder, PNG by default."
  (should (zerop (call-process "magick" nil nil nil
                               "-size" (format "%dx%d" width height)
                               "gradient:red-blue" "-set" "comment" file
                               (concat (or coder "PNG") ":" file))))
  file)

(defun niwa-thumbnail-test--wait ()
  "Wait for queued and running conversions, at most ten seconds."
  (with-timeout (10 (error "Thumbnail conversion did not finish"))
    (while (or niwa-view-thumbnail--queue
               (process-live-p niwa-view-thumbnail--process))
      (accept-process-output nil 0.05))
    ;; Let the sentinel of the last exited process run.
    (accept-process-output nil 0.05)))

(defun niwa-thumbnail-test--insert-preview (path max-width)
  "Insert a full-size preview of PATH at MAX-WIDTH; return its start."
  (let ((start (point))
        (inhibit-read-only t))
    (insert path)
    (put-text-property start (point) 'display
                       (create-image path nil nil :max-width max-width))
    (insert "\n")
    start))

(niwa-deftest niwa-view-thumbnail--key ()
  ,test
  (test)
  :doc "source size, mtime, cap and converter each invalidate the key"
  (niwa-thumbnail-test--with-state
    (let* ((file (niwa-thumbnail-test--header-png
                  (file-name-concat dir "a.png") 4000 2000))
           (converter '("magick" "/usr/bin/magick" 1 (1 2 3 4)))
           (key (lambda () (niwa-view-thumbnail--key
                            (niwa-view-thumbnail--source file) 512 converter)))
           (first (funcall key)))
      (should (equal '(4000 2000) (take 2 (nthcdr 3 (niwa-view-thumbnail--source file)))))
      (should (equal first (funcall key)))
      (set-file-times file '(1000 0 0 1000))
      (let ((touched (funcall key)))
        (should-not (equal first touched))
        ;; A nanosecond-only change is still a new key.
        (set-file-times file '(1000 0 0 2000))
        (should-not (equal touched (funcall key)))
        (setq touched (funcall key))
        (write-region "x" nil file t 'silent)
        (set-file-times file '(1000 0 0 2000))
        (should-not (equal touched (funcall key)))))
    (let ((source (niwa-view-thumbnail--source
                   (file-name-concat dir "a.png"))))
      (should-not (equal (niwa-view-thumbnail--key source 512 '("m" 1))
                         (niwa-view-thumbnail--key source 256 '("m" 1))))
      (should-not (equal (niwa-view-thumbnail--key source 512 '("m" 1))
                         (niwa-view-thumbnail--key source 512 '("m" 2)))))))

(niwa-deftest niwa-view-thumbnail-image ()
  ,test
  (test)
  :doc "ineligible sources keep today's display and queue nothing"
  (niwa-thumbnail-test--with-state
    (let ((large (niwa-thumbnail-test--header-png
                  (file-name-concat dir "large.png") 4000 2000))
          (small (niwa-thumbnail-test--header-png
                  (file-name-concat dir "small.png") 400 200))
          (fake (file-name-concat dir "fake.png"))
          started)
      (write-region "not a png at all, just text bytes" nil fake nil 'silent)
      (with-temp-buffer
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'niwa-view-thumbnail--start)
                   (lambda () (setq started t)))
                  ((symbol-function 'executable-find)
                   (lambda (&rest _) nil)))
          ;; No converter.
          (should-not (niwa-view-thumbnail-image large 300))
          (should-not niwa-view-thumbnail--converter))
        (setq niwa-view-thumbnail--converter '("magick" . version))
        (cl-letf (((symbol-function 'niwa-view-thumbnail--start)
                   (lambda () (setq started t)))
                  ((symbol-function 'file-truename)
                   (lambda (file &rest _)
                     (when (file-remote-p file)
                       (error "Remote I/O for %s" file))
                     file)))
          (should-not (niwa-view-thumbnail-image "/ssh:nohost:/x.png" 300))
          (should-not (niwa-view-thumbnail-image small 300))
          (should-not (niwa-view-thumbnail-image fake 300))
          ;; The original would be rejected, so is its thumbnail.
          (let ((max-image-size 3000))
            (should-not (niwa-view-thumbnail-image large 300)))
          (let ((max-image-size 10.0))
            (cl-letf (((symbol-function 'frame-pixel-width) (lambda (&rest _) 300))
                      ((symbol-function 'frame-pixel-height) (lambda (&rest _) 3000)))
              (should-not (niwa-view-thumbnail-image large 300))))
          (should-not started)
          (should (zerop (hash-table-count niwa-view-thumbnail--jobs)))
          ;; Outside a session nothing is cached either.
          (let ((niwa--session nil))
            (should-not (niwa-view-thumbnail-image large 300)))
          (should-not started)
          ;; An eligible source queues one coalesced job.
          (should-not (niwa-view-thumbnail-image large 300))
          (should-not (niwa-view-thumbnail-image large 280))
          (should started)
          (should (= 1 (hash-table-count niwa-view-thumbnail--jobs)))
          (should (= 1 (length niwa-view-thumbnail--queue)))))))

  :doc "a rejected original keeps its ordinary guarded preview spec"
  (niwa-thumbnail-test--with-state
    (let ((large (niwa-thumbnail-test--header-png
                  (file-name-concat dir "large.png") 4000 2000))
          (max-image-size 3000))
      (setq niwa-view-thumbnail--converter '("magick" . version))
      (with-temp-buffer
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'display-images-p) (lambda (&rest _) t))
                  ((symbol-function 'niwa-view--path-target) (lambda (_) t)))
          (let ((spec (niwa-view--image-display large 300)))
            (should (equal large (plist-get (cdr spec) :file)))
            (should (eql 300 (plist-get (cdr spec) :max-width)))))
        (should (zerop (hash-table-count niwa-view-thumbnail--jobs))))))

  :doc "a generated thumbnail is private, stripped, and shown at today's size"
  (skip-unless (executable-find "magick"))
  (niwa-thumbnail-test--with-state
    (let ((source (niwa-thumbnail-test--real-png
                   (file-name-concat dir "secret-name.png") 2001 1003)))
      (niwa-view-test--with-buffers
	(set-buffer view-buf)
        (setq-local niwa--session session)
        (let ((start (progn (goto-char (point-min))
                            (niwa-thumbnail-test--insert-preview source 300)))
              (point (progn (goto-char 3) (point))))
          (set-buffer-modified-p nil)
          (should-not (niwa-view-thumbnail-image source 300))
          (niwa-thumbnail-test--wait)
          (let* ((spec (cdr (get-text-property start 'display)))
                 (thumbnail (plist-get spec :file))
                 (session-dir (file-name-directory thumbnail))
                 (root niwa-view-thumbnail--root))
            (should (string-prefix-p runtime root))
            (should (string-match-p "\\`[0-9a-f]\\{64\\}\\.png\\'"
                                    (file-name-nondirectory thumbnail)))
            ;; Original 2001x1003 shrinks to 300 x ceil(300*1003/2001) = 151.
            (should (equal '(300 151 1)
                           (list (plist-get spec :width) (plist-get spec :height)
                                 (plist-get spec :scale))))
            (should (equal '(512 257)
                           (take 2 (nthcdr 3 (niwa-view-thumbnail--source thumbnail)))))
            (should (= #o700 (file-modes root)))
            (should (= #o700 (file-modes session-dir)))
            (should (= #o600 (file-modes thumbnail)))
            (should (= #o600 (file-modes (file-name-concat root "policy.xml"))))
            (should (equal (list (file-name-nondirectory thumbnail))
                           (directory-files session-dir nil "\\`[^.]")))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally thumbnail)
              (dolist (leak (list "secret-name" dir "date:" "tEXt" "zTXt" "iTXt"))
                (goto-char (point-min))
                (should-not (search-forward leak nil t))))
            (should (= point (point)))
            (should-not (buffer-modified-p))
            ;; A later render finds the thumbnail without new work.
            (should (equal thumbnail
                           (plist-get (cdr (niwa-view-thumbnail-image source 300))
                                      :file)))
            (should-not niwa-view-thumbnail--queue))))))

  :doc "stale jobs leave killed buffers, changed regions and changed sources alone"
  (skip-unless (executable-find "magick"))
  (niwa-thumbnail-test--with-state
    (let ((source (niwa-thumbnail-test--real-png
                   (file-name-concat dir "a.png") 1600 800))
          (killed (generate-new-buffer " thumb-killed")))
      (niwa-view-test--with-buffers
	(with-current-buffer killed
          (setq-local niwa--session session)
          (niwa-thumbnail-test--insert-preview source 300)
          (should-not (niwa-view-thumbnail-image source 300)))
	(set-buffer view-buf)
	(setq-local niwa--session session)
	(goto-char (point-min))
	(let ((changed (niwa-thumbnail-test--insert-preview source 300))
              (kept (niwa-thumbnail-test--insert-preview source 300)))
          (should-not (niwa-view-thumbnail-image source 300))
          (should (= 1 (hash-table-count niwa-view-thumbnail--jobs)))
          (kill-buffer killed)
          (let ((inhibit-read-only t))
            (put-text-property changed (1- kept) 'display "other"))
          (niwa-thumbnail-test--wait)
          (should (equal "other" (get-text-property changed 'display)))
          (should-not (equal source (plist-get (cdr (get-text-property kept 'display))
                                               :file)))
          ;; A source changed while converting commits nothing.
          (let* ((other (niwa-thumbnail-test--real-png
                         (file-name-concat dir "b.png") 1600 800))
                 (start (progn (goto-char (point-min))
                               (niwa-thumbnail-test--insert-preview other 300))))
            (should-not (niwa-view-thumbnail-image other 300))
            (set-file-times other '(1000 0 0 1000))
            (niwa-thumbnail-test--wait)
            (should (equal other (plist-get (cdr (get-text-property start 'display))
                                            :file)))
            (should (= 1 (length (directory-files
                                  (car (gethash session niwa-view-thumbnail--sessions))
                                  nil "\\`[^.]"))))))))))

(niwa-deftest niwa-view-thumbnail--end-session ()
  ,test
  (test)
  :doc "ending the session kills its conversion and deletes its thumbnails"
  (skip-unless (executable-find "magick"))
  (niwa-thumbnail-test--with-state
    (let ((done (niwa-thumbnail-test--real-png
                 (file-name-concat dir "done.png") 1600 800))
          (pending (niwa-thumbnail-test--real-png
                    (file-name-concat dir "pending.png") 4000 3000)))
      (niwa-view-test--with-buffers
	(set-buffer view-buf)
        (setq-local niwa--session session)
        (niwa-view-thumbnail-image done 300)
        (niwa-thumbnail-test--wait)
        (should (niwa-view-thumbnail-image done 300))
        (niwa-view-thumbnail-image pending 300)
        (let ((process niwa-view-thumbnail--process)
              (directory (car (gethash session niwa-view-thumbnail--sessions))))
          (should (process-live-p process))
          (run-hooks 'niwa-session-end-hook)
          (should-not (process-live-p process))
          (should-not (file-exists-p directory))
          (should (zerop (hash-table-count niwa-view-thumbnail--jobs)))
          (accept-process-output nil 0.05)
          (should-not (file-exists-p directory)))))))

(niwa-deftest niwa-view-thumbnail--source ()
  ,test
  (test)
  :doc "a FIFO named *.png is never opened to read its header"
  (skip-unless (executable-find "mkfifo"))
  (niwa-thumbnail-test--with-state
    (let ((fifo (file-name-concat dir "pipe.png"))
          read)
      (should (zerop (call-process "mkfifo" nil nil nil fifo)))
      (cl-letf (((symbol-function 'insert-file-contents-literally)
                 (lambda (&rest _) (setq read t) nil)))
        (should-not (niwa-view-thumbnail--source fifo)))
      (should-not read))))

(niwa-deftest niwa-view-thumbnail--sentinel ()
  ,test
  (test)
  :doc "a failing source recheck is a miss that removes the part file"
  (skip-unless (executable-find "magick"))
  (niwa-thumbnail-test--with-state
    (let* ((failing (niwa-thumbnail-test--real-png
                     (file-name-concat dir "a.png") 1600 800))
           (next (niwa-thumbnail-test--real-png
                  (file-name-concat dir "b.png") 1600 800))
           (source (symbol-function 'niwa-view-thumbnail--source)))
      (with-temp-buffer
        (setq-local niwa--session session)
        (let ((start (niwa-thumbnail-test--insert-preview failing 300)))
          (should-not (niwa-view-thumbnail-image failing 300))
          (should-not (niwa-view-thumbnail-image next 300))
          (should (= 1 (length niwa-view-thumbnail--queue)))
          (cl-letf (((symbol-function 'niwa-view-thumbnail--source)
                     (lambda (file)
                       (if (equal file (file-truename failing))
                           (error "Recheck failed")
                         (funcall source file)))))
            (niwa-thumbnail-test--wait))
          (should (equal failing (plist-get (cdr (get-text-property start 'display))
                                            :file)))
          (let ((directory (car (gethash session niwa-view-thumbnail--sessions))))
            (should-not (directory-files directory nil "\\.part\\'"))
            (should (= 1 (length (directory-files directory nil "\\.png\\'"))))
            (should (niwa-view-thumbnail-image next 300))))))))

(defun niwa-thumbnail-test--usable-p (type coder)
  "Return non-nil when Emacs shows TYPE and `magick' writes CODER here."
  (and (executable-find "magick")
       (image-type-available-p type)
       (string-match-p (format "^ *%s\\*? +%s +rw" coder coder)
                       (shell-command-to-string "magick -list format"))))

(defun niwa-thumbnail-test--generate (session file max-width)
  "Generate FILE's thumbnail at MAX-WIDTH for SESSION; return its spec."
  (with-temp-buffer
    (setq-local niwa--session session)
    (should-not (niwa-view-thumbnail-image file max-width))
    (niwa-thumbnail-test--wait)
    (niwa-view-thumbnail-image file max-width)))

(niwa-deftest niwa-view-thumbnail--formats ()
  ,test
  (test)
  :doc "each table format is sniffed from its bytes and thumbnailed at today's size"
  (dolist (format niwa-view-thumbnail--formats)
    (pcase-let ((`(,type ,coder) format))
      (ert-info ((format "%s" coder))
		(when (niwa-thumbnail-test--usable-p type coder)
		  (niwa-thumbnail-test--with-state
		    ;; The extension lies; the signature decides.
		    (let* ((file (niwa-thumbnail-test--real-png
				  (file-name-concat dir "image.png") 1600 800 coder))
			   (source (niwa-view-thumbnail--source file))
			   (spec (niwa-thumbnail-test--generate session file 300)))
		      (should (equal (list 1600 800 format)
				     (nthcdr 3 source)))
		      (should spec)
		      (should (equal '(300 150 1)
				     (list (plist-get (cdr spec) :width)
					   (plist-get (cdr spec) :height)
					   (plist-get (cdr spec) :scale))))
		      (should (equal '(512 256)
				     (take 2 (nthcdr 3 (niwa-view-thumbnail--source
							(plist-get (cdr spec) :file))))))))))))

  :doc "a GIF never reaches the dimension reader or the converter"
  (skip-unless (executable-find "magick"))
  (niwa-thumbnail-test--with-state
    (let ((file (niwa-thumbnail-test--real-png
                 (file-name-concat dir "image.png") 1600 800 "GIF"))
          entered)
      (setq niwa-view-thumbnail--converter '("magick" . version))
      (cl-letf (((symbol-function 'niwa-view-thumbnail--dimensions)
                 (lambda (&rest _) (setq entered 'dimensions) nil))
                ((symbol-function 'make-process)
                 (lambda (&rest _) (setq entered 'converter) nil)))
        (with-temp-buffer
          (setq-local niwa--session session)
          (should-not (niwa-view-thumbnail-image file 300))))
      (should-not entered)
      (should (zerop (hash-table-count niwa-view-thumbnail--jobs)))))

  :doc "multi-frame WebP sources keep today's display without a job"
  (dolist (coder '("WEBP"))
    (ert-info (coder)
	      (when (niwa-thumbnail-test--usable-p (intern (downcase coder)) coder)
		(niwa-thumbnail-test--with-state
		  (let ((file (file-name-concat dir "anim.png")))
		    (should (zerop (call-process "magick" nil nil nil "-size" "1600x800"
						 "xc:red" "xc:blue" "-loop" "0"
						 (concat coder ":" file))))
		    (should-not (niwa-view-thumbnail--source file))
		    (with-temp-buffer
		      (setq-local niwa--session session)
		      (should-not (niwa-view-thumbnail-image file 300)))
		    (should (zerop (hash-table-count niwa-view-thumbnail--jobs))))))))

  :doc "an EXIF-rotated JPEG keeps Emacs's unrotated orientation and size"
  (skip-unless (niwa-thumbnail-test--usable-p 'jpeg "JPEG"))
  (niwa-thumbnail-test--with-state
    (let ((file (niwa-thumbnail-test--real-png
                 (file-name-concat dir "photo.jpg") 1600 800 "JPEG")))
      ;; Splice an APP1 EXIF segment with Orientation 6 (rotate 90 CW).
      (let ((coding-system-for-read 'binary)
            (coding-system-for-write 'binary))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally file)
          (goto-char 3)
          (insert "\377\341\0\"Exif\0\0MM\0*\0\0\0\b\0\1"
                  "\1\22\0\3\0\0\0\1\0\6\0\0\0\0\0\0")
          (write-region nil nil file nil 'silent)))
      (should (equal "RightTop"
                     (car (process-lines "magick" "identify" "-format"
                                         "%[orientation]" file))))
      (let ((spec (niwa-thumbnail-test--generate session file 300)))
        (should (equal '(300 150) (list (plist-get (cdr spec) :width)
                                        (plist-get (cdr spec) :height))))
        (should (equal '(512 256)
                       (take 2 (nthcdr 3 (niwa-view-thumbnail--source
                                          (plist-get (cdr spec) :file)))))))))

  :doc "formats Emacs or the converter lacks keep today's display"
  (skip-unless (niwa-thumbnail-test--usable-p 'jpeg "JPEG"))
  (niwa-thumbnail-test--with-state
    (let ((file (niwa-thumbnail-test--real-png
                 (file-name-concat dir "photo.jpg") 1600 800 "JPEG")))
      (with-temp-buffer
        (setq-local niwa--session session)
        (should (niwa-view-thumbnail--converter))
        (let ((niwa-view-thumbnail--coders '("PNG")))
          (should-not (niwa-view-thumbnail-image file 300)))
        (cl-letf (((symbol-function 'image-type-available-p)
                   (lambda (type) (not (eq type 'jpeg)))))
          (should-not (niwa-view-thumbnail-image file 300))))
      (should (zerop (hash-table-count niwa-view-thumbnail--jobs))))))

(niwa-deftest niwa-view-thumbnail--policy ()
  ,test
  (test)
  :doc "the converter policy reads only table coders"
  (skip-unless (executable-find "magick"))
  (niwa-thumbnail-test--with-state
    (let ((bmp (file-name-concat dir "x.bmp"))
          (png (niwa-thumbnail-test--real-png
                (file-name-concat dir "x.png") 40 20))
          (process-environment process-environment))
      (should (zerop (call-process "magick" nil nil nil "-size" "40x20"
                                   "xc:red" (concat "BMP3:" bmp))))
      (push (concat "MAGICK_CONFIGURE_PATH=" (niwa-view-thumbnail--root))
            process-environment)
      (should (zerop (call-process "magick" nil nil nil (concat "PNG:" png)
                                   "-thumbnail" "10x"
                                   (concat "PNG:" png ".out"))))
      (should-not (zerop (call-process "magick" nil nil nil (concat "BMP:" bmp)
                                       "-thumbnail" "10x"
                                       (concat "PNG:" bmp ".out")))))))

(niwa-deftest niwa-view-thumbnail--request ()
  ,test
  (test)
  :doc "a burst with one slot left admits one conversion"
  (niwa-thumbnail-test--with-state
    (setq niwa-view-thumbnail--converter '("magick" . version))
    (with-temp-buffer
      (setq-local niwa--session session)
      (let ((files (mapcar (lambda (name)
                             (niwa-thumbnail-test--header-png
                              (file-name-concat dir name) 4000 2000))
                           '("a.png" "b.png" "c.png"))))
        (setcdr (niwa-view-thumbnail--session-entry session)
                (1- niwa-view-thumbnail--max-entries))
        ;; None finishes before the burst is admitted.
        (cl-letf (((symbol-function 'niwa-view-thumbnail--start) #'ignore))
          (dolist (file files)
            (should-not (niwa-view-thumbnail-image file 300))))
        (should (= 1 (hash-table-count niwa-view-thumbnail--jobs)))
        (should (= niwa-view-thumbnail--max-entries
                   (cdr (gethash session niwa-view-thumbnail--sessions))))))))

(niwa-deftest niwa-view-thumbnail--end-session/inspection ()
  ,test
  (test)
  :doc "closing an inspection with a conversion pending deletes its thumbnails"
  (skip-unless (executable-find "magick"))
  (niwa-thumbnail-test--with-state
    (let* ((workspace (niwa-workspace--create
                       :type 'project :id dir :root dir :name "inspect"))
           (source (niwa-thumbnail-test--real-png
                    (file-name-concat dir "big.png") 6000 4000))
           (data (generate-new-buffer " *thumbnail-inspection*"))
           session-ended)
      (unwind-protect
          (progn
            (with-current-buffer data
              (org-mode)
              (setf (niwa-session-workspace session) workspace)
              (setq-local niwa--session session)
              (cl-letf (((symbol-function 'niwa-reminders-install-defaults) #'ignore)
                        ((symbol-function 'niwa-preset--build-handlers) #'identity)
                        ((symbol-function
                          'niwa-session-artifacts-install-gptel-save-state-advice)
                         #'ignore)
                        ((symbol-function 'niwa-skills-install) #'ignore)
                        ((symbol-function 'niwa-skills-install-activation-hook)
                         #'ignore)
                        ((symbol-function 'niwa-view--ensure) #'ignore)
                        ((symbol-function 'niwa-permission-validate-persistent-stores)
                         #'ignore)
                        ((symbol-function 'niwa--run-session-start-hooks) #'ignore)
                        ((symbol-function 'niwa--probe-session-target) #'ignore)
                        ((symbol-function 'niwa-plugins-notify-pending-consent)
                         #'ignore))
                (niwa--chat-buffer-init-common data workspace "resume" t))
              (should-not (memq #'niwa--run-session-end-hooks kill-buffer-hook)))
            (with-temp-buffer
              (setq-local niwa--session session
                          niwa--data-buffer data)
              (should-not (niwa-view-thumbnail-image source 300)))
            (let ((process niwa-view-thumbnail--process)
                  (directory (car (gethash session niwa-view-thumbnail--sessions)))
                  (niwa-session-end-hook
                   (cons (lambda () (setq session-ended t)) niwa-session-end-hook))
                  (kill-buffer-query-functions nil))
              (should (process-live-p process))
              (kill-buffer data)
              (should-not session-ended)
              (should-not (process-live-p process))
              (should-not (file-exists-p directory))
              (should-not (gethash session niwa-view-thumbnail--sessions))))
        (when (buffer-live-p data)
          (let ((kill-buffer-query-functions nil)) (kill-buffer data)))))))

(niwa-deftest niwa-view-thumbnail--start ()
  ,test
  (test)
  :doc "a failed launch returns its slot, so a later conversion still runs"
  (skip-unless (executable-find "magick"))
  (niwa-thumbnail-test--with-state
    (let ((failed (niwa-thumbnail-test--real-png
                   (file-name-concat dir "a.png") 1600 800))
          (later (niwa-thumbnail-test--real-png
                  (file-name-concat dir "b.png") 1600 800)))
      (with-temp-buffer
        (setq-local niwa--session session)
        (setcdr (niwa-view-thumbnail--session-entry session)
                (1- niwa-view-thumbnail--max-entries))
        (setq niwa-view-thumbnail--converter
              (cons (file-name-concat dir "missing-magick") 'version))
        (should-not (niwa-view-thumbnail-image failed 300))
        (should-not niwa-view-thumbnail--process)
        (should (= (1- niwa-view-thumbnail--max-entries)
                   (cdr (gethash session niwa-view-thumbnail--sessions))))
        ;; The handler forgot the broken converter; the real one is found.
        (should-not (niwa-view-thumbnail-image later 300))
        (should (process-live-p niwa-view-thumbnail--process))
        (niwa-thumbnail-test--wait)
        (should (niwa-view-thumbnail-image later 300))
        (should (= niwa-view-thumbnail--max-entries
                   (cdr (gethash session niwa-view-thumbnail--sessions))))))))

(niwa-deftest niwa-view-thumbnail--sentinel/miss ()
  ,test
  (test)
  :doc "a rejected source converts once until the image is replaced"
  (skip-unless (executable-find "magick"))
  (niwa-thumbnail-test--with-state
    (let* ((file (file-name-concat dir "broken.png"))
           (make (symbol-function 'make-process))
           (conversions 0))
      ;; A valid header with no image data: magick fails to decode it.
      (niwa-thumbnail-test--header-png file 1600 800)
      (with-temp-buffer
        (setq-local niwa--session session)
        (cl-letf (((symbol-function 'make-process)
                   (lambda (&rest args)
                     (when (equal "niwa-thumbnail" (plist-get args :name))
                       (cl-incf conversions))
                     (apply make args))))
          (dotimes (_ 2)
            (should-not (niwa-view-thumbnail-image file 300))
            (niwa-thumbnail-test--wait))
          (should (= 1 conversions))
          (should (zerop (cdr (gethash session niwa-view-thumbnail--sessions))))
          (niwa-thumbnail-test--real-png file 1600 800)
          (set-file-times file '(1000 0 0 1000))
          (should-not (niwa-view-thumbnail-image file 300))
          (niwa-thumbnail-test--wait)
          (should (= 2 conversions))
          (should (niwa-view-thumbnail-image file 300)))))))

(provide 'test-niwa-view-thumbnail)

;;; test-niwa-view-thumbnail.el ends here
