;;; test-niwa-change-buffer.el --- Visiting buffer rollback -*- lexical-binding: t; -*-

;;; Commentary:

;; Exercise the checked transaction with real visited-file timestamps.

;;; Code:

(require 'niwa-change)
(require 'niwa-pin)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-change-rollback/visited-modtime
  ()
  (let ((path (make-temp-file "niwa-change-buffer-" nil nil "original")) (current ,current) buffer)
    (unwind-protect
        (progn
          (set-file-times path (seconds-to-time 1000000))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (unless current (set-file-times path (seconds-to-time 2000000)))
            (should (eq current (verify-visited-file-modtime buffer)))
            (let ((receipt (niwa-change-commit
                            (list (list :path path :action 'write :content "replacement")))))
              (should (equal "replacement" (buffer-string)))
              (niwa-change-rollback receipt))
            (should (equal "original" (buffer-string)))
            (should (eq current (verify-visited-file-modtime buffer)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-file path)))
  (current)
  :doc "a current buffer stays current after rollback rewrites its file"
  t
  :doc "a previously stale buffer stays stale"
  nil)

(niwa-deftest niwa-change-snapshot/pinned-mode
  (:doc "bytes and mode describe the pinned file, not the current name")
  (let* ((dir (make-temp-file "niwa-change-pair-" t))
         (path (file-name-concat dir "current"))
         (pinned (file-name-concat dir "original")))
    (unwind-protect
        (progn
          (should (equal (niwa-change-snapshot path)
                         (list :path path :exists nil :bytes nil :mode nil)))
          (write-region "first" nil path nil 'silent)
          (set-file-modes path #o600)
          (rename-file path pinned)
          (write-region "second" nil path nil 'silent)
          (set-file-modes path #o700)
          ;; Model the name and the open file as distinct, without a timing race.
          (cl-letf (((symbol-function 'niwa-pin-open-path) (lambda (_) pinned)))
            (should (equal (niwa-change-snapshot path)
                           (list :path path :exists t :bytes "first" :mode #o600)))))
      (delete-directory dir t))))

(provide 'test-niwa-change-buffer)
;;; test-niwa-change-buffer.el ends here
