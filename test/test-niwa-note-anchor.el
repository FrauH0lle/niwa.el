;;; test-niwa-note-anchor.el --- Note anchor preservation tests -*- lexical-binding: t; -*-

;;; Commentary:

;; Resolve anchors captured from real notes against edited source text.

;;; Code:

(require 'bytecomp)
(require 'niwa-note-source)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-instruction-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-instruction-test-support"))

(defun niwa-note-anchor-test--capture (source end)
  "Capture a real note spanning 2..END in SOURCE."
  (with-temp-buffer
    (insert source)
    (let ((note (niwa-instruction-test--create-note-in
                 (current-buffer) 2 end 'char)))
      (niwa-note-anchor-capture note))))

(niwa-deftest niwa-note-anchor-resolve
  (:vars ((niwa--instruction-states (make-hash-table :test #'equal))
          (niwa--instruction-current-state-key :global)
          (niwa-instruction-anchor-context-chars 1)))
  ,test
  (test)
  :doc "keeps unchanged bounds and relocates unique text after an insertion"
  (let ((anchor (niwa-note-anchor-test--capture "LcatR" 5)))
    (should (equal 1 (plist-get anchor :schema)))
    (should (equal "cat" (plist-get anchor :text)))
    (with-temp-buffer
      (insert "LcatR")
      (should (equal '(2 . 5) (niwa-note-anchor-resolve anchor 2 5)))
      (goto-char (point-min))
      (insert "above\n")
      (should (equal '(8 . 11) (niwa-note-anchor-resolve anchor 2 5)))))
  :doc "duplicates require unique context or a confining parent range"
  (let ((anchor (niwa-note-anchor-test--capture "LcatR" 5)))
    (dolist (case '(("xxLcat!LcatR" nil (9 . 12))
                    ("xx!catRLcatR" nil (9 . 12))
                    ("xxLcatRLcatR" nil nil)
                    ("xxLcatRLcatR" (8 . 13) (9 . 12))
                    ("xxLcatRLcatR" (1 . 5) nil)))
      (pcase-let ((`(,source ,range ,expected) case))
        (with-temp-buffer
          (insert source)
          (should (equal expected (niwa-note-anchor-resolve anchor 2 5 range)))))))
  :doc "bodyless notes relocate by their surrounding text unless ambiguous"
  (let ((anchor (niwa-note-anchor-test--capture "LR" 2)))
    (should (plist-get anchor :bodyless))
    (with-temp-buffer
      (insert "xxLR")
      (should (equal '(4 . 4) (niwa-note-anchor-resolve anchor 2 2)))
      (insert "LR")
      (should-not (niwa-note-anchor-resolve anchor 2 2))))
  :doc "over-limit text accepts hash alone or context alone, but cannot relocate"
  (let* ((niwa-instruction-anchor-text-max-chars 2)
         (anchor (niwa-note-anchor-test--capture "LcatR" 5)))
    (should-not (plist-get anchor :text))
    (should (equal (secure-hash 'sha256 "cat") (plist-get anchor :text-hash)))
    (dolist (case '(("!cat?" (2 . 5)) ("LdogR" (2 . 5)) ("xxLcatR" nil)))
      (with-temp-buffer
        (insert (car case))
        (should (equal (cadr case) (niwa-note-anchor-resolve anchor 2 5))))))
  :doc "missing anchors pass through; trusted positions obey buffer and parent bounds"
  (let ((anchor (niwa-note-anchor-test--capture "LcatR" 5)))
    (with-temp-buffer
      (insert "xxLcatR")
      (should (equal '(2 . 5) (niwa-note-anchor-resolve nil 2 5 '(3 . 8))))
      (should (equal '(2 . 5) (niwa-note-anchor-resolve anchor 2 5 '(2 . 5) t)))
      (should-not (niwa-note-anchor-resolve anchor 2 5 '(3 . 8) t))
      (should-not (niwa-note-anchor-resolve nil 2 9 nil t)))))

(niwa-deftest niwa-note-anchor-resolve/cold-load ()
  ,test
  (test)
  :doc "the compiled source owner resolves without the persistence store"
  (let* ((root (file-name-directory (locate-library "niwa-note-source")))
         (dir (make-temp-file "niwa-note-anchor-" t))
         (source (file-name-concat dir "niwa-note-source.el"))
         (compiled (byte-compile-dest-file source)))
    (unwind-protect
        (progn
          (copy-file (file-name-concat root "niwa-note-source.el") source)
          (should (byte-compile-file source))
          (with-temp-buffer
            (let ((status
                   (call-process
                    (expand-file-name invocation-name invocation-directory)
                    nil t nil "-Q" "--batch" "-L" root "-l" compiled "--eval"
                    (prin1-to-string
                     '(with-temp-buffer
                        (insert "xxLcatR")
                        (unless (equal '(4 . 7)
                                       (niwa-note-anchor-resolve
                                        '(:schema 1 :text "cat") 2 5))
                          (error "Compiled owner failed to relocate text"))
                        (when (featurep 'niwa-persistence)
                          (error "Resolving loaded persistence")))))))
              (ert-info ((buffer-string)) (should (equal 0 status))))))
      (delete-directory dir t))))

(provide 'test-niwa-note-anchor)
;;; test-niwa-note-anchor.el ends here
