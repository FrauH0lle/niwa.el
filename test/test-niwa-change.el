;;; test-niwa-change.el --- Checked change engine regressions -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise checked changes, visiting buffers, and rollback through public verbs.

;;; Code:

(require 'niwa-change)
(require 'niwa-pin)
(require 'niwa-instruction-edit)
(require 'niwa-journal-claim)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(niwa-deftest niwa-change-prepare
  ()
  (let* ((directory (make-temp-file "change-prepare-" t))
         (path (file-name-concat directory "source")))
    (unwind-protect
        (cl-labels ((snapshot (text)
                      (write-region text nil path nil 'silent)
                      (list :path path :exists t
                            :bytes (with-temp-buffer
                                     (insert-file-contents-literally path)
                                     (buffer-string)))))
          ,test)
      (delete-directory directory t)))
  (test)
  :doc "identical replacements already applied retain disjoint edits"
  (let* ((before (snapshot "one\ntwo\nthree\nfour\n"))
         (after (snapshot "ONE\ntwo\nthree\nfour\n"))
         (current (snapshot "one\ntwo\nthree\nuser\n"))
         (prepared (niwa-change-prepare before after current t)))
    (should-not (plist-get prepared :conflicts))
    (should (equal "one\ntwo\nthree\nuser\n" (plist-get (plist-get prepared :after) :bytes))))
  :doc "identical insertions already applied are not inserted twice"
  (let* ((before (snapshot "one\ntwo\nthree\nfour\n"))
         (after (snapshot "one\ninserted\ntwo\nthree\nfour\n"))
         (current (snapshot "one\ninserted\ntwo\nthree\nuser\n"))
         (prepared (niwa-change-prepare before after current)))
    (should-not (plist-get prepared :conflicts))
    (should (equal "one\ninserted\ntwo\nthree\nuser\n" (plist-get (plist-get prepared :after) :bytes))))
  :doc "different overlapping replacements still refuse the whole change"
  (let* ((before (snapshot "one\ntwo\nthree\nfour\n"))
         (after (snapshot "ONE\ntwo\nthree\nfour\n"))
         (current (snapshot "mine\ntwo\nthree\nuser\n"))
         (prepared (niwa-change-prepare before after current)))
    (should (plist-get prepared :conflicts))
    (should-not (plist-get prepared :after))))

(niwa-deftest niwa-change-commit ()
  ,test
  (test)
  :doc "a single ordered batch still synchronizes buffers and rolls back a failed synchronization"
  (let* ((root (make-temp-file "niwa-patch-batch-" t))
         (paths (mapcar (lambda (leaf) (file-name-concat root leaf)) '("a" "b")))
         (changes (mapcar (lambda (path) (list :action 'write :path path :content "new")) paths))
         (writer (lambda (_before after)
                   (niwa-pin-write-file-atomically
                    (plist-get after :path) (plist-get after :bytes)
                    'no-conversion (plist-get after :mode))))
         (calls 0)
         (batch (lambda (before after)
                  (should (equal paths (mapcar (lambda (row) (plist-get row :path)) before)))
                  (should (equal paths (mapcar (lambda (row) (plist-get row :path)) after)))
                  (cl-incf calls)
                  (cl-mapc writer before after)))
         buffer)
    (unwind-protect
        (progn
          (dolist (path paths) (write-region "old" nil path nil 'silent))
          (setq buffer (find-file-noselect (car paths)))
          (cl-letf (((symbol-function 'niwa-instruction-replace-buffer-contents)
                     (lambda (&rest _) (error "Injected sync failure"))))
            (should-error (niwa-change-commit changes :mutate writer :batch batch)))
          (should (= 1 calls))
          (dolist (path paths) (should (equal "old" (plist-get (niwa-change-snapshot path) :bytes))))
          (niwa-change-commit changes :mutate writer :batch batch)
          (should (= 2 calls))
          (should (equal "new" (with-current-buffer buffer (buffer-string)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory root t)))

  :doc "Rolls back files and newly-created directories after an I/O failure"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-patch-rollback-" t)))
         (first (file-name-concat root "created" "first.txt"))
         (blocker (file-name-concat root "blocker"))
         (second (file-name-concat blocker "second.txt")))
    (unwind-protect
        (progn
          (with-temp-file blocker (insert "not a directory"))
          (should-error
           (niwa-change-commit
            (list (list :action 'write :path first :content "first\n")
                  (list :action 'write :path second :content "second\n")))
           :type 'error)
          (should-not (file-exists-p first))
          (should-not (file-exists-p (file-name-directory first)))
          (should (file-regular-p blocker)))
      (when (file-directory-p root) (delete-directory root t))))

  :doc "Restores non-default-coded bytes after a later write failure"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-patch-coding-rollback-" t)))
         (path (file-name-concat root "latin-1.el"))
         (blocker (file-name-concat root "blocker"))
         (failure (file-name-concat blocker "failure.el"))
         (original
          ";; -*- coding: iso-latin-1 -*-\nold: \u00e4\nkeep: \u00f6\n"))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'iso-latin-1))
            (with-temp-file path (insert original)))
          (with-temp-file blocker (insert "not a directory"))
          (should-error
           (niwa-change-commit
            (list (list :action 'write :path path :bytes
                        (encode-coding-string "replacement\n" 'iso-latin-1))
                  (list :action 'write :path failure :content "fail\n"))))
          (should
           (equal (encode-coding-string original 'iso-latin-1)
                  (plist-get (niwa-change-snapshot path) :bytes))))
      (when (file-directory-p root) (delete-directory root t))))

  :doc "Restores a deleted file's literal bytes and mode after a later failure"
  (let* ((root (make-temp-file "niwa-patch-deleted-rollback-" t))
         (path (file-name-concat root "binary.dat"))
         (blocker (file-name-concat root "blocker"))
         (failure (file-name-concat blocker "failure.txt"))
         (bytes (unibyte-string 0 255 13 10 128)))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'no-conversion))
            (write-region bytes nil path nil 'silent))
          (set-file-modes path #o751)
          (with-temp-file blocker (insert "not a directory"))
          (should-error
           (niwa-change-commit
            (list (list :action 'delete :path path)
                  (list :action 'write :path failure :content "fail\n"))))
          (should (equal bytes (plist-get (niwa-change-snapshot path) :bytes)))
          (should (= #o751 (file-modes path))))
      (delete-directory root t)))

  :doc "Synchronizes a buffer visiting the same file through another name"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-patch-sync-" t)))
         (target (file-name-concat root "target.txt"))
         (alias (file-name-concat root "alias.txt"))
         buffer)
    (unwind-protect
        (progn
          (with-temp-file target (insert "old\n"))
          (make-symbolic-link target alias)
          (setq buffer (find-file-noselect alias))
          (niwa-change-commit
           (list (list :action 'write :path target :content "new\n")))
          (should (equal "new\n" (with-current-buffer buffer
                                    (buffer-string))))
          (should (verify-visited-file-modtime buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p root) (delete-directory root t))))

  :doc "Preserves intervening disk edits when buffer synchronization fails"
  (let* ((root (make-temp-file "niwa-patch-intervening-" t))
         (first (file-name-concat root "one.txt"))
         (second (file-name-concat root "two.txt")) buffer)
    (unwind-protect
        (progn
          (with-temp-file first (insert "old one\n"))
          (with-temp-file second (insert "old two\n"))
          (setq buffer (find-file-noselect second))
          (with-current-buffer buffer
            (add-hook 'before-change-functions
                      (lambda (&rest _)
                        (write-region "intervening edit\n" nil first nil 'silent)
                        (error "Sync failure after external edit")) nil t))
          (should-error
           (niwa-change-commit
            (list (list :action 'write :path first :content "new one\n")
                  (list :action 'write :path second :content "new two\n")))
           :type 'niwa-change-partial-rollback)
          (should (equal "intervening edit\n" (niwa-test--read-file first)))
          (should (equal "old two\n" (niwa-test--read-file second))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t)))

  :doc "Expected before-state rejects changed bytes and unexpected creation before any write"
  (let* ((root (make-temp-file "niwa-patch-expected-" t))
         (first (file-name-concat root "one.txt"))
         (second (file-name-concat root "two.txt")))
    (unwind-protect
        (progn
          (with-temp-file first (insert "old one\n"))
          (let ((before (niwa-change-snapshot first)))
            (write-region "changed\n" nil first nil 'silent)
            (should-error (niwa-change-commit
                           (list (list :action 'write :path second :content "new two\n")
                                 (list :action 'write :path first :content "new one\n" :before before))))
            (should-not (file-exists-p second))
            (should (equal "changed\n" (niwa-test--read-file first))))
          (should-error (niwa-change-commit
                         (list (list :action 'write :path first :content "new one\n"
                                     :before (list :path first :exists nil)))))
          (should (equal "changed\n" (niwa-test--read-file first))))
      (delete-directory root t)))

  :doc "Lost target ownership prevents rollback from undoing a successor's files"
  (let* ((root (make-temp-file "niwa-patch-owner-" t))
         (first (file-name-concat root "one.txt"))
         (second (file-name-concat root "two.txt"))
         (key (file-name-concat root "claims"))
         (claim (niwa-journal-claim-acquire key 180))
         successor buffer)
    (unwind-protect
        (progn
          (with-temp-file first (insert "old one\n"))
          (with-temp-file second (insert "old two\n"))
          (setq buffer (find-file-noselect first))
          (with-current-buffer buffer
            (add-hook 'before-change-functions
                      (lambda (&rest _)
                        (niwa-journal-claim-settle claim 'cancelled "")
                        (setq successor (niwa-journal-claim-acquire key 180))
                        (write-region "successor edit\n" nil second nil 'silent)) nil t))
          (should-error
           (niwa-change-commit
            (list (list :action 'write :path first :content "new one\n")
                  (list :action 'write :path second :content "new two\n"))
            :currentp (lambda () (and (equal claim (niwa-journal-claim-current key))
                             (not (niwa-journal-claim-outcome claim)))))
           :type 'niwa-change-partial-rollback)
          (should successor)
          (should-not (niwa-journal-claim-outcome successor))
          (should (equal "new one\n" (niwa-test--read-file first)))
          (should (equal "successor edit\n" (niwa-test--read-file second))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when successor (niwa-journal-claim-settle successor 'cancelled ""))
      (delete-directory root t)))

  :doc "Preserves an intervening unsaved buffer edit when another synchronization fails"
  (let* ((root (make-temp-file "niwa-patch-buffer-intervening-" t))
         (first (file-name-concat root "one.txt"))
         (second (file-name-concat root "two.txt")) first-buffer second-buffer)
    (unwind-protect
        (progn
          (with-temp-file first (insert "old one\n"))
          (with-temp-file second (insert "old two\n"))
          (setq first-buffer (find-file-noselect first) second-buffer (find-file-noselect second))
          (with-current-buffer second-buffer
            (add-hook 'before-change-functions
                      (lambda (&rest _)
                        (with-current-buffer first-buffer
                          (goto-char (point-max)) (insert "user draft\n"))
                        (error "Sync failure with a draft")) nil t))
          (should-error
           (niwa-change-commit
            (list (list :action 'write :path first :content "new one\n")
                  (list :action 'write :path second :content "new two\n")))
           :type 'niwa-change-partial-rollback)
          (with-current-buffer first-buffer
            (should (equal "new one\nuser draft\n" (buffer-string)))
            (should (buffer-modified-p)))
          (should (equal "old one\n" (niwa-test--read-file first)))
          (should (equal "old two\n" (niwa-test--read-file second))))
      (dolist (buffer (list first-buffer second-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t)))

  :doc "A quit during buffer synchronization rolls back before propagating cancellation"
  (let* ((root (make-temp-file "niwa-patch-quit-" t))
         (path (file-name-concat root "one.txt")) buffer cancelled)
    (unwind-protect
        (progn
          (with-temp-file path (insert "old\n"))
          (setq buffer (find-file-noselect path))
          (with-current-buffer buffer
            (add-hook 'before-change-functions (lambda (&rest _) (signal 'quit nil)) nil t))
          (condition-case nil
              (niwa-change-commit (list (list :action 'write :path path :content "new\n")))
            (quit (setq cancelled t)))
          (should cancelled)
          (should (equal "old\n" (niwa-test--read-file path)))
          (should (equal "old\n" (with-current-buffer buffer (buffer-string)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (delete-directory root t)))

  :doc "Reports an incomplete rollback with the original failure and path"
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-patch-partial-rollback-" t)))
         (first-directory (file-name-concat root "first"))
         (second-directory (file-name-concat root "second"))
         (first (file-name-concat first-directory "one.txt"))
         (second (file-name-concat second-directory "two.txt"))
         first-buffer
         second-buffer)
    (unwind-protect
        (progn
          (make-directory first-directory)
          (make-directory second-directory)
          (with-temp-file first (insert "old one\n"))
          (with-temp-file second (insert "old two\n"))
          (setq first-buffer (find-file-noselect first)
                second-buffer (find-file-noselect second))
          (with-current-buffer second-buffer
            (add-hook 'before-change-functions
                      (lambda (&rest _)
                        (set-file-modes first-directory #o500)
                        (error "Sync failure"))
                      nil t))
          (let* ((failure
                  (should-error
                   (niwa-change-commit
                    (list (list :action 'write :path first
                                :content "new one\n")
                          (list :action 'write :path second
                                :content "new two\n")))
                   :type 'niwa-change-partial-rollback))
                 (message (error-message-string failure)))
            (should (string-match-p "Sync failure" message))
            (should (string-match-p (regexp-quote first) message)))
          (should (equal "new one\n"
                         (niwa-test--read-file first)))
          (should (equal "old two\n"
                         (niwa-test--read-file second))))
      (when (file-directory-p first-directory)
        (set-file-modes first-directory #o700))
      (dolist (buffer (list first-buffer second-buffer))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (when (file-directory-p root) (delete-directory root t)))))

(niwa-deftest niwa-change-missing-parent-directories
  (:doc "Lists absent parent directories below an existing root") ,test (test)
  (let* ((root (make-temp-file "niwa-patch-parents-" t))
         (path (file-name-concat root "a" "b" "x")))
    (unwind-protect
        (should (= 2 (length
                      (niwa-change-missing-parent-directories path))))
      (delete-directory root t))))

(niwa-deftest niwa-change-rollback
  (:doc "A committed deletion restores literal bytes and executable mode") ,test (test)
  (let ((path (make-temp-file "change-restore-"))
        (bytes (unibyte-string 255 0 128)))
    (unwind-protect
        (progn
          (niwa-change-commit (list (list :path path :action 'write :bytes bytes :mode #o750)))
          (let ((before (niwa-change-snapshot path)))
            (should (equal bytes (plist-get before :bytes)))
            (should (= #o750 (plist-get before :mode)))
            (let ((receipt (niwa-change-commit (list (list :path path :action 'delete)))))
              (should-not (file-exists-p path))
              (should-not (niwa-change-rollback receipt))
              (should (niwa-change-same-p before (niwa-change-snapshot path))))))
      (when (file-exists-p path) (delete-file path))))

  :doc "Rolling back a byte-only write restores the visiting buffer and disk"
  (let ((path (make-temp-file "change-visited-" nil nil "old\n")) buffer)
    (unwind-protect
        (progn
          (setq buffer (find-file-noselect path))
          (let ((receipt (niwa-change-commit (list (list :action 'write :path path :bytes "new\n")))))
            (should (equal "new\n" (with-current-buffer buffer (buffer-string))))
            (should-not (niwa-change-rollback receipt))
            (should (equal "old\n" (niwa-test--read-file path)))
            (should (equal "old\n" (with-current-buffer buffer (buffer-string))))
            (should-not (buffer-modified-p buffer))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-file path)))

  :doc "Rollback lists every intervening edit in snapshot order and restores other paths"
  (let* ((root (make-temp-file "change-rollback-" t))
         (paths (mapcar (lambda (leaf) (file-name-concat root leaf)) '("a" "b" "c"))))
    (unwind-protect
        (let ((receipt (niwa-change-commit
                        (mapcar (lambda (path) (list :path path :action 'write :content "new"))
                                paths))))
          (dolist (path (cdr paths)) (with-temp-file path (insert "intervening")))
          (let ((failure (should-error (niwa-change-rollback receipt)
                                       :type 'niwa-change-partial-rollback)))
            (should (equal (cdr paths) (nth 2 failure))))
          (should-not (file-exists-p (car paths)))
          (dolist (path (cdr paths)) (should (equal "intervening" (niwa-test--read-file path)))))
      (delete-directory root t)))

  :doc "Rollback leaves a directory it did not create, and reports one replaced since"
  (dolist (case '(foreign replaced))
    (let* ((root (make-temp-file "change-directories-" t))
           (directory (file-name-concat root "new"))
           (path (file-name-concat directory "file"))
           (write (symbol-function 'niwa-pin-write-file-atomically)))
      (unwind-protect
          (let ((receipt
                 (cl-letf (((symbol-function 'niwa-pin-write-file-atomically)
                            (lambda (file &rest args)
                              ;; Another writer creates the predicted parent first.
                              (when (eq case 'foreign) (make-directory directory))
                              (apply write file args))))
                   (niwa-change-commit (list (list :path path :action 'write :content "x"))))))
            (when (eq case 'replaced)
              (rename-file directory (file-name-concat root "moved"))
              (make-directory directory)
              (copy-file (file-name-concat root "moved" "file") path))
            (if (eq case 'foreign)
                (should-not (niwa-change-rollback receipt))
              (should (equal (list directory)
                             (nth 2 (should-error (niwa-change-rollback receipt)
                                                  :type 'niwa-change-partial-rollback)))))
            (should-not (file-exists-p path))
            (should (file-directory-p directory)))
        (delete-directory root t))))

  :doc "Rollback never removes a created directory through a parent swapped for a link"
  (let* ((root (make-temp-file "change-swapped-parent-" t))
         (parent (file-name-concat root "p"))
         (child (file-name-concat parent "q"))
         (moved (file-name-concat root "moved")))
    (unwind-protect
        (let ((receipt (niwa-change-commit
                        (list (list :path (file-name-concat child "f") :action 'write :content "x")))))
          (delete-file (file-name-concat child "f"))
          ;; The same inode is now reached only through a symlinked parent.
          (rename-file parent moved)
          (make-symbolic-link moved parent)
          (should (equal (list child parent)
                         (nth 2 (should-error (niwa-change-rollback receipt)
                                              :type 'niwa-change-partial-rollback))))
          (should (file-directory-p (file-name-concat moved "q"))))
      (delete-directory root t)))

  :doc "A write that fails after creating its parents leaves no directory behind"
  (let* ((root (make-temp-file "change-failed-parent-" t))
         (directory (file-name-concat root "new"))
         ;; The parent and temporary file are made; the rename then fails.
         (path (file-name-concat directory (make-string 300 ?x))))
    (unwind-protect
        (progn
          (should-error (niwa-change-commit (list (list :path path :action 'write :content "x"))))
          (should-not (file-exists-p directory)))
      (delete-directory root t)))

  :doc "A failed write whose parent cleanup also fails names the parent it left"
  (let* ((root (make-temp-file "change-failed-cleanup-" t))
         (directory (file-name-concat root "new"))
         (path (file-name-concat directory (make-string 300 ?x)))
         (real niwa-pin--script)
         ;; A dedicated helper whose rmdir always fails, so its own cleanup does.
         (wrapper (make-temp-file "niwa-pin-deny-rmdir-" nil ".py"
                                  (format "import os, runpy\ndef deny(*args, **kwargs):\n    raise PermissionError(13, 'Injected cleanup failure')\nos.rmdir = deny\nrunpy.run_path(%S, run_name='__main__')\n"
                                          real))))
    (let ((niwa-pin--script wrapper)
          (niwa-pin--helpers nil))
      (unwind-protect
          (let ((failure (should-error
                          (niwa-change-commit (list (list :path path :action 'write :content "x")))
                          :type 'niwa-change-partial-rollback)))
            ;; The write's own error stays the cause; the left parent is named.
            (should (string-match-p "too long" (error-message-string (nth 1 failure))))
            (should (member directory (nth 2 failure)))
            (should (file-directory-p directory)))
        (dolist (entry niwa-pin--helpers)
          (when (process-live-p (cdr entry)) (delete-process (cdr entry))))
        (delete-file wrapper)
        (delete-directory root t))))

  :doc "KEEP-LINKS refuses to delete a link planted at a deleted or rolled-back path"
  (dolist (step '(delete rollback control))
    (let* ((root (make-temp-file "change-delete-link-" t))
           (path (file-name-concat root "file"))
           (outside (make-temp-file "change-outside-" nil nil "outside\n"))
           (delete (symbol-function 'niwa-pin-delete-file))
           planted)
      (unwind-protect
          (progn
            (unless (eq step 'rollback) (write-region "old\n" nil path nil 'silent))
            (let ((receipt (and (eq step 'rollback)
                                (niwa-change-commit (list (list :path path :action 'write :content "new\n"))
                                                    :keep-links t))))
              (cl-letf (((symbol-function 'niwa-pin-delete-file)
                         (lambda (file &rest args)
                           ;; Past the commit's recheck of this path.
                           (delete-file file)
                           (make-symbolic-link outside file)
                           (setq planted t)
                           (apply delete file args))))
                (pcase step
                  ('delete (should-error (niwa-change-commit (list (list :path path :action 'delete))
                                                             :keep-links t)))
                  ('rollback (should-error (niwa-change-rollback receipt)
                                           :type 'niwa-change-partial-rollback))
                  ('control (niwa-change-commit (list (list :path path :action 'delete)))))))
            (should planted)
            (should (eq (not (eq step 'control)) (and (file-symlink-p path) t)))
            (should (equal "outside\n" (niwa-test--read-file outside))))
        (delete-file outside)
        (delete-directory root t))))

  :doc "A buffer or change group that cannot be restored is named; the rest recovers"
  (dolist (step '(restore cancel))
    (let* ((root (make-temp-file "change-buffer-restore-" t))
           (paths (mapcar (lambda (leaf) (file-name-concat root leaf)) '("a" "b")))
           (restore (symbol-function 'niwa-change--restore-buffer))
           (cancel (symbol-function 'cancel-change-group))
           buffers broken)
      (unwind-protect
          (progn
            (dolist (path paths) (write-region "old\n" nil path nil 'silent))
            (setq buffers (mapcar #'find-file-noselect paths))
            (let ((changes (mapcar (lambda (path) (list :path path :action 'write :content "new\n"))
                                   paths)))
              (cl-letf (((symbol-function 'niwa-change--restore-buffer)
                         (lambda (snapshot)
                           (if (and (eq step 'restore) (eq (plist-get snapshot :buffer) (car buffers)))
                               (progn (setq broken t) (error "Injected buffer restore failure"))
                             (funcall restore snapshot))))
                        ((symbol-function 'cancel-change-group)
                         (lambda (group)
                           (if (and (eq step 'cancel) (eq (current-buffer) (car buffers)))
                               (progn (setq broken t) (error "Injected cancel failure"))
                             (funcall cancel group)))))
                (let ((failure
                       (should-error
                        (if (eq step 'restore)
                            (niwa-change-rollback (niwa-change-commit changes))
                          ;; The second buffer's synchronization fails inside the commit.
                          (with-current-buffer (cadr buffers)
                            (add-hook 'before-change-functions
                                      (lambda (&rest _) (error "Injected sync failure")) nil t))
                          (niwa-change-commit changes))
                        :type 'niwa-change-partial-rollback)))
                  (should broken)
                  (should (equal (list (car paths)) (nth 2 failure))))))
            (dolist (path paths) (should (equal "old\n" (niwa-test--read-file path))))
            (should (equal "old\n" (with-current-buffer (cadr buffers) (buffer-string)))))
        (dolist (buffer buffers)
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-buffer-modified-p nil))
            (kill-buffer buffer)))
        (delete-directory root t)))))

(niwa-deftest niwa-change-commit/currentp
  (:doc "A stale owner cannot create a file; a current owner can")
  (let* ((root (make-temp-file "change-owner-" t))
         (path (file-name-concat root "file"))
         (changes (list (list :path path :action 'write :content "x"))))
    (unwind-protect
        (progn
          (should-error (niwa-change-commit changes :currentp (lambda () nil)))
          (should-not (file-exists-p path))
          (niwa-change-commit changes :currentp (lambda () t))
          (should (equal "x" (niwa-test--read-file path))))
      (delete-directory root t))))

(niwa-deftest niwa-change-map-extents
  (:doc "Several extents map together; an insertion touches both edges of its gap")
  (should (equal (niwa-change-map-extents "a\nb\nc\nd\n" "a\nb\nx\nc\nd\n"
                                        '((0 . 1) (1 . 2) (1 . 3) (2 . 3) (3 . 4)))
                 '((:start 0 :end 1 :touched nil)
                   (:start 1 :end 3 :touched ((2 . 3)))
                   (:start 1 :end 4 :touched ((2 . 3)))
                   (:start 3 :end 4 :touched ((2 . 3)))
                   (:start 4 :end 5 :touched nil)))))

(niwa-deftest niwa-change-map-extents-multiple-runs
  (:doc "one diff maps insertions before, inside, after and at both edges")
  (let ((calls 0) (call (symbol-function 'call-process)))
    (cl-letf (((symbol-function 'call-process)
               (lambda (program &rest args)
                 (when (equal program "diff") (cl-incf calls))
                 (apply call program args))))
      (should (equal (niwa-change-map-extents
                      "a\nb\nc\nd\ne\n" "X\na\nb\nY\nc\nd\ne\nZ\n"
                      '((1 . 4) (0 . 2) (2 . 5)))
                     '((:start 2 :end 6 :touched ((3 . 4)))
                       (:start 1 :end 4 :touched ((0 . 1) (3 . 4)))
                       (:start 4 :end 8 :touched ((3 . 4) (7 . 8)))))))
    (should (= 1 calls))))

(niwa-deftest niwa-change-text
  (:doc "A Latin-1 cookie decodes the captured bytes without rereading the path")
  (let ((path (make-temp-file "change-text-" nil ".el"))
        (text ";; -*- coding: iso-latin-1 -*-\n;; ä\n"))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'iso-latin-1))
            (write-region text nil path nil 'silent))
          (let ((snapshot (niwa-change-snapshot path))
                (expected (with-temp-buffer
                            (insert-file-contents path)
                            (cons (buffer-string) last-coding-system-used))))
            (with-temp-file path (insert "other"))
            (should (equal text (car expected)))
            (should (equal expected (niwa-change-text snapshot)))
            (delete-file path)
            (should (equal expected (niwa-change-text snapshot)))))
      (when (file-exists-p path) (delete-file path)))))

(provide 'test-niwa-change)
;;; test-niwa-change.el ends here
