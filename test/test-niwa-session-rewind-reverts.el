;;; test-niwa-session-rewind-reverts.el --- Rewind recorded changes -*- lexical-binding: t -*-

;;; Commentary:

;; Exercise recorded changes through session Rewind with real history and files.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-session-test-support"))
(require 'niwa-session-rewind)
(require 'niwa-session-rewind-merge)
(require 'niwa-session-artifacts)
(require 'niwa-pipeline)
(require 'niwa-tool-patch)
(require 'niwa-note-merge)
(require 'niwa-report)
(require 'niwa-change)
(require 'niwa-pin)
(require 'niwa-notes)
(require 'niwa-session-publication)
(require 'niwa-claude-code-fork)
(require 'niwa-chat)
(require 'niwa-execution)
;; Defines the changed-on-disk question a test stubs; it has no feature.
(load "userlock" nil t)

(defun niwa-test-rewind-reverts--call (turns current check &optional patch)
  "Record TURNS, install CURRENT bytes, and call CHECK in a real session.
Each turn is (BEFORE AFTER).  CHECK receives the buffer, target and source path.
With PATCH, run that ApplyPatch hunk through the real pipeline for capture."
  (pcase-let* ((`(,workspace . ,directory)
                (test-niwa-session-persistence--make-tempdir-workspace))
               (session (niwa-session-create "main" workspace))
               (buffer (generate-new-buffer " *rewind recorded changes*"))
               (path (file-name-concat directory "source.txt"))
               (other (file-name-concat directory "other.txt")))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (setq-local niwa--session session)
          (setq-local niwa--workspace workspace)
          (setf (niwa-session-permission-mode session) 'full-auto
                (niwa-session-root-buffer session) buffer)
          (setq-local gptel-backend
                      (test-niwa-session-persistence--agent-backend))
          (setq-local gptel-model 'test-model)
          (gptel-mode 1)
          (niwa-session-artifacts-save session buffer)
          (cl-loop for (before after) in turns
                   for turn from 1 do
                   (let ((coding-system-for-write 'no-conversion))
                     (write-region before nil path nil 'silent))
                   (niwa-request-begin session)
                   (if patch
                       (progn
                         (unless (niwa-tool-get "ApplyPatch")
                           (niwa-tool-patch-register))
                         (let (reply)
                           (niwa-pipeline-run-tool-outcome
                            (niwa-tool-get "ApplyPatch")
                            (lambda (value) (setq reply value))
                            (list :patch
                                  (format "*** Begin Patch\n*** Update File: %s\n%s\n*** End Patch"
                                          path patch)))
                           (let ((deadline (+ (float-time) 10)))
                             (while (and (not reply) (< (float-time) deadline))
                               (accept-process-output nil 0.05)))
                           (should (eq 'success (plist-get reply :status)))))
                     (puthash path before
                              (niwa-request-file-snapshots
                               niwa--current-request))
                     (let ((coding-system-for-write 'no-conversion))
                       (write-region after nil path nil 'silent)))
                   (puthash other (format "clean-%d\n" (1- turn))
                            (niwa-request-file-snapshots
                             niwa--current-request))
                   (write-region (format "clean-%d\n" turn) nil other nil 'silent)
                   (goto-char (point-max))
                   (insert (format "Prompt %d\n" turn))
                   (insert (propertize (format "Reply %d\n" turn)
                                       'gptel 'response))
                   (setf (niwa-session-turn-count session) turn)
                   (niwa-session-artifacts-save session buffer t)
                   (niwa-request-end))
          (let ((coding-system-for-write 'no-conversion))
            (write-region current nil path nil 'silent))
          (let ((target (copy-sequence
                         (cdr (cl-find-if
                               (lambda (entry)
                                 (= 1 (plist-get (cdr entry) :cum-turn)))
                               (niwa-session-rewind--prompt-candidates
                                session))))))
            (should target)
            (cl-letf (((symbol-function 'display-buffer) #'ignore))
              (funcall check buffer target path))))
      (test-niwa-session-persistence--release-and-kill buffer session)
      (delete-directory directory t)
      (niwa-workspace-clear-registry))))

(niwa-deftest niwa-session-rewind-rewind (:quiet t)
  ,test
  (test)
  :doc "reverts discarded turns newest first and preserves unrelated later edits"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nthree\nfour\n" "UNO\ntwo\nthree\nfour\n"))
   "UNO\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (niwa-session-rewind-rewind buffer target)))
     (should (equal "one\ntwo\nthree\nuser\n"
                    (niwa-session-artifacts-read-file-raw path)))
     (with-current-buffer buffer
       (should-not (string-match-p "Prompt" (buffer-string))))))
  :doc "one Rewind reads each immutable backup only once across repeated planning"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n") ("ONE\ntwo\n" "UNO\ntwo\n"))
   "UNO\ntwo\n"
   (lambda (buffer target _path)
     (let* ((reads (make-hash-table :test #'equal))
            (read-artifact (symbol-function 'niwa-session-artifacts-read-artifact)))
       ;; Count logical backups: equal bytes may share one published blob.
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'niwa-session-artifacts-read-artifact)
                  (lambda (session logical &rest args)
                    (when (string-prefix-p "file-history/" logical)
                      (cl-incf (gethash logical reads 0)))
                    (apply read-artifact session logical args))))
         (should (niwa-session-rewind-rewind buffer target)))
       (should (> (hash-table-count reads) 0))
       (maphash (lambda (_name count) (should (= 1 count))) reads))))
  :doc "conflict cancellation preserves all files, unsaved text and conversation"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\n" "one\nTWO\nthree\n"))
   "one\nuser\nthree\n"
   (lambda (buffer target path)
     (let ((transcript (with-current-buffer buffer (buffer-string)))
           (source (find-file-noselect path))
           asked)
       (unwind-protect
           (progn
             (with-current-buffer source
               (goto-char (point-max))
               (insert "unsaved\n"))
             (cl-letf (((symbol-function 'read-char-choice)
                        (lambda (prompt _choices &rest _)
                          (setq asked prompt)
                          (let ((report (with-current-buffer
                                            "*niwa-rewind-impact*"
                                          (buffer-string))))
                            (should (string-match-p (regexp-quote path) report))
                            (should (string-match-p "lines 2-2" report))
                            (should (string-match-p "Captured files restored: 1" report)))
                          (with-current-buffer "*niwa-rewind-impact*"
                            (goto-char (point-min))
                            (re-search-forward "^conflict +")
                            (should-error
                             (call-interactively (key-binding (kbd "d")))
                             :type 'user-error))
                          ?c))
                       ((symbol-function 'yes-or-no-p)
                        (lambda (&rest _) (ert-fail "Conflict was not refused"))))
               (should-not (niwa-session-rewind-rewind buffer target)))
             (should-not (get-buffer "*niwa-rewind-impact*"))
             (should (string-match-p (regexp-quote "[c]ancel") asked))
             (should (equal transcript
                            (with-current-buffer buffer (buffer-string))))
             (should (equal "one\nuser\nthree\n"
                            (niwa-session-artifacts-read-file-raw path)))
             (should (equal "clean-1\n"
                            (niwa-session-artifacts-read-file-raw
                             (file-name-concat (file-name-directory path)
                                               "other.txt"))))
             (with-current-buffer source
               (should (buffer-modified-p))
               (should (equal "one\nuser\nthree\nunsaved\n" (buffer-string)))))
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source)))))
  :doc "force restores snapshots only after a separate overwrite confirmation"
  (dolist (confirmed '(nil t))
    (niwa-test-rewind-reverts--call
     '(("one\ntwo\nthree\n" "one\nTWO\nthree\n"))
     "one\nuser\nthree\n"
     (lambda (buffer target path)
       (let ((transcript (with-current-buffer buffer (buffer-string)))
             confirmation)
         (cl-letf (((symbol-function 'read-char-choice)
                    (lambda (_prompt choices &rest _)
                      (should (memq ?f choices))
                      ?f))
                   ((symbol-function 'yes-or-no-p)
                    (lambda (prompt)
                      (setq confirmation prompt)
                      confirmed)))
           (should (eq confirmed
                       (niwa-session-rewind-rewind buffer target))))
         (should (string-match-p "Force snapshot restore" confirmation))
         (should (string-match-p "overwrite" confirmation))
         (should (equal (if confirmed "one\ntwo\nthree\n" "one\nuser\nthree\n")
                        (niwa-session-artifacts-read-file-raw path)))
         (should (equal (if confirmed "clean-0\n" "clean-1\n")
                        (niwa-session-artifacts-read-file-raw
                         (file-name-concat (file-name-directory path) "other.txt"))))
         (unless confirmed
           (should (equal transcript
                          (with-current-buffer buffer (buffer-string)))))))))
  :doc "saved buffer edits are re-planned as reversals, not snapshot restores"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n"))
   "ONE\ntwo\nthree\nfour\n"
   (lambda (buffer target path)
     (let ((source (find-file-noselect path)))
       (unwind-protect
           (progn
             (with-current-buffer source
               (goto-char (point-min))
               (search-forward "four")
               (replace-match "user"))
             (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?s))
                       ((symbol-function 'save-some-buffers)
                        (lambda (&rest _) (with-current-buffer source (save-buffer))))
                       ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
               (should (niwa-session-rewind-rewind buffer target)))
             (should (equal "one\ntwo\nthree\nuser\n"
                            (niwa-session-artifacts-read-file-raw path)))
             (with-current-buffer source
               (should-not (buffer-modified-p))
               (should (equal "one\ntwo\nthree\nuser\n" (buffer-string)))))
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source)))))
  :doc "keeps intervening edits between turns and shifted unrelated later lines"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nother-session\nfour\n" "UNO\ntwo\nother-session\nfour\n"))
   "UNO\ntwo\nother-session\nextra\nfour\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (niwa-session-rewind-rewind buffer target)))
     (should (equal "one\ntwo\nother-session\nextra\nfour\n"
                    (niwa-session-artifacts-read-file-raw path)))))
  :doc "after-response boundary keeps that turn and reverses only later turns"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nthree\nfour\n" "UNO\ntwo\nthree\nfour\n"))
   "UNO\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (niwa-session-rewind-rewind buffer target 'after)))
     (should (equal "ONE\ntwo\nthree\nuser\n"
                    (niwa-session-artifacts-read-file-raw path)))
     (with-current-buffer buffer
       (should (string-match-p "Reply 1" (buffer-string)))
       (should-not (string-match-p "Prompt 2" (buffer-string))))))
  :doc "a file changed during confirmation aborts without reverting other files"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\n" "ONE\ntwo\nthree\n"))
   "ONE\ntwo\nthree\n"
   (lambda (buffer target path)
     (let ((transcript (with-current-buffer buffer (buffer-string))))
       (cl-letf (((symbol-function 'yes-or-no-p)
                  (lambda (&rest _)
                    (write-region "concurrent\ntwo\nthree\n" nil path nil 'silent)
                    t)))
         (should-error (niwa-session-rewind-rewind buffer target)))
       (should (equal transcript (with-current-buffer buffer (buffer-string))))
       (should (equal "concurrent\ntwo\nthree\n"
                      (niwa-session-artifacts-read-file-raw path)))
       (should (equal "clean-1\n"
                      (niwa-session-artifacts-read-file-raw
                       (file-name-concat (file-name-directory path) "other.txt")))))))
  :doc "rollback restores only written files and preserves the edit refusing a later write"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let ((write (symbol-function 'niwa-pin-write-file-atomically))
           (transcript (with-current-buffer buffer (buffer-string))))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'niwa-pin-write-file-atomically)
                  (lambda (file &rest args)
                    (prog1 (apply write file args)
                      (when (and (equal file (file-name-concat (file-name-directory path) "other.txt"))
                                 (equal (car args) "clean-0\n"))
                        (write-region "concurrent\ntwo\n" nil path nil 'silent))))))
         (should-error (niwa-session-rewind-rewind buffer target)))
       (should (equal "concurrent\ntwo\n" (niwa-test--read-file path)))
       (should (equal "clean-1\n"
                      (niwa-test--read-file
                       (file-name-concat (file-name-directory path) "other.txt"))))
       (should (equal transcript (with-current-buffer buffer (buffer-string)))))))
  :doc "quit after a written file rolls that file back before abandoning Rewind"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let ((write (symbol-function 'niwa-pin-write-file-atomically))
           quit)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'niwa-pin-write-file-atomically)
                  (lambda (file &rest args)
                    (prog1 (apply write file args)
                      (when (equal (car args) "clean-0\n")
                        (signal 'quit nil))))))
         (condition-case nil
             (niwa-session-rewind-rewind buffer target)
           (quit (setq quit t))))
       (should quit)
       (should (equal "ONE\ntwo\n" (niwa-test--read-file path)))
       (should (equal "clean-1\n"
                      (niwa-test--read-file
                       (file-name-concat (file-name-directory path) "other.txt")))))))
  :doc "a failed results render still rolls all completed file writes back"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let ((render (symbol-function 'niwa-report-render)) injected)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'niwa-report-render)
                  (lambda (report &rest args)
                    (if (equal "Restore results" (plist-get report :title))
                        (progn (setq injected t) (error "Injected results render failure"))
                      (apply render report args)))))
         (should-error (niwa-session-rewind-rewind buffer target)))
       (should injected)
       (should (equal "ONE\ntwo\n" (niwa-test--read-file path)))
       (should (equal "clean-1\n"
                      (niwa-test--read-file
                       (file-name-concat (file-name-directory path) "other.txt")))))))
  :doc "literal pipeline captures preserve CRLF when reversing a real ApplyPatch"
  (niwa-test-rewind-reverts--call
   '(("one\r\ntwo\r\nthree\r\nfour\r\n" "ONE\r\ntwo\r\nthree\r\nfour\r\n"))
   "ONE\r\ntwo\r\nthree\r\nuser\r\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
               ((symbol-function 'read-char-choice)
                (lambda (&rest _) (ert-fail "Unrelated CRLF edit conflicts"))))
       (should (niwa-session-rewind-rewind buffer target)))
     (should (equal "one\r\ntwo\r\nthree\r\nuser\r\n"
                    (niwa-session-artifacts-read-file-raw path))))
   "@@\n-one\n+ONE")
  :doc "reports a gap in a later discarded turn even after an earlier capture"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\n" "ONE\ntwo\nthree\n")
     ("ONE\ntwo\nthree\n" "ONE\ntwo\nthree\n"))
   "ONE\ntwo\nthree\n"
   (lambda (buffer target path)
     ;; Represent a later failed capture of an already tracked path.
     (with-current-buffer buffer
       (push (cons path '(:gap "later capture failed" :version 2))
             (cdr (assq 2 (niwa-session-file-snapshots niwa--session)))))
     (cl-letf (((symbol-function 'yes-or-no-p)
                (lambda (&rest _)
                  (with-current-buffer "*niwa-rewind-impact*"
                    (should (string-match-p "later capture failed" (buffer-string))))
                  nil)))
       (should-not (niwa-session-rewind-rewind buffer target)))))
  :doc "an already undone hunk preserves unrelated user edits without a conflict"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n"))
   "one\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
               ((symbol-function 'read-char-choice)
                (lambda (&rest _) (ert-fail "Already undone hunk conflicts"))))
       (should (niwa-session-rewind-rewind buffer target)))
     (should (equal "one\ntwo\nthree\nuser\n"
                    (niwa-test--read-file path)))))
  :doc "a change already undone by the user needs no file write or force"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "one\ntwo\n"
   (lambda (buffer target path)
     (let ((mtime (file-attribute-modification-time (file-attributes path))))
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'read-char-choice)
                  (lambda (&rest _) (ert-fail "Already undone change refused"))))
         (should (niwa-session-rewind-rewind buffer target)))
       (should (equal "one\ntwo\n" (niwa-session-artifacts-read-file-raw path)))
       (should (equal mtime (file-attribute-modification-time
                            (file-attributes path)))))))
  :doc "merge resolves overlaps without writing until the complete Rewind commits"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n"))
   "mine\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (let ((transcript (with-current-buffer buffer (buffer-string))))
       (save-window-excursion
         (cl-letf (((symbol-function 'read-char-choice)
                    (lambda (_prompt choices &rest _)
                      (should (memq ?m choices)) ?m))
                   ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (should-not (niwa-session-rewind-rewind buffer target))
           (should (derived-mode-p 'niwa-note-merge-mode))
           (should-not (get-buffer "*niwa-rewind-impact*"))
           (should (equal transcript (with-current-buffer buffer (buffer-string))))
           (should (equal "mine\ntwo\nthree\nuser\n"
                          (niwa-session-artifacts-read-file-raw path)))
           (should-error (niwa-note-merge-confirm) :type 'user-error)
           (with-current-buffer buffer
             (put-text-property (point-min) (point-max) 'fontified t))
           (goto-char (point-min))
           (search-forward "<<<<<<<")
           (smerge-keep-lower)
           (niwa-note-merge-confirm)
           (should (equal "one\ntwo\nthree\nuser\n"
                          (niwa-session-artifacts-read-file-raw path)))
           (should (equal "clean-0\n"
                          (niwa-session-artifacts-read-file-raw
                           (file-name-concat (file-name-directory path) "other.txt"))))
           (with-current-buffer buffer
             (should-not (string-match-p "Prompt" (buffer-string)))))))))
  :doc "keeping a deleted current file in a Rewind merge leaves it deleted"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (delete-file path)
     (save-window-excursion
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?m))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (niwa-session-rewind-rewind buffer target)
         (goto-char (point-min)) (search-forward "<<<<<<<")
         (smerge-keep-upper)
         (niwa-note-merge-confirm)
         (should-not (file-exists-p path))))))
  :doc "merge chains conflicting turns newest first and writes neither round early"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("ONE\ntwo\nthree\nfour\n" "UNO\ntwo\nthree\nfour\n"))
   "mine\ntwo\nthree\nuser\n"
   (lambda (buffer target path)
     (save-window-excursion
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?m))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (niwa-session-rewind-rewind buffer target)
         (goto-char (point-min)) (search-forward "<<<<<<<")
         (smerge-keep-upper)
         (niwa-note-merge-confirm)
         (should (derived-mode-p 'niwa-note-merge-mode))
         (should (equal "mine\ntwo\nthree\nuser\n" (niwa-test--read-file path)))
         (should (equal "clean-2\n" (niwa-test--read-file
                                      (file-name-concat (file-name-directory path) "other.txt"))))
         (goto-char (point-min)) (search-forward "<<<<<<<")
         (smerge-keep-lower)
         (niwa-note-merge-confirm)
         (should (equal "one\ntwo\nthree\nuser\n" (niwa-test--read-file path)))
         (should (equal "clean-0\n" (niwa-test--read-file
                                      (file-name-concat (file-name-directory path) "other.txt"))))))))
  :doc "each older conflict offers cancel, force or merge again"
  (dolist (choice '(?c ?f ?m))
    (niwa-test-rewind-reverts--call
     '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
       ("ONE\ntwo\nthree\nfour\n" "UNO\ntwo\nthree\nfour\n"))
     "mine\ntwo\nthree\nuser\n"
     (lambda (buffer target path)
       (let ((rounds 0)
             (transcript (with-current-buffer buffer (buffer-string))))
         (save-window-excursion
           (cl-letf (((symbol-function 'read-char-choice)
                      (lambda (_prompt choices &rest _)
                        (should (equal '(?c ?f ?m) choices))
                        (if (= 1 (cl-incf rounds)) ?m choice)))
                     ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
             (niwa-session-rewind-rewind buffer target)
             (goto-char (point-min))
             (search-forward "<<<<<<<")
             (smerge-keep-upper)
             (niwa-note-merge-confirm)
             (should (= 2 rounds))
             (should-not (get-buffer "*niwa-rewind-impact*"))
             (when (eq choice ?m)
               (should (derived-mode-p 'niwa-note-merge-mode))
               (goto-char (point-min))
               (search-forward "<<<<<<<")
               (smerge-keep-lower)
               (niwa-note-merge-confirm))
             (should (equal (pcase choice
                              (?c "mine\ntwo\nthree\nuser\n")
                              (?f "one\ntwo\nthree\nfour\n")
                              (?m "one\ntwo\nthree\nuser\n"))
                            (niwa-test--read-file path)))
             (when (eq choice ?c)
               (should (equal transcript (with-current-buffer buffer (buffer-string)))))))))))
  :doc "merge starts from the intermediate text after a newer clean reversal"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\nthree\nfour\n" "ONE\ntwo\nthree\nfour\n")
     ("mine\ntwo\nthree\nfour\n" "mine\ntwo\nTHREE\nfour\n"))
   "mine\ntwo\nTHREE\nuser\n"
   (lambda (buffer target path)
     (save-window-excursion
       (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?m))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (niwa-session-rewind-rewind buffer target)
         (should-not (string-search "THREE" (buffer-string)))
         (goto-char (point-min)) (search-forward "<<<<<<<")
         (smerge-keep-lower)
         (niwa-note-merge-confirm)
         (should (equal "one\ntwo\nthree\nuser\n" (niwa-test--read-file path)))))))
  :doc "merge cancellation and source changes leave all files and history untouched"
  (dolist (action '(cancel source final))
    (niwa-test-rewind-reverts--call
     '(("one\ntwo\n" "ONE\ntwo\n")) "mine\ntwo\n"
     (lambda (buffer target path)
       (let ((transcript (with-current-buffer buffer (buffer-string))))
         (save-window-excursion
           (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?m))
                     ((symbol-function 'yes-or-no-p)
                      (lambda (&rest _)
                        (when (eq action 'final)
                          (write-region "later\ntwo\n" nil path nil 'silent))
                        t)))
             (niwa-session-rewind-rewind buffer target)
             (let ((merge (current-buffer)))
               (if (eq action 'cancel)
                   (niwa-note-merge-cancel)
                 (goto-char (point-min)) (search-forward "<<<<<<<")
                 (smerge-keep-lower)
                 (when (eq action 'source)
                   (write-region "later\ntwo\n" nil path nil 'silent))
                 (should-error (niwa-note-merge-confirm))
                 (when (buffer-live-p merge)
                   (with-current-buffer merge (niwa-note-merge-cancel))))
               (should (equal (if (eq action 'cancel) "mine\ntwo\n" "later\ntwo\n")
                              (niwa-test--read-file path)))
               (should (equal "clean-1\n" (niwa-test--read-file
                                            (file-name-concat (file-name-directory path) "other.txt"))))
               (should (equal transcript (with-current-buffer buffer (buffer-string)))))))))))
  :doc "a Rewind merge with two conflicting files resolves each against its own entry"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "mine\ntwo\n"
   (lambda (buffer target path)
     (let ((other (file-name-concat (file-name-directory path) "other.txt")))
       (write-region "mine-other\n" nil other nil 'silent)
       (save-window-excursion
         (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?m))
                   ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
           (niwa-session-rewind-rewind buffer target)
           (goto-char (point-min))
           (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
           (niwa-note-merge-confirm)
           (should (equal "one\ntwo\n" (niwa-test--read-file path)))
           (should (equal "clean-0\n" (niwa-test--read-file other))))))))
  :doc "keeps the current mode of an executable it reverts"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n"))
   "ONE\ntwo\n"
   (lambda (buffer target path)
     (set-file-modes path #o755)
     (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
       (should (niwa-session-rewind-rewind buffer target)))
     (should (equal "one\ntwo\n" (niwa-test--read-file path)))
     (should (= #o755 (file-modes path))))
   "@@\n-one\n+ONE")
  :doc "control: ApplyPatch keeps the current mode of an executable"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n"))
   "ONE\ntwo\n"
   (lambda (_buffer _target path)
     (set-file-modes path #o755)
     (should (eq 'success
                 (plist-get (niwa-test-rewind-reverts--apply-patch path "@@\n-ONE\n+one")
                            :status)))
     (should (equal "one\ntwo\n" (niwa-test--read-file path)))
     (should (= #o755 (file-modes path))))
   "@@\n-one\n+ONE"))

(niwa-deftest niwa-note-merge-open (:quiet t)
  ,test
  (test)
  :doc "COMMIT pairs each change with its own section's data across several turns"
  (let* ((dir (make-temp-file "niwa-merge-pairs-" t))
         (turns '(3 1 2))
         (paths (mapcar (lambda (turn) (file-name-concat dir (format "f%d.txt" turn))) turns))
         merge got)
    (unwind-protect
        (save-window-excursion
          (cl-mapc (lambda (path turn)
                     (write-region (format "mine%d\n" turn) nil path nil 'silent))
                   paths turns)
          (setq merge
                (niwa-note-merge-open
                 "*pairs*"
                 (cl-mapcar (lambda (path turn)
                              (list :path path :disk (niwa-change-snapshot path)
                                    :coding 'utf-8-unix :yours (format "mine%d\n" turn)
                                    :seen "base\n" :proposed (format "ai%d\n" turn)
                                    :data (list :turn turn)))
                            paths turns)
                 (lambda (pairs) (setq got pairs))))
          (goto-char (point-min))
          (while (search-forward "<<<<<<<" nil t) (smerge-keep-lower))
          (niwa-note-merge-confirm))
      (when (buffer-live-p merge)
        (with-current-buffer merge (set-buffer-modified-p nil))
        (kill-buffer merge))
      (delete-directory dir t))
    (should (equal turns (mapcar (lambda (pair) (plist-get (cdr pair) :turn)) got)))
    (should (equal paths (mapcar (lambda (pair) (plist-get (car pair) :path)) got)))
    (should (equal '("ai3\n" "ai1\n" "ai2\n")
                   (mapcar (lambda (pair) (plist-get (car pair) :content)) got)))
    ;; Negative control: a mispairing (sorted turns) must not equal the real result.
    (should-not (equal (sort (copy-sequence turns) #'<)
                       (mapcar (lambda (pair) (plist-get (cdr pair) :turn)) got)))))

(defun niwa-test-rewind-reverts--apply-patch (path patch)
  "Run PATCH against PATH through the real ApplyPatch pipeline; return reply."
  (niwa-request-begin niwa--session)
  (unwind-protect
      (let (reply)
        (niwa-pipeline-run-tool-outcome
         (niwa-tool-get "ApplyPatch")
         (lambda (value) (setq reply value))
         (list :patch (format "*** Begin Patch\n*** Update File: %s\n%s\n*** End Patch"
                              path patch)))
        (let ((deadline (+ (float-time) 10)))
          (while (and (not reply) (< (float-time) deadline))
            (accept-process-output nil 0.05)))
        reply)
    (niwa-request-end)))

(niwa-deftest niwa-session-rewind-file-transaction (:quiet t)
  ,test
  (test)
  :doc "publication failure restores files and buffers but preserves intervening edits"
  (dolist (edit '(nil other both))
    (niwa-test-rewind-reverts--call
     '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
     (lambda (buffer target path)
       (let ((source (find-file-noselect path))
             (other (file-name-concat (file-name-directory path) "other.txt"))
             (transcript (with-current-buffer buffer (buffer-string)))
             observed)
         (unwind-protect
             (let ((failure
                    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                              ((symbol-function 'niwa-session-publication-publish)
                               (lambda (&rest _)
                                 (setq observed (list (niwa-test--read-file path)
                                                      (with-current-buffer source (buffer-string))))
                                 (when edit (write-region "later" nil other nil 'silent))
                                 (when (eq edit 'both) (write-region "newer" nil path nil 'silent))
                                 (error "Injected publication failure"))))
                      (should-error (niwa-session-rewind-rewind buffer target)))))
               (should (equal '("one\ntwo\n" "one\ntwo\n") observed))
               (should (string-match-p (if edit "rollback incomplete" "Injected publication failure")
                                       (error-message-string failure)))
               (should (equal (if edit "later" "clean-1\n") (niwa-test--read-file other)))
               (when edit (should (string-match-p (regexp-quote other) (error-message-string failure))))
               (when (eq edit 'both)
                 (should (string-match-p (regexp-quote path) (error-message-string failure))))
               (should (equal (if (eq edit 'both) "newer" "ONE\ntwo\n") (niwa-test--read-file path)))
               (with-current-buffer source
                 (should-not (buffer-modified-p))
                 (should (equal (if (eq edit 'both) "one\ntwo\n" "ONE\ntwo\n") (buffer-string))))
               (should (equal transcript (with-current-buffer buffer (buffer-string)))))
           (kill-buffer source))))))
  :doc "an aliased captured path refuses the command before it changes the other file"
  (niwa-test-rewind-reverts--call
   '(("one\n" "ONE\n")) "ONE\n"
   (lambda (buffer target path)
     (let ((transcript (with-current-buffer buffer (buffer-string)))
           (truename (symbol-function 'file-truename)) checked)
       (cl-letf (((symbol-function 'file-truename)
                  (lambda (file &rest args)
                    (if (equal path file)
                        (progn (setq checked t) (concat path ".alias"))
                      (apply truename file args))))
                 ((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
         (should-error (niwa-session-rewind-rewind buffer target) :type 'user-error))
       (should checked)
       (should (equal "ONE\n" (niwa-test--read-file path)))
       (should (equal "clean-1\n" (niwa-test--read-file
                                 (file-name-concat (file-name-directory path) "other.txt"))))
       (should (equal transcript (with-current-buffer buffer (buffer-string)))))))
  :doc "existing leaf and directory aliases within the project refuse before confirmation"
  (dolist (leaf '(nil t))
    (niwa-test-rewind-reverts--call
     '(("previous\n" "same\n")) "same\n"
     (lambda (buffer target path)
       (let* ((dir (file-name-directory path))
              (real (file-name-concat dir "real"))
              (link (file-name-concat dir "alias"))
              (copy (file-name-concat real "source.txt"))
              (alias (if leaf link (file-name-concat link "source.txt")))
              (session (buffer-local-value 'niwa--session buffer))
              (transcript (with-current-buffer buffer (buffer-string))))
         (make-directory real)
         (copy-file path copy)
         (make-symbolic-link (if leaf copy real) link)
         ;; Feed an inert existing alias to planning, without writing through it.
         (setcar (assoc path (cdr (assq 1 (niwa-session-file-snapshots session)))) alias)
         (cl-letf (((symbol-function 'yes-or-no-p)
                    (lambda (&rest _) (ert-fail "Reached confirmation"))))
           (let ((err (should-error (niwa-session-rewind-rewind buffer target) :type 'user-error)))
             (should (string-match-p (regexp-quote alias) (error-message-string err)))))
         (should (equal "same\n" (niwa-test--read-file copy)))
         (should (equal transcript (with-current-buffer buffer (buffer-string))))))))
  :doc "a leaf link planted after validation is refused at the write and kept"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let ((write (symbol-function 'niwa-pin-write-file-atomically))
           (outside (make-temp-file "niwa-rewind-outside-" nil nil "ONE\ntwo\n"))
           (other (file-name-concat (file-name-directory path) "other.txt"))
           (transcript (with-current-buffer buffer (buffer-string)))
           planted)
       (unwind-protect
           (progn
             (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                       ((symbol-function 'niwa-pin-write-file-atomically)
                        (lambda (file &rest args)
                          ;; Past the preflight and the commit's recheck.
                          (when (and (equal file path) (not planted))
                            (setq planted t)
                            (delete-file path)
                            (make-symbolic-link outside path))
                          (apply write file args))))
               (should-error (niwa-session-rewind-rewind buffer target)))
             (should planted)
             (should (equal outside (file-symlink-p path)))
             (should (equal "ONE\ntwo\n" (niwa-test--read-file outside)))
             (should (equal "clean-1\n" (niwa-test--read-file other)))
             (should (equal transcript (with-current-buffer buffer (buffer-string)))))
         (when (file-symlink-p path) (delete-file path))
         (delete-file outside)))))
  :doc "a buffer that cannot be restored is named and the numbered recovery copies stay"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let* ((other (file-name-concat (file-name-directory path) "other.txt"))
            (visiting (find-file-noselect other))
            (restore (symbol-function 'niwa-change--restore-buffer))
            (write (symbol-function 'niwa-pin-write-file-atomically))
            (save-path (niwa-session-save-path (buffer-local-value 'niwa--session buffer)))
            injected)
       (unwind-protect
           (let* ((failure
                   (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                             ((symbol-function 'niwa-pin-write-file-atomically)
                              (lambda (file &rest args)
                                (if (equal file path)
                                    (error "Injected write failure")
                                  (apply write file args))))
                             ((symbol-function 'niwa-change--restore-buffer)
                              (lambda (snapshot)
                                (if (eq (plist-get snapshot :buffer) visiting)
                                    (progn (setq injected t)
                                           (error "Injected buffer restore failure"))
                                  (funcall restore snapshot)))))
                     (should-error (niwa-session-rewind-rewind buffer target))))
                  (message (error-message-string failure)))
             (should injected)
             (should (string-match-p "rollback incomplete" message))
             (should (string-match-p (regexp-quote other) message))
             ;; The portable session moves the recovery directory to its target.
             (should (directory-files-recursively
                      (file-name-concat save-path ".recovery") "\\`[0-9]\\{6\\}\\'"))
             (should (equal "clean-1\n" (niwa-test--read-file other)))
             (should (equal "ONE\ntwo\n" (niwa-test--read-file path))))
         (with-current-buffer visiting (set-buffer-modified-p nil))
         (kill-buffer visiting)))))
  :doc "a draft typed while native history is prepared stops Rewind, keeping draft and head"
  (dolist (edit '(draft none))
    (niwa-test-rewind-reverts--call
     '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
     (lambda (buffer target path)
       (let* ((session (buffer-local-value 'niwa--session buffer))
              (directory (file-name-directory path))
              (source (find-file-noselect path))
              (head (plist-get (niwa-session-publication session) :head))
              fork completion)
         (setf (niwa-session-external-conversations session)
               (list (cons "root" (list :engine 'claude-code :id "old" :host (system-name)
                                        :directory directory :state 'ready
                                        :boundaries (list (list :turn 1 :id "old" :host (system-name)
                                                                :directory directory
                                                                :message-id "answer-1"))))))
         (unwind-protect
             (progn
               (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                         ((symbol-function 'niwa-claude-code-fork-histories)
                          (lambda (_histories callback &rest _) (setq fork callback) :pending))
                         ((symbol-function 'niwa--run-session-start-hooks) #'ignore))
                 ;; Confirm, then type, then finish the native fork.
                 (should (eq :pending (niwa-session-rewind-rewind
                                       buffer target nil nil nil
                                       (lambda (result error) (setq completion (list result error))))))
                 (should fork)
                 (when (eq edit 'draft)
                   (with-current-buffer source
                     (goto-char (point-max))
                     (insert "draft\n")))
                 (funcall fork nil nil #'ignore))
               (if (eq edit 'none)
                   (progn
                     (should (car completion))
                     (should (equal "one\ntwo\n" (niwa-test--read-file path))))
                 (should-not (car completion))
                 (should (string-match-p "unsaved edits" (cadr completion)))
                 (should (string-match-p (regexp-quote path) (cadr completion)))
                 (should (equal head (plist-get (niwa-session-publication session) :head)))
                 (should (equal "ONE\ntwo\n" (niwa-test--read-file path)))
                 (with-current-buffer source
                   (should (buffer-modified-p))
                   (should (equal "ONE\ntwo\ndraft\n" (buffer-string))))
                 (with-current-buffer buffer
                   (should (string-match-p "Reply 1" (buffer-string))))))
           (with-current-buffer source (set-buffer-modified-p nil))
           (kill-buffer source))))))
  :doc "a draft typed while Redo prepares native history stops Redo, keeping draft and head"
  (dolist (edit '(draft none))
    (niwa-test-rewind-reverts--call
     '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
     (lambda (buffer target path)
       (let* ((session (buffer-local-value 'niwa--session buffer))
              (directory (file-name-directory path))
              fork completion source head redo)
         (setf (niwa-session-external-conversations session)
               (list (cons "root" (list :engine 'claude-code :id "old" :host (system-name)
                                        :directory directory :state 'ready
                                        :boundaries (list (list :turn 1 :id "old" :host (system-name)
                                                                :directory directory
                                                                :message-id "answer-1"))))))
         ;; Publish turn 1 with its native history, then rewind its file change.
         (niwa-session-artifacts-save session buffer t)
         (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                   ((symbol-function 'niwa-claude-code-fork-histories)
                    (lambda (_histories callback &rest _) (funcall callback nil nil #'ignore)))
                   ((symbol-function 'niwa--run-session-start-hooks) #'ignore))
           (niwa-session-rewind-rewind buffer target))
         (should (equal "one\ntwo\n" (niwa-test--read-file path)))
         (setq redo (plist-get (cl-find 1 (niwa-session-rewind-published-heads session)
                                        :key (lambda (entry) (plist-get entry :turn-count)))
                               :head)
               head (plist-get (niwa-session-publication session) :head)
               source (find-file-noselect path))
         (should redo)
         (unwind-protect
             (progn
               (cl-letf (((symbol-function 'niwa-claude-code-fork-histories)
                          (lambda (_histories callback &rest _) (setq fork callback) :pending))
                         ((symbol-function 'niwa--run-session-start-hooks) #'ignore))
                 ;; Confirm, then type, then finish the native fork.
                 (should (eq :pending (niwa-session-rewind-restore-head
                                       session buffer redo (lambda (_impact) t) nil
                                       (lambda (result error) (setq completion (list result error))))))
                 (should fork)
                 (when (eq edit 'draft)
                   (with-current-buffer source
                     (goto-char (point-max))
                     (insert "draft\n")))
                 (funcall fork nil nil #'ignore))
               (if (eq edit 'none)
                   (progn
                     (should (car completion))
                     (should (equal "ONE\ntwo\n" (niwa-test--read-file path))))
                 (should-not (car completion))
                 (should (string-match-p "Redo stopped" (cadr completion)))
                 (should (equal head (plist-get (niwa-session-publication session) :head)))
                 (should (equal "one\ntwo\n" (niwa-test--read-file path)))
                 (with-current-buffer source
                   (should (buffer-modified-p))
                   (should (equal "one\ntwo\ndraft\n" (buffer-string))))))
           (with-current-buffer source (set-buffer-modified-p nil))
           (kill-buffer source))))))
  :doc "a draft typed after the early check stops Rewind under the lease or at the write"
  (dolist (point '(staging write))
    (niwa-test-rewind-reverts--call
     '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
     (lambda (buffer target path)
       (let* ((session (buffer-local-value 'niwa--session buffer))
              (other (file-name-concat (file-name-directory path) "other.txt"))
              (source (find-file-noselect path))
              (head (plist-get (niwa-session-publication session) :head))
              (write (symbol-function 'niwa-pin-write-file-atomically))
              (stop (symbol-function 'niwa-execution-stop-services))
              (typed nil) stopped
              (draft (lambda ()
                       (unless typed
                         (setq typed t)
                         (with-current-buffer source
                           (goto-char (point-max))
                           (insert "draft\n"))))))
         (unwind-protect
             (let ((failure
                    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                              ((symbol-function 'niwa-pin-write-file-atomically)
                               (lambda (file &rest args)
                                 (when (and (eq point 'write) (equal file other)) (funcall draft))
                                 (apply write file args)))
                              ((symbol-function 'niwa-execution-stop-services)
                               (lambda (&rest args) (setq stopped t) (apply stop args))))
                      (let ((org-mode-hook
                             (if (eq point 'staging)
                                 (cons (lambda ()
                                         (when (string-match-p "staging" (buffer-name)) (funcall draft)))
                                       org-mode-hook)
                               org-mode-hook)))
                        (should-error (niwa-session-rewind-rewind buffer target))))))
               (should typed)
               ;; A draft typed before the lease stops Rewind before services stop.
               (should (eq (eq point 'write) stopped))
               (should (string-match-p "Rewind stopped" (error-message-string failure)))
               (should (equal "ONE\ntwo\n" (niwa-test--read-file path)))
               (should (equal "clean-1\n" (niwa-test--read-file other)))
               (should (equal head (plist-get (niwa-session-publication session) :head)))
               (with-current-buffer source
                 (should (buffer-modified-p))
                 (should (equal "ONE\ntwo\ndraft\n" (buffer-string)))))
           (with-current-buffer source (set-buffer-modified-p nil))
           (kill-buffer source))))))
  :doc "a draft typed into one buffer during another file's write stops Rewind, even one equal to the plan's text"
  ;; The second draft is exactly what Rewind would write to other.txt.
  (dolist (text '("clean-1\ndraft\n" "clean-0\n"))
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let* ((other (file-name-concat (file-name-directory path) "other.txt"))
            (visiting (find-file-noselect other))
            (write (symbol-function 'niwa-pin-write-file-atomically))
            (head (plist-get (niwa-session-publication (buffer-local-value 'niwa--session buffer)) :head))
            typed)
       (unwind-protect
           (let ((failure
                  (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                            ((symbol-function 'niwa-pin-write-file-atomically)
                             (lambda (file &rest args)
                               ;; other.txt is already written; type into it during source.txt's write.
                               (when (and (equal file path) (not typed))
                                 (setq typed t)
                                 ;; The user answers Emacs's changed-on-disk question and types.
                                 (cl-letf (((symbol-function 'ask-user-about-supersession-threat) #'ignore))
                                   (with-current-buffer visiting
                                     (erase-buffer)
                                     (insert text))))
                               (apply write file args))))
                    (should-error (niwa-session-rewind-rewind buffer target)))))
             (should typed)
             (should (string-match-p "Rewind stopped" (error-message-string failure)))
             (should (equal "clean-1\n" (niwa-test--read-file other)))
             (should (equal "ONE\ntwo\n" (niwa-test--read-file path)))
             (should (equal head (plist-get (niwa-session-publication (buffer-local-value 'niwa--session buffer)) :head)))
             (with-current-buffer visiting
               (should (buffer-modified-p))
               (should (equal text (buffer-string)))))
         (with-current-buffer visiting (set-buffer-modified-p nil))
         (kill-buffer visiting))))))
  :doc "a draft typed during an immediate Redo stops it before files or history change"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let ((session (buffer-local-value 'niwa--session buffer))
           (backup (symbol-function 'niwa-session-rewind--backup-restore-files))
           (stop (symbol-function 'niwa-execution-stop-services))
           source head redo typed stopped)
       (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                 ((symbol-function 'niwa--run-session-start-hooks) #'ignore))
         (niwa-session-rewind-rewind buffer target))
       (should (equal "one\ntwo\n" (niwa-test--read-file path)))
       (setq redo (plist-get (cl-find 1 (niwa-session-rewind-published-heads session)
                                      :key (lambda (entry) (plist-get entry :turn-count)))
                             :head)
             head (plist-get (niwa-session-publication session) :head)
             source (find-file-noselect path))
       (should redo)
       (unwind-protect
           (let ((failure
                  (cl-letf (((symbol-function 'niwa-session-rewind--backup-restore-files)
                             (lambda (&rest args)
                               ;; After confirmation, under the lease.
                               (setq typed t)
                               (with-current-buffer source
                                 (goto-char (point-max))
                                 (insert "draft\n"))
                               (apply backup args)))
                            ((symbol-function 'niwa-execution-stop-services)
                             (lambda (&rest args) (setq stopped t) (apply stop args))))
                    (should-error (niwa-session-rewind-restore-head
                                   session buffer redo (lambda (_impact) t))))))
             (should typed)
             (should-not stopped)
             (should (string-match-p "Redo stopped" (error-message-string failure)))
             (should (equal "one\ntwo\n" (niwa-test--read-file path)))
             (should (equal head (plist-get (niwa-session-publication session) :head)))
             (with-current-buffer source
               (should (buffer-modified-p))
               (should (equal "one\ntwo\ndraft\n" (buffer-string)))))
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source)))))
  :doc "publication failure after a deletion restores the file and its note's source"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let* ((session (buffer-local-value 'niwa--session buffer))
            (workspace (buffer-local-value 'niwa--workspace buffer))
            (source (find-file-noselect path))
            deleted uuid)
       ;; The file did not exist before turn 1, so Rewind deletes it.
       (plist-put (cdr (assoc path (cdr (assq 1 (niwa-session-file-snapshots session)))))
                  :pre-backup-name nil)
       (unwind-protect
           (progn
             (with-current-buffer source
               (setq-local niwa--workspace workspace)
               (goto-char (point-min))
               (setq uuid (overlay-get (niwa-note-create "keep my source") 'niwa-uuid)))
             (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                       ((symbol-function 'niwa-session-publication-publish)
                        (lambda (&rest _)
                          (setq deleted (and (not (file-exists-p path))
                                             (buffer-local-value 'niwa--buffer-source-missing source)))
                          (error "Injected publication failure"))))
               (should-error (niwa-session-rewind-rewind buffer target)))
             (should deleted)
             (should (equal "ONE\ntwo\n" (niwa-test--read-file path)))
             (should-not (buffer-local-value 'niwa--buffer-source-missing source))
             (let ((note (cl-find uuid (niwa-notes-open workspace) :test #'equal
                                  :key (lambda (entry) (plist-get entry :uuid)))))
               (should note)
               (should-not (plist-get note :source-missing))))
         (with-current-buffer source (set-buffer-modified-p nil))
         (kill-buffer source)))))
  :doc "Rewind keeps point and note on an unchanged line"
  (niwa-test-rewind-reverts--call
   '(("one\ntwo\n" "ONE\ntwo\n")) "ONE\ntwo\n"
   (lambda (buffer target path)
     (let ((source (find-file-noselect path)) uuid annotation)
       (unwind-protect
           (progn
             (with-current-buffer source
               (setq-local niwa--workspace (buffer-local-value 'niwa--workspace buffer))
               (goto-char 6)
               (setq uuid (overlay-get (niwa-note-create "keep this line") 'niwa-uuid))
               (should uuid)
               (setq annotation (make-overlay 5 8))
               (overlay-put annotation 'evaporate t)
               (goto-char 6))
             (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
               (should (niwa-session-rewind-rewind buffer target)))
             (with-current-buffer source
               (should (= 6 (point)))
               (should (equal '(5 8) (list (overlay-start annotation)
                                           (overlay-end annotation))))
               ;; The note store reinstalls presentations after publication.
               (should (cl-find uuid (overlays-at 6)
                                :key (lambda (o) (overlay-get o 'niwa-uuid))
                                :test #'equal))))
         (kill-buffer source))))))

(provide 'test-niwa-session-rewind-reverts)
;;; test-niwa-session-rewind-reverts.el ends here
