;;; test-niwa-sandbox-memory-roots.el --- Memory root isolation -*- lexical-binding: t -*-

;;; Commentary:

;; Sandbox preparation must not plant workspace state in writable memory roots.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-sandbox)
(require 'niwa-permission-rules)
(defvar niwa-sessions-directory)

(niwa-deftest niwa-sandbox-prepare/stores-survive-replacement
  (:doc "stores stay read-only to a running child across host replacement")
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil))
    (skip-unless (plist-get (niwa-sandbox-probe) :available))
    ;; A relative root outside .niwa and an absolute one: both are walked
    ;; globs, not the root-anchored default.
    (dolist (layout '(relative absolute))
      (let* ((root (file-name-as-directory (make-temp-file "niwa-sandbox-stores-" t)))
             (sessions (if (eq layout 'relative) "var/sessions"
                         (file-name-concat root "store/sessions")))
             (niwa-sessions-directory sessions)
             (niwa-protected-paths
              (eval (car (get 'niwa-protected-paths 'standard-value)) t))
             (sidecar (file-name-concat (expand-file-name sessions root)
                                        "S" "session.meta.eld"))
             (stores (list (file-name-concat root ".niwa/state/instructions.eld")
                           (file-name-concat root ".niwa/state/plugins.eld")
                           sidecar))
             prepared process)
        (unwind-protect
            (progn
              (make-directory (file-name-concat root "src"))
              (make-directory (file-name-concat root ".niwa/shared") t)
              (make-directory (file-name-concat root ".niwa/state") t)
              (make-directory (file-name-directory sidecar) t)
              (dolist (store stores) (write-region "old" nil store nil 'silent))
              (setq prepared
                    (niwa-sandbox-prepare
                     (list "sh" "-c"
                           (concat
                            "cd \"$1\"; "
                            ;; Exit 1N or 2N: store N was writable before or
                            ;; after the host replaced it; 14: a file was created
                            ;; in state/.  Plugin data and tracked configuration
                            ;; at the top of .niwa/ stay writable (5, 6).
                            "w() { if (printf child >\"$2\") 2>/dev/null; then exit $1; fi; }; "
                            "w 11 \"$2\"; w 12 \"$3\"; w 13 \"$4\"; w 14 .niwa/state/new.eld; "
                            "if mkdir \"$(dirname \"$4\")/../T\" 2>/dev/null; then exit 2; fi; "
                            "printf ok >.niwa/shared/a || exit 3; "
                            "printf ok >.niwa/state/plugin-data/p || exit 5; "
                            "printf ok >.niwa/hooks.json || exit 6; "
                            ": >ready; while [ ! -e go ]; do sleep 0.05; done; "
                            "w 21 \"$2\"; w 22 \"$3\"; w 23 \"$4\"; "
                            "printf ok >.niwa/shared/b || exit 4")
                           "sh" root (nth 0 stores) (nth 1 stores) (nth 2 stores))
                     (file-name-concat root "src") (list root)))
              (should (eq 'confined (plist-get prepared :state)))
              (setq process (make-process :name "niwa-stores" :buffer nil
                                          :command (plist-get prepared :command)
                                          :connection-type 'pipe))
              (with-timeout (20 (error "Child never reached its first checks"))
                (while (and (process-live-p process)
                            (not (file-exists-p (file-name-concat root "ready"))))
                  (accept-process-output process 0.05)))
              ;; Replace each store by rename, as the control program does.
              (dolist (store stores)
                (write-region "new" nil (concat store ".tmp") nil 'silent)
                (should (zerop (call-process "mv" nil nil nil "-fT"
                                             (concat store ".tmp") store))))
              (write-region "" nil (file-name-concat root "go") nil 'silent)
              (with-timeout (20 (error "Child did not finish"))
                (while (process-live-p process)
                  (accept-process-output process 0.05)))
              (should (equal (list layout 0) (list layout (process-exit-status process))))
              (dolist (store stores)
                (should (equal "new" (with-temp-buffer
                                       (insert-file-contents store)
                                       (buffer-string)))))
              (should-not (file-exists-p (file-name-concat root ".niwa/state/new.eld")))
              (should (file-exists-p (file-name-concat root ".niwa/state/plugin-data/p")))
              (should (file-exists-p (file-name-concat root ".niwa/hooks.json")))
              (should (file-exists-p (file-name-concat root ".niwa/shared/b"))))
          (when (process-live-p process) (delete-process process))
          (when prepared (niwa-sandbox-cleanup prepared))
          (delete-directory root t))))))

(niwa-deftest niwa-sandbox-prepare/git-checkout-project-config
  (:doc "a confined git checkout updates tracked .niwa/ configuration")
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil)
        (niwa-sessions-directory ".niwa/sessions"))
    (skip-unless (plist-get (niwa-sandbox-probe) :available))
    (skip-unless (executable-find "git"))
    (let* ((root (file-name-as-directory (make-temp-file "niwa-sandbox-checkout-" t)))
           (default-directory root)
           (niwa-protected-paths
            (eval (car (get 'niwa-protected-paths 'standard-value)) t))
           (files '(".niwa/hooks.json" ".niwa/permissions.eld"))
           (git (lambda (&rest args)
                  (should (eq 0 (apply #'call-process "git" nil nil nil
                                       "-c" "user.name=t" "-c" "user.email=t@t" args)))))
           (contents (lambda (file)
                       (with-temp-buffer
                         (insert-file-contents (file-name-concat root file))
                         (buffer-string))))
           prepared)
      (unwind-protect
          (progn
            (make-directory (file-name-concat root ".niwa/state") t)
            (write-region ".niwa/state/\n" nil (file-name-concat root ".gitignore") nil 'silent)
            (dolist (file files) (write-region "main" nil file nil 'silent))
            (funcall git "init" "-q" "-b" "main")
            (funcall git "add" ".")
            (funcall git "commit" "-q" "-m" "main")
            (funcall git "checkout" "-q" "-b" "other")
            (dolist (file files) (write-region "other" nil file nil 'silent))
            (funcall git "commit" "-q" "-am" "other")
            (funcall git "checkout" "-q" "main")
            ;; Git's own metadata needs its usual grant; .niwa/ needs none.
            (setq prepared
                  (niwa-sandbox-prepare
                   (list "git" "checkout" "-q" "other") root (list root)
                   `(:file-system ((:path ,(file-name-concat root ".git")
                                    :access write :recursive t)))))
            (should (eq 'confined (plist-get prepared :state)))
            (with-temp-buffer
              (let ((status (apply #'call-process (car (plist-get prepared :command))
                                   nil t nil (cdr (plist-get prepared :command)))))
                (ert-info ((buffer-string)) (should (eq 0 status)))))
            (dolist (file files)
              (should (equal (list file "other") (list file (funcall contents file))))))
        (when prepared (niwa-sandbox-cleanup prepared))
        (delete-directory root t)))))

(niwa-deftest niwa-sandbox-prepare/memory-roots
  (:doc "keeps local and external memory roots unchanged while protecting workspace state")
  (let ((niwa-sandbox-mode 'required)
        (niwa-sandbox--probe-cache nil)
        ;; Bound as when niwa runs, whatever this file's load order.
        (niwa-sessions-directory ".niwa/sessions"))
    (skip-unless (plist-get (niwa-sandbox-probe) :available))
    (let* ((root (make-temp-file "niwa-sandbox-memory-" t))
           (external (make-temp-file "niwa-sandbox-global-memory-" t))
           (secondary (make-temp-file "niwa-sandbox-secondary-" t))
           (memory (file-name-concat root ".niwa/memory"))
           (workdir (file-name-concat root "src"))
           prepared)
      (unwind-protect
          (progn
            (make-directory memory t)
            (make-directory workdir)
            (dolist (relative '(".niwa/artifacts/.state" ".niwa/leases"))
              (make-directory (file-name-concat secondary relative) t))
            (dolist (directory (list memory external))
              (write-region "# Memory\n" nil
                            (file-name-concat directory "MEMORY.md") nil 'silent))
            (setq prepared
                  (niwa-sandbox-prepare
                   (list "sh" "-c"
                         (concat
                          "set -e; cd ..; "
                          "if touch .niwa/artifacts/.state/forged 2>/dev/null; then exit 1; fi; "
                          "if touch .niwa/leases/forged 2>/dev/null; then exit 2; fi; "
                          "if mkdir .niwa/schedules/forged 2>/dev/null; then exit 5; fi; "
                          "if touch .niwa/sessions/forged 2>/dev/null; then exit 6; fi; "
                          "if touch .niwa/state/forged 2>/dev/null; then exit 7; fi; "
                          "if touch \"$1/.niwa/artifacts/.state/forged\" 2>/dev/null; then exit 3; fi; "
                          "if touch \"$1/.niwa/leases/forged\" 2>/dev/null; then exit 4; fi; "
                          "printf authored >.niwa/artifacts/new.html")
                         "sh" secondary)
                   workdir (list root memory external secondary)))
            (should (eq 'confined (plist-get prepared :state)))
            (dolist (directory (list memory external))
              (should (equal '("MEMORY.md")
                             (directory-files directory nil directory-files-no-dot-files-regexp)))
              (should (equal "# Memory\n"
                             (with-temp-buffer
                               (insert-file-contents (file-name-concat directory "MEMORY.md"))
                               (buffer-string)))))
            (dolist (relative '(".niwa/artifacts/.state" ".niwa/leases" ".niwa/schedules" ".niwa/state"
                                ".niwa/sessions"))
              (should (file-directory-p (file-name-concat root relative))))
            (with-temp-buffer
              (should (zerop (apply #'call-process (car (plist-get prepared :command))
                                    nil (current-buffer) nil
                                    (cdr (plist-get prepared :command))))))
            (should (file-exists-p (file-name-concat root ".niwa/artifacts/new.html")))
            (should-not (file-exists-p (file-name-concat root ".niwa/artifacts/.state/forged")))
            (should-not (file-exists-p (file-name-concat root ".niwa/leases/forged")))
            (should-not (file-exists-p (file-name-concat root ".niwa/schedules/forged"))))
        (when prepared (niwa-sandbox-cleanup prepared))
        (delete-directory secondary t)
        (delete-directory external t)
        (delete-directory root t)))))

(provide 'test-niwa-sandbox-memory-roots)
;;; test-niwa-sandbox-memory-roots.el ends here
