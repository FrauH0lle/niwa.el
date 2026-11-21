;;; test-niwa-session-migration.el --- Explicit migration tests -*- lexical-binding: t -*-

;;; Commentary:
;; Exercise the standalone converter on real files, then use the normal reader.

;;; Code:

(require 'niwa-session-test-support
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           "niwa-session-test-support"))
;; Name the source: stale bytecode beside the script would otherwise shadow the
;; converter under test.
(require 'niwa-migrate-session
         (file-name-concat (file-name-directory (or load-file-name buffer-file-name))
                           ".." "scripts" "migrate-sessions.el"))

(defun niwa-migration-test--stable-attributes (file)
  "Return FILE's attributes except access time, which reads may change."
  (let ((attributes (file-attributes file)))
    (setf (nth 4 attributes) nil)
    attributes))

(defconst niwa-migration-test--steps
  `(("v0.5.99" ,niwa-session-codec-format-version identity))
  "Test-only upgrade to exercise staging and installation.")

(defmacro niwa-migration-test--with-source (version &rest body)
  "Run BODY with a two-head VERSION session and a separate destination."
  (declare (indent 1) (debug t))
  `(let* ((root (make-temp-file "niwa-migration-" t))
          (source (file-name-concat root "old"))
          (destination (file-name-concat root "converted"))
          (session (test-niwa-session-persistence--make-session root))
          (old (niwa-session-codec-serialize session))
          (head ".publications/generation-bbbbbbbbbbbbbbbbbbbb/manifest.eld")
          (lease-file (file-name-concat source ".lease/00000000000000000001.eld")))
     (unwind-protect
         (progn
           (setq old (plist-put old :version ,version))
           ;; The native writer shares equal object identities with #N=/#N#.
           (setq old (plist-put old :first-user-message "Shared preview"))
           (setq old (plist-put old :latest-user-message (plist-get old :first-user-message)))
           (make-directory (file-name-concat source ".lease") t)
           (niwa-migrate-session--write (file-name-concat source "session.meta.eld") old)
           (dolist (name '("aaaaaaaaaaaaaaaaaaaa" "bbbbbbbbbbbbbbbbbbbb"))
             (let* ((directory (concat ".publications/generation-" name))
                    (meta (concat directory "/000001.data"))
                    (chat (concat directory "/000002.data")))
               (make-directory (file-name-concat source directory) t)
               (niwa-migrate-session--write (file-name-concat source meta) old)
               (with-temp-file (file-name-concat source chat)
                 (insert "#+title: Kept transcript\nUser and assistant text.\n"))
               (niwa-migrate-session--write
                (file-name-concat source directory "manifest.eld")
                (list :sidecar "session.meta.eld" :artifacts
                      (list (list "session.meta.eld" :published meta :sha256
                                  (niwa-migrate-session--hash (file-name-concat source meta)))
                            (list "segment-0001.chat.org" :published chat :sha256
                                  (niwa-migrate-session--hash (file-name-concat source chat))))))))
           (niwa-migrate-session--write
            lease-file (list :generation 1 :transfer-generation 1 :status 'released
                             :publication-head head :unsettled-mutation nil
                             :client-id (make-string 64 ?a) :renewed-at 1 :expires-at 2))
           ,@body)
       (delete-directory root t)
       (niwa-workspace-clear-registry))))

(niwa-deftest niwa-migrate-session-copy (:quiet t)
  ,test
  (test)
  :doc "drops retained generations and fixed caches no reader accepts"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (let* ((stale ".publications/generation-aaaaaaaaaaaaaaaaaaaa")
           (data (concat stale "/000001.data"))
           (manifest-file (file-name-concat source stale "manifest.eld"))
           (manifest (niwa-migrate-session--read manifest-file)))
      (niwa-migrate-session--write (file-name-concat source data)
                                      (plist-put (copy-tree old) :version "v0.5.5"))
      (setf (plist-get (cdar (plist-get manifest :artifacts)) :sha256)
            (niwa-migrate-session--hash (file-name-concat source data)))
      (niwa-migrate-session--write manifest-file manifest)
      (niwa-migrate-session--write (file-name-concat source "session.meta.eld")
                                      (plist-put (copy-tree old) :version "v0.5.5"))
      (should (niwa-migrate-session-copy source destination))
      (should-not (file-exists-p (file-name-concat destination stale "manifest.eld")))
      (should (file-exists-p (file-name-concat destination data)))
      (should-not (file-exists-p (file-name-concat destination "session.meta.eld")))
      (should (file-exists-p (file-name-concat destination head)))
      (let ((before (niwa-migrate-session--snapshot source)))
        (should (eq 'current (plist-get (cdar (niwa-migrate-sessions source)) :status)))
        (should (equal before (niwa-migrate-session--snapshot source))))))

  :doc "takes an expired active lease, left by a crash, as closed"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (niwa-migrate-session--write
     lease-file (plist-put (niwa-migrate-session--read lease-file) :status 'active))
    (should (niwa-migrate-session-copy source destination)))

  :doc "refuses corrupt, unsupported, live and unsafe sources without changing them"
  (dolist (fault '(version checksum path active unsettled missing-lease lock recovery symlink))
    (niwa-migration-test--with-source niwa-session-codec-format-version
      (pcase fault
        ;; The published head's sidecar; a stale fixed cache would be dropped.
        ('version
         (let* ((data ".publications/generation-bbbbbbbbbbbbbbbbbbbb/000001.data")
                (file (file-name-concat source head))
                (manifest (niwa-migrate-session--read file)))
           (niwa-migrate-session--write (file-name-concat source data)
                                           (plist-put (copy-tree old) :version "v0.5.0"))
           (setf (plist-get (cdar (plist-get manifest :artifacts)) :sha256)
                 (niwa-migrate-session--hash (file-name-concat source data)))
           (niwa-migrate-session--write file manifest)))
        ('checksum (with-temp-file (file-name-concat source ".publications/generation-aaaaaaaaaaaaaaaaaaaa/000002.data")
                     (insert "corrupt")))
        ('path (let* ((file (file-name-concat source head))
                      (manifest (niwa-migrate-session--read file)))
                 (setf (plist-get (cdar (plist-get manifest :artifacts)) :published) "../../outside")
                 (niwa-migrate-session--write file manifest)))
        ((or 'active 'unsettled)
         (let ((lease (niwa-migrate-session--read lease-file)))
           ;; A live holder still renews its lease.
           (setq lease (if (eq fault 'active)
                           (plist-put (plist-put lease :status 'active)
                                      :expires-at (+ (float-time) 600))
                         (plist-put lease :unsettled-mutation t)))
           (niwa-migrate-session--write lease-file lease)))
        ('missing-lease (delete-directory (file-name-concat source ".lease") t))
        ('lock (with-temp-file (file-name-concat source ".lock") (insert "held")))
        ('recovery (make-directory (file-name-concat source ".recovery"))
                   (with-temp-file (file-name-concat source ".recovery/pending") (insert "repair")))
        ('symlink (make-symbolic-link root (file-name-concat source "escape"))))
      (let ((before (niwa-migrate-session--hash (file-name-concat source "session.meta.eld"))))
        (should-error (niwa-migrate-session-copy source destination))
        (should-not (file-exists-p destination))
        (should (equal before (niwa-migrate-session--hash (file-name-concat source "session.meta.eld")))))))

  :doc "never overwrites an existing destination or copies into itself"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (make-directory destination)
    (should-error (niwa-migrate-session-copy source destination))
    (should (file-directory-p destination))
    (should-error (niwa-migrate-session-copy source (file-name-concat source "nested")))
    (should-not (file-exists-p (file-name-concat source "nested")))))

(niwa-deftest niwa-migrate-session--read ()
  (dolist (text '("#1=(:a #1#)" "(:a 1) (:b 2)" "(:a 1 :a 2)" "(setq migration-executed t)"))
    (let ((file (make-temp-file "niwa-migration-read-")))
      (unwind-protect
          (progn
            (with-temp-file file (insert text))
            (should-error (niwa-migrate-session--read file)))
        (delete-file file)))))

(niwa-deftest niwa-migrate-session-main (:quiet t)
  ,test
  (test)
  :doc "help needs no directory and invalid argument lists fail"
  (let ((command-line-args-left '("--" "--help")))
    (should (string-search "DIRECTORY" (with-output-to-string (niwa-migrate-session-main))))
    (should-not command-line-args-left))
  (dolist (args '(nil ("--") ("--" "one" "two") ("--unknown")))
    (let ((command-line-args-left args))
      (should-error (niwa-migrate-session-main)))))

(niwa-deftest niwa-migrate-sessions (:quiet t)
  ,test
  (test)
  :doc "in-place migration preserves original bytes in a backup and reruns are a no-op"
  (niwa-migration-test--with-source "v0.5.99"
    (let* ((niwa-migrate-session--steps niwa-migration-test--steps)
           (before (mapcar (lambda (file)
                            (cons (file-relative-name file source)
                                  (niwa-migrate-session--hash file)))
                          (directory-files-recursively source ".")))
           (row (cdar (niwa-migrate-sessions source)))
           (backup (plist-get row :backup)))
      (should (eq 'migrated (plist-get row :status)))
      (should (file-directory-p backup))
      (should-not (file-in-directory-p backup source))
      (dolist (entry before)
        (should (equal (cdr entry)
                       (niwa-migrate-session--hash (file-name-concat backup (car entry))))))
      (let ((files (directory-files root t))
            (attributes (niwa-migration-test--stable-attributes (file-name-concat source head))))
        (should (eq 'current (plist-get (cdar (niwa-migrate-sessions source)) :status)))
        (should (equal files (directory-files root t)))
        (should (equal attributes (niwa-migration-test--stable-attributes
                                   (file-name-concat source head)))))
      (should (equal "v0.5.99"
                     (plist-get (niwa-migrate-session--read
                                 (file-name-concat backup "session.meta.eld")) :version)))))

  :doc "already-current metadata is not rewritten or backed up"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (let ((before (directory-files root t))
          (attributes (niwa-migration-test--stable-attributes
                       (file-name-concat source "session.meta.eld"))))
      (should (eq 'current (plist-get (cdar (niwa-migrate-sessions source)) :status)))
      (should (equal before (directory-files root t)))
      (should (equal attributes (niwa-migration-test--stable-attributes
                                 (file-name-concat source "session.meta.eld"))))))

  :doc "project migration continues after unsupported sessions and leaves them unchanged"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (let* ((sessions (file-name-concat root ".niwa" "sessions"))
           (good (file-name-concat sessions "good"))
           (bad (file-name-concat sessions "unsupported")))
      (make-directory sessions t)
      (rename-file source good)
      (make-directory bad)
      (make-directory (file-name-concat sessions ".migration-backup-hidden"))
      (niwa-migrate-session--write (file-name-concat bad "session.meta.eld")
                                      '(:version "v0.5.16"))
      (let* ((before (niwa-migrate-session--hash (file-name-concat bad "session.meta.eld")))
             (report (niwa-migrate-sessions root)))
        (should (eq 'current (plist-get (cdr (assoc "good" report)) :status)))
        (should (eq 'failed (plist-get (cdr (assoc "unsupported" report)) :status)))
        (should (string-search "unsupported version v0.5.16"
                               (plist-get (cdr (assoc "unsupported" report)) :reason)))
        (should (equal before (niwa-migrate-session--hash
                               (file-name-concat bad "session.meta.eld")))))
      ;; Backups/staging must not become newly discoverable sessions.
      (should (equal '("good" "unsupported")
                     (mapcar #'car (niwa-migrate-sessions sessions))))))

  :doc "a project with a live session is refused before any other session is changed"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (let* ((sessions (file-name-concat root ".niwa" "sessions"))
           (good (file-name-concat sessions "a-closed"))
           (live (file-name-concat sessions "z-live")))
      (make-directory sessions t)
      (rename-file source good)
      (make-directory live)
      (niwa-migrate-session--write (file-name-concat live "session.meta.eld") old)
      (with-temp-file (file-name-concat live ".lock") (insert "held"))
      (let ((before (niwa-migrate-session--hash (file-name-concat good "session.meta.eld"))))
        (should-error (niwa-migrate-sessions root))
        (should (equal before (niwa-migrate-session--hash
                               (file-name-concat good "session.meta.eld")))))))

  :doc "installation failure restores the original instead of leaving its path missing"
  (niwa-migration-test--with-source "v0.5.99"
    (let ((niwa-migrate-session--steps niwa-migration-test--steps)
          (rename (symbol-function 'rename-file))
          (before (niwa-migrate-session--hash (file-name-concat source "session.meta.eld")))
          injected)
      (cl-letf (((symbol-function 'rename-file)
                 (lambda (from to &optional overwrite)
                   (if (and (not injected)
                            (equal (directory-file-name to) (directory-file-name source)))
                       (progn (setq injected t) (error "Injected install failure"))
                     (funcall rename from to overwrite)))))
        (let ((row (cdar (niwa-migrate-sessions source))))
          (should injected)
          (should (eq 'failed (plist-get row :status)))
          (should (string-search "Injected install failure" (plist-get row :reason)))))
      (should (equal before (niwa-migrate-session--hash
                             (file-name-concat source "session.meta.eld"))))))

  :doc "source changes during staging abort replacement"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (let ((copy (symbol-function 'niwa-migrate-session-copy))
          (chat (file-name-concat source ".publications/generation-bbbbbbbbbbbbbbbbbbbb/000002.data"))
          injected)
      (cl-letf (((symbol-function 'niwa-migrate-session-copy)
                 (lambda (&rest args)
                   (prog1 (apply copy args)
                     (unless injected
                       (setq injected t)
                       (write-region "\nConcurrent edit" nil chat t 'silent))))))
        (let ((row (cdar (niwa-migrate-sessions source))))
          (should injected)
          (should (eq 'failed (plist-get row :status)))))
      (should (equal niwa-session-codec-format-version
                     (plist-get (niwa-migrate-session--read
                                 (file-name-concat source "session.meta.eld")) :version)))
      (should (string-suffix-p "Concurrent edit"
                               (with-temp-buffer (insert-file-contents chat) (buffer-string))))))

  :doc "project state links are refused rather than migrating their target"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (let* ((state (file-name-concat root "elsewhere"))
           (sessions (file-name-concat state "sessions"))
           (actual (file-name-concat sessions "session")))
      (rename-file (file-name-concat root ".niwa") state)
      (make-directory sessions t)
      (rename-file source actual)
      (make-symbolic-link state (file-name-concat root ".niwa"))
      (let ((before (niwa-migrate-session--hash (file-name-concat actual "session.meta.eld"))))
        (should-error (niwa-migrate-sessions root))
        (should (equal before (niwa-migrate-session--hash
                               (file-name-concat actual "session.meta.eld")))))))

  :doc "an existing migration lock blocks replacement and is not removed by the contender"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (let ((lock (file-name-concat root ".migration-lock"))
          (before (niwa-migrate-session--hash (file-name-concat source "session.meta.eld"))))
      (make-directory lock)
      (should-error (niwa-migrate-sessions source))
      (should (file-directory-p lock))
      (should (equal before (niwa-migrate-session--hash
                             (file-name-concat source "session.meta.eld"))))))

  :doc "nil authoritative metadata is invalid, not an unpublished empty session"
  (dolist (authority '(fixed portable))
    (niwa-migration-test--with-source niwa-session-codec-format-version
      (if (eq authority 'fixed)
          (progn
            (delete-directory (file-name-concat source ".lease") t)
            (delete-directory (file-name-concat source ".publications") t)
            (niwa-migrate-session--write (file-name-concat source "session.meta.eld") nil))
        (let* ((file (file-name-concat source (file-name-directory head) "000001.data"))
               (manifest-file (file-name-concat source head))
               (manifest (niwa-migrate-session--read manifest-file)))
          (niwa-migrate-session--write file nil)
          (setf (plist-get (cdar (plist-get manifest :artifacts)) :sha256)
                (niwa-migrate-session--hash file))
          (niwa-migrate-session--write manifest-file manifest)))
      (let ((before (directory-files root t))
            (row (cdar (niwa-migrate-sessions source))))
        (should (eq 'failed (plist-get row :status)))
        (should (stringp (plist-get row :reason)))
        (should (equal before (directory-files root t))))))

  :doc "unsupported, future and missing versions fail without a source rewrite"
  (dolist (version '("v0.5.16" "v99.0.0" nil))
    (niwa-migration-test--with-source niwa-session-codec-format-version
      (delete-directory (file-name-concat source ".lease") t)
      (delete-directory (file-name-concat source ".publications") t)
      (niwa-migrate-session--write (file-name-concat source "session.meta.eld")
                                      (list :version version))
      (let ((before (directory-files root t))
            (digest (niwa-migrate-session--hash (file-name-concat source "session.meta.eld"))))
        (let ((row (cdar (niwa-migrate-sessions source))))
          (should (eq 'failed (plist-get row :status)))
          (should (string-search (format "unsupported version %s" version)
                                 (plist-get row :reason))))
        (should (equal before (directory-files root t)))
        (should (equal digest (niwa-migrate-session--hash
                               (file-name-concat source "session.meta.eld"))))))))

(niwa-deftest niwa-migrate-session-cli (:quiet t)
  ,test
  (test)
  :doc "real batch invocation accepts current sessions and rejects unsupported versions"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (let ((script (file-name-concat (file-name-directory (locate-library "niwa"))
                                   "scripts" "migrate-sessions.el")))
      (dolist (expected '(0 1))
        (when (= expected 1)
          (delete-directory (file-name-concat source ".lease") t)
          (delete-directory (file-name-concat source ".publications") t)
          (niwa-migrate-session--write (file-name-concat source "session.meta.eld")
                                          '(:version "v0.5.16")))
        (with-temp-buffer
          (let ((status (call-process
                         (expand-file-name invocation-name invocation-directory) nil t nil
                         "--batch" "-Q" "--eval"
                         (prin1-to-string `(setq load-path ',load-path debug-on-error nil))
                         "-l" script "-f" "niwa-migrate-session-main" "--" source)))
            (if (zerop expected)
                (progn
                  (should (zerop status))
                  (should (string-search "old: CURRENT" (buffer-string))))
              (should-not (zerop status))
              (should (string-search "old: FAILED" (buffer-string)))
              (should (string-search "unsupported version v0.5.16" (buffer-string))))))))))

(defun niwa-migration-test--respell-legacy (source)
  "Respell SOURCE, a current-format fixture, as the v0.6.0 layout.
Data files end `.el', and manifests and leases record those names.  This is
independent of the converter: plain text substitution, no shared helpers."
  (dolist (file (directory-files-recursively source "\\.eld\\(\\.1\\)?\\'"))
    (rename-file file (replace-regexp-in-string "\\.eld\\(\\.1\\)?\\'" ".el\\1" file)))
  (dolist (file (append (directory-files-recursively source "\\`manifest\\.el\\'")
                        (directory-files (file-name-concat source ".lease") t "\\.el\\'")))
    (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
      (with-temp-file file
        (insert (replace-regexp-in-string "\\.eld\"" ".el\"" text))))))

(defun niwa-migration-test--put (base relative &optional content)
  "Write CONTENT, default RELATIVE, to RELATIVE below BASE, creating parents."
  (let ((file (file-name-concat base relative)))
    (make-directory (file-name-directory file) t)
    (with-temp-file file (insert (or content relative)))
    file))

(defun niwa-migration-test--text (file)
  "Return FILE's content, or nil when it does not exist."
  (when (file-exists-p file)
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(niwa-deftest niwa-migrate-session-v0.6.0 (:quiet t)
  ,test
  (test)
  :doc "renames every data family, rewrites recorded names, and keeps all else"
  (niwa-migration-test--with-source "v0.6.0"
    (niwa-migration-test--respell-legacy source)
    (let ((families '("telemetry-log.el" "telemetry-log.el.1" "hook-log.el"
                      "permission-log.el" "repair-log.el" "segment-repair-backups.el"
                      "segment-repair-backups.el.1"
                      "tool-results/media/media-0123abcd.el"
                      ".lease/fences/fence-00000000000000000001.el"
                      ".lease/requests/request-00000000000000000002.el"
                      ".lease/requests/decision-00000000000000000002.el"))
          (kept '("plans/plan.md" "tool-results/executions/execution-A.log"
                  "file-history/old.el" "notes.el" "hooks.el" "agents/a/segment.chat.org")))
      (dolist (name (append families kept))
        (niwa-migration-test--put source name))
      (let* ((row (cdar (niwa-migrate-sessions source)))
             (backup (plist-get row :backup))
             (lease (niwa-migrate-session--read
                     (file-name-concat source ".lease/00000000000000000001.eld")))
             (manifest (niwa-migrate-session--read (file-name-concat source head))))
        (should (eq 'migrated (plist-get row :status)))
        (dolist (name families)
          (let ((new (replace-regexp-in-string "\\.el\\(\\.1\\)?\\'" ".eld\\1" name)))
            (should (equal name (niwa-migration-test--text (file-name-concat source new))))
            (should-not (file-exists-p (file-name-concat source name)))
            (should (equal name (niwa-migration-test--text (file-name-concat backup name))))))
        (dolist (name kept)
          (should (equal name (niwa-migration-test--text (file-name-concat source name)))))
        (should (equal head (plist-get lease :publication-head)))
        (should (equal "session.meta.eld" (plist-get manifest :sidecar)))
        (should (equal '("session.meta.eld" "segment-0001.chat.org")
                       (mapcar #'car (plist-get manifest :artifacts))))
        ;; Only the unmigrated kept controls still end `.el'.
        (should (equal (sort (copy-sequence '("file-history/old.el" "hooks.el" "notes.el")) #'string<)
                       (sort (mapcar (lambda (file) (file-relative-name file source))
                                     (directory-files-recursively source "\\.el\\'"))
                             #'string<)))
        ;; The current runtime reads the converted session.
        (should (plist-get (niwa-session-publication-read source head) :sidecar))
        (should (equal niwa-session-codec-format-version
                       (plist-get (niwa-migrate-session--read
                                   (plist-get (niwa-session-publication-read source head)
                                              :sidecar))
                                  :version)))
        (should (eq 'current (plist-get (cdar (niwa-migrate-sessions source)) :status))))))

  :doc "renames the logical name of a published media artifact"
  (niwa-migration-test--with-source "v0.6.0"
    (let* ((logical "tool-results/media/media-0123abcd.el")
           (manifest-file (file-name-concat source ".publications/generation-bbbbbbbbbbbbbbbbbbbb/manifest.eld"))
           (manifest (niwa-migrate-session--read manifest-file))
           (data ".publications/generation-bbbbbbbbbbbbbbbbbbbb/000003.data"))
      (niwa-migration-test--put source data "media")
      (setf (plist-get manifest :artifacts)
            (append (plist-get manifest :artifacts)
                    (list (list logical :published data :sha256
                                (niwa-migrate-session--hash (file-name-concat source data))))))
      (niwa-migrate-session--write manifest-file manifest)
      (niwa-migration-test--respell-legacy source)
      (niwa-migration-test--put source logical "media")
      (let ((row (cdar (niwa-migrate-sessions source))))
        (should (eq 'migrated (plist-get row :status)))
        (should (member (concat logical "d")
                        (mapcar #'car (plist-get (niwa-migrate-session--read
                                                  (file-name-concat source head))
                                                 :artifacts)))))))

  :doc "refuses a session that a pending journal capture still pins"
  (niwa-migration-test--with-source "v0.6.0"
    (niwa-migration-test--respell-legacy source)
    (niwa-migration-test--put source (concat ".journal-pins/" (make-string 64 ?a) ".json") "{}")
    (let* ((before (niwa-migrate-session--snapshot source))
           (row (cdar (niwa-migrate-sessions source))))
      (should (eq 'failed (plist-get row :status)))
      (should (string-search "pending journal capture" (plist-get row :reason)))
      (should (equal before (niwa-migrate-session--snapshot source)))))

  :doc "refuses when a family file exists under both spellings"
  (niwa-migration-test--with-source "v0.6.0"
    (niwa-migration-test--respell-legacy source)
    (niwa-migration-test--put source "hook-log.el")
    (niwa-migration-test--put source "hook-log.eld")
    (let* ((before (niwa-migrate-session--snapshot source))
           (row (cdar (niwa-migrate-sessions source))))
      (should (eq 'failed (plist-get row :status)))
      (should (string-search "Both" (plist-get row :reason)))
      (should (equal before (niwa-migrate-session--snapshot source)))))

  :doc "a live v0.6.0 session blocks the project before another session changes"
  (niwa-migration-test--with-source "v0.6.0"
    (niwa-migration-test--respell-legacy source)
    (let* ((sessions (file-name-concat root ".niwa" "sessions"))
           (closed (file-name-concat sessions "a-closed"))
           (live (file-name-concat sessions "z-live"))
           (lease (file-name-concat live ".lease/00000000000000000001.el")))
      (make-directory sessions t)
      (copy-directory source live nil t t)
      (rename-file source closed)
      (niwa-migrate-session--write
       lease (plist-put (plist-put (niwa-migrate-session--read lease) :status 'active)
                        :expires-at (+ (float-time) 600)))
      (let ((before (niwa-migrate-session--snapshot closed)))
        (should-error (niwa-migrate-sessions root))
        (should (equal before (niwa-migrate-session--snapshot closed))))))

  :doc "a session that never published is renamed and reported as migrated"
  (let* ((root (make-temp-file "niwa-migration-" t))
         (source (file-name-concat root "never")))
    (unwind-protect
        (progn
          (niwa-migrate-session--write
           (niwa-migration-test--put source ".lease/00000000000000000001.el")
           (list :generation 1 :transfer-generation 1 :status 'released
                 :publication-head nil :unsettled-mutation nil
                 :client-id (make-string 64 ?a) :renewed-at 1 :expires-at 2))
          (niwa-migration-test--put source "hook-log.el")
          (let ((row (cdar (niwa-migrate-sessions source))))
            (ert-info ((prin1-to-string row))
              (should (eq 'migrated (plist-get row :status)))))
          (should (file-exists-p (file-name-concat source ".lease/00000000000000000001.eld")))
          (should (file-exists-p (file-name-concat source "hook-log.eld")))
          ;; Migrated, it is the ordinary unpublished session again.
          (should (eq 'empty (plist-get (cdar (niwa-migrate-sessions source)) :status))))
      (delete-directory root t))))

(defconst niwa-migration-test--workspace-families
  `("permissions.el" "input-history.el" "instructions.el" "plugins.el" "skills-state.el"
    "artifacts/.state/doc/meta.el" "artifacts/.state/doc/versions/index.el"
    "schedules/s1/schedule.el" "schedules/s1/runs/1700000000.el"
    "leases/artifacts/doc/00000000000000000001.el"
    "leases/artifacts/doc/requests/request-00000000000000000002.el"
    "leases/instructions/00000000000000000003.el"
    "state/diagnostics/telemetry-log.el" "state/diagnostics/telemetry-log.el.1"
    "state/shared-notes/cycle.el" "state/shared-notes/observation.el"
    ,(concat "state/journal/passes/" (make-string 64 ?a) "/prepared.el")
    ,(concat "state/journal/passes/" (make-string 64 ?a) "/accepted.el")
    ,(concat "state/journal/decision-slots/" (make-string 64 ?b) ".el")
    ,(concat "state/journal/decisions/" (make-string 64 ?b) ".el")
    ,(concat "state/journal/writes/" (make-string 64 ?b) ".el"))
  "Workspace data files of v0.6.0, relative to `.niwa'.")

(defconst niwa-migration-test--workspace-kept
  `("hooks.el" "sessions/s/notes.el" "shared/example.el" "artifacts/doc/index.html"
    "leases/artifacts/doc/notes.el"
    "state/media/clipboard-1-1.png" "skills/s/SKILL.md"
    ,(concat "state/journal/coverage/" (make-string 64 ?c) ".json")
    ,(concat "state/journal/captures/" (make-string 64 ?d) "/seal.json"))
  "Files a workspace migration must leave alone, relative to `.niwa'.")

(niwa-deftest niwa-migrate-workspace (:quiet t)
  ,test
  (test)
  :doc "renames every workspace data family and keeps everything else"
  (let* ((root (make-temp-file "niwa-migration-workspace-" t))
         (state (file-name-concat root ".niwa")))
    (unwind-protect
        (progn
          (dolist (name (append niwa-migration-test--workspace-families
                                niwa-migration-test--workspace-kept))
            (niwa-migration-test--put state name))
          (let ((row (niwa-migrate-workspace root)))
            (should (eq 'migrated (plist-get row :status)))
            (should (= (length niwa-migration-test--workspace-families) (plist-get row :files))))
          (dolist (name niwa-migration-test--workspace-families)
            (let ((new (replace-regexp-in-string "\\.el\\(\\.1\\)?\\'" ".eld\\1" name)))
              (should (equal name (niwa-migration-test--text (file-name-concat state new))))
              (should-not (file-exists-p (file-name-concat state name)))))
          (dolist (name niwa-migration-test--workspace-kept)
            (should (equal name (niwa-migration-test--text (file-name-concat state name)))))
          (should (eq 'current (plist-get (niwa-migrate-workspace root) :status))))
      (delete-directory root t)))

  :doc "a file under both spellings refuses the pass before any rename"
  (let* ((root (make-temp-file "niwa-migration-workspace-" t))
         (state (file-name-concat root ".niwa")))
    (unwind-protect
        (progn
          (niwa-migration-test--put state "input-history.el")
          (niwa-migration-test--put state "permissions.el")
          (niwa-migration-test--put state "permissions.eld")
          (should-error (niwa-migrate-workspace root))
          (should (file-exists-p (file-name-concat state "input-history.el")))
          (should-not (file-exists-p (file-name-concat state "input-history.eld")))
          (should-not (file-exists-p (file-name-concat state ".migration-lock"))))
      (delete-directory root t)))

  :doc "a conflict or an unreadable expiry record refuses before any write"
  (dolist (fault '(conflict unreadable))
    (let* ((root (make-temp-file "niwa-migration-workspace-" t))
           (state (file-name-concat root ".niwa"))
           (pass (make-string 64 ?a))
           (record (concat "state/journal/expiry/" (make-string 64 ?f) ".json"))
           (text (format "{\"generation\":1,\"entries\":[{\"private\":[{\"file\":\"passes/%s/prepared.el\",\"sha256\":\"x\"}]}]}"
                         pass)))
      (unwind-protect
          (progn
            (niwa-migration-test--put state (concat "state/journal/passes/" pass "/prepared.el"))
            (niwa-migration-test--put state "input-history.el")
            (niwa-migration-test--put state record text)
            (if (eq fault 'conflict)
                (progn (niwa-migration-test--put state "permissions.el")
                       (niwa-migration-test--put state "permissions.eld"))
              (niwa-migration-test--put state (concat "state/journal/expiry/" (make-string 64 ?e) ".json")
                                        "{not json"))
            (should-error (niwa-migrate-workspace root))
            (should (equal text (niwa-migration-test--text (file-name-concat state record))))
            (should (file-exists-p (file-name-concat state "input-history.el")))
            (should (file-exists-p (file-name-concat state "state/journal/passes" pass "prepared.el"))))
        (delete-directory root t))))

  :doc "a live session refuses a direct workspace pass"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (let ((sessions (file-name-concat root ".niwa" "sessions")))
      (make-directory sessions t)
      (rename-file source (file-name-concat sessions "live"))
      (with-temp-file (file-name-concat sessions "live" ".lock") (insert "held"))
      (niwa-migration-test--put (file-name-concat root ".niwa") "permissions.el")
      (should-error (niwa-migrate-workspace root))
      (should (file-exists-p (file-name-concat root ".niwa" "permissions.el")))))

  :doc "a pending journal capture refuses the pass; a settled one does not"
  (let* ((root (make-temp-file "niwa-migration-workspace-" t))
         (state (file-name-concat root ".niwa"))
         (capture (concat "state/journal/captures/" (make-string 64 ?e) "/")))
    (unwind-protect
        (progn
          (niwa-migration-test--put state "permissions.el")
          (niwa-migration-test--put state (concat capture "ready"))
          (let ((error (should-error (niwa-migrate-workspace root))))
            (should (string-search "pending" (error-message-string error))))
          (should (file-exists-p (file-name-concat state "permissions.el")))
          (niwa-migration-test--put state (concat capture "retired"))
          (should (eq 'migrated (plist-get (niwa-migrate-workspace root) :status)))
          (should (file-exists-p (file-name-concat state "permissions.eld"))))
      (delete-directory root t)))

  :doc "a linked project state directory is refused"
  (let* ((root (make-temp-file "niwa-migration-workspace-" t))
         (elsewhere (file-name-concat root "elsewhere")))
    (unwind-protect
        (progn
          (niwa-migration-test--put elsewhere "permissions.el")
          (make-symbolic-link elsewhere (file-name-concat root ".niwa"))
          (should-error (niwa-migrate-workspace root))
          (should (file-exists-p (file-name-concat elsewhere "permissions.el"))))
      (delete-directory root t)))

  :doc "a project run reports its workspace after its sessions"
  (niwa-migration-test--with-source niwa-session-codec-format-version
    (let ((sessions (file-name-concat root ".niwa" "sessions")))
      (make-directory sessions t)
      (rename-file source (file-name-concat sessions "one"))
      (niwa-migration-test--put (file-name-concat root ".niwa") "permissions.el")
      (let ((report (niwa-migrate-project root)))
        (should (equal '("one" "workspace") (mapcar #'car report)))
        (should (eq 'migrated (plist-get (cdr (assoc "workspace" report)) :status)))
        (should (file-exists-p (file-name-concat root ".niwa" "permissions.eld")))))))

(niwa-deftest niwa-migrate-user (:quiet t)
  ,test
  (test)
  :doc "renames user data families and leaves plugins, hooks and libraries alone"
  (let* ((root (make-temp-file "niwa-migration-user-" t))
         (niwa-user-dir (file-name-concat root "user/"))
         (xdg (file-name-concat root "xdg"))
         (process-environment (cons (concat "XDG_STATE_HOME=" xdg) process-environment))
         (md5 (md5 "x"))
         (families (list "permissions.el" "trusted-hooks.el" "lobbies.el"
                         "plugin-update-check.el" (concat "wake-ups/" md5 ".el")
                         (concat "workspace-schedules/" md5 ".el")))
         (kept '("hooks.el" "hooks.json" "remote-artifacts.eld" "plugins/p/lib.el"
                 "whiteboard-libraries/l.el" "wake-ups/other.txt")))
    (unwind-protect
        (progn
          (dolist (name (append families kept))
            (niwa-migration-test--put niwa-user-dir name))
          (niwa-migration-test--put xdg "niwa/remote-notes.el")
          (niwa-migration-test--put xdg "niwa/other.el")
          (let ((row (niwa-migrate-user)))
            (should (eq 'migrated (plist-get row :status)))
            (should (= 7 (plist-get row :files))))
          (dolist (name families)
            (should (equal name (niwa-migration-test--text
                                 (file-name-concat niwa-user-dir (concat name "d")))))
            (should-not (file-exists-p (file-name-concat niwa-user-dir name))))
          (should (equal "niwa/remote-notes.el"
                         (niwa-migration-test--text (file-name-concat xdg "niwa/remote-notes.eld"))))
          (dolist (name kept)
            (should (equal name (niwa-migration-test--text (file-name-concat niwa-user-dir name)))))
          (should (file-exists-p (file-name-concat xdg "niwa/other.el")))
          (should (eq 'current (plist-get (niwa-migrate-user) :status))))
      (delete-directory root t)))

  :doc "absent state directories are already current"
  (let* ((root (make-temp-file "niwa-migration-user-" t))
         (niwa-user-dir (file-name-concat root "missing/"))
         (process-environment (cons (concat "XDG_STATE_HOME=" (file-name-concat root "nowhere"))
                                    process-environment)))
    (unwind-protect
        (should (eq 'current (plist-get (niwa-migrate-user) :status)))
      (delete-directory root t)))

  :doc "an existing migration lock blocks the pass"
  (let* ((root (make-temp-file "niwa-migration-user-" t))
         (niwa-user-dir (file-name-concat root "user/")))
    (unwind-protect
        (progn
          (niwa-migration-test--put niwa-user-dir "permissions.el")
          (make-directory (file-name-concat niwa-user-dir ".migration-lock"))
          (should-error (niwa-migrate-user))
          (should (file-exists-p (file-name-concat niwa-user-dir "permissions.el")))
          (should (file-directory-p (file-name-concat niwa-user-dir ".migration-lock"))))
      (delete-directory root t))))

(niwa-deftest niwa-migrate-session-user-cli (:quiet t)
  ,test
  (test)
  :doc "a real batch invocation with --user renames the user state under HOME"
  (let* ((root (make-temp-file "niwa-migration-user-cli-" t))
         (script (file-name-concat (file-name-directory (locate-library "niwa"))
                                   "scripts" "migrate-sessions.el"))
         (process-environment (append (list (concat "HOME=" root)
                                            (concat "XDG_STATE_HOME=" root "/state"))
                                      process-environment)))
    (unwind-protect
        (progn
          (niwa-migration-test--put (file-name-concat root ".niwa") "permissions.el")
          (with-temp-buffer
            (let ((status (call-process
                           (expand-file-name invocation-name invocation-directory) nil t nil
                           "--batch" "-Q" "--eval"
                           (prin1-to-string `(setq load-path ',load-path debug-on-error nil))
                           "-l" script "-f" "niwa-migrate-session-main" "--" "--user")))
              (should (zerop status))
              (should (string-search "user: MIGRATED (1 files renamed)" (buffer-string)))))
          (should (file-exists-p (file-name-concat root ".niwa" "permissions.eld"))))
      (delete-directory root t))))

(provide 'test-niwa-session-migration)
;;; test-niwa-session-migration.el ends here
