;;; migrate-sessions.el --- Explicit stepwise persisted-state migration -*- lexical-binding: t -*-

;;; Commentary:
;; Point this script at a closed local session, project, or sessions directory,
;; or give `--user' for the user-level state.  It uses explicit adjacent format
;; steps outside the runtime loader.  The chain starts at `v0.6.0', whose data
;; files end in `.el'; the current format stores Lisp data as `.eld'.  Each
;; changed session is validated in a private staging copy before replacing its
;; original; the original stays in a hidden sibling backup directory.  Current
;; sessions are untouched.  A project directory also renames its workspace
;; state; `--user' renames the state under `niwa-user-dir' and the XDG state
;; directory.  Keep niwa closed throughout migration.
;; Run from the repository root:
;; npx @emacs-eask/cli emacs --batch -L . -l scripts/migrate-sessions.el \
;;   -f niwa-migrate-session-main -- SESSION-OR-PROJECT-DIRECTORY
;; Pass `--user' instead of the directory for the user-level state.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'seq)
(require 'niwa-structs)
(require 'niwa-session-codec)
(require 'niwa-session-publication)

(defun niwa-migrate-session--read (file)
  "Read one literal plist from FILE, refusing circular or trailing data."
  (with-temp-buffer
    (insert-file-contents file)
    (let* ((read-circle t)
           (value (read (current-buffer)))
           (pending (list (cons nil value)))
           (visited (make-hash-table :test #'eq))
           keys)
      ;; Native sidecars share objects using #N=/#N#.  Permit sharing, but
      ;; reject actual cycles before copying or validating nested metadata.
      (while pending
        (pcase-let* ((`(,done . ,object) (pop pending)))
          (when (or (consp object) (vectorp object))
            (cond
             (done (puthash object 'done visited))
             ((eq (gethash object visited) 'visiting) (error "Cyclic metadata in %s" file))
             ((not (gethash object visited))
              (puthash object 'visiting visited)
              (push (cons t object) pending)
              (if (consp object)
                  (progn (push (cons nil (cdr object)) pending)
                         (push (cons nil (car object)) pending))
                (mapc (lambda (item) (push (cons nil item) pending)) object)))))))
      (unless (and (proper-list-p value) (cl-evenp (length value)))
        (error "Invalid metadata plist: %s" file))
      (cl-loop for (key _value) on value by #'cddr do
               (unless (and (keywordp key) (not (memq key keys)))
                 (error "Invalid or duplicate metadata key in %s" file))
               (push key keys))
      (forward-comment (point-max))
      (unless (eobp) (error "Trailing metadata in %s" file))
      value)))

(defun niwa-migrate-session--write (file value)
  "Write literal VALUE to FILE in the private destination copy."
  (let ((coding-system-for-write 'utf-8-unix)
        (print-length nil) (print-level nil) (print-circle t) (print-quoted t))
    (with-temp-file file (prin1 value (current-buffer)) (insert "\n"))))

(defun niwa-migrate-session--hash (file)
  "Return the SHA-256 of FILE's exact bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(define-error 'niwa-migrate-session-unconvertible
  "Session has nothing this converter reads")

(defconst niwa-migrate-session--steps
  '(("v0.6.0" "v0.7.0" identity niwa-migrate-session--v0.6.0-layout))
  "Ordered (SOURCE TARGET FUNCTION [LAYOUT]) format steps.
Each FUNCTION takes one sidecar plist.  An optional LAYOUT converts a
session's file names: called with a directory it returns the planned renames,
and with APPLY non-nil it performs them in that directory, a private copy.  It
plans nothing for a later layout.  Layouts run before the sidecar is read,
because the layout decides where the sidecar is.  Append a step when the
format changes; dropping the oldest step deletes its entry and its helpers.")

(defun niwa-migrate--eld-name (file)
  "Return FILE with its `.el', or `.el.1' rotation archive, suffix spelled `.eld'."
  (replace-regexp-in-string "\\.el\\(\\.1\\)?\\'" ".eld\\1" file))

(defun niwa-migrate--family-p (relative families)
  "Return non-nil when path RELATIVE matches one of the FAMILIES regexps."
  (cl-some (lambda (regexp) (string-match-p regexp relative)) families))

(defun niwa-migrate--moves (files base families)
  "Plan renaming those of FILES under BASE that FAMILIES name, to `.eld'.
FAMILIES are regexps over the path relative to BASE; a file no family names is
never touched.  Return (FROM . TO) pairs.  Signal when a target already exists,
so a caller can refuse before writing anything."
  (let (moves)
    (dolist (file files)
      (when (niwa-migrate--family-p (file-relative-name file base) families)
        (let ((target (niwa-migrate--eld-name file)))
          (when (file-exists-p target)
            (error "Both %s and %s exist; resolve them before migrating" file target))
          (push (cons file target) moves))))
    (nreverse moves)))

(defun niwa-migrate--apply-moves (moves)
  "Rename each (FROM . TO) of MOVES."
  (dolist (move moves)
    (rename-file (car move) (cdr move))))

(defun niwa-migrate--with-lock (lock function)
  "Call FUNCTION holding the migration LOCK directory, refusing a second run."
  (condition-case nil
      (make-directory lock)
    (file-already-exists
     (error "Migration lock already exists: %s; check for a running converter before removing it"
            lock)))
  (unwind-protect (funcall function)
    (delete-directory lock)))

;;;; v0.6.0 session layout

(defconst niwa-migrate-session--v0.6.0-families
  '("\\`session\\.meta\\.el\\'"
    "\\`\\.lease/[0-9]\\{20\\}\\.el\\'"
    "\\`\\.lease/\\(?:fences/fence\\|requests/\\(?:request\\|decision\\)\\)-[0-9]\\{20\\}\\.el\\'"
    "\\`\\.publications/generation-[0-9a-f]\\{20\\}/manifest\\.el\\'"
    "\\`\\(?:hook\\|permission\\|repair\\|telemetry\\)-log\\.el\\(?:\\.1\\)?\\'"
    "\\`segment-repair-backups\\.el\\(?:\\.1\\)?\\'"
    "\\`tool-results/media/media-[0-9a-f]+\\.el\\'")
  "Session files named `.el' in v0.6.0, as paths relative to the session.
Each becomes `.eld'.  Nothing else is renamed: transcripts, plans, execution
output and published `.data' files keep their names.")

(defun niwa-migrate-session--v0.6.0-name (name)
  "Return logical artifact NAME spelled `.eld' when v0.6.0 spelled it `.el'."
  (if (string-match-p
       "\\`\\(?:session\\.meta\\|tool-results/media/media-[0-9a-f]+\\)\\.el\\'" name)
      (concat name "d")
    name))

(defun niwa-migrate-session--v0.6.0-layout (directory &optional apply)
  "Return the planned v0.6.0 renames of session DIRECTORY.
With APPLY, perform them in DIRECTORY, a private copy, then rewrite the
logical names in each publication manifest and the head path in each lease
record.  Published `.data' files keep their names and bytes.  A session whose
data already ends `.eld' plans nothing.  Capture pins name v0.6.0 heads and
are bound to hashed capture descriptors, so a pinned session is refused."
  (let ((moves (niwa-migrate--moves (directory-files-recursively directory ".")
                                    directory niwa-migrate-session--v0.6.0-families))
        (pins (file-name-concat directory ".journal-pins")))
    (when (and moves (file-directory-p pins) (directory-files pins nil "\\.json\\'"))
      (error "A pending journal capture pins this session; let memory review settle it first"))
    (when (and apply moves)
      (niwa-migrate--apply-moves moves)
      (dolist (path (and (file-directory-p (file-name-concat directory ".publications"))
                         (directory-files-recursively
                          (file-name-concat directory ".publications") "\\`manifest\\.eld\\'")))
        (let ((manifest (niwa-migrate-session--read path)))
          (setf (plist-get manifest :sidecar)
                (niwa-migrate-session--v0.6.0-name (plist-get manifest :sidecar)))
          (setf (plist-get manifest :artifacts)
                (mapcar (lambda (entry)
                          (cons (niwa-migrate-session--v0.6.0-name (car entry)) (cdr entry)))
                        (plist-get manifest :artifacts)))
          (niwa-migrate-session--write path manifest)))
      (dolist (path (and (file-directory-p (file-name-concat directory ".lease"))
                         (directory-files (file-name-concat directory ".lease") t
                                          "\\`[0-9]\\{20\\}\\.eld\\'")))
        (let* ((lease (niwa-migrate-session--read path))
               (head (plist-get lease :publication-head)))
          (when head
            (setf (plist-get lease :publication-head)
                  (replace-regexp-in-string "/manifest\\.el\\'" "/manifest.eld" head t t))
            (niwa-migrate-session--write path lease)))))
    moves))

;;;; Session driver

(defun niwa-migrate-session--layouts (directory &optional apply)
  "Plan, or with APPLY perform, every step's layout change in DIRECTORY.
Return the planned renames."
  (cl-loop for step in niwa-migrate-session--steps
           when (nth 3 step) append (funcall (nth 3 step) directory apply)))

(defun niwa-migrate-session--sidecar (data)
  "Return DATA migrated step by step to the current runtime format.
Source DATA is never modified.  Unsupported versions fail rather than
guessing a path."
  (unless (or (null niwa-migrate-session--steps)
              (equal (cadar (last niwa-migrate-session--steps))
                     niwa-session-codec-format-version))
    (error "Migration chain does not target the current session format"))
  (setq data (copy-tree data))
  (let* ((version (plist-get data :version))
         (steps (member (assoc version niwa-migrate-session--steps)
                        niwa-migrate-session--steps)))
    (unless (or steps (equal version niwa-session-codec-format-version))
      (signal 'niwa-migrate-session-unconvertible
              (list (format "unsupported version %s; minimum supported is %s"
                            version (or (caar niwa-migrate-session--steps)
                                        niwa-session-codec-format-version)))))
    (dolist (step steps)
      (unless (equal (plist-get data :version) (car step))
        (error "Disconnected migration chain at %s" (car step)))
      (setq data (funcall (nth 2 step) data))
      (setq data (plist-put data :version (cadr step)))))
  (when-let* ((goal (plist-get data :goal)))
    (niwa-session-codec--goal-from-plist goal))
  (niwa-session-codec-validate-current-sidecar data))

(defun niwa-migrate-session--closed (directory &optional liveness-only)
  "Require closed DIRECTORY and return its exact lease snapshot.
A released lease is closed, and so is an active one whose holder stopped
renewing it: its expiry has passed, as after a crash.  Refuse recovery or
unsettled mutation rather than claiming to repair it.  With LIVENESS-ONLY,
only prove the session closed: an older layout's records need not validate,
since its private converted copy is validated in full."
  (when (file-exists-p (file-name-concat directory ".lock"))
    (error "Close the session and release its PID lock first"))
  (when-let* ((recovery (file-name-concat directory ".recovery"))
              ((file-directory-p recovery))
              ((directory-files recovery nil directory-files-no-dot-files-regexp)))
    (error "Resolve session recovery before migration"))
  (let* ((lease-dir (file-name-concat directory ".lease"))
         (files (and (file-directory-p lease-dir)
                     (directory-files lease-dir t "\\`[0-9]\\{20\\}\\.[a-z]+\\'")))
         (records (mapcar #'niwa-migrate-session--read (reverse files)))
         (latest (cl-find-if (lambda (row) (not (eq 'aborted (plist-get row :status)))) records)))
    (unless (or liveness-only (cl-every #'niwa-session-durability--valid-lease-p records))
      (error "Invalid portable lease record"))
    (when (and (file-directory-p lease-dir)
               (not (and latest
                         (not (plist-get latest :unsettled-mutation))
                         (or (eq 'released (plist-get latest :status))
                             ;; Local migration: the target clock is ours.
                             (and (eq 'active (plist-get latest :status))
                                  (numberp (plist-get latest :expires-at))
                                  (< (plist-get latest :expires-at) (float-time)))))))
      (error "Close the session and release its portable lease first"))
    ;; No head is a session closed before it ever published.
    (when-let* (((not liveness-only))
                (head (plist-get latest :publication-head)))
      (unless (and (stringp head)
                   (niwa-session-publication-valid-head-p head)
                   (file-regular-p (file-name-concat directory head)))
        (error "Missing or invalid published session head")))
    (mapcar (lambda (file) (cons (file-name-nondirectory file)
                               (niwa-migrate-session--hash file))) files)))

(defun niwa-migrate-session--head (lease source)
  "Return SOURCE's published head as a relative path, or nil.
LEASE is the snapshot `niwa-migrate-session--closed' returned."
  (when lease
    (let ((latest (cl-find-if (lambda (row) (not (eq 'aborted (plist-get row :status))))
                              (mapcar (lambda (entry)
                                        (niwa-migrate-session--read
                                         (file-name-concat source ".lease" (car entry))))
                                      (reverse lease)))))
      (plist-get latest :publication-head))))

(defun niwa-migrate-session-copy (source destination)
  "Copy closed local SOURCE to new DESTINATION and convert its metadata.
Retain session identity, transcripts, artifacts and every publication head.
Return the count of distinct converted sidecar files.  Failure removes only
the new copy.  The caller must keep SOURCE closed throughout conversion."
  (setq source (directory-file-name (expand-file-name source))
        destination (directory-file-name (expand-file-name destination)))
  (when (or (file-remote-p source) (file-remote-p destination))
    (error "Migration requires local directories"))
  (unless (file-directory-p source) (error "Session directory does not exist"))
  (when (or (file-exists-p destination) (file-symlink-p destination)
            (file-in-directory-p destination source))
    (error "Destination must be new and outside the source session"))
  (let* ((files (directory-files-recursively source "." t))
         (lease (niwa-migrate-session--closed source))
         (sidecars (make-hash-table :test #'equal))
         (hashes (make-hash-table :test #'equal))
         manifests success)
    (dolist (file (cons source files))
      (when (or (file-symlink-p file)
                (not (or (file-directory-p file) (file-regular-p file))))
        (error "Session contains a link or special file: %s" file)))
    (make-directory destination)
    (set-file-modes destination #o700)
    (unwind-protect
        (progn
          (copy-directory source (file-name-as-directory destination) t nil t)
          (set-file-modes destination #o700)
          (dolist (file files)
            (let ((relative (file-relative-name file source)))
              (when (string-match-p
                     "\\`\\.publications/generation-[[:xdigit:]]\\{20\\}/manifest\\.eld\\'" relative)
                (let* ((path (file-name-concat destination relative))
                       (manifest (niwa-session-publication--validate-manifest
                                  (niwa-migrate-session--read path) path))
                       (entries (plist-get manifest :artifacts)))
                  ;; Collection may leave an older manifest naming files of a
                  ;; generation it removed.  Nothing can read that manifest;
                  ;; drop it from the copy, keeping its data files, which later
                  ;; manifests may still name.  The published head must be whole.
                  (unless (cl-every (lambda (entry)
                                      (file-regular-p
                                       (file-name-concat destination
                                                         (plist-get (cdr entry) :published))))
                                    entries)
                    (when (equal relative (niwa-migrate-session--head lease source))
                      (error "The published session head is incomplete"))
                    (delete-file path)
                    (setq entries nil manifest nil))
                  (dolist (entry entries)
                    (let* ((published (plist-get (cdr entry) :published))
                           (expected (plist-get (cdr entry) :sha256)))
                      (let* ((artifact (file-name-concat destination published))
                             (digest (or (gethash artifact hashes)
                                         (puthash artifact (niwa-migrate-session--hash artifact) hashes))))
                        (unless (equal expected digest)
                          (error "Artifact checksum mismatch: %s" published))
                        (when (equal (car entry) "session.meta.eld")
                          (puthash artifact (cons path (gethash artifact sidecars))
                                   sidecars)))))
                  (when manifest (push (cons path manifest) manifests))))))
          (let ((sidecar (file-name-concat destination "session.meta.eld")))
            (when (file-regular-p sidecar) (puthash sidecar 'fixed sidecars)))
          (when (zerop (hash-table-count sidecars))
            (signal 'niwa-migrate-session-unconvertible (list "no session metadata")))
          (let ((head (when-let* ((relative (niwa-migrate-session--head lease source)))
                        (file-name-concat destination relative)))
                dropped)
            (maphash
             (lambda (file owners)
               (condition-case err
                   (let ((data (niwa-migrate-session--sidecar
                                (niwa-migrate-session--read file))))
                     (when (and (eq 'portable (plist-get data :authority-mode)) (not lease))
                       (error "Portable session is missing its released lease"))
                     (niwa-migrate-session--write file data))
                 (error
                  ;; An old retained generation whose sidecar no reader
                  ;; accepts, or a portable session's stale fixed cache, is
                  ;; dropped; authoritative sidecars must validate.
                  (if (or (and (eq owners 'fixed) (not lease))
                          (and (listp owners) (member head owners)))
                      (signal (car err) (cdr err))
                    (if (eq owners 'fixed)
                        (delete-file file)
                      (dolist (manifest owners)
                        (when (file-exists-p manifest) (delete-file manifest))
                        (setq manifests (cl-remove manifest manifests
                                                   :key #'car :test #'equal))))
                    (push file dropped)))))
             sidecars)
            (dolist (file dropped) (remhash file sidecars)))
          (clrhash hashes)
          (dolist (row manifests)
            (dolist (entry (plist-get (cdr row) :artifacts))
              (let ((file (file-name-concat destination (plist-get (cdr entry) :published))))
                (when (gethash file sidecars)
                  (setf (plist-get (cdr entry) :sha256)
                        (or (gethash file hashes)
                            (puthash file (niwa-migrate-session--hash file) hashes))))))
            (niwa-migrate-session--write (car row) (cdr row)))
          (unless (equal lease (niwa-migrate-session--closed source))
            (error "Source lease changed during migration; keep it closed and retry"))
          (setq success t)
          (hash-table-count sidecars))
      (unless success (delete-directory destination t)))))

(defun niwa-migrate-session--snapshot (directory)
  "Return a sorted content and metadata fingerprint of safe DIRECTORY.
Refuse links and special files before reading or copying their contents."
  (let (snapshot)
    (dolist (file (cons directory (directory-files-recursively directory "." t)))
      (when (or (file-symlink-p file)
                (not (or (file-directory-p file) (file-regular-p file))))
        (error "Session contains a link or special file: %s" file))
      (let ((attributes (file-attributes file 'integer)))
        (push (list (file-relative-name file directory)
                    (file-attribute-type attributes)
                    (file-attribute-modes attributes)
                    (file-attribute-modification-time attributes)
                    (unless (file-directory-p file)
                      (niwa-migrate-session--hash file)))
              snapshot)))
    (sort snapshot (lambda (a b) (string< (car a) (car b))))))

(defun niwa-migrate-session--directories (directory)
  "Resolve local DIRECTORY to a list of session directories.
Accept one session, a project root, or its `.niwa/sessions' container."
  (when (file-remote-p directory)
    (error "Migration requires local directories"))
  (setq directory (directory-file-name (expand-file-name directory)))
  (unless (file-directory-p directory)
    (error "Migration directory does not exist: %s" directory))
  (let ((path directory))
    (while path
      (when (file-symlink-p path)
        (error "Migration directory contains a symbolic link: %s" path))
      (setq path (unless (equal path "/")
                   (directory-file-name (file-name-directory path))))))
  (cond
   ((or (directory-files directory nil "\\`session\\.meta\\.")
        (file-directory-p (file-name-concat directory ".lease"))
        (file-directory-p (file-name-concat directory ".publications")))
    (list directory))
   ((file-directory-p (file-name-concat directory ".niwa"))
    (when (file-symlink-p (file-name-concat directory ".niwa"))
      (error "Project state directory is a symbolic link"))
    (let ((sessions (file-name-concat directory ".niwa" "sessions")))
      (when (file-symlink-p sessions)
        (error "Sessions directory is a symbolic link: %s" sessions))
      (when (file-directory-p sessions)
        (cl-remove-if-not #'file-directory-p
                          (directory-files sessions t "\\`[^.]")))))
   ((and (equal (file-name-nondirectory directory) "sessions")
         (equal (file-name-nondirectory
                 (directory-file-name (file-name-directory directory))) ".niwa"))
    (cl-remove-if-not #'file-directory-p
                      (directory-files directory t "\\`[^.]")))
   (t (error "Expected a session directory or project directory: %s" directory))))

(defun niwa-migrate-session--authoritative-data (directory lease)
  "Read DIRECTORY's authoritative sidecar using captured LEASE.
Only a portable session released without ever publishing returns nil."
  (let ((sidecar
         (if (file-directory-p (file-name-concat directory ".lease"))
             (when-let* ((head (niwa-migrate-session--head lease directory)))
               (or (plist-get (niwa-session-publication-read directory head) :sidecar)
                   (error "Missing published session metadata")))
           (file-name-concat directory "session.meta.eld"))))
    (when sidecar
      (or (niwa-migrate-session--read sidecar)
          (error "Empty authoritative session metadata: %s" sidecar)))))

(defun niwa-migrate-session--install (source staged)
  "Replace SOURCE with validated STAGED and return the original's backup path.
An ordinary failed install restores SOURCE.  Never remove an original backup,
even when rollback itself fails."
  (let* ((parent (file-name-directory source))
         (name (file-name-nondirectory source))
         (container (make-temp-file
                     (file-name-concat parent (concat ".migration-backup-" name "-")) t))
         (backup (file-name-concat container "session"))
         (inhibit-quit t))
    (unwind-protect
        (progn
          (rename-file source backup)
          (condition-case err
              (rename-file staged source)
            (error
             (condition-case rollback
                 (rename-file backup source)
               (error
                (error "Install failed (%s); rollback failed (%s); original is at %s"
                       (error-message-string err) (error-message-string rollback) backup)))
             (signal (car err) (cdr err))))
          backup)
      (unless (file-exists-p backup)
        (delete-directory container)))))

(defun niwa-migrate-session--one (source)
  "Migrate one closed SOURCE and return its status plist.
A SOURCE in an older layout is first copied and converted by the steps'
layouts, so the rest of the conversion reads and validates the current one.
The caller owns serialization and reports ordinary failures per session."
  (let* ((before (niwa-migrate-session--snapshot source))
         (lease (niwa-migrate-session--closed source t))
         (relayout (niwa-migrate-session--layouts source))
         (container (make-temp-file
                     (file-name-concat (file-name-directory source) ".migration-stage-") t))
         (staged (file-name-concat container "session"))
         (work source))
    (cl-flet ((unchanged ()
                (unless (and (equal before (niwa-migrate-session--snapshot source))
                             (equal lease (niwa-migrate-session--closed source t)))
                  (error "Source changed during migration; keep it closed and retry"))))
      (unwind-protect
          (progn
            (when relayout
              (setq work (file-name-concat container "relayout"))
              (make-directory work)
              (set-file-modes work #o700)
              (copy-directory source (file-name-as-directory work) t nil t)
              (niwa-migrate-session--layouts work t))
            (let ((data (niwa-migrate-session--authoritative-data
                         work (niwa-migrate-session--closed work))))
              (cond
               ((and (null data) (not relayout)) (list :status 'empty))
               ((null data)
                ;; Never published: convert a fixed cache, or drop it like any
                ;; stale portable cache.
                (when-let* ((fixed (file-name-concat work "session.meta.eld"))
                            ((file-exists-p fixed)))
                  (condition-case nil
                      (niwa-migrate-session--write
                       fixed (niwa-migrate-session--sidecar (niwa-migrate-session--read fixed)))
                    (error (delete-file fixed))))
                (unchanged)
                (list :status 'migrated :backup (niwa-migrate-session--install source work)))
               (t
                (niwa-migrate-session--sidecar data)
                (niwa-migrate-session-copy work staged)
                (unchanged)
                (if (and (not relayout)
                         (equal (plist-get data :version) niwa-session-codec-format-version))
                    (list :status 'current)
                  (list :status 'migrated
                        :backup (niwa-migrate-session--install source staged)))))))
        (when (file-directory-p container)
          (delete-directory container t))))))

(defun niwa-migrate-sessions (directory)
  "Migrate supported sessions under local DIRECTORY to the current format.
DIRECTORY is a session, project, or `.niwa/sessions' directory.  Return
rows (NAME :status STATUS :backup PATH :reason TEXT), where
STATUS is migrated, current, empty, or failed.  Failed sessions stay in place;
independent sessions continue.  Unsafe ownership aborts before any writes."
  (let ((directories (niwa-migrate-session--directories directory)))
    (when directories
      ;; Refuse unsafe ownership in any selected session before writing.
      (dolist (source directories)
        (when (file-symlink-p source)
          (error "Session directory is a symbolic link: %s" source))
        (niwa-migrate-session--closed source t))
      (niwa-migrate--with-lock
       (file-name-concat (file-name-directory (car directories)) ".migration-lock")
       (lambda ()
         (mapcar (lambda (source)
                   (cons (file-name-nondirectory source)
                         (condition-case err
                             (niwa-migrate-session--one source)
                           (error (list :status 'failed :reason (error-message-string err))))))
                 directories))))))

;;
;;; Workspace and user state

(defconst niwa-migrate-workspace--v0.6.0-families
  '("\\`\\(?:permissions\\|input-history\\|instructions\\|plugins\\|skills-state\\)\\.el\\'"
    "\\`artifacts/\\.state/[^/]+/\\(?:meta\\|versions/index\\)\\.el\\'"
    "\\`schedules/[^/]+/\\(?:schedule\\|runs/[0-9]+\\)\\.el\\'"
    "\\`leases/\\(?:artifacts/[^/]+\\|instructions\\)/\\(?:[0-9]\\{20\\}\\|\\(?:fences/fence\\|requests/\\(?:request\\|decision\\)\\)-[0-9]\\{20\\}\\)\\.el\\'"
    "\\`state/diagnostics/telemetry-log\\.el\\(?:\\.1\\)?\\'"
    "\\`state/shared-notes/\\(?:cycle\\|observation\\)\\.el\\'"
    "\\`state/journal/passes/[0-9a-f]\\{64\\}/\\(?:prepared\\|accepted\\)\\.el\\'"
    "\\`state/journal/\\(?:decision-slots\\|decisions\\|writes\\)/[^/]+\\.el\\'")
  "Workspace files named `.el' in v0.6.0, relative to the workspace `.niwa'.
Expiry records name the journal ones; they are rewritten with them.")

(defconst niwa-migrate-user--v0.6.0-families
  '("\\`\\(?:permissions\\|trusted-hooks\\|lobbies\\|plugin-update-check\\)\\.el\\'"
    "\\`\\(?:wake-ups\\|workspace-schedules\\)/[0-9a-f]\\{32\\}\\.el\\'")
  "User files named `.el' in v0.6.0, relative to `niwa-user-dir'.")

(defun niwa-migrate--files (directory &optional skip)
  "Return the `.el' and `.el.1' files below DIRECTORY, or nil when it is absent.
Do not descend into the top-level directories named in SKIP."
  (when (file-directory-p directory)
    (directory-files-recursively
     directory "\\.el\\(?:\\.1\\)?\\'" nil
     (lambda (subdirectory)
       (not (member (file-relative-name subdirectory directory) skip))))))

(defun niwa-migrate--replace (file text)
  "Atomically replace FILE with TEXT, written as UTF-8."
  (let ((temporary (make-temp-file (concat file ".migrating-")))
        (coding-system-for-write 'utf-8-unix))
    (write-region text nil temporary nil 'silent)
    (rename-file temporary file t)))

(defun niwa-migrate--read-text (file)
  "Return FILE's exact content decoded as UTF-8."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents file))
    (buffer-string)))

(defun niwa-migrate-workspace--pending-captures (journal)
  "Return the ids of ready, unsettled journal captures under state dir JOURNAL.
A pending capture's descriptor hashes the v0.6.0 publication head, so it cannot
be renamed; memory review must settle it first."
  (let ((captures (file-name-concat journal "captures")) pending)
    (when (file-directory-p captures)
      (dolist (directory (directory-files captures t "\\`[0-9a-f]\\{64\\}\\'"))
        (when (and (file-exists-p (file-name-concat directory "ready"))
                   (not (file-exists-p (file-name-concat directory "retired")))
                   (not (file-exists-p (file-name-concat directory "discard.json"))))
          (push (file-name-nondirectory directory) pending))))
    pending))

(defun niwa-migrate-workspace--expiry-writes (journal)
  "Return the (FILE . TEXT) writes renaming private paths in JOURNAL's expiry records.
JOURNAL is the workspace's `state/journal' directory.  A private path is
renamed exactly when the workspace families rename its file.  An accepted
record is bound by hash: its claim outcome and completion receipt hold the
hash of its exact bytes, so they come first and accept either hash, letting
an interrupted run repeat.  Reads and parses every record before any write."
  (let ((expiry (file-name-concat journal "expiry")) writes)
    (when (file-directory-p expiry)
      (dolist (path (directory-files expiry t "\\`[0-9a-f]\\{64\\}\\.json\\'"))
        (let* ((text (niwa-migrate--read-text path))
               (object (json-parse-string text))
               changed)
          (seq-doseq (entry (gethash "entries" object))
            (seq-doseq (record (gethash "private" entry))
              (let ((file (gethash "file" record)))
                (when (niwa-migrate--family-p (concat "state/journal/" file)
                                              niwa-migrate-workspace--v0.6.0-families)
                  (puthash "file" (niwa-migrate--eld-name file) record)
                  (setq changed t)))))
          (when changed
            (let* ((new (decode-coding-string (json-serialize object) 'utf-8-unix))
                   (hashes (list (secure-hash 'sha256 text) (secure-hash 'sha256 new)))
                   (outcome (file-name-concat
                             journal "mutation"
                             (format "%020d.outcome" (gethash "generation" object))))
                   (done (concat path ".done")))
              (when (file-exists-p outcome)
                (let ((record (json-parse-string (niwa-migrate--read-text outcome))))
                  (when (member (gethash "payload" record) hashes)
                    (puthash "payload" (cadr hashes) record)
                    (push (cons outcome (decode-coding-string (json-serialize record) 'utf-8-unix))
                          writes))))
              (when (and (file-exists-p done) (member (niwa-migrate--read-text done) hashes))
                (push (cons done (cadr hashes)) writes))
              (push (cons path new) writes))))))
    (nreverse writes)))

(defun niwa-migrate-workspace (root)
  "Rename the persisted workspace state of project ROOT to `.eld'.
Return (:status migrated :files N :expiry M) or (:status current).  Refuse
before writing anything while a session is live or a journal capture is
pending, when both spellings of a file exist, or when a record is unreadable."
  (let* ((state (file-name-concat (directory-file-name (expand-file-name root)) ".niwa"))
         (journal (file-name-concat state "state" "journal")))
    (when (file-symlink-p state)
      (error "Project state directory is a symbolic link"))
    (unless (file-directory-p state)
      (error "No project state directory: %s" state))
    (dolist (session (niwa-migrate-session--directories root))
      (niwa-migrate-session--closed session t))
    (when-let* ((pending (niwa-migrate-workspace--pending-captures journal)))
      (error "%d journal captures are pending; let memory review settle them first"
             (length pending)))
    (niwa-migrate--with-lock
     (file-name-concat state ".migration-lock")
     (lambda ()
       (let ((moves (niwa-migrate--moves
                     (niwa-migrate--files state '("sessions" "worktrees" "shared" "memory"))
                     state niwa-migrate-workspace--v0.6.0-families))
             (writes (niwa-migrate-workspace--expiry-writes journal)))
         (dolist (write writes)
           (niwa-migrate--replace (car write) (cdr write)))
         (niwa-migrate--apply-moves moves)
         (let ((records (cl-count-if (lambda (write) (string-suffix-p ".json" (car write)))
                                     writes)))
           (if (or moves writes)
               (list :status 'migrated :files (length moves) :expiry records)
             (list :status 'current))))))))

(defun niwa-migrate-user ()
  "Rename the user-level persisted state to `.eld'.
Cover `niwa-user-dir' and the XDG state directory's `niwa' directory.  A batch
run reads neither init file nor customization, so point it at another user
directory with HOME or `--eval \\='(setq niwa-user-dir ...)'.
Return (:status migrated :files N) or (:status current)."
  (let* ((user (file-name-as-directory (expand-file-name niwa-user-dir)))
         (xdg (file-name-as-directory
               (expand-file-name "niwa" (or (getenv "XDG_STATE_HOME") "~/.local/state/"))))
         (moves (append
                 (niwa-migrate--moves
                  (append (niwa-migrate--files user '("plugins" "native" "whiteboard-libraries"
                                                      "claude-agent-logs" "skills" "memory")))
                  user niwa-migrate-user--v0.6.0-families)
                 (niwa-migrate--moves (niwa-migrate--files xdg) xdg
                                      '("\\`remote-notes\\.el\\'")))))
    (if (null moves)
        (list :status 'current)
      (niwa-migrate--with-lock
       (file-name-concat (if (file-directory-p user) user xdg) ".migration-lock")
       (lambda () (niwa-migrate--apply-moves moves)))
      (list :status 'migrated :files (length moves)))))

(defun niwa-migrate-project (directory)
  "Migrate sessions under DIRECTORY and, for a project root, its workspace state.
Return the session rows of `niwa-migrate-sessions', followed by a
\"workspace\" row when DIRECTORY is a project root."
  (let ((report (niwa-migrate-sessions directory)))
    (if (file-directory-p (file-name-concat directory ".niwa"))
        (append report
                (list (cons "workspace"
                            (condition-case err
                                (niwa-migrate-workspace directory)
                              (error (list :status 'failed
                                           :reason (error-message-string err)))))))
      report)))

(defun niwa-migrate-session-main ()
  "Migrate the DIRECTORY, or with `--user' the user state, from the command line."
  (let ((args command-line-args-left)
        (usage (concat "Usage: niwa-migrate-session-main -- DIRECTORY\n"
                       "       niwa-migrate-session-main -- --user\n"
                       "DIRECTORY is a closed session, project, or .niwa/sessions directory;"
                       " a project also migrates its workspace state.\n"
                       "--user migrates the user-level state.\n")))
    (setq command-line-args-left nil)
    (when (equal (car args) "--") (setq args (cdr args)))
    (if (equal args '("--help"))
        (princ usage)
      (unless (= (length args) 1) (error "%s" (string-trim-right usage)))
      (let* ((report (if (equal args '("--user"))
                         (list (cons "user" (niwa-migrate-user)))
                       (niwa-migrate-project (car args))))
             (failed (cl-count-if (lambda (row) (eq (plist-get (cdr row) :status) 'failed))
                                  report)))
        (dolist (row report)
          (let* ((result (cdr row))
                 (status (plist-get result :status)))
            (princ (format "%s: %s%s\n" (car row) (upcase (symbol-name status))
                           (pcase status
                             ('migrated
                              (if (plist-get result :backup)
                                  (format " (backup: %s)" (plist-get result :backup))
                                (format " (%d files renamed%s)" (plist-get result :files)
                                        (if (plist-get result :expiry)
                                            (format ", %d expiry records rewritten"
                                                    (plist-get result :expiry))
                                          ""))))
                             ('failed (format " (%s)" (plist-get result :reason)))
                             (_ ""))))))
        (princ (format "%d items checked; %d failed; target %s.\n"
                       (length report) failed niwa-session-codec-format-version))
        (when (> failed 0)
          (error "%d items failed migration checks; see above" failed))))))

(provide 'niwa-migrate-session)
;;; migrate-sessions.el ends here
