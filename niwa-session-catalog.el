;;; niwa-session-catalog.el --- List and clean up saved sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Discover saved sessions, present the session chooser, and remove stale
;; sessions under the existing persistence entry points.

;;; Code:

(require 'cl-lib)
(require 'niwa-session-artifacts)
(require 'niwa-session-codec)
(require 'niwa-session-durability)
(require 'niwa-structs)
(require 'niwa-utilities)

;; `niwa-artifact-store'
(declare-function niwa-artifact-store-dedicated-p
                  "niwa-artifact-store" (workspace session-id id))
(autoload 'niwa-artifact-store-dedicated-p "niwa-artifact-store")

;; `niwa-execution-target'
(declare-function niwa-execution-target-create "niwa-execution-target" (directory))
(declare-function niwa-execution-target-incarnation-changed-p "niwa-execution-target" (cl-x) t)
(declare-function niwa-execution-target-observe-incarnation "niwa-execution-target" (target))
(declare-function niwa-execution-target-probe "niwa-execution-target" (target &optional refresh sandbox-mode))
(declare-function niwa-execution-target-remote-p "niwa-execution-target" (target))
(declare-function niwa-execution-target-restore-incarnation "niwa-execution-target" (target incarnation))
(autoload 'niwa-execution-target-create "niwa-execution-target")
(autoload 'niwa-execution-target-observe-incarnation "niwa-execution-target")
(autoload 'niwa-execution-target-probe "niwa-execution-target")
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")
(autoload 'niwa-execution-target-restore-incarnation "niwa-execution-target")

;; `niwa-journal-capture'
(declare-function niwa-journal-capture-read "niwa-journal-capture" (workspace id &optional observation))
(autoload 'niwa-journal-capture-read "niwa-journal-capture")

;; `niwa-journal-pins'
(declare-function niwa-journal-pins-observe "niwa-journal-pins" (session-dir))
(declare-function niwa-journal-pins-present-p "niwa-journal-pins" (session-dir))
(autoload 'niwa-journal-pins-present-p "niwa-journal-pins")
(autoload 'niwa-journal-pins-observe "niwa-journal-pins")

;; `niwa-journal-process'
(declare-function niwa-journal-process-discard "niwa-journal-process" (workspace id &optional original-source))
(autoload 'niwa-journal-process-discard "niwa-journal-process")

;; `niwa-persistence'
(declare-function niwa-instruction-store-note-session-ids "niwa-persistence" (workspace))

;; `niwa-session-control-fs'
(declare-function niwa-session-control-fs-async-busy-p "niwa-session-control-fs" (directory))
(declare-function niwa-session-control-fs-physical-path "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-program-value "niwa-session-control-fs" (result))
(declare-function niwa-session-control-fs-run-program "niwa-session-control-fs" (operations &optional lock-directory))
(declare-function niwa-session-control-fs-tree-sizes "niwa-session-control-fs" (paths))
(autoload 'niwa-session-control-fs-physical-path
  "niwa-session-control-fs")

;; `niwa-session-control-transfer'
(declare-function niwa-session-control-transfer-root-buffer-for-id "niwa-session-control-transfer" (session-id))
(autoload 'niwa-session-control-transfer-root-buffer-for-id
  "niwa-session-control-transfer")

;; `niwa-session-persistence'
(autoload 'niwa-session-persistence--classify-sidecar "niwa-session-persistence")
(autoload 'niwa-session-persistence--classify-sidecar-data "niwa-session-persistence")
(autoload 'niwa-session-persistence-load-sidecar "niwa-session-persistence")
(autoload 'niwa-session-persistence-restore "niwa-session-persistence")

;; `niwa-session-publication'
(declare-function niwa-session-publication-read "niwa-session-publication" (session-dir &optional head names))
(declare-function niwa-session-publication-read-batch
                  "niwa-session-publication" (directories listings))
(autoload 'niwa-session-publication-read "niwa-session-publication")
(autoload 'niwa-session-publication-read-batch "niwa-session-publication")

;; `niwa-session-recovery'
(declare-function niwa-session-recovery--abandon "niwa-session-recovery" (session))
(declare-function niwa-session-recovery-read "niwa-session-recovery" (session-dir &optional root-observation))
(autoload 'niwa-session-recovery--abandon "niwa-session-recovery")
(autoload 'niwa-session-recovery-read "niwa-session-recovery")

;; `niwa-session-rewind'
(declare-function niwa-session-rewind-format-relative-time "niwa-session-rewind" (iso))
(autoload 'niwa-session-rewind-format-relative-time
  "niwa-session-rewind")

;; `niwa-transport'
(declare-function niwa-transport-schedule-idle
                  "niwa-transport" (table key tag path thunk))
(autoload 'niwa-transport-schedule-idle "niwa-transport")

;; `niwa-workspace-identity'
(declare-function niwa-workspace-identity-read "niwa-workspace-identity" (root))
(autoload 'niwa-workspace-identity-read "niwa-workspace-identity")

;; `niwa-workspace-schedule'
(autoload 'niwa-workspace-schedule-cleanup-runs "niwa-workspace-schedule")

(defcustom niwa-session-max-age-days 30
  "Auto-cleanup threshold for old sessions, in days.

Sessions older than this are deleted by a sweep that runs when the
`niwa' session chooser lists sessions, after a session opens, after
a root turn ends, and when Emacs exits.  Each workspace is swept at
most once a day per Emacs, and only an interactive Emacs sweeps from
session activity or exit.  Age comes from `:updated-at', or the sidecar
or session directory modification time when metadata cannot provide it;
a portable session's last lease renewal must also be older.  Sessions
with a live, publishing, mutating or transfer-reserved lease are skipped.
Legacy pre-lease control state is always refused.  So are the
`niwa-session-keep-recent-count' most-recently-updated sessions.  A nil
value disables auto-cleanup entirely.  The idle activity sweep also expires
workspace schedule run records by settlement time, retaining the records
needed to check for overlapping runs."
  :type '(choice (integer :tag "Days")
          (const :tag "Disabled" nil))
  :group 'niwa)

(defcustom niwa-session-keep-recent-count 3
  "Number of most-recently-updated sessions exempt from auto-cleanup.

The newest sessions of a workspace, counted by the same timestamp
auto-cleanup ages against, survive `niwa-session-max-age-days'
regardless of age.  Without this floor a long absence would delete the
very session a returning user opens the chooser to resume: the chooser
sweeps expired sessions before listing them, and at that point the
session is not yet locked.  A nil value applies the age cap to every
session."
  :type '(choice (integer :tag "Sessions")
          (const :tag "No floor" nil))
  :group 'niwa)

;;
;;; Session listing & resume command

(defconst niwa-session-persistence--pre-lease-message
  "Session %s uses an unsupported transcript format (before portable leases)"
  "Diagnostic for an unsupported PID-lock session, given its directory.")

(defvar niwa-session-persistence--summary-cache
  (make-hash-table :test #'equal)
  "Classified session summaries keyed by sidecar path and file fingerprint.")

(defun niwa-session-persistence--sidecar-summary (sidecar)
  "Return roster fields extracted from current SIDECAR."
  (list :session-id (plist-get sidecar :session-id)
        :session-name (plist-get sidecar :session-name)
        :latest-late-delivery (plist-get sidecar :latest-late-delivery)
        :preset-name (plist-get sidecar :preset-name)
        :note-conversation (plist-get sidecar :note-conversation)
        :workspace (plist-get sidecar :workspace)
        :created-at (plist-get sidecar :created-at)
        :updated-at (plist-get sidecar :updated-at)
        :current-segment (plist-get sidecar :current-segment)
        :attached-artifacts (plist-get sidecar :attached-artifacts)
        :total-turn-count (plist-get sidecar :total-turn-count)
        :first-user-message (plist-get sidecar :first-user-message)
        :latest-user-message (plist-get sidecar :latest-user-message)
        :chats (niwa-session-artifacts-chats
                (plist-get sidecar :chapters)
                (plist-get sidecar :prompt-index)
                (or (plist-get sidecar :current-segment) 1)
                (plist-get sidecar :session-name))
        :fork-point-ids
        (cl-loop
         for segment in (plist-get sidecar :prompt-index)
         append
         (cl-loop
          for prompt in (cdr segment)
          for id = (plist-get prompt :fork-point-id)
          when (stringp id)
          collect id))
        :working-directory (plist-get sidecar :working-directory)
        :forked-from-session-id (plist-get sidecar :forked-from-session-id)
        :fork-type (plist-get sidecar :fork-type)
        :forked-from-fork-point-id
        (plist-get sidecar :forked-from-fork-point-id)
        :worktree-source-root (plist-get sidecar :worktree-source-root)
        :dedicated-artifact (plist-get sidecar :dedicated-artifact)
        :worktree-directory (plist-get sidecar :worktree-directory)
        :worktree-branch (plist-get sidecar :worktree-branch)
        :worktree-base-commit (plist-get sidecar :worktree-base-commit)))

(defun niwa-session-persistence--read-summary (sidecar-path)
  "Return a classified picker summary for SIDECAR-PATH.

Cheap by design: only fields displayed in the picker (annotations,
sort key, and rejection details) are retained.  The full sidecar plist is
left on disk until restore actually reads it.  An unchanged file fingerprint
reuses its previous classification."
  (let* ((path (expand-file-name sidecar-path))
         (attributes (file-attributes path))
         (fingerprint
          (and attributes
               (list (file-attribute-file-identifier attributes)
                     (file-attribute-modification-time attributes)
                     (file-attribute-size attributes))))
         (cached
          (and fingerprint
               (gethash path niwa-session-persistence--summary-cache))))
    (if (and cached (equal fingerprint (car cached)))
        (cdr cached)
      (let* ((classified
              (niwa-session-persistence--classify-sidecar path))
             (sidecar (plist-get classified :sidecar))
             (summary
              (if sidecar
                  (list :status 'current
                        :summary
                        (niwa-session-persistence--sidecar-summary sidecar))
                classified)))
        (when fingerprint
          (puthash path (cons fingerprint summary)
                   niwa-session-persistence--summary-cache))
        summary))))

(defvar niwa-session-persistence--list-sessions-cache
  (make-hash-table :test #'equal)
  "Last live session enumeration per workspace root, as (t . RESULT).

Decorative consumers reuse the newest live listing instead of rescanning
the workspace on every redraw.  Every live enumeration refreshes it.")

(defun niwa-session-persistence--control-artifacts (entries)
  "Return control probe results for ENTRIES, in one program.

Each candidate is probed for the three artifacts that make a directory a
session -- the obsolete PID lock, the portable lease, and the sidecar --
and every probe of every candidate travels in a single target program.
Probing them one at a time is one target process each, which is what made
enumerating a workspace cost hundreds of round trips.

A candidate proved free of a PID lock is recorded through
`niwa-session-durability-note-no-pid-lock', so the assertions the
publication read repeats per entry reuse this program's proof instead of
paying for their own.  Directories that do carry one are left alone: their
own assertion reports the authority violation at its canonical call site."
  (let* ((names '(".lock" ".lease" "session.meta.eld"))
         (results
          (niwa-session-control-fs-run-program
           (cl-loop for entry in entries
                    append
                    (mapcar (lambda (name)
                              (list :op 'path-exists-p
                                    :path (file-name-concat entry name)
                                    :optional t))
                            names))))
         observed)
    (dolist (entry entries (nreverse observed))
      ;; Results arrive in the order the operations were emitted: one per
      ;; name, in `names' order, per entry.
      (let ((entry-results (list (pop results) (pop results) (pop results))))
        (when (eq 'absent (plist-get (car entry-results) :status))
          (niwa-session-durability-note-no-pid-lock entry))
        (push (cons entry entry-results) observed)))))

(defun niwa-session-persistence--lease-listings (entries)
  "Return an alist of ENTRIES to their observed lease-directory listings.

Every candidate's lease directory is listed in one target program.
`niwa-session-durability--generation-paths' accepts such an observation
in place of its own round trip, so the publication read below spends no
listing of its own; an empty or absent directory is recorded as `none',
which that helper distinguishes from no observation at all.  An entry whose
listing failed is left out, and lists for itself."
  (let ((results
         (niwa-session-control-fs-run-program
          (mapcar (lambda (entry)
                    (list :op 'list-directory
                          :path (file-name-concat entry ".lease")
                          :optional t))
                  entries)))
        alist)
    (dolist (entry entries (nreverse alist))
      (let* ((result (pop results))
             (status (plist-get result :status)))
        (when (memq status '(ok absent))
          (push (cons entry (or (plist-get result :value) 'none)) alist))))))

(defun niwa-session-persistence--directory-entries (directory regexp)
  "Observe real subdirectories in DIRECTORY matching REGEXP, with attributes."
  (condition-case nil
      (cl-loop for (path . attrs) in (directory-files-and-attributes directory t regexp)
               when (eq t (file-attribute-type attrs)) collect (cons path attrs))
    (file-missing nil)))

(defun niwa-session-persistence--publication-observations (entries)
  "Return control, listing and publication observations for ENTRIES."
  (when entries
    (let* ((controls (niwa-session-persistence--control-artifacts entries))
           (listings (niwa-session-persistence--lease-listings entries)))
      (list controls listings
            (if (file-remote-p (car entries))
                ;; Larger programs use the input carrier rather than the
                ;; optional archive; don't split a remote discovery into trips.
                (niwa-session-publication-read-batch entries listings)
              (mapcan (lambda (batch)
                        (niwa-session-publication-read-batch batch listings))
                      (seq-partition entries 32)))))))

(defun niwa-session-persistence--scan (workspace regexp)
  "Observe WORKSPACE session directories matching REGEXP and their publications."
  (niwa-session-durability-with-transaction
    (let ((entries (niwa-session-persistence--directory-entries
                    (niwa-session-artifacts-sessions-dir workspace) regexp)))
      (cons entries (niwa-session-persistence--publication-observations
                     (mapcar #'car entries))))))

(defun niwa-session-persistence--observed-update-time
    (updated-at sidecar-path save-path &optional attributes)
  "Return when the session at SAVE-PATH was last updated, or nil.
UPDATED-AT is authoritative when it parses as a session timestamp.
Otherwise the sidecar at SIDECAR-PATH lends its modification time, then the
directory itself.  ATTRIBUTES supplies the already-listed directory attributes."
  (or (niwa-session-persistence-parse-iso-time updated-at)
      (file-attribute-modification-time
       (or (and sidecar-path (file-attributes sidecar-path))
           attributes
           (file-attributes save-path)))))

(defun niwa-session-persistence--incompatible-entry
    (save-path authority-mode status &optional publication sidecar-path)
  "Return an incompatible discovery record for SAVE-PATH.
AUTHORITY-MODE, STATUS, PUBLICATION, and SIDECAR-PATH describe the failed
cold discovery without making it resumable."
  (list :kind 'incompatible
        :save-path (file-name-as-directory save-path)
        :authority-mode authority-mode
        :status (plist-get status :status)
        :diagnostic (plist-get status :diagnostic)
        :version (plist-get status :version)
        :updated-at
        ;; A remote stat that fails leaves the row undated rather than
        ;; unlisted.
        (when-let* ((time (condition-case nil
                              (niwa-session-persistence--observed-update-time
                               (plist-get status :updated-at)
                               sidecar-path save-path)
                            (file-error nil))))
          (format-time-string "%FT%H-%M-%S" time))
        :publication publication))

(defun niwa-session-persistence--discover-entry
    (entry authority-mode control-results listing &optional observation)
  "Return the discovery record for session directory ENTRY.
AUTHORITY-MODE is the workspace's portable profile.  CONTROL-RESULTS and
LISTING are this enumeration's legacy-control and lease observations.
OBSERVATION is its batched publication/sidecar read.  Failed or contradictory
controls signal, so the caller keeps an undiscoverable, uninspectable row."
  (niwa-session-codec-validate-authority-mode authority-mode)
  (when-let* ((failed
               (seq-find (lambda (result)
                           (not (memq (plist-get result :status) '(ok absent))))
                         control-results)))
    (niwa-session-control-fs-program-value failed))
  (if (and (eq 'absent (plist-get (nth 1 control-results) :status))
           (or (eq 'ok (plist-get (nth 0 control-results) :status))
               (eq 'ok (plist-get (nth 2 control-results) :status))))
      (niwa-session-persistence--incompatible-entry
       entry authority-mode
       (list :status 'unsupported
             :diagnostic (format niwa-session-persistence--pre-lease-message entry)))
    (when-let* ((err (plist-get observation :error)))
      (signal (car err) (cdr err)))
    (when (seq-find (lambda (result) (eq (plist-get result :status) 'ok))
                   control-results)
      ;; A shared scan may have run in a separate discovery transaction.
      (when (eq 'absent (plist-get (car control-results) :status))
        (niwa-session-durability-note-no-pid-lock entry))
      (niwa-session-codec-authority-mode-for-path entry nil authority-mode))
    (let* ((publication (if observation (plist-get observation :publication)
                          (niwa-session-publication-read entry nil listing)))
           (sidecar (plist-get publication :sidecar))
           (status
            (if sidecar
                (if observation
                    (let* ((classified
                            (niwa-session-persistence--classify-sidecar-data
                             (plist-get observation :sidecar-data)))
                           (data (plist-get classified :sidecar)))
                      (if data
                          (list :status 'current :summary
                                (niwa-session-persistence--sidecar-summary data))
                        classified))
                  (niwa-session-persistence--read-summary sidecar))
              (list :status 'missing
                    :diagnostic "Portable session has no committed publication"))))
      (if (eq (plist-get status :status) 'current)
          (list :save-path (file-name-as-directory entry)
                :summary (plist-get status :summary) :publication publication)
        (niwa-session-persistence--incompatible-entry
         entry authority-mode status publication sidecar)))))

(defun niwa-session-persistence--enumerate-sessions
    (workspace &optional cached scan observe-sidecar)
  "Return compatible and incompatible persisted sessions for WORKSPACE.

The result is `(:sessions LIST :incompatible LIST)'.  When CACHED is non-nil,
reuse the last live enumeration when one exists.  SCAN is an optional fresh
observation from this picker invocation, with any deleted directories removed.
OBSERVE-SIDECAR receives each compatible directory and its verified sidecar."
  (let ((root (niwa-workspace-root workspace)))
    (or (and cached (not observe-sidecar)
             (cdr (gethash
                   root
                   niwa-session-persistence--list-sessions-cache)))
        (let* ((authority-mode
                (niwa-session-authority-mode-for-workspace workspace))
               sessions
               incompatible)
          ;; One enumeration is one transaction: the PID-lock proofs and the
          ;; target clock reading that every entry's publication read repeats
          ;; are paid once for the whole listing.
          (niwa-session-durability-with-transaction
           (pcase-let ((`(,entries ,controls ,listings ,observations)
                        (or scan (niwa-session-persistence--scan
                                  workspace "\\`\\(?:[^.]\\|\\.niwa-save-as-\\)"))))
             (pcase-dolist (`(,entry . ,_) entries)
               (let* ((observation (cdr (assoc entry observations)))
                      (record
                       (condition-case err
                           (niwa-session-persistence--discover-entry
                            entry authority-mode
                            (cdr (assoc entry controls))
                            (cdr (assoc entry listings)) observation)
                         ;; Discovery itself failed, so not even the
                         ;; transcript can be trusted for inspection.
                         (error
                          (niwa-session-persistence--incompatible-entry
                           entry authority-mode
                           (list :status 'undiscoverable
                                 :diagnostic (error-message-string err)))))))
                 (if (eq (plist-get record :kind) 'incompatible)
                     (push record incompatible)
                   (when observe-sidecar
                     (funcall observe-sidecar entry (plist-get observation :sidecar-data)))
                   (push record sessions))))))
          (setq sessions
                (sort sessions
                      :key (lambda (item)
                             (or (plist-get (plist-get item :summary)
                                            :updated-at)
                                 ""))
                      :lessp #'string-greaterp)
                incompatible
                (sort incompatible
                      :key (lambda (item) (or (plist-get item :updated-at) ""))
                      :lessp #'string-greaterp))
          (let ((result (list :sessions sessions
                              :incompatible incompatible)))
            (puthash root (cons t result)
                     niwa-session-persistence--list-sessions-cache)
            result)))))

(defun niwa-session-persistence-list-sessions (workspace &optional cached observe-sidecar)
  "Return a list of `(:save-path :summary)' plists for WORKSPACE's sessions.

Sorted by `:updated-at' descending.  Incompatible sessions remain absent from
this compatible-only interface.  Portable sessions are listed only when their
lease names a valid immutable publication; fixed portable sidecars are ignored.

When CACHED is non-nil, reuse this process's last live enumeration for
WORKSPACE when one exists.  Only decorations tolerant of a listing as
old as the last picker, resume, or fork should pass it; anything that
decides authority or names sessions to the user enumerates live.
OBSERVE-SIDECAR forces a live read; it receives each compatible
directory and its sidecar."
  (plist-get (niwa-session-persistence--enumerate-sessions workspace cached nil observe-sidecar)
             :sessions))

(defun niwa-session-persistence-conversation-variants
    (session fork-point-id &optional sessions)
  "Return SESSION's persisted conversation variants at FORK-POINT-ID.
The Source is first, followed by direct Children.  Each returned session
entry has a `:variant-origin' of `source', `conversation', or `worktree'.
SESSIONS may supply an already-loaded session summary list."
  (let* ((session-id (niwa-session-session-id session))
         (source-id
          (if (equal fork-point-id
                     (niwa-session-forked-from-fork-point-id session))
              (niwa-session-forked-from-session-id session)
            session-id))
         variants)
    (dolist
        (entry
         (or sessions
             (when-let* ((workspace
                          (niwa-session-workspace session)))
               (niwa-session-persistence-list-sessions workspace))))
      (let* ((summary (plist-get entry :summary))
             (id (plist-get summary :session-id))
             origin)
        (when (member fork-point-id (plist-get summary :fork-point-ids))
          (cond
           ((equal id source-id)
            (setq origin 'source))
           ((and (equal source-id
                        (plist-get summary :forked-from-session-id))
                 (equal fork-point-id
                        (plist-get summary :forked-from-fork-point-id)))
            (setq origin (plist-get summary :fork-type)))))
        (when (memq origin '(source conversation worktree))
          (setq entry (copy-sequence entry))
          (plist-put entry :variant-origin origin)
          (push entry variants))))
    (sort variants
          (lambda (left right)
            (let ((left-source
                   (eq (plist-get left :variant-origin) 'source))
                  (right-source
                   (eq (plist-get right :variant-origin) 'source)))
              (if (eq left-source right-source)
                  (let* ((left-summary (plist-get left :summary))
                         (right-summary (plist-get right :summary))
                         (left-created
                          (or (plist-get left-summary :created-at) ""))
                         (right-created
                          (or (plist-get right-summary :created-at) "")))
                    (if (equal left-created right-created)
                        (string-lessp
                         (or (plist-get left-summary :session-id) "")
                         (or (plist-get right-summary :session-id) ""))
                      (string-lessp left-created right-created)))
                left-source))))))

(defun niwa-session-persistence--format-session-candidate (entry &optional detail)
  "Return a `completing-read' display string for session ENTRY.
Shows a relative-time annotation first so the newest session is
easiest to recognise at a glance.  DETAIL, when given, names the machine
holding the session and is placed ahead of the message preview, which is the
one field with no bound on its length."
  (let* ((s        (plist-get entry :summary))
         (updated  (plist-get s :updated-at))
         (relative (niwa-session-rewind-format-relative-time updated))
         (name     (or (plist-get s :session-name) "?"))
         (preview  (or (plist-get s :latest-user-message)
                       (plist-get s :first-user-message)
                       ""))
         (chats    (plist-get s :chats))
         (earlier
          (mapcar (lambda (chat) (plist-get chat :title))
                  (reverse (seq-remove (lambda (chat) (plist-get chat :current-p))
                                       chats))))
         (turns    (or (plist-get s :total-turn-count) 0))
         (worktree
          (when (eq (plist-get s :fork-type) 'worktree)
            (let* ((origin (plist-get s :worktree-directory))
                   (current (plist-get s :working-directory))
                   (retargeted
                    (and
                     (stringp origin)
                     (stringp current)
                     (not
                      (equal
                       (file-name-as-directory (expand-file-name origin))
                       (file-name-as-directory
                        (expand-file-name current))))))
                   (origin-missing
                    (and (stringp origin)
                         (not (file-directory-p origin))))
                   (status
                    (cond
                     ((and retargeted origin-missing)
                      "retargeted; original missing")
                     (retargeted "retargeted")
                     (origin-missing "missing")
                     (t "active"))))
              (format "  Worktree Fork: %s (%s)"
                      (or current origin "?") status)))))
    (format "%-12s  %s  [%d chat%s, %d turns]%s%s%s  %s"
            relative name (max 1 (length chats))
            (if (> (length chats) 1) "s" "")
            turns
            ;; Newest first, bounded: the preview after it is the row's end.
            (if earlier
                (concat "  ↩ "
                        (truncate-string-to-width
                         (string-join earlier " · ") 60 nil nil "…"))
              "")
            (or worktree "")
            (if detail (format "  (%s)" detail) "")
            preview)))

(defun niwa-session-persistence--entry-live-buffer (entry)
  "Return the live root buffer already open for session ENTRY, or nil."
  (when-let* ((session-id (plist-get (plist-get entry :summary) :session-id)))
    (niwa-session-control-transfer-root-buffer-for-id session-id)))

(defun niwa-session-persistence--cleanup-reasons (assessment)
  "Describe the cleanup blockers in ASSESSMENT.
Pending journal captures are omitted: the journal settles them unattended."
  (mapconcat
   (lambda (reason)
     (pcase reason
       ('publishing "interrupted publication")
       ('unsettled-mutation "unresolved mutation")
       ('recovery "recovery data retained")
       ('live-owner "live owner")
       ('transfer-reserved "control transfer reserved")
       (_ "unreadable control data")))
   (remq 'journal-pins (plist-get assessment :reasons)) ", "))

(defun niwa-session-persistence--entry-authority (workspace entry &optional assessment)
  "Return `(:action LABEL :detail TEXT :held BOOL)' for WORKSPACE session ENTRY.

The verb names the outcome of choosing this row, which the lease decides:
whether anyone is writing the session right now, not whether its files
happen to be local or reached over TRAMP.  DETAIL names the machine
involved, which is the part a verb cannot carry.  HELD says whether some
client is writing this session now, which is a narrower question than
whether the row resumes: an expired lease offers a takeover precisely
because nobody is writing it any more.

ASSESSMENT, when supplied, is the picker batch's observation of ENTRY."
  (ignore workspace)
  (cond
   ((when-let* ((buffer (niwa-session-persistence--entry-live-buffer entry)))
      (require 'niwa-session-persistence)
      (list :action "Switch"
            :detail (if (buffer-local-value 'niwa-session--read-only-mode
                                            buffer)
                        "already open here, read-only"
                      "already open here")
            :held t)))
   (t
    (let* ((status (or assessment (niwa-session-durability-cleanup-assessment
                                  (plist-get entry :save-path))))
           (host (plist-get status :host))
           (elsewhere (and host (not (equal host (system-name))) host)))
      (cond
       ((and (plist-get status :recoverable)
             ;; Pending captures alone are routine; the journal drains them on its own.
             (remq 'journal-pins (plist-get status :reasons)))
        (list :action "Recover"
              :detail (concat "cleanup blocked: "
                              (niwa-session-persistence--cleanup-reasons status))
              :held nil :cleanup status))
       ((eq 'unknown (plist-get status :state))
        (list :action "Inspect" :detail "cleanup blocked: unreadable control data"
              :held nil :cleanup status))
       (t
        (pcase (plist-get status :state)
          ('foreign
           (list :action "Join"
                 :detail (format "held by %s" (or host "another client"))
                 :held t))
          ('expired
           (list :action "Take over"
                 :detail (if host
                             (format "lease expired, was %s" host)
                           "lease expired")
                 :held nil))
          (_
           (list :action "Resume"
                 :detail (and elsewhere
                              (format "last held by %s" elsewhere))
                 :held nil)))))))))


(defun niwa-session-persistence--entry-action (workspace entry)
  "Return the entry action label for WORKSPACE session ENTRY."
  (plist-get (niwa-session-persistence--entry-authority workspace entry)
             :action))

(defun niwa-session-persistence--size-incompatible (entries)
  "Return incompatible ENTRIES extended with their measured directory sizes.
ENTRIES themselves are cached listing state and stay untouched.  The size is
a decorative column, so a measurement that fails as a whole leaves every row
at `size unavailable' rather than hiding the chooser."
  (condition-case nil
      (cl-mapcar
       (lambda (entry result)
         (if (eq (plist-get result :status) 'ok)
             (append entry (list :size (plist-get result :value)))
           entry))
       entries
       (niwa-session-control-fs-tree-sizes
        (mapcar (lambda (entry) (plist-get entry :save-path)) entries)))
    (error entries)))

(defun niwa-session-persistence--incompatible-reason (entry)
  "Return the human-readable incompatibility reason for ENTRY."
  (if (eq (plist-get entry :status) 'unsupported)
      (format "unsupported %s"
              (or (plist-get entry :version) "version missing"))
    (format "%s: %s" (plist-get entry :status) (plist-get entry :diagnostic))))

(defun niwa-session-persistence--format-incompatible-candidate (entry)
  "Return a `completing-read' display string for incompatible ENTRY."
  (format "%-12s  %-16s  %s  %s"
          (niwa-session-rewind-format-relative-time
           (plist-get entry :updated-at))
          (if-let* ((size (plist-get entry :size)))
              (file-size-human-readable size)
            "size unavailable")
          (niwa-session-persistence--incompatible-reason entry)
          (plist-get entry :save-path)))

(defun niwa-session-persistence--note-p (sidecar)
  "Return non-nil when SIDECAR describes an original note conversation."
  (and (or (plist-get sidecar :note-conversation)
           (eq (plist-get sidecar :preset-name) 'niwa-note))
       (not (plist-get sidecar :forked-from-session-id))))

(defun niwa-session-persistence--dedicated-p (workspace sidecar)
  "Return non-nil when SIDECAR is a note or artifact conversation in WORKSPACE.
Original note sessions are always hidden from ordinary chat selection.
An artifact session becomes ordinary when its artifact no longer names it."
  (or (niwa-session-persistence--note-p sidecar)
      (when-let* ((id (plist-get sidecar :dedicated-artifact)))
        (niwa-artifact-store-dedicated-p
         workspace (plist-get sidecar :session-id) id))))

(defun niwa-session-persistence-without-dedicated (workspace entries)
  "Return listing ENTRIES of WORKSPACE without artifact conversations.
An artifact's dedicated session is reached from its artifact, not listed
among the user's chats."
  (cl-remove-if (lambda (entry)
                  (niwa-session-persistence--dedicated-p
                   workspace (plist-get entry :summary)))
                entries))

(defvar niwa-session-persistence--shared-files-accepted-p nil
  "Non-nil after accepting the shared-project-files advisory in this Emacs.
This acknowledges a general risk, not authority over another session's
lease or files.  Refusing or quitting the prompt leaves it unacknowledged.")

(defun niwa-session-persistence-choose-entry (workspace)
  "Choose a persisted WORKSPACE session or return `new'.

Return nil when no persisted sessions exist.  Choosing an incompatible row
returns `(:action inspect :buffer BUFFER)', where BUFFER shows authoritative
transcript bytes without becoming a resumable session.  Other rows return
the restored chat buffer and follow the session's authority: an unheld
session resumes, one already open in this Emacs is switched to, one another
client holds is joined read-only and follows the owner from there, and an
expired lease is taken over.  Starting a new session alongside an active
writer requires accepting the shared-files advisory once per Emacs process.
Recovery-blocked sessions offer Recover, Discard and Cancel.  Discard refreshes
the chooser; it never implicitly starts a replacement session."
  (let ((result 'refresh) refreshed)
    (while (eq result 'refresh)
      (setq result (niwa-session-persistence--choose-entry-once workspace refreshed)
            refreshed t))
    result))

(defun niwa-session-persistence--choose-entry-once (workspace refreshed)
  "Choose one WORKSPACE entry, or return `refresh' after a discard.
REFRESHED keeps an empty chooser open after deleting its last session."
  ;; Expired sessions and locks left behind by dead Emacsen are swept before
  ;; listing, so the chooser never offers a row that exists only because
  ;; nothing has cleaned up after a previous invocation.
  (let* ((scan (niwa-session-persistence--scan
                workspace "\\`\\(?:[^.]\\|\\.niwa-save-as-\\)"))
         (_ (niwa-session-persistence-cleanup-expired workspace nil scan))
         (enumeration (niwa-session-persistence--enumerate-sessions workspace nil scan))
         (sessions (niwa-session-persistence-without-dedicated
                    workspace (plist-get enumeration :sessions)))
         (incompatible
          (niwa-session-persistence--size-incompatible
           (plist-get enumeration :incompatible))))
    (when (or sessions incompatible refreshed)
      (let* ((new-label "Start new session")
             (held-p nil)
             (remote (file-remote-p (niwa-workspace-root workspace)))
             (probe-entries (if remote sessions
                              (seq-remove #'niwa-session-persistence--entry-live-buffer sessions)))
             (assessments
              (when probe-entries
                (let ((paths (mapcar (lambda (entry) (plist-get entry :save-path)) probe-entries)))
                  (if remote (niwa-session-durability-assess-batch paths)
                    ;; A failed local batch falls back to the per-row authority checks.
                    (ignore-errors (niwa-session-durability-assess-batch paths))))))
             (session-candidates
              (mapcar
               (lambda (entry)
                 (let* ((authority
                         (if assessments
                             (niwa-session-persistence--entry-authority
                              workspace entry (cdr (assoc (plist-get entry :save-path) assessments)))
                           (niwa-session-persistence--entry-authority workspace entry)))
                        (action (plist-get authority :action)))
                   ;; Whether another writer is live is already answered by
                   ;; the row's own authority read; asking the target again per
                   ;; candidate is a second round trip for the same fact.
                   (when (plist-get authority :held)
                     (setq held-p t))
                   (cons
                    (format
                     "%-10s %s" action
                     (niwa-session-persistence--format-session-candidate
                      entry (plist-get authority :detail)))
                    (if (plist-get authority :cleanup)
                        (append entry (list :cleanup (plist-get authority :cleanup)))
                      entry))))
               sessions))
             (incompatible-candidates
              (mapcar
               (lambda (entry)
                 (cons
                  (format
                   "%-10s %s" "Inspect"
                   (niwa-session-persistence--format-incompatible-candidate
                    entry))
                  entry))
               incompatible))
             (candidates
              (append session-candidates incompatible-candidates))
             (choices (cons new-label (mapcar #'car candidates)))
             (choice
              (completing-read
               (if incompatible
                   (format "Niwa session (%d cannot resume): "
                           (length incompatible))
                 "Niwa session: ")
               (niwa--ordered-completion-table
                choices 'niwa-session-entry)
               nil t nil nil (car choices))))
        (if (equal choice new-label)
            (progn
              (when (and held-p
                         (not niwa-session-persistence--shared-files-accepted-p))
                (unless
                    (yes-or-no-p
                     (concat
                      "Another session writer is active. Independent sessions "
                      "can race over project files; use a Worktree Fork for "
                      "isolation. Start anyway? "))
                  (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "New session was not started"))
                (setq niwa-session-persistence--shared-files-accepted-p t))
              'new)
          (let ((entry (cdr (assoc choice candidates))))
            (if (eq (plist-get entry :kind) 'incompatible)
                (if (eq (plist-get entry :status) 'undiscoverable)
                    (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace) "Session transcript is not safely inspectable: %s"
						   (plist-get entry :diagnostic))
                  (list
                   :action 'inspect
                   :buffer
                   (niwa-session-artifacts-inspect-cold-session
                    (plist-get entry :save-path)
                    (plist-get entry :publication) (niwa-diagnostic-scope workspace))))
              (let* ((assessment (plist-get entry :cleanup))
                     (action (when (plist-get assessment :recoverable)
                               (completing-read
                                "Retained session: "
                                '("Recover" "Discard" "Cancel") nil t nil nil "Recover"))))
                (cond
                 ((equal action "Cancel") (signal 'quit nil))
                 ((equal action "Discard")
                  (if (niwa-session-persistence-discard workspace entry)
                      'refresh
                    (signal 'quit nil)))
                 ((eq 'unknown (plist-get assessment :state))
                  (list :action 'inspect :buffer
                        (niwa-session-artifacts-inspect-cold-session
                         (plist-get entry :save-path)
                         (plist-get entry :publication)
                         (niwa-diagnostic-scope workspace))))
                 (t
                  (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
                    (let ((buffer (niwa-session-persistence-restore
                                   (plist-get entry :save-path) nil nil workspace))
                          (recovery (plist-get assessment :recovery)))
                      (when (equal action "Recover")
                        (message "Recovery opened; %s. %s"
                                 (niwa-session-persistence--cleanup-reasons assessment)
                                 (cond
                                  ((and (listp recovery) (plist-get recovery :directory))
                                   (format "Inspect recovery data at %s before explicitly abandoning it."
                                           (plist-get recovery :directory)))
                                  ((memq 'unsettled-mutation (plist-get assessment :reasons))
                                   "Verify previous processes stopped before M-x niwa-retry-target-readiness.")
                                  (t "Review the last committed transcript; prior client-local staging may be unavailable."))))
                      buffer))))))))))))

(defun niwa-session-persistence-choose-conversation-variant
    (variants current-session-id)
  "Choose from ordered VARIANTS, marking CURRENT-SESSION-ID in place."
  (let* ((candidates
          (mapcar
           (lambda (entry)
             (let* ((summary (plist-get entry :summary))
                    (id (plist-get summary :session-id))
                    (origin (plist-get entry :variant-origin))
                    (cwd (or (plist-get summary :working-directory) "?"))
                    (normalized-cwd
                     (and (stringp cwd)
                          (file-name-as-directory
                           (expand-file-name cwd))))
                    (shared-p
                     (and
                      normalized-cwd
                      (> (cl-count
                          normalized-cwd variants
                          :test #'equal
                          :key
                          (lambda (candidate)
                            (when-let* ((path
                                        (plist-get
                                         (plist-get candidate :summary)
                                         :working-directory))
                                       ((stringp path)))
                              (file-name-as-directory
                               (expand-file-name path)))))
                         1)))
                    (label
                     (pcase origin
                       ('source "Source")
                       ('conversation "Conversation")
                       ('worktree "Worktree")))
                    (details
                     (pcase origin
                       ('conversation
                        (if shared-p
                            " — shared files"
                          " — independent directory"))
                       ('worktree
                        (let* ((worktree
                                (plist-get summary :worktree-directory))
                               (branch
                                (plist-get summary :worktree-branch))
                               (retargeted
                                (and
                                 (stringp worktree)
                                 (stringp cwd)
                                 (not
                                  (equal
                                   (file-name-as-directory
                                    (expand-file-name worktree))
                                   normalized-cwd))))
                               (missing
                                (and (stringp worktree)
                                     (not (file-directory-p worktree))))
                               (status
                                (cond
                                 ((and retargeted missing)
                                  "retargeted; original missing")
                                 (retargeted "retargeted")
                                 (missing "missing")
                                 (t "active"))))
                          (format " — branch %s — worktree %s (%s)"
                                  (or branch "?")
                                  (or worktree "?")
                                  status)))
                       (_ "")))
                    (preview
                     (or (plist-get summary :latest-user-message)
                         (plist-get summary :first-user-message)
                         ""))
                    (display
                     (format "%s%-12s — %s — id %s — cwd %s%s — %s"
                             (if (equal id current-session-id) "* " "  ")
                             label
                             (or (plist-get summary :session-name) "?")
                             id cwd details preview)))
               (cons display entry)))
           variants))
         (displays (mapcar #'car candidates))
         (collection
          (niwa--ordered-completion-table
           displays 'niwa-conversation-variant))
         (current
          (cl-find
           current-session-id candidates
           :test #'equal
           :key (lambda (candidate)
                  (plist-get (plist-get (cdr candidate) :summary)
                             :session-id))))
         (chosen
          (completing-read
           "Switch variant: " collection nil t nil nil
           (or (car current) (car displays)))))
    (cdr (assoc chosen candidates))))


;;
;;; Auto-cleanup

(defvar niwa-session-persistence--cleanup-throttle
  (make-hash-table :test #'equal)
  "Workspace keys mapped to the `float-time' of their last expiry sweep.

Keyed on `(WORKSPACE-TYPE . WORKSPACE-ID)'.  In memory only, so a new
Emacs sweeps at its first opportunity.")

(defvar niwa-session-persistence--cleanup-interval (* 24 60 60)
  "Least seconds between two expiry sweeps of one workspace.
Ages are counted in days, so sweeping more often finds nothing new,
while a long-running Emacs must still sweep again.")

(defvar niwa-session-persistence--cleanups (make-hash-table :test #'equal)
  "Workspace keys with a queued expiry sweep.")

(defun niwa-session-persistence--cleanup-key (workspace)
  "Return WORKSPACE's expiry-sweep throttle key."
  (cons (niwa-workspace-type workspace)
        (niwa-workspace-id workspace)))

(defun niwa-session-persistence--cleanup-due-p (workspace)
  "Return non-nil when WORKSPACE has not been swept within the interval."
  (let ((last (gethash (niwa-session-persistence--cleanup-key workspace)
                       niwa-session-persistence--cleanup-throttle)))
    (or (null last)
        (>= (- (float-time) last)
            niwa-session-persistence--cleanup-interval))))

(defun niwa-session-persistence-schedule-cleanup (workspace)
  "Offer WORKSPACE an expiry sweep once Emacs is idle, when one is due.

Session activity calls this so a long-running Emacs keeps expiring old
sessions without waiting for the chooser or for exit.  The sweep runs
after the current command, when the workspace's transport is idle and
no input is pending.  A batch Emacs never sweeps from activity, for the
same reason it does not sweep at exit.  Return the queued timer, or
nil when nothing was queued."
  (when (and workspace
             niwa-session-max-age-days
             (not noninteractive)
             (niwa-session-persistence--cleanup-due-p workspace))
    (niwa-transport-schedule-idle
     niwa-session-persistence--cleanups
     (niwa-session-persistence--cleanup-key workspace)
     'session-cleanup
     (niwa-session-artifacts-sessions-dir workspace)
     (lambda ()
       (condition-case err
           (progn
             (niwa-session-persistence-cleanup-expired workspace)
             (when niwa-session-max-age-days
               (niwa-workspace-schedule-cleanup-runs
                workspace (- (float-time) (* niwa-session-max-age-days 86400)))))
         (error
          (niwa-diagnostic-message (niwa-diagnostic-scope workspace) "Session cleanup failed: %s"
                   (error-message-string err))))))))

(defun niwa-session-persistence-parse-iso-time (str)
  "Parse `YYYY-MM-DDTHH-MM-SS' STR to a time value, or nil on failure."
  (when (and str (stringp str)
             (string-match
              "\\`\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)T\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)\\'"
              str))
    (encode-time
     (string-to-number (match-string 6 str))
     (string-to-number (match-string 5 str))
     (string-to-number (match-string 4 str))
     (string-to-number (match-string 3 str))
     (string-to-number (match-string 2 str))
     (string-to-number (match-string 1 str)))))

(defun niwa-session-persistence--discard-identity (workspace entry &optional session)
  "Validate WORKSPACE's selected ENTRY and return its cold SESSION identity.
When SESSION is non-nil, recheck its target without accepting replacement."
  (unless (eq 'portable (niwa-session-authority-mode-for-workspace workspace))
    (user-error "Explicit recovery discard requires a portable project session"))
  (let* ((directory (file-name-as-directory
                     (niwa-session-control-fs-physical-path
                      (plist-get entry :save-path))))
         (sessions-directory (file-name-as-directory
                              (niwa-session-control-fs-physical-path
                               (niwa-session-artifacts-sessions-dir workspace)))))
    (when (niwa-session-control-fs-async-busy-p directory)
      (user-error "Session has a pending local control program; retry when it settles"))
    (unless (equal sessions-directory
                   (file-name-directory (directory-file-name directory)))
      (user-error "Session does not belong to this workspace's session directory"))
    (when (niwa-session-persistence--entry-live-buffer entry)
      (user-error "Session is open in this Emacs; close it before discarding"))
    (let* ((publication (niwa-session-publication-read directory))
           (sidecar (and publication
                         (niwa-session-persistence-load-sidecar
                          (plist-get publication :sidecar)
                          (niwa-diagnostic-scope workspace))))
           (identity (niwa-workspace-identity-read (niwa-workspace-root workspace))))
      (unless (and sidecar identity
                   (eq 'portable (plist-get sidecar :authority-mode))
                   (equal identity (plist-get (plist-get sidecar :workspace) :workspace-id))
                   (equal (plist-get sidecar :session-id)
                          (plist-get (plist-get entry :summary) :session-id))
                   (equal (plist-get publication :head)
                          (plist-get (plist-get entry :publication) :head)))
        (user-error "Session identity or publication changed; select it again"))
      (let ((target (if session (niwa-session-execution-target session)
                      (niwa-execution-target-create (niwa-workspace-root workspace)))))
        (unless session
          (niwa-execution-target-restore-incarnation
           target (plist-get sidecar :target-incarnation)))
        (if (and (not session) (niwa-execution-target-remote-p target))
            (let ((readiness (niwa-execution-target-probe target t 'off)))
              (unless (eq 'ready (plist-get readiness :status))
                (user-error "Execution target is unavailable: %s"
                            (or (plist-get readiness :error) (plist-get readiness :reason)))))
          (niwa-execution-target-observe-incarnation target))
        (when (niwa-execution-target-incarnation-changed-p target)
          (user-error "Execution target changed; refusing session discard"))
        (or session
            (niwa-session--create
             :workspace workspace :authority-mode 'portable :execution-target target
             :session-id (plist-get sidecar :session-id)
             :name (plist-get sidecar :session-name) :save-path directory
             :publication publication))))))

(defun niwa-session-persistence-discard (workspace entry)
  "Confirm and discard WORKSPACE's retained session ENTRY without restoring it.
Refuse changed identities and live owners.  Captures and recovery are settled
through their owners before deletion; failure may retain approved omissions."
  (niwa-with-diagnostic-errors (niwa-diagnostic-scope workspace)
    (let* ((assessment (niwa-session-durability-cleanup-assessment
			(plist-get entry :save-path)))
           (session (niwa-session-persistence--discard-identity workspace entry))
           (directory (niwa-session-save-path session)))
      (unless (memq (plist-get assessment :state) '(available expired))
	(user-error "Session cannot be discarded: %s"
                    (niwa-session-persistence--cleanup-reasons assessment)))
      (when (yes-or-no-p
             (format (concat "Discard session %s (%s) permanently, including its transcript, "
                             "recovery data and pending journal captures? Approved capture/recovery "
                             "actions may persist if deletion fails. Project-file effects remain. "
                             "Confirm the previous client and its child writers have stopped: ")
                     (niwa-session-name session)
                     (or (and (plist-get assessment :reasons)
                              (niwa-session-persistence--cleanup-reasons assessment))
			 "saved session data")))
	(condition-case err
            (niwa-session-durability-discard-session
             session
             (lambda ()
               (niwa-session-persistence--discard-identity workspace entry session)
               (dolist (proof (cdr (niwa-journal-pins-observe directory)))
		 (when (eq 'verify (plist-get proof :op))
                   (let* ((id (file-name-base (plist-get proof :path)))
                          (capture (niwa-journal-capture-read workspace id)))
                     (unless (and capture
                                  (equal (niwa-session-session-id session)
					 (plist-get (plist-get capture :metadata) :session)))
                       (user-error "Journal capture %s is unreadable or belongs to another session" id))
                     (niwa-journal-process-discard workspace id directory))))
               (when (niwa-session-recovery-read directory)
		 (niwa-session-recovery--abandon session))
               (when (plist-get (niwa-session-lease session) :unsettled-mutation)
		 (unless (niwa-session-durability-set-unsettled-mutation session nil)
                   (user-error "Could not acknowledge unresolved mutation")))
               (niwa-session-persistence--discard-identity workspace entry session)))
          (error (user-error "Session retained; approved recovery/capture actions may have completed: %s"
                             (error-message-string err))))))))

(cl-defun niwa-session-persistence--delete-saved
    (workspace save-path max-age-seconds
               &optional (note-session-ids 'unread))
  "Delete saved session SAVE-PATH unless something may still hold it.
Journal pins always keep it.  Deletion uses
`niwa-session-durability-delete-abandoned' with MAX-AGE-SECONDS.
Original note conversations
require proof that WORKSPACE no longer references them.  NOTE-SESSION-IDS
is the sweep's reference snapshot, or `unread' to obtain it here.
Return non-nil only when the directory was deleted."
  (and (not (niwa-journal-pins-present-p save-path))
       (let ((sidecar (condition-case nil
                          (let* ((publication
                                  (niwa-session-publication-read save-path))
                                 (data (and publication
                                            (niwa-session-codec-read
                                             (plist-get publication :sidecar)))))
                            (and (listp data) data))
                        (error nil))))
         (not
          (when (or (niwa-session-persistence--note-p sidecar)
                    (not (and (plist-member sidecar :preset-name)
                              (symbolp (plist-get sidecar :preset-name)))))
            (when (eq note-session-ids 'unread)
              (require 'niwa-persistence)
              (setq note-session-ids
                    (niwa-instruction-store-note-session-ids workspace)))
            (or (eq note-session-ids t)
                (member (plist-get sidecar :session-id) note-session-ids)
                (member (file-name-nondirectory (directory-file-name save-path))
                        note-session-ids)))))
       (niwa-session-durability-delete-abandoned
        save-path max-age-seconds)))

(defun niwa-session-persistence-delete (workspace save-path)
  "Delete WORKSPACE's saved session at SAVE-PATH now.
The safety rules of `niwa-session-persistence-cleanup-expired' apply
without its age cap: a session a client may still hold, or whose journal
captures are pending, is kept.  Referenced note conversations and uncertain
note stores are kept too.  Return non-nil only when it was deleted."
  (niwa-session-persistence--delete-saved
   workspace save-path 0))

(defun niwa-session-persistence-cleanup-expired (workspace &optional force scan)
  "Delete WORKSPACE sessions older than `niwa-session-max-age-days'.

Scans session directories independently of resume compatibility.  Uses
`:updated-at' when available, otherwise the sidecar or directory modification
time.

The `niwa-session-keep-recent-count' most-recently-updated sessions are
never deleted regardless of age.  Original note conversations do not count
toward that floor, and are eligible only after their notes leave the store;
an uncertain reference check retains them.  Pending journal capture pins prevent
deletion.  Sessions open in this Emacs are skipped; the others use
`niwa-session-durability-delete-abandoned', which also requires the last
lease renewal to be older than the cap.  Throttled to at most one sweep
per `(workspace-type . workspace-id)' within
`niwa-session-persistence--cleanup-interval'; when FORCE is non-nil
the throttle is bypassed.  SCAN may supply the picker's fresh directory and
publication observations; entries deleted by the sweep are removed from it.

Returns the number of sessions deleted, or nil when the cap is nil or the
workspace was swept too recently."
  (when niwa-session-max-age-days
    (let ((ws-key (niwa-session-persistence--cleanup-key workspace)))
      (when (or force (niwa-session-persistence--cleanup-due-p workspace))
        (puthash ws-key (float-time)
                 niwa-session-persistence--cleanup-throttle)
        (let ((threshold-secs (* niwa-session-max-age-days 24 60 60))
              (now            (float-time))
              (deleted        0)
              (candidates     nil)
              (note-candidates nil)
              (note-session-ids 'unread))
          (pcase-let ((`(,entries ,_controls ,_listings ,observations)
                       (or scan (niwa-session-persistence--scan workspace "\\`[^.]"))))
            (dolist (entry entries)
              (let ((save-path (car entry)))
                (unless (string-prefix-p "." (file-name-nondirectory save-path))
                  (let* ((observation (cdr (assoc save-path observations)))
                         (publication (plist-get observation :publication))
                         (data (plist-get observation :sidecar-data))
                         (sidecar (and (listp data) data))
                         (parsed-time
                          (niwa-session-persistence--observed-update-time
                           (plist-get sidecar :updated-at)
                           (plist-get publication :sidecar) save-path (cdr entry))))
                    (let ((candidate (list save-path
                                           (and parsed-time (float-time parsed-time))
                                           (plist-get sidecar :session-id))))
                      (cond
                       ((or (niwa-session-persistence--note-p sidecar)
                            (not (and (plist-member sidecar :preset-name)
                                      (symbolp (plist-get sidecar :preset-name)))))
                        (when (eq note-session-ids 'unread)
                          (require 'niwa-persistence)
                          (setq note-session-ids
                                (niwa-instruction-store-note-session-ids workspace)))
                        (unless (or (eq note-session-ids t)
                                    (member (plist-get sidecar :session-id) note-session-ids)
                                    (member (file-name-nondirectory (directory-file-name save-path))
                                            note-session-ids))
                          (if (niwa-session-persistence--note-p sidecar)
                              (push candidate note-candidates)
                            (push candidate candidates))))
                       ((not (niwa-session-persistence--dedicated-p workspace sidecar))
                        (push candidate candidates)))))))))
          ;; The newest sessions are exempt regardless of age: the chooser
          ;; sweeps before any lock is taken, so without this floor a
          ;; 30-day absence would delete the session the user came back to
          ;; resume.  A session without a readable timestamp sorts oldest,
          ;; so it never displaces a dated one from the floor.
          (setq candidates
                (sort candidates
                      (lambda (a b) (> (or (cadr a) 0) (or (cadr b) 0)))))
          (when niwa-session-keep-recent-count
            (setq candidates
                  (nthcdr niwa-session-keep-recent-count candidates)))
          ;; Note conversations never consume the ordinary retention floor.
          (pcase-dolist (`(,save-path ,updated-secs ,session-id)
                         (append note-candidates candidates))
            (when (and updated-secs
                       (> (- now updated-secs) threshold-secs)
                       ;; A session open here may outlive its lease after
                       ;; suspend.
                       (not (and session-id
                                 (niwa-session-control-transfer-root-buffer-for-id
                                  session-id)))
                       ;; One unreadable or contested session must not end
                       ;; the sweep.
                       (condition-case nil
                           (niwa-session-persistence--delete-saved
                            workspace save-path threshold-secs note-session-ids)
                         (error nil)))
              (cl-incf deleted)
              (when scan
                (setcar scan (assoc-delete-all save-path (car scan))))))
          (when (> deleted 0)
            (message "Cleaned up %d expired session%s"
                     deleted (if (= deleted 1) "" "s")))
          deleted)))))

(provide 'niwa-session-catalog)

;;; niwa-session-catalog.el ends here
