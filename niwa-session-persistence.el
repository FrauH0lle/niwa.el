;;; niwa-session-persistence.el --- Save and restore chat sessions -*- lexical-binding: t -*-

;;; Commentary:

;; Session persistence facade for lifecycle, mid-turn checkpoints, resume,
;; and locking.  Listing and stale-session cleanup belong to
;; `niwa-session-catalog'; sidecar coding belongs to `niwa-session-codec';
;; paths, artifacts, snapshots, and segment writes belong to
;; `niwa-session-artifacts'; restore plans and the Rewind transaction belong
;; to `niwa-session-rewind'; and Fork projection, publication, Worktree
;; restoration belong to `niwa-session-fork'; session naming belongs to
;; `niwa-session-naming'.

;;; Code:

(require 'niwa-session-artifacts)
(require 'niwa-session-catalog)
(require 'niwa-session-codec)
(require 'niwa-session-durability)
(require 'niwa-telemetry)
(require 'niwa-utilities)

(eval-when-compile
  (require 'cl-lib)
  (require 'niwa-agents)
  (require 'niwa-structs))

;; `files'
(defvar remote-file-name-inhibit-cache)

;; `gptel'
(declare-function gptel-mode "ext:gptel" (&optional arg))
(defvar gptel--preset)
(defvar gptel-display-buffer-action)
(defvar gptel-system-prompt)

;; `niwa-agent-control'
(declare-function niwa-agent-control-active-turn-p "niwa-agent-control" (session))
(declare-function niwa-agent-control-reoffer-mail "niwa-agent-control" (session))
(declare-function niwa-agent-control-restore-lost-monitors "niwa-agent-control" (session monitors))
(autoload 'niwa-agent-control-active-turn-p "niwa-agent-control")
(autoload 'niwa-agent-control-reoffer-mail "niwa-agent-control")
(autoload 'niwa-agent-control-restore-lost-monitors "niwa-agent-control")

;; `niwa-agent-conversation'
(declare-function niwa-agent-conversation-prepare-save
                  "niwa-agent-conversation" (invocation))
(autoload 'niwa-agent-conversation-prepare-save "niwa-agent-conversation")
(autoload 'niwa-agent-conversation-save "niwa-agent-conversation")

;; `niwa-agent-persistence'
(declare-function niwa-agent-persistence-restore-tree "niwa-agent-persistence" (session root-buffer readonly-p))
(autoload 'niwa-agent-persistence-restore-tree "niwa-agent-persistence")

;; `niwa-chat'
(declare-function niwa--chat-buffer-disable-org-element-cache "niwa-chat" nil)
(declare-function niwa--chat-buffer-init-common "niwa-chat" (buf workspace source &optional inspection-p))
(declare-function niwa--normalize-session-directory "niwa-chat" (directory workspace))
(autoload 'niwa--chat-buffer-disable-org-element-cache "niwa-chat")
(autoload 'niwa--chat-buffer-init-common "niwa-chat")
(autoload 'niwa--normalize-session-directory "niwa-chat")

;; `niwa-engine'
(declare-function niwa-engine-assert-local-history
                  "niwa-engine" (session operation &optional backend root-only))
(autoload 'niwa-engine-assert-local-history "niwa-engine")

;; `niwa-execution'
(declare-function niwa-execution-teardown-all "niwa-execution" nil)
(declare-function niwa-execution-unsettled-mutation-p "niwa-execution" (session))
(autoload 'niwa-execution-unsettled-mutation-p "niwa-execution")

;; `niwa-execution-target'
(autoload 'niwa-execution-target-remote-p "niwa-execution-target")

;; `niwa-hooks'
(declare-function niwa-hooks-flush-log "niwa-hooks" (session))

;; `niwa-journal-capture'
(declare-function niwa-journal-capture-seal-and-schedule "niwa-journal-capture"
                  (session buffer trigger &optional captures))
(autoload 'niwa-journal-capture-seal-and-schedule "niwa-journal-capture")

;; `niwa-journal-process'
(declare-function niwa-journal-process-stop-all "niwa-journal-process" ())
(defvar niwa-journal-process--inhibit-scheduling)

;; `niwa-memory-decision'
(declare-function niwa-memory-decision-stop-recovery "niwa-memory-decision" ())
(defvar niwa-memory-decision--inhibit-recovery)

;; `niwa-memory-pass'
(declare-function niwa-memory-pass-stop-all "niwa-memory-pass" ())
(defvar niwa-memory-pass--inhibit-scheduling)

;; `niwa-permission-log'
(declare-function niwa-permission-log-flush "niwa-permission-log" (session))

;; `niwa-ptc-checkpoint'
(autoload 'niwa-ptc-checkpoint-reconcile "niwa-ptc-checkpoint")

;; `niwa-sandbox'
(defvar niwa-sandbox-mode)

;; `niwa-schedule'
(declare-function niwa-schedule-restore-session "niwa-schedule" (session buffer))
(autoload 'niwa-schedule-restore-session "niwa-schedule")

;; `niwa-session-artifacts'
(autoload 'niwa-session-artifacts-inhibit-so-long
  "niwa-session-artifacts")
(defvar niwa-session-artifacts-require-agent-commit-p)
(defvar niwa-session-artifacts--terminal-sessions)

;; `niwa-session-checkpoint'
(autoload 'niwa-session-checkpoint-capture "niwa-session-checkpoint")
(autoload 'niwa-session-checkpoint-install "niwa-session-checkpoint")
(autoload 'niwa-session-checkpoint-project "niwa-session-checkpoint")

;; `niwa-session-codec'
(defvar niwa-session-codec-format-version)

;; `niwa-session-collection'
(declare-function niwa-session-collection-schedule "niwa-session-collection" (session))
(autoload 'niwa-session-collection-schedule "niwa-session-collection")

;; `niwa-session-control-fs'
(defvar niwa-session-control-fs--async-wait)
(autoload 'niwa-session-control-fs-append-rotating
  "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-path-exists-p
  "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-physical-path
  "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-write-file
  "niwa-session-control-fs")

;; `niwa-session-control-transfer'
(declare-function niwa-session-control-transfer-notify "niwa-session-control-transfer" (session event &rest args))
(declare-function niwa-session-control-transfer-register-root-buffer "niwa-session-control-transfer" (session buffer))
(declare-function niwa-session-control-transfer-root-buffer-for-id "niwa-session-control-transfer" (session-id))
(autoload 'niwa-session-control-transfer-notify
  "niwa-session-control-transfer")
(autoload 'niwa-session-control-transfer-register-root-buffer
  "niwa-session-control-transfer")
(autoload 'niwa-session-control-transfer-root-buffer-for-id
  "niwa-session-control-transfer")

;; `niwa-session-diagnostics'
(declare-function niwa-session-publication-call-with-diagnostic-batch
                  "niwa-session-diagnostics" (session function))
(declare-function niwa-session-publication-diagnostic-backlog-p
                  "niwa-session-diagnostics" (session))
(autoload 'niwa-session-publication-diagnostic-backlog-p
  "niwa-session-diagnostics")
(autoload 'niwa-session-publication-call-with-diagnostic-batch
  "niwa-session-diagnostics")

;; `niwa-session-durability'
(defvar niwa-session-durability--transaction-clock)
(autoload 'niwa-session-durability-disclose "niwa-session-durability")
(autoload 'niwa-session-durability-lease-acquire
  "niwa-session-durability")
(autoload 'niwa-session-durability-lease-owned-p
  "niwa-session-durability")
(autoload 'niwa-session-durability-lease-release
  "niwa-session-durability")
(autoload 'niwa-session-durability-lease-status
  "niwa-session-durability")

;; `niwa-session-fork'
(declare-function niwa-session-fork-clone-session
                  "niwa-session-fork"
                  (session policy &rest keys))
(autoload 'niwa-session-fork-clone-session "niwa-session-fork")

;; `niwa-session-naming'
(declare-function niwa-rename-session "niwa-session-naming" (new-name))
(declare-function niwa-session-naming-adopt-explicit "niwa-session-naming" (session name))
(declare-function niwa-session-naming-cancel "niwa-session-naming" ())
(declare-function niwa-session-naming-normalize "niwa-session-naming" (name))
(autoload 'niwa-rename-session "niwa-session-naming" nil t)
(autoload 'niwa-session-naming-adopt-explicit "niwa-session-naming")
(autoload 'niwa-session-naming-cancel "niwa-session-naming")
(autoload 'niwa-session-naming-normalize "niwa-session-naming")

;; `niwa-session-publication'
(declare-function niwa-session-publication-async-eligible-p
                  "niwa-session-publication" (session))
(declare-function niwa-session-publication-barrier
                  "niwa-session-publication" (session &optional token))
(declare-function niwa-session-publication-busy-p
                  "niwa-session-publication" (session))
(declare-function niwa-session-publication-defer
                  "niwa-session-publication"
                  (session preparation &optional failure token))
(declare-function niwa-session-publication-publish "niwa-session-publication" (session artifacts &optional require-commit prepared token))
(declare-function niwa-session-publication-read "niwa-session-publication" (session-dir &optional head names))
(declare-function niwa-session-publication-stopping
                  "niwa-session-publication" (session))
(declare-function niwa-session-publication-submit
                  "niwa-session-publication"
                  (session prepare acknowledge &optional mail-ids retain-failure select-staged superseded))
(autoload 'niwa-session-publication-async-eligible-p "niwa-session-publication")
(autoload 'niwa-session-publication-barrier "niwa-session-publication")
(autoload 'niwa-session-publication-busy-p "niwa-session-publication")
(autoload 'niwa-session-publication-defer "niwa-session-publication")
(autoload 'niwa-session-publication-stopping "niwa-session-publication")
(autoload 'niwa-session-publication-submit "niwa-session-publication")
(autoload 'niwa-session-publication-publish "niwa-session-publication")
(autoload 'niwa-session-publication-read "niwa-session-publication")

;; `niwa-session-recovery'
(defvar niwa-session-recovery--mutation-cache)

;; `niwa-session-save-as'
(declare-function niwa-session-save-as--rename-live-session-buffers "niwa-session-save-as" (session data-buffer))
(declare-function niwa-session-save-as-run "niwa-session-save-as" (session buffer new-name new-id new-save-path))
(autoload 'niwa-session-save-as--rename-live-session-buffers
  "niwa-session-save-as")
(autoload 'niwa-session-save-as-run "niwa-session-save-as")

;; `niwa-transport'
(autoload 'niwa-transport-cancel-idle "niwa-transport")
(autoload 'niwa-transport-cancel-pending "niwa-transport")
(autoload 'niwa-transport-run-when-idle "niwa-transport")
(autoload 'niwa-transport-schedule-idle "niwa-transport")

;; `niwa-structs'
(defvar niwa--agent-invocation)
(defvar niwa--current-request)
(defvar niwa--session)
(defvar niwa--workspace)
(defvar niwa-session--read-only-mode)

;; `niwa-tool-repair'
(declare-function niwa-tool-repair-flush-log "niwa-tool-repair-diagnostics" (session))

;; `niwa-tool-render-data'
(defvar niwa-tool-render-data-close)
(defvar niwa-tool-render-data-open)
(autoload 'niwa-tool-render-data-blocks "niwa-tool-render-data")
(autoload 'niwa-tool-render-data-call-range-p "niwa-tool-render-data")
(autoload 'niwa-tool-render-data-reconcile-lost-executions
  "niwa-tool-render-data")
(autoload 'niwa-tool-render-data-without-owner
  "niwa-tool-render-data")

;; `niwa-transcript-audit'
(autoload 'niwa-transcript-audit-records "niwa-transcript-audit")

;; `niwa-transcript-restore'
(declare-function niwa-transcript-restore-gptel-state "niwa-transcript-restore" nil)
(declare-function niwa-transcript-restore-properties "niwa-transcript-restore" (&optional only-if-missing))
(autoload 'niwa-transcript-restore-gptel-state
  "niwa-transcript-restore")
(autoload 'niwa-transcript-restore-properties "niwa-transcript-restore")

;; `niwa-utilities'
(autoload 'niwa--call-with-bare-transcript-mode "niwa-utilities")
(autoload 'niwa--forget-place "niwa-utilities")
(autoload 'niwa--transcript-org-mode "niwa-utilities")
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")
(autoload 'niwa-diagnostic-warning "niwa-utilities")

;; `niwa-view'
(defvar niwa--data-buffer)
(defvar niwa--view-buffer)

;; `niwa-workspace'
(declare-function niwa-workspace "niwa-workspace" (&optional buffer))
(declare-function niwa-workspace-ensure-generated-state-ignored "niwa-workspace" (workspace))
(declare-function niwa-workspace-get-or-create "niwa-workspace" (type id root name))
(defvar niwa-workspace-additional-roots)
(defvar niwa-workspace--registry)
(autoload 'niwa-workspace "niwa-workspace")
(autoload 'niwa-workspace-ensure-generated-state-ignored
  "niwa-workspace")
(autoload 'niwa-workspace-get-or-create "niwa-workspace")

;; `niwa-workspace-identity'
(declare-function niwa-workspace-identity-ensure "niwa-workspace-identity" (root))
(autoload 'niwa-workspace-identity-ensure "niwa-workspace-identity")

;; `org'
(defvar org-agenda-file-menu-enabled)

;; `saveplace'
(defvar save-place-mode)

;; `so-long'
(defvar so-long--inhibited)
(defvar so-long-predicate)

;;
;;; Customization

(defcustom niwa-file-history-max-snapshot-bytes (* 1024 1024)
  "Soft size cap (bytes) for individual pre-turn file checkpoints.

Larger content is persisted as an explicit checkpoint gap with a warning.
Defaults to 1 MB."
  :type 'integer
  :group 'niwa)

(defcustom niwa-session-checkpoint-interval 30
  "Seconds between conversation checkpoints while a request is in flight.

Emacs auto-saves only after keyboard input, so a turn that runs
unattended, such as a Goal or a retained agent, would otherwise reach
disk only when it settles.  While any root or agent request is in
flight, modified conversations are checkpointed this often through the
same path as `auto-save-hook'.  A nil value leaves mid-turn checkpoints
to Emacs auto-save alone."
  :type '(choice (number :tag "Seconds")
          (const :tag "Only on Emacs auto-save" nil))
  :group 'niwa)


;;
;;; Sidecar lifecycle

(defun niwa-session-persistence-write-current-buffer-atomically (path)
  "Write the current buffer to PATH through a same-directory rename."
  (niwa-session-control-fs-write-file
   (niwa-session-control-fs-physical-path path)
   (buffer-substring-no-properties (point-min) (point-max))))

(defun niwa-session-persistence--segment-text (session path)
  "Return SESSION's committed transcript text at PATH, or nil when absent.
Portable transcripts live in the publication, not at PATH itself."
  (when (niwa-session-artifacts-artifact-present-p
         session (file-name-nondirectory path) t)
    ;; Published artifacts are literal bytes; transcripts are UTF-8.
    (decode-coding-string
     (niwa-session-artifacts-read-artifact
      session (file-name-nondirectory path) t)
     'utf-8-unix)))

(defun niwa-session-persistence--execution-successor-ids (text)
  "Return structured execution ids present in transcript TEXT."
  (when (and text (string-search ":execution-id" text))
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (org-mode))
      (niwa-transcript-restore-properties)
      (let (ids)
        (dolist (record
                 (niwa-transcript-audit-records
                  (buffer-string)))
          (when (memq (plist-get record :type)
                      '(execution-archive execution-completion))
            (when-let* ((id (plist-get (plist-get record :render-data)
                                       :execution-id)))
              (cl-pushnew id ids :test #'equal))))
        (dolist (block
                 (niwa-tool-render-data-blocks
                  (buffer-substring-no-properties (point-min) (point-max))))
          (when-let* ((stored (caddr block))
                      (begin (+ (point-min) (car block)))
                      (end (+ (point-min) (cadr block)))
                      ((niwa-tool-render-data-call-range-p
                        stored begin end))
                      (data
                       (niwa-tool-render-data-without-owner stored))
                      (id (plist-get data :execution-id)))
            (cl-pushnew id ids :test #'equal)))
        ids))))

(defconst niwa-session-persistence--segment-backup-max-bytes
  (* 64 1024 1024)
  "Rotation size of `segment-repair-backups.eld'; at most twice this is kept.")

(defun niwa-session-persistence--back-up-segment (path text)
  "Append transcript PATH's TEXT to its session's repair backups.
Closed segments change only through repairs, and publication cleanup drops
the replaced copy within a few saves; this keeps it for diagnosis and
rollback.  A failed backup is reported and does not block the repair."
  (condition-case err
      (let ((print-escape-newlines t) (print-length nil) (print-level nil))
        (niwa-session-control-fs-append-rotating
         (niwa-session-control-fs-physical-path
          (file-name-concat (file-name-directory path) "segment-repair-backups.eld"))
         (concat (prin1-to-string
                  (list :time (format-time-string "%FT%T%z")
                        :segment (file-name-nondirectory path)
                        :content (substring-no-properties text)))
                 "\n")
         niwa-session-persistence--segment-backup-max-bytes))
    (error
     (niwa-diagnostic-warning
      (niwa-diagnostic-scope path) 'niwa
      (format "Could not back up %s before repairing it: %s"
              (file-name-nondirectory path) (error-message-string err))))))

(defun niwa-session-persistence--reconcile-lost-execution-file
    (path text &optional successor-execution-ids artifact-callback lost-monitor)
  "Mark stale running execution rows in transcript PATH with TEXT as lost.

When ARTIFACT-CALLBACK is non-nil, pass it the repaired replacement instead
of writing PATH directly.  SUCCESSOR-EXECUTION-IDS and LOST-MONITOR are
passed to `niwa-tool-render-data-reconcile-lost-executions'."
  (when (and text (or (string-search ":execution-id" text)
                      (string-search ":live-execution-p" text)))
    (with-temp-buffer
      (insert text)
      (delay-mode-hooks (org-mode))
      (niwa-transcript-restore-properties)
      (let ((count
              (niwa-tool-render-data-reconcile-lost-executions
              (current-buffer) successor-execution-ids lost-monitor)))
        (when (> count 0)
          (niwa-session-persistence--back-up-segment path text)
          (niwa-session-artifacts-stabilize-gptel-bounds)
          (if artifact-callback
              (funcall
               artifact-callback
               (list :path path
                     :content (buffer-string)
                     :coding 'utf-8-unix))
            (niwa-session-persistence-write-current-buffer-atomically
             path)))
        count))))

(defun niwa-session-persistence-reconcile-lost-execution-segments
    (session &optional exclude-path artifact-callback lost-monitor)
  "Repair stale execution rows in SESSION segments except EXCLUDE-PATH.

ARTIFACT-CALLBACK and LOST-MONITOR have the meanings described by
`niwa-session-persistence--reconcile-lost-execution-file'."
  (let ((save-path (niwa-session-save-path session))
        (successor-ids
         (and exclude-path
              (niwa-session-persistence--execution-successor-ids
               (niwa-session-persistence--segment-text
                session exclude-path))))
        (count 0))
    (cl-loop for segment downfrom (niwa-session-current-segment session) to 1
             for path = (niwa-session-artifacts-segment-path
                         save-path segment)
             for text = (unless (and exclude-path
                                     (string= (expand-file-name path)
                                              (expand-file-name exclude-path)))
                          (niwa-session-persistence--segment-text session path))
             when text
             do (cl-incf
                 count
                 (or (niwa-session-persistence--reconcile-lost-execution-file
                      path text successor-ids artifact-callback lost-monitor)
                     0))
             and do
             (dolist (id (niwa-session-persistence--execution-successor-ids text))
               (cl-pushnew id successor-ids :test #'equal)))
    count))

(defun niwa-session-persistence--flush-diagnostic-logs-now (session)
  "Retry SESSION diagnostics queued before or after materialization.

The four logs share one lease reservation.  Each append otherwise opened a
transaction of its own -- recovery refresh, lease renewal, ownership
reading, then a reservation renewing the lease on entry and committing it
on exit -- so a flush point paid that four times over for data nothing
reads live."
  (let ((flush
         (lambda ()
           (niwa-telemetry-flush session)
           (when (fboundp 'niwa-hooks-flush-log)
             (niwa-hooks-flush-log session))
           (when (fboundp 'niwa-tool-repair-flush-log)
             (niwa-tool-repair-flush-log session))
           (when (fboundp 'niwa-permission-log-flush)
             (niwa-permission-log-flush session)))))
    ;; Only reserve when a log actually has something.  Each flush already
    ;; returns early on an empty queue, and reserving for four of those
    ;; would renew the lease -- target I/O, and a warning when the lease
    ;; cannot be renewed -- to write nothing at all.  The four queues are
    ;; struct slots, so asking costs nothing.
    (if (or (niwa-session-telemetry-pending session)
            (niwa-session-hook-log-pending session)
            (niwa-session-repair-log-pending session)
            (niwa-session-permission-log-pending session)
            (niwa-session-publication-diagnostic-backlog-p session))
        (niwa-session-publication-call-with-diagnostic-batch
         session flush)
      (funcall flush))))

(defun niwa-session-persistence-flush-diagnostic-logs (session)
  "Flush SESSION diagnostics, keeping target I/O off the caller's path.

A remote diagnostic flush is a whole publication transaction carrying
data nothing reads live, so it waits for an idle transport instead of
extending the save or settlement that queued it.  A local flush is one
append and runs inline.  Emacs exit flushes inline either way."
  (let ((save-path (niwa-session-save-path session)))
    (if (and save-path (file-remote-p save-path))
        (progn
          ;; The zero timer lets the save or settlement that queued the
          ;; diagnostics return first; transport idleness alone would run
          ;; the flush inline, because the transport is usually idle by
          ;; the time a save reaches its cleanup.
          (run-at-time
           0 nil #'niwa-transport-run-when-idle
           (list 'diagnostic-flush session) save-path
           (lambda ()
             (niwa-session-persistence--flush-diagnostic-logs-now
              session))))
      (niwa-session-persistence--flush-diagnostic-logs-now session))))

(defun niwa-session-persistence-allocate-session-id (sessions-dir)
  "Return a fresh id absent from SESSIONS-DIR and live session buffers."
  (cl-loop repeat 33
           for candidate = (niwa-session-generate-id)
           for path = (file-name-concat sessions-dir candidate)
           unless (or (file-exists-p path) (file-symlink-p path)
                      (cl-some
                       (lambda (buffer)
                         (when-let* ((session (buffer-local-value
                                              'niwa--session buffer)))
                           (equal candidate (niwa-session-session-id session))))
                       (buffer-list)))
           return candidate
           finally (error "Could not allocate a unique session id after 33 attempts")))


(defun niwa-session-persistence-shallow-ensure-files (session buffer)
  "Materialize SESSION and BUFFER paths without writing the sidecar.

Used by sub-agent allocation: a sub-agent can spawn during
the parent's first turn (before any DONE handler has run), so we
need the session directory and `agents/' subdirectory before its retained
identity exists.  The later acknowledged registry commit performs the full
root snapshot and sidecar publication.

Returns SESSION's `save-path' on success, or nil on failure.  Idempotent."
  (or (niwa-session-save-path session)
      (condition-case err
          (let* ((_disclosure
                  (when (niwa-execution-target-remote-p
                         (niwa-session-execution-target session))
                    (niwa-session-durability-disclose session)))
                 (_workspace-identity
                  (progn
                    (niwa-workspace-identity-ensure
                     (niwa-workspace-root
                      (niwa-session-workspace session)))))
                 (sessions-dir (niwa-session-artifacts-sessions-dir
                                (niwa-session-workspace session)))
                 (session-id
                  (or (niwa-session-session-id session)
                      (error "Session has no identity")))
                 (save-path (file-name-as-directory
                             (file-name-concat sessions-dir session-id)))
                 (segment-path (niwa-session-artifacts-segment-path
                                save-path 1))
                 (now (format-time-string "%FT%H-%M-%S")))
            (make-directory sessions-dir t)
            (make-directory save-path)
            (make-directory (file-name-concat save-path "agents") t)
            (make-directory (file-name-concat save-path "file-history") t)
            (niwa-session-persistence-lock-acquire
             save-path (buffer-name buffer) session)
            (setf (niwa-session-session-id session)      session-id)
            (setf (niwa-session-save-path session)       save-path)
            (setf (niwa-session-created-at session)      now)
            (setf (niwa-session-updated-at session)      now)
            (setf (niwa-session-current-segment session) 1)
            (niwa-session-persistence-flush-diagnostic-logs session)
            (niwa-workspace-ensure-generated-state-ignored
             (niwa-session-workspace session))
            (with-current-buffer buffer
              (unless buffer-file-name
                (setq buffer-file-name segment-path))
              (niwa-session-artifacts-disown-save-machinery))
            (niwa-session-control-transfer-register-root-buffer
             session buffer)
            save-path)
        (error
         (niwa-diagnostic-message (niwa-diagnostic-scope session) "Shallow session materialization failed: %S" err)
         nil))))

(defun niwa-session-persistence-record-running-transcript
    (session entry)
  "Insert ENTRY into SESSION's agent-transcripts.  ENTRY is (ID . PLIST)."
  (when (and session (consp entry))
    (setf (alist-get (car entry)
                     (niwa-session-agent-transcripts session)
                     nil nil #'equal)
          (cdr entry))))

(defun niwa-session-persistence-update-transcript-entry
    (session agent-id updates)
  "Return nil after merging transcript data into SESSION entry for AGENT-ID.
The argument UPDATES is the change plist."
  (when (and session agent-id)
    (let ((existing (alist-get agent-id
                               (niwa-session-agent-transcripts session)
                               nil nil #'equal)))
      (when existing
        (let ((merged (copy-sequence existing)))
          (cl-loop for (k v) on updates by #'cddr do
                   (setq merged (plist-put merged k v)))
          (setf (alist-get agent-id
                           (niwa-session-agent-transcripts session)
                           nil nil #'equal)
                merged))))))

(defun niwa-session-persistence-write-sidecar-now (session buffer &optional prepared checkpoint token)
  "Best-effort sidecar rewrite for SESSION and BUFFER.

Only writes when the sidecar file already exists on disk -- i.e.
a full root snapshot has written `session.meta.eld'.  Before that, the session
may be shallowly materialized (directory + lock + agents/ but no sidecar), so
this observational rewrite remains deferred to the next critical agent commit
or root autosave.  PREPARED admits the owning checkpoint's final sidecar.
CHECKPOINT captures a matching transcript already written by the caller.
TOKEN is the caller's gate token."
  (when (and (memq session (bound-and-true-p niwa-session-artifacts--terminal-sessions))
             (not prepared))
    (user-error "Session checkpoint is being prepared"))
  (when (and session (niwa-session-save-path session)
             (niwa-session-codec-portable-authority-p session)
             (niwa-session-artifacts-artifact-present-p
              session "session.meta.eld"))
    (condition-case err
        (progn
          ;; Drain async work before the sidecar is captured.
          (niwa-session-publication-barrier session token)
          (niwa-session-durability-with-transaction
            (niwa-session-artifacts-assert-mutation-authority session buffer)
            (niwa-session-publication-publish
             session
             (list (niwa-session-artifacts--sidecar-artifact
                    session buffer checkpoint))
             nil prepared token)))
      (error
       (niwa-diagnostic-message (niwa-diagnostic-scope session)
                                   "Sidecar rewrite failed: %S" err)
       nil)
      (:success t))))

(defun niwa-session-persistence-save-agent-state (session &optional token)
  "Commit SESSION's agent state through its authoritative root buffer.
Keep root transcript fields at the last committed checkpoint.  Before the
first root snapshot, materialize the complete root rather than a bare sidecar.
TOKEN is the caller's gate token."
  (when-let* ((buffer (niwa-session-root-buffer session))
              ((buffer-live-p buffer)))
    (let ((niwa-session-artifacts-require-agent-commit-p t))
      (when (if (and (niwa-session-save-path session)
                     (niwa-session-artifacts-artifact-present-p
                      session "session.meta.eld" t))
                (niwa-session-artifacts-publish-sidecar-state
                 session buffer token)
              (niwa-session-artifacts-save session buffer nil t token))
        ;; The committed sidecar already carries the current registry,
        ;; so a pending observational save is absorbed.
        (niwa-session-persistence-cancel-deferred-agent-save session)
        t))))

(defcustom niwa-session-persistence-agent-save-debounce 2.0
  "Seconds to coalesce best-effort agent-state saves before publishing.
Observational agent persists -- activity transitions, explicit mailbox
discards -- fire in bursts.  Inside this window they collapse into
one sidecar-only save, and a synchronous acknowledged commit absorbs a
pending one entirely."
  :type 'number
  :group 'niwa)

(defvar niwa-session-persistence--deferred-agent-saves
  (make-hash-table :test #'eq)
  "Sessions with a pending debounced agent-state save, mapped to timers.")

(defun niwa-session-persistence-save-agent-state-soon (session)
  "Schedule one coalesced best-effort sidecar save for SESSION.
Agent state, queued input, pause and recovery changes use it.  Unlike
`niwa-session-persistence-save-agent-state', the deferred
save is not an acknowledged agent commit: it rewrites only the session
sidecar (the transcript segment is committed at settlement), may be
queued behind an active publication, and is cancelled outright when a
synchronous commit lands first."
  (when (and (niwa-session-save-path session)
             (not (gethash session
                           niwa-session-persistence--deferred-agent-saves)))
    (puthash session
             (run-at-time niwa-session-persistence-agent-save-debounce nil
                          #'niwa-session-persistence--deferred-agent-save
                          session)
             niwa-session-persistence--deferred-agent-saves)))

(defun niwa-session-persistence--deferred-agent-save (session)
  "Run the debounced best-effort agent-state save for SESSION."
  (if-let* ((save-path (niwa-session-save-path session)))
      (niwa-transport-run-when-idle
       (list 'agent-state-save session) save-path
       (lambda ()
         ;; Keep SESSION registered while transport work is deferred so
         ;; the exit hook can still flush it inline.
         (remhash session niwa-session-persistence--deferred-agent-saves)
         (if (niwa-session-publication-active-p session)
             ;; Timer callbacks perform no target I/O while a critical
             ;; publication is active; re-arm and try again after it.
             (niwa-session-persistence-save-agent-state-soon session)
           (when-let* ((buffer (niwa-session-root-buffer session))
                       ((buffer-live-p buffer)))
             (condition-case nil
                 (niwa-session-artifacts-save-agent-registry
                  session buffer)
               (error nil))))))
    (remhash session niwa-session-persistence--deferred-agent-saves)))

(defun niwa-session-persistence-cancel-deferred-agent-save (session)
  "Cancel any pending debounced agent-state save for SESSION."
  (when-let* ((timer (gethash session
                              niwa-session-persistence--deferred-agent-saves)))
    (cancel-timer timer)
    (remhash session niwa-session-persistence--deferred-agent-saves))
  (niwa-transport-cancel-pending (list 'agent-state-save session)))


;;
;;; First user message extraction

(defun niwa-session-persistence-first-user-message (buffer)
  "Return a one-line preview of the first user prompt in BUFFER, or nil.

Use the same canonical prompt derivation as the live and restored prompt
index, excluding structural mailbox deliveries, metadata, summaries and
tool/reasoning scaffolding.  Keep the first non-empty line, truncated to
120 characters."
  (plist-get (car (niwa-session-artifacts-collect-prompts buffer 120))
             :preview))


;;
;;; Buffer selection

(defun niwa-session-persistence-notify-session-event
    (session event &rest args)
  "Send semantic lifecycle EVENT to SESSION's registered observers."
  (apply #'niwa-session-control-transfer-notify session event args))

(defun niwa-session-persistence-root-data-buffer-p (buffer)
  "Return non-nil when BUFFER is an explicitly registered session root.

Unassociated ordinary buffers remain eligible for low-level inspection, but
session buffers must cross the session-owned root registration seam."
  (and (buffer-live-p buffer)
       (with-current-buffer buffer
         (let ((session (and (boundp 'niwa--session)
                             niwa--session)))
           (and (not niwa--agent-invocation)
                (or (and session
                         (eq buffer (niwa-session-root-buffer session)))
                    (and (null session)
                         (not (bound-and-true-p
                               niwa-session--inspection-buffer-p)))))))))

(defun niwa-session-persistence-authoritative-buffer (buffer)
  "Return the authoritative session root for BUFFER, or nil.

The root registration is the only projection boundary persistence needs to
know.  View buffers never become roots because an initialized view has
already registered the session-owned data buffer."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((session (and (boundp 'niwa--session) niwa--session)))
        (cond
         (niwa--agent-invocation nil)
         ((and session (buffer-live-p (niwa-session-root-buffer session)))
          (niwa-session-root-buffer session))
         ((niwa-session-persistence-root-data-buffer-p buffer)
          buffer))))))

(defun niwa-session-persistence-root-buffer-for-session
  (session &optional buffer)
  "Return SESSION's live root data buffer, preferring BUFFER."
  (or (let ((registered (niwa-session-root-buffer session)))
        (and (buffer-live-p registered) registered))
      (let ((candidate
             (niwa-session-persistence-authoritative-buffer buffer)))
        (and candidate
             (eq session
                 (buffer-local-value 'niwa--session candidate))
             candidate))))


;;
;;; Locking

(defun niwa-session-persistence-lock-acquire
    (session-dir buffer-name &optional session)
  "Acquire SESSION-DIR's portable lease for BUFFER-NAME.
SESSION supplies the live owner.  Return nil when another client owns the
lease, permitting read-only inspection; legacy pre-lease state is refused."
  (niwa-session-codec-authority-mode-for-path session-dir session)
  (when session
    (niwa-session-durability-disclose session))
  (niwa-session-durability-lease-acquire session-dir buffer-name session))

(defun niwa-session-persistence-lock-release (session-dir &optional session token)
  "Release this client's portable lease for SESSION-DIR.
SESSION supplies the live owner; TOKEN is the caller's mutation gate token."
  (niwa-session-codec-authority-mode-for-path session-dir session)
  (niwa-session-durability-lease-release session-dir session token))

(defun niwa-session-persistence-release-on-kill ()
  "Buffer-local `kill-buffer-hook' that releases session mutation authority.
Publication work is drained before the final save and lease release.  A root
buffer also stops new admissions.  An unresolved job keeps the session's
state and lease for explicit publication recovery."
  (when (and (boundp 'niwa--session)
             niwa--session)
    (when (niwa-session-persistence--drain-for-close
           niwa--session (current-buffer))
      (niwa-session-persistence-autosave-buffer (current-buffer))
      (when-let* ((dir (niwa-session-save-path niwa--session)))
        (niwa-journal-capture-seal-and-schedule niwa--session (current-buffer) 'session-end)
        (niwa-session-persistence--wait-async)
        (condition-case _
            (niwa-session-persistence-lock-release dir niwa--session)
          (error nil))))))

(defun niwa-session-persistence--wait-async ()
  "Let already-running asynchronous control programs deliver their callbacks.
Closing and exit call it after their last write, before a lease is released."
  (when (featurep 'niwa-session-control-fs)
    (niwa-session-control-fs-wait-async)))

(defun niwa-session-persistence--drain-temporarily (session)
  "Drain SESSION's publication work with admissions and wakes stopped.
The previous stopping state is restored on every exit, so a later veto leaves
the session usable.  Notifications retained while stopped are re-armed through
`niwa-agent-control-reoffer-mail', which never offers inline, and only when
some were actually suppressed.  An unresolved job signals."
  (let ((was-stopping (niwa-session-publication-stopping-p session)))
    (unwind-protect (niwa-session-publication-stopping session)
      (setf (niwa-session-publication-stopping-p session) was-stopping)
      (when (and (not was-stopping)
                 (niwa-session-mail-notifications session))
        (niwa-agent-control-reoffer-mail session)))))

(defun niwa-session-persistence--gate-owner-p (session)
  "Return non-nil when SESSION can own asynchronous publication work.
Only materialized portable sessions publish through the session gate."
  (and session
       (niwa-session-save-path session)
       (niwa-session-codec-portable-authority-p session)))

(defun niwa-session-persistence--drain-for-close (session buffer)
  "Drain SESSION's publication work for closing BUFFER.
The root buffer stops new admissions; other buffers only wait.  Return non-nil
when closing may proceed with its save and lease release.  An unresolved job
warns and returns nil, leaving explicit recovery or abandonment possible."
  (or (not (niwa-session-persistence--gate-owner-p session))
      (let ((was-stopping (niwa-session-publication-stopping-p session)))
        (condition-case err
            (progn
              (if (eq buffer (niwa-session-root-buffer session))
                  (niwa-session-publication-stopping session)
                (niwa-session-publication-barrier session))
              (when (niwa-session-pending-publication session)
                (user-error "Session has pending publication; retry or abandon it first"))
              t)
          (error
           ;; Recovery may admit work again, so admissions reopen.
           (setf (niwa-session-publication-stopping-p session) was-stopping)
           (niwa-diagnostic-warning (niwa-diagnostic-scope session)
          'niwa
          (format "Session publication is unresolved; not saving or releasing: %s"
                  (error-message-string err))
          :warning)
           nil)))))

(defun niwa-session-persistence-allow-buffer-close-p ()
  "Return non-nil when closing the current session buffer may proceed.
A `kill-buffer-query-functions' member: it drains an owned root buffer's
publication work while teardown has not yet started, and refuses the close
with a message when a job stays unresolved, so explicit recovery remains
possible."
  (or (not (and (boundp 'niwa--session) niwa--session))
      (not (niwa-session-persistence--gate-owner-p niwa--session))
      (not (eq (current-buffer) (niwa-session-root-buffer niwa--session)))
      ;; The drain is the decision.  Another query may still veto the close,
      ;; so admissions reopen; the kill hook stops them for good.
      (condition-case err
          (progn
            (niwa-session-persistence--drain-temporarily niwa--session)
            t)
        (error
         (niwa-diagnostic-message (niwa-diagnostic-scope niwa--session)
          (concat "Session publication is unresolved (%s); run "
                  "niwa-session-publication-retry or "
                  "niwa-session-publication-abandon before closing")
          (error-message-string err))
         nil))))


;;
;;; Automatic persistence

(defvar-local niwa-session--save-failed nil
  "Non-nil when the most recent auto-save failed in this buffer.
Set by terminal and lifecycle auto-save handlers on any save error and
cleared on the next successful save.  Surfaced by
`niwa-session-persistence-header-segment' so the user has a
visible cue in addition to the `display-warning'.")

(defun niwa-session-persistence--note-autosave-failure (buffer err &optional scope)
  "Mark BUFFER's auto-save failed with ERR and warn using captured SCOPE."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq-local niwa-session--save-failed t)
      (force-mode-line-update)))
  (niwa-diagnostic-warning (or scope (niwa-diagnostic-scope buffer))
   'niwa
   (format "Could not auto-save %s: %s"
           (if (buffer-live-p buffer) (buffer-name buffer) "buffer")
           (error-message-string err))
   :warning))

(defun niwa-session-persistence-autosave-buffer (buffer &optional token)
  "Save BUFFER's owned conversation, containing and reporting save failures.
Retained agents use their transcript writer.  Root buffers use session
publication.  Views and read-only inspection buffers never write.  While the
session's publication gate is owned the save is deferred before anything is
captured, and the return value is nil.  TOKEN is the caller's gate token; a
deferred save runs with its own."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (bound-and-true-p niwa--session)
                 (niwa-session-workspace niwa--session)
                 ;; Opening a conversation is not durable work.  Already
                 ;; saved sessions still checkpoint an intentionally empty
                 ;; transcript, and first-turn partial text is worth saving.
                 (or (niwa-session-save-path niwa--session)
                     (save-restriction (widen) (> (buffer-size) 0)))
                 (not (bound-and-true-p niwa-session--read-only-mode))
                 (not (bound-and-true-p niwa-session--inspection-buffer-p))
                 (or niwa--agent-invocation
                     (eq buffer (niwa-session-root-buffer niwa--session))))
        (let ((scope (niwa-diagnostic-scope niwa--session)))
        (unless (and (niwa-session-persistence--gate-owner-p niwa--session)
                     (niwa-session-publication-defer
                      niwa--session
                      (lambda (prepared-token)
                        (niwa-session-persistence-autosave-buffer
                         buffer prepared-token))
                      (lambda (err)
                        (niwa-session-persistence--note-autosave-failure
                         buffer err scope))
                      token))
        (let ((inhibit-quit t))
          (condition-case err
              (let* ((agent niwa--agent-invocation)
                     (portable (niwa-session-codec-portable-authority-p
                                niwa--session))
                     ;; A checkpoint needs a marker, so let the transcript
                     ;; writer include it instead of publishing twice.
                     (_ (when (and agent portable)
                          (setf (niwa-agent-invocation-sidecar-dirty agent) t)))
                     (saved
                      (if agent
                          (niwa-agent-conversation-save agent nil token)
                        (niwa-session-artifacts-save
                         niwa--session buffer nil nil token))))
                (unless saved
                  (error "Conversation was not saved"))
                ;; Agent writes can be retained publication batches awaiting
                ;; a sidecar commit.  A checkpoint must be visible on resume,
                ;; even when the root transcript has not changed.
                (when (and agent portable
                           (or (niwa-session-publication-uncommitted-batches
                                niwa--session)
                               (niwa-session-publication-queue niwa--session)
                               (niwa-agent-invocation-sidecar-dirty agent)))
                  (let ((root (niwa-session-root-buffer niwa--session)))
                    (if (niwa-session-artifacts-artifact-present-p
                         niwa--session "session.meta.eld" t)
                        (niwa-session-artifacts-publish-sidecar-state
                         niwa--session root token)
                      (unless (with-current-buffer root
                                (niwa-session-artifacts-save
                                 niwa--session root nil nil token))
                        (error "Agent checkpoint has no saved root")))))
                (setq-local niwa-session--save-failed nil)
                (force-mode-line-update)
                saved)
            ((error quit)
             (niwa-session-persistence--note-autosave-failure buffer err scope)
             nil)))))))))

(defvar niwa-session-persistence--autosaves (make-hash-table :test #'eq)
  "Conversation buffers with a queued auto-save opportunity.")

(defvar-local niwa-session--checkpoint-pending nil
  "Identity of this buffer's accepted background checkpoint, or nil.")

(defun niwa-session-persistence-checkpoint-buffer (buffer)
  "Checkpoint BUFFER in the background when its authority permits it.
Only warm local portable root and agent conversations use an asynchronous
publication job.  Other authorities use the synchronous autosave writer.
Return `pending' for accepted work, nil for an ineligible or absorbed save.
Capture and the modified/failed/uncommitted recheck run only under the gate.
Failures retain frozen publication recovery and the existing save-failed cue.
Explicit saves and teardown keep using the synchronous writer and its barrier."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and niwa--session
                 (niwa-session-workspace niwa--session)
                 (not (bound-and-true-p niwa-session--read-only-mode))
                 (not (bound-and-true-p niwa-session--inspection-buffer-p))
                 (or niwa--agent-invocation
                     (eq buffer (niwa-session-root-buffer niwa--session)))
                 (not (niwa-session-publication-stopping-p niwa--session))
                 (not niwa-session--checkpoint-pending)
                 (or (buffer-modified-p) niwa-session--save-failed
                     (niwa-session-publication-uncommitted-batches niwa--session)))
        (if (not (niwa-session-publication-async-eligible-p niwa--session))
            (niwa-session-persistence-autosave-buffer buffer)
          (let* ((session niwa--session)
                 (scope (niwa-diagnostic-scope session))
                 (identity (list buffer))
                 prepared fallback
                 (acknowledge
                  (lambda (success err)
                    (when (and (buffer-live-p buffer)
                               (eq identity (buffer-local-value
                                             'niwa-session--checkpoint-pending buffer)))
                      (with-current-buffer buffer
                        (setq niwa-session--checkpoint-pending nil)
                        (condition-case cleanup
                            (if success
                                (if fallback
                                    ;; The gate is released before ACK.  A
                                    ;; missing canonical agent transcript must
                                    ;; use its ordinary writer, not a successful
                                    ;; no-op that clears the failure cue.
                                    (niwa-session-persistence-autosave-buffer buffer)
                                  (when prepared (funcall (plist-get prepared :finish)))
                                  (setq niwa-session--save-failed nil)
                                  (force-mode-line-update))
                              (niwa-session-persistence--note-autosave-failure
                               buffer (if (stringp err) (list 'error err) err) scope))
                          ((error quit)
                           (niwa-session-persistence--note-autosave-failure
                            buffer cleanup scope))))))))
            (setq niwa-session--checkpoint-pending identity)
            (condition-case err
                (niwa-session-publication-submit
                 session
                 (lambda ()
                   (when (and (buffer-live-p buffer)
                              (eq session (buffer-local-value 'niwa--session buffer)))
                     (with-current-buffer buffer
                       ;; A synchronous save ahead of this preparation can absorb
                       ;; it.  No transcript, sidecar or hooks were frozen yet.
                       (when (or (buffer-modified-p) niwa-session--save-failed
                                 (niwa-session-publication-uncommitted-batches session))
                         (niwa-session-artifacts-recover-working-directory session)
                         (niwa-session-artifacts-check-target-incarnation session buffer)
                         (setq prepared
                               (niwa-session-artifacts-call-with-checkpoint
                                session
                                (lambda ()
                                  (if niwa--agent-invocation
                                      (niwa-agent-conversation-prepare-save
                                       niwa--agent-invocation)
                                    (niwa-session-artifacts-prepare-save
                                     session buffer nil nil t)))))
                         (setq fallback (null prepared))
                         (prog1 (plist-get prepared :artifacts)
                           ;; The job owns captured payload strings until staging.
                           ;; ACK retains only the finish/selection continuations.
                           (setf (plist-get prepared :artifacts) nil))))))
                 acknowledge nil t
                 (lambda (batch)
                   (if-let* ((select (plist-get prepared :select-staged)))
                       (funcall select batch)
                     batch)))
              ((error quit)
               (funcall acknowledge nil err)
               nil))))))))

(defun niwa-session-persistence-autosave-soon (buffer)
  "Queue one transport-safe checkpoint of conversation BUFFER.
Repeated calls before it runs coalesce.  The checkpoint runs only if
BUFFER still has unsaved changes by then; settlement or an explicit save
can absorb it."
  (niwa-transport-schedule-idle
   niwa-session-persistence--autosaves buffer 'conversation-autosave
   (buffer-local-value 'default-directory buffer)
   (lambda ()
     ;; Recheck at execution, including failed saves whose transcript
     ;; write succeeded before their marker failed.
     (when (and (buffer-live-p buffer)
                (or (buffer-modified-p buffer)
                    (buffer-local-value 'niwa-session--save-failed buffer)
                    (with-current-buffer buffer
                      (and (bound-and-true-p niwa--session)
                           (niwa-session-publication-uncommitted-batches
                            niwa--session)))))
       (niwa-session-persistence-checkpoint-buffer buffer)))))

(defun niwa-session-persistence-autosave ()
  "Checkpoint modified data buffers during Emacs auto-save.
Queue one transport-safe opportunity per buffer, yielding between saves.
A failed write does not prevent other conversations from being saved.
Settlement and exit still persist synchronously.  The in-flight
checkpoint timer calls this too, because Emacs auto-saves only after
input and an unattended turn receives none."
  (dolist (buffer (buffer-list))
    (when (and (buffer-live-p buffer)
               (with-current-buffer buffer
                 (bound-and-true-p niwa--session))
               (or (buffer-modified-p buffer)
                   (buffer-local-value 'niwa-session--save-failed buffer)
                   (with-current-buffer buffer
                     (and (eq buffer (niwa-session-root-buffer
                                      niwa--session))
                          (niwa-session-publication-uncommitted-batches
                           niwa--session)))))
      (niwa-session-persistence-autosave-soon buffer))))


;;;; In-flight checkpoints

(defvar niwa-session-persistence--checkpoint-timer nil
  "Repeating timer that checkpoints conversations during requests, or nil.")

(defun niwa-session-persistence--request-in-flight-p ()
  "Return non-nil when some niwa buffer has a request in flight."
  (and (boundp 'niwa--current-request)
       (cl-some (lambda (buffer)
                  (buffer-local-value 'niwa--current-request buffer))
                (buffer-list))))

(defun niwa-session-persistence--checkpoint-tick ()
  "Checkpoint modified conversations; stop once no request is in flight.
The stopping tick checkpoints too: changes deferred to these checkpoints,
such as completion breadcrumbs, may have landed after the last request
settled and saved only its own buffer."
  (unless (niwa-session-persistence--request-in-flight-p)
    (niwa-session-persistence-stop-checkpoints))
  (niwa-session-persistence-autosave))

(defun niwa-session-persistence-start-checkpoints ()
  "Checkpoint conversations periodically until no request is in flight.
Request admission calls this; repeated calls share one timer, which
stops itself on the first tick that finds every request settled."
  (when (and niwa-session-checkpoint-interval
             (not (memq niwa-session-persistence--checkpoint-timer
                        timer-list)))
    (setq niwa-session-persistence--checkpoint-timer
          (run-at-time niwa-session-checkpoint-interval
                       niwa-session-checkpoint-interval
                       #'niwa-session-persistence--checkpoint-tick))))

(defun niwa-session-persistence-stop-checkpoints ()
  "Stop the in-flight checkpoint timer."
  (when (timerp niwa-session-persistence--checkpoint-timer)
    (cancel-timer niwa-session-persistence--checkpoint-timer))
  (setq niwa-session-persistence--checkpoint-timer nil))

(defun niwa-session-persistence-checkpoints-active-p ()
  "Return non-nil while in-flight checkpoints periodically save conversations."
  (and (timerp niwa-session-persistence--checkpoint-timer)
       (memq niwa-session-persistence--checkpoint-timer timer-list)
       t))

(defun niwa-session-persistence-header-segment ()
  "Return a header-line fragment summarising persistence state.
Empty when the session has no unusual state; highlights save
failures and read-only mode when active."
  (let ((parts nil))
    (when (and (bound-and-true-p niwa--session)
               (niwa-session-pending-publication niwa--session))
      (push (propertize " [Publication pending] " 'face 'error) parts))
    (when (bound-and-true-p niwa--session)
      (when-let* ((lease-state
                   (plist-get (niwa-session-lease niwa--session)
                              :state))
                  ((memq lease-state '(foreign expired lost contested))))
        (push (propertize (format " [Lease: %s] " lease-state)
                          'face 'warning)
              parts)))
    (when (bound-and-true-p niwa-session--save-failed)
      (push (propertize " [Save failed] " 'face 'error) parts))
    (when (bound-and-true-p niwa-session--read-only-mode)
      (push (propertize " [Read-only] " 'face 'warning) parts))
    (if parts (apply #'concat parts) "")))


;;
;;; Read path (resume)

(defun niwa-session-persistence-load-sidecar (path &optional scope)
  "Read a current-version sidecar plist from PATH.
Return nil when the sidecar is missing or unreadable.  Signal when a
readable sidecar has an unsupported version or obsolete shape.
SCOPE is the explicit diagnostic root when the caller knows its owner."
  (let* ((result (niwa-session-persistence--classify-sidecar path))
         (diagnostic (plist-get result :diagnostic)))
    (pcase (plist-get result :status)
      ('current (plist-get result :sidecar))
      ((or 'missing 'unreadable)
       (niwa-diagnostic-warning (or scope 'unavailable) 'niwa (format "%s: %s" diagnostic path) :warning)
       nil)
      (_ (error "%s" diagnostic)))))

(defun niwa-session-persistence--classify-sidecar-data (sidecar)
  "Classify already-read SIDECAR using the current persistence schema."
  (cond
   ((not (and (plistp sidecar) (keywordp (car sidecar))))
    (list :status 'unreadable :diagnostic "Invalid session sidecar"))
   ((not (equal (plist-get sidecar :version)
                niwa-session-codec-format-version))
    (list :status 'unsupported
          :version (plist-get sidecar :version)
          :updated-at (plist-get sidecar :updated-at)
          :diagnostic (format "Unsupported session version: %s"
                              (or (plist-get sidecar :version) "missing"))))
   (t
    (condition-case err
        (progn
          (niwa-session-codec-validate-current-sidecar sidecar)
          (list :status 'current :sidecar sidecar))
      (error
       (list :status 'obsolete
             :version (plist-get sidecar :version)
             :updated-at (plist-get sidecar :updated-at)
             :diagnostic (error-message-string err)))))))

(defun niwa-session-persistence--classify-sidecar (path)
  "Return the compatibility classification for the sidecar at PATH.

The returned plist has `:status' equal to `current', `missing', `unreadable',
`unsupported', or `obsolete'.  Current results carry `:sidecar'; readable
incompatible results retain their observed version, update stamp, and exact
validation diagnostic."
  (if (not (file-exists-p path))
      (list :status 'missing :diagnostic "Sidecar is missing")
    (condition-case err
        (niwa-session-persistence--classify-sidecar-data
         (niwa-session-codec-read path))
      (error
       (list :status 'unreadable
             :diagnostic (error-message-string err))))))

(defvar-local niwa-session--read-only-mode nil
  "Non-nil when this chat buffer is in read-only session mode.
Set by the restore path when a cross-host lock cannot be safely
broken.  While set: autosave is inhibited (the terminal DONE handler
early-outs), the view buffer refuses to insert into the data buffer,
and the data buffer itself is marked `buffer-read-only'.")

(defun niwa-session-persistence-apply-read-only-mode (buf &optional reason)
  "Put BUF into read-only session mode.

REASON replaces the default explanation.  Handing control away deliberately
is not the same event as finding someone else already holding the lock, and
saying the wrong one is how a user learns to ignore the notice.

The notice is a message, not a warning: the buffer states its own authority
in the interaction zone and the cockpit header for as long as it holds, so a
popped-up window would repeat durably visible state and steal the frame at
the moment the user is watching the handoff.  A lease that fails or is lost
is a different matter and still warns.
See `niwa-session--read-only-mode' for semantics."
  (with-current-buffer buf
    (setq buffer-read-only t)
    (setq-local niwa-session--read-only-mode t)
    (niwa-diagnostic-message (niwa-diagnostic-scope buf) "%s"
             (or reason
                 "session opened read-only; another client holds the lease"))))

(defun niwa-session-persistence-find-live-buffer (session-id buf-name)
  "Return the named live root buffer for SESSION-ID, or nil.

The canonical session buffer name is the durable locator.  Persistence does
not search unrelated buffers or infer roles from view-local variables."
  (or (niwa-session-control-transfer-root-buffer-for-id session-id)
      (let ((candidate (get-buffer buf-name)))
        (when (and (niwa-session-persistence-root-data-buffer-p candidate)
                   (with-current-buffer candidate
                     (or (not (bound-and-true-p niwa--session))
                         (equal (niwa-session-session-id niwa--session)
                                session-id))))
          candidate))))

(defun niwa-session-persistence--hydrate-restored-buffer
    (buf session workspace segment-path acquired additional-roots
         lifecycle-source &optional artifact-callback checkpoint-callback lost-monitors)
  "Hydrate fresh restore buffer BUF and return its durable repair count.
SESSION and WORKSPACE are planted before niwa restores persisted state.
SEGMENT-PATH is reconciled when ACQUIRED owns the session lock.  Additional
workspace roots and LIFECYCLE-SOURCE restore the saved session environment.
ARTIFACT-CALLBACK collects remote transcript replacements for one batch.
CHECKPOINT-CALLBACK receives the candidate frozen with repaired root bytes.
LOST-MONITORS contains the lost facts already repaired in older segments."
  (let ((ptc-insertions 0)
        (ptc-consumptions 0)
        checkpoint-captured)
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (let ((org-agenda-file-menu-enabled nil))
          (org-mode)))
      (setq-local niwa--session session)
      (setq-local niwa--workspace workspace)
      (setq-local default-directory
                  (niwa-session-working-directory session))
      (when additional-roots
        (setq-local niwa-workspace-additional-roots additional-roots))
      (when (fboundp 'niwa--chat-buffer-disable-org-element-cache)
        (niwa--chat-buffer-disable-org-element-cache))
      (niwa-transcript-restore-gptel-state)
      (when acquired
        (niwa-session-artifacts-check-target-incarnation session buf)
        (pcase-let ((`(,inserted . ,consumed)
                     (niwa-ptc-checkpoint-reconcile session)))
          (setq ptc-insertions inserted
                ptc-consumptions consumed))
        (let ((transcript-repairs
               (+ ptc-insertions
                  (niwa-tool-render-data-reconcile-lost-executions
                   buf nil (lambda (facts) (push facts lost-monitors))))))
          (when lost-monitors
            (niwa-agent-control-restore-lost-monitors session lost-monitors))
          (when (or (> transcript-repairs 0) (> ptc-consumptions 0))
            ;; PTC insertions and lost-execution repairs both change transcript
            ;; length; the repair publication needs current offsets.  Rewrite
            ;; only bounds: chat setup has not installed the advice that keeps
            ;; `gptel--save-state' from writing request config to the drawer.
            (niwa-session-artifacts-stabilize-gptel-bounds)
            (when checkpoint-callback
              (funcall checkpoint-callback
                       (niwa-session-checkpoint-capture
                        session "/root" (niwa-session-artifacts--live-sidecar session buf)))
              (setq checkpoint-captured t))
            (when artifact-callback
                (funcall
                 artifact-callback
                 (list :path segment-path
                       :content (buffer-string)
                       :coding (or buffer-file-coding-system 'utf-8-unix))))
            (set-buffer-modified-p nil))))
      (unless acquired
        (niwa-session-persistence-apply-read-only-mode buf))
      ;; Freeze reconciled metadata before initialization hooks advance it.
      (when (and acquired checkpoint-callback (not checkpoint-captured))
        (funcall checkpoint-callback
                 (niwa-session-checkpoint-capture
                  session "/root" (niwa-session-artifacts--live-sidecar session buf))))
      (niwa--chat-buffer-init-common
       buf workspace (or lifecycle-source "resume") (not acquired))
      (when acquired
        (niwa-schedule-restore-session session buf))
      (+ ptc-consumptions
         (prog1
             (niwa-agent-persistence-restore-tree
              session buf (bound-and-true-p niwa-session--read-only-mode))
           (niwa-session-artifacts-load-instructions session))))))

(defun niwa-session-persistence--finish-restored-buffer
    (buf session live persist-repairs-p &optional repair-artifacts repair-checkpoint)
  "Finish restoring BUF for SESSION and return BUF.
LIVE means BUF was already initialized.  When PERSIST-REPAIRS-P is non-nil,
write repaired sidecar state before rendering the companion view.
REPAIR-ARTIFACTS are portable transcript replacements published before the
sidecar commit marker.  REPAIR-CHECKPOINT was frozen with the root repair."
  (with-current-buffer buf
    (when persist-repairs-p
      (let ((published-p nil))
        (condition-case err
            (progn
              (niwa-session-publication-publish
               session
               (append
                repair-artifacts
                (list (list
                       :path (niwa-session-artifacts-sidecar-path
                              (niwa-session-save-path session))
                       :content
                       (niwa-session-artifacts-printed-value
                        (niwa-session-artifacts-build-sidecar
                         session buf repair-checkpoint))
                       :checkpoint repair-checkpoint
                       :commit-marker t))))
              (setq published-p t))
          (error
           (unless (niwa-session-pending-publication session)
             (signal (car err) (cdr err)))))
        (when (and published-p
                   (cl-find buffer-file-name repair-artifacts
                            :key (lambda (artifact)
                                   (plist-get artifact :path))
                            :test #'equal))
          (set-visited-file-modtime))))
    (unless live
      (niwa-session-persistence-notify-session-event
       session 'load-history session))
    (niwa-session-persistence-notify-session-event
     session 'rerender))
  buf)

(defun niwa-session-persistence-resume-id (workspace session-id)
  "Resume WORKSPACE session SESSION-ID, or return nil when unavailable."
  (unless (and (stringp session-id)
               (not (string-empty-p session-id))
               (equal session-id (file-name-nondirectory session-id))
               (not (member session-id '("." ".."))))
    (error "Invalid session id: %S" session-id))
  (let ((session-dir
         (file-name-as-directory
          (file-name-concat
           (niwa-session-artifacts-sessions-dir workspace)
           session-id))))
    (when (file-directory-p session-dir)
      (niwa-session-persistence-restore session-dir nil nil workspace))))

(defun niwa-session-persistence--cold-workspace (session-dir sidecar)
  "Return the workspace SIDECAR names, as reachable from SESSION-DIR.

The persisted root is target-native, so it is only a complete path for a
client whose filesystem authority is the target itself.  SESSION-DIR is a
live path below that same root, so its TRAMP prefix -- empty on the target,
the client's spelling of the connection elsewhere -- qualifies the persisted
root for the client doing the restore."
  (let* ((saved (plist-get sidecar :workspace))
         (root (concat (file-remote-p session-dir)
                       (plist-get saved :target-native-root))))
    (niwa-workspace-get-or-create
     (plist-get saved :type) root root (plist-get saved :name))))

(defun niwa-session-persistence--pre-lease-p (session-dir)
  "Return non-nil when SESSION-DIR is a file session from before the lease.
It has a PID lock or a plain sidecar, but no lease.  Only failure paths ask."
  (and (not (niwa-session-control-fs-path-exists-p
             (file-name-concat session-dir ".lease")))
       (or (niwa-session-control-fs-path-exists-p
            (file-name-concat session-dir ".lock"))
           (niwa-session-control-fs-path-exists-p
            (niwa-session-artifacts-sidecar-path session-dir)))))

(defun niwa-session-persistence-restore
    (session-dir &optional lifecycle-source session-override workspace)
  "Restore the chat buffer for the session at SESSION-DIR.

Loads the exact current-version sidecar, opens the live segment file in a
buffer named
per `niwa-session-buffer-name', enables `org-mode' and
`gptel-mode' without gptel's Org-state restore, restores text-property
bounds through niwa's transcript path, hydrates the session struct on
the buffer, then runs `niwa--chat-buffer-init-common'.

Returns the chat buffer.  If a buffer for this session is already
alive (matched by session-id), switches to it instead of re-loading.

LIFECYCLE-SOURCE defaults to \"resume\".  SESSION-OVERRIDE, when non-nil,
is the already-projected in-memory session used by a newly published fork.
WORKSPACE is the currently opened workspace authority.

Tasks are deserialized from the sidecar.  Touched-files and
mentions-shown reset to empty hash tables on load."
  (let* ((_authority-check
          (condition-case err
              (niwa-session-codec-authority-mode-for-path
               session-dir session-override
               (and workspace
                    (niwa-session-authority-mode-for-workspace workspace)))
            (error
             (if (niwa-session-persistence--pre-lease-p session-dir)
                 (user-error niwa-session-persistence--pre-lease-message
                             session-dir)
               (signal (car err) (cdr err))))))
         (publication
          (or (niwa-session-publication-read session-dir)
              (if (niwa-session-persistence--pre-lease-p session-dir)
                  (user-error niwa-session-persistence--pre-lease-message
                              session-dir)
                (user-error "Portable session has no committed publication: %s"
                            session-dir))))
         (sidecar-path (plist-get publication :sidecar))
         (sidecar
          (niwa-session-persistence-load-sidecar
           sidecar-path (niwa-diagnostic-scope (or session-override workspace))))
         (had-sidecar-p    (not (null sidecar)))
         (opened-workspace
          (or workspace
              (and session-override
                   (niwa-session-workspace session-override))
              (and sidecar
                   (niwa-session-persistence--cold-workspace
                    session-dir sidecar))
              (niwa-workspace)))
         (result           (when sidecar
                             (niwa-session-codec-deserialize
                              sidecar opened-workspace)))
         (session          (or session-override
                               (plist-get result :session)
                               (user-error
                                "Portable session has no valid published sidecar: %s"
                                session-dir)))
         (agent-registry-repaired-p
          (plist-get result :agent-registry-repaired-p))
         (additional-roots (plist-get result :additional-roots))
         (workspace        (niwa-session-workspace session))
         (sidecar-current-n (and had-sidecar-p
                                 (niwa-session-current-segment session))))
    ;; `save-path' is intentionally not serialized in the sidecar: the
    ;; session directory itself is the source of truth at restore time.
    (setf (niwa-session-save-path session)
          (file-name-as-directory session-dir)
          (niwa-session-publication session) publication)
    ;; Workspace relocation: rewrite path-bearing fields if the saved
    ;; root no longer matches the current workspace's root.
    (when had-sidecar-p
      (niwa-session-artifacts-reconcile-relocation
       session (plist-get sidecar :workspace)))
    (let* ((buf-name     (niwa-session-buffer-name
                          (niwa-session-name session)
                          workspace))
           (session-id   (niwa-session-session-id session))
           ;; Prefer session-id-based lookup so two saved sessions
           ;; named `main' in one workspace don't collide.
           (live         (niwa-session-persistence-find-live-buffer
                          session-id buf-name))
           (cwd-retargeted-p
            (when (and (not live)
                       (not (file-directory-p
                             (niwa-session-working-directory session))))
              (setf (niwa-session-working-directory session)
                    (niwa--normalize-session-directory
                     (read-directory-name
                      (format "Session directory %s is missing; resume in: "
                              (niwa-session-working-directory session))
                      (niwa-workspace-root workspace)
                      (niwa-workspace-root workspace)
                      t)
                     workspace))))
           ;; Fence before opening artifacts.  A foreign holder permits only
           ;; inspection; a live local buffer already owns its session.
           (acquired
           (unless live
              (if (and session-override
                       (niwa-session-durability-lease-owned-p session))
                  t
                (niwa-session-persistence-lock-acquire
                 session-dir buf-name session)))))
      (let ((repair-artifacts nil)
            repair-callback
            buf
            (setup-done nil))
        (unwind-protect
            (progn
              (setq repair-callback
                    (and acquired
                         (not live)
                         (lambda (artifact)
                           (push artifact repair-artifacts))))
              ;; Recheck the captured head after fencing, before restoring bytes.
              (when (and acquired (not live))
                (niwa-session-durability-call-with-reserved-lease
                 session
                 (lambda ()
                   (let ((current (niwa-session-publication-read session-dir)))
                     (unless (and current
                                  (equal (plist-get publication :head)
                                         (plist-get current :head)))
                       (user-error
                        "Session state changed while acquiring its lease; retry restore"))
                     (setf (niwa-session-publication session) current)))))
              (let* ((segment-n (niwa-session-current-segment session))
                     (segment-path
                      (niwa-session-artifacts-segment-path
                       session-dir segment-n))
                     repair-checkpoint lost-monitors
                     (agent-repairs 0))
                (setq
                 buf
                 (or
                  live
                  (condition-case err
                      (niwa-session-artifacts-find-artifact-noselect
                       session (file-name-nondirectory segment-path)
                       (not acquired))
                    (file-missing
                     (user-error "Published session segment is unavailable: %s (%s)"
                                 segment-path (error-message-string err))))))
                (unless (and buf (buffer-live-p buf))
                  (user-error "Published session segment is unavailable: %s"
                              segment-path))
                (when (and acquired (not live))
                  (niwa-session-persistence-reconcile-lost-execution-segments
                   session segment-path repair-callback
                   (lambda (facts) (push facts lost-monitors))))
                (with-current-buffer buf
                  (niwa-session-set-root-buffer session buf)
                  (unless (equal (buffer-name) buf-name)
                    (rename-buffer buf-name t))
                  (unless (and buffer-file-name
                               (equal (expand-file-name buffer-file-name)
                                      (expand-file-name segment-path)))
                    (setq buffer-file-name segment-path))
                  (when
                      (and
                       (derived-mode-p 'org-mode)
                       (fboundp
                        'niwa--chat-buffer-disable-org-element-cache))
                    (niwa--chat-buffer-disable-org-element-cache)))
                (unless live
                  (setq agent-repairs
                        (niwa-session-persistence--hydrate-restored-buffer
                         buf session workspace segment-path acquired
                         additional-roots lifecycle-source repair-callback
                         (lambda (candidate) (setq repair-checkpoint candidate))
                         lost-monitors)))
                (niwa-session-persistence--finish-restored-buffer
                 buf session live
                 (and acquired
                      (or repair-artifacts
                          (and
                           had-sidecar-p
                           (or cwd-retargeted-p
                               agent-registry-repaired-p
                               (> agent-repairs 0)
                               (and sidecar-current-n
                                    (not (= sidecar-current-n segment-n)))))))
                 (nreverse repair-artifacts) repair-checkpoint)
                ;; A crash may leave generations no settled turn reclaimed.
                ;; Schedule their scan without delaying the restored view.
                (when (and acquired (not live))
                  (niwa-session-collection-schedule session))
                (setq setup-done t)
                buf))
          ;; Any failure after acquisition releases the session lease and
          ;; removes a freshly opened buffer.
          (unless setup-done
            (when (and acquired (not live))
              (condition-case _
                  (niwa-session-persistence-lock-release
                   session-dir session)
                (error nil)))
            (when (and (not live) buf (buffer-live-p buf))
              (condition-case _
                  (kill-buffer buf)
                (error nil)))))))))


;;;###autoload
(defun niwa-save-session (&optional arg)
  "Save the current niwa session to disk explicitly.

Forces a save even when nothing has changed since the last
auto-save (useful after manual edits to the chat buffer).  Triggers lazy
materialization if the session has not yet hit disk.

With a prefix ARG, prompts for a new session name and creates an independent
child under a fresh id.  File-workspace sessions clone their directory.
Portable project sessions materialize only the parent's current committed
logical artifacts and start fresh lease and publication histories.  The
current buffer is repointed at the child; the parent remains resumable.

To rename the current session in place, use `niwa-rename-session'."
  (interactive "P")
  (let* ((data-buf
          (cond
           ((and (boundp 'niwa--session) niwa--session) (current-buffer))
           ((and (boundp 'niwa--data-buffer) niwa--data-buffer
                 (buffer-live-p niwa--data-buffer))
            niwa--data-buffer)
           (t (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "Not in a niwa chat or view buffer"))))
         (session (buffer-local-value 'niwa--session data-buf))
         (scope (niwa-diagnostic-scope (or session data-buf))))
    (unless session
      (niwa-diagnostic-user-error (niwa-diagnostic-scope data-buf) "Active buffer has no niwa session"))
    (niwa-with-diagnostic-errors scope
      (when arg
        (niwa-engine-assert-local-history session "Save As"))
      (niwa-session-artifacts-assert-mutation-authority session data-buf))
    (when arg
      (when (niwa-agent-control-active-turn-p session)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session) "Interrupt active agent turns before save-as"))
      ;; Queued input would otherwise be delivered from both copies.
      (when (niwa-session-pending-input-p session)
        (niwa-diagnostic-user-error (niwa-diagnostic-scope session)
         "Resolve pending input in the Pending Inputs cockpit or clear it with C-c C-q before save-as")))
    (niwa-with-diagnostic-errors scope
      (cond
       (arg
        (niwa-session-persistence--save-as session data-buf))
       (t
        (niwa-session-artifacts-save session data-buf nil t)
        (message "Session saved."))))))

(defun niwa-session-persistence--save-as (session data-buf)
  "Save SESSION as an independent child under a fresh id.
Called from `niwa-save-session' with a prefix arg.  Copy committed logical
artifacts and retained execution output, excluding unfinished spools.
Repoint DATA-BUF at the child after it commits."
  ;; Force materialization so the child has a durable parent.
  (unless (niwa-session-save-path session)
    (niwa-session-artifacts-save session data-buf nil t))
  (let* ((old-save-path (niwa-session-save-path session))
         (new-name (read-string
                    "Save session as (new name): "
                    (niwa-session-name session)))
         (display-name (niwa-session-naming-normalize new-name))
         (parent-dir (file-name-directory
                      (directory-file-name old-save-path)))
         (new-id (niwa-session-persistence-allocate-session-id parent-dir))
         (new-save-path (file-name-as-directory
                         (file-name-concat parent-dir new-id))))
    ;; Publish the parent completely before deriving the child.
    (with-current-buffer data-buf (niwa-session-naming-cancel))
    (niwa-session-artifacts-save session data-buf)
    (niwa-session-save-as-run
     session data-buf display-name new-id new-save-path)
    (message "Session saved as %s" display-name)
    new-save-path))


;;
;;; Hook plumbing

;;; Exit persistence

;; Turn settlement saves success, error, and abort before request teardown,
;; while request-local file-history checkpoints are still available.

(defun niwa-session-persistence--allow-emacs-exit-p ()
  "Return non-nil when every live session may exit safely."
  (let* ((unresolved nil)
         (scope 'unavailable)
         (blocker
         (cl-some
          (lambda (buffer)
            (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   (when (and (boundp 'niwa--session)
                              niwa--session)
                     (setq scope (niwa-diagnostic-scope niwa--session))
                     (cond
                      ;; Queued and in-flight async publication drains here,
                      ;; leaving the session usable if another query vetoes.
                      ((and (niwa-session-persistence--gate-owner-p
                             niwa--session)
                            (niwa-session-publication-busy-p
                             niwa--session)
                            (condition-case err
                                (progn
                                  (niwa-session-persistence--drain-temporarily
                                   niwa--session)
                                  nil)
                              (error (setq unresolved
                                           (error-message-string err))
                                     t)))
                       'publication-job)
                      ((niwa-session-pending-publication
                        niwa--session)
                       'publication)
                      ((and
                        (not
                         (eq 'foreign
                             (plist-get
                              (niwa-session-lease niwa--session)
                              :state)))
                        (niwa-execution-unsettled-mutation-p
                         niwa--session))
                       'mutation))))))
          (buffer-list))))
    (pcase blocker
      ('publication-job
       (niwa-diagnostic-message scope
        (concat
         "Session publication is unresolved (%s); run "
         "niwa-session-publication-retry or "
         "niwa-session-publication-abandon first")
        unresolved)
       nil)
      ('publication
       (niwa-diagnostic-message scope
        (concat
         "Session publication is pending; run "
         "niwa-session-publication-retry or "
         "niwa-session-publication-abandon first"))
       nil)
      ('mutation
       (niwa-diagnostic-message scope
        (concat
         "Target mutation is unsettled; stop live executions or run "
         "niwa-retry-target-readiness to acknowledge it first"))
       nil)
      (_ t))))

(defun niwa-session-persistence--stop-publications ()
  "Stop admissions and drain publication jobs of every live session.
Return the sessions whose job stayed unresolved; each is warned about once.
Their state and leases are left for explicit publication recovery."
  (let (sessions blocked)
    (dolist (buffer (buffer-list))
      (when (buffer-live-p buffer)
        (when-let* ((session (buffer-local-value 'niwa--session buffer)))
          (cl-pushnew session sessions :test #'eq))))
    (maphash (lambda (session _) (cl-pushnew session sessions :test #'eq))
             niwa-session-persistence--deferred-agent-saves)
    (dolist (session sessions)
      (when (niwa-session-persistence--gate-owner-p session)
        (condition-case err
            (niwa-session-publication-stopping session)
          (error
           (push session blocked)
           (niwa-diagnostic-warning (niwa-diagnostic-scope session)
            'niwa
            (format "Session publication is unresolved at exit; not saving or releasing: %s"
                    (error-message-string err))
            :warning)))))
    blocked))

(defun niwa-session-persistence--kill-emacs-hook ()
  "Save sessions, clean expired state, and release locks on Emacs exit.

Runs unconditionally so that locks don't outlive the Emacs process
that wrote them.  Best-effort: individual errors are swallowed so one
bad buffer can't block exit.  Publication jobs are drained and their
admissions stopped before execution teardown.  A session whose job stays
unresolved is neither saved, flushed nor released, so explicit recovery
remains possible; execution teardown still runs."
  (let* ((niwa-journal-process--inhibit-scheduling t)
        (niwa-memory-decision--inhibit-recovery t)
        (niwa-memory-pass--inhibit-scheduling t)
        (inhibit-quit t)
        (blocked (niwa-session-persistence--stop-publications)))
    (niwa-transport-cancel-idle
     niwa-session-persistence--autosaves 'conversation-autosave)
    (when (featurep 'niwa-journal-process)
      (ignore-errors (niwa-journal-process-stop-all)))
    (when (featurep 'niwa-memory-decision)
      (ignore-errors (niwa-memory-decision-stop-recovery)))
    (when (featurep 'niwa-memory-pass)
      (ignore-errors (niwa-memory-pass-stop-all)))
    (when (fboundp 'niwa-execution-teardown-all)
      (ignore-errors (niwa-execution-teardown-all)))
    ;; Agent buffers are deliberately rejected by the root writer.  Flush
    ;; them through their owner before saving sidecars or releasing leases.
    (dolist (buffer (buffer-list))
      (when (and (buffer-live-p buffer)
                 (with-current-buffer buffer
                   niwa--agent-invocation))
        (unless (memq (buffer-local-value 'niwa--session buffer) blocked)
          (niwa-session-persistence-autosave-buffer buffer))))
    ;; A debounced agent-state save left in its window would be lost, and
    ;; registry mutations do not mark the root buffer modified, so the
    ;; modified-only save loop below cannot cover them.
    (maphash (lambda (session timer)
               (cancel-timer timer)
               (unless (memq session blocked)
                 (niwa-session-persistence-autosave-buffer
                  (niwa-session-root-buffer session))))
             niwa-session-persistence--deferred-agent-saves)
    (clrhash niwa-session-persistence--deferred-agent-saves)
    (let (lock-dirs)
      (dolist (buf (buffer-list))
        (when (buffer-live-p buf)
          (with-current-buffer buf
            (when (and (boundp 'niwa--session)
                       niwa--session
                       (not niwa--agent-invocation)
                       (not (memq niwa--session blocked)))
              (when (or (buffer-modified-p)
                        niwa-session--save-failed
                        (niwa-session-publication-uncommitted-batches
                         niwa--session))
                (niwa-session-persistence-autosave-buffer
                 (niwa-session-persistence-authoritative-buffer buf)))
              ;; Seal only the completed checkpoint, even if the exit save
              ;; above included an unfinished response.  Exit starts no model.
              (ignore-errors
                (niwa-journal-capture-seal-and-schedule
                 niwa--session buf 'session-end))
              ;; An unmodified buffer still owes its queued diagnostics: a
              ;; deferred remote flush never fires once Emacs is exiting,
              ;; and an asynchronous append would die with it.
              (ignore-errors
                (let ((niwa-session-control-fs--async-wait t))
                  (niwa-session-persistence--flush-diagnostic-logs-now
                   niwa--session)))
              (when-let* ((dir (niwa-session-save-path niwa--session)))
                (cl-pushnew dir lock-dirs :test #'equal))))))
      ;; Keep live locks through cleanup so an exit-save failure cannot expose
      ;; an old session directory for deletion.  A batch Emacs (a script or
      ;; test subprocess) only registered workspaces incidentally, often its
      ;; working directory, and must not sweep the user's sessions.
      (when (and (not noninteractive)
                 (boundp 'niwa-workspace--registry)
                 (hash-table-p niwa-workspace--registry))
        (maphash
         (lambda (_ workspace)
           ;; Like activation, never open a connection here: C-g is
           ;; inhibited, so an unreachable host would hang the exit.
           (let ((root (niwa-workspace-root workspace)))
             (unless (and (stringp root) (file-remote-p root)
                          (not (file-remote-p root nil t)))
               (ignore-errors
                 (niwa-session-persistence-cleanup-expired workspace)))))
         niwa-workspace--registry))
      (niwa-session-persistence--wait-async)
      (dolist (dir lock-dirs)
        (condition-case _
            (niwa-session-persistence-lock-release dir)
          (error nil))))))

;; Install at file-load time so exit persistence runs even when the user
;; never called `niwa-install' this Emacs (e.g. running `niwa' to
;; resume is the only command invoked).  Duplicate adds are no-ops by `add-hook'.
(add-hook 'kill-emacs-query-functions
          #'niwa-session-persistence--allow-emacs-exit-p)
(add-hook 'auto-save-hook #'niwa-session-persistence-autosave)
(add-hook 'kill-emacs-hook #'niwa-session-persistence--kill-emacs-hook)


(provide 'niwa-session-persistence)

;;; niwa-session-persistence.el ends here
