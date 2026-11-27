;;; niwa-schedules-list.el --- Workspace standing jobs cockpit -*- lexical-binding: t -*-

;;; Commentary:

;; Inspect and manage the current workspace's standing jobs.  The schedule
;; store owns timing, mutations, and history; this table retains only row IDs
;; and a snapshot for display.

;;; Code:

(require 'niwa-cockpit)
(require 'niwa-theme-faces)
(require 'niwa-report)
(require 'niwa-workspace)

(autoload 'niwa-schedule-timing-label "niwa-schedule")
(autoload 'niwa-schedule-pending-sessions "niwa-schedule")
(autoload 'niwa-schedule-observe-sidecar "niwa-schedule")
(autoload 'niwa-schedule-overview-late-delivery "niwa-schedule")
(autoload 'niwa-schedule-latest-late-delivery "niwa-schedule")
(autoload 'niwa--workspace-sessions "niwa-chat")
(autoload 'niwa-session-persistence-list-sessions "niwa-session-catalog")
(autoload 'niwa--display-chat-buffer "niwa-chat")
(autoload 'niwa-session-persistence-resume-id "niwa-session-persistence")

;; `niwa-workspace-schedule'
(autoload 'niwa-workspace-schedule-list "niwa-workspace-schedule")
(autoload 'niwa-workspace-schedule-runs "niwa-workspace-schedule")
(autoload 'niwa-workspace-schedule-delete "niwa-workspace-schedule")
(autoload 'niwa-workspace-schedule-set-paused "niwa-workspace-schedule-runs")

(defun niwa-schedules-list--time (seconds)
  "Format SECONDS as a local date and time, or a dash when absent."
  (if seconds (format-time-string "%Y-%m-%d %H:%M" seconds) "—"))

(defun niwa-schedules-list--entry (job _context)
  "Return JOB's tabulated entry."
  (list (plist-get job :id)
        (vector (plist-get job :name)
                (niwa-state-cell (symbol-name (plist-get job :status)))
                (niwa-schedules-list--time (plist-get job :due))
                (niwa-schedule-timing-label (plist-get job :timing)))))

(defvar-local niwa-schedules-list--receipts nil
  "Confirmed deliveries by session ID in this cockpit's workspace.")

(defvar-local niwa-schedules-list--discovering nil
  "Non-nil while a listing is collecting committed snapshots before printing.")

(defun niwa-schedules-list--remember (session)
  "Remember SESSION's live receipt for this cockpit."
  (when-let* ((receipt (niwa-schedule-latest-late-delivery session)))
    (puthash (niwa-session-session-id session) (cons (niwa-session-name session) receipt)
             niwa-schedules-list--receipts)))

(defun niwa-schedules-list--discover (context)
  "Discover saved wake-ups and confirmed delivery receipts for CONTEXT."
  (let ((ws (niwa-cockpit-context-workspace context)) (niwa-schedules-list--discovering t))
    (setq niwa-schedules-list--receipts (make-hash-table :test #'equal))
    (dolist (entry (niwa-session-persistence-list-sessions ws nil #'niwa-schedule-observe-sidecar))
      (let* ((summary (plist-get entry :summary)) (receipt (plist-get summary :latest-late-delivery)))
        (when receipt
          (puthash (plist-get summary :session-id) (cons (plist-get summary :session-name) receipt)
                   niwa-schedules-list--receipts))))
    (dolist (entry (niwa--workspace-sessions ws))
      (niwa-schedules-list--remember (buffer-local-value 'niwa--session (cdr entry))))))

(defun niwa-schedules-list-refresh ()
  "Read the workspace's saved sessions and standing jobs again."
  (interactive)
  (niwa-schedules-list--discover (niwa-cockpit-surface-context))
  (niwa-cockpit-surface-refresh))

(defun niwa-schedules-list--session-groups ()
  "Join pending wake-ups and unexpired delivery notices by session ID."
  (let ((groups (make-hash-table :test #'equal)))
    (dolist (group (niwa-schedule-pending-sessions (niwa-cockpit-context-workspace)))
      (puthash (plist-get group :session-id) group groups))
    (maphash (lambda (id entry)
               (when-let* ((notice (niwa-schedule-overview-late-delivery (cdr entry))))
                 (puthash id (plist-put (or (gethash id groups)
                                            (list :session-id id :session-name (car entry)))
                                        :notice notice) groups)))
             niwa-schedules-list--receipts)
    (sort (hash-table-values groups) :key (lambda (group) (plist-get group :session-id)))))

(defun niwa-schedules-list--groups ()
  "Keep workspace job rows and controls separate from read-only wake-ups."
  (when-let* ((groups (niwa-schedules-list--session-groups)))
    (append
     (list (cons "Workspace schedules" tabulated-list-entries) '("Session wake-ups"))
     (cl-loop for group in groups for id = (plist-get group :session-id)
              collect (cons (propertize
                             (concat (format "%s (%s)" (plist-get group :session-name) id)
                                     (when-let* ((notice (plist-get group :notice))) (concat "  " notice)))
                             'tabulated-list-id (cons id 'heading))
                            (cl-loop for wake in (plist-get group :entries) collect
                                     (list (cons id (plist-get wake :id))
                                           (vector (plist-get wake :id) (niwa-state-cell "pending")
                                                   (niwa-schedules-list--time (plist-get wake :due))
                                                   (string-join (split-string (plist-get wake :note)) " ")))))))))

(defun niwa-schedules-list--run-line (workspace run)
  "Describe RUN, linking to its saved session in WORKSPACE when present."
  (let ((id (plist-get run :session-id)))
    (concat (niwa-schedules-list--time (or (plist-get run :fired-at)
                                              (plist-get run :scheduled-at)))
            "  " (symbol-name (or (plist-get run :outcome) (plist-get run :status)))
            (if (plist-get run :notified) "  notified" "  quiet")
            (when id
              (concat "  "
                      (buttonize (copy-sequence id)
                                 (lambda (_)
                                   (if-let* ((data (niwa-session-persistence-resume-id workspace id)))
                                       (niwa--display-chat-buffer data)
                                     (niwa-diagnostic-user-error (niwa-diagnostic-scope workspace)
                                                                    "Run session unavailable: %s" id)))
                                 nil "Open run session")))
            (when-let* ((message (plist-get run :error))) (concat "  " message)))))

(defun niwa-schedules-list--details (job context)
  "Return JOB's details and most recent runs first for CONTEXT."
  (let* ((ws (niwa-cockpit-context-workspace context))
         (id (plist-get job :id)) (runs (niwa-workspace-schedule-runs ws id)))
    (list :title (plist-get job :name) :subtitle id
          :sections (list (list :id 'job :title "Standing job"
                                :body (niwa-report-fields
                                       (list "Status" (plist-get job :status))
                                       (when-let* ((reason (plist-get job :pause-reason)))
                                         (list "Pause reason" reason))
                                       (list "Timing" (niwa-schedule-timing-label (plist-get job :timing)))
                                       (list "Next run" (niwa-schedules-list--time (plist-get job :due)))
                                       (list "Expires" (niwa-schedules-list--time (plist-get job :expires-at)))))
                          (list :id 'history :title "Run history"
                                :body (if runs
                                          (mapconcat (lambda (run) (niwa-schedules-list--run-line ws run))
                                                     (reverse runs) "\n")
                                        "No runs yet."))))))

(defun niwa-schedules-list--set-paused (paused)
  "Set the selected job's PAUSED state and refresh its row."
  (let ((id (plist-get (niwa-cockpit-surface-selected) :id)))
    (niwa-workspace-schedule-set-paused (niwa-cockpit-context-workspace) id paused)))

(defun niwa-schedules-list-pause ()
  "Pause the selected standing job."
  (interactive)
  (niwa-schedules-list--set-paused t))

(defun niwa-schedules-list-resume ()
  "Resume the selected standing job."
  (interactive)
  (niwa-schedules-list--set-paused nil))

(defun niwa-schedules-list-delete ()
  "Delete the selected standing job after confirmation."
  (interactive)
  (let* ((job (niwa-cockpit-surface-selected))
         (id (plist-get job :id))
         (workspace (niwa-cockpit-context-workspace)))
    (when (yes-or-no-p (format "Delete standing job %s (%s)? " (plist-get job :name) id))
      (niwa-workspace-schedule-delete workspace id))))

(defconst niwa-schedules-list--surface
  '(:label "standing jobs" :row-label "job" :buffer-name "*niwa standing jobs*"
    :mode niwa-schedules-list-mode
    :keys (("p" "Pause job" niwa-schedules-list-pause)
           ("r" "Resume job" niwa-schedules-list-resume)
           ("d" "Delete job" niwa-schedules-list-delete))
    :format [("Job" 20 t) ("Status" 8 t) ("Next run" 16 t) ("Timing / Note" 16 t)]
    :entry niwa-schedules-list--entry
    :details niwa-schedules-list--details
    :setup niwa-schedules-list--discover
    :collect (lambda (context) (niwa-workspace-schedule-list (niwa-cockpit-context-workspace context)))
    :header (lambda (jobs context)
              (niwa-cockpit-format-header
               "standing jobs" (niwa-workspace-name (niwa-cockpit-context-workspace context))
               (format "%d jobs" (length jobs)))))
  "Standing jobs cockpit surface.")

(define-derived-mode niwa-schedules-list-mode tabulated-list-mode "Standing Jobs"
  "Browse the workspace's standing jobs."
  (niwa-cockpit-setup-tabulated-surface niwa-schedules-list--surface)
  (keymap-local-set "g" #'niwa-schedules-list-refresh)
  (setq-local tabulated-list-groups #'niwa-schedules-list--groups))

(defun niwa-schedules-list--anchor (position)
  "Return a function locating POSITION's row and column after a reprint."
  (save-excursion
    (goto-char position)
    (let ((id (tabulated-list-get-id)) (line (line-number-at-pos)) (column (current-column)))
      (lambda ()
        (save-excursion
          (goto-char (point-min))
          (if-let* ((id) (match (text-property-search-forward 'tabulated-list-id id #'equal)))
              (goto-char (prop-match-beginning match))
            (forward-line (1- line)))
          (move-to-column column)
          (point))))))

(defun niwa-schedules-list--refresh-wake-ups (root id &optional delivered)
  "Reprint wake-up groups for session ID under ROOT without reading stores.
DELIVERED is the live session with a newly confirmed receipt."
  (when-let* ((id) (buffer (get-buffer (plist-get niwa-schedules-list--surface :buffer-name))))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'niwa-schedules-list-mode)
                 (equal root (directory-file-name (niwa-workspace-root (niwa-cockpit-context-workspace)))))
        (when delivered (niwa-schedules-list--remember delivered))
        (unless niwa-schedules-list--discovering
          (let ((cursor (niwa-schedules-list--anchor (point)))
                (windows (cl-loop for w in (get-buffer-window-list nil nil t)
                                  collect (list w (niwa-schedules-list--anchor (window-point w))
                                                (niwa-schedules-list--anchor (window-start w))))))
            (tabulated-list-print)
            (goto-char (funcall cursor))
            (pcase-dolist (`(,window ,cursor ,start) windows)
              (set-window-point window (funcall cursor))
              (set-window-start window (funcall start) t))))))))

(defun niwa-schedules-list--late-delivery (session)
  "Update the overview for SESSION's confirmed delivery."
  (when-let* ((workspace (niwa-session-workspace session)))
    (niwa-schedules-list--refresh-wake-ups
     (directory-file-name (niwa-workspace-root workspace)) (niwa-session-session-id session) session)))

(defun niwa-schedules-list--refresh-workspace (workspace)
  "Refresh the standing jobs cockpit if it shows WORKSPACE's store."
  (when-let* ((buffer (get-buffer (plist-get niwa-schedules-list--surface :buffer-name))))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'niwa-schedules-list-mode)
                 (equal (niwa-workspace-state-dir workspace)
                        (niwa-workspace-state-dir (niwa-cockpit-context-workspace))))
        (niwa-cockpit-surface-refresh)))))

(add-hook 'niwa-workspace-schedule-changed-functions #'niwa-schedules-list--refresh-workspace)
(add-hook 'niwa-schedule-pending-changed-functions #'niwa-schedules-list--refresh-wake-ups)
(add-hook 'niwa-schedule-late-delivery-hook #'niwa-schedules-list--late-delivery)

;;;###autoload
(defun niwa-schedules-list-open (&optional context)
  "Open the standing jobs of CONTEXT or the current workspace."
  (interactive)
  (niwa-cockpit-open-surface
   niwa-schedules-list--surface
   (or context (niwa-cockpit-context-for-buffer (current-buffer))
       (niwa-cockpit-workspace-context
        (or (niwa-workspace)
            (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer))
                                           "No workspace here"))
        (current-buffer)))))

(provide 'niwa-schedules-list)
;;; niwa-schedules-list.el ends here
