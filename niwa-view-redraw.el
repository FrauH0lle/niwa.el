;;; niwa-view-redraw.el --- Full transcript re-render scheduler -*- lexical-binding: t -*-

;;; Commentary:

;; Owns full re-renders of a view buffer: planning the projection, the
;; batched `while-no-input' installation with priority turns, latest-turn
;; refresh, reanchoring after a projection and the synchronous full render.
;; `niwa-view-render' owns turn projection and live updates; its
;; `niwa-view-render-initialize' loads this module for every view.

;;; Code:

(require 'niwa-view-render)
(eval-when-compile (require 'cl-lib))

;; `cl-extra'
(declare-function cl-subseq "cl-extra" (seq start &optional end))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-fork-point-spans
                  "niwa-session-artifacts" (buffer))

;; `niwa-session-catalog'
(declare-function niwa-session-persistence-list-sessions
                  "niwa-session-catalog" (workspace &optional cached observe-sidecar))

;; `niwa-structs'
(declare-function niwa-session-workspace "niwa-structs" (cl-x) t)

;; `niwa-transcript'
(declare-function niwa-transcript--overlay-range
                  "niwa-transcript" (segments segment))
(declare-function niwa-transcript--skip-leading-properties-drawer
		  "niwa-transcript" (pos))
(declare-function niwa-transcript-scan-cancel "niwa-transcript" (job))
(declare-function niwa-transcript-scan-result "niwa-transcript" (job))
(declare-function niwa-transcript-scan-start "niwa-transcript" (start end))
(declare-function niwa-transcript-scan-step "niwa-transcript" (job))
(declare-function niwa-transcript-segments "niwa-transcript"
		  (start end))
(declare-function niwa-transcript-summary-body
		  "niwa-transcript" (source))
(declare-function niwa-transcript-summary-source
		  "niwa-transcript" (scan-start &optional agent-p))
(defvar niwa-transcript--tool-block-index)
(defvar niwa-view--path-link-memo)

;; `niwa-transcript-restore'
(declare-function niwa-transcript-restore-properties
                  "niwa-transcript-restore" (&optional only-if-missing))

;; `niwa-transport'
(declare-function niwa-transport-busy-p "niwa-transport" (target))

;; `niwa-utilities'
(declare-function niwa--gc-hold "niwa-utilities" (key live-p))
(declare-function niwa--gc-release "niwa-utilities" (key))
(declare-function niwa--timer-pending-p "niwa-utilities" (timer))

;; `niwa-view'
(declare-function niwa-view--header-string
                  "niwa-view" (data-buf))
(declare-function niwa-view--pinned-prompt "niwa-view" (window))
(declare-function niwa-view--refresh-continuation-prompt
                  "niwa-view" (data-buffer historical-p))
(declare-function niwa-view--render-status-now
                  "niwa-view" (&optional data-buf))
(declare-function niwa-view--unattended-p "niwa-view" (&optional buffer))
(declare-function niwa-view-rerender "niwa-view" (&optional buffer))
(defvar niwa-view--interaction-marker)
(defvar niwa-view--status-marker)

;; `niwa-view-composer'
(declare-function niwa-view-refresh-input-prompt
                  "niwa-view-composer" ())

;; `niwa-view-disclosure'
(declare-function niwa-view-disclosure-capture-state
                  "niwa-view-disclosure" (from to))
(declare-function niwa-view-disclosure-reset-state
                  "niwa-view-disclosure" ())
(declare-function niwa-view-disclosure-restore-state
                  "niwa-view-disclosure" (from to states))
(declare-function niwa-view-disclosure-section-bounds
                  "niwa-view-disclosure" (&optional property))
(declare-function niwa-view-disclosure-source-range
                  "niwa-view-disclosure" (data-buffer start end))
(declare-function niwa-view-disclosure-source-start
                  "niwa-view-disclosure" (source))
(declare-function niwa-view-disclosure-state-entry
                  "niwa-view-disclosure" (source vtype))
(declare-function niwa-view-disclosure-state-snapshot
                  "niwa-view-disclosure" ())

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-focused-visits
                  "niwa-view-interaction" ())
(declare-function niwa-view--interaction-rebuild
                  "niwa-view-interaction" (&optional owned visits))

;; `niwa-view-segments'
(declare-function niwa-view-segments-banner
                  "niwa-view-segments" ())
(declare-function niwa-view-segments-chats-line
                  "niwa-view-segments" ())

;; `niwa-view-stream'
(declare-function niwa-view--ensure-request-progress
                  "niwa-view-stream" (&optional data-buf status))
(declare-function niwa-view--schedule-tool-boundary-render
                  "niwa-view-stream" (data-buf))
(declare-function niwa-view--snapshot-tool-animation-targets
                  "niwa-view-stream" ())
(declare-function niwa-view-stream-in-flight-turn-start-position
                  "niwa-view-stream" ())
(declare-function niwa-view-stream-set-in-flight-turn-start
                  "niwa-view-stream" (position))
(defvar niwa-view--carried-tool-samples)
(defvar niwa-view--data-turn-start)
(defvar niwa-view--pending-tool-calls)

;; `niwa-view-viewport'
(declare-function niwa-view--call-with-render-boundaries-advancing
                  "niwa-view-viewport" (thunk &optional types))
(declare-function niwa-view-call-preserving-user-view-state
                  "niwa-view-viewport" (thunk &optional region follow-output))
(declare-function niwa-view-transcript-tail "niwa-view-viewport" ())
(declare-function niwa-view-transcript-tail-marker "niwa-view-viewport" ())


;;
;;; Full re-render
(defun niwa-view--live-tail-rendered-position
    (live-tail limit &optional prefix-start prefix-end)
  "Return the position where LIVE-TAIL already appears before LIMIT.

Full rerenders may preserve an in-flight view tail when the data buffer
has not yet received a replacement assistant turn.  Some refresh paths
can lose the data-turn anchor while the same assistant text is already
renderable from the data buffer; in that case appending the preserved
tail would duplicate the visible transcript.  PREFIX-START restricts
stable-prefix matching to the latest rendered assistant turn, which
ends at PREFIX-END."
  (let* ((tail (string-trim
                (substring-no-properties (or live-tail ""))))
         (lines (and (not (string-empty-p tail))
                     (split-string tail "\n[ \t\n]*" t "[ \t]+")))
         (stable-lines
          (cl-loop for line in lines
                   unless (niwa-view--volatile-live-tail-line-p line)
                   collect line))
         (prefix-lines (cl-subseq stable-lines
                                  0 (min 2 (length stable-lines)))))
    (when lines
      (or (niwa-view--live-tail-lines-rendered-position lines limit)
          (when (and prefix-start prefix-end (cdr prefix-lines))
            (niwa-view--live-tail-lines-rendered-position
             prefix-lines prefix-end prefix-start))))))

(defun niwa-view--volatile-live-tail-line-p (line)
  "Return non-nil when LINE is too volatile for live-tail matching.
In-flight tool decorations never reach this check: they are stripped
before the tail is matched."
  (let ((trimmed (string-trim (or line ""))))
    (or (string-empty-p trimmed)
        (string-match-p "\\`[[:space:]]*… Thinking\\.\\.\\." trimmed)
        (string-match-p "\\`[[:space:]]*[✓✗●!›…]?[[:space:]]*Agent:"
                        trimmed))))

(defun niwa-view--live-tail-lines-rendered-position
    (lines limit &optional start)
  "Return position where literal LINES appear from START to LIMIT.
Blank gaps and indentation between lines are allowed."
  (when lines
    (cl-labels
        ((skip-gap ()
           (let (saw-newline)
             (while (and (< (point) limit)
                         (memq (char-after) '(?\s ?\t ?\n)))
               (when (eq (char-after) ?\n)
                 (setq saw-newline t))
               (forward-char 1))
             saw-newline))
         (looking-at-line-p (line)
           (let ((end (+ (point) (length line))))
             (and (<= end limit)
                  (equal line (buffer-substring-no-properties
                               (point) end))
                  (or (= end limit)
                      (eq (char-after end) ?\n))))))
      (let ((first (car lines))
            (rest (cdr lines)))
        (save-excursion
          (goto-char (or start (point-min)))
          (unless (bolp)
            (forward-line 1))
          (catch 'found
            (while (< (point) limit)
              (skip-chars-forward " \t" limit)
              (let ((candidate (point)))
                (save-excursion
                  (when (looking-at-line-p first)
                    (goto-char (+ (point) (length first)))
                    (catch 'mismatch
                      (dolist (line rest)
                        (unless (and (skip-gap)
                                     (looking-at-line-p line))
                          (throw 'mismatch nil))
                        (goto-char (+ (point) (length line))))
                      (throw 'found candidate)))))
              (forward-line 1))))))))

(defun niwa-view--compaction-header (&optional open)
  "Return the compaction row's header text.
A trailing marker says the row expands: `▸' collapsed, `▾' when OPEN."
  (propertize (concat "--- conversation compacted --- " (if open "▾" "▸"))
              'font-lock-face 'niwa-view-separator))

(defun niwa-view--insert-compaction-indicator
    (view-buf data-buf source &optional hook-audits)
  "Insert the compaction summary row into VIEW-BUF.
SOURCE is the summary's (START . END) range in DATA-BUF.  The row is a
source-backed disclosure, collapsed by default: expanding it shows the
summary the model received.  HOOK-AUDITS are audit records attached to it."
  (when (buffer-live-p view-buf)
    (with-current-buffer view-buf
      (save-excursion
        (goto-char (niwa-view-transcript-tail-marker))
        (let ((tail-type (marker-insertion-type (niwa-view-transcript-tail-marker))))
          (set-marker-insertion-type (niwa-view-transcript-tail-marker) t)
          (unwind-protect
              (let* ((source (niwa-view-disclosure-source-range
                              data-buf (car source) (cdr source)))
                     (saved (niwa-view-disclosure-state-entry
                             source 'compaction-summary))
                     (start (point)))
                (niwa-view--insert-rendered-tool
                 (list :header (niwa-view--compaction-header
                                (and saved (not (cdr saved))))
                       :vtype 'compaction-summary
                       :initially-collapsed-p t
                       :body-mode 'markdown-mode
                       ;; Only a remembered expansion needs the body now; a
                       ;; collapsed row costs nothing.
                       :body (and saved (not (cdr saved))
                                  (with-current-buffer data-buf
                                    (niwa-transcript-summary-body source)))
                       :hook-audits hook-audits)
                 source)
                (niwa-view-render-add-display-properties
                 start (point) 'compaction-summary))
            (set-marker-insertion-type (niwa-view-transcript-tail-marker) tail-type)))))))

(defun niwa-view--summary-hook-audits (data-buf start end)
  "Return hook audit records stored in DATA-BUF summary START..END."
  (when (and data-buf (buffer-live-p data-buf) (< start end))
    (with-current-buffer data-buf
      (niwa-view--hook-audit-records-from-text
       (buffer-substring start end)))))

(defun niwa-view--full-rerender-reset
    (data-buf session-data-buf historical-p agent-transcript-p)
  "Reset the current view before projecting DATA-BUF.
SESSION-DATA-BUF supplies the live header.  HISTORICAL-P inserts the
historical banner.  AGENT-TRANSCRIPT-P selects the headerless layout."
  (unless agent-transcript-p
    (niwa-view--refresh-continuation-prompt data-buf historical-p))
  (niwa-view-render--invalidate-live-tail)
  (niwa-view--forget-tool-row-starts)
  (if agent-transcript-p
      (progn
        (niwa-view--debug-log
         'full-rerender-delete-transcript
         :region (niwa-view--debug-region (point-min) (point-max))
         :state (niwa-view--debug-state data-buf))
        (delete-region (point-min) (point-max))
        (goto-char (point-min)))
    (niwa-view--debug-log
     'full-rerender-delete-display
     :region (niwa-view--debug-region
              (point-min)
              (marker-position (niwa-view-transcript-tail-marker)))
     :state (niwa-view--debug-state data-buf))
    (delete-region (point-min) (niwa-view-transcript-tail-marker))
    (goto-char (point-min))
    (insert (niwa-view--header-string session-data-buf))
    (if historical-p
        (insert (niwa-view-segments-banner))
      (when-let* ((line (niwa-view-segments-chats-line)))
        (insert line))))
  (set-marker (niwa-view-transcript-tail-marker) (point))
  (when (markerp niwa-view--status-marker)
    (set-marker niwa-view--status-marker (point)))
  (when (markerp niwa-view--interaction-marker)
    (set-marker niwa-view--interaction-marker (point)))
  (niwa-view--debug-log
   'full-rerender-after-header
   :state (niwa-view--debug-state data-buf)))

(defun niwa-view-render--plan-bounds (data-buf view-buf)
  "Return DATA-BUF's source bounds and summary for VIEW-BUF.
Call after restoring properties.  This does not mutate the view."
  (with-current-buffer data-buf
    (let ((scan-start
           (niwa-transcript--skip-leading-properties-drawer (point-min)))
          summary-source)
      (when-let* ((transcript-start
                   (buffer-local-value 'niwa-view--transcript-start view-buf))
                  ((markerp transcript-start))
                  ((eq (marker-buffer transcript-start) data-buf)))
        (setq scan-start (max scan-start (marker-position transcript-start))))
      ;; Agents keep their task anchor ahead of the summary.  Discovery stays
      ;; in full-history preparation, never on the streaming/grouping path.
      (pcase-let ((`(,source . ,next)
                   (niwa-transcript-summary-source
                    scan-start
                    (buffer-local-value 'niwa-view--agent-transcript-p
                                        view-buf))))
        (setq summary-source source
              scan-start next))
      (let ((summary-audits
             (and summary-source
                  (niwa-view--summary-hook-audits
                   data-buf (car summary-source) (cdr summary-source)))))
        (list :start scan-start :end (point-max)
              :summary-source summary-source :summary-audits summary-audits)))))

(defun niwa-view-render--without-summary (segments summary-source)
  "Return SEGMENTS without the skipped SUMMARY-SOURCE range.
Leading root summaries are already outside the scan.  An agent summary can
overlap prompt or whitespace segments; retain their portions on either side."
  (if (or (not summary-source) (not segments)
          (<= (cdr summary-source) (cadar segments)))
      segments
    (cl-remove-if
     (lambda (segment) (eq (car segment) 'compaction-summary))
     (niwa-transcript--overlay-range
      segments (list 'compaction-summary
                     (car summary-source) (cdr summary-source))))))

(defun niwa-view-render--plan-finish
    (plan turns data-buf session-data-buf agent-transcript-p)
  "Add TURNS and variant metadata to PLAN without changing the view."
  (let* ((session (and (not agent-transcript-p)
                       (buffer-local-value 'niwa--session session-data-buf)))
         (fork-points
          (vconcat
           (when (and session (niwa-session-save-path session))
             (with-current-buffer data-buf
               (save-restriction
                 (narrow-to-region (or (plist-get plan :start) (point-min))
                                   (or (plist-get plan :end) (point-max)))
                 (niwa-session-artifacts-fork-point-spans data-buf))))))
         (variants
          (when (and (> (length fork-points) 0)
                     (niwa-session-workspace session))
            ;; Settled buttons use the last live listing; activation enumerates
            ;; live instead of requiring a target round trip per rendered turn.
            (niwa-session-persistence-list-sessions
             (niwa-session-workspace session) 'cached))))
    (append plan (list :turns turns :session session :variants variants
                       :fork-points fork-points))))

(defun niwa-view--full-rerender-plan
    (data-buf session-data-buf view-buf agent-transcript-p)
  "Prepare DATA-BUF's canonical turns without changing VIEW-BUF.
SESSION-DATA-BUF supplies live session metadata.  AGENT-TRANSCRIPT-P selects
headerless agent history.  Return source limits, turns, summary and variants."
  (with-current-buffer data-buf
    (unless (niwa-view--running-agent-transcript-buffer-p)
      (niwa-transcript-restore-properties t))
    (let ((plan (niwa-view-render--plan-bounds data-buf view-buf)))
      (save-restriction
        (narrow-to-region (plist-get plan :start) (plist-get plan :end))
        (niwa-view-render--plan-finish
         plan (niwa-view--group-into-turns
               (niwa-view-render--without-summary
                (niwa-transcript-segments (point-min) (point-max))
                (plist-get plan :summary-source))
               data-buf)
         data-buf session-data-buf agent-transcript-p)))))

(defun niwa-view-render--plan-fontify-step (job)
  "Prepare one response's Markdown in JOB without projecting a partial turn.
Non-response segments are skipped in groups.  Completed output enters the
bounded persistent response cache as well as JOB's publication table."
  (if-let* ((fontify (plist-get job :fontify)))
      (when (niwa-view--markdown-fontify-job-step fontify 2048)
        (let ((key (plist-get job :fontify-key))
              (output (niwa-view--markdown-fontify-job-result fontify)))
          (let ((inhibit-quit t))
            (when (hash-table-p niwa-view--response-fontify-cache)
              (niwa-view--cache-put niwa-view--response-fontify-cache key output
                                      'niwa-view--response-cache-entries))
            (puthash key output (plist-get job :fontified))
            (niwa-view--markdown-fontify-job-cancel fontify)
            (setf (plist-get job :fontify) nil
                  (plist-get job :fontify-key) nil))))
    (let ((turns (plist-get job :fontify-turns))
          (segments (plist-get job :fontify-segments))
          (count 0))
      (while (and (< count 24) (not (plist-get job :fontify))
                  (or segments turns))
        (unless segments
          (setq segments (plist-get (car turns) :segments)
                turns (cdr turns)))
        (when-let* ((segment (pop segments))
                    ((eq (car segment) 'response))
                    ((and niwa-view-fontify-responses
                          (niwa-view--markdown-fontify-mode)))
                    (text (with-current-buffer (plist-get job :data)
                            (niwa-view--visible-response-text
                             (string-trim (buffer-substring-no-properties
                                           (cadr segment) (caddr segment))))))
                    ((not (string-empty-p text))))
          (let ((key (list :response niwa-view-fontify-responses
                           (niwa-view--markdown-fontify-mode)
                           (niwa-view--render-cache-key text))))
            (unless (or (gethash key (plist-get job :fontified))
                        (and (hash-table-p niwa-view--response-fontify-cache)
                             (gethash key niwa-view--response-fontify-cache)))
              ;; Mode setup and private-buffer ownership transfer are atomic;
              ;; subsequent fontification is interruptible, including long lines.
              (let ((inhibit-quit t))
                (setf (plist-get job :fontify-key) key
                      (plist-get job :fontify)
                      (niwa-view--markdown-fontify-job-start text))))))
        (let ((inhibit-quit t))
          (setf (plist-get job :fontify-turns) turns
                (plist-get job :fontify-segments) segments))
        (setq count (1+ count)))
      (unless (or turns segments (plist-get job :fontify))
        (let ((states (niwa-view-disclosure-state-snapshot)))
          (let ((inhibit-quit t))
            (setf (plist-get job :tools-turns) (plist-get job :turns)
                  ;; Preparation reads remembered fold choices, not reader/composer
                  ;; anchors.  Any intervening view mutation invalidates its entries;
                  ;; publication still captures the reader's current state below.
                  (plist-get job :tool-state-tick) (buffer-modified-tick)
                  (plist-get job :tool-states) states
                  (plist-get job :prepare-stage) 'tools)))))))

(defun niwa-view-render--plan-tool-step (job)
  "Prepare a few settled tool entries in JOB without mutating the view.
The same cache is used when projecting the canonical run.  The source and
the reader's disclosure state are checked again before publication."
  (let ((turns (plist-get job :tools-turns))
        (segments (plist-get job :tools-segments))
        (data (plist-get job :data))
        (plan (plist-get job :plan))
        (count 0)
        (niwa-view--activity-entry-cache (plist-get job :tool-cache)))
    (while (and (< count 4) (or segments turns))
      (unless segments
        (setq segments
              (with-current-buffer data
                (save-restriction
                  (narrow-to-region (plist-get plan :start) (plist-get plan :end))
                  (cl-remove-if-not
                   (lambda (segment)
                     (niwa-view--tool-activity-tool-segment-p segment data))
                   (niwa-view--merge-tool-hook-audit-segments
                    (plist-get (car turns) :segments) data))))
              turns (cdr turns))
        (let ((inhibit-quit t))
          (setf (plist-get job :tools-turns) turns
                (plist-get job :tools-segments) segments)))
      (when-let* ((segment (car segments)))
        (with-current-buffer data
          (save-restriction
            (narrow-to-region (plist-get plan :start) (plist-get plan :end))
            (with-current-buffer (plist-get job :view)
              (niwa-view--tool-segment-entry segment data))))
        ;; Commit each normalized entry independently.  An interrupted callback
        ;; may repeat the current entry, never the earlier entries in its group.
        (setq segments (cdr segments))
        (let ((inhibit-quit t))
          (setf (plist-get job :tools-segments) segments)))
      (setq count (1+ count)))
    (unless (or turns segments)
      (setf (plist-get job :prepare-stage) 'finish))))

(defun niwa-view-render--plan-step (job)
  "Advance JOB's canonical source preparation by one bounded stage.
Return non-nil when the complete plan is ready.  Until then the old view
remains intact; no unclassified placeholder can acquire a wrong prompt."
  (let ((data (plist-get job :data))
        (view (current-buffer))
        (stage (plist-get job :prepare-stage)))
    (pcase stage
      ('restore
       (with-current-buffer data
         (unless (niwa-view--running-agent-transcript-buffer-p)
           (niwa-transcript-restore-properties t))
         (setf (plist-get job :tick) (buffer-modified-tick)))
       (setf (plist-get job :prepare-stage) 'bounds))
      ('bounds
       (let* ((plan (niwa-view-render--plan-bounds data view))
              (scan (with-current-buffer data
                      (save-restriction
                        (narrow-to-region (plist-get plan :start) (plist-get plan :end))
                        (niwa-transcript-scan-start (point-min) (point-max))))))
         (let ((inhibit-quit t))
           (setf (plist-get job :plan) plan
                 (plist-get job :scan) scan
                 (plist-get job :prepare-stage) 'scan))))
      ('scan
       (when (with-current-buffer data
               (save-restriction
                 (narrow-to-region (plist-get (plist-get job :plan) :start)
                                   (plist-get (plist-get job :plan) :end))
                 (niwa-transcript-scan-step (plist-get job :scan))))
         (let ((inhibit-quit t))
           (setf (plist-get job :remaining)
                 (niwa-transcript-scan-result (plist-get job :scan))
                 (plist-get job :scan) nil
                 (plist-get job :prepare-stage) 'filter))))
      ('filter
       (let ((rest (plist-get job :remaining))
             (filtered (plist-get job :filtered-rev))
             (summary-source (plist-get (plist-get job :plan) :summary-source))
             (count 0))
         (while (and rest (< count 24))
           (if (and summary-source
                    (< (cadar rest) (cdr summary-source))
                    (> (caddar rest) (car summary-source)))
               (dolist (segment (niwa-view-render--without-summary
                                 (list (car rest)) summary-source))
                 (push segment filtered))
             (push (car rest) filtered))
           (setq rest (cdr rest) count (1+ count)))
         (let ((remaining (if rest rest (reverse filtered))))
           (let ((inhibit-quit t))
             (setf (plist-get job :remaining) remaining
                   (plist-get job :filtered-rev) (and rest filtered))
             (unless rest
               (setf (plist-get job :prepare-stage) 'group))))))
      ('group
       (let* ((rest (plist-get job :remaining))
              (chunk (cl-loop for seg in rest repeat 24 while seg collect seg))
              (remaining (nthcdr (length chunk) rest))
              (result (niwa-view--group-into-turns
                       chunk data (plist-get job :group-state) (car remaining))))
         (let ((inhibit-quit t))
           (setf (plist-get job :remaining) remaining)
           (if remaining
               (setf (plist-get job :group-state) result)
             (setf (plist-get job :turns) result
                   (plist-get job :fontify-turns) result
                   (plist-get job :group-state) nil
                   (plist-get job :prepare-stage) 'fontify)))))
      ('fontify
       (niwa-view-render--plan-fontify-step job))
      ('tools
       (niwa-view-render--plan-tool-step job))
      ('finish
       (let ((plan (niwa-view-render--plan-finish
                    (plist-get job :plan) (plist-get job :turns) data
                    (plist-get job :live-data) (plist-get job :agent))))
         (let ((inhibit-quit t))
           (setf (plist-get job :plan) plan
                 (plist-get job :turns) nil
                 (plist-get job :fontify-turns) nil
                 (plist-get job :fontify-segments) nil
                 (plist-get job :tools-turns) nil
                 (plist-get job :tools-segments) nil
                 (plist-get job :remaining) nil
                 (plist-get job :prepare-stage) 'done)))))
    (eq (plist-get job :prepare-stage) 'done)))

(defun niwa-view--full-rerender-project
    (data-buf session-data-buf render-view-buf
              agent-transcript-p data-turn-start-pos saved-states)
  "Project DATA-BUF into its view and return the last-turn rendering state.
SESSION-DATA-BUF supplies live session metadata.  RENDER-VIEW-BUF is
the view when AGENT-TRANSCRIPT-P marks an agent transcript.
DATA-TURN-START-POS identifies the live turn.  SAVED-STATES restores
matching disclosure state."
  (let* ((niwa-view--markdown-resolved-mode
          (let ((niwa-view--markdown-resolved-mode 'unresolved))
            (niwa-view--markdown-fontify-mode)))
         (view-buf (if agent-transcript-p render-view-buf
                     (buffer-local-value 'niwa--view-buffer data-buf)))
         (plan (niwa-view--full-rerender-plan
                data-buf session-data-buf view-buf agent-transcript-p))
         (niwa-view--conversation-variant-sessions (plist-get plan :variants))
         (niwa-view--conversation-fork-points (plist-get plan :fork-points))
         last-assistant-turn-start last-assistant-turn-end
         last-assistant-turn-data-start last-current-assistant-turn-start
         last-current-assistant-turn-data-start last-turn-role)
    (when-let* ((summary-source (plist-get plan :summary-source)))
      (niwa-view--insert-compaction-indicator
       view-buf data-buf summary-source (plist-get plan :summary-audits)))
    (with-current-buffer data-buf
      (save-restriction
        (narrow-to-region (plist-get plan :start) (plist-get plan :end))
        (with-current-buffer view-buf
          (dolist (turn (plist-get plan :turns))
            (setq last-turn-role (plist-get turn :role))
            (when (eq last-turn-role 'assistant)
              (let ((view-turn-start
                     (copy-marker (niwa-view-transcript-tail-marker) nil)))
                (setq last-assistant-turn-start view-turn-start
                      last-assistant-turn-data-start (plist-get turn :start))
                (when (and data-turn-start-pos (plist-get turn :end)
                           (> (plist-get turn :end) data-turn-start-pos))
                  (setq last-current-assistant-turn-start view-turn-start
                        last-current-assistant-turn-data-start (plist-get turn :start)))))
            (niwa-view--render-turn turn data-buf t (plist-get plan :session))
            (when (eq last-turn-role 'assistant)
              (setq last-assistant-turn-end
                    (copy-marker (niwa-view-transcript-tail-marker) nil))))
          (when saved-states
            (niwa-view-disclosure-restore-state
             (point-min) (niwa-view-transcript-tail-marker) saved-states)))))
    (list :view-buffer view-buf
          :last-assistant-turn-start last-assistant-turn-start
          :last-assistant-turn-end last-assistant-turn-end
          :last-assistant-turn-data-start last-assistant-turn-data-start
          :last-current-assistant-turn-start last-current-assistant-turn-start
          :last-current-assistant-turn-data-start last-current-assistant-turn-data-start
          :last-turn-role last-turn-role)))

(defconst niwa-view-render--gc-grace 5
  "Seconds a finished history job keeps deferring collection during typing.")

(defun niwa-view-render-cancel-batch ()
  "Cancel this view's pending full-history callbacks and release their markers."
  (when niwa-view-render--batch
    (when-let* ((timer (plist-get niwa-view-render--batch :timer)))
      (cancel-timer timer))
    (when-let* ((scan (plist-get niwa-view-render--batch :scan)))
      (niwa-transcript-scan-cancel scan))
    (when-let* ((fontify (plist-get niwa-view-render--batch :fontify)))
      (niwa-view--markdown-fontify-job-cancel fontify))
    (dolist (entry (plist-get niwa-view-render--batch :pending))
      (set-marker (nth 1 entry) nil)
      (set-marker (nth 2 entry) nil))
    (setf (plist-get niwa-view-render--batch :settled-until)
          (+ (float-time) niwa-view-render--gc-grace))
    (setq niwa-view-render--batch nil)))

(defun niwa-view-render--hold-gc (job)
  "Hold the busy collection threshold while this view runs batch JOB.
Collection waits for a pause in typing while the job runs and briefly
after it: a refresh allocates tens of megabytes, and one collection
landing in a callback was its slowest step.  A job parked on an
unattended view releases the hold, which would otherwise keep the
threshold raised and the maintenance timer waking each second until the
view is shown again; resuming takes it again."
  (let ((view (current-buffer)))
    (niwa--gc-hold
     job (lambda ()
           (or (and (buffer-live-p view)
                    (eq job (buffer-local-value
                             'niwa-view-render--batch view)))
               (< (float-time)
                  (or (plist-get job :settled-until) 0)))))))

(defun niwa-view-render-resume-batch ()
  "Resume this view's paused history work without scheduling duplicate callbacks."
  (when (and niwa-view-render--batch
             (or (eq (plist-get niwa-view-render--batch :phase) 'prepare)
                 (plist-get niwa-view-render--batch :pending))
             (not (niwa-view--unattended-p))
             (not (niwa--timer-pending-p
                   (plist-get niwa-view-render--batch :timer))))
    (niwa-view-render--hold-gc niwa-view-render--batch)
    (setf (plist-get niwa-view-render--batch :timer)
          (run-at-time 0.001 nil #'niwa-view-render--batch-step
                       (current-buffer) niwa-view-render--batch))))

(defun niwa-view-render--priority-turns (turns &optional anchors-only)
  "Return TURNS visible to the current view's readers.
With ANCHORS-ONLY, return only turns needed immediately to restore point,
selection and window starts, plus their governing prompts; other visible
turns may follow in callbacks."
  (let ((ranges (list (cons (point) (1+ (point))))) sources selected prompt)
    (when (and mark-active (mark))
      (push (cons (region-beginning) (1+ (region-end))) ranges))
    (dolist (window (niwa-view--windows 'geometry))
      ;; The response at the top is anchored, but its governing user turn
      ;; can be farther up.  Keep that header in the immediate projection.
      (when anchors-only
        (when-let* ((pinned (niwa-view--pinned-prompt window)))
          (push (cons (car pinned) (1+ (car pinned))) ranges)))
      (push (cons (window-start window)
                  (if anchors-only
                      (1+ (window-start window))
                    (or (window-end window) (window-start window)))) ranges)
      (push (cons (window-point window) (1+ (window-point window))) ranges))
    (dolist (range ranges)
      (let ((pos (max (point-min) (car range)))
            (end (min (point-max) (cdr range))))
        (while (< pos end)
          (when-let* ((source (get-text-property pos 'niwa-view-source))
                      ((consp source))
                      (start (niwa-view-disclosure-source-start source)))
            (push start sources))
          (setq pos (next-single-property-change pos 'niwa-view-source nil end)))))
    (dolist (turn turns)
      (when (and anchors-only (eq (plist-get turn :role) 'user))
        (setq prompt turn))
      (when (seq-some (lambda (source)
                       (and (<= (plist-get turn :start) source)
                            (< source (plist-get turn :end)))) sources)
        (when (and anchors-only prompt (not (eq prompt turn))
                   (not (memq prompt selected)))
          (push prompt selected))
        (push turn selected)))
    ;; A new view has no source anchors.  Its current conversation comes first.
    (or (nreverse selected)
        (if (and anchors-only prompt (not (eq prompt (car (last turns)))))
            (list prompt (car (last turns)))
          (last turns)))))

(defun niwa-view-render--batch-turn (job entry)
  "Replace JOB's placeholder ENTRY with its canonical rendered turn."
  (let* ((data (plist-get job :data))
         (plan (plist-get job :plan))
         (start (nth 1 entry))
         (end (nth 2 entry))
         (insertion (copy-marker start t))
         (niwa-view--render-insertion-marker insertion)
         (niwa-view--markdown-resolved-mode
          (if (plist-member job :markdown-mode)
              (plist-get job :markdown-mode)
            (niwa-view--markdown-fontify-mode)))
         (niwa-view--conversation-variant-sessions (plist-get plan :variants))
         (niwa-view--conversation-fork-points (plist-get plan :fork-points))
         (niwa-transcript--tool-block-index (plist-get job :index))
         (niwa-transcript-audit--decode-cache (plist-get job :audits))
         (niwa-view--activity-entry-cache (plist-get job :tool-cache))
         (view (current-buffer))
         (inhibit-read-only t))
    (unwind-protect
        (progn
          (set-marker-insertion-type start nil)
          (set-marker-insertion-type end t)
          (delete-region start end)
          (with-current-buffer data
            (save-restriction
              (narrow-to-region (plist-get plan :start) (plist-get plan :end))
              (with-current-buffer view
                (niwa-view--render-turn (car entry) data t (plist-get plan :session)))))
          (niwa-view-disclosure-restore-state
           start end (plist-get job :states)))
      (set-marker insertion nil)
      (set-marker-insertion-type start t)
      (set-marker-insertion-type end nil))))

(defun niwa-view-render--replace-turn (data plan turn pos)
  "Re-render TURN from DATA under PLAN in place of the rendered turn at POS.
PLAN supplies the source restriction, session and variants, as a turn
context does.  Preserve reader, composer and disclosure state.  Return
non-nil on success; nil leaves the old rendering in place."
  (let* ((bounds (save-excursion
                   (goto-char pos)
                   (niwa-view-disclosure-section-bounds
                    'niwa-view-turn-id)))
         (from (copy-marker (car bounds)))
         (to (copy-marker (cdr bounds)))
         (copy (copy-sequence turn))
         (job (list :data data :plan plan
                    :index (make-hash-table :test #'eq)
                    :audits (make-hash-table :test #'equal)
                    :states (niwa-view-disclosure-capture-state from to))))
    (setf (plist-get copy :render-id)
          (get-text-property pos 'niwa-view-turn-id))
    (unwind-protect
        (condition-case err
            (progn
              (niwa-view-call-preserving-user-view-state
               (lambda ()
                 (atomic-change-group
                   (niwa-view-render--batch-turn
                    job (list copy from to)))))
              t)
          (error
           (niwa-view--debug-log 'local-render-recovery :error err)
           nil))
      (set-marker from nil)
      (set-marker to nil))))

(defun niwa-view-render--refresh-source (data start end)
  "Refresh the rendered turn containing DATA's START..END, if still current.
Return non-nil on success.  The caller already owns view mutation.  A source
change or missing context leaves the existing full-history fallback in charge."
  (when (and (buffer-live-p data)
             (eq data (niwa-view-segments-display-buffer)))
    (let ((pos (point-min))
          (limit (niwa-view-transcript-tail))
          (tick (with-current-buffer data (buffer-modified-tick))))
      (catch 'refreshed
        (while (< pos limit)
          (let* ((context (get-text-property pos 'niwa-view-turn-context))
                 (turn (plist-get context :turn)))
            (when (and (eq data (plist-get context :data))
                       (eql tick (plist-get context :tick))
                       (<= (plist-get turn :start) start end
                           (plist-get turn :end)))
              (throw 'refreshed
                     (niwa-view-render--replace-turn data context turn pos)))
            (setq pos (next-single-property-change
                       pos 'niwa-view-turn-context nil limit))))))))

(defun niwa-view-render--last-turn-position ()
  "Return the start of the last rendered turn before the composer, or nil."
  (let ((pos (niwa-view-transcript-tail))
        found)
    (while (and (not found) pos (> pos (point-min)))
      (setq pos (previous-single-property-change
                 pos 'niwa-view-turn-context nil (point-min)))
      (when (get-text-property pos 'niwa-view-turn-context)
        (setq found pos)))
    found))

(defun niwa-view-render--refresh-latest-turn-now (data)
  "Re-render DATA's newest assistant turn through DATA's end.
Return non-nil when the view is current afterwards; nil asks for a full
projection.  The caller owns view mutation."
  (cond
   ((not (buffer-live-p data)) t)
   ;; An archived segment is on display: the live turn is not rendered.
   ((not (eq data (niwa-view-segments-display-buffer))) t)
   ((niwa-view-stream-in-flight-turn-start-position) nil)
   (t
    (when-let* ((pos (niwa-view-render--last-turn-position))
                (context (get-text-property pos 'niwa-view-turn-context))
                ((eq data (plist-get context :data)))
                (turn (plist-get context :turn))
                ((eq (plist-get turn :role) 'assistant))
                (end (with-current-buffer data (point-max)))
                ((<= (plist-get turn :end) end))
                (turns
                 (with-current-buffer data
                   (save-restriction
                     (narrow-to-region (plist-get context :start) end)
                     (niwa-view--group-into-turns
                      (niwa-transcript-segments (plist-get turn :start) end)
                      data))))
                (fresh (car turns))
                ((null (cdr turns)))
                ((eq (plist-get fresh :role) 'assistant))
                ((eql (plist-get fresh :start) (plist-get turn :start))))
      (let ((plan (copy-sequence context))
            (metadata (niwa-view-render--plan-finish
                       nil nil data niwa--data-buffer
                       niwa-view--agent-transcript-p)))
        (setf (plist-get plan :end) end
              (plist-get plan :variants) (plist-get metadata :variants)
              (plist-get plan :fork-points) (plist-get metadata :fork-points))
        (niwa-view-render--replace-turn data plan fresh pos))))))

(defun niwa-view-render-refresh-latest-turn (data)
  "Re-render the newest assistant turn after settlement appended to DATA.
Saving a settled turn appends its durable fork-point record after the
response, outside the coordinates that turn was rendered with.  Only that
turn needs new source coordinates, so it is rendered again alone instead
of rebuilding history.  When the newest rendered turn cannot be matched
to DATA's tail, schedule a full projection instead."
  ;; A pending history job revalidates the source tick and restarts by
  ;; itself.  Entering the writer queue would force it to finish now.
  (unless niwa-view-render--batch
    (niwa-view-render-mutate
     'latest-turn
     (lambda ()
       (unless (niwa-view-render--refresh-latest-turn-now data)
         (niwa-view-rerender (current-buffer)))))))

(defun niwa-view-render--batch-step (view job)
  "Advance one preparation stage or project one turn of JOB in VIEW."
  (when (buffer-live-p view)
    (with-current-buffer view
      (when (eq job niwa-view-render--batch)
        (when-let* ((timer (plist-get job :timer))) (cancel-timer timer))
        (setf (plist-get job :timer) nil)
        (niwa-view-render-mutate
         'full-batch
         (lambda ()
           (let ((niwa-view--markdown-resolved-mode (plist-get job :markdown-mode))
                 (niwa-view-prepare-enabled t)
                 (niwa-view--tool-cache-used (plist-get job :cache-used)))
             ;; Timers can fire inside a yielding writer.  Validate and consume
             ;; the job only after acquiring ownership, never when merely queued.
             (when (eq job niwa-view-render--batch)
               (let ((data (plist-get job :data)))
		 (cond
                  ((not (buffer-live-p data)) (niwa-view-render-cancel-batch))
                  ((niwa-view--unattended-p) (niwa--gc-release job))
                  ((niwa-transport-busy-p (buffer-local-value 'default-directory data))
                   (setf (plist-get job :timer)
			 (run-at-time 0.1 nil #'niwa-view-render--batch-step view job)))
                  ((or (not (eq data (niwa-view-segments-display-buffer)))
                       (not (= (plist-get job :tick)
                               (with-current-buffer data (buffer-modified-tick)))))
                   (niwa-view-render--start-batch))
                  ((eq (plist-get job :phase) 'prepare)
                   (condition-case err
                       (let ((niwa-transcript--tool-block-index
                              (plist-get job :index))
                             (niwa-transcript-audit--decode-cache
                              (plist-get job :audits)))
                         (pcase
                             (let ((niwa-view--tool-cache-used
                                    (plist-get job :prepared-cache-used)))
                               ;; Tag normal completion: `while-no-input' returns
                               ;; t on input abort; ordinary quit returns nil or
                               ;; propagates when the caller permits quitting.
                               ;; Only one pure preparation unit is cancellable.
                               (if (eq (plist-get job :prepare-stage) 'restore)
                                   (if (niwa-view-render--plan-step job)
                                       'ready 'pending)
                                 (while-no-input
                                   (if (niwa-view-render--plan-step job)
                                       'ready 'pending))))
                           ('ready (niwa-view-render--install-batch job))
                           ('nil (niwa-view-render-cancel-batch))
                           (_ (niwa-view-render-resume-batch))))
                     (quit
                      (niwa-view-render-cancel-batch)
                      (signal 'quit nil))
                     (error
                      (niwa-view-render-cancel-batch)
                      (niwa-view--debug-log 'batch-prepare-recovery :error err)
                      (niwa-view-render--full-now))))
                  (t
                   (condition-case err
                       (let ((niwa-view-render--prepared-responses
                              (plist-get job :fontified)))
                         (niwa-view-call-preserving-user-view-state
                          (lambda ()
                            (atomic-change-group
                              (niwa-view-render--batch-turn
                               job (car (plist-get job :pending)))))))
                     (error
                      (niwa-view-render-cancel-batch)
                      (niwa-view--debug-log 'batch-render-recovery :error err)
                      (niwa-view-render--full-now)))
                   (when (eq job niwa-view-render--batch)
                     (let ((entry (pop (plist-get job :pending))))
                       (set-marker (nth 1 entry) nil)
                       (set-marker (nth 2 entry) nil))
                     (if (plist-get job :pending)
			 (niwa-view-render-resume-batch)
                       (niwa-view--sweep-tool-rendering-cache
                        (plist-get job :cache-used))
                       (niwa-view-render-cancel-batch))))))))))))))

(defun niwa-view-render-batched-full ()
  "Rebuild history around its readers, then fill remaining turns in callbacks."
  (niwa-view-render-mutate
   'full (lambda ()
           (let ((niwa-view-prepare-enabled t))
             (niwa-view-render--start-batch)))))

(defun niwa-view-render--start-batch ()
  "Start a resumable settled-history projection with mutation ownership held.
In-flight streaming retains synchronous reconciliation of view-only live text."
  (niwa-view-render-cancel-batch)
  (if (niwa-view-stream-in-flight-turn-start-position)
      (niwa-view-render--full-now)
    (let* ((data (niwa-view-segments-display-buffer))
           (job (list :data data :view (current-buffer)
                      :markdown-mode
                      (let ((niwa-view--markdown-resolved-mode 'unresolved))
                        (niwa-view--markdown-fontify-mode))
                      :live-data niwa--data-buffer
                      :agent niwa-view--agent-transcript-p
                      :started (float-time) :phase 'prepare
                      :prepare-stage 'restore :scan nil :remaining nil
                      :filtered-rev nil :group-state nil :turns nil
                      :fontify-turns nil :fontify-segments nil
                      :fontify nil :fontify-key nil
                      :fontified (make-hash-table :test #'equal)
                      :tools-turns nil :tools-segments nil :tool-state-tick nil
                      :tool-states nil
                      :tool-cache (make-hash-table :test #'equal)
                      :prepared-cache-used (make-hash-table :test #'equal)
                      :cache-used (make-hash-table :test #'equal)
                      :plan nil :states nil :pending nil :timer nil
                      :settled-until nil
                      :tick (with-current-buffer data (buffer-modified-tick))
                      :index (make-hash-table :test #'eq)
                      :audits (make-hash-table :test #'equal))))
      (setq niwa-view-render--batch job)
      (niwa-view-render-resume-batch))))

(defun niwa-view-render--install-batch (job)
  "Publish JOB's prepared history with reader state captured now.
Until the complete plan is ready, keep the previous view in place."
  (let* ((view (current-buffer))
         (data (plist-get job :data))
         (live-data (plist-get job :live-data))
         (historical (not (eq data live-data)))
         (agent (plist-get job :agent))
         (started (plist-get job :started))
         (plan (plist-get job :plan))
         (interaction-visits (niwa-view--interaction-focused-visits))
         (niwa-view-render--prepared-responses (plist-get job :fontified))
         (niwa-transcript--tool-block-index (plist-get job :index))
         (niwa-transcript-audit--decode-cache (plist-get job :audits))
         ;; Everything a reader can see renders now.  A visible placeholder
         ;; would be drawn and then replaced, moving text the reader is on.
         (immediate (append (niwa-view-render--priority-turns
                             (plist-get plan :turns))
                            (niwa-view-render--priority-turns
                             (plist-get plan :turns) t)))
           (states (niwa-view-disclosure-capture-state
                    (point-min) (niwa-view-transcript-tail)))
           installed)
      ;; The projection removes running rows; their next live render
      ;; restores the phase they displayed.
      (when (and (not historical) niwa-view--pending-tool-calls)
        (setq niwa-view--carried-tool-samples
              (niwa-view--snapshot-tool-animation-targets)))
      ;; A reader can move onto any pending response before its callback.
      ;; Project chat prompts now so each such response has a real header
      ;; above it, with both a preview and a click target.  Keep the costly
      ;; assistant turns deferred.
      (unless agent
        (dolist (turn (plist-get plan :turns))
          (when (eq (plist-get turn :role) 'user)
            (push turn immediate))))
      (setf (plist-get job :states) states)
      (if (and (eql (buffer-modified-tick) (plist-get job :tool-state-tick))
                   ;; The fresh capture above can itself remember choices that
                   ;; preparation had not seen, without modifying view text.
                   (equal (niwa-view-disclosure-state-snapshot)
                          (plist-get job :tool-states)))
          ;; Normalized entry hits bypass the rendering cache lookup.  Accept
          ;; their dependency keys only at publication, never on an input abort.
          (maphash (lambda (key _) (puthash key t (plist-get job :cache-used)))
                   (plist-get job :prepared-cache-used))
        (clrhash (plist-get job :tool-cache)))
      (unwind-protect
          (progn
            (niwa-view-call-preserving-user-view-state
             (lambda ()
               (atomic-change-group
                 (let ((inhibit-read-only t))
                   (niwa-view--full-rerender-reset data live-data historical agent)
                   (when-let* ((summary (plist-get plan :summary-source)))
                     (niwa-view--insert-compaction-indicator
                      view data summary (plist-get plan :summary-audits)))
                   (dolist (turn (plist-get plan :turns))
                     (goto-char (niwa-view-transcript-tail-marker))
                     (let ((start (copy-marker (point) nil)))
                       (niwa-view-render--with-boundaries-advancing
                         (insert (propertize
                                  "Loading conversation turn...\n"
                                  'read-only t
                                  'niwa-view-type 'history-pending
                                  'niwa-view-turn-id (make-symbol "pending-turn")
                                  'niwa-view-source
                                  (niwa-view-disclosure-source-range
                                   data (plist-get turn :start) (plist-get turn :end)))))
                       (set-marker-insertion-type start t)
                       (push (list turn start (copy-marker (point) nil))
                             (plist-get job :pending))))
                   (setf (plist-get job :pending) (nreverse (plist-get job :pending)))
                   (dolist (entry (plist-get job :pending))
                     (when (memq (car entry) immediate)
                       (niwa-view-render--batch-turn job entry)
                       (set-marker (nth 1 entry) nil)
                       (set-marker (nth 2 entry) nil)))
                   (setf (plist-get job :pending)
                         (cl-remove-if (lambda (entry) (memq (car entry) immediate))
                                       (plist-get job :pending)))
                   (niwa-view--full-rerender-finish
                    data live-data
                    (list :view-buffer view
                          :interaction-visits interaction-visits)
                    historical
                    started)))))
            (setf (plist-get job :phase) 'project)
            (if (plist-get job :pending)
                (niwa-view-render-resume-batch)
              (niwa-view--sweep-tool-rendering-cache (plist-get job :cache-used))
              (niwa-view-render-cancel-batch))
            (setq installed t))
        (unless installed (niwa-view-render-cancel-batch)))))

(defun niwa-view--reanchor-data-turn-start (data-buf position)
  "Point `niwa-view--data-turn-start' at POSITION in DATA-BUF.
Call from the view buffer.  POSITION nil leaves the marker untouched."
  (when position
    (when (markerp niwa-view--data-turn-start)
      (set-marker niwa-view--data-turn-start nil))
    (setq niwa-view--data-turn-start
          (with-current-buffer data-buf
            (copy-marker position nil)))))

(defun niwa-view--full-rerender-reanchor
    (data-buf rendering in-flight-was data-turn-start-pos preserved-live-tail)
  "Recover the live-turn anchor after projecting DATA-BUF.
RENDERING is the projection state.  IN-FLIGHT-WAS enables recovery.
DATA-TURN-START-POS identifies the active data turn, and
PRESERVED-LIVE-TAIL contains any view-only streamed text."
  (when in-flight-was
    (let ((view-buf (plist-get rendering :view-buffer))
          (last-assistant
           (plist-get rendering :last-assistant-turn-start))
          (last-assistant-end
           (plist-get rendering :last-assistant-turn-end))
          (last-current-assistant
           (plist-get rendering :last-current-assistant-turn-start))
          (last-role (plist-get rendering :last-turn-role)))
      (with-current-buffer view-buf
        (let ((tail-start
               (and preserved-live-tail
                    (niwa-view--live-tail-rendered-position
                     preserved-live-tail (niwa-view-transcript-tail-marker)
                     last-assistant last-assistant-end))))
          (cond
           (last-current-assistant
            (niwa-view--debug-log
             'full-rerender-reanchor
             :decision 'current-assistant
             :last-turn-role last-role
             :last-assistant-turn-start last-assistant
             :last-current-assistant-turn-start last-current-assistant
             :data-turn-start data-turn-start-pos
             :state (niwa-view--debug-state data-buf))
            (niwa-view-stream-set-in-flight-turn-start
             last-current-assistant)
            (niwa-view--reanchor-data-turn-start
             data-buf
             (plist-get rendering :last-current-assistant-turn-data-start)))
           ((and (not data-turn-start-pos)
                 (eq last-role 'assistant)
                 last-assistant)
            (niwa-view--debug-log
             'full-rerender-reanchor
             :decision 'last-assistant
             :last-turn-role last-role
             :last-assistant-turn-start last-assistant
             :state (niwa-view--debug-state data-buf))
            (niwa-view-stream-set-in-flight-turn-start last-assistant)
            (niwa-view--reanchor-data-turn-start
             data-buf
             (plist-get rendering :last-assistant-turn-data-start)))
           (tail-start
            (niwa-view--debug-log
             'full-rerender-reanchor
             :decision 'existing-live-tail
             :last-turn-role last-role
             :tail-start tail-start
             :state (niwa-view--debug-state data-buf))
            (niwa-view-stream-set-in-flight-turn-start tail-start))
           (preserved-live-tail
            (goto-char (niwa-view-transcript-tail-marker))
            (niwa-view-render--with-boundaries-advancing
              (let ((tail-start (point)))
                (insert preserved-live-tail)
                (niwa-view--debug-log
                 'full-rerender-reanchor
                 :decision 'preserved-live-tail
                 :last-turn-role last-role
                 :tail-start tail-start
                 :state (niwa-view--debug-state data-buf))
                (niwa-view-stream-set-in-flight-turn-start tail-start))))
           (t
            (niwa-view--debug-log
             'full-rerender-reanchor
             :decision 'prompt-boundary
             :last-turn-role last-role
             :state (niwa-view--debug-state data-buf))
            (niwa-view-stream-set-in-flight-turn-start
             (niwa-view-transcript-tail-marker))))
          ;; The projection just rendered everything up to the data
          ;; buffer's end as settled history.  When no assistant turn
          ;; anchored the data marker above, park it at that end so the
          ;; next incremental render extracts only content that arrives
          ;; later, instead of re-rendering from a stale position -- a
          ;; whole-buffer rewrite (compaction, segment rotation)
          ;; collapses the old marker to `point-min', which made every
          ;; incremental render re-render the entire transcript.
          (unless (or last-current-assistant
                      (and (not data-turn-start-pos)
                           (eq last-role 'assistant)
                           last-assistant))
            (niwa-view--reanchor-data-turn-start
             data-buf
             (with-current-buffer data-buf (point-max)))))))))

(defun niwa-view--full-rerender-finish
    (data-buf live-data-buf rendering historical-p start-time)
  "Rebuild live chrome after projecting DATA-BUF.
LIVE-DATA-BUF owns live status.  RENDERING identifies the view.
HISTORICAL-P suppresses live-only rows.  START-TIME is for diagnostics.
The projection drew settled history only; in-flight tool calls return
with a live render of the turn.  Under a projection writer that render
queues behind it and runs before redisplay, so the running rows never
disappear from the screen; otherwise it waits for the next boundary render."
  (with-current-buffer (plist-get rendering :view-buffer)
    (when (and (not historical-p) niwa-view--pending-tool-calls)
      (if (and niwa-view-render--owner
               (niwa-view-stream-in-flight-turn-start-position)
               (markerp niwa-view--data-turn-start))
          (niwa-view-render-live-update live-data-buf)
        (niwa-view--schedule-tool-boundary-render live-data-buf)))
    (unless niwa-view--agent-transcript-p
      (niwa-view-refresh-input-prompt)
      ;; Reset removed these zones too.  Materialize them under the current
      ;; writer so its preservation wrapper can resolve their reader anchors;
      ;; queued redraws would run only after those anchors were restored.
      (niwa-view--render-status-now live-data-buf)
      (niwa-view--interaction-rebuild
       t (plist-get rendering :interaction-visits))
      (niwa-view--ensure-request-progress live-data-buf)
      (when (fboundp 'niwa-note-frame-refresh)
        (niwa-note-frame-refresh)))
    (niwa-view--debug-log
     'full-rerender-after-render
     :last-assistant-turn-start
     (plist-get rendering :last-assistant-turn-start)
     :last-current-assistant-turn-start
     (plist-get rendering :last-current-assistant-turn-start)
     :elapsed (- (float-time) start-time)
     :state (niwa-view--debug-state data-buf))))

(defun niwa-view--full-rerender (&optional transcript-buffer source-changed-p)
  "Rebuild the projection, coalescing nested requests for its current source.
TRANSCRIPT-BUFFER selects the transcript; SOURCE-CHANGED-P drops old fold state."
  (niwa-view-render-mutate
   'full
   (lambda ()
     (niwa-view-render--full-now transcript-buffer source-changed-p))
   source-changed-p))

(defun niwa-view-render--full-now (&optional transcript-buffer source-changed-p)
  "Re-render the view from TRANSCRIPT-BUFFER or its displayed transcript.
Wipe all rendered content and re-render from scratch.  Used after
compaction, session resume, or manual refresh.

When SOURCE-CHANGED-P is non-nil, do not carry disclosure state from the
previously projected transcript into this render.

Preserves the active composer, live window state, and an in-flight
assistant anchor while rebuilding the transcript projection and live
view chrome."
  (unless niwa--data-buffer
    (error "No data buffer"))
  (unless (bound-and-true-p niwa-view-prepare-enabled)
    (niwa-view-prepare-cancel))
  (niwa-view-render-cancel-batch)
  (atomic-change-group
    (niwa-view-call-preserving-user-view-state
      (lambda ()
      (let* ((niwa-view--tool-cache-used (make-hash-table :test #'equal))
             (niwa-transcript--tool-block-index (make-hash-table :test #'eq))
             (niwa-view--path-link-memo (make-hash-table :test #'equal))
             (niwa-transcript-audit--decode-cache (make-hash-table :test #'equal))
             (start-time (float-time))
             (data-buf
              (or transcript-buffer
                  (niwa-view-segments-display-buffer)))
             (live-data-buf niwa--data-buffer)
             (historical-p (not (eq data-buf live-data-buf)))
             (session-data-buf
              (if historical-p live-data-buf data-buf))
             (render-view-buf (current-buffer))
             (agent-transcript-p niwa-view--agent-transcript-p)
             (inhibit-read-only t)
             (inhibit-modification-hooks t)
             (inhibit-redisplay t)
             (saved-states
              (and (not source-changed-p)
                   (niwa-view-transcript-tail-marker)
                   (niwa-view-disclosure-capture-state
                    (point-min)
                    (niwa-view-transcript-tail-marker))))
             (data-turn-start-pos
              (and (not historical-p)
                   (markerp niwa-view--data-turn-start)
                   (marker-position niwa-view--data-turn-start)))
             (in-flight-was
              (and (not historical-p)
                   (niwa-view-stream-in-flight-turn-start-position)))
             (interaction-visits (niwa-view--interaction-focused-visits))
             (preserved-live-tail
              (when-let* (((not historical-p))
                          ((not agent-transcript-p))
                          (tail-start
                           (niwa-view-stream-in-flight-turn-start-position))
                          ((markerp niwa-view--status-marker))
                          (tail-end
                           (marker-position niwa-view--status-marker))
                          ((< tail-start tail-end)))
                (niwa-view--strip-pending-decorations-from-string
                 (buffer-substring tail-start tail-end)))))
        (when (and (not historical-p) niwa-view--pending-tool-calls)
          (setq niwa-view--carried-tool-samples
                (niwa-view--snapshot-tool-animation-targets)))
        (niwa-view--debug-log
         'full-rerender-begin
         :in-flight-was in-flight-was
         :preserved-live-tail-len
         (and preserved-live-tail (length preserved-live-tail))
         :state (niwa-view--debug-state data-buf))
        ;; Visible-state capture cannot see children of a folded group.
        ;; Retain their identities while rebuilding the same transcript.
        (when source-changed-p
          (niwa-view-disclosure-reset-state))
        (niwa-view--full-rerender-reset
         data-buf session-data-buf historical-p agent-transcript-p)
        (let ((rendering
               (niwa-view--full-rerender-project
                data-buf session-data-buf render-view-buf
                agent-transcript-p data-turn-start-pos saved-states)))
          (setq rendering (plist-put rendering :interaction-visits
                                     interaction-visits))
          (niwa-view--full-rerender-reanchor
           data-buf rendering in-flight-was
           data-turn-start-pos preserved-live-tail)
          (prog1
              (niwa-view--full-rerender-finish
               data-buf live-data-buf rendering historical-p start-time)
            (niwa-view--sweep-tool-rendering-cache
             niwa-view--tool-cache-used))))))))

(provide 'niwa-view-redraw)
;;; niwa-view-redraw.el ends here
