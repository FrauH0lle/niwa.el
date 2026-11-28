;;; niwa-view-render.el --- Transcript rendering and navigation -*- lexical-binding: t -*-

;;; Commentary:

;; Owns transcript turn grouping, source-backed rendering, folding,
;; incremental redraws, and segment projection state for niwa view buffers.
;; It requires `niwa-view-tool-render', which interprets tool renderings,
;; and `niwa-view-mailbox', which presents mailbox deliveries.
;; Disclosure content and whole-turn collapse belong to
;; `niwa-view-disclosure'; full re-renders belong to `niwa-view-redraw',
;; which `niwa-view-render-initialize' loads.  Navigation commands live in
;; `niwa-view'.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-activity-group)
(require 'niwa-execution-transcript)
(require 'niwa-plan)
(require 'niwa-tool-render-data)
(require 'niwa-view-mailbox)
(require 'niwa-view-tool-render)
(require 'niwa-view-fontify)
(require 'niwa-view-pair)
(require 'niwa-utilities)
(require 'niwa-view-prepare)

;; `cl-extra'
(declare-function cl-some "cl-extra"
		  (predicate sequence &rest more-sequences))

;; `cl-macs'
(declare-function cl-gensym "cl-macs" (&optional prefix))

;; `cl-seq'
(declare-function cl-find "cl-seq" (cl-item cl-seq &rest cl-keys))


;; `niwa-view'
(defvar niwa-view--prompt-headers)

;; `niwa-agents'
(declare-function niwa-agent-invocation-p "niwa-agents" (cl-x))
(declare-function niwa-agent-invocation-transcript-status
		  "niwa-agents" (cl-x) t)

;; `niwa-note-frame'
(declare-function niwa-note-frame-refresh
                  "niwa-note-frame" ())


;; `niwa-review'
(declare-function niwa-review-strip-user-action-blocks
		  "niwa-review" (text))
(autoload 'niwa-review-strip-user-action-blocks "niwa-review")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-fork-point-at-source
                  "niwa-session-artifacts"
                  (buffer source-start source-end &optional fork-points))

;; `niwa-session-catalog'
(declare-function niwa-session-persistence-conversation-variants
                  "niwa-session-catalog"
                  (session fork-point-id &optional sessions))

;; `niwa-tool-registry'
(declare-function niwa-tool-display-string "niwa-tool-registry"
                  (tool-name args &optional scope))


;; `niwa-tool-ui'
(declare-function niwa-tool-ui--render-agent "niwa-tool-ui"
		  (name args result render-data))

;; `niwa-transcript'
(declare-function niwa-transcript--mailbox-any-block-at-point
		  "niwa-transcript" (limit))
(declare-function niwa-transcript--mailbox-open-at-point
                  "niwa-transcript" (kind limit))
(declare-function niwa-transcript--org-tool-block-parts
		  "niwa-transcript" (start end))
(declare-function niwa-transcript--system-reminder-range-at-point
		  "niwa-transcript" (limit))
(declare-function niwa-transcript--tool-block-bounds-for-run
		  "niwa-transcript"
		  (seg-start seg-end &optional limit))
(declare-function niwa-transcript--tool-id-in-range
		  "niwa-transcript" (start end))
(declare-function niwa-transcript-segments "niwa-transcript"
		  (start end))
(defvar niwa-transcript--tool-block-index)
(defvar niwa-view--path-link-memo)

;; `niwa-transcript-audit'
(autoload 'niwa--strip-hook-audit-blocks "niwa-transcript-audit")

;; `niwa-utilities'
(defvar niwa--hook-audit-close)
(defvar niwa--hook-audit-open)
(defvar niwa-transcript-audit--decode-cache)
(autoload 'niwa--warn-once "niwa-utilities")
(autoload 'niwa-diagnostic-message "niwa-utilities")
(autoload 'niwa-diagnostic-scope "niwa-utilities")
(autoload 'niwa-diagnostic-user-error "niwa-utilities")


;; `niwa-view'
(declare-function niwa-view-next-turn "niwa-view" ())
(declare-function niwa-view-switch-conversation-variant
                  "niwa-view" (fork-point-id))
(defvar niwa-view--display-map)
(defvar niwa-view--interaction-marker)
(defvar niwa-view--status-marker)
(defvar niwa-view--transcript-start)

;; `niwa-view-agent'
(declare-function niwa-view--render-agent-status
                  "niwa-view-agent" ())
(defvar niwa-view--agent-handle-map)
(defvar niwa-view--agent-path)

;; `niwa-view-composer'
(declare-function niwa-view--ensure-interactive-chat-view
                  "niwa-view-composer" ())

;; `niwa-view-disclosure'
(declare-function niwa-view--collapse-turn "niwa-view-disclosure" ())
(declare-function niwa-view-disclosure--state-key-at
                  "niwa-view-disclosure" (position mailbox-counts))
(declare-function niwa-view-disclosure-apply-rendering-state
                  "niwa-view-disclosure" (rendering source))
(declare-function niwa-view-disclosure-capture-state
                  "niwa-view-disclosure" (from to))
(declare-function niwa-view-disclosure-data-substring
                  "niwa-view-disclosure"
                  (data-buf start end &optional properties))
(declare-function niwa-view-disclosure-indent-text
                  "niwa-view-disclosure" (text width))
(autoload 'niwa-view-disclosure-indent-text "niwa-view-disclosure")
(declare-function niwa-view-disclosure-initialize
                  "niwa-view-disclosure" ())
(declare-function niwa-view-disclosure-rebase-state
                  "niwa-view-disclosure" (delta))
(declare-function niwa-view-disclosure-record-state
                  "niwa-view-disclosure" (source vtype collapsed))
(declare-function niwa-view-disclosure-record-state-for-key
                  "niwa-view-disclosure" (key collapsed))
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
(declare-function niwa-view-disclosure-state-for-key
                  "niwa-view-disclosure" (key))
(declare-function niwa-view-disclosure-state-key
                  "niwa-view-disclosure"
                  (source vtype &optional previous-key))
(declare-function niwa-view-disclosure-truncate-line
                  "niwa-view-disclosure" (text limit))
(defvar niwa-view-disclosure--settling-p)

;; `niwa-view-interaction'
(declare-function niwa-view--interaction-rebuild
                  "niwa-view-interaction" (&optional owned visits))

;; `niwa-view-markdown'
(declare-function niwa-view--last-live-response-boundary
                  "niwa-view-markdown" (data-buf start end))


;; `niwa-view-redraw'
(declare-function niwa-view--full-rerender
                  "niwa-view-redraw" (&optional transcript-buffer source-changed-p))
(declare-function niwa-view-render--full-now
                  "niwa-view-redraw" (&optional transcript-buffer source-changed-p))
(declare-function niwa-view-render-cancel-batch "niwa-view-redraw" ())

;; `niwa-view-segments'
(declare-function niwa-view-historical-segment-p
                  "niwa-view-segments" ())
(declare-function niwa-view-segments-display-buffer
                  "niwa-view-segments" ())
(declare-function niwa-view-segments-initialize
                  "niwa-view-segments" ())
(autoload 'niwa-view-segments-display-buffer "niwa-view-segments")
(autoload 'niwa-view-historical-segment-p "niwa-view-segments")

;; `niwa-view-stream'
(declare-function niwa-view--adopt-tool-animation-targets
                  "niwa-view-stream" (start end previous))
(declare-function niwa-view--ensure-request-progress
                  "niwa-view-stream" (&optional data-buf status))
(declare-function niwa-view--retire-pending-tool-rows
                  "niwa-view-stream" ())
(declare-function niwa-view--snapshot-tool-animation-targets
                  "niwa-view-stream" ())
(declare-function niwa-view-animation-frame
                  "niwa-view-animation" (style label seconds face &optional frame))
(declare-function niwa-view-stream-in-flight-turn-start-position
                  "niwa-view-stream" ())
(defvar niwa-view--data-turn-start)
(defvar niwa-view--carried-tool-samples)
(defvar niwa-view--in-flight-turn-start)
(defvar niwa-view--pending-tool-calls)
(defvar niwa-view--spinner-tool-targets)
(defvar niwa-view-tool-spinner-style)

;; `niwa-view-table'
(defvar niwa-view-table--streaming-p)

;; `niwa-view-viewport'
(declare-function niwa-view-call-preserving-user-view-state
                  "niwa-view-viewport" (thunk &optional region follow-output))
(declare-function niwa-view--call-with-render-boundaries-advancing
                  "niwa-view-viewport" (thunk &optional types))
(declare-function niwa-view--position-render-anchor
                  "niwa-view-viewport" (pos &optional window region))
(declare-function niwa-view--render-anchor-position
                  "niwa-view-viewport" (anchor &optional strict))
(declare-function niwa-view-transcript-tail "niwa-view-viewport" ())
(declare-function niwa-view-transcript-tail-marker "niwa-view-viewport" ())

;; `niwa-view-zone'
(declare-function niwa-view-zone-bounds-at
                  "niwa-view-zone" (&optional position))
(declare-function niwa-view-zone-region "niwa-view-zone" (zone))

;; `org'
(defvar org-mode-hook)


;;
;;; Customization

(defcustom niwa-view-fontify-responses t
  "Non-nil means fontify response bodies using Markdown syntax.
Each assistant response stays as model-written Markdown in the view and
is fontified through `markdown-ts-mode' when its tree-sitter grammars are
installed; otherwise the raw Markdown text stays visible."
  :type 'boolean
  :group 'niwa)


(defcustom niwa-view-hide-markdown-markup t
  "Non-nil means hide Markdown markup delimiters in the view.
Heading hashes, emphasis asterisks, and code-span backticks stay in the
text but are made invisible, so `**bold**' reads as bold alone: only
display changes, never the text the view holds or the positions it maps
back to the data buffer.  Has no effect without the `markdown-ts-mode'
grammars."
  :type 'boolean
  ;; The default needs no re-render, and the setter is defined further down.
  :initialize #'custom-initialize-default
  :set #'niwa-view--set-hide-markdown-markup
  :group 'niwa)


(defcustom niwa-view-render-debug nil
  "Non-nil means trace view buffer render decisions.
The trace is written to `niwa-view-render-debug-buffer-name'.
It includes marker positions, replacement decisions, and short text
previews around the live in-flight region.  Enable only while
reproducing a view-rendering bug."
  :type 'boolean
  :group 'niwa)

(defcustom niwa-view-render-debug-buffer-name "*niwa-view-render-trace*"
  "Name of the buffer that receives view-render debug traces."
  :type 'string
  :group 'niwa)

(defcustom niwa-view-user-input-collapse-line-threshold 15
  "User prompts longer than this many lines fold to a one-line summary.
The folded line shows the truncated first line and the hidden line
count, and expands in place.  Zero disables folding."
  :type 'integer
  :group 'niwa)

(defcustom niwa-view-tool-group-collapse-threshold 2
  "Runs of more than this many consecutive tool rows fold into one group.
The group renders as a one-line activity summary that expands into the
individual tool rows.  Zero disables grouping."
  :type 'integer
  :group 'niwa)

(defcustom niwa-view-render-cache-max-entries 256
  "Maximum number of response-fontification and source-bounds cache entries.
Tool renderings instead retain the working set used by the last full
projection; a fixed entry limit made long activity runs reparse every tool."
  :type 'integer
  :group 'niwa)


;;
;;; Summary primitives

(defconst niwa-view--thinking-glyph "… "
  "Prefix shown in front of thinking/reasoning summary lines.")

(defconst niwa-view--response-glyph "▸ "
  "Prefix shown in front of collapsed response summary lines.")


(defun niwa-view--operation-line
    (marker marker-face label &optional detail metadata label-face)
  "Return a compact propertized operation summary line.
MARKER is the leading status glyph.  MARKER-FACE styles that glyph.
LABEL is the primary label and DETAIL/METADATA are optional secondary
fragments.  LABEL-FACE defaults to `niwa-view-tool-name'."
  (concat
   (propertize (concat "  " marker " ")
               'font-lock-face marker-face)
   (propertize label
               'font-lock-face (or label-face 'niwa-view-tool-name))
   (when (and detail (not (string-empty-p detail)))
     (concat "  "
             (propertize detail
                         'font-lock-face 'niwa-view-tool-argument)))
   (when (and metadata (not (string-empty-p metadata)))
     (concat " "
             (niwa-view--tool-metadata-text metadata)))))

(defun niwa-view--tool-metadata-text (metadata)
  "Return propertized summary METADATA text.

Diff metadata of the form `(+N -M)' keeps the parenthesized wrapper in
the normal metadata face while styling the added and removed counts
separately."
  (when (and metadata (not (string-empty-p metadata)))
    (if (string-match "\\`(\\(\\+[0-9]+\\) \\(-[0-9]+\\))\\'" metadata)
        (concat
         (propertize "(" 'font-lock-face 'niwa-view-tool-metadata)
         (propertize (match-string 1 metadata)
                     'font-lock-face 'niwa-view-tool-diff-added)
         (propertize " " 'font-lock-face 'niwa-view-tool-metadata)
         (propertize (match-string 2 metadata)
                     'font-lock-face 'niwa-view-tool-diff-removed)
         (propertize ")" 'font-lock-face 'niwa-view-tool-metadata))
      (propertize metadata
                  'font-lock-face 'niwa-view-tool-metadata))))

(defun niwa-view--tool-call-line
    (marker marker-face name &optional primary-arg metadata name-face)
  "Return a propertized compact tool call line.
MARKER and MARKER-FACE describe the leading status glyph.  NAME is the
tool label.  PRIMARY-ARG, when non-empty, is rendered after a literal
colon so every tool row keeps the same `Tool: argument' shape.
METADATA is optional secondary summary text.  NAME-FACE overrides the
face used for NAME."
  (concat
   (propertize (concat "  " marker " ")
               'font-lock-face marker-face)
   (propertize (or name "Tool")
               'font-lock-face (or name-face 'niwa-view-tool-name))
   (when (and primary-arg (not (string-empty-p primary-arg)))
     (concat ": "
             (propertize primary-arg
                         'font-lock-face 'niwa-view-tool-argument)))
   (when (and metadata (not (string-empty-p metadata)))
     (concat " "
             (niwa-view--tool-metadata-text metadata)))))

(defun niwa-view--tool-result-error-p (result-text)
  "Return non-nil when RESULT-TEXT resembles a tool-level failure."
  (and (stringp result-text)
       (string-match-p
        "\\`[ \t\n]*\\(?:Error\\b\\|FAILED\\b\\|Tool failed\\b\\)"
        result-text)))

(defun niwa-view--tool-summary-line
    (name primary-arg result-lines &optional blocked error-p)
  "Return a propertized collapsed tool summary line.
NAME is the tool name, PRIMARY-ARG is the renderer-provided compact
argument, and RESULT-LINES is the number of output lines.  BLOCKED is
the hook-block plist returned by `niwa-view--tool-hook-blocked-info'.
ERROR-P means the result itself looks like a tool-level failure."
  (let* ((blocked-p (and blocked t))
         (failed-p (or blocked-p error-p))
         (summary
          (niwa-view--tool-call-line
           (if failed-p "×" "✓")
           (if failed-p
               'error
             'niwa-view-tool-marker)
           name
           primary-arg
           (format "(%d lines)" result-lines))))
    (if blocked-p
        (concat
         summary
         "\n"
         (propertize "    blocked by "
                     'font-lock-face 'niwa-view-tool-metadata)
         (propertize
          (format "%s: %s"
                  (plist-get blocked :event)
                  (plist-get blocked :reason))
          'font-lock-face 'niwa-view-tool-metadata))
      summary)))

(defun niwa-view--tool-header-fallback-info (raw)
  "Return fallback tool display info parsed from RAW's org tool header."
  (when (and (stringp raw)
             (string-match "^#\\+begin_tool[[:space:]]+(\\([^[:space:])]+\\)" raw))
    (let ((name (match-string 1 raw))
          primary)
      (dolist (key '("file_path" "path" "command" "pattern" "url" "id"))
        (when (and (not primary)
                   (string-match
                    (format ":%s[[:space:]]+\\\"\\([^\\\"]+\\)\\\"" key)
                    raw))
          (setq primary (match-string 1 raw))))
      (list :name name :primary-arg primary))))

(defun niwa-view--tool-fallback-line (raw)
  "Return a compact propertized fallback summary for unparseable RAW."
  (let* ((header-info (niwa-view--tool-header-fallback-info raw))
         (material (string-trim
                    (replace-regexp-in-string
                     "#\\+\\(?:begin\\|end\\)_\\(?:tool\\|reasoning\\)[^\n]*\n?"
                     "" (or raw "")))))
    (unless (string-empty-p material)
      (if header-info
          (niwa-view--tool-call-line
           "?"
           'niwa-view-tool-warning
           (plist-get header-info :name)
           (plist-get header-info :primary-arg)
           nil)
        (niwa-view--operation-line
         "?"
         'niwa-view-tool-warning
         (niwa--truncate-display (replace-regexp-in-string "[\n\r]+" " " raw)
          60 "...")
         nil nil
         'niwa-view-tool-summary)))))

(defun niwa-view--text-has-font-lock-face-p (text)
  "Return non-nil when TEXT already carries any `font-lock-face'."
  (and (stringp text)
       (> (length text) 0)
       (text-property-not-all 0 (length text) 'font-lock-face nil text)))

(defun niwa-view--summary-with-face (summary face)
  "Return SUMMARY with FACE when it has no existing font-lock styling."
  (if (or (null face)
          (niwa-view--text-has-font-lock-face-p summary))
      summary
    (propertize summary 'font-lock-face face)))

(defun niwa-view--insert-summary-region (summary props)
  "Insert SUMMARY followed by a newline and add non-face PROPS.
Text-local fontification in SUMMARY is preserved."
  (let ((start (point)))
    (insert summary)
    (unless (and (> (point) start)
                 (eq (char-before) ?\n))
      (insert "\n"))
    (add-text-properties start (point) props)
    start))


;;
;;; Buffer-locals


(defvar-local niwa-view--render-insertion-marker nil
  "Temporary marker used by render helpers as their insertion point.
Nil means render at the view's prompt/zone boundary.  Incremental history
rebuilds bind this to `niwa-view--status-marker' so the in-flight
assistant turn is inserted into the history region above status and
interaction zones instead of inside them.")


(defvar-local niwa-view--tool-rendering-cache nil
  "Hash table caching parsed/rendered tool metadata for this view.")

(defvar-local niwa-view--response-fontify-cache nil
  "Hash table caching response fontification for this view.")

(defvar niwa-view-render--prepared-responses nil
  "Dynamically bound response cache owned by a scheduled full projection.")

(defvar-local niwa-view--response-cache-entries 0
  "Approximate number of entries in `niwa-view--response-fontify-cache'.")

(defvar niwa-view--conversation-variant-sessions nil
  "Persisted session summaries bound once around a full render.")

(defvar niwa-view--conversation-fork-points nil
  "Ordered vector of fork-point records bound by the current projection.
Nil asks standalone renderers to query their current source instead.")

(defvar-local niwa-view--user-pre-rendered nil
  "Non-nil when the most recent user turn was pre-rendered by the view.

Set by `niwa-view--insert-user-message' when the view's send path
echoes the user's input immediately, and consumed (cleared) by
`niwa-view-stream-render-response' to skip the user turn that
`niwa-transcript-segments' may pick up for the same exchange,
which would otherwise produce a duplicate \"You\" block above the
assistant reply.  Tests that drive function
`niwa-view-stream-render-response' directly (without going through the
send path) leave the flag nil and see user
turns rendered as usual.")

(defvar-local niwa-view--live-data-tail-start nil
  "Data-buffer marker where the mutable live render tail starts.")

(defvar-local niwa-view--live-view-tail-start nil
  "View-buffer marker where the mutable live render tail starts.")

(defvar-local niwa-view--live-source-change-hook nil
  "Change hook that invalidates this view's retained live prefix.")

(defvar niwa-view-render--reader-members nil
  "Tool rows under a reader when the current projection writer started.
Each element is a row's data source start, or an in-flight call's key.")

(defvar-local niwa-view-render--pending-reader-group nil
  "Running-only group's fold, as (TURN-MARKER START COLLAPSED).
Set when the group opens beneath the reader (COLLAPSED nil) or when the
user folds it; a matching slot decides the fold over the reader's row.
START is the data end where its first result will arrive; the canonical
tool segment includes gptel's separator and Org wrapper.  Once a result
lands, transfer the fold to the source-backed group's disclosure state.
TURN-MARKER is compared by identity so an aborted turn cannot lend its
fold to a later turn starting at the same data position.")

(defvar-local niwa-view-render--batch nil
  "Pending full-history projection, owned by this view.")

(defun niwa-view-render-initialize ()
  "Initialize transcript-rendering state in the current view buffer."
  (require 'niwa-view-redraw)
  (niwa-view-render--invalidate-live-tail)
  (niwa-view-render-cancel-batch)
  (add-hook 'kill-buffer-hook #'niwa-view-render--discard-mouse-writes -90 t)
  (add-hook 'kill-buffer-hook #'niwa-view-render-cancel-batch nil t)
  (add-hook 'kill-buffer-hook #'niwa-view-render--invalidate-live-tail nil t)
  ;; Loading the mode also registers its grammars in
  ;; `treesit-language-source-alist', so the install hint has a source.
  (require 'markdown-ts-mode nil t)
  (niwa-view--markdown-grammar-hint)
  (require 'org-src nil t)
  (require 'xml)
  (require 'niwa-execution-telemetry)
  (require 'niwa-overlay-ui)
  (require 'niwa-review)
  (require 'niwa-session-artifacts)
  (require 'niwa-session-persistence)
  (require 'niwa-tool-render-data)
  (require 'niwa-transcript)
  (require 'niwa-transcript-audit)
  (require 'niwa-transcript-restore)
  (require 'niwa-view-disclosure)
  (niwa-view-prepare-initialize)
  (require 'niwa-view-segments)
  (require 'niwa-view-stream)
  (require 'niwa-view-zone)
  (setq-local niwa-view--tool-rendering-cache
              (make-hash-table :test #'equal))
  (niwa-view-disclosure-initialize)
  (setq-local niwa-view--response-fontify-cache
              (make-hash-table :test #'equal))
  (setq-local niwa-view--response-cache-entries 0)
  (setq-local niwa-view--user-pre-rendered nil)
  (setq-local niwa-view--live-data-tail-start nil)
  (setq-local niwa-view--live-view-tail-start nil)
  (setq-local niwa-view--live-source-change-hook nil)
  (niwa-view-segments-initialize))

(defun niwa-view-render--invalidate-live-tail ()
  "Forget the current view buffer's retained live-render tail."
  (let ((data-marker niwa-view--live-data-tail-start)
        (view-marker niwa-view--live-view-tail-start)
        (change-hook niwa-view--live-source-change-hook))
    (when-let* (((markerp data-marker))
                (data-buf (marker-buffer data-marker))
                ((buffer-live-p data-buf))
                ((functionp change-hook)))
      (with-current-buffer data-buf
        (remove-hook 'before-change-functions change-hook t)))
    (setq niwa-view--live-data-tail-start nil
          niwa-view--live-view-tail-start nil)
    (when (markerp data-marker)
      (set-marker data-marker nil))
    (when (markerp view-marker)
      (set-marker view-marker nil))))

(defun niwa-view--live-tail-valid-p (data-buf)
  "Return non-nil when this view's retained tail belongs to DATA-BUF."
  (and (markerp niwa-view--live-data-tail-start)
       (marker-position niwa-view--live-data-tail-start)
       (eq (marker-buffer niwa-view--live-data-tail-start) data-buf)
       (markerp niwa-view--live-view-tail-start)
       (marker-position niwa-view--live-view-tail-start)
       (eq (marker-buffer niwa-view--live-view-tail-start)
           (current-buffer))))

(defun niwa-view--live-tail-continues-p (data-buf)
  "Return non-nil when the retained tail can continue DATA-BUF's current turn.
The tail must follow the turn's view start, and its data start must still
be where its unit was drawn: an edit that bypassed change hooks, such as a
retry discarding output, moves it."
  (and (niwa-view--live-tail-valid-p data-buf)
       (when-let* ((start (niwa-view-stream-in-flight-turn-start-position)))
         (>= niwa-view--live-view-tail-start start))
       (eql (marker-position niwa-view--live-data-tail-start)
            (get-text-property niwa-view--live-view-tail-start
                               'niwa-view-live-unit-source))))

(defun niwa-view--live-tail-intact-p (data-buf first-end end)
  "Return non-nil if the retained DATA-BUF unit remains whole through END.
FIRST-END tracks the end of its original first character, with insertion
type nil.  That character must survive before one uninterrupted source run;
otherwise an advancing start marker could accept a replaced unit's suffix."
  (when (niwa-view--live-tail-valid-p data-buf)
    (let ((start (marker-position niwa-view--live-view-tail-start))
          (source (marker-position niwa-view--live-data-tail-start)))
      (and (< start end)
           (= (1+ start) first-end)
           (or (= start (point-min))
               (not (equal source
                           (get-text-property
                            (1- start) 'niwa-view-live-unit-source))))
           (not (text-property-not-all
                 start end 'niwa-view-live-unit-source source))))))

(defun niwa-view--mark-live-render-unit (start source-start)
  "Mark START through point as one live unit backed by SOURCE-START."
  (when (< start (point))
    (put-text-property start (point)
                       'niwa-view-live-unit-source source-start)))

(defun niwa-view--note-source-change (view-buf data-buf beg _end)
  "Invalidate VIEW-BUF when DATA-BUF changes before retained position BEG."
  (when (buffer-live-p view-buf)
    (with-current-buffer view-buf
      (when (and (markerp niwa-view--live-data-tail-start)
                 (niwa-view--live-tail-valid-p data-buf)
                 (< beg (marker-position niwa-view--live-data-tail-start)))
        (niwa-view-render--invalidate-live-tail)))))

(defun niwa-view--retain-last-live-render-unit (data-buf start end)
  "Retain the last live render unit between START and END for DATA-BUF."
  (let ((pos (1- end)) source)
    (while (and (>= pos start) (null source))
      (setq source (get-text-property pos 'niwa-view-live-unit-source))
      (unless source
        (setq pos (1- (or (previous-single-property-change
                           (1+ pos) 'niwa-view-live-unit-source nil start)
                          start)))))
    (if (not source)
        (niwa-view-render--invalidate-live-tail)
      (let ((view-start pos))
        (while (and (> view-start start)
                    (equal source
                           (get-text-property
                            (1- view-start)
                            'niwa-view-live-unit-source)))
          (setq view-start (previous-single-property-change
                            view-start 'niwa-view-live-unit-source
                            nil start)))
        (niwa-view-render--invalidate-live-tail)
        (unless niwa-view--live-source-change-hook
          (let ((view-buf (current-buffer)))
            (setq niwa-view--live-source-change-hook
                  (lambda (beg end)
                    (niwa-view--note-source-change
                     view-buf (current-buffer) beg end)))))
        (let ((change-hook niwa-view--live-source-change-hook))
          (setq niwa-view--live-data-tail-start
                (with-current-buffer data-buf
                  (add-hook 'before-change-functions change-hook nil t)
                  (copy-marker source nil))))
        (setq niwa-view--live-view-tail-start
              (copy-marker view-start nil))))))

(defun niwa-view--split-live-response-tail (segments data-buf)
  "Split SEGMENTS at the last stable response block in DATA-BUF."
  (let ((reversed (reverse segments)) done out)
    (dolist (seg reversed out)
      (if (or done (not (eq (car seg) 'response)))
          (push seg out)
        (setq done t)
        (let ((boundary (niwa-view--last-live-response-boundary
                         data-buf (cadr seg) (caddr seg))))
          (if boundary
              (progn
                (push (list 'response boundary (caddr seg)) out)
                (push (list 'response (cadr seg) boundary) out))
            (push seg out)))))))

(defun niwa-view--rebase-data-sources (delta)
  "Shift rendered data-buffer source coordinates by DELTA."
  (unless (zerop delta)
    (niwa-view-disclosure-rebase-state delta)
    ;; Property values are ordinary Lisp objects.  Mutating each shared
    ;; value once updates its coordinates without modifying the visible
    ;; buffer and forcing a frame-wide redisplay.
    (let ((seen-sources (make-hash-table :test #'eq))
          (seen-keys (make-hash-table :test #'eq))
          (limit (point-max))
          (pos (point-min)))
      (while (< pos limit)
        (let* ((source (get-text-property pos 'niwa-view-source))
               (next (or (next-single-property-change
                          pos 'niwa-view-source nil limit)
                         limit)))
          (when (and (consp source)
                     (not (gethash source seen-sources)))
            (puthash source t seen-sources)
            (when (integerp (car source))
              (setcar source (+ (car source) delta)))
            (when (integerp (cdr source))
              (setcdr source (+ (cdr source) delta))))
          (setq pos next)))
      (setq pos (point-min))
      (while (< pos limit)
        (let* ((key (get-text-property pos 'niwa-view-source-key))
               (next (or (next-single-property-change
                          pos 'niwa-view-source-key nil limit)
                         limit)))
          (when (and (consp key)
                     (not (gethash key seen-keys)))
            (puthash key t seen-keys)
            (when (and (eq (car key) 'source)
                       (integerp (nth 2 key)))
              (setcar (nthcdr 2 key) (+ (nth 2 key) delta))))
          (setq pos next))))
    (when (hash-table-p niwa-view--tool-rendering-cache)
      (clrhash niwa-view--tool-rendering-cache))))


(defun niwa-view--debug-buffer ()
  "Return the view-render debug buffer, creating it when needed."
  (get-buffer-create niwa-view-render-debug-buffer-name))

(defun niwa-view-render-debug-enable (&optional clear)
  "Enable view-render debug tracing.
With prefix argument CLEAR, erase the trace buffer first."
  (interactive "P")
  (setq niwa-view-render-debug t)
  (when clear
    (niwa-view-render-debug-clear))
  (message "niwa view render trace enabled (buffer: %s)"
           niwa-view-render-debug-buffer-name))

(defun niwa-view-render-debug-disable ()
  "Disable view-render debug tracing."
  (interactive)
  (setq niwa-view-render-debug nil)
  (message "niwa view render trace disabled"))

(defun niwa-view-render-debug-clear ()
  "Erase the view-render debug trace buffer."
  (interactive)
  (with-current-buffer (niwa-view--debug-buffer)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun niwa-view-render-debug-open ()
  "Open the view-render debug trace buffer."
  (interactive)
  (pop-to-buffer (niwa-view--debug-buffer)))

(defun niwa-view--debug-marker-position (marker)
  "Return MARKER's position, or nil when MARKER is not live."
  (and (markerp marker)
       (marker-buffer marker)
       (marker-position marker)))

(defun niwa-view--debug-region (start end)
  "Return a compact plist describing START..END in the current buffer."
  (when (and start end (<= start end))
    (let* ((len (- end start))
           (limit 180)
           (preview-end (min end (+ start limit)))
           (preview
            (when (<= start preview-end)
              (buffer-substring-no-properties start preview-end))))
      (list :start start
            :end end
            :len len
            :preview
            (when preview
              (replace-regexp-in-string
               "\n" "\\\\n"
               (if (> len limit)
                   (concat preview "...")
                 preview)
               t t))))))

(defun niwa-view--debug-fragment-position (position)
  "Return managed-fragment coordinates for POSITION, or nil."
  (when (and (integer-or-marker-p position)
             (< position (point-max)))
    (when-let* ((bounds (niwa-view-zone-bounds-at position)))
      (list :namespace (plist-get bounds :namespace)
            :id (plist-get bounds :id)
            :section (get-text-property
                      position 'niwa-view-zone-section)
            :offset (- position (plist-get bounds :start))))))

(defun niwa-view--debug-state (&optional data-buf start end)
  "Return a plist describing the current view-render state.
DATA-BUF, START, and END describe the data-buffer range being rendered."
  (when niwa-view-render-debug
    (let* ((input (niwa-view--debug-marker-position
                   (niwa-view-transcript-tail-marker)))
           (window (and (eq (window-buffer (selected-window))
                            (current-buffer))
			(selected-window)))
           (window-point (and window (window-point window)))
           (window-start (and window (window-start window)))
           (status (niwa-view--debug-marker-position
                    niwa-view--status-marker))
           (interaction (niwa-view--debug-marker-position
			 niwa-view--interaction-marker))
           (in-flight (niwa-view--debug-marker-position
                       niwa-view--in-flight-turn-start))
           (data-start (niwa-view--debug-marker-position
			niwa-view--data-turn-start))
           (tail-end (or status input))
           (live-tail (and in-flight tail-end
                           (<= in-flight tail-end)
                           (niwa-view--debug-region in-flight tail-end))))
      (list :view (buffer-name)
            :point (point)
            :point-fragment (niwa-view--debug-fragment-position (point))
            :window-point window-point
            :window-point-fragment
            (niwa-view--debug-fragment-position window-point)
            :window-start window-start
            :window-start-fragment
            (niwa-view--debug-fragment-position window-start)
            :point-max (point-max)
            :input input
            :status status
            :interaction interaction
            :in-flight in-flight
            :data-turn-start data-start
            :pending niwa-view--pending-tool-calls
            :live-tail live-tail
            :data-buffer (and (buffer-live-p data-buf)
                              (buffer-name data-buf))
            :data-start start
            :data-end end
            :data-point-max (and (buffer-live-p data-buf)
				 (with-current-buffer data-buf (point-max)))))))

(defun niwa-view--debug-log (event &rest data)
  "Log EVENT and DATA when `niwa-view-render-debug' is enabled."
  (when niwa-view-render-debug
    (condition-case err
        (let ((print-length 80)
              (print-level 8))
          (with-current-buffer (niwa-view--debug-buffer)
            (goto-char (point-max))
            (insert
             (format "[%s] %-24s %S\n"
                     (format-time-string "%H:%M:%S.%3N")
                     event
                     data))))
      (error
       (niwa-diagnostic-message
        (niwa-diagnostic-scope niwa--data-buffer)
        "%s (View render trace failed)" (error-message-string err))))))

(defun niwa-view--debug-turn-summary (turns data-buf)
  "Return compact debug metadata for DATA-BUF.
TURNS is the list of rendered turn plists."
  (when niwa-view-render-debug
    (mapcar
     (lambda (turn)
       (let* ((start (plist-get turn :start))
              (end (plist-get turn :end))
              (text
               (and (buffer-live-p data-buf)
                    start end
                    (with-current-buffer data-buf
                      (buffer-substring-no-properties
                       start (min end (+ start 120)))))))
         (list :role (plist-get turn :role)
               :start start
               :end end
               :preview (and text
                             (replace-regexp-in-string "\n" "\\\\n"
                                                       text t t)))))
     turns)))


;;
;;; Display properties and fontification


(defun niwa-view--default-display-keymap (vtype)
  "Return the default display-region keymap for VTYPE."
  (if (eq vtype 'agent-handle)
      niwa-view--agent-handle-map
    niwa-view--display-map))

(defun niwa-view-render-add-display-properties
    (start end &optional default-vtype)
  "Mark START..END read-only and attach default display keymaps.
Existing local `keymap' properties, such as transcript attribution
buttons, are preserved.  DEFAULT-VTYPE is used when a character has
no `niwa-view-type' property yet.  Invisibility is rear-nonsticky so
decorating a collapsed card cannot hide its visible terminator from point."
  (add-text-properties start end
                       '(read-only t
                         front-sticky (read-only keymap)
                         rear-nonsticky
                         (read-only keymap line-prefix wrap-prefix invisible)))
  (let ((pos start))
    (while (< pos end)
      (let* ((keymap-next
              (or (next-single-property-change pos 'keymap nil end) end))
             (type-next
              (or (next-single-property-change
                   pos 'niwa-view-type nil end)
                  end))
             (next (min keymap-next type-next)))
        (unless (get-text-property pos 'keymap)
          (put-text-property
           pos next 'keymap
           (niwa-view--default-display-keymap
            (or (get-text-property pos 'niwa-view-type)
                default-vtype))))
        (setq pos next)))))


(defun niwa-view--set-hide-markdown-markup (symbol value)
  "Set SYMBOL to VALUE and re-render every view that already drew markup.
The `invisible\=' property is applied while fontifying rather than at
display time, so the reusable Markdown buffer and every cached rendering
predate the new setting and would otherwise keep the old look until the
session restarted."
  (let ((changed (not (eq (and (boundp symbol) (symbol-value symbol))
                          value))))
    (set-default symbol value)
    (when (and changed (fboundp 'niwa-view--release-markdown-fontify-buffer))
      (niwa-view--release-markdown-fontify-buffer)
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (derived-mode-p 'niwa-view-mode)
            (when (hash-table-p niwa-view--response-fontify-cache)
              (clrhash niwa-view--response-fontify-cache)
              (setq-local niwa-view--response-cache-entries 0))
            (when (hash-table-p niwa-view--tool-rendering-cache)
              (clrhash niwa-view--tool-rendering-cache))
            (when (buffer-live-p niwa--data-buffer)
              (niwa-view--full-rerender))))))))

(defun niwa-view--markdown-grammar-hint ()
  "Warn once that Markdown view text stays unfontified, and how to fix it.
Without this the failure is silent: responses render as raw Markdown with
no indication that a missing grammar is the reason.  The hint is a user
affordance, so batch Emacs -- where nobody can act on it -- stays quiet."
  (when (and (not noninteractive)
             niwa-view-fontify-responses
             (not (niwa-view--markdown-grammars-ready-p)))
    (niwa--warn-once
     'global
     'view-render-markdown-grammar
     "Markdown responses render unfontified: the `markdown' and \
`markdown-inline' tree-sitter grammars are not installed. \
Run `M-x markdown-ts-mode-install-parsers' to build them")))

(defun niwa-view--visible-response-text (text)
  "Return response TEXT with model protocol hidden when appropriate."
  (let ((text (niwa-view--strip-render-data-display-text text)))
    (when (niwa-view--strip-proposed-plans-p text)
      (setq text (niwa-plan-strip-proposed text)))
    (replace-regexp-in-string "^</?proposed_plan>[ \t]*\n?" "" text)))

(defun niwa-view--render-cache-key (text)
  "Return a compact cache key for TEXT content."
  ;; `sxhash-equal' samples long strings: equal-length metadata edits can
  ;; collide even with matching length, prefix, and suffix checks.
  (secure-hash 'sha1 text))

(defun niwa-view--source-position (pos)
  "Return POS as an integer so markers and integers key identically.
The targeted agent refresh passes markers where the full render passes
integers; a marker never `equal's its position, so the same segment
would otherwise occupy two cache entries."
  (if (markerp pos) (marker-position pos) pos))

(defun niwa-view--cache-put (table key value counter-symbol)
  "Put VALUE in TABLE under KEY and bump COUNTER-SYMBOL.
Clear TABLE before adding beyond `niwa-view-render-cache-max-entries'."
  (unless (gethash key table)
    (when (>= (symbol-value counter-symbol)
              niwa-view-render-cache-max-entries)
      (clrhash table)
      (set counter-symbol 0))
    (set counter-symbol (1+ (symbol-value counter-symbol))))
  (puthash key value table)
  value)

(defun niwa-view--fontify-response (text)
  "Return TEXT with view-safe Markdown face properties.
Returns normalized TEXT without faces when
`niwa-view-fontify-responses' is nil or no Markdown mode is available.
MODE is resolved only to key the cache; the fontification itself goes
through the tag so it reuses `niwa-view--markdown-fontify-target'.
Faces are stored as `font-lock-face' so they survive the view
buffer's font-lock refontification cycles."
  (let* ((start-time (float-time))
         (text (niwa-view--visible-response-text text))
         (mode (niwa-view--markdown-fontify-mode))
         (cache (and (hash-table-p niwa-view--response-fontify-cache)
                     niwa-view--response-fontify-cache))
         (key (and cache
                   (list :response
                         niwa-view-fontify-responses
                         mode
                         (niwa-view--render-cache-key text))))
         (cached (and key (or (and niwa-view-render--prepared-responses
                                   (gethash key niwa-view-render--prepared-responses))
                              (gethash key cache)))))
    (prog1
        (or cached
            (let ((rendered
                   (if (and niwa-view-fontify-responses mode)
                       (condition-case err
                           ;; The tag, not MODE: only the tag routes to the
                           ;; reusable buffer, and responses are the hot path
                           ;; the reuse exists for.
                           (niwa-view--fontify-as text 'markdown-mode)
                         (error
                          (niwa--warn-once
                           (niwa-diagnostic-scope niwa--data-buffer)
                           'view-render-fontify
                           "%s (Could not fontify response as Markdown)"
                           (error-message-string err))
                          text))
                     text)))
              (if key
                  (niwa-view--cache-put cache key rendered
                                           'niwa-view--response-cache-entries)
                rendered)))
      (niwa-view--debug-log
       'fontify-response
       :chars (length text)
       :cached (and cached t)
       :elapsed (- (float-time) start-time)))))


;;
;;; Rendering state and agent helpers

(defun niwa-view--running-agent-transcript-buffer-p ()
  "Return non-nil when the current buffer is a live agent transcript."
  (let ((inv niwa--agent-invocation))
    (and (niwa-agent-invocation-p inv)
         (eq (niwa-agent-invocation-transcript-status inv)
             'running))))

(defun niwa-view--group-into-turns (segments data-buf &optional state next)
  "Group SEGMENTS by conversation role.
A turn is a list of consecutive segments belonging to one role.
A new user segment starts a new turn.  Returns a list of turns,
where each turn is a plist (:role ROLE :segments SEGS :start S :end E).
ROLE is `user' or `assistant'.

DATA-BUF is the authoritative transcript buffer.

A segment classified as `user' (gptel property nil) starts a new turn
when it follows another `user' or `response' segment, when non-scaffolding
text follows a closed `reasoning' or `tool' segment, or when it contains a
retained `* Agent Task:' heading.  When it follows an `ignore' segment it is
reasoning text embedded in the assistant turn and is absorbed as such.

Additionally, a nil segment immediately after a `response' is
absorbed into the assistant turn when the next segment is `ignore'
or `tool' (mid-turn reasoning gap between response chunks), but only
when DATA-BUF shows that the segment is org scaffolding rather than a
real user message.

With NEXT, continue grouping later: NEXT is the following segment used for
lookahead at the end of SEGMENTS.  Return the resumable STATE rather than
the completed turns.  Pass that state into the next call.  A final call
without NEXT returns the complete turns."
  (let ((turns (plist-get state :turns))
        (current-segs (plist-get state :current-segs))
        (current-role (plist-get state :current-role))
        (turn-start (plist-get state :turn-start))
        (prev-type (plist-get state :prev-type))
        (rest segments))
    (while rest
      (let* ((seg (car rest))
             (type (car seg))
             (seg-start (cadr seg))
             (seg-end (caddr seg))
             (next-type (car-safe (or (cadr rest) next)))
             ;; One substring and one render-data extraction per segment
             ;; feed every classification below.  The predicate wrappers
             ;; used to extract the same span three to five times per
             ;; segment on every streaming frame, which dominated live
             ;; render allocation.  Properties are kept: the side-channel
             ;; extractors authorize blocks through text properties.
             (seg-text
              (and data-buf
                   (memq type '(user render-data ignored))
                   (with-current-buffer data-buf
                     (save-restriction
                       (widen)
                       (let* ((pmin (point-min))
                              (pmax (point-max))
                              (s (max pmin (min seg-start pmax)))
                              (e (max pmin (min seg-end pmax))))
                         (if (< s e) (buffer-substring s e) ""))))))
             (seg-render-data
              (and seg-text
                   (cdr (niwa-tool-render-data-extract seg-text))))
             (seg-render-kind
              (and (consp seg-render-data)
                   (plist-get seg-render-data :kind)))
             (hook-audit-only-p
              (and (eq type 'ignored)
                   seg-text
                   (niwa-transcript-audit-only-p seg-text)))
             (seg-scaffolding-only-p
              (and (memq type '(user ignored))
                   seg-text
                   ;; Audit-only segments already preserve the preceding
                   ;; type below.  Their scaffolding verdict is never used;
                   ;; do not clean the encoded payload to compute it.
                   (not hook-audit-only-p)
                   (niwa-view--scaffolding-only-text-p seg-text)))
             (scaffolding-before-hook-audit-p
              (and (eq type 'user)
                   seg-scaffolding-only-p
                     (let ((following (or (cadr rest) next)))
                       (and (eq (car-safe following) 'ignored)
                          (niwa-view--hook-audit-only-segment-p
                           data-buf (cadr following) (caddr following))))))
             (review-action-p
              (and (eq type 'user)
                   data-buf
                   (niwa-view--review-action-segment-p
                    data-buf seg-start seg-end)))
             (agent-task-p
              (and (eq type 'user)
                   data-buf
                   (with-current-buffer data-buf
                     (save-excursion
                       (goto-char seg-start)
                       (let ((case-fold-search nil))
                         (re-search-forward
                          "^\\* Agent Task:" seg-end t))))))
             (system-reminder-p (eq type 'reminder))
             (inline-skill-render-p
              (eq seg-render-kind 'inline-skill))
             (request-summary-p
              (eq seg-render-kind 'request-summary))
             (render-data-only-p
              (and seg-text
                   (not (and (memq type '(render-data ignored))
                             (eq seg-render-kind 'collaboration-event)
                             (eq (plist-get seg-render-data :event)
                                 'started)))
                   (niwa-view--render-data-only-text-p seg-text)))
             ;; Assistant-side glue with no turn before it has nothing to
             ;; join and nothing to draw.  Rewinding to before the first
             ;; turn leaves exactly this: leading whitespace still
             ;; carrying tool or audit properties.  An in-flight turn
             ;; always follows a user turn, so it is never dropped here.
             (leading-assistant-residue-p
              (and (null current-role)
                   (null turns)
                   (memq type '(ignored tool reasoning))
                   data-buf
                   (niwa-view--blank-segment-p
                    data-buf seg-start seg-end))))
        (cond
         (review-action-p
          nil)
         (system-reminder-p
          (unless current-role
            (setq current-role 'assistant
                  turn-start seg-start))
          (push (list 'system-reminder seg-start (caddr seg)) current-segs))
         (request-summary-p
          (unless current-role
            (setq current-role 'assistant
                  turn-start seg-start))
          (push (list 'request-summary seg-start (caddr seg)) current-segs))
         (scaffolding-before-hook-audit-p
          nil)
         ((and (null current-role)
               (eq (plist-get (car turns) :role) 'user)
               (or (memq type '(prompt hook-context))
                   hook-audit-only-p
                   (and (eq type 'render-data)
                        (eq seg-render-kind 'user-display)
                        (stringp (plist-get seg-render-data :text)))
                   inline-skill-render-p))
          (let ((turn (copy-sequence (car turns))))
            (setq turn
                  (plist-put turn :segments
                             (append (plist-get turn :segments)
                                     (list seg))))
            (setq turn (plist-put turn :end (caddr seg)))
            (setq turns (cons turn (cdr turns)))))
         (render-data-only-p
          nil)
         (leading-assistant-residue-p
          nil)
         ((eq type 'task-background)
          (when current-segs
            (push (list :role current-role
                        :segments (reverse current-segs)
                        :start turn-start
                        :end (caddr (car current-segs)))
                  turns))
          (push (list :role 'task-background
                      :segments (list seg)
                      :start seg-start
                      :end (caddr seg))
                turns)
          (setq current-segs nil current-role nil turn-start nil))
         ((and (eq type 'user)
               (or agent-task-p
                   (memq prev-type '(nil user response))
                   (and (memq prev-type '(reasoning tool))
                        data-buf
                        (not seg-scaffolding-only-p)))
               ;; Look-ahead: a scaffolding-only nil gap right after a
               ;; response is assistant-side glue.  Require DATA-BUF proof
               ;; so a real user prompt remains a user turn.
               (not (and (eq prev-type 'response)
                         (or (and (memq next-type
                                       '(reasoning ignored tool mailbox
                                         reminder render-data))
                                  seg-scaffolding-only-p)
                             (and (eq next-type 'response)
                                  data-buf
                                  seg-scaffolding-only-p)))))
          ;; Genuine user turn: either the first segment, follows a
          ;; user/response segment, or follows reasoning or tool activity.
          (progn
            ;; Flush any accumulated assistant turn
            (when current-segs
              (push (list :role current-role
                          :segments (reverse current-segs)
                          :start turn-start
                          :end (caddr (car current-segs)))
                    turns))
            ;; Start a new user turn (single segment)
            (push (list :role 'user
                        :segments (list seg)
                        :start seg-start
                        :end (caddr seg))
                  turns)
            (setq current-segs nil current-role nil turn-start nil)))
         (t
          ;; Assistant-side segment (response, tool, ignore, pure
          ;; mailbox delivery, or reasoning text misclassified as user).
          (unless current-role
            (setq current-role 'assistant
                  turn-start seg-start))
          (push seg current-segs)))
        (setq prev-type
              (cond
               (system-reminder-p prev-type)
               (request-summary-p 'response)
               (hook-audit-only-p prev-type)
               (render-data-only-p prev-type)
               (leading-assistant-residue-p prev-type)
               ((and (eq type 'ignored) seg-scaffolding-only-p)
                prev-type)
               (scaffolding-before-hook-audit-p prev-type)
               (t type)))
        (setq rest (cdr rest))))
    (if next
        (list :turns turns :current-segs current-segs
              :current-role current-role :turn-start turn-start
              :prev-type prev-type)
      ;; Only the final slice closes an assistant turn.  Earlier slices keep
      ;; the segments reversed so adding to the current turn remains cheap.
      (when current-segs
        (push (list :role current-role
                    :segments (reverse current-segs)
                    :start turn-start
                    :end (caddr (car current-segs)))
              turns))
      (reverse turns))))


;;
;;; Tool one-liner generation

(defun niwa-view--tool-one-liner (data-buf seg-start seg-end)
  "Generate a one-line summary for a tool segment.
Reads the tool content from DATA-BUF between SEG-START and SEG-END,
parses the S-expression to extract tool name, and builds a summary.

Skips a leading `#+begin_tool …' / `#+end_reasoning' / blank-line
preamble before parsing so a segment whose start drifted into the
org-block scaffolding (incremental render boundary expansion or a
patched render-data block can shift the gptel-property run) still
produces a `Bash: …' / `Read: …' header instead of bare `Tool'."
  (with-current-buffer data-buf
    (let* ((raw (niwa-view--tool-segment-text seg-start seg-end))
           (wrapped-p (niwa-view--tool-wrapped-text-p raw))
           (text (substring raw (niwa-view--tool-readable-start raw))))
      (condition-case nil
          (let* ((sexp (read text))
                 (name (plist-get sexp :name))
                 (args (plist-get sexp :args))
                 ;; Count result lines (text after the sexp)
                 (sexp-end (with-temp-buffer
                             (insert text)
                             (goto-char (point-min))
                             (forward-sexp 1)
                             (point)))
                 (result-text (string-trim (substring text sexp-end)))
                 (result-text
                  (if wrapped-p
                      (niwa-view--strip-trailing-tool-marker
                       result-text)
                    result-text))
                 (result-text
                  (niwa--strip-hook-audit-blocks result-text))
                 (result-lines (length (split-string result-text "\n" t)))
                 (primary-arg (niwa-tool-display-string
                               name args (niwa-diagnostic-scope niwa--data-buffer)))
                 (blocked (niwa-view--tool-hook-blocked-info
                           result-text))
                 (error-p (niwa-view--tool-result-error-p result-text))
                 (summary
                  (niwa-view--tool-summary-line
                   name primary-arg result-lines blocked error-p)))
            summary)
        (error
         ;; Fallback: show truncated raw text
         (niwa-view--tool-fallback-line raw))))))

(defun niwa-view--tool-hook-blocked-info (result-text)
  "Return hook blocking info parsed from RESULT-TEXT, or nil."
  (when (and (stringp result-text)
             (string-match
              "\\(?:Error:[ \t]*\\)?\\(?:Permission denied:[ \t]*\\)?blocked by \\(PreToolUse\\|PermissionRequest\\):[ \t]*\\(.+\\)"
              result-text))
    (list :event (match-string 1 result-text)
          :reason (string-trim (match-string 2 result-text)))))

(defun niwa-view--read-args-media-p (args)
  "Return non-nil when Read ARGS identify a media-capable file."
  (when-let* ((path (plist-get args :file_path))
              ((stringp path))
              (ext (downcase (or (file-name-extension path) ""))))
    (member ext '("pdf" "png" "jpg" "jpeg" "gif" "webp"))))


;;
;;; Thinking block summary

(defun niwa-view--reasoning-source-bounds (data-buf start end)
  "Return reasoning block bounds inside DATA-BUF START..END.
When a restored transcript leaves assistant-side text unpropertized,
the thinking group can start before the structural reasoning block.
Prefer the explicit `#+begin_reasoning' marker so expanding a thinking
summary does not include preceding agent-result or tool output text."
  (with-current-buffer data-buf
    (save-excursion
      (save-restriction
        (widen)
        (let* ((pmin (point-min))
               (pmax (point-max))
               (s (max pmin (min start pmax)))
               (e (max pmin (min end pmax)))
               block-start block-end)
          (when (< s e)
            (goto-char s)
            (when (re-search-forward "^#\\+begin_reasoning\\b[^\n]*\n?"
                                     e t)
              (setq block-start (match-beginning 0))
              (setq block-end
                    (if (re-search-forward "^#\\+end_reasoning[^\n]*\n?"
                                           e t)
                        (match-end 0)
                      e))
              (cons block-start block-end))))))))

(defun niwa-view--strip-tool-blocks-from-reasoning (text)
  "Return reasoning TEXT with nested org tool blocks removed."
  (let ((text (or text ""))
        (case-fold-search (default-value 'case-fold-search)))
    (if (not (string-match-p "^#\\+begin_tool\\b" text))
        text
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (re-search-forward "^#\\+begin_tool\\b" nil t)
          (let ((start (match-beginning 0)))
            (if (re-search-forward "^#\\+end_tool[^\n]*\n?" nil t)
                (delete-region start (point))
              (delete-region start (point-max)))))
        (buffer-string)))))

(defconst niwa-view--clean-reasoning-cache-size 24
  "Reasoning-text cleanings retained by `niwa-view--clean-reasoning-cache'.

Sized for the completed reasoning blocks a transcript re-renders, not for
history: each entry holds a whole cleaned block, and a miss costs only the
work this cache exists to skip.")

(defvar niwa-view--clean-reasoning-cache nil
  "Cleaned reasoning text, keyed by text after trusted audit removal.

Global for the same reason as `niwa-view--scaffolding-only-cache\': the
cleaning depends on nothing but its argument.

Audit removal runs before lookup because trust depends on text properties,
which string equality ignores.

It earns its place on redraw.  A render walks every turn in the transcript,
so each tick re-cleans every completed reasoning block as well as the live
one -- four passes over each, allocating a fresh copy per pass.  Completed
blocks do not change, so they hit.  A streaming block has no end marker and
is not inserted: each growing version would miss and retain an obsolete
raw/cleaned pair.")

(defun niwa-view--clean-reasoning-text (text)
  "Strip org scaffolding markers from reasoning TEXT.
Removes trusted audit records, reasoning block markers, nested tool blocks,
and generated system reminder wrappers.  TEXT must retain audit properties.

Completed blocks are cached globally; see
`niwa-view--clean-reasoning-cache'."
  (setq text (niwa--strip-hook-audit-blocks text))
  (or (cdr (assoc text niwa-view--clean-reasoning-cache))
      (let ((cleaned (niwa-view--clean-reasoning-text-1 text)))
        (when (string-match-p "^#\\+end_reasoning\\b" text)
          (setq niwa-view--clean-reasoning-cache
                (cons (cons text cleaned)
                      (let ((kept
                             (seq-take
                              niwa-view--clean-reasoning-cache
                              (1- niwa-view--clean-reasoning-cache-size))))
                        (assoc-delete-all text kept)))))
        cleaned)))

(defun niwa-view--clean-reasoning-text-1 (text)
  "Return TEXT with reasoning scaffolding removed, without consulting a cache."
  (let ((cleaned (niwa-view--strip-render-data-display-text text)))
    (setq cleaned (niwa-view--strip-system-reminder-blocks cleaned))
    (setq cleaned (niwa-view--strip-tool-blocks-from-reasoning cleaned))
    ;; One pass over the marker lines rather than three.  Each
    ;; `replace-regexp-in-string' copies the whole string, and reasoning
    ;; text runs to thousands of lines while every redraw re-cleans it.
    (let ((markers "#\\+\\(?:begin\\|end\\)_\\(?:reasoning\\|tool\\)[^\n]*\n?"))
      (if (string-match-p markers cleaned)
          (replace-regexp-in-string markers "" cleaned)
        cleaned))))

(defun niwa-view--strip-render-data-display-text (text)
  "Return TEXT without hidden render-data side-channel scaffolding."
  (niwa-tool-render-data-strip (or text "")))

(defun niwa-view--render-data-only-text-p (text)
  "Return non-nil if TEXT is only render-data scaffolding."
  (and (stringp text)
       ;; Without a block the text itself is what remains; skip the copies.
       (string-search niwa-tool-render-data-open text)
       (string-match-p "[^ \t\n\r]" text)
       (not (string-match-p
             "[^ \t\n\r]"
             (niwa-view--strip-render-data-display-text text)))))

(defun niwa-view--hook-audit-only-segment-p (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END is only hook audit data."
  (with-current-buffer data-buf
    (niwa-transcript-audit-buffer-only-p seg-start seg-end)))

(defun niwa-view--system-reminder-body-from-text (text)
  "Return generated system reminder body from TEXT, or nil.
TEXT must contain only one complete `<system-reminder>' block plus
surrounding whitespace.  Embedded literal examples are not treated
as generated control markup."
  (when (stringp text)
    (with-temp-buffer
      (insert (string-trim text))
      (goto-char (point-min))
      (when-let* ((range
                   (niwa-transcript--system-reminder-range-at-point
                    (point-max)))
                  ((= (nth 3 range) (point-max))))
        (string-trim
         (buffer-substring-no-properties (nth 1 range) (nth 2 range)))))))

(defun niwa-view--strip-system-reminder-blocks (text)
  "Return TEXT without generated `<system-reminder>' blocks."
  (let ((text (or text ""))
        (case-fold-search (default-value 'case-fold-search)))
    (if (not (string-match-p "<system-reminder>" text))
        text
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (search-forward "<system-reminder>" nil t)
          (let ((start (match-beginning 0)))
            (if (search-forward "</system-reminder>" nil t)
                (progn
                  (delete-region start (point))
                  (goto-char start))
              (goto-char (point-max)))))
        (buffer-string)))))

(defun niwa-view--system-reminder-line-count (body)
  "Return the non-empty line count for system reminder BODY."
  (length (split-string (or body "") "\n" t "[ \t]+")))

(defun niwa-view--inline-skill-attachments-summary (text)
  "Return the attached-skill row header for inline-skill TEXT, or nil."
  (when-let* ((data (niwa-view--inline-skill-render-data-from-text text))
              (attachments (plist-get data :attachments)))
    (niwa-view--attached-skills-summary
     (mapcar (lambda (attachment) (plist-get attachment :name)) attachments)
     (length attachments))))

(defun niwa-view--system-reminder-summary (data-buf seg-start seg-end)
  "Return collapsed summary for DATA-BUF's SEG-START..SEG-END system reminder."
  (with-current-buffer data-buf
    ;; The inline-skill probe needs the render-data text properties, so
    ;; the properties are stripped only for the generic reminder path.
    (let ((text (buffer-substring seg-start seg-end)))
      (or (niwa-view--inline-skill-attachments-summary text)
          (let* ((body (niwa-view--system-reminder-body-from-text
                        (substring-no-properties text)))
                 (lines (max 1 (niwa-view--system-reminder-line-count
                                body))))
            (propertize
             (format "  \u25c7 System reminder (%d %s)"
                     lines
                     (if (= lines 1) "line" "lines"))
             'font-lock-face 'niwa-view-system-reminder))))))

(defun niwa-view--attached-skills-summary (names count)
  "Return the collapsed header for COUNT attached-skill reminders.
NAMES labels the attached skills when the record carries them."
  (propertize
   (format "  \u25c7 %d attached skill%s%s"
           count
           (if (= 1 count) "" "s")
           (if names
               (format " (%s)" (mapconcat #'identity names ", "))
             ""))
   'font-lock-face 'niwa-view-system-reminder))

(defun niwa-view--scaffolding-only-p (data-buf seg-start seg-end)
  "Return non-nil if DATA-BUF region [SEG-START, SEG-END] is org-only glue.
A segment is org-only when it contains nothing but `#+begin_…' /
`#+end_…' marker lines, blank lines, and whitespace.  Used by the
assistant-turn renderer to drop glue segments between adjacent
`ignore'/`tool' segments so they don't surface as fake `Thinking…
\(1 lines)' entries.

Source-of-truth: the gptel text-property scheme alone leaves
markers and blank lines unpropertised, so the segment extractor
classifies them as `user'.  Without this filter, the assistant
turn shows one bogus thinking summary per tool boundary."
  (with-current-buffer data-buf
    (save-restriction
      (widen)
      (let* ((pmin (point-min))
             (pmax (point-max))
             (s (max pmin (min seg-start pmax)))
             (e (max pmin (min seg-end pmax))))
        (unless (niwa-view--leading-content-p s e)
          (niwa-view--scaffolding-only-text-p
           (and (< s e) (buffer-substring-no-properties s e))))))))

(defconst niwa-view--leading-content-regexp
  "\\`\\(?:[ \t\r\n]*#\\+\\(?:begin\\|end\\)_reasoning\\b[^\n]*\\(?:\n\\|\\'\\)\\)*[ \t\r\n]*[^ \t\r\n#<]"
  "Match text that visibly begins with content.
Whitespace and reasoning marker lines may precede it; any other character
there is outside every block that reasoning cleaning removes, since each
removed construct begins with `#' or `<'.  Most segments are content, and
proving that by cleaning a copy of each on every redraw dominated its
allocation.")

(defun niwa-view--leading-content-p (start end)
  "Return non-nil when the current buffer's START..END begins with content.
See `niwa-view--leading-content-regexp'."
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (looking-at-p (substring niwa-view--leading-content-regexp 2)))))

(defun niwa-view--blank-segment-p (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END holds only whitespace.

Unlike `niwa-view--scaffolding-only-p', this judges the text as it
stands rather than as glue: a tool or reasoning block with a body is
never blank, however much of it is marker lines."
  (with-current-buffer data-buf
    (save-restriction
      (widen)
      (let* ((pmin (point-min))
             (pmax (point-max))
             (s (max pmin (min seg-start pmax)))
             (e (max pmin (min seg-end pmax))))
        (or (>= s e)
            (string-blank-p (buffer-substring-no-properties s e)))))))

(defconst niwa-view--scaffolding-only-cache-size 512
  "Scaffolding verdicts retained by `niwa-view--scaffolding-only-cache'.

Only true verdicts are retained.  Growing live reasoning and ordinary user
text would otherwise keep whole false-result strings globally reachable.")

(defvar niwa-view--scaffolding-only-cache (make-hash-table :test #'equal)
  "Scaffolding verdicts, keyed by the segment text they were derived from.

Global, not per buffer.  The verdict is a pure function of the text, so a
segment answers the same in whichever buffer asks -- and the render walks
more than one, so a buffer-local cache was refilled from empty rather than
reused.  That is why an earlier per-buffer version did not reduce this at
all.

A render walks every segment and asks each one whether it is glue, and the
answer came from cleaning the whole segment and testing the result for
emptiness -- four passes allocating a fresh copy each.  That was 81% of all
reasoning-cleaning allocation in a profiled remote turn, and it recurs on
every redraw because the segments do not change.

Only glue is retained.  Real content includes the growing live reasoning
segment, so caching false verdicts would preserve up to 512 progressively
larger copies merely to avoid recomputing nil.")

(defun niwa-view--scaffolding-only-text-p (text)
  "Return non-nil if TEXT is org-only glue, or nil.
Nil TEXT counts as glue.  See `niwa-view--scaffolding-only-p'.

Cached globally; see `niwa-view--scaffolding-only-cache'."
  (or (null text)
      (gethash text niwa-view--scaffolding-only-cache)
      (when (and (not (string-match-p
                       niwa-view--leading-content-regexp text))
                 (string-empty-p
             (string-trim (niwa-view--clean-reasoning-text-1 text))))
        ;; Cleared rather than evicted one by one: recomputing a verdict is
        ;; the work this cache already exists to skip, and a transcript long
        ;; enough to overflow re-examines its oldest segments least.
        (when (>= (hash-table-count niwa-view--scaffolding-only-cache)
                  niwa-view--scaffolding-only-cache-size)
          (clrhash niwa-view--scaffolding-only-cache))
        (puthash text t niwa-view--scaffolding-only-cache))))

(defun niwa-view--inline-skill-render-data-from-text (text)
  "Return inline-skill render-data from TEXT, or nil."
  (let ((data (cdr (niwa-tool-render-data-extract text))))
    (and (consp data)
         (eq (plist-get data :kind) 'inline-skill)
         data)))

(defun niwa-view--collaboration-event-from-text (text)
  "Return a canonical started collaboration event from TEXT, or nil."
  (let ((data (cdr (niwa-tool-render-data-extract text))))
    (and (consp data)
         (eq (plist-get data :kind) 'collaboration-event)
         (eq (plist-get data :event) 'started)
         data)))

(defun niwa-view--request-summary-render-data-from-text (text)
  "Return request-summary render-data from TEXT, or nil."
  (let ((data (cdr (niwa-tool-render-data-extract text))))
    (and (consp data)
         (eq (plist-get data :kind) 'request-summary)
         data)))

(defun niwa-view--collaboration-event-segment-p
    (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's span carries a started collaboration event."
  (with-current-buffer data-buf
    (niwa-view--collaboration-event-from-text
     (buffer-substring seg-start seg-end))))

(defun niwa-view--delete-request-summaries (data-buf start end)
  "Delete request-summary render-data blocks in DATA-BUF START..END.
Return the last deleted summary, so settlement can preserve its timing."
  (with-current-buffer data-buf
    (save-excursion
      (let ((case-fold-search nil)
            summary
            (limit (copy-marker (or end (point-max)) t)))
        (goto-char (or start (point-min)))
        (unwind-protect
            (while (search-forward niwa-tool-render-data-open limit t)
              (when-let* ((open (match-beginning 0))
                          (block (niwa-tool-render-data-block-at
                                  open (marker-position limit))))
                (goto-char (cadr block))
                (when (and (eq (plist-get (caddr block) :kind) 'request-summary)
                           (not (plist-member (caddr block) :niwa-tool-use-id))
                           (niwa-tool-render-data-trusted-range-p
                            open (cadr block)))
                  (setq summary (caddr block))
                  (delete-region (car block) (cadr block))
                  (goto-char (car block)))))
          (set-marker limit nil))
        summary))))

(defun niwa-view--request-summary-elapsed-seconds (data-buf &optional now)
  "Return active seconds for DATA-BUF's current request at NOW, or nil."
  (when-let* (((buffer-live-p data-buf))
              (request
               (buffer-local-value 'niwa--current-request data-buf)))
    (niwa-request-active-elapsed-seconds request now)))

(defun niwa-view--append-request-summary
    (data-buf search-start &optional extra)
  "Append hidden request-summary render-data to DATA-BUF if needed.
SEARCH-START bounds duplicate detection to the current response tail.
EXTRA is additional request metadata to persist.
Return the new data-buffer end position."
  (when-let* ((now (current-time))
              (elapsed (niwa-view--request-summary-elapsed-seconds
                        data-buf now)))
    (with-current-buffer data-buf
      (let* ((tail-start (or search-start (point-min)))
             (started (float-time
                       (niwa-request-started-at niwa--current-request)))
             (previous (niwa-view--delete-request-summaries
                        data-buf tail-start (point-max)))
             (summary
              (if (equal started (plist-get previous :started-at))
                  (copy-sequence previous)
                (list :kind 'request-summary :elapsed-seconds elapsed
                      :started-at started :finished-at (float-time now)))))
        (while extra
          (setq summary (plist-put summary (pop extra) (pop extra))))
        (save-excursion
          (goto-char (point-max))
          (unless (bolp) (insert "\n"))
          (insert
           (niwa-tool-render-data-format summary)))))
    (with-current-buffer data-buf
      (point-max))))

(defun niwa-view--review-action-segment-p (data-buf seg-start seg-end)
  "Return non-nil when DATA-BUF's SEG-START..SEG-END is only review action."
  (with-current-buffer data-buf
    (let ((text (buffer-substring-no-properties seg-start seg-end)))
      (and (string-search "<user_action>" text)
           (string-search "<action>review</action>" text)
           (string-empty-p
            (string-trim
             (niwa-review-strip-user-action-blocks text)))))))

(defun niwa-view--thinking-summary (data-buf seg-start seg-end)
  "Generate a summary for a thinking/reasoning block.
Reads content from DATA-BUF between SEG-START and SEG-END.
Returns empty string when the block is trivial (only whitespace
or org scaffolding markers)."
  (let* ((bounds (niwa-view--reasoning-source-bounds
                  data-buf seg-start seg-end))
         (seg-start (or (car-safe bounds) seg-start))
         (seg-end (or (cdr-safe bounds) seg-end)))
    (with-current-buffer data-buf
      (let* ((text (buffer-substring seg-start seg-end))
             (cleaned (niwa-view--clean-reasoning-text text))
             (count (niwa-view--nonblank-line-count cleaned)))
        (if (> count 0)
            (concat
             "  "
             (propertize niwa-view--thinking-glyph
                         'font-lock-face 'niwa-view-thinking-marker)
             (propertize (format "Thinking... (%d lines)" count)
                         'font-lock-face 'niwa-view-thinking-summary))
          "")))))

(defun niwa-view--nonblank-line-count (text)
  "Return the number of non-blank lines in TEXT.

Counts in place.  The summary needs only the number, and building the
list of lines to take its length allocated one string per line of a
reasoning block that reaches thousands of lines -- on every redraw."
  (let ((count 0) (start 0) (length (length text)))
    (while (< start length)
      (let* ((newline (string-search "\n" text start))
             (end (or newline length))
             (blank t)
             (index start))
        (while (and blank (< index end))
          (unless (memq (aref text index) '(?\s ?\t))
            (setq blank nil))
          (setq index (1+ index)))
        (unless blank (setq count (1+ count)))
        (setq start (if newline (1+ newline) length))))
    count))


;;
;;; Rendering

(defun niwa-view--strip-proposed-plans-p (text)
  "Return non-nil when TEXT's proposed-plan protocol blocks should be hidden.
Active planning hides incomplete streamed blocks.  Complete protocol blocks
stay hidden on later full rerenders without session-global hash history."
  (or (and niwa--session
           (niwa-session-plan-mode niwa--session))
      (niwa-plan-extract-proposed text)))

(defun niwa-view--current-render-insertion-marker ()
  "Return the marker render helpers should insert at."
  (or (and (markerp niwa-view--render-insertion-marker)
           (marker-position niwa-view--render-insertion-marker)
           niwa-view--render-insertion-marker)
      (niwa-view-transcript-tail-marker)))

(defun niwa-view--history-insertion-marker ()
  "Return the boundary where transcript/live-tail text should be inserted.

The history region ends at `niwa-view--status-marker'.  Status and
interaction UI live below that boundary and above the input prompt."
  (unless (and (markerp niwa-view--status-marker)
               (eq (marker-buffer niwa-view--status-marker)
                   (current-buffer))
               (marker-position niwa-view--status-marker))
    (error "View status marker is not live"))
  niwa-view--status-marker)

(defun niwa-view--after-header-position ()
  "Return the first history position after the session header.
The earlier-chats line belongs to the header chrome."
  (save-excursion
    (goto-char (point-min))
    ;; Agent transcript chrome lives in the header line, not in the
    ;; transcript.  Its first text line is already history.
    (unless niwa-view--agent-transcript-p
      (forward-line 1)
      (when (get-text-property (point) 'niwa-view-chats-line)
        (goto-char (or (next-single-property-change
                        (point) 'niwa-view-chats-line)
                       (point-max)))))
    (point)))

(defun niwa-view--transcript-history-position-p (pos)
  "Return non-nil when POS belongs to rendered transcript history."
  (or (get-text-property pos 'niwa-view-source)
      (get-text-property pos 'niwa-view-pending-tool-live)
      (memq (get-text-property pos 'niwa-view-type)
            '(activity-separator
              hook-context
              mailbox-delivery
              prompt-summary
              request-summary
              response
              system-reminder-summary
              task-background
              thinking-summary
              tool-event
              tool-summary
              turn-header
              turn-summary
              user))))

(defun niwa-view--non-history-view-position-p (pos)
  "Return non-nil when POS belongs to a non-history view row."
  (and (get-text-property pos 'niwa-view-type)
       (not (niwa-view--transcript-history-position-p pos))))

(defun niwa-view--history-tail-position ()
  "Return the best-effort end of rendered transcript history.

When boundary markers are detached, recover the history/status boundary
from the last rendered transcript character.  If no
transcript has been rendered yet, return the position after the header."
  (let* ((after-header (niwa-view--after-header-position))
         (pos (point-max)))
    (while (and (> pos after-header)
                (not
                 (niwa-view--transcript-history-position-p (1- pos))))
      (setq pos (or (previous-property-change pos nil after-header)
                    after-header)))
    (if (and (> pos after-header)
             (niwa-view--transcript-history-position-p (1- pos)))
        pos
      after-header)))

;;;; Turn assembly
;;
;; Live-region rendering, mutation and settling, and the assembly of user,
;; assistant, activity and tool-group rows into a turn.

;;;;; In-flight tool calls
;;
;; A tool call the model has started but whose result has not reached the
;; data buffer has no source to project.  The live render still shows it,
;; in the grammar of the row it will become: it joins the turn's trailing
;; activity run, counts toward that run's grouping, and marks the row as
;; running.  When its result lands, the next render draws the same row
;; settled, so finishing changes the marker, not the layout.  Pending
;; decorations carry `niwa-view-pending-tool-live'; nothing else depends
;; on them, and no disclosure state is keyed by them.

(defvar niwa-view--render-pending nil
  "Cell (ENTRIES) of in-flight tool calls the live render still has to draw.
Bound only while the last in-flight turn renders.  The turn's trailing
activity run takes ENTRIES with `niwa-view--take-render-pending';
otherwise the live render appends them after the turn.")

(defun niwa-view--take-render-pending ()
  "Return and claim the in-flight tool entries of the current live render."
  (when niwa-view--render-pending
    (prog1 (car niwa-view--render-pending)
      (setcar niwa-view--render-pending nil))))

(defun niwa-view--pending-marker (id)
  "Return the leading marker area of a running row animated under ID.
Glyph spinner styles animate the marker itself.  Shimmer animates the
row's label instead, and static draws nothing moving, so both keep the
running dot.  Before insertion the display frame is unknown; glyphs use a
portable first frame."
  (if (memq niwa-view-tool-spinner-style '(shimmer static))
      (propertize "  ● " 'font-lock-face 'niwa-view-agent-running)
    (let ((frame (niwa-view-animation-frame
                  niwa-view-tool-spinner-style "" 0 'niwa-view-ephemeral
                  :multiple)))
      (concat (propertize "  " 'font-lock-face 'niwa-view-agent-running)
              (propertize frame
                          'font-lock-face 'niwa-view-ephemeral
                          'niwa-view-inline-spinner-frame t
                          'niwa-view-tool-animation-id id
                          'display frame)))))

(defun niwa-view--pending-label (text id)
  "Return TEXT, shimmering under animation ID when shimmer is the style.
The sweep uses TEXT's own face, so the label keeps the color it has
once its call settles."
  (if (eq niwa-view-tool-spinner-style 'shimmer)
      (let ((face (or (get-text-property 0 'font-lock-face text)
                      'niwa-view-ephemeral)))
        (propertize text
                    'niwa-view-inline-spinner-frame t
                    'niwa-view-tool-animation-id id
                    'niwa-view-tool-animation-face face
                    'display (niwa-view-animation-frame
                              'shimmer (substring-no-properties text) 0 face
                              (selected-frame))))
    text))

(defun niwa-view--running-header (line count &optional virtual)
  "Return group header LINE marked running with COUNT calls in flight.
The settled marker gives way to the running one, and `· N running'
follows the summary, whose failure counts stay beside their tools.  With
no call left
running, a failure takes the `×' marker.  Unless VIRTUAL, the replaced
marker is kept on the new one so a stopped turn can restore it in place."
  (let ((marker (if count
                    (niwa-view--pending-marker 'pending-run)
                  (propertize "  × " 'font-lock-face 'error)))
        (sep (propertize " · " 'font-lock-face 'niwa-view-tool-metadata)))
    (concat
     (propertize marker
                 'niwa-view-pending-tool-live t
                 'niwa-view-settled-marker
                 (and (not virtual) (substring line 0 4)))
     (substring line 4)
     (when count
       (propertize
        (concat sep
                (niwa-view--pending-label
                 (propertize (format "%d running" count)
                             'font-lock-face 'niwa-view-tool-metadata)
                 'pending-run))
        'niwa-view-pending-tool-live t)))))

(defun niwa-view--pending-children (entries)
  "Return activity group children for in-flight tool ENTRIES."
  (mapcar (lambda (entry)
            (list :kind 'pending :tool (nth 1 entry) :entry entry))
          entries))

(defun niwa-view--pending-entry-settled-p (entry)
  "Return non-nil when in-flight ENTRY finished before its result landed.
gptel writes a parallel batch's results only after the whole batch, so a
call the pipeline reports as finished keeps its place in the run until
then, shown finished instead of running."
  (plist-get (nth 3 entry) :settled))

(defun niwa-view--pending-running-count (entries)
  "Return how many in-flight ENTRIES are still running, or nil for none."
  (let ((count (cl-count-if-not #'niwa-view--pending-entry-settled-p
                                entries)))
    (and (> count 0) count)))

(defun niwa-view--pending-failed-count (entries)
  "Return how many in-flight ENTRIES finished failed, or nil for none."
  (let ((count (cl-count-if
                (lambda (entry)
                  (memq (niwa-view--pending-entry-settled-p entry)
                        '(error failed denied blocked)))
                entries)))
    (and (> count 0) count)))

(defun niwa-view--pending-activity-entry (entry)
  "Return the activity-group entry for in-flight tool ENTRY.
It runs until it settles; one that finished before its result landed takes
its outcome, so its count does not change when the row does."
  (let ((settled (niwa-view--pending-entry-settled-p entry)))
    (list :role 'member :kind 'tool :name (or (nth 1 entry) "Tool")
          :status (cond ((memq settled '(error failed denied blocked)) 'failed)
                        (settled 'completed)
                        (t 'running))
          :pending entry)))

(defun niwa-view--insert-pending-tool-row (entry)
  "Insert the row for in-flight tool ENTRY.
ENTRY is (KEY NAME DETAIL METADATA).  The row uses the settled tool row's
`Tool: argument' grammar; shimmer animates the tool name.  A call that
finished before its result landed takes the settled marker and is still."
  (let ((key (car entry))
        (detail (nth 2 entry))
        (settled (niwa-view--pending-entry-settled-p entry))
        (start (point)))
    (insert (cond
             ((memq settled '(error failed denied blocked))
              (propertize "  × " 'font-lock-face 'error))
             (settled
              (propertize "  ✓ " 'font-lock-face 'niwa-view-tool-marker))
             (t (niwa-view--pending-marker key)))
            (let ((name (propertize (or (nth 1 entry) "Tool")
                                    'font-lock-face 'niwa-view-tool-name)))
              (if settled name (niwa-view--pending-label name key)))
            (if (and detail (not (string-empty-p detail)))
                (concat ": " (propertize detail 'font-lock-face
                                         'niwa-view-tool-argument))
              ""))
    (when-let* ((hint (niwa-view--permission-approval-hint
                      (niwa-view--permission-approval-reason (nth 3 entry)))))
      (insert "  " hint))
    (insert "\n")
    (add-text-properties start (point)
                         `(niwa-view-type tool-pending
                           niwa-view-pending-key ,key
                           niwa-view-pending-tool-use-id
                           ,(plist-get (nth 3 entry) :tool-use-id)
                           niwa-view-pending-tool-live t))
    (niwa-view--apply-rendering-indent start (point))))

(defun niwa-view--insert-pending-run (entries)
  "Insert in-flight tool ENTRIES that no settled activity run absorbed.
Above the grouping threshold they form one running group row, as they
will once settled; otherwise each call gets its own running row.  When a
reader is on one of their rows, the group is drawn open beneath its
header, so that row stays on screen."
  (niwa-view--ensure-blank-line-after-response)
  (let ((group (car (niwa-activity-group-partition
                     (mapcar #'niwa-view--pending-activity-entry entries)
                     niwa-view-tool-group-collapse-threshold))))
    (if (plist-get group :group)
	(let* ((start (point))
               (data-buf (niwa-view-segments-display-buffer))
               (data-end (and (buffer-live-p data-buf)
                              (with-current-buffer data-buf (point-max))))
               (slot (and data-end
                          (eq (car niwa-view-render--pending-reader-group)
                              niwa-view--data-turn-start)
                          (eql (cadr niwa-view-render--pending-reader-group)
                               data-end)))
               ;; A fold the user chose beats the reader's row.
               (open (if slot
                         (not (nth 2 niwa-view-render--pending-reader-group))
                       (niwa-view-render--reader-on-member-p nil entries))))
          (when (and open data-end (not slot))
            (setq niwa-view-render--pending-reader-group
                  (list niwa-view--data-turn-start data-end nil)))
          (niwa-view--insert-summary-region
           (niwa-view--rendering-header-line
            (list :header (plist-get group :summary)
                  :vtype 'tool-group
                  :pending-count (niwa-view--pending-running-count entries)
                  :pending-failed (niwa-view--pending-failed-count entries)
                  :disclosure-open open
                  :pending-virtual t))
           `(niwa-view-type tool-pending
             niwa-view-collapsed ,(not open)
             niwa-view-pending-group-end ,data-end
             niwa-view-pending-tool-live t))
          (niwa-view--apply-rendering-indent start (point))
          (when open
            (mapc #'niwa-view--insert-pending-tool-row entries)))
      (mapc #'niwa-view--insert-pending-tool-row entries))))

(defun niwa-view--retire-pending-decorations (start end)
  "Remove in-flight tool decorations between START and END.
Virtual rows disappear; a running group regains its settled marker and
loses its running count.  Return non-nil when anything changed."
  (let ((inhibit-read-only t)
        (end (copy-marker end))
        changed)
    (save-excursion
      (let ((pos start))
        (while (and (< pos end)
                    (setq pos (text-property-any
                               pos end 'niwa-view-pending-tool-live t)))
          (let* ((settled (get-text-property pos 'niwa-view-settled-marker))
                 (stop (min (or (next-single-property-change
                                 pos 'niwa-view-pending-tool-live nil end)
                                end)
                            (or (next-single-property-change
                                 pos 'niwa-view-settled-marker nil end)
                                end))))
            (setq changed t)
            (if (not settled)
                (delete-region pos stop)
              ;; Keep the row's section identity, not the running look.
              (let ((section
                     (cl-loop for (key value) on (text-properties-at pos)
                              by #'cddr
                              unless (memq key '(font-lock-face
                                                 face display
                                                 niwa-view-pending-tool-live
                                                 niwa-view-settled-marker
                                                 niwa-view-inline-spinner-frame
                                                 niwa-view-tool-animation-id))
                              append (list key value))))
                (delete-region pos stop)
                (goto-char pos)
                (insert settled)
                (add-text-properties pos (point) section)
                (setq pos (point))))))))
    (set-marker end nil)
    changed))

(defun niwa-view--strip-pending-decorations-from-string (text)
  "Return TEXT without in-flight tool decorations, or nil when empty."
  (when text
    (with-temp-buffer
      (insert text)
      (let ((inhibit-modification-hooks t))
        (niwa-view--retire-pending-decorations (point-min) (point-max)))
      (let ((str (buffer-string)))
        (unless (string-empty-p str)
          str)))))

(defmacro niwa-view-render--with-boundaries-advancing (&rest body)
  "Execute BODY while view zone boundary markers advance."
  (declare (indent 0) (debug t))
  `(niwa-view--call-with-render-boundaries-advancing
    (lambda () ,@body)))

(defun niwa-view--recover-in-flight-turn-start
    (data-from history-start history-end)
  "Recover an in-flight turn start between HISTORY-START and HISTORY-END.
DATA-FROM is the first data-buffer position for the in-flight turn."
  (when (and data-from history-start history-end (< history-start history-end))
    ;; Step by property runs: the view holds the whole transcript, and a
    ;; per-character walk cost a third of every streamed update.
    (let ((pos history-start)
          first-source)
      (while (and (< pos history-end) (not first-source))
        ;; A source whose marker no longer points anywhere, such as one into
        ;; a killed archive, cannot start the in-flight turn.
        (let ((start (niwa-view-disclosure-source-start
                      (get-text-property pos 'niwa-view-source))))
          (if (and start (>= start data-from))
              (setq first-source pos)
            (setq pos (next-single-property-change
                       pos 'niwa-view-source nil history-end)))))
      (when first-source
        (let ((scan first-source)
              header)
          (while (and (> scan history-start) (not header))
            (setq scan (previous-single-property-change
                        scan 'niwa-view-type nil history-start))
            (when (eq (get-text-property scan 'niwa-view-type)
                      'turn-header)
              (setq header scan)))
          (or header first-source))))))

(defun niwa-view--pre-rendered-user-visible-p ()
  "Return non-nil when the current in-flight marker follows a user block.
This detects whether the send-path echo inserted by
`niwa-view--insert-user-message' is still present.  A full rerender
can wipe that ephemeral block while an in-flight marker remains live, so
the marker alone is not enough to decide whether a leading user turn
from the data buffer should be filtered."
  (when-let* ((pos (niwa-view-stream-in-flight-turn-start-position))
              ((> pos (point-min))))
    (save-excursion
      (goto-char pos)
      (skip-chars-backward " \t\n")
      (and (> (point) (point-min))
           (memq (get-text-property (1- (point)) 'niwa-view-type)
                 '(user user-input-summary))))))

(defun niwa-view--render-live-region (data-buf settle-p &optional start end fold)
  "Render the in-flight turn from DATA-BUF, settling it when SETTLE-P.

Call from the view buffer.  Deletes the region between variable
`niwa-view--in-flight-turn-start' and the history boundary (the
current rendering of the in-flight assistant turn) and re-renders from
the data buffer range \[`niwa-view--data-turn-start',
end-of-data-buffer], grouping segments into turns and rendering them at
the history boundary.

Unless SETTLE-P, calls in `niwa-view--pending-tool-calls' join the
last turn's trailing activity run as running calls, or follow the turn
when it ends in something they cannot join, so the user sees what is
running before results land in the data buffer.  They are drawn by this
render only, and change the marker, not the layout, once settled.

Assistant streaming remains data-buffer-derived full/incremental turn
rerendering here: transcript parsing, collapse recovery, and final
response reconciliation still need the authoritative data buffer.

Optional START / END are used by the post-response path to decide
whether the caller already has explicit segment coordinates.  When
supplied, they are preferred over the marker-based range so the
final `--render-response' invocation still gets gptel's authoritative
response bounds.  An explicit START also redraws past the retained tail.

FOLD, a (KEY . COLLAPSED) disclosure state, replaces the state the old
projection still shows for KEY, so the redraw shows the requested fold.

User turns inside the extracted range are filtered only when the
current user input is still visible immediately before the in-flight
assistant marker.  The user's input is echoed by
`niwa-view--insert-user-message' at send time, but a full rerender
can remove that ephemeral echo before the final response render runs.
In that case the user turn must be rendered from the data buffer.

Section-level collapse state (expanded thinking block, collapsed
tool summary, …) is captured before the delete and re-applied after
the render so user toggles survive streaming ticks."
  (let* ((niwa-transcript--tool-block-index (make-hash-table :test #'eq))
         (niwa-view--path-link-memo (make-hash-table :test #'equal))
         (niwa-transcript-audit--decode-cache (make-hash-table :test #'equal))
         (retained-p (and (not settle-p)
                          (not start)
                          (niwa-view--live-tail-continues-p data-buf)))
         (turn-from (and (markerp niwa-view--data-turn-start)
                         (marker-position niwa-view--data-turn-start)))
         (data-from
          (if retained-p
              (marker-position niwa-view--live-data-tail-start)
            (cond
             ((and start turn-from) (min start turn-from))
             (start)
             (turn-from))))
         (data-to
          (or end
              (with-current-buffer data-buf (point-max))))
         (transcript-start niwa-view--transcript-start)
         (segments (when (and data-from data-to)
                     (with-current-buffer data-buf
                       (save-restriction
                         ;; Segmentation may expand to the preceding property
                         ;; boundary.  In a side conversation that run can
                         ;; include inherited reminders, even when DATA-FROM
                         ;; already follows the newly submitted prompt.
                         (when (and (markerp transcript-start)
                                    (eq (marker-buffer transcript-start) data-buf))
                           (narrow-to-region
                            (max (point-min) (marker-position transcript-start))
                            (point-max)))
                         (when retained-p
                           (narrow-to-region data-from data-to))
                         (niwa-transcript-segments data-from data-to)))))
         (segments (if settle-p
                       segments
                     (niwa-view--split-live-response-tail
                      segments data-buf)))
         (turns (niwa-view--group-into-turns segments data-buf))
         (in-flight-p
          (if retained-p
              (marker-position niwa-view--live-view-tail-start)
            (niwa-view-stream-in-flight-turn-start-position)))
         (pre-rendered-user-visible-p
          (niwa-view--pre-rendered-user-visible-p))
         (history-tail (niwa-view--history-tail-position))
         ;; The send echo already anchors a fresh response at the history
         ;; boundary.  Recovery is for a response that may already be drawn.
         (fresh-tail-p (and (not settle-p)
                            in-flight-p (>= in-flight-p history-tail)
                            pre-rendered-user-visible-p))
         (pending niwa-view--pending-tool-calls)
         ;; Known before the write, so the reader anchors below can be
         ;; scoped to the text this render may replace.
         (history-start (niwa-view--after-header-position))
         ;; Reject markers that pass `markerp' but are detached
         ;; (`marker-position' returns nil): they would crash
         ;; `<=' / `delete-region' / `apply-collapse-states' below.
         (status-pos (and (markerp niwa-view--status-marker)
                          (marker-position niwa-view--status-marker)))
         (status-valid-p
          (and status-pos
               (>= status-pos history-tail)
               (not (niwa-view--non-history-view-position-p
                     status-pos))
               (not (and (> status-pos history-start)
                         (niwa-view--non-history-view-position-p
                          (1- status-pos))))))
         (rebuild-end
          (or (and status-valid-p niwa-view--status-marker)
              (copy-marker history-tail t)
              (niwa-view-transcript-tail-marker)))
         (rebuild-end-pos (marker-position rebuild-end))
         ;; A retained tail already knows where to delete from.
         (recovered-start
          (unless (or fresh-tail-p (and retained-p in-flight-p)
                      ;; A qualifying source here proves the first one (and
                      ;; its header) cannot follow the valid delete marker.
                      (and settle-p data-from in-flight-p
                           (<= history-start in-flight-p)
                           (< in-flight-p rebuild-end-pos)
                           (when-let* ((source (niwa-view-disclosure-source-start
                                                (get-text-property in-flight-p 'niwa-view-source))))
                             (>= source data-from))))
            (niwa-view--recover-in-flight-turn-start
             data-from history-start rebuild-end-pos))))
    (niwa-view--debug-log
     'incremental-extract
     :state (niwa-view--debug-state data-buf data-from data-to)
     :start start
     :end end
     :data-from data-from
     :data-to data-to
     :segments (length segments)
     :turns (mapcar (lambda (turn) (plist-get turn :role)) turns)
     :turn-detail (niwa-view--debug-turn-summary turns data-buf)
     :pre-rendered-user niwa-view--user-pre-rendered
     :pre-rendered-user-visible pre-rendered-user-visible-p)
    ;; Filter the send-path user turn.  `niwa-transcript-segments' expands a
    ;; start position back to the containing `gptel' property run, so a
    ;; data-turn marker sitting at the end of the prompt can still yield
    ;; a leading user turn whose source starts before DATA-FROM.  That is
    ;; the prompt already echoed by the send path, not new mailbox/user
    ;; content that arrived later in the turn.
    (while (and turns
                (eq (plist-get (car turns) :role) 'user)
                (or niwa-view--user-pre-rendered
                    pre-rendered-user-visible-p
                    (and data-from
                         (< (or (plist-get (car turns) :start)
                                data-from)
                            data-from))))
      (setq turns (cdr turns)))
    (setq niwa-view--user-pre-rendered nil)
    (niwa-view--debug-log
     'incremental-filtered
     :turns (mapcar (lambda (turn) (plist-get turn :role)) turns)
     :turn-detail (niwa-view--debug-turn-summary turns data-buf)
     :pending pending
     :state (niwa-view--debug-state data-buf data-from data-to))
    (niwa-view-call-preserving-user-view-state
      (lambda ()
        ;; rebuild region stops at status-marker (top of zone
        ;; 2) rather than the prompt boundary, so any future status- or
        ;; interaction-zone overlay anchors survive the re-render.
        ;; both boundaries coincide when the zones are empty, so this
        ;; is a no-op for current behavior; setting it correctly now
        ;; prevents a phase-8 regression when zone overlays land.
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t)
              ;; Running rows are redrawn below; keep their animation phase.
              ;; A full projection may already have removed them.
              (preserved-tools
               (when pending
                 (append (niwa-view--snapshot-tool-animation-targets)
                         (prog1 niwa-view--carried-tool-samples
                           (setq niwa-view--carried-tool-samples nil)))))
              (animated (or pending niwa-view--spinner-tool-targets)))
          ;; With nothing to redraw, calls that finished leave no row that a
          ;; replacement would remove.
          (unless (or turns pending)
            (niwa-view--retire-pending-tool-rows))
          (let* (;; Permission prompts and tool callbacks can trigger a view
                 ;; refresh before gptel has written any result segments.  In
                 ;; that case keep the previous in-flight rendering instead of
                 ;; replacing it with a blank region.
                 (replace-p (or turns pending))
                 (delete-start
                  (or (and retained-p in-flight-p)
                      (and in-flight-p
                           (>= in-flight-p history-start)
                           (<= in-flight-p rebuild-end-pos)
                           (not (and recovered-start
                                     (< in-flight-p recovered-start)
                                     (niwa-view--transcript-history-position-p
                                      in-flight-p)))
                           in-flight-p)
                      recovered-start))
                 (capture-p
                  (and delete-start
                       rebuild-end
                       (<= delete-start rebuild-end-pos)))
                 (saved-states
                  (let ((states (when (and replace-p capture-p)
                                  (niwa-view-disclosure-capture-state
                                   delete-start
                                   rebuild-end-pos))))
                    ;; Capture remembered the old fold; the request wins.
                    (when fold
                      (niwa-view-disclosure-record-state-for-key
                       (car fold) (cdr fold))
                      (push fold states))
                    states))
                 (render-start nil))
            (niwa-view--debug-log
             'incremental-decision
           :replace-p replace-p
           :capture-p capture-p
           :rebuild-end (niwa-view--debug-marker-position rebuild-end)
           :saved-states (length saved-states)
           :state (niwa-view--debug-state data-buf data-from data-to))
          ;; Wipe the current in-flight assistant turn render (if any)
          ;; so we can re-render it from scratch from the updated data.
          (when (and replace-p capture-p)
            (niwa-view--debug-log
             'incremental-delete
             :region (niwa-view--debug-region
                      delete-start
                      rebuild-end-pos)
             :state (niwa-view--debug-state data-buf data-from data-to))
            (delete-region delete-start rebuild-end)
            (niwa-view--debug-log
             'incremental-after-delete
             :state (niwa-view--debug-state data-buf data-from data-to)))
          (when replace-p
            ;; The in-flight turn belongs to the history region.  Insert it
            ;; at the status boundary so any real-text status/interaction
            ;; UI below that boundary remains below the transcript.
            (let ((niwa-view--render-insertion-marker rebuild-end)
                  (continuation-p retained-p)
                  (in-flight (and pending (not settle-p) (list pending))))
              (setq render-start (marker-position rebuild-end))
              (let ((rest turns))
                (while rest
                  ;; In-flight calls belong to the last turn's trailing run.
                  (let ((niwa-view--render-pending
                         (and (null (cdr rest)) in-flight)))
                    (niwa-view--render-turn
                     (car rest) data-buf nil nil
                     (prog1 continuation-p
                       (setq continuation-p nil))))
                  (setq rest (cdr rest))))
              ;; Calls that no trailing run absorbed follow the turn.
              (when-let* ((left (car in-flight)))
                (save-excursion
                  (goto-char rebuild-end)
                  (niwa-view-render--with-boundaries-advancing
                    ;; A response that opens with tool calls has no turn in
                    ;; the transcript yet.  Draw its heading now, so the row
                    ;; does not move when the first result brings it.  It is
                    ;; part of the running decoration: a turn stopped before
                    ;; any result removes it with the rows.
                    (let ((interim (niwa-view--live-turn-needs-header-p
                                    (min (or (niwa-view-stream-in-flight-turn-start-position)
                                             render-start)
                                         render-start)
                                    (point) data-buf))
                          (rows-start (point)))
                      (when interim
                        (niwa-view--insert-assistant-header
                         nil '(niwa-view-pending-tool-live t)))
                      (niwa-view--insert-pending-run left)
                      ;; The interim turn ends like the turn it stands for,
                      ;; so the rows below stay put when results land.
                      (when interim
                        (insert (propertize "\n" 'niwa-view-pending-tool-live t)
                                (propertize "\n"
                                            'font-lock-face 'niwa-view-turn-rule
                                            'niwa-view-pending-tool-live t)))
                      ;; Rows outside a turn miss its read-only pass.
                      (niwa-view-render-add-display-properties
                       rows-start (point) 'tool-pending)))))
              (niwa-view--ensure-request-progress data-buf))
            (niwa-view--debug-log
             'incremental-after-insert
             :state (niwa-view--debug-state data-buf data-from data-to)))
          ;; Restore user-toggled collapse/expand state that the delete
          ;; above just wiped.  Walk the freshly rendered span and toggle
          ;; only sections whose saved state differs from the default.
          ;; Record the complete final unit before a toggle can split its
          ;; source property.  Earlier disclosures may change length without
          ;; changing this unit; rescanning after restore can instead pick
          ;; only the last fragment of a rewritten unit.
          (when replace-p
            (if settle-p
                (niwa-view-render--invalidate-live-tail)
              (niwa-view--retain-last-live-render-unit
               data-buf render-start (marker-position rebuild-end))))
          (let ((tail-first-end
                 (when niwa-view--live-view-tail-start
                   (copy-marker (1+ niwa-view--live-view-tail-start) nil)))
                (tail-end
                 (when niwa-view--live-view-tail-start
                   (copy-marker
                    (next-single-property-change
                     niwa-view--live-view-tail-start
                     'niwa-view-live-unit-source nil
                     (marker-position rebuild-end))
                    t)))
                restored completed)
            (when niwa-view--live-view-tail-start
              ;; Follow insertions before the unit.  FIRST-END distinguishes
              ;; those from a replacement that consumes its first character.
              (set-marker-insertion-type niwa-view--live-view-tail-start t))
            (unwind-protect
                (progn
                  (setq restored
                        (when (and saved-states delete-start
                                   (marker-position rebuild-end))
                          (niwa-view-disclosure-restore-state
                           delete-start (marker-position rebuild-end)
                           saved-states)))
                  (setq completed t))
              (when (or (not completed)
                        (and restored
                             (not (and tail-end
                                       (niwa-view--live-tail-intact-p
                                        data-buf
                                        (marker-position tail-first-end)
                                        (marker-position tail-end))))))
                (niwa-view-render--invalidate-live-tail))
              (when niwa-view--live-view-tail-start
                (set-marker-insertion-type
                 niwa-view--live-view-tail-start nil))
              (when tail-first-end (set-marker tail-first-end nil))
              (when tail-end (set-marker tail-end nil))))
          (when (and animated replace-p (marker-position rebuild-end))
            (niwa-view--adopt-tool-animation-targets
             (or render-start (point-min)) (marker-position rebuild-end)
             preserved-tools))
          (niwa-view--ensure-request-progress data-buf)
          (unless niwa-view--agent-transcript-p
            (niwa-view--render-agent-status)
            (niwa-view--interaction-rebuild)
            (when (fboundp 'niwa-note-frame-refresh)
              (niwa-note-frame-refresh))))))
      ;; A render rewrites only the in-flight turn and the zones below it;
      ;; anchor readers there without counting old history.
      (let ((turn-start (niwa-view-stream-in-flight-turn-start-position)))
        (cond
         ((and (or fresh-tail-p retained-p) in-flight-p)
          (cons (min (if fresh-tail-p history-tail in-flight-p)
                     (or turn-start in-flight-p))
                (point-max)))
         ;; Without a turn start, retiring running rows starts at the top.
         (turn-start
          (cons (apply #'min history-tail turn-start
                       (delq nil (list in-flight-p recovered-start)))
                (point-max)))))
      t)))

(defvar-local niwa-view-render--owner nil
  "Non-nil while this view has an active projection writer.")

(defvar-local niwa-view-render--pending nil
  "Coalesced (KEY GENERATION SOURCE FUNCTION CLEANUP) projection intentions.")

(defvar-local niwa-view-render--generation 0
  "Identity of the current source/turn mutation generation.")

(defvar-local niwa-view-render--terminal-p nil
  "Non-nil once terminal presentation has been requested for this turn.")

(defvar-local niwa-view-render--terminal-turn nil
  "Data-turn marker belonging to the latest terminal presentation intent.")

(defvar-local niwa-view-render--mouse-resume-hook nil
  "Temporary global command hook draining writes after mouse release.")

(defvar-local niwa-view-render--mouse-resume-timer nil
  "One-shot deadline preventing a lost mouse release from freezing the view.")

(defvar-local niwa-view-render--expired-mouse-gesture nil
  "Mouse drag identity whose hold already reached its deadline.")

(defun niwa-view-render--mouse-gesture ()
  "Return the identity of the current mouse tracking gesture.
Built-in drag tracking spans commands, while macro-based tracking keeps
the initiating command event throughout its nested input loop."
  (or (if (eq track-mouse 'drag-tracking)
          (terminal-parameter nil 'mouse-drag-start)
        last-command-event)
      t))

(defun niwa-view-render--mouse-held-p ()
  "Return non-nil while any unexpired mouse tracking needs stable positions."
  (and (or track-mouse
           ;; A macro-based client can hand the release back to the command
           ;; loop.  Only an existing hold scans input during that handoff.
           (and niwa-view-render--mouse-resume-hook
                (niwa-view-render--queued-mouse-release-p)))
       (not (eq (niwa-view-render--mouse-gesture)
                niwa-view-render--expired-mouse-gesture))))

(defun niwa-view-render--cancel-mouse-resume ()
  "Remove this view's temporary mouse-release hook, including on teardown."
  (when (timerp niwa-view-render--mouse-resume-timer)
    (cancel-timer niwa-view-render--mouse-resume-timer))
  (setq niwa-view-render--mouse-resume-timer nil)
  (when niwa-view-render--mouse-resume-hook
    (remove-hook 'post-command-hook niwa-view-render--mouse-resume-hook)
    (setq niwa-view-render--mouse-resume-hook nil)))

(defun niwa-view-render--discard-mouse-writes ()
  "Invalidate held projections before teardown can drain them."
  (cl-incf niwa-view-render--generation)
  (setq niwa-view-render--expired-mouse-gesture (niwa-view-render--mouse-gesture))
  (when niwa-view-render--mouse-resume-hook
    (niwa-view-render--resume-after-mouse (current-buffer))))

(defun niwa-view-render--queued-mouse-release-p ()
  "Return non-nil if a tracked command handed its release back to input."
  (seq-some
   (lambda (event)
     (when (memq (car-safe event) '(t no-record))
       (setq event (cdr event)))
     (and (mouse-event-p event)
          (event-modifiers event)
          (not (memq 'down (event-modifiers event)))))
   unread-command-events))

(defun niwa-view-render--resume-after-mouse (buffer &optional expired)
  "Drain BUFFER's held writes after release, or when EXPIRED forces progress."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when expired
        (setq niwa-view-render--expired-mouse-gesture
              (niwa-view-render--mouse-gesture)))
      (unless (niwa-view-render--mouse-held-p)
        ;; Remove both wakeups before running writers, even if one signals.
        (niwa-view-render--cancel-mouse-resume)
        (niwa-view-render-mutate 'mouse-resume #'ignore)))))

(defun niwa-view-render--wait-for-mouse ()
  "Drain coalesced writes after the current mouse gesture finishes.
The hook is global only while needed: release can select another buffer.
A two-second deadline bounds the hold if release is lost outside Emacs."
  (unless niwa-view-render--mouse-resume-hook
    (let ((buffer (current-buffer)))
      (setq niwa-view-render--mouse-resume-hook
            (lambda () (niwa-view-render--resume-after-mouse buffer)))
      (setq niwa-view-render--mouse-resume-timer
            (run-at-time 2 nil #'niwa-view-render--resume-after-mouse buffer t)))
    (add-hook 'post-command-hook niwa-view-render--mouse-resume-hook)))

(defun niwa-view-render-terminal ()
  "Invalidate incremental work for the current turn, without changing text."
  (setq niwa-view-render--terminal-p t
        niwa-view-render--terminal-turn niwa-view--data-turn-start))

(defun niwa-view-render-mutate (key function &optional replacement cleanup)
  "Run FUNCTION as this view's sole projection writer.
Every projection write takes ownership.  KEY coalesces nested requests;
it never exempts writes from ownership or the mouse hold.
They carry the displayed source identity, never saved view coordinates.
REPLACEMENT invalidates queued work from the previous source or turn.
A new turn retains status and interaction redraws for the same source.
Optional CLEANUP must run even if replacement invalidates FUNCTION.
Any mouse tracking defers writes until release: queued mouse events carry
numeric buffer positions that cannot follow a length-changing projection.
Return FUNCTION's value when immediate, or t when queued.  Composed internal
operations call their owned implementations rather than entering here again."
  (when replacement
    (niwa-view-prepare-cancel)
    (cl-incf niwa-view-render--generation)
    ;; Discard obsolete projection, not its mandatory terminal release.
    ;; Releases stay ahead of the replacement so they cannot clear its state.
    (setq niwa-view-render--pending
          (cl-remove-if-not
           (lambda (entry)
             ;; Beginning a turn does not rebuild these view-owned zones.
             ;; Their thunks read current state; the source check still applies.
             (or (and (eq key 'begin-turn)
                      (memq (car entry) '(status interaction))
                      (= (nth 1 entry) (1- niwa-view-render--generation))
                      (setf (nth 1 entry) niwa-view-render--generation))
                 (nth 4 entry)))
           niwa-view-render--pending)))
  (let ((entry (list key niwa-view-render--generation
                     (niwa-view-segments-display-buffer) function cleanup)))
    (if (or niwa-view-render--owner (niwa-view-render--mouse-held-p))
        (progn
          (if-let* ((previous (assoc key niwa-view-render--pending)))
              (setcdr previous (cdr entry))
            (setq niwa-view-render--pending
                  (nconc niwa-view-render--pending (list entry))))
          (unless niwa-view-render--owner
            (niwa-view-render--wait-for-mouse))
          t)
      (setq niwa-view-render--owner t)
      ;; Fontification can yield to redisplay after a writer deletes its old
      ;; projection.  Keep intermediate text hidden through the queue drain.
      (let ((inhibit-redisplay t)
            failure result)
        (cl-labels
            ((run (work)
               (pcase-let ((`(,work-key ,generation ,source ,thunk ,release) work))
                 (unwind-protect
                     (condition-case err
                         (when (and (= generation niwa-view-render--generation)
                                    (eq source (niwa-view-segments-display-buffer)))
                           ;; Transcript writers need a complete projection.
                           ;; Chrome uses live zone markers, so a nested status
                           ;; update must not synchronously finish the batch.
                           (when (and niwa-view-render--batch
                                      (not (memq work-key
                                                 '(full-batch status interaction
                                                   background-elapsed elapsed-tick
                                                   mouse-resume))))
                             (niwa-view-render-cancel-batch)
                             (unless (eq work-key 'full)
                               (niwa-view-render--full-now)))
                           ;; Rows a reader is on when the writer deletes
                           ;; them must not fold out from under them.
                           (let ((niwa-view-render--reader-members
                                  (niwa-view-render--reader-members)))
                             (funcall thunk)))
                       ((error quit) (unless failure (setq failure err)) nil))
                   (when release
                     (condition-case err
                         (funcall release)
                       ((error quit) (unless failure (setq failure err)))))))))
          (unwind-protect
              (progn
                (unwind-protect
                    (setq result (run entry))
                  ;; A queued writer's failure must not starve the terminal
                  ;; or replacement behind it.  Report the first error only
                  ;; after every still-current intention has been drained.
                  (while niwa-view-render--pending
                    (run (pop niwa-view-render--pending)))
                  ;; A terminal turn has no further incremental update, and a
                  ;; replaced turn cannot continue its tail; drop the tail and
                  ;; the source observer with it.
                  (when (or niwa-view-render--terminal-p
                            (and (markerp niwa-view--live-data-tail-start)
                                 (not (niwa-view--live-tail-continues-p
                                       (marker-buffer
                                        niwa-view--live-data-tail-start)))))
                    (niwa-view-render--invalidate-live-tail))
                  ;; The paired closing-rule overlay starts on the last row's
                  ;; newline.  A splice outside the preserving entry, such as
                  ;; a disclosure toggle, would leave that row inside it.
                  (niwa-view-pair-refresh-turn-rule))
                (if failure (signal (car failure) (cdr failure)) result))
            (setq niwa-view-render--owner nil
                  niwa-view-render--pending nil)))))))

(defun niwa-view-render-live-update (data-buf)
  "Update the current in-flight turn from DATA-BUF's mutable tail.
Nothing happens while the view projects an archived segment: the live
turn is not on display, and drawing it would replace that segment's text."
  (unless (eq niwa-view-render--terminal-turn niwa-view--data-turn-start)
    (setq niwa-view-render--terminal-p nil))
  (unless (or niwa-view-render--terminal-p
              (niwa-view-historical-segment-p))
    (niwa-view-render-mutate
     'live
     (lambda ()
       (unless (or niwa-view-render--terminal-p
                   (niwa-view-historical-segment-p))
         (niwa-view--render-live-region data-buf nil))))))

(defconst niwa-view-render--live-folds
  '(tool-summary tool-group thinking-summary response tool-pending)
  "Folds the live render draws itself inside the in-flight turn.
Each is redrawn from the recorded fold; `tool-pending' is the header of a
running-only group, whose fold lives in
`niwa-view-render--pending-reader-group'.  Other folds there are spliced
and the turn redrawn around them.")

(defun niwa-view-render-fold-request (vtype collapsed)
  "Return the live writer's request to show the row at point COLLAPSED.
VTYPE is the row's view type.  Return nil unless the row is a
`niwa-view-render--live-folds' row of the live in-flight turn.  The
request carries the row's identity, not its position, so it can be
applied after other writes with `niwa-view-render-apply-fold'."
  (let ((data-buf (niwa-view-segments-display-buffer)))
    (when (and (eq data-buf niwa--data-buffer)
               (buffer-live-p data-buf)
               (memq vtype niwa-view-render--live-folds)
               (not (and niwa-view-render--terminal-p
                         (eq niwa-view-render--terminal-turn
                             niwa-view--data-turn-start)))
               (when-let* ((start (niwa-view-stream-in-flight-turn-start-position)))
                 (>= (point) start)))
      (if (eq vtype 'tool-pending)
          (unless (get-text-property (point) 'niwa-view-pending-key)
            ;; The data end the drawn group's first result lands at: a
            ;; result arriving before TAB or delivery must still hand over.
            (list niwa-view--data-turn-start
                  (list 'group (get-text-property
                                (point) 'niwa-view-pending-group-end))
                  collapsed))
        (when-let* (((get-text-property (point) 'niwa-view-source))
                    (key (niwa-view-disclosure--state-key-at
                          (point) (make-hash-table :test #'equal))))
          (list niwa-view--data-turn-start key collapsed
                ;; The tail marker itself: each retained render makes a
                ;; new one, so a deferred request cannot trust a later tail.
                (and (niwa-view--live-tail-continues-p data-buf)
                     (>= (point) niwa-view--live-view-tail-start)
                     niwa-view--live-view-tail-start)))))))

(defun niwa-view-render-apply-fold (request)
  "Redraw the live turn with fold REQUEST, holding projection ownership.
REQUEST comes from `niwa-view-render-fold-request'.  A request for a
replaced or settled turn is dropped rather than applied to another row."
  (pcase-let ((`(,turn ,key ,collapsed ,in-tail) request)
              (data-buf (niwa-view-segments-display-buffer)))
    (when (and (eq turn niwa-view--data-turn-start)
               (markerp turn) (marker-position turn)
               (eq data-buf niwa--data-buffer)
               (buffer-live-p data-buf)
               (not niwa-view-render--terminal-p))
      (when (eq (car-safe key) 'group)
        (setq niwa-view-render--pending-reader-group
              (list turn (cadr key) collapsed)))
      ;; Only the retained unit and what follows is redrawn incrementally.
      (unless (or (eq (car-safe key) 'group)
                  (and in-tail (eq in-tail niwa-view--live-view-tail-start)
                       (niwa-view--live-tail-continues-p data-buf)))
        (niwa-view-render--invalidate-live-tail))
      (niwa-view--render-live-region
       data-buf nil nil nil
       (if (not (eq (car-safe key) 'group))
           (cons key collapsed)
         ;; A result that settled the group first was drawn with its own
         ;; fold; that projection's state must not outrank this request.
         (let ((end (with-current-buffer data-buf (point-max))))
           (when-let* (((< (cadr key) end))
                       (group-key (niwa-view-disclosure-state-key
                                   (cons (cadr key) end) 'tool-group)))
             (cons group-key collapsed))))))))

(defun niwa-view-render-set-fold (source vtype collapsed splice)
  "Show the disclosure at point, SOURCE of VTYPE, COLLAPSED.
Call with projection ownership held.  Inside the live in-flight turn, the
live render redraws `niwa-view-render--live-folds' with the recorded fold.
SPLICE rewrites any other row at point from the data buffer."
  (if-let* ((request (and (or source (eq vtype 'tool-pending))
                          (niwa-view-render-fold-request vtype collapsed))))
      (niwa-view-render-apply-fold request)
    (let ((live (and (eq (niwa-view-segments-display-buffer) niwa--data-buffer)
                     (when-let* ((start (niwa-view-stream-in-flight-turn-start-position)))
                       (>= (point) start)))))
      (funcall splice)
      (niwa-view-render--invalidate-live-tail)
      ;; The splice drew the row from the transcript alone, without the
      ;; running calls only the live render draws.
      (when (and live niwa-view--pending-tool-calls)
        (niwa-view--render-live-region niwa--data-buffer nil)))))

(defun niwa-view-render-settle (data-buf start end)
  "Exactly reconcile DATA-BUF's completed response from START through END.
Disclosure state keys are computed with their durable post-settle
anchors: this render runs before the stream clears the in-flight turn
markers, and keys captured or stamped under the temporary `(in-flight)'
anchor here would be orphaned the moment those markers clear, collapsing
sections the user expanded during the turn on the next render."
  (niwa-view-render-terminal)
  (let ((start (with-current-buffer data-buf (copy-marker start)))
        (end (with-current-buffer data-buf (copy-marker end t))))
    (niwa-view-render-mutate
     'settle
     (lambda () (niwa-view-render--settle-now data-buf start end)))))

(defun niwa-view-render--settle-now (data-buf start end)
  "Reconcile DATA-BUF's START..END with projection ownership already held."
  (niwa-view-render--invalidate-live-tail)
  (let ((niwa-view-disclosure--settling-p t))
    (niwa-view--render-live-region data-buf t start end)))

(defun niwa-view--conversation-variant-button
    (data-buf source-start source-end &optional session)
  "Return DATA-BUF's variant switch for SOURCE-START..SOURCE-END, or nil.
SESSION supplies live session context when DATA-BUF is archived."
  (when-let* ((session
               (or session
                   (buffer-local-value 'niwa--session data-buf)))
              ((niwa-session-save-path session))
              (fork-point
               (niwa-session-artifacts-fork-point-at-source
                data-buf source-start source-end
                niwa-view--conversation-fork-points))
              (fork-point-id (plist-get fork-point :fork-point-id))
              (variants
               (progn
                 (niwa-session-persistence-conversation-variants
                  session fork-point-id
                  niwa-view--conversation-variant-sessions)))
              ((> (length variants) 1))
              (current
               (cl-find
                (niwa-session-session-id session)
                variants
                :test #'equal
                :key (lambda (entry)
                       (plist-get (plist-get entry :summary)
                                  :session-id)))))
    (let* ((origin (plist-get current :variant-origin))
           (label
            (pcase origin
              ('source "Source")
              ('conversation "Conversation")
              ('worktree "Worktree")))
           (text
            (format "[⇆ %s · %d variants]" label (length variants))))
      (propertize
       text
       'face 'link
       'mouse-face 'highlight
       'help-echo "Switch conversation variant"
       'niwa-view-variant-fork-point-id fork-point-id
       'niwa-view-zone-activate
       (lambda ()
         (niwa-view-switch-conversation-variant fork-point-id))))))

(defun niwa-view--render-turn
    (turn data-buf &optional decorate-variants variant-session continuation-p)
  "Render a single TURN into the view buffer at the input marker.
DATA-BUF is the gptel data buffer for reading source content.
TURN is a plist with :role, :segments, :start, :end.
DECORATE-VARIANTS adds conversation variant switches for settled history.
VARIANT-SESSION supplies their live session context when DATA-BUF is archived.
CONTINUATION-P appends an already-started assistant turn without a new header."
  (let ((role (plist-get turn :role))
        (segments (plist-get turn :segments))
        (turn-start (plist-get turn :start))
        (turn-end (plist-get turn :end))
        (context (unless continuation-p
                   (with-current-buffer data-buf
                     (list :data data-buf :tick (buffer-modified-tick)
                           :turn turn :start (point-min) :end (point-max)
                           :session variant-session
                           :fork-points niwa-view--conversation-fork-points
                           :variants niwa-view--conversation-variant-sessions))))
        (turn-source nil))
    (setq turn-source
          (niwa-view-disclosure-source-range data-buf turn-start turn-end))
    ;; Skip user turns that are empty after cleaning (e.g., turns
    ;; containing only org reasoning markers or response separators).
    (unless (and (eq role 'user)
                 (string-empty-p
                  (niwa-view--user-turn-text segments data-buf))
                 (null (niwa-view--user-turn-prompt-drawers
                        segments data-buf)))
    (save-excursion
      (let ((target (niwa-view--current-render-insertion-marker)))
        (goto-char target)
        ;; Temporarily let the marker advance past our insertions so
        ;; successive turns are appended in order.  Incremental renders
        ;; bind TARGET to the status boundary; full renders use the
        ;; input marker.
        (niwa-view-render--with-boundaries-advancing
          (let ((inhibit-read-only t)
                (insert-start (point)))
            (pcase role
              ('task-background
               (niwa-view--render-task-background
                segments data-buf turn-source))
              ('user
               (niwa-view--render-user-turn
                segments data-buf))
              ('assistant
               (niwa-view--render-assistant-turn
                segments data-buf
                (and decorate-variants
                     (niwa-view--conversation-variant-button
                      data-buf turn-start turn-end variant-session))
                continuation-p)))
            ;; Trailing separator: a rule closes an assistant turn, a plain
            ;; spacer follows any other turn that rendered something.
            (cond
             ((not (eq role 'assistant))
              (when (> (point) insert-start)
                (insert (propertize "\n" 'font-lock-face
                                    'niwa-view-separator))))
             ;; The request summary already drew the closing rule.  Leave
             ;; one row under it before the next heading.
             ((eq (niwa-view--previous-rendered-type) 'request-summary)
              (insert (propertize "\n" 'font-lock-face
                                  'niwa-view-separator)))
             (t
              ;; Blank line above the rule so it doesn't butt up against
              ;; the last response line.
              (niwa-view--ensure-blank-line-before-response)
              (insert (propertize "\n" 'font-lock-face
                                  'niwa-view-turn-rule))))
            ;; Apply read-only to the entire block.  Per-segment source
            ;; coordinates are set by the individual render functions;
            ;; tag text that has no segment-level source with the turn
            ;; bounds (headers, separators).
            (niwa-view-render-add-display-properties
             insert-start (point))
            ;; Fill in source on regions that have none yet (headers,
            ;; separators) so the entire block is navigable.  Mailbox
            ;; deliveries are locally toggled cards; do not stamp them
            ;; with the enclosing assistant turn source, otherwise TAB
            ;; can reinterpret them as the previous source-backed tool.
            (let ((pos insert-start))
              (while (< pos (point))
                (let* ((source-next
                        (or (next-single-property-change
                             pos 'niwa-view-source nil (point))
                            (point)))
                       (type-next
                        (or (next-single-property-change
                             pos 'niwa-view-type nil (point))
                            (point)))
                       (next (min source-next type-next)))
                  (cond
                   ((eq (get-text-property pos 'niwa-view-type)
                        'mailbox-delivery)
                    (remove-text-properties
                     pos next
                     '(niwa-view-source nil
                       niwa-view-source-key nil
                       niwa-view-agent-handle-p nil
                       niwa-view-agent-status nil))
                    (setq pos next))
                   ((get-text-property pos 'niwa-view-source)
                    (setq pos next))
                   (t
                    (put-text-property pos next 'niwa-view-source
                                       turn-source)
                    (setq pos next))))))
            ;; Tag every character in this turn with a unique id so
            ;; turn-level fold/unfold can find the whole span even after
            ;; inner sections have been expanded or collapsed.
            (add-text-properties
             insert-start (point)
             `(niwa-view-turn-context ,context
               niwa-view-turn-id
               ,(or (and continuation-p
                         (> insert-start (point-min))
                         (get-text-property
                          (1- insert-start) 'niwa-view-turn-id))
                    (plist-get turn :render-id)
                    (gensym "niwa-view-turn-")))))))))))

(defun niwa-view--user-turn-display-text (segments data-buf)
  "Return persisted view text from user SEGMENTS in DATA-BUF, or nil."
  (with-current-buffer data-buf
    (cl-loop for seg in segments
             when (eq (car seg) 'render-data)
             for data = (cdr (niwa-tool-render-data-extract
                              (buffer-substring
                               (cadr seg) (caddr seg))))
             when (and (eq (plist-get data :kind) 'user-display)
                       (stringp (plist-get data :text)))
             return (plist-get data :text))))

(defun niwa-view--user-turn-text (segments data-buf)
  "Extract cleaned user text from SEGMENTS in DATA-BUF.
Returns the concatenated, trimmed text with org scaffolding removed.
Empty string when the turn contains only whitespace or markers."
  (or (niwa-view--user-turn-display-text segments data-buf)
      (with-current-buffer data-buf
        (let (parts)
          (dolist (seg segments)
            (let* ((seg-start (cadr seg))
                   (seg-end (caddr seg))
                   (text (buffer-substring seg-start seg-end)))
              ;; An agent task heading shows as its title; its drawer (the
              ;; agent task path) is metadata, not prompt text.
              (when (let ((case-fold-search nil))
                      (string-match "\\`[ \t\n]*\\* \\(Agent Task:\\)" text))
                (setq text (substring text (match-beginning 1)))
                (when (string-match
                       "\\`\\([^\n]*\n\\):PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?"
                       text)
                  (setq text (replace-match "\\1" t nil text))))
              ;; Strip hidden view render-data side channels.
              (setq text (niwa-view--strip-render-data-display-text text))
              ;; Strip hidden hook audit side channels; they render as separate
              ;; disclosures below the affected transcript artifact.
              (setq text (niwa--strip-hook-audit-blocks text))
              ;; Strip synthetic review action blocks.  They stay in the data
              ;; buffer so the model can resolve follow-ups like "fix finding 2",
              ;; but the normal view should show only the user's visible prompt.
              (setq text (niwa-review-strip-user-action-blocks text))
              ;; Strip model-only hook and Goal lifecycle context.
              (setq text (niwa-view--strip-model-context-blocks text))
              ;; Strip prompt drawer content
              (when (string-match "\\`:PROMPT:\n\\(?:.*\n\\)*?:END:\n?" text)
                (setq text (replace-match "" t t text)))
              ;; Strip leading gptel-org `:PROPERTIES: ... :END:' drawer.
              ;; gptel-org stores per-buffer state (preset, model, system
              ;; prompt, GPTEL_BOUNDS) here; without this strip, the entire
              ;; system prompt leaks into the visible "You" turn on a full
              ;; rerender that didn't pre-narrow past the drawer.
              (when (string-match "\\`[ \t\n]*:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?" text)
                (setq text (replace-match "" t t text)))
              ;; Strip reasoning and tool markers only when present:
              ;; replacement otherwise copies the whole delivery.
              ;; gptel emits `#+begin_tool ...'
              ;; and `#+end_tool' without the `gptel' text property, so the
              ;; separator text around a tool block appears here as a user
              ;; segment -- skip it, otherwise the raw header would render
              ;; as a spurious "You" turn.
              (dolist (markers '("#\\+\\(?:begin\\|end\\)_reasoning[^\n]*\n?"
                                 "#\\+begin_tool[^\n]*\n?"
                                 "#\\+end_tool[^\n]*\n?"))
                (when (string-match-p markers text)
                  (setq text (replace-regexp-in-string markers "" text))))
              (let ((trimmed (string-trim text)))
                (unless (string-empty-p trimmed)
                  (push trimmed parts)))))
          (if (cdr parts)
              (string-join (nreverse parts) "\n")
            (or (car parts) ""))))))

(defun niwa-view--strip-model-context-blocks (text)
  "Return TEXT without generated hook or Goal context blocks."
  (let ((case-fold-search (default-value 'case-fold-search)))
    (if (not (or (string-match-p "<\\(hook\\|goal\\)-context\\(?:[ \\t][^>]*\\)?>" text)
                 (string-search "\n\n\n" text)))
        text
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (re-search-forward
                "<\\(hook\\|goal\\)-context\\(?:[ \\t][^>]*\\)?>" nil t)
          (let ((start (match-beginning 0))
                (close (format "</%s-context>" (match-string 1))))
            (if (search-forward close nil t)
                (progn
                  (delete-region start (point))
                  (goto-char start))
              (goto-char (point-max)))))
        (goto-char (point-min))
        (while (re-search-forward "\n\\{3,\\}" nil t)
          (replace-match "\n\n" t t))
        (buffer-string)))))

(autoload 'niwa-view--hook-audit-records-from-text "niwa-view-audit")
(autoload 'niwa-view--hook-audit-key "niwa-view-audit")
(autoload 'niwa-view--merge-hook-audits "niwa-view-audit")
(autoload 'niwa-view--indent-hook-audit-text "niwa-view-audit")
(autoload 'niwa-view--hook-audit-value-text "niwa-view-audit")
(autoload 'niwa-view--prompt-rewrite-audit-record "niwa-view-audit")
(autoload 'niwa-view--user-turn-hook-audits "niwa-view-audit")
(autoload 'niwa-view--insert-hook-audit-block "niwa-view-audit")
(autoload 'niwa-view-audit-breadcrumb-present-p "niwa-view-audit")
(autoload 'niwa-view-audit-breadcrumb-status "niwa-view-audit")
(autoload 'niwa-view-audit-toggle-hook-audit "niwa-view-audit")
(autoload 'niwa-view--decorate-code-blocks-in-range
  "niwa-view-markdown")
(autoload 'niwa-view--decorate-local-images-in-range
  "niwa-view-markdown")
(autoload 'niwa-view--decorate-markdown-in-range
  "niwa-view-markdown")
(autoload 'niwa-view--last-live-response-boundary
  "niwa-view-markdown")
(autoload 'niwa-view--linkify-paths-in-range "niwa-view-markdown")
(autoload 'niwa-view--linkify-note-mentions-in-range "niwa-view-markdown")

(defun niwa-view--inline-skill-prompt-summary-body (text)
  "Return collapsed prompt body for inline-skill TEXT, or nil."
  (when-let* ((data (niwa-view--inline-skill-render-data-from-text text))
              (body (plist-get data :prompt))
              (body (string-trim body))
              ((not (string-empty-p body))))
    body))

(defun niwa-view--inline-skill-reminder-body (text)
  "Return the attached-skill reminder body of inline-skill TEXT, or nil."
  (when-let* ((data (niwa-view--inline-skill-render-data-from-text text))
              (attachments (plist-get data :attachments)))
    (mapconcat (lambda (attachment) (plist-get attachment :body))
               attachments "\n\n")))

(defun niwa-view--hook-context-unescape (text)
  "Unescape XML entities in hook context TEXT."
  (replace-regexp-in-string
   "&amp;" "&"
   (replace-regexp-in-string
    "&lt;" "<"
    (replace-regexp-in-string
     "&gt;" ">"
     (replace-regexp-in-string
      "&quot;" "\"" (or text "") t t)
     t t)
    t t)
   t t))

(defun niwa-view--hook-context-event-body (text)
  "Return TEXT without the wrapper newlines added around event bodies."
  (let ((text (or text "")))
    (when (string-prefix-p "\n" text)
      (setq text (substring text 1)))
    (when (string-suffix-p "\n" text)
      (setq text (substring text 0 -1)))
    (niwa-view--hook-context-unescape text)))

(defun niwa-view--hook-context-events-from-body (body)
  "Return event-tagged hook context entries parsed from BODY."
  (when (stringp body)
    (let (events)
      (with-temp-buffer
        (insert body)
        (goto-char (point-min))
        (while (re-search-forward
                (concat "<hook-event[ \t\n]+name=\"\\([^\"]+\\)\""
                        "\\(?: source=\"\\([^\"]*\\)\"\\)?"
                        "\\(?: file=\"\\([^\"]*\\)\"\\)?"
                        "\\(?: plugin=\"\\([^\"]*\\)\"\\)?>") nil t)
          (let ((event (niwa-view--hook-context-unescape
                        (match-string 1)))
                (source (match-string 2))
                (source-file (match-string 3))
                (plugin-name (match-string 4))
                (body-start (point)))
            (when (search-forward "</hook-event>" nil t)
              (let ((event-body
                     (niwa-view--hook-context-event-body
                      (buffer-substring-no-properties
                       body-start (match-beginning 0)))))
                (unless (string-empty-p event-body)
                  (let ((entry (list :event event :body event-body)))
                    (dolist (field (list (cons :source source)
                                         (cons :source-file source-file)
                                         (cons :plugin-name plugin-name)))
                      (when (cdr field)
                        (setq entry
                              (plist-put
                               entry (car field)
                               (niwa-view--hook-context-unescape
                                (cdr field))))))
                    (push entry events))))))))
      (nreverse events))))

(defun niwa-view--hook-context-events-from-text (text)
  "Return generated hook context entries parsed from TEXT."
  (when (stringp text)
    (let (events)
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (while (search-forward "<hook-context>" nil t)
          (let ((body-start (point)))
            (when (search-forward "</hook-context>" nil t)
              (setq events
                    (append
                     events
                     (niwa-view--hook-context-events-from-body
                      (buffer-substring-no-properties
                       body-start (match-beginning 0)))))))))
      events)))

(defun niwa-view--user-turn-hook-contexts (segments data-buf)
  "Return hook context blocks found in user SEGMENTS from DATA-BUF."
  (with-current-buffer data-buf
    (let (events first-start last-end)
      (dolist (seg segments)
        (when (memq (car seg) '(user hook-context))
          (let ((seg-end (caddr seg)))
            (save-excursion
              (goto-char (cadr seg))
              (while (search-forward "<hook-context>" seg-end t)
                (let ((block-start (match-beginning 0))
                      (body-start (point)))
                  (when (search-forward "</hook-context>" seg-end t)
                    (unless first-start
                      (setq first-start block-start))
                    (setq last-end (point))
                    (setq events
                          (append
                           events
                           (niwa-view--hook-context-events-from-body
                            (buffer-substring-no-properties
                             body-start (match-beginning 0))))))))))))
      (when events
        (list (list :start first-start
                    :end last-end
                    :events events))))))

(defun niwa-view--normalize-hook-context-events (value)
  "Return normalized hook context event entries from VALUE."
  (cond
   ((null value) nil)
   ((stringp value)
    (niwa-view--hook-context-events-from-body value))
   ((and (listp value)
         (keywordp (car-safe value))
         (plist-member value :body))
    (list value))
   ((listp value)
    (delq nil
          (mapcar (lambda (entry)
                    (when (and (listp entry)
                               (keywordp (car-safe entry))
                               (plist-member entry :body))
                      (let ((body (string-trim
                                   (format "%s" (plist-get entry :body)))))
                        (unless (string-empty-p body)
                          (let ((copy (copy-sequence entry)))
                            (setq copy
                                  (plist-put
                                   copy :event
                                   (format "%s" (or (plist-get entry :event)
                                                    "UserPromptSubmit"))))
                            (plist-put copy :body body))))))
                  value)))))

(defun niwa-view--format-hook-context-block (events expanded)
  "Return display text for hook context EVENTS.
When EXPANDED is non-nil, include each event name and body."
  (let ((events (niwa-view--normalize-hook-context-events events)))
    (concat
     "  \u25c7 hook context added\n"
     (when expanded
       (mapconcat
        (lambda (entry)
          (let ((body (plist-get entry :body)))
            (niwa-view-disclosure-indent-text
             (concat
              (plist-get entry :event) "\n"
              (mapconcat
               (lambda (field)
                 (when-let* ((value (plist-get entry (car field))))
                   (format "%s: %s\n" (cdr field) value)))
               '((:source . "source") (:source-file . "file")
                 (:plugin-name . "plugin"))
               "")
              body "\n")
             4)))
        events
        "")))))

(defun niwa-view--insert-hook-context-block
    (events &optional source expanded)
  "Insert a hook context disclosure for EVENTS.
SOURCE, when non-nil, is the source range in the data buffer.
EXPANDED means insert the disclosure body expanded."
  (when-let* ((events (niwa-view--normalize-hook-context-events events)))
    (let ((start (point))
          (id (gensym "niwa-hook-context-")))
      (insert (niwa-view--format-hook-context-block events expanded))
      (add-text-properties
       start (point)
       `(font-lock-face niwa-view-hook-context
         niwa-view-type hook-context
         niwa-view-collapsed ,(not expanded)
         niwa-view-hook-context-id ,id
         niwa-view-hook-context-events ,events
         niwa-view-source ,source
         niwa-view-source-key ,(niwa-view-disclosure-state-key
                                   source 'hook-context))))))

(defun niwa-view-render-toggle-hook-context ()
  "Toggle a hook context disclosure."
  (let* ((bounds (or (niwa-view-disclosure-section-bounds
                      'niwa-view-hook-context-id)
                     (niwa-view-disclosure-section-bounds)))
         (source (and bounds
                      (get-text-property
                       (car bounds) 'niwa-view-source)))
         (events (or (and bounds
                          (get-text-property
                           (car bounds) 'niwa-view-hook-context-events))
                     (and source
                          (buffer-live-p
                           (niwa-view-segments-display-buffer))
                          (niwa-view--hook-context-events-from-text
                           (niwa-view-disclosure-data-substring
                            (niwa-view-segments-display-buffer)
                            (car source)
                            (cdr source))))))
         (collapsed (and bounds
                         (get-text-property
                          (car bounds) 'niwa-view-collapsed)))
         (turn-id (and bounds
                       (get-text-property
                        (car bounds) 'niwa-view-turn-id))))
    (unless bounds
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No collapsible section at point"))
    (let ((inhibit-read-only t)
          (start (car bounds))
          (end (cdr bounds)))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (niwa-view--insert-hook-context-block events source collapsed)
        (niwa-view-disclosure-record-state source 'hook-context
                                                     (not collapsed))
        (when turn-id
          (put-text-property start (point)
                             'niwa-view-turn-id turn-id))
        (niwa-view-render-add-display-properties
         start (point) 'hook-context)))))

(defun niwa-view--inline-skill-info (segments data-buf)
  "Return inline-skill render info from SEGMENTS in DATA-BUF, or nil."
  (with-current-buffer data-buf
    (let (info hook-audits)
      (dolist (seg segments)
        (when (memq (car seg)
                    '(user hook-context prompt render-data ignored))
          (let ((text (buffer-substring (cadr seg) (caddr seg))))
            (unless info
              (dolist (block (niwa-tool-render-data-blocks text))
                (when-let* (((not info))
                            (data (caddr block))
                            ((eq (plist-get data :kind) 'inline-skill))
                            ((not (plist-member data :niwa-tool-use-id)))
                            ((niwa-tool-render-data-trusted-range-p
                              (car block) (cadr block) text)))
                  ;; Anchor the record, not its surrounding segment: gptel
                  ;; may append separators to that segment during dispatch.
                  ;; The codec already decoded the payload and its bounds.
                  (setq info
                        (plist-put
                         data :source
                         (niwa-view-disclosure-source-range
                          data-buf (+ (cadr seg) (car block))
                          (+ (cadr seg) (cadr block))))))))
            (setq hook-audits
                  (append hook-audits
                          (niwa-view--hook-audit-records-from-text text))))))
      (when info
        (plist-put info :hook-audits hook-audits)))))

(defun niwa-view--render-task-background (segments data-buf turn-source)
  "Render the advisory task-background SEGMENTS of DATA-BUF at TURN-SOURCE.
The block is folded like a tool result rather than shown as parent
dialogue, because the following Agent Task is the authoritative one."
  (let ((text
         (with-current-buffer data-buf
           (buffer-substring-no-properties
            (cadar segments) (caddar segments)))))
    (niwa-view--insert-rendered-tool
     (list :header "Task background"
           :body (replace-regexp-in-string
                  "\\`<task-background>[ \t]*\n\\|\n?</task-background>[ \t]*\n?\\'"
                  "" text)
           :body-mode 'markdown-mode
           :vtype 'task-background
           :initially-collapsed-p t)
     turn-source)))

(defun niwa-view--user-turn-attribution (segments data-buf)
  "Return the guest or background-event attribution owning this turn, or nil.
SEGMENTS are the turn's data-buffer spans.  The attribution block sits
in the turn's trailing audit strip, or inside the turn's own span when
segment repair absorbed it, so the lookup covers the turn extent plus
the contiguous run of audit blocks that follows it."
  (when (and segments (buffer-live-p data-buf))
    (with-current-buffer data-buf
      (let* ((start (cadr (car segments)))
             (end (apply #'max (mapcar #'caddr segments)))
             (strip-end
              (save-excursion
                (goto-char end)
                (catch 'done
                  (while t
                    (skip-chars-forward " \t\r\n")
                    (if (looking-at-p (regexp-quote niwa--hook-audit-open))
                        (unless (search-forward niwa--hook-audit-close
                                                nil t)
                          (throw 'done nil))
                      (throw 'done nil))))
                (point))))
        (cl-loop for (position . record)
                 in (niwa-transcript-audit-turn-attributions)
                 when (and (>= position start) (< position strip-end))
                 return record)))))

(defun niwa-view--user-input-line-count (text)
  "Return the number of lines in user input TEXT."
  (length (split-string (string-trim-right text "\n+") "\n")))

(defun niwa-view--user-input-fold-p (text)
  "Return non-nil when user input TEXT should render folded.
Org block markers keep the text unfolded: their spans get their own
decorations, which a plain-text fold would hide undecorated."
  (and (> niwa-view-user-input-collapse-line-threshold 0)
       (not (string-match-p "^#\\+begin_" text))
       (> (niwa-view--user-input-line-count text)
          niwa-view-user-input-collapse-line-threshold)))

(defun niwa-view--user-input-fold-summary (text)
  "Return the one-line folded summary for user input TEXT."
  (let* ((hidden (1- (niwa-view--user-input-line-count text)))
         (first-line (car (split-string text "\n"))))
    (concat
     (niwa-view-disclosure-truncate-line first-line 80)
     " "
     (propertize (format "(+%d line%s)" hidden (if (= hidden 1) "" "s"))
                 'font-lock-face 'niwa-view-tool-metadata))))

(defun niwa-view--insert-user-input-fold (text &optional source)
  "Insert user input TEXT folded to an expandable one-line summary.
SOURCE, when available, is the data-buffer range backing the turn so the
fold state survives re-renders; the send-path echo has none and loses
its fold state to the next full rerender, which re-folds from source."
  (let ((start (point)))
    (insert (niwa-view--user-input-fold-summary text))
    (unless (eq (char-before) ?\n)
      (insert "\n"))
    (add-text-properties
     start (point)
     `(niwa-view-type user-input-summary
       niwa-view-collapsed t
       niwa-view-user-input-text ,text))
    (when source
      (add-text-properties
       start (point)
       `(niwa-view-source ,source
         niwa-view-source-key
         ,(niwa-view-disclosure-state-key source 'user-input-summary))))
    (niwa-view-render-add-display-properties
     start (point) 'user-input-summary)))

(defun niwa-view-render-toggle-user-input ()
  "Toggle the folded user input section at point.
The full input travels in a text property rather than being re-read
from the data buffer, so the send-path echo -- which has no source
coordinates yet -- folds and expands the same way as a rendered turn."
  (let* ((bounds (and (eq (get-text-property (point) 'niwa-view-type)
                        'user-input-summary)
                       (niwa-view-disclosure-section-bounds
                        'niwa-view-type)))
         (start (car-safe bounds)))
    (unless bounds
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No collapsible section at point"))
    (let* ((inhibit-read-only t)
           (end (cdr bounds))
           (collapsed (get-text-property start 'niwa-view-collapsed))
           (source (get-text-property start 'niwa-view-source))
           (source-key (get-text-property start 'niwa-view-source-key))
           (turn-id (get-text-property start 'niwa-view-turn-id))
           (text (or (get-text-property start 'niwa-view-user-input-text)
                     (buffer-substring-no-properties start end))))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (let ((ins-start (point)))
          (if collapsed
              (progn
                (insert (niwa-view--fontify-as text 'markdown-mode))
                (unless (eq (char-before) ?\n)
                  (insert "\n")))
            (insert (niwa-view--user-input-fold-summary text))
            (insert "\n"))
          (add-text-properties
           ins-start (point)
           `(niwa-view-type user-input-summary
             niwa-view-collapsed ,(not collapsed)
             niwa-view-user-input-text ,text))
          (when source
            (let ((key (niwa-view-disclosure-state-key
                        source 'user-input-summary source-key)))
              (add-text-properties
               ins-start (point)
               `(niwa-view-source ,source
                 niwa-view-source-key ,key))
              (niwa-view-disclosure-record-state-for-key
               key (not collapsed))))
          (when turn-id
            (put-text-property
             ins-start (point) 'niwa-view-turn-id turn-id))
          (niwa-view-render-add-display-properties
           ins-start (point) 'user-input-summary))))))

(defun niwa-view--insert-shared-context (display &optional source)
  "Insert the attributed question and folded context from DISPLAY.
SOURCE bounds the original user input.  Anchor the disclosure to the exact
context suffix so live echoes and later full renders share its fold state.
Return the start of the inserted disclosure."
  (insert (niwa-view--fontify-as (plist-get display :text) 'markdown-mode)
          "\n")
  (let ((body (plist-get display :context))
        (start (point)))
    (when (and source (markerp (car source)))
      (setq source
            (with-current-buffer (marker-buffer (car source))
              (save-excursion
                (goto-char (car source))
                (when (search-forward body (cdr source) t)
                  (niwa-view-disclosure-source-range
                   (current-buffer) (- (point) (length body)) (point)))))))
    (niwa-view--insert-user-input-fold
     (concat (plist-get display :label) "\n" body) source)
    start))

(defun niwa-view--prompt-preview (text shared-display)
  "Return a one-line preview without mailbox blocks from visible TEXT.
Use SHARED-DISPLAY text when it replaces the original prompt."
  (let ((visible (or (plist-get shared-display :text) text)))
    (when (string-match-p "<agent-\\(?:result\\|message\\)\\s-+" visible)
      (setq visible
            (with-temp-buffer
              (insert visible)
              (goto-char (point-min))
              (while (re-search-forward
                      "<agent-\\(?:result\\|message\\)\\s-+" nil t)
                (goto-char (match-beginning 0))
                (if-let* ((block (niwa-transcript--mailbox-any-block-at-point
                                  (point-max))))
                    (delete-region (point) (plist-get block :close-end))
                  (forward-char 1)))
              (buffer-substring-no-properties (point-min) (point-max)))))
    (unless (string-blank-p visible)
      (let ((preview (replace-regexp-in-string
                      "[[:space:]\n]+" " " (string-trim visible))))
        ;; Header-line evaluation measures this string on every redisplay.
        ;; Retain the full prompt only in its source-backed transcript row.
        (if (> (length preview) 512)
            (concat (substring preview 0 511) "…")
          preview)))))

(defun niwa-view--insert-skill-disclosures (info)
  "Insert source-backed attachment and Prompt disclosures from skill INFO."
  (let ((attachments (plist-get info :attachments))
        (body (string-trim (or (plist-get info :prompt) "")))
        (source (plist-get info :source)))
    (when attachments
      ;; Section bounds use source identity, not equality.  Keep the two
      ;; rows separate even though they originate in the same source record.
      (niwa-view--insert-rendered-tool
       (list :header (niwa-view--attached-skills-summary
                      (mapcar (lambda (attachment) (plist-get attachment :name))
                              attachments)
                      (length attachments))
             :body (mapconcat (lambda (attachment) (plist-get attachment :body))
                              attachments "\n\n")
             :body-mode 'markdown-mode
             :vtype 'system-reminder-summary
             :initially-collapsed-p t)
       (cons (car source) (cdr source))))
    (unless (string-empty-p body)
      (niwa-view--insert-rendered-tool
       (list :header "Prompt" :body body :body-mode 'markdown-mode
             :vtype 'prompt-summary :initially-collapsed-p t)
       source))))

(defun niwa-view--render-user-turn (segments data-buf)
  "Render user SEGMENTS from DATA-BUF."
  (let* ((insert-start (point))
         (raw-text (niwa-view--user-turn-text segments data-buf))
         (prompt-drawers (niwa-view--user-turn-prompt-drawers
                          segments data-buf))
         (hook-contexts (niwa-view--user-turn-hook-contexts
                         segments data-buf))
         (inline-skill (niwa-view--inline-skill-info segments data-buf))
         (hook-audits (niwa-view--merge-hook-audits
                       (niwa-view--user-turn-hook-audits
                        segments data-buf)
                       (plist-get inline-skill :hook-audits)))
         (inline-source-seg (cl-find 'user segments :key #'car))
         (text (if prompt-drawers
                   raw-text
                 (or (niwa-view--user-turn-display-text segments data-buf)
                     (plist-get inline-skill :display-text)
                     raw-text)))
         (attribution (niwa-view--user-turn-attribution segments data-buf))
         (shared-display (niwa-transcript-audit-shared-context
                          text (plist-get attribution :shared)))
         (text-start nil))
    (when (eq (plist-get attribution :type) 'background-event)
      (setq text ""))
    (cond
     ((and (string-empty-p text)
           (null prompt-drawers)
           (null hook-contexts)
           (null hook-audits))
      nil)
     ((niwa-view--mailbox-only-text-p text)
      (setq text-start (point))
      (insert text)
      (niwa-view--decorate-agent-result-blocks text-start (point))
      (niwa-view--decorate-agent-message-blocks text-start (point)))
     (t
      (let* ((guest (plist-get attribution :name))
             (background
              (eq (plist-get attribution :type) 'background-event)))
        (unless background (setq niwa-view--prompt-headers nil))
        (insert (propertize
                 (cond
                  (background "")
                  (guest (format "%s (guest)\n" guest))
                  (t "You\n"))
                 'font-lock-face
                 (if (or guest background)
                     'niwa-view-guest-header
                   'niwa-view-user-header)
                 'niwa-view-type 'turn-header
                 'niwa-view-prompt-preview
                 (niwa-view--prompt-preview text shared-display)
                 'niwa-view-turn-role 'user
                 'niwa-view-collapsed nil)))
      (setq text-start (point))
      (unless (string-empty-p text)
        (cond
         (shared-display
          (niwa-view--insert-shared-context
           shared-display
           (niwa-view-disclosure-source-range
            data-buf (cadr (car segments)) (apply #'max (mapcar #'caddr segments)))))
         ((and (null prompt-drawers)
               (niwa-view--user-input-fold-p text))
          (niwa-view--insert-user-input-fold
           text
           (when-let* ((user-segs
                       (cl-remove-if-not
                        (lambda (seg) (eq (car seg) 'user))
                        segments)))
             (niwa-view-disclosure-source-range
              data-buf
              (cadr (car user-segs))
              (caddr (car (last user-segs)))))))
         (t
          (insert (if prompt-drawers
                      text
                    (niwa-view--fontify-as text 'markdown-mode)))
          (unless (eq (char-before) ?\n)
            (insert "\n")))))
      ;; Decorate mailbox blocks that appear inside mixed user text.
      (niwa-view--decorate-agent-result-blocks text-start (point))
      (niwa-view--decorate-agent-message-blocks text-start (point))
      (niwa-view--linkify-note-mentions-in-range text-start (point))
      (dolist (ctx hook-contexts)
        (niwa-view--insert-hook-context-block
         (plist-get ctx :events)
         (niwa-view-disclosure-source-range
          data-buf (plist-get ctx :start) (plist-get ctx :end))))
      (dolist (audit hook-audits)
        (niwa-view--insert-hook-audit-block
         audit
         (plist-get audit :source)))
      (dolist (drawer prompt-drawers)
        (niwa-view--insert-rendered-tool
         (list :header "Prompt"
               :body (plist-get drawer :body)
               :body-mode 'markdown-mode
               :vtype 'prompt-summary
               :initially-collapsed-p t)
         (niwa-view-disclosure-source-range
          data-buf (plist-get drawer :start) (plist-get drawer :end))))
      (when (and inline-skill inline-source-seg)
        (niwa-view--insert-skill-disclosures inline-skill))))
    ;; A background wake and its attribution can both render nothing.
    ;; Keep visible reminders, but do not give a hidden turn spacer rows.
    (when (> (point) insert-start)
      (insert "\n"))))

(defun niwa-view--user-turn-prompt-drawers (segments data-buf)
  "Return prompt drawer plists from user SEGMENTS in DATA-BUF.
Each plist contains :start, :end, and :body for a `:PROMPT:' drawer."
  (with-current-buffer data-buf
    (let (drawers)
      (dolist (seg segments)
        (when (memq (car seg) '(user prompt ignored))
          (let ((seg-end (caddr seg)))
            (save-excursion
              (goto-char (cadr seg))
              (while (re-search-forward "^:PROMPT:\n" seg-end t)
                (let ((drawer-start (match-beginning 0))
                      (body-start (match-end 0)))
                  (when (re-search-forward "^:END:[ \t]*\n?" seg-end t)
                    (let ((body-end (match-beginning 0))
                          (drawer-end (match-end 0)))
                      (push (list :start drawer-start
                                  :end drawer-end
                                  :body (buffer-substring-no-properties
                                         body-start body-end))
                            drawers)))))))))
      (nreverse drawers))))

(defun niwa-view--flush-thinking-group (thinking-group data-buf)
  "Render accumulated THINKING-GROUP segments from DATA-BUF.
Merge fragments of one reasoning block into an expanded disclosure."
  (when thinking-group
    (let* ((segs (nreverse thinking-group))
           (first-start (cadr (car segs)))
           (last-end (caddr (car (last segs))))
           (bounds (niwa-view--reasoning-source-bounds
                    data-buf first-start last-end))
           (first-start (or (car-safe bounds) first-start))
           (last-end (or (cdr-safe bounds) last-end))
           (source (niwa-view-disclosure-source-range
                    data-buf first-start last-end))
           (summary (niwa-view--thinking-summary
                     data-buf first-start last-end)))
      (unless (string-empty-p summary)
        (let ((unit-start (point)))
          (niwa-view--ensure-blank-line-after-response)
          (niwa-view--insert-rendered-tool
           (list :header (niwa-view--summary-with-face
                          summary 'niwa-view-thinking-summary)
                 :body (with-current-buffer data-buf
                         (string-trim
                          (niwa-view--clean-reasoning-text
                           (buffer-substring first-start last-end))))
                 :body-mode 'markdown-mode
                 :vtype 'thinking-summary
                 :initially-collapsed-p nil)
           source)
          (niwa-view--mark-live-render-unit unit-start first-start))))))

(defun niwa-view--render-system-reminder-segment (seg data-buf)
  "Render system-reminder SEG from DATA-BUF as a control row."
  (let* ((seg-start (cadr seg))
         (seg-end (caddr seg))
         (source (niwa-view-disclosure-source-range data-buf seg-start seg-end))
         (body
          (with-current-buffer data-buf
            (niwa-view--system-reminder-body-from-text
             (buffer-substring-no-properties seg-start seg-end))))
         (summary (niwa-view--system-reminder-summary
                   data-buf seg-start seg-end)))
    (niwa-view--ensure-blank-line-after-response)
    (niwa-view--insert-rendered-tool
     (list :header summary
           :body body
           :body-mode 'markdown-mode
           :vtype 'system-reminder-summary
           :initially-collapsed-p t)
     source)))

(defun niwa-view--request-summary-line (render-data)
  "Return the visible request summary line for RENDER-DATA."
  (let ((elapsed (plist-get render-data :elapsed-seconds))
        (finished (plist-get render-data :finished-at))
        (system-time-locale "C"))
    (when (numberp elapsed)
      (concat
       (format "── Worked for %s" (niwa--duration-label elapsed))
       (when (numberp finished)
         (concat " · "
                 (propertize
                  (format-time-string "%a %d %b %H:%M" finished)
                  'help-echo (niwa-view--request-summary-details render-data))))
       " "))))

(defun niwa-view--request-summary-details (render-data)
  "Return full timing details for RENDER-DATA, or nil without timestamps."
  (let ((started (plist-get render-data :started-at))
        (finished (plist-get render-data :finished-at))
        (elapsed (plist-get render-data :elapsed-seconds))
        (system-time-locale "C"))
    (when (and (numberp started) (numberp finished) (numberp elapsed))
      (format "Started: %s\nFinished: %s\nActive work: %s"
              (format-time-string "%a %d %b %Y %H:%M:%S %z" started)
              (format-time-string "%a %d %b %Y %H:%M:%S %z" finished)
              (niwa--duration-label elapsed)))))

(defun niwa-view--render-request-summary-segment (seg data-buf)
  "Render request-summary SEG from DATA-BUF as the assistant turn's closing rule.
The label sits on the rule: a strike-through on the extended newline
continues it to the window edge, so no width has to be computed for
narrow or split windows.  `niwa-view--render-turn' then omits its own
rule."
  (let* ((seg-start (cadr seg))
         (seg-end (caddr seg))
         (render-data
          (with-current-buffer data-buf
            (niwa-view--request-summary-render-data-from-text
             (buffer-substring seg-start seg-end))))
         (line (and render-data
                    (niwa-view--request-summary-line render-data)))
         (failure-rendering
          (niwa-view--segment-rendering data-buf seg-start seg-end))
         (source (niwa-view-disclosure-source-range data-buf seg-start seg-end)))
    (when failure-rendering
      (niwa-view--ensure-blank-line-after-response)
      (niwa-view--insert-rendered-tool failure-rendering source))
    (when line
      (niwa-view--ensure-blank-line-before-response)
      (let ((start (point)))
        (insert line "\n")
        (put-text-property start (1- (point))
                           'font-lock-face 'niwa-view-separator)
        (put-text-property (1- (point)) (point)
                           'font-lock-face 'niwa-view-request-summary-rule)
        (add-text-properties
         start (point)
         `(niwa-view-type request-summary
           niwa-view-source ,source
           niwa-view-collapsed nil))))))

(defun niwa-view--ensure-blank-line-before-response ()
  "Insert a blank line before a response segment when missing.
Visually separates the response text from preceding thinking summaries,
tool summaries, or the \"Assistant\" turn header.  A blank line is only
added when the text before point does not already end with a blank line
-- so consecutive response segments don't accumulate extra spacing."
  (unless (or (bobp)
              (and (eq (char-before) ?\n)
                   (> (1- (point)) (point-min))
                   (eq (char-before (1- (point))) ?\n)))
    (insert "\n")))

(defun niwa-view--previous-rendered-type ()
  "Return the `niwa-view-type' of the preceding rendered character."
  (let ((pos (point)))
    (while (and (> pos (point-min))
                (memq (char-before pos) '(?\n ?\s ?\t)))
      (setq pos (1- pos)))
    (when (> pos (point-min))
      (get-text-property (1- pos) 'niwa-view-type))))

(defun niwa-view--ensure-blank-line-after-response ()
  "Insert a blank line before activity following response prose.
Activity hangs below the prose that announced it; the rule that sets it
apart comes before the next prose instead, see
`niwa-view--insert-activity-rule-before-response'."
  (when (eq (niwa-view--previous-rendered-type) 'response)
    (niwa-view--ensure-blank-line-before-response)))

(defun niwa-view--activity-before-point-p ()
  "Return non-nil when this turn's preceding rendered row is activity.
Activity is anything other than prose or a heading: tool rows, thinking,
reminders, mailbox cards.  Turn-closing chrome (the user spacer, the turn
rule) ends the search, so a turn never takes its rule from the turn
before it."
  (let ((pos (point)))
    (while (and (> pos (point-min))
                (memq (char-before pos) '(?\n ?\s ?\t))
                (not (memq (get-text-property (1- pos) 'font-lock-face)
                           '(niwa-view-separator
                             niwa-view-request-summary-rule
                             niwa-view-turn-rule))))
      (setq pos (1- pos)))
    (and (> pos (point-min))
         (not (memq (char-before pos) '(?\n ?\s ?\t)))
         (when-let* ((type (get-text-property (1- pos) 'niwa-view-type)))
           (not (memq type '(response turn-header activity-separator)))))))

(defconst niwa-view--activity-rule
  (concat (propertize "  " 'niwa-view-type 'activity-separator
                      'niwa-view-collapsed nil)
          (propertize "\n"
                      'font-lock-face 'niwa-view-activity-rule
                      'niwa-view-type 'activity-separator
                      'niwa-view-collapsed nil))
  "Text of the quiet rule before prose that follows activity.
The two leading columns stay undecorated, which insets the rule.")

(defun niwa-view--insert-activity-rule-before-response ()
  "Insert a quiet inset rule before prose that follows activity.
The rule closes the activity the previous prose announced, so each
section reads as intent followed by its actions.  It is decided when
the prose arrives: the activity before it is already rendered, so no
lookahead or later repair is needed."
  (when (niwa-view--activity-before-point-p)
    (niwa-view--ensure-blank-line-before-response)
    (insert niwa-view--activity-rule)))

(defun niwa-view--tool-activity-tool-segment-p (seg data-buf)
  "Return non-nil when SEG belongs to a tool run in DATA-BUF."
  (or (eq (car seg) 'tool)
      (and (eq (car seg) 'ignored)
           (niwa-view--hook-audit-only-segment-p
            data-buf (cadr seg) (caddr seg)))))

(defvar niwa-view--activity-entry-cache nil
  "Entries shared by classification and insertion of one activity run.")

(defun niwa-view--tool-segment-entry (seg data-buf)
  "Return SEG's normalized tool or standalone audit entry in DATA-BUF.
Reuse classification within one activity run while source and session state
stay unchanged, including live terminal completions for archived rows.
Each caller owns the returned entry's coalescing count."
  (let* ((live-data niwa--data-buffer)
         (state (and niwa-view--activity-entry-cache
                     (niwa-view--session-render-state-fingerprint
                      niwa--session)))
         (key (and niwa-view--activity-entry-cache
                   (with-current-buffer data-buf
                     (list data-buf seg (buffer-modified-tick)
                           (point-min) (point-max) state
                           (and (buffer-live-p live-data)
                                (not (eq live-data data-buf))
                                (list live-data
                                      (with-current-buffer live-data
                                        (buffer-modified-tick))))))))
         (cached (and key (gethash key niwa-view--activity-entry-cache))))
    (if cached
        (copy-sequence (cdr cached))
      (let ((entry
	     (let* ((seg-start (cadr seg))
		    (seg-end (caddr seg))
		    (source (niwa-view-disclosure-source-range
			     data-buf seg-start seg-end))
		    (audit-p (eq (car seg) 'ignored))
		    (audits (when audit-p
			      (with-current-buffer data-buf
				(niwa-view--hook-audit-records-from-text
				 (buffer-substring seg-start seg-end)
				 nil data-buf seg-start))))
		    (rendering (unless audit-p
				 (niwa-view--segment-rendering
				  data-buf seg-start seg-end t)))
		    (vtype (or (plist-get rendering :vtype) 'tool-summary))
		    (state (and rendering
				(niwa-view-disclosure-state-entry source vtype))))
	       (when (and state (not (cdr state)))
		 (setq rendering
		       (or (niwa-view--segment-rendering
			    data-buf seg-start seg-end)
			   rendering)))
	       (let ((group-child (plist-get rendering :group-child)))
		 (when group-child
		   (setq rendering
			 (plist-put (copy-sequence rendering) :group-child nil)))
		 (unless (or (plist-get rendering :hidden-p)
			     (and audit-p (null audits)))
		   (list :kind (if audit-p 'hook-audit 'tool)
			 :start seg-start
			 :end seg-end
			 :source source
			 :rendering rendering
			 :hook-audits audits
			 :group-child group-child
			 :count 1))))))
        (when key
          (puthash key (cons t (copy-sequence entry))
                   niwa-view--activity-entry-cache))
        entry))))


(defun niwa-view--mailbox-activity-entry (seg data-buf)
  "Return the activity entry for mailbox SEG in DATA-BUF.
Keep the canonical card text so grouped and standalone deliveries use
one renderer, including execution summaries and sender links.  Return nil
for this transcript's own completion delivery, which renders nothing.
Classification and insertion reuse this entry within one activity run."
  (let* ((key (and niwa-view--activity-entry-cache
                   (let ((agent niwa-view--agent-path))
                     (with-current-buffer data-buf
                       (list 'mailbox data-buf seg (buffer-modified-tick)
                             (point-min) (point-max) agent)))))
         (cached (and key (gethash key niwa-view--activity-entry-cache))))
    (if cached
        (copy-sequence (cdr cached))
      (let* ((text (niwa-view--user-turn-text (list seg) data-buf))
             (opener (and (string-match
                           "\\`[ \t\n]*<\\(agent-result\\|agent-message\\)\\s-[^>]*>" text)
                          (match-string 0 text)))
             (result-p (and opener (equal (match-string 1 text) "agent-result")))
             (notice
              (and opener
                   (or result-p
                       (member (niwa-view--mail-attribute opener "type")
                               '("EXECUTION" "MONITOR" "MONITOR_TERMINAL")))))
             (entry
              (unless (and notice (not result-p)
                           (niwa-view--own-completion-mail-p
                            opener (niwa-view--mail-attribute opener "sender")))
                (list :kind 'mailbox :start (cadr seg) :end (caddr seg)
                      :notice-p notice
                      :group-child
                      (list :kind 'mailbox :status 'success
                            :rendering (list :mailbox-text text))))))
        (when key
          (puthash key (cons t (copy-sequence entry)) niwa-view--activity-entry-cache))
        entry))))

(defun niwa-view--completion-record-p (record)
  "Return non-nil when hook audit RECORD is an execution breadcrumb."
  (eq (plist-get record :type) 'execution-breadcrumb))

(defun niwa-view--split-completion-entries (entries)
  "Return activity ENTRIES with execution breadcrumbs as entries of their own.
A breadcrumb lands in the hook audits of whichever tool or audit segment
precedes it, which would make that row demand individual presentation.  As a
`completion' entry it instead folds into the activity group and is counted
there, and still renders as its own line when no group absorbs it."
  (mapcan
   (lambda (entry)
     (let* ((audit-p (eq (plist-get entry :kind) 'hook-audit))
            (rendering (plist-get entry :rendering))
            (audits (if audit-p
                        (plist-get entry :hook-audits)
                      (plist-get rendering :hook-audits)))
            (crumbs (cl-remove-if-not #'niwa-view--completion-record-p
                                      audits)))
       (if (null crumbs)
           (list entry)
         (let ((others (cl-remove-if #'niwa-view--completion-record-p
                                     audits)))
           (append
            (cond
             ((not audit-p)
              (list (plist-put (copy-sequence entry) :rendering
                               (plist-put (copy-sequence rendering)
                                          :hook-audits others))))
             (others
              (list (plist-put (copy-sequence entry) :hook-audits others))))
            (mapcar
             (lambda (record)
               (let ((source (or (plist-get record :source)
                                 (plist-get entry :source))))
                 (list :kind 'completion
                       :start (marker-position (car source))
                       :end (marker-position (cdr source))
                       :source source
                       :count 1
                       :group-child
                       (list :kind 'completion :status 'success
                             :rendering (list :completion-record record)))))
             crumbs))))))
   entries))

(defun niwa-view--tool-activity-entries (segments data-buf)
  "Return ordered tool, audit and mailbox entries for SEGMENTS in DATA-BUF.
Rows the renderer hides and this transcript's own completion mail have no
entry, so they neither join nor split activity.  Adjacent visible rows with
equal `:coalesce-key' values become one entry counting the combined calls,
with every call's hook audits in source order.  Execution breadcrumbs
become entries of their own."
  (let (out)
    (dolist (seg (niwa-view--merge-tool-hook-audit-segments segments data-buf))
      (when-let* ((entry
                   (pcase (car seg)
                     ('tool (niwa-view--tool-segment-entry seg data-buf))
                     ('mailbox (niwa-view--mailbox-activity-entry seg data-buf))
                     ('ignored
                      (when (niwa-view--hook-audit-only-segment-p
                             data-buf (cadr seg) (caddr seg))
                        (niwa-view--tool-segment-entry seg data-buf))))))
        (let* ((rendering (plist-get entry :rendering))
               (key (plist-get rendering :coalesce-key))
               (previous (car out)))
          (if (and key previous
                   (equal key (plist-get (plist-get previous :rendering)
                                         :coalesce-key)))
              (setcar out
                      (plist-put
                       (plist-put entry :rendering
                                  (plist-put
                                   (copy-sequence rendering) :hook-audits
                                   (append (plist-get (plist-get previous :rendering)
                                                      :hook-audits)
                                           (plist-get rendering :hook-audits))))
                       :count (1+ (plist-get previous :count))))
            (push entry out)))))
    (niwa-view--split-completion-entries (nreverse out))))

(defun niwa-view--activity-group-entry (entry)
  "Return the activity-group entry for view activity ENTRY.
Tool rows that demand individual presentation, coalescing rows, notices
and standalone audits are boundaries."
  (pcase (plist-get entry :kind)
    ('tool
     (let ((rendering (plist-get entry :rendering)))
       (list :role (if (and (null (plist-get rendering :coalesce-key))
                            (niwa-view--tool-group-entry-p entry))
                       'member
                     'boundary)
             :kind 'tool
             :name (or (plist-get rendering :group-tool)
                       (plist-get (plist-get entry :group-child) :tool)
                       "Tool")
             :status (pcase (niwa-view--rendering-status rendering)
                       ('error 'failed)
                       ('warning 'warning)
                       ('running 'running)
                       (_ 'completed))
             :unconfined (niwa-view--sandbox-unconfined-p
                          (plist-get rendering :sandbox-summary))
             :view entry)))
    ('completion
     (let ((record (plist-get (plist-get (plist-get entry :group-child) :rendering)
                              :completion-record)))
       (list :role 'member :kind 'completion
             :completion (cons (list (plist-get record :execution-id)
                                     (plist-get record :owner))
                               (downcase (niwa-view-audit-breadcrumb-status record)))
             :view entry)))
    ('mailbox
     (list :role (if (plist-get entry :notice-p) 'boundary 'member)
           :kind 'message :view entry))
    (_ (list :role 'boundary :view entry))))

(defun niwa-view--render-activity-entry (entry data-buf)
  "Insert view activity ENTRY from DATA-BUF as its own row.
Return `rendered' or `fallback' for a tool row, nil otherwise."
  (let ((rendering (plist-get entry :rendering))
        (count (plist-get entry :count)))
    (when (and rendering (> count 1))
      (setq rendering
            (plist-put (copy-sequence rendering) :header
                       (format "%s ×%d" (plist-get rendering :header) count))))
    (pcase (plist-get entry :kind)
      ('completion
       (let ((record (plist-get (plist-get (plist-get entry :group-child) :rendering)
                                :completion-record)))
         (niwa-view--insert-hook-audit-block record (plist-get record :source)))
       nil)
      ('hook-audit
       (dolist (record (plist-get entry :hook-audits))
         (niwa-view--insert-hook-audit-block record (plist-get record :source))))
      ('mailbox
       (when-let* ((child (niwa-view--tool-group-child entry data-buf 0)))
         (niwa-view--insert-child-call-block
          child (plist-get child :source) 'derive ""))
       nil)
      (_
       (if rendering
           (progn
             (niwa-view--insert-rendered-tool rendering (plist-get entry :source))
             'rendered)
         (when-let* ((summary (niwa-view--tool-one-liner
                               data-buf (plist-get entry :start) (plist-get entry :end))))
           (let ((source (plist-get entry :source))
                 (ins-start (point)))
             (niwa-view--insert-summary-region
              (niwa-view--summary-with-face summary 'niwa-view-tool-summary)
              `(niwa-view-type tool-summary
                niwa-view-collapsed t
                niwa-view-source ,source
                niwa-view-source-key
                ,(niwa-view-disclosure-state-key source 'tool-summary)))
             (niwa-view--decorate-markdown-in-range ins-start (point)))
           'fallback))))))

(defun niwa-view--render-tool-activity (segments data-buf &optional pending)
  "Render activity SEGMENTS from DATA-BUF in source order.
`niwa-activity-group-partition' decides which runs fold into an activity
group; other rows render individually, each tool call as its own
collapsible entry.  Keep this activity mutable as one live unit until a
response or other turn boundary arrives, so later tool calls can join the
same group.  PENDING in-flight tool entries join the trailing run as
running calls, or follow the activity when it ends in something they
cannot join."
  (let ((unit-start (point))
        (niwa-view--activity-entry-cache
         (or niwa-view--activity-entry-cache (make-hash-table :test #'equal)))
        (start-time (float-time))
        (rendered 0)
        (fallbacks 0)
        ;; Spacing after prose is decided once per activity, not per row.
        (spaced nil))
    (dolist (item (niwa-activity-group-partition
                   (nconc (mapcar #'niwa-view--activity-group-entry
                                  (niwa-view--tool-activity-entries segments data-buf))
                          (mapcar #'niwa-view--pending-activity-entry pending))
                   niwa-view-tool-group-collapse-threshold))
      (unless (or spaced
                  (memq (plist-get (plist-get item :view) :kind)
                        '(completion hook-audit mailbox)))
        (niwa-view--ensure-blank-line-after-response)
        (setq spaced t))
      (cond
       ((plist-get item :pending)
        (niwa-view--insert-pending-tool-row (plist-get item :pending)))
       ((not (plist-get item :group))
        (pcase (niwa-view--render-activity-entry (plist-get item :view) data-buf)
          ('rendered (cl-incf rendered))
          ('fallback (cl-incf fallbacks))))
       ((cl-some (lambda (entry) (plist-get entry :view)) (plist-get item :entries))
        (niwa-view--insert-tool-group item data-buf)
        (cl-incf rendered))
       (t
        (niwa-view--insert-pending-run
         (mapcar (lambda (entry) (plist-get entry :pending))
                 (plist-get item :entries))))))
    (niwa-view--debug-log
     'render-tool-activity
     :segments (length segments)
     :rendered rendered
     :fallbacks fallbacks
     :elapsed (- (float-time) start-time))
    (niwa-view--mark-live-render-unit
     unit-start (cadr (car segments)))))

(defun niwa-view--insert-assistant-header (&optional variant-button extra)
  "Insert the `Assistant' turn heading at point.
VARIANT-BUTTON, when non-nil, follows the label.  EXTRA is a property
list added to the heading's text."
  (let ((header-start (point)))
    (insert "Assistant")
    (when variant-button
      (insert "  " variant-button))
    (insert "\n")
    (add-text-properties
     header-start (point)
     (append '(font-lock-face niwa-view-assistant-header
               niwa-view-type turn-header
               niwa-view-turn-role assistant
               niwa-view-collapsed nil)
             extra))))

(defun niwa-view--live-turn-needs-header-p (start end _data-buf)
  "Return non-nil when running calls at END need an `Assistant' heading.
START..END is the in-flight turn's rendering.  The heading is missing
when no turn heading precedes END there, or the last one belongs to
another role, such as a mailbox delivery."
    (let ((pos start) last)
      (while (and (< pos end)
                  (setq pos (text-property-any pos end 'niwa-view-type
                                               'turn-header)))
        (setq last pos
              pos (or (next-single-property-change
                       pos 'niwa-view-type nil end)
                      end)))
      (not (and last (eq (get-text-property last 'niwa-view-turn-role)
                         'assistant)))))

(defun niwa-view--render-assistant-turn
    (segments data-buf &optional variant-button continuation-p)
  "Render assistant SEGMENTS from DATA-BUF.
Response text is shown inline, tool calls as collapsed one-liners,
and substantive reasoning as standalone, initially expanded disclosures.
Reasoning splits tool activity runs.  Fragments of one thinking block
are merged into a single disclosure.  VARIANT-BUTTON, when non-nil,
is inserted beside the header.  CONTINUATION-P suppresses that header."
  (unless continuation-p
    (niwa-view--insert-assistant-header variant-button))
  (let ((view-buf (current-buffer))
        (last-seg (car (last segments)))
        tool-group thinking-group request-summary-group)
    (cl-labels
        ((flush-thinking ()
           (niwa-view--flush-thinking-group thinking-group data-buf)
           (setq thinking-group nil))
         (flush-tools (&optional trailing)
           (when tool-group
             (niwa-view--render-tool-activity
              (nreverse tool-group) data-buf
              ;; Only the turn's final activity can take in-flight calls.
              (and trailing (niwa-view--take-render-pending)))
             (setq tool-group nil))))
      (dolist (seg segments)
        (let ((type (car seg)))
          (pcase type
            ('response
             ;; Flush accumulated groups
             (flush-thinking)
             (flush-tools)
             ;; Insert response text with source tracking
             (let ((seg-start (cadr seg))
                   (seg-end (caddr seg))
                   (source nil))
               (setq source
                     (niwa-view-disclosure-source-range data-buf seg-start seg-end))
               (with-current-buffer data-buf
                 (let ((text (string-trim
                               (buffer-substring-no-properties seg-start seg-end))))
                   (setq text (niwa-view--visible-response-text text))
                   (with-current-buffer view-buf
                     (unless (string-empty-p text)
                       (let ((unit-start (point)))
                         (niwa-view--ensure-blank-line-before-response)
                         ;; The rule belongs to this prose's live unit, so
                         ;; retained activity above it never changes.
                         (niwa-view--insert-activity-rule-before-response)
                         (let ((start (point)))
                         (insert (niwa-view--fontify-response text) "\n")
                         (let ((response-end (copy-marker (point) t)))
                           (add-text-properties
                            start response-end
                            `(niwa-view-source ,source
                              niwa-view-source-key ,(niwa-view-disclosure-state-key
                                                        source
                                                        'response)
                              niwa-view-type response
                              niwa-view-collapsed nil))
                           (niwa-view--decorate-agent-result-blocks
                            start response-end)
                           (niwa-view--decorate-agent-message-blocks
                            start response-end)
                           ;; Idle table layout must not prettify a table the
                           ;; next chunk could extend.  Only the streamed turn's
                           ;; last segment can still grow; terminal
                           ;; reconciliation redecorates without this flag.
                           (let ((niwa-view-table--streaming-p
                                  (and (eq seg last-seg)
                                       (not niwa-view-disclosure--settling-p)
                                       (markerp niwa-view--data-turn-start)
                                       (marker-position niwa-view--data-turn-start)
                                       (niwa-view-stream-in-flight-turn-start-position)
                                       (>= seg-start
                                           (marker-position
                                            niwa-view--data-turn-start)))))
                             (niwa-view--decorate-markdown-in-range
                              start response-end))
                           (goto-char response-end)
                           (set-marker response-end nil)))
                         (niwa-view--mark-live-render-unit
                          unit-start seg-start))))))))
            ('tool
             ;; Flush thinking group before tools
             (flush-thinking)
             ;; Accumulate consecutive tool segments
             (push seg tool-group))
            ('system-reminder
             (flush-thinking)
             (flush-tools)
             (niwa-view--render-system-reminder-segment seg data-buf))
            ('request-summary
             (push seg request-summary-group))
            ('mailbox
             (flush-thinking)
             ;; Leading non-tool entries stay standalone, as they do in
             ;; `niwa-activity-group-partition' for the browser adapter.
             (if tool-group
                 (push seg tool-group)
               (let ((text (niwa-view--user-turn-text (list seg) data-buf))
                     (text-start nil))
                 (niwa-view--ensure-blank-line-before-response)
                 (setq text-start (point))
                 (insert text "\n")
                 (niwa-view--decorate-agent-result-blocks text-start (point))
                 (niwa-view--decorate-agent-message-blocks text-start (point)))))
            ('user
             (let ((seg-start (cadr seg))
                   (seg-end (caddr seg)))
               ;; Drop org-only glue (`#+end_tool', `#+begin_tool …',
               ;; blank lines) so it doesn't surface as a one-line
               ;; `Thinking…' between adjacent tool blocks.  Skip without
               ;; flushing the tool-group so consecutive tool segments
               ;; separated only by glue still group / render together.
               (unless (niwa-view--scaffolding-only-p
                        data-buf seg-start seg-end)
                 (flush-tools)
                 (push seg thinking-group))))
            ((or 'reasoning 'render-data 'ignored)
             (cond
              ((and (eq type 'ignored)
                    (niwa-view--hook-audit-only-segment-p
                     data-buf (cadr seg) (caddr seg))
                    (not tool-group))
               ;; This includes leading completion breadcrumbs: only a tool
               ;; starts a run in `niwa-activity-group-partition'.
               (flush-thinking)
               (let* ((source (niwa-view-disclosure-source-range
                               data-buf (cadr seg) (caddr seg)))
                      (text (with-current-buffer data-buf
                              (buffer-substring
                               (cadr seg) (caddr seg)))))
                 (dolist (record
                          (niwa-view--hook-audit-records-from-text text))
                   (niwa-view--insert-hook-audit-block record source))))
              ((niwa-view--collaboration-event-segment-p
                data-buf (cadr seg) (caddr seg))
               (flush-thinking)
               (flush-tools)
               (niwa-view--render-collaboration-event-segment data-buf seg))
              ((and tool-group
                    (niwa-view--hook-audit-only-segment-p
                     data-buf (cadr seg) (caddr seg)))
               (push seg tool-group))
              ((not (niwa-view--scaffolding-only-p
                     data-buf (cadr seg) (caddr seg)))
               ;; Only substantive thinking partitions a run.  Empty blocks
               ;; and org glue must not stop neighboring tools grouping.
               (flush-tools)
               ;; The grammar keeps distinct structural reasoning blocks as
               ;; distinct segments, even when their gptel properties match.
               (when (and (eq (caar thinking-group) 'reasoning)
                          (eq type 'reasoning))
                 (flush-thinking))
               (push seg thinking-group)))))))
      ;; Flush remaining groups
      (flush-thinking)
      (flush-tools t)
      ;; Calls after the turn's last prose start their run here, where
      ;; they will settle, rather than after the turn's closing spacing.
      (when-let* ((left (niwa-view--take-render-pending)))
        (niwa-view--insert-pending-run left))
      (dolist (seg (nreverse request-summary-group))
        (niwa-view--render-request-summary-segment seg data-buf)))))

(defun niwa-view--render-collaboration-event-segment (data-buf seg)
  "Render a canonical started collaboration event SEG from DATA-BUF."
  (let* ((seg-start (cadr seg))
         (seg-end (caddr seg))
         (source (niwa-view-disclosure-source-range data-buf seg-start seg-end))
         (render-data
          (with-current-buffer data-buf
            (niwa-view--collaboration-event-from-text
             (buffer-substring seg-start seg-end)))))
    (when-let* ((rendering
                 (and render-data
                      (niwa-tool-ui--render-agent
                       "Agent"
                       (list :task_name
                             (file-name-nondirectory
                              (or (plist-get render-data :path) "agent"))
                             :message
                             (or (plist-get render-data :description) ""))
                       (or (plist-get render-data :body) "")
                       render-data))))
      (niwa-view--insert-rendered-tool rendering source))))

(defun niwa-view--same-tool-call-segment-p (left right data-buf)
  "Return non-nil when LEFT and RIGHT belong to the same tool call."
  (and (eq (car left) 'tool)
       (eq (car right) 'tool)
       (with-current-buffer data-buf
         (and (not (niwa-transcript--org-tool-block-parts
                    (cadr right) (caddr right)))
              (let ((left-id (niwa-transcript--tool-id-in-range
                              (cadr left) (caddr left)))
                    (right-id (niwa-transcript--tool-id-in-range
                               (cadr right) (caddr right))))
                (and left-id right-id (equal left-id right-id)))))))

(defun niwa-view--merge-tool-hook-audit-segments (segments data-buf)
  "Merge hook audit side-channel SEGMENTS into adjacent tool segments."
  (let (out)
    (dolist (seg segments (nreverse out))
      (cond
       ((and out
             (eq (caar out) 'tool)
             (eq (car seg) 'ignored)
             (niwa-view--hook-audit-only-segment-p
              data-buf (cadr seg) (caddr seg)))
        (setcar out (list 'tool (cadar out) (caddr seg))))
       ((and out
             (niwa-view--same-tool-call-segment-p (car out) seg data-buf))
        (setcar out (list 'tool (cadar out) (caddr seg))))
       (t
        (push seg out))))))

(defun niwa-view--tool-group-child (entry data-buf index &optional summary-only)
  "Return the nested-call plist for activity ENTRY in DATA-BUF, or nil.
INDEX discriminates the row's disclosure key from its siblings.  A
segment whose own rendering is hidden yields nil so grouping does not
resurrect rows the renderer suppressed.  SUMMARY-ONLY permits a cached
tool name instead of reparsing arguments and results for a collapsed group.
A large result still being prepared off the main thread is never parsed
here; its row shows the call as running until the result is published."
  (when-let* ((child
               (or (and (eq (plist-get (plist-get entry :rendering) :vtype)
                            'tool-preparing)
                        (list :tool (plist-get (plist-get entry :rendering)
                                               :group-tool)
                              :status 'running))
                   (and summary-only
                        (when-let* ((name (plist-get (plist-get entry :rendering)
                                                    :group-tool)))
                          (list :tool name)))
                   (plist-get entry :group-child)
                   (when-let* ((parsed
                                (niwa-view--tool-call-parse
                                 data-buf
                                 (plist-get entry :start)
                                 (plist-get entry :end))))
                     (list :tool (plist-get parsed :name)
                           :args (plist-get parsed :args)
                           :status
                           (niwa-view--tool-render-status
                            (plist-get parsed :result)
                            (plist-get parsed :render-data))
                           :result (plist-get parsed :result)
                           :render-data (plist-get parsed :render-data))))))
    (append (list :id (format "%d" index) :order index
                  :source (niwa-view-disclosure-source-range
                           data-buf (plist-get entry :start)
                           (plist-get entry :end)))
            child)))

(defun niwa-view--tool-group-rendering (group data-buf &optional summary-only)
  "Return the rendering of activity GROUP from DATA-BUF, or nil.
GROUP comes from `niwa-activity-group-partition'.  The result reuses the
compound-tool row machinery: each tool or delivery is a `:child-calls'
entry with its own disclosure state, and in-flight calls follow the
settled children as running rows.  A newly formed group keeps an already
open child visible.  SUMMARY-ONLY omits cached tool bodies; recompute
before expanding children."
  (let* ((entries (plist-get group :entries))
         (pending (delq nil (mapcar (lambda (entry) (plist-get entry :pending))
                                    entries)))
         (settled
          (let ((index 0)
                out)
            (dolist (entry entries (nreverse out))
              (when-let* ((view (plist-get entry :view)))
                (when-let* ((child (niwa-view--tool-group-child
                                    view data-buf index summary-only)))
                  (push child out))
                (cl-incf index)))))
         (children (append settled (niwa-view--pending-children pending))))
    (when children
      (let ((reader-p (niwa-view-render--reader-on-member-p settled pending))
            completions)
        (dolist (entry entries)
          (when-let* ((completion (plist-get entry :completion))
                      ((not (assoc (car completion) completions))))
            (push (cons (car completion)
                        (plist-get (plist-get (plist-get (plist-get entry :view)
                                                         :group-child)
                                              :rendering)
                                   :completion-record))
                  completions)))
      (list :header (plist-get group :summary)
            :execution-completions (nreverse (mapcar #'cdr completions))
            :vtype 'tool-group
            :expandable-p t
            :child-calls children
            :pending-count (niwa-view--pending-running-count pending)
            :pending-failed (niwa-view--pending-failed-count pending)
            :unconfined (plist-get group :unconfined)
            ;; Only a group whose every call failed is an error; some
            ;; failures warn, as the summary's own counts say which.
            :status (let ((failed (plist-get group :failed)))
                      (cond ((and (> failed 0) (= failed (plist-get group :tools))) 'error)
                            ((or (> failed 0) (plist-get group :warning)) 'warning)))
            :reader-open reader-p
            ;; A new group must not hide a row the reader already opened or
            ;; is on.  An explicit fold of the group itself still takes
            ;; precedence.
            :initially-collapsed-p
            (not (or reader-p
                     (cl-some
                      (lambda (child)
                        (when-let* ((state
                                     (niwa-view-disclosure-state-for-key
                                      (niwa-view--child-call-state-key
                                       child (plist-get child :source)))))
                          (not (cdr state))))
                      settled))))))))

(defun niwa-view--fit-group-header (header rendering)
  "Return HEADER shortened so RENDERING's folded group row is one screen line.
The summary grows with every kind of tool a run uses, and its running count
comes and goes; a summary that wrapped would add or drop a row as it did.
Room is kept for that count, the unconfined note and the disclosure
marker, measured against the narrowest window showing the view.  Failure
counts cut off with the summary are added up after the `…', as in
`… (2 failed)', so a folded row always shows that a call failed."
  (let* ((windows (niwa-view--windows 'geometry))
         (width (if windows
                    (apply #'min (mapcar #'window-body-width windows))
                  (window-body-width)))
         (count (plist-get rendering :pending-count))
         (suffix (concat (and count (format " · %d running" count))
                         (and (plist-get rendering :unconfined) " · unconfined")
                         " ▸"))
         ;; The marker column, a line prefix for nested rows, and the
         ;; last column, which would wrap the line in a terminal.
         (available (max 10 (- width 4 2 1 (string-width suffix)))))
    (if (<= (string-width header) available)
        header
      (let ((len (length header)) (pos 0) (total 0) runs)
        (while (setq pos (text-property-any pos len 'niwa-activity-group-part
                                            'failure header))
          (let* ((end (next-single-property-change
                       pos 'niwa-activity-group-part header len))
                 (n (string-to-number (substring header pos end))))
            (push (list pos end n) runs)
            (setq total (+ total n) pos end)))
        (if (zerop total)
            (truncate-string-to-width header available nil nil "…")
          (let ((cut (length (truncate-string-to-width
                              header
                              (max 1 (- available 1 (string-width
                                                     (format " (%d failed)" total))))))))
            ;; A count is shown whole or not at all: a cut inside ` (N
            ;; failed)' ends before it instead.
            (dolist (run runs)
              (when (and (< (- (nth 0 run) 2) cut) (< cut (1+ (nth 1 run))))
                (setq cut (max 0 (- (nth 0 run) 2)))))
            (let ((hidden (cl-loop for (_start end n) in runs
                                   unless (<= (1+ end) cut) sum n)))
              (concat (substring header 0 cut) "…"
                      (when (> hidden 0)
                        (concat " "
                                (propertize "(" 'niwa-activity-group-part 'muted)
                                (propertize (format "%d failed" hidden)
                                            'niwa-activity-group-part 'failure)
                                (propertize ")" 'niwa-activity-group-part
                                            'muted)))))))))))

(defun niwa-view-render--reader-members ()
  "Return the tool rows under this view's point and its window points."
  (let (members)
    (dolist (pos (cons (point)
                       (mapcar #'window-point
                               (niwa-view--windows 'visibility))))
      (when (and (< pos (point-max))
                 (memq (get-text-property pos 'niwa-view-type)
                       '(tool-summary tool-pending)))
        (if-let* ((key (get-text-property pos 'niwa-view-pending-key)))
            (progn
              (push key members)
              ;; Its settled row is found by the call's transcript id.
              (when-let* ((id (get-text-property
                               pos 'niwa-view-pending-tool-use-id)))
                (push (list :tool-use-id id) members)))
          (when-let* ((start (niwa-view-disclosure-source-start
                              (get-text-property pos 'niwa-view-source))))
            (push start members)))))
    members))

(defun niwa-view-render--reader-on-member-p (children pending)
  "Return non-nil when a reader is on one of CHILDREN or PENDING calls.
CHILDREN are settled group children; PENDING are in-flight entries.  A
reader on a running row still holds that call once it settles: the row's
bound tool-use id is the id its result carries in the transcript."
  (when-let* ((members niwa-view-render--reader-members))
    (let ((ids (delq nil (mapcar (lambda (member)
                                   (plist-get (and (consp member)
                                                   (keywordp (car member))
                                                   member)
                                              :tool-use-id))
                                 members))))
      (or (cl-some (lambda (child)
                     (let ((source (plist-get child :source)))
                       (or (member (niwa-view-disclosure-source-start source)
                                   members)
                           (and ids (consp source) (markerp (car source))
                                (member (with-current-buffer
                                            (marker-buffer (car source))
                                          (niwa-transcript--tool-id-in-range
                                           (car source) (cdr source)))
                                        ids)))))
                   children)
          (cl-some (lambda (entry)
                     (or (member (car entry) members)
                         (and ids (member (plist-get (nth 3 entry) :tool-use-id)
                                          ids))))
                   pending)))))

(defun niwa-view--tool-group-rendering-from-source
    (data-buf start end)
  "Rebuild a grouped activity rendering for DATA-BUF START..END.
`niwa-transcript-segments' expands its END to the containing property
run, so re-segmenting a folded run also returns the segment that begins
where the run ended.  That row was never part of the group: keeping it
summarizes the following response, mailbox, or prompt as one more
delivery.

Entries are classified in the calling view buffer, as the live render
does: view-local state such as `niwa-view--agent-path' decides whether
a delivery is this transcript's own completion mail, which neither joins
nor splits a run.  Classified from the data buffer, an agent view's own
completion mail became a boundary, so a toggle rebuilt only the first
part of the group and dropped the rest."
  (let* ((view-buf (current-buffer))
         (entries
          (with-current-buffer data-buf
            (save-restriction
              (widen)
              (let ((segments (cl-remove-if (lambda (seg) (>= (cadr seg) end))
                                            (niwa-transcript-segments start end))))
                (with-current-buffer view-buf
                  (niwa-view--tool-activity-entries segments data-buf))))))
         (group (seq-find
                 (lambda (item) (plist-get item :group))
                 (niwa-activity-group-partition
                  (mapcar #'niwa-view--activity-group-entry entries) nil))))
    (and group (niwa-view--tool-group-rendering group data-buf))))

(defun niwa-view--tool-group-entry-p (entry)
  "Return non-nil when ENTRY may fold into a grouped activity row.
Rows that demand individual presentation stay out: agent handles and other
non-tool vtypes, compound tools with their own nested
rows, rows carrying hook audits, rows their renderer wants expanded or compact,
coalesced rows, and renderer fallbacks.  Warning and note disclosures
stay with their nested row inside the group.  Failed calls and results still
being prepared stay too: a run's grouping must not change when a call fails
or its result is published."
  (let ((rendering (plist-get entry :rendering)))
    (or (eq (plist-get entry :kind) 'completion)
    (and rendering
         (= (plist-get entry :count) 1)
         (memq (or (plist-get rendering :vtype) 'tool-summary)
               '(tool-summary tool-preparing))
         (null (plist-get rendering :child-calls))
         (null (plist-get rendering :hook-audits))
         (not (plist-get rendering :force-expanded-p))
         (not (and (plist-member rendering :expandable-p)
                   (not (plist-get rendering :expandable-p))))
         (not (and (plist-member rendering :initially-collapsed-p)
                   (not (plist-get rendering :initially-collapsed-p))))))))

(defun niwa-view--insert-tool-group (group data-buf)
  "Insert activity GROUP as one expandable row for DATA-BUF.
GROUP comes from `niwa-activity-group-partition'; its in-flight calls
join as running rows.  Return non-nil when the group row was inserted."
  ;; A split breadcrumb entry follows its call but ends inside the call's
  ;; segment, so the last entry need not reach the group's end.  Collapse
  ;; removes only rows contained in this range.
  (let* ((entries (delq nil (mapcar (lambda (entry) (plist-get entry :view))
                                    (plist-get group :entries))))
         (group-start (apply #'min (mapcar (lambda (entry)
                                             (plist-get entry :start))
                                           entries)))
         (group-end (apply #'max (mapcar (lambda (entry)
                                           (plist-get entry :end))
                                         entries)))
         (rendering (niwa-view--tool-group-rendering group data-buf t))
         (source (and rendering
                      (niwa-view-disclosure-source-range
                       data-buf group-start group-end))))
    (when rendering
      (when (and (eq (car niwa-view-render--pending-reader-group)
                     niwa-view--data-turn-start)
                 (eql (cadr niwa-view-render--pending-reader-group) group-start))
        (unless (niwa-view-disclosure-state-entry source 'tool-group)
          (niwa-view-disclosure-record-state
           source 'tool-group (nth 2 niwa-view-render--pending-reader-group)))
        (setq niwa-view-render--pending-reader-group nil))
      (unless (plist-get
               (niwa-view-disclosure-apply-rendering-state rendering source)
               :initially-collapsed-p)
        ;; A group that formed open under the reader stays open after the
        ;; reader moves on: folding later would move what they just read.
        (when (and (plist-get rendering :reader-open)
                   (not (niwa-view-disclosure-state-entry source 'tool-group)))
          (niwa-view-disclosure-record-state source 'tool-group nil))
        (setq rendering (niwa-view--tool-group-rendering group data-buf)))
      (niwa-view--insert-rendered-tool rendering source)
      t)))

(defun niwa-view--tool-readable-start (raw)
  "Return the reader offset of the tool call in RAW.

Text-property boundaries can include org drawers, `#+begin_tool'
markers, or other unpropertized scaffolding.  Prefer the structural
tool form itself when it is present inside RAW.  Return an offset so
reading the call does not copy its potentially large result body."
  (let ((start 0))
    (when (string-match
           "\\`[ \t\n]*:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?" raw)
      (setq start (match-end 0)))
    (when (and (string-match
                "\\(?:[ \t]*\\(?:#\\+\\(?:begin\\|end\\)_\\(?:tool\\|reasoning\\)[^\n]*\\)?\n\\)+"
                raw start)
               (= (match-beginning 0) start))
      (setq start (match-end 0)))
    (if (string-match "(\\s-*:name\\_>" raw start)
        (match-beginning 0)
      start)))

(defun niwa-view--tool-wrapped-text-p (raw)
  "Return non-nil when RAW includes persisted org tool block scaffolding."
  (and (stringp raw)
       (string-match-p "\\`\\(?:[ \t\n]*\\|:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?\\)*#\\+begin_tool\\b"
                       raw)))

(defun niwa-view--direct-tool-readable-text-p (raw)
  "Return non-nil when RAW itself begins with a readable tool call."
  (when (stringp raw)
    (let ((text (string-trim-left raw)))
      (and (string-match-p "\\`(\\s-*:name\\_>" text)
           (condition-case nil
               (let ((sexp (read text)))
                 (and (listp sexp)
                      (stringp (plist-get sexp :name))))
             (error nil))))))

(defun niwa-view--complete-wrapped-tool-text-p (raw)
  "Return non-nil when RAW is already one complete wrapped tool block."
  (and (niwa-view--tool-wrapped-text-p raw)
       (string-match-p "\n#\\+end_tool[^\n]*\n?\\'" raw)
       (condition-case nil
           (let ((sexp (car (read-from-string
                            raw (niwa-view--tool-readable-start raw)))))
             (and (listp sexp)
                  (stringp (plist-get sexp :name))))
         (error nil))))

(defun niwa-view--strip-trailing-tool-marker (text)
  "Return TEXT without a trailing org `#+end_tool' marker."
  (if (stringp text)
      (string-trim-right
       (replace-regexp-in-string "\n*#\\+end_tool[^\n]*\\'" "" text t t))
    text))

(defvar-local niwa-view--tool-block-bounds-memo nil
  "Data-buffer memo for `niwa-view--tool-block-bounds'.
Keys are (SEG-START SEG-END TICK) integers with TICK from
`buffer-modified-tick': bounds depend on buffer text and gptel text
properties, and transcript restoration stamps properties without
character changes, so the chars tick would serve stale bounds.
Outside a projection the buffer itself keys the boundary index of
`niwa-transcript--tool-block-bounds-for-run'.
Bounded through `niwa-view--cache-put'.")

(defvar-local niwa-view--tool-block-bounds-memo-entries 0
  "Entry count backing `niwa-view--tool-block-bounds-memo'.")

(defun niwa-view--tool-block-bounds (seg-start seg-end)
  "Return org tool-block bounds overlapping SEG-START..SEG-END, or nil.

Restored `GPTEL_BOUNDS' can drift into the `#+begin_tool' line or
past `#+end_tool' when older transcripts are opened.  The org block
markers remain structural anchors, so use them to recover the whole
tool block before parsing the tool plist and render-data side channel.

The result is memoized per segment and buffer tick; segment text parsing
asks for the same bounds several times per redraw.  Outside a projection
the memo also holds the canonical boundary index, which validates itself
against the buffer tick and narrowing.  Unindexed recovery scans all earlier
history for every tool, so cold renders outside a projection were cubic."
  (let* ((key (list (niwa-view--source-position seg-start)
                    (niwa-view--source-position seg-end)
                    (buffer-modified-tick)))
         (memo (or niwa-view--tool-block-bounds-memo
                   (setq niwa-view--tool-block-bounds-memo
                         (make-hash-table :test #'equal))))
         (cached (gethash key memo 'niwa--miss)))
    (if (not (eq cached 'niwa--miss))
        cached
      (niwa-view--cache-put
       memo key
       (let ((niwa-transcript--tool-block-index
              (or niwa-transcript--tool-block-index memo)))
         (niwa-transcript--tool-block-bounds-for-run seg-start seg-end))
       'niwa-view--tool-block-bounds-memo-entries))))

(defun niwa-view--tool-segment-text (seg-start seg-end &optional raw)
  "Return raw tool text for SEG-START..SEG-END.
If the segment overlaps an org tool block, expand to the block bounds
first so stale restored text properties do not hide the `(:name ...)'
form or the render-data block from the parser.
RAW, when supplied, is the already copied SEG-START..SEG-END text."
  (let ((raw (or raw (buffer-substring seg-start seg-end))))
    (if (or (niwa-view--complete-wrapped-tool-text-p raw)
            (and (not (niwa-view--tool-wrapped-text-p raw))
                 (niwa-view--direct-tool-readable-text-p raw)))
        raw
      (pcase-let ((`(,start . ,end)
                   (or (niwa-view--tool-block-bounds seg-start seg-end)
                       (cons seg-start seg-end))))
        (buffer-substring start end)))))

;;
;;; Segment state

(defun niwa-view--rendered-turn-starts ()
  "Return rendered turn starts before the input zone."
  (let ((pos (point-min))
        (limit (niwa-view-transcript-tail))
        starts)
    (while (< pos limit)
      (let ((id (get-text-property pos 'niwa-view-turn-id)))
        (when id
          (push pos starts))
        (setq pos
              (or (next-single-property-change
                   pos 'niwa-view-turn-id nil limit)
                  limit))))
    (nreverse starts)))

(defun niwa-view-render-capture-segment-state ()
  "Capture point, window, and disclosure state for the displayed segment.
Point and the window start are kept as semantic render anchors as well:
the segment is projected again on return, and deferred formatting such as
table layout makes that projection differ in length from this one."
  (let* ((boundary (niwa-view-transcript-tail))
         (turn-starts (niwa-view--rendered-turn-starts))
         (window (get-buffer-window (current-buffer) t)))
    (list
     :point (point)
     :point-anchor (niwa-view--position-render-anchor (point))
     :window-start (and window (window-start window))
     :window-start-anchor
     (and window (niwa-view--position-render-anchor (window-start window)))
     :collapsed-turns
     (cl-loop
      for pos in turn-starts
      for index from 1
      when (get-text-property pos 'niwa-view-collapsed)
      collect index)
     :collapse-states
     (and boundary
          (niwa-view-disclosure-capture-state
           (point-min) boundary)))))

(defun niwa-view-render--restore-segment-state (state direction)
  "Restore segment view STATE, using DIRECTION for a first visit."
  (when-let* ((collapse-states (plist-get state :collapse-states)))
    (niwa-view-disclosure-restore-state
     (point-min) (niwa-view-transcript-tail) collapse-states))
  (let ((turn-starts (niwa-view--rendered-turn-starts)))
    (dolist (pos
             (sort
              (delq
               nil
               (mapcar
                (lambda (index)
                  (nth (1- index) turn-starts))
                (plist-get state :collapsed-turns)))
              #'>))
      (goto-char pos)
      (niwa-view--collapse-turn)))
  (cond
   ((plist-get state :point)
    (goto-char
     (max (point-min)
          (min (or (niwa-view--render-anchor-position
                    (plist-get state :point-anchor))
                   (plist-get state :point))
               (niwa-view-transcript-tail)))))
   ((eq direction 'backward)
    (let ((pos (point-min))
          (limit (niwa-view-transcript-tail))
          target)
      (while (< pos limit)
        (when (get-text-property pos 'niwa-view-turn-role)
          (setq target pos))
        (setq pos
              (or (next-single-property-change
                   pos 'niwa-view-turn-role nil limit)
                  limit)))
      (when target
        (goto-char target))))
   (t
    (goto-char (point-min))
    (niwa-view-next-turn)))
  (when-let* ((window (get-buffer-window (current-buffer) t))
              (start (or (niwa-view--render-anchor-position
                          (plist-get state :window-start-anchor))
                         (plist-get state :window-start))))
    (set-window-start window (min start (point-max)) t)))

(defun niwa-view-render-project-segment (data-buffer state direction)
  "Render DATA-BUFFER and restore projection STATE for DIRECTION.
The segment owner holds projection ownership across source replacement."
  (niwa-view-render--full-now data-buffer t)
  (niwa-view-render--restore-segment-state state direction))


;;
;;; Optimistic user turn rendering

(defun niwa-view--insert-user-message
    (text &optional kind hook-context prompt-summary-body
          prompt-summary-source hook-audits attribution user-source)
  "Render TEXT as a user message in the history region.
Inserts at the history boundary with read-only protection.
KIND does not affect rendering.
HOOK-CONTEXT is model-visible hook context to summarize in the view.
PROMPT-SUMMARY-BODY, when non-nil, is shown as a collapsed Prompt
section backed by PROMPT-SUMMARY-SOURCE when available.  HOOK-AUDITS
is a list of hook audit records to render under the user turn.
ATTRIBUTION identifies a background event or a collaboration guest and any
attached shared context.
USER-SOURCE bounds the submitted prompt in the data buffer, including
trusted skill metadata used for immediate attachment disclosures.

Sets `niwa-view--user-pre-rendered' so the post-response render
path knows to skip the user turn it would otherwise extract for this
same exchange -- see `niwa-view-stream-render-response'.  Returns a
marker at the end of the inserted block."
  (ignore kind)
  (niwa-view--ensure-interactive-chat-view)
  (save-excursion
    (goto-char (niwa-view--history-insertion-marker))
    (niwa-view-render--with-boundaries-advancing
      (let ((inhibit-read-only t)
            (start (point))
            (fold-start nil)
            (guest-name (plist-get attribution :name))
            (background
             (eq (plist-get attribution :type) 'background-event))
            (shared-display (niwa-transcript-audit-shared-context
                             text (plist-get attribution :shared)))
            user-end)
        (unless background (setq niwa-view--prompt-headers nil))
        (insert (propertize (cond
                             (background "")
                             (guest-name (format "%s (guest)\n" guest-name))
                             (t "You\n"))
                            'font-lock-face
                            (if (or guest-name background)
                                'niwa-view-guest-header
                              'niwa-view-user-header)
                            'niwa-view-prompt-preview
                            (niwa-view--prompt-preview text shared-display)))
        (cond
         (background)
         (shared-display
          (setq fold-start
                (niwa-view--insert-shared-context shared-display user-source)))
         ((niwa-view--user-input-fold-p text)
          (setq fold-start (point))
          (niwa-view--insert-user-input-fold text))
         (t
          (insert (niwa-view--fontify-as text 'markdown-mode))
          (unless (eq (char-before) ?\n)
            (insert "\n"))))
        (setq user-end (point))
        (when-let* ((events (niwa-view--hook-context-events-from-text
                             hook-context)))
          (niwa-view--insert-hook-context-block events))
        (dolist (audit hook-audits)
          (niwa-view--insert-hook-audit-block
           audit
           (plist-get audit :source)))
        (let ((inline-skill
               (when (and user-source (buffer-live-p niwa--data-buffer))
                 (with-current-buffer niwa--data-buffer
                   (niwa-view--inline-skill-info
                    (niwa-transcript-segments (car user-source) (cdr user-source))
                    (current-buffer))))))
          (if inline-skill
              (niwa-view--insert-skill-disclosures inline-skill)
            (let ((prompt-body (and (stringp prompt-summary-body)
                                    (string-trim prompt-summary-body))))
              (when (and prompt-body (not (string-empty-p prompt-body)))
                (niwa-view--insert-rendered-tool
                 (list :header "Prompt" :body prompt-body
                       :body-mode 'markdown-mode :vtype 'prompt-summary
                       :initially-collapsed-p t)
                 prompt-summary-source)))))
        (insert (propertize "\n" 'font-lock-face 'niwa-view-separator))
        (add-text-properties start (point)
                             `(read-only t
                               keymap ,niwa-view--display-map
                               front-sticky (read-only keymap)
                               rear-nonsticky (read-only keymap)))
        (put-text-property start user-end 'niwa-view-type 'user)
        ;; The blanket `user' stamp above overwrites the fold's own
        ;; vtype, which is what its toggle dispatches on.
        (when fold-start
          (put-text-property fold-start user-end
                             'niwa-view-type 'user-input-summary))
        (niwa-view-pair-refresh-turn-rule)
        (setq niwa-view--user-pre-rendered t)
        (copy-marker (point) nil)))))


(provide 'niwa-view-render)
;;; niwa-view-render.el ends here
