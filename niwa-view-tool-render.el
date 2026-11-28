;;; niwa-view-tool-render.el --- Tool renderer interpreter for views -*- lexical-binding: t -*-

;;; Commentary:

;; Interprets the rendering plists that tool renderers return from the
;; `render-data' that `niwa-tool-render-data' encodes: parsing tool
;; segments, invoking renderers with a generic fallback, inserting tool
;; rows and nested child calls, and caching renderings per view.
;; `niwa-view-render' requires this module and assembles turns around the
;; rows it inserts; the interpreter calls back into that turn assembly for
;; pending rows and agent-block decoration, so it declares those functions
;; rather than requiring their module.

;;; Code:

(require 'niwa-tool-render-data)
(require 'niwa-view-fontify)

;; `ext:org'
(declare-function org-mode "ext:org" ())

;; `ext:org-src'
(declare-function org-unescape-code-in-string "ext:org-src" (s))

;; `niwa-execution-telemetry'
(declare-function niwa-execution-telemetry-sandbox-summary-class
                  "niwa-execution-telemetry" (summary))

;; `niwa-execution-transcript'
(declare-function niwa-execution-transcript-pending-render-data
                  "niwa-execution-transcript"
                  (data-buffer tool-use-id))

;; `niwa-gptel-retry'
(declare-function niwa-gptel-retry-available-p "niwa-gptel-retry" (buffer &optional failure-id))
(declare-function niwa-gptel-retry-failed-turn "niwa-gptel-retry" (buffer failure-id))
(autoload 'niwa-gptel-retry-available-p "niwa-gptel-retry")
(autoload 'niwa-gptel-retry-failed-turn "niwa-gptel-retry")

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-read-transcript-segment
                  "niwa-session-artifacts" (session descriptor))
(declare-function niwa-session-artifacts-transcript-segments
                  "niwa-session-artifacts"
                  (session live-buffer &optional agent-transcript-p))
(autoload 'niwa-session-artifacts-read-transcript-segment "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-transcript-segments "niwa-session-artifacts")

;; `niwa-tool-registry'
(declare-function niwa-tool-display-string "niwa-tool-registry"
                  (tool-name args &optional scope))
(declare-function niwa-tool-for-call "niwa-tool-registry" (name))
(declare-function niwa-tool-get "niwa-tool-registry"
		  (name &optional category))
(declare-function niwa-tool-name "niwa-tool-registry" (cl-x) t)
(declare-function niwa-tool-renderer "niwa-tool-registry" (cl-x)
                  t)
(defvar niwa-tool-render-summary-only)

;; `niwa-utilities'
(autoload 'niwa-diagnostic-format "niwa-utilities")

;; `niwa-view-agent'
(defvar niwa-view--agent-label-map)

;; `niwa-view-audit'
(autoload 'niwa-view--hook-audit-records-from-text "niwa-view-audit")
(autoload 'niwa-view--insert-hook-audit-block "niwa-view-audit")
(autoload 'niwa-view--merge-hook-audits "niwa-view-audit")
(autoload 'niwa-view-audit-insert-history-link "niwa-view-audit")
(autoload 'niwa-view-audit-show-control-result "niwa-view-audit")

;; `niwa-view-disclosure'
(declare-function niwa-view-disclosure--toggle-mailbox
                  "niwa-view-disclosure" ())
(declare-function niwa-view-disclosure-apply-rendering-state
                  "niwa-view-disclosure" (rendering source))
(declare-function niwa-view-disclosure-capture-state
                  "niwa-view-disclosure" (from to))
(declare-function niwa-view-disclosure-record-state-for-key
                  "niwa-view-disclosure" (key collapsed))
(declare-function niwa-view-disclosure-section-bounds
                  "niwa-view-disclosure" (&optional property))
(declare-function niwa-view-disclosure-source-range
                  "niwa-view-disclosure" (data-buffer start end))
(declare-function niwa-view-disclosure-state-for-key
                  "niwa-view-disclosure" (key))
(declare-function niwa-view-disclosure-state-key
                  "niwa-view-disclosure"
                  (source vtype &optional previous-key))

;; `niwa-view-mailbox'
(declare-function niwa-view--decorate-agent-message-blocks
                  "niwa-view-mailbox" (start end))
(declare-function niwa-view--decorate-agent-result-blocks
                  "niwa-view-mailbox" (start end))

;; `niwa-view-markdown'
(autoload 'niwa-view--decorate-markdown-in-range
  "niwa-view-markdown")

;; `niwa-view-prepare'
(declare-function niwa-view-prepare-get "niwa-view-prepare" (source start end key))
(defvar niwa-view-prepare-enabled)

;; `niwa-view-render'
(declare-function niwa-view--complete-wrapped-tool-text-p
                  "niwa-view-render" (raw))
(declare-function niwa-view--direct-tool-readable-text-p
                  "niwa-view-render" (raw))
(declare-function niwa-view--fit-group-header
                  "niwa-view-render" (header rendering))
(declare-function niwa-view--insert-pending-tool-row
                  "niwa-view-render" (entry))
(declare-function niwa-view--insert-summary-region
                  "niwa-view-render" (summary props))
(declare-function niwa-view--operation-line
                  "niwa-view-render" (marker marker-face label &optional detail metadata label-face))
(declare-function niwa-view--read-args-media-p "niwa-view-render" (args))
(declare-function niwa-view--request-summary-render-data-from-text
                  "niwa-view-render" (text))
(declare-function niwa-view--running-header
                  "niwa-view-render" (line count &optional virtual))
(declare-function niwa-view--source-position "niwa-view-render" (pos))
(declare-function niwa-view--strip-trailing-tool-marker
                  "niwa-view-render" (text))
(declare-function niwa-view--tool-block-bounds
                  "niwa-view-render" (seg-start seg-end))
(declare-function niwa-view--tool-call-line
                  "niwa-view-render" (marker marker-face name &optional primary-arg metadata name-face))
(declare-function niwa-view--tool-readable-start "niwa-view-render" (raw))
(declare-function niwa-view--tool-result-error-p
                  "niwa-view-render" (result-text))
(declare-function niwa-view--tool-segment-text
                  "niwa-view-render" (seg-start seg-end &optional raw))
(declare-function niwa-view--tool-wrapped-text-p "niwa-view-render" (raw))
(declare-function niwa-view-render-add-display-properties
                  "niwa-view-render" (start end &optional default-vtype))
(declare-function niwa-view-render-mutate
                  "niwa-view-render" (key function &optional replacement cleanup))
(defvar niwa-view--tool-rendering-cache)
(defvar niwa-view-mailbox-collapse-line-threshold)

;; `niwa-view-segments'
(declare-function niwa-view-segments-display-buffer
                  "niwa-view-segments" ())
(autoload 'niwa-view-segments-display-buffer "niwa-view-segments")

;; `niwa-view-stream'
(defvar niwa-view--execution-events)

;; `niwa-view-viewport'
(declare-function niwa-view--call-with-render-boundaries-advancing
                  "niwa-view-viewport" (thunk &optional types))
(declare-function niwa-view-call-preserving-user-view-state
                  "niwa-view-viewport" (thunk &optional region follow-output))
(declare-function niwa-view-transcript-tail "niwa-view-viewport" ())


;;
;;; Renderer plist interpreter

;; Tools can register a pure `renderer' function that consumes the `render-data'
;; side-channel attached to their result and returns a rendering plist of the
;; form:
;;
;;   (:header STRING            ; one-line collapsed summary
;;    :body STRING              ; full expanded body text
;;    :preview-body STRING      ; body shown when the row renders expanded
;;                              ; by default; explicit expansion shows :body
;;    :body-mode SYMBOL         ; major-mode symbol for fontification (or nil)
;;    :status SYMBOL            ; optional visual status, e.g. success/error
;;    :expandable-p BOOL        ; nil means render as a compact event line
;;    :hidden-p BOOL            ; non-nil means insert nothing
;;    :coalesce-key STRING      ; adjacent equal keys keep only the last row
;;    :child-calls LIST         ; nested calls rendered as their own rows
;;    :initially-collapsed-p BOOL)
;;
;; `:child-calls' belongs to compound tools that run other tools
;; (ToolCall).  Each entry is a plist (:id ID :tool NAME :args PLIST :result STRING :status SYM
;; :render-data DATA) and is rendered by that tool's own registered renderer,
;; so a nested Grep row gets Grep's header and `grep-mode' body.  Rows are
;; inserted only while the owning block is expanded, and each row is its own
;; collapsible section.
;;
;; The interpreter below parses the tool segment in the data buffer, invokes the
;; renderer (with a condition-case fallback to the generic renderer on error),
;; and inserts the rendered output. Expand and collapse re-invoke the renderer
;; on every transition so no state is cached in text properties.

(defvar niwa-view-render--parse-checkpoint nil
  "Optional callback between source-preparation stages.
Preparation owners may validate their source and yield here.  Ordinary callers
leave this nil.  Registered renderers are never invoked at these checkpoints.")

(defmacro niwa-view-render--parse-step (&rest body)
  "Evaluate BODY, then offer a source-preparation checkpoint."
  (declare (indent 0) (debug t))
  `(prog1 (progn ,@body)
     (when niwa-view-render--parse-checkpoint
       (funcall niwa-view-render--parse-checkpoint))))

(defun niwa-view--tool-call-parse (data-buf seg-start seg-end &optional raw)
  "Parse the tool segment in DATA-BUF between SEG-START and SEG-END.
Return a plist (:name NAME :args ARGS :result STRING :render-data DATA)
or nil when the segment is not a well-formed tool block.

When RAW is non-nil, use it as the already-expanded segment text.

Skips any leading `#+begin_tool …' / `#+end_reasoning' / blank-line
scaffolding before reading the call sexp -- gptel writes the open
tool marker on its own line with no `gptel' property, so a segment
whose start drifted onto the marker (boundary expansion, patched
render-data block) would otherwise fail to parse and force the
renderer to fall back to the bare `Tool' one-liner."
  (with-current-buffer data-buf
    (let* ((raw (or raw
                    (niwa-view--tool-segment-text seg-start seg-end)))
           (wrapped-p (niwa-view--tool-wrapped-text-p raw))
           (read-start (niwa-view--tool-readable-start raw))
           (tool-id
            (let ((pos seg-start)
                  found prop)
              (while (and (< pos seg-end) (not found))
                (setq prop (get-text-property pos 'gptel))
                (when (and (consp prop) (eq (car prop) 'tool))
                  (setq found (cdr prop)))
                (setq pos (or (next-single-property-change
                               pos 'gptel nil seg-end)
                              seg-end)))
              found)))
      (condition-case nil
          (let* ((parsed (read-from-string raw read-start))
                 (sexp (car parsed))
                 (name (plist-get sexp :name))
                 (args (plist-get sexp :args)))
            (when (stringp name)
              (let* ((full-result (niwa-view-render--parse-step
                                   (niwa--trim-tool-result (substring raw (cdr parsed)))))
                     (full-result
                      (if (and (derived-mode-p 'org-mode)
                               (fboundp 'org-unescape-code-in-string))
                          (niwa-view-render--parse-step
                            (org-unescape-code-in-string full-result))
                        full-result))
                     (hook-audits
                      (pcase-let* ((bounds
                                    (niwa-view--tool-block-bounds
                                     seg-start seg-end))
                                   (audit-start
                                    (min seg-start
                                         (or (car-safe bounds) seg-start)))
                                   (audit-end
                                    (max seg-end
                                         (or (cdr-safe bounds) seg-end))))
                        (save-excursion
                          (goto-char audit-start)
                          (when (search-forward niwa--hook-audit-open audit-end t)
                            (niwa-view--hook-audit-records-from-text
                             (buffer-substring audit-start audit-end)
                             nil data-buf audit-start)))))
                     (full-result
                      (niwa--strip-hook-audit-blocks full-result))
                     (extract (niwa-view-render--parse-step
                               (niwa-tool-render-data-extract
                               full-result
                               niwa--session
                               tool-id
                               (and (stringp tool-id)
                                    (not (string-empty-p tool-id))
                                    (equal name "Read")
                                    (niwa-view--read-args-media-p args)))))
                     (visible-result (car extract)))
                (list :name name
                      :tool-use-id tool-id
                      :args args
                      :result (if wrapped-p
                                  (niwa-view--strip-trailing-tool-marker
                                   visible-result)
                                visible-result)
                      :render-data (cdr extract)
                      :hook-audits hook-audits))))
        (error nil)))))

(defun niwa-view--rendering-plist-p (p)
  "Return non-nil when P is a structurally valid rendering plist.
Requires:
  `:header'               -- a string (required).
  `:body' (if present)    -- must be a string.
  `:preview-body' (if present) -- must be a string.
  `:body-mode' (if present) -- must be a symbol.
  `:status' (if present) -- must be a symbol.
  `:expandable-p' (if present) -- must be a boolean.
  `:hidden-p' (if present) -- must be a boolean.
  `:coalesce-key' (if present) -- must be a string.
  `:child-calls' (if present) -- must be a list of plists.
Malformed plists are rejected here so the interpreter never tries to
insert a non-string or `funcall' a non-symbol."
  (and (listp p)
       (stringp (plist-get p :header))
       (let ((body (plist-get p :body))
             (preview (plist-get p :preview-body))
             (mode (plist-get p :body-mode))
             (status (plist-get p :status))
             (expandable (plist-get p :expandable-p))
             (hidden (plist-get p :hidden-p))
             (children (plist-get p :child-calls))
             (coalesce-key (plist-get p :coalesce-key)))
         (and (or (null body) (stringp body))
              (or (null preview) (stringp preview))
              (or (null children)
                  (and (listp children)
                       (seq-every-p (lambda (child)
                                      (and (listp child)
                                           (keywordp (car-safe child))))
                                    children)))
              (or (null mode) (symbolp mode))
              (or (not (plist-member p :status)) (symbolp status))
              (or (not (plist-member p :expandable-p))
                  (memq expandable '(nil t)))
              (or (not (plist-member p :hidden-p))
                  (memq hidden '(nil t)))
              (or (not (plist-member p :coalesce-key))
                  (stringp coalesce-key))))))

(defun niwa-view--presentation-render-data (render-data)
  "Return RENDER-DATA with a successful observation's visual status.
The handler's error status is still sent to the model, including nested
ToolCall error values.  A collected process failure belongs to Bash, not
to the input or poll that successfully observed it."
  (if (and (memq (plist-get render-data :execution-control) '(input poll))
           (eq (plist-get render-data :control-succeeded-p) t))
      (plist-put (copy-sequence render-data) :status 'success)
    render-data))

(defun niwa-view--permission-approval-reason (render-data)
  "Return the bounded reviewer explanation in canonical RENDER-DATA.
This is display-only provenance for this invocation, not permission authority.
Child approvals belong to child render data, never the enclosing call."
  (let ((records (plist-get render-data :permission-approvals)) reasons)
    (when (proper-list-p records)
      (dolist (record records)
        (when (and (proper-list-p record)
                   (eq (plist-get record :source) 'reviewer)
                   (eq (plist-get record :outcome) 'allow-once)
                   (stringp (plist-get record :reason)))
          (push (substring-no-properties (plist-get record :reason)) reasons))))
    (when reasons
      (truncate-string-to-width
       (string-join (delete-dups (nreverse reasons)) "\n") 1024 nil nil "…"))))

(defun niwa-view--permission-approval-hint (reason)
  "Return a subdued approval hint with REASON as its tooltip, or nil."
  (when reason
    (propertize "Auto-approved · once"
                'font-lock-face 'niwa-view-tool-metadata
                'help-echo reason)))

(defun niwa-view--tool-render-status (result &optional render-data)
  "Return the renderer dispatch status for RESULT and RENDER-DATA."
  (setq render-data (niwa-view--presentation-render-data render-data))
  (or (pcase (plist-get render-data :status)
        ('success 'success)
        ((or 'error 'failed 'denied 'blocked) 'error))
      (and (niwa-view--tool-result-error-p result) 'error)
      'success))

(defun niwa-view--renderer-for-status (renderer status)
  "Return renderer function from RENDERER for STATUS, or nil.

RENDERER may be a function or an alist mapping status symbols to
functions.  Alist lookup tries STATUS first, then `default'."
  (cond
   ((functionp renderer) renderer)
   ((listp renderer)
    (let ((fn (or (alist-get status renderer nil nil #'eq)
                  (alist-get 'default renderer nil nil #'eq))))
      (and (functionp fn) fn)))))

(defun niwa-view--renderer-malformed-p (renderer status)
  "Return non-nil if RENDERER is present but unusable for STATUS."
  (cond
   ((null renderer) nil)
   ((functionp renderer) nil)
   ((listp renderer)
    (let ((entry (or (assoc status renderer)
                     (assoc 'default renderer))))
      (and entry (not (functionp (cdr entry))))))
   (t t)))

(defun niwa-view--invoke-renderer (tool render-data args result)
  "Invoke TOOL's renderer with NAME, ARGS, RESULT, and RENDER-DATA.
Return the rendering plist, or nil when no renderer is registered, the
renderer returns nil (opt-out), the renderer signals an error, or the
returned plist fails `niwa-view--rendering-plist-p'.  Errors and
malformed returns are surfaced once per tool via `niwa--warn-once';
callers treat a nil return as \"use the generic tool renderer\".

The renderer receives RENDER-DATA as-is (possibly nil): data-driven
renderers like the ApplyPatch summary can check for their kind
and opt out; output-driven renderers (Grep, Bash, Read, ...) work
straight off ARGS and RESULT without needing render-data."
  (if-let* ((child (niwa-tool-render-data-direct-call
                    (and tool (niwa-tool-name tool)) render-data)))
      (let* ((name (plist-get child :tool))
             (underlying (niwa-tool-for-call name))
             (data (plist-get child :render-data)))
        (or (niwa-view--invoke-renderer underlying data
                                           (plist-get child :args) result)
            (niwa-view--generic-tool-rendering
             name (plist-get child :args) result nil data)))
    (let* ((render-data (niwa-view--presentation-render-data render-data))
           (explicit-status
            (and (memq (plist-get render-data :status) '(success error))
		 (plist-get render-data :status)))
           (renderer (and tool (niwa-tool-renderer tool)))
           (status (niwa-view--tool-render-status result render-data))
           (fn (and renderer
                    (niwa-view--renderer-for-status renderer status))))
      (when renderer
	(let ((tool-label (or (and tool (niwa-tool-name tool)) "tool")))
          (cond
           ((not fn)
            (when (niwa-view--renderer-malformed-p renderer status)
              (niwa--warn-once
               (niwa-diagnostic-scope niwa--data-buffer)
               (list 'view-render-renderer-uncallable tool-label)
               "Renderer for %s is not callable for status %s"
               tool-label status))
            nil)
           (t
            (condition-case err
		(let ((plist (funcall fn tool-label args result render-data)))
                  (cond
                   ((null plist) nil)
                   ((niwa-view--rendering-plist-p plist)
                    (when-let* ((reason (niwa-view--permission-approval-reason
                                        render-data)))
                      (setq plist (plist-put (copy-sequence plist)
                                             :permission-approval-reason reason)))
                    (if (or (eq status 'error)
                            (and explicit-status
                                 (not (memq (plist-get plist :status)
                                            '(warning running cancelled)))))
			(plist-put (copy-sequence plist)
                                   :status status)
                      plist))
                   (t
                    (niwa--warn-once
                     (niwa-diagnostic-scope niwa--data-buffer)
                     (list 'view-render-renderer-malformed tool-label)
                     "Renderer for %s returned malformed plist: %S"
                     tool-label plist)
                    nil)))
              (error
               (niwa--warn-once
		(niwa-diagnostic-scope niwa--data-buffer)
		(list 'view-render-renderer-failed tool-label)
		"%s (Renderer for %s failed)"
		(error-message-string err) tool-label)
               nil)))))))))

(defun niwa-view--tool-result-line-count (result)
  "Return the number of non-empty lines in RESULT."
  (if (not (stringp result))
      0
    (let ((pos 0)
          (lines 0)
          (len (length result)))
      (while (< pos len)
        (let ((next (or (string-search "\n" result pos) len)))
          (unless (= pos next)
            (cl-incf lines))
          (setq pos (1+ next))))
      lines)))

(defun niwa-view--generic-tool-rendering
    (name args result &optional collapsed-only render-data)
  "Return a generic rendering plist for parsed tool NAME, ARGS, and RESULT.
This is used for tools without a custom renderer, including third-party
and MCP-style tools that are not registered in niwa's tool registry.
When COLLAPSED-ONLY is non-nil, omit the body from the returned plist.
RENDER-DATA may carry the pipeline's structured `:status'."
  (let* ((tool-name (or name "Tool"))
         (primary (and (listp args)
                       (condition-case nil
                           (niwa-tool-display-string
                            tool-name args (niwa-diagnostic-scope niwa--data-buffer))
                         (error nil))))
         (lines (niwa-view--tool-result-line-count result))
         (status (niwa-view--tool-render-status result render-data))
         (metadata (if (eq status 'error)
                       "error"
                     (format "%d %s" lines
                             (if (= lines 1) "line" "lines"))))
         (header (concat tool-name
                         (when (and primary
                                    (not (string-empty-p primary)))
                           (concat ": " primary))
                         (format " (%s)" metadata))))
    (list :header header
          :body (and (not collapsed-only) (stringp result) result)
          :body-mode nil
          :status status
          :permission-approval-reason
          (niwa-view--permission-approval-reason render-data)
          :initially-collapsed-p t)))

(defun niwa-view-data-buffer-major-mode ()
  "Return the major mode of the data buffer the view is attached to.

Use this from a tool renderer that wants to fontify its body in the
same flavor as the chat transcript.  Niwa data buffers are
`org-mode' for gptel state and tool-result storage, while assistant
responses are stored as raw Markdown and converted only by the view's
response renderer.

Returns nil (verbatim) when no data buffer is attached, so
`niwa-view--fontify-as' inserts the text without activating a mode."
  (when-let* ((data-buffer (niwa-view-segments-display-buffer))
              ((buffer-live-p data-buffer)))
    (buffer-local-value 'major-mode data-buffer)))

(defun niwa-view--queue-origin-fingerprint (queue)
  "Return the ORIGIN values in QUEUE for render cache invalidation."
  (mapcar (lambda (entry)
            (plist-get entry :origin))
          queue))

(defun niwa-view--session-render-state-fingerprint (session)
  "Return state from SESSION that can affect cached tool renderings.

Agent registry state is deliberately absent: agent handle status
reaches a rendering only through render-data blocks patched into the
transcript text, which the content term of the cache key already
invalidates, while registry activity changes on every agent tick and
would defeat the cache exactly when it matters.  New live-state
dependencies must either ride a text patch or clear
`niwa-view--tool-rendering-cache' at the mutation point."
  (when session
    (list :permission-origins
          (niwa-view--queue-origin-fingerprint
           (niwa-session-permission-queue session))
          :plan-pending
          (and (niwa-session-pending-plan-approval session) t))))

(defun niwa-view--stamp-agent-handle (start end rendering)
  "Stamp START..END with handle properties from RENDERING."
  (when-let* ((agent-path (plist-get rendering :agent-path)))
    (add-text-properties
     start end
     `(niwa-view-agent-path ,agent-path
       niwa-view-agent-handle-p t
       niwa-view-agent-status ,(plist-get rendering :agent-status)))))

(defun niwa-view--rendering-header-face (rendering)
  "Return the face for RENDERING's visible header line."
  (cond
   ((eq (plist-get rendering :vtype) 'request-failure)
    'niwa-view-handle-error)
   ((and (eq (or (plist-get rendering :vtype) 'tool-summary)
             'agent-handle)
         (eq (plist-get rendering :agent-status) 'running))
    'niwa-view-agent-running)
   (t 'niwa-view-tool-summary)))

(defun niwa-view--buttonize-agent-header-label (line agent-path)
  "Return LINE with its visible AGENT-PATH made clickable.
AGENT-PATH is stored on the label so it opens the retained transcript."
  (when (and agent-path
             (string-match (regexp-quote agent-path) line))
    (add-text-properties
     (match-beginning 0) (match-end 0)
     `(font-lock-face link
       keymap ,niwa-view--agent-label-map
       mouse-face highlight
       follow-link t
       help-echo "Open agent transcript"
       niwa-view-agent-path ,agent-path)
     line))
  line)

(defun niwa-view--split-rendering-tool-header (header)
  "Split renderer HEADER into (NAME ARG METADATA).
Return nil when HEADER is not a `Tool: argument' style line."
  (when (string-match "\\`\\([^:\n]+\\):[ \t]*\\(.+\\)\\'" header)
    (let ((name (match-string 1 header))
          (arg (match-string 2 header))
          metadata)
      (when (string-match
             "\\`\\(.*\\)[ \t]+\\(([^()\n]+)\\)\\'"
             arg)
        (setq metadata (match-string 2 arg))
        (setq arg (match-string 1 arg)))
      (list name arg metadata))))

(defun niwa-view--rendering-status (rendering &optional render-data)
  "Return RENDERING's visual status, with errors preceding warnings.
RENDER-DATA supplies child status and sandbox facts before attachment.
Execution outcome remains independent of lifecycle and disclosure state."
  (let ((status (plist-get rendering :status))
        (render-data (niwa-view--presentation-render-data render-data)))
    (cond
     ((or (memq status '(error failed denied blocked))
          (memq (plist-get render-data :status) '(error failed denied blocked)))
      'error)
     ((memq status '(running cancelled)) status)
     ((memq (plist-get render-data :status) '(warning running cancelled))
      (plist-get render-data :status))
     ((or (eq status 'warning)
          (eq (niwa-execution-telemetry-sandbox-summary-class
               (or (plist-get rendering :sandbox-summary)
                   (plist-get render-data :sandbox-summary)))
              'warning))
      'warning)
     (t status))))

(defun niwa-view--rendering-header-line (rendering)
  "Return the propertized collapsed header line for RENDERING."
  (let* ((vtype (or (plist-get rendering :vtype) 'tool-summary))
         (group-p (eq vtype 'tool-group))
         (header (if group-p
                     (niwa-view--fit-group-header
                      (or (plist-get rendering :header) "Tool") rendering)
                   (or (plist-get rendering :header) "Tool")))
         (status (plist-get rendering :agent-status))
         (tool-status (niwa-view--rendering-status rendering))
         (agent-p (eq vtype 'agent-handle))
         (prompt-p (eq vtype 'prompt-summary))
         (marker (cond
                  ((eq vtype 'request-failure) "✗")
                  ((eq vtype 'tool-preparing) "…")
                  (prompt-p "◆")
                  ((eq tool-status 'error) "×")
                  ((eq tool-status 'cancelled) "●")
                  ((and (not agent-p) (eq tool-status 'running)) "●")
                  ((and agent-p (eq status 'running)) "●")
                  ((and agent-p (memq status '(blocked waiting))) "!")
                  ((and agent-p (memq status '(error failed))) "×")
                  ((and agent-p (eq status 'aborted)) "✗")
                  ((and agent-p (memq status '(incomplete nil))) "…")
                  ((and agent-p (eq status 'completed)) "✓")
                  (agent-p "›")
                  ((eq tool-status 'warning) "!")
                  (t "✓")))
         (marker-face (cond
                       ((eq tool-status 'cancelled)
                        'niwa-view-tool-metadata)
                       ((equal marker "×") 'error)
                       ((member marker '("!" "✗"))
                        'niwa-view-tool-warning)
                       ((string= marker "●")
                        'niwa-view-agent-running)
                       ((member marker '("…" "›"))
                        'niwa-view-tool-metadata)
                       (prompt-p
                        'niwa-view-response-marker)
                       (t 'niwa-view-tool-marker)))
         (tool-split
          (and (not agent-p)
               (not group-p)
               (niwa-view--split-rendering-tool-header header))))
    (if (memq vtype '(system-reminder-summary thinking-summary compaction-summary))
        header
      (let ((line
             (cond
              (group-p
               (niwa-view--group-header-text
                marker marker-face header (plist-get rendering :unconfined)))
              (tool-split
               (niwa-view--tool-call-line
                marker marker-face
                (nth 0 tool-split)
                (nth 1 tool-split)
                (nth 2 tool-split)))
              (t
               (niwa-view--operation-line
                marker marker-face header nil nil
                (niwa-view--rendering-header-face rendering))))))
        (when-let* ((agent-path (plist-get rendering :agent-path)))
          (setq line (niwa-view--buttonize-agent-header-label
                      line agent-path)))
        (when-let* ((id (plist-get rendering :failure-id))
                    (data (plist-get rendering :failure-buffer))
                    ((niwa-gptel-retry-available-p data id)))
          (setq line
                (concat line "  "
                        (propertize
                         "[Retry]" 'font-lock-face 'link 'mouse-face 'highlight
                         'help-echo (if (plist-get rendering :external-history)
                                        "Retry from before the failed prompt (older sessions reuse native history); file effects are not undone"
                                      "Replay the prepared request without repeating completed tools")
                         'niwa-view-zone-activate
                         (lambda ()
                           (niwa-with-diagnostic-errors (niwa-diagnostic-scope data)
                             (niwa-gptel-retry-failed-turn data id)))))))
        (when-let* ((id (plist-get rendering :execution-control-id)))
          (setq line
                (concat line "  "
                        (propertize
                         "[Show result]"
                         'font-lock-face 'link
                         'mouse-face 'highlight
                         'help-echo "RET: open the controlled execution"
                         'niwa-view-zone-activate
                         (lambda ()
                           (niwa-view-audit-show-control-result id))))))
        (when-let* ((hint (niwa-view--permission-approval-hint
                          (plist-get rendering :permission-approval-reason))))
          (setq line (concat line "  " hint)))
        (let* ((count (plist-get rendering :pending-count))
               (pending-failed (plist-get rendering :pending-failed)))
          (when (or count pending-failed)
            (setq line (niwa-view--running-header
                        line count (plist-get rendering :pending-virtual))))
          ;; The disclosure marker follows the running count, so stopping a
          ;; turn removes the count without moving the marker's text.
          (if group-p
              (concat line " "
                      (propertize
                       (if (plist-get rendering :disclosure-open) "▾" "▸")
                       'font-lock-face 'niwa-view-tool-metadata))
            line))))))

(defun niwa-view--group-header-text (marker marker-face summary unconfined)
  "Return an activity group's header from MARKER, MARKER-FACE and SUMMARY.
SUMMARY comes from `niwa-activity-group-partition': its call count
takes the tool-name face, like the `Name:' of a tool row, and its failure
counts take the error face.  UNCONFINED adds `· unconfined' once for the
members whose own rows say they ran without confinement."
  (let ((text (propertize summary 'font-lock-face 'niwa-view-tool-summary))
        (pos 0))
    (while (< pos (length text))
      (let ((part (get-text-property pos 'niwa-activity-group-part text))
            (next (next-single-property-change
                   pos 'niwa-activity-group-part text (length text))))
        (when part
          (put-text-property pos next 'font-lock-face
                             (pcase part
                               ('count '(niwa-view-tool-name bold))
                               ('failure 'error)
                               (_ 'niwa-view-tool-metadata))
                             text))
        (setq pos next)))
    (concat (propertize (concat "  " marker " ") 'font-lock-face marker-face)
            text
            (when unconfined
              (propertize " · unconfined"
                          'font-lock-face 'niwa-view-tool-metadata)))))

(defun niwa-view--sandbox-summary-line (summary)
  "Return the durable disclosure line for material sandbox SUMMARY.
A `warning' class states that something the model asked for did not run.
A `note' class records the boundary the call actually ran with, which is
the boundary the session was configured or granted -- nothing went wrong,
so it takes the quiet marker and metadata face instead of shouting on
every row."
  (when-let* ((class
               (and summary
                    (niwa-execution-telemetry-sandbox-summary-class
                     summary)))
              (face (if (eq class 'note)
                        'niwa-view-tool-metadata
                      'niwa-view-tool-warning)))
    (let* ((notep (eq class 'note))
           (attempts (or (plist-get summary :attempt-count) 0))
           (started (or (plist-get summary :started-count) 0))
           (refused (or (plist-get summary :refused-count) 0))
           (writes (or (plist-get summary :additional-write-count) 0))
           (sandbox (plist-get summary :sandbox))
           (filesystem (plist-get summary :filesystem))
           (network (plist-get summary :network))
           (proc (plist-get summary :proc))
           (all-refused (and (> attempts 0)
                             (= refused attempts)
                             (zerop started)))
           details)
      (if all-refused
          (setq details '("execution refused" "no child started"))
        (pcase sandbox
          ('escalated (push "full execution access" details))
          ('off (push "sandbox disabled · ran without confinement" details))
          ('unavailable
           (push "sandbox unavailable · ran without confinement" details))
          ('refused (push "execution refused" details))
          ((pred (lambda (value)
                   (and value (not (eq value 'bubblewrap)))))
           (push (format "sandbox %s" sandbox) details)))
        (unless (memq sandbox '(escalated off unavailable))
          (when (and filesystem (not (eq filesystem 'workspace-write)))
            (push (pcase filesystem
                    ('unrestricted "unrestricted filesystem access")
                    ('unavailable "filesystem confinement unavailable")
                    (_ (format "filesystem %s" filesystem)))
                  details))
          (when (and network (not (eq network 'isolated)))
            (push (pcase network
                    ('unrestricted "network access allowed")
                    ('unavailable "network confinement unavailable")
                    (_ (format "network %s" network)))
                  details))
          (when (eq proc 'host)
            (push "host /proc access" details))
          (when (> writes 0)
            (push "additional filesystem write access" details)))
        (when (< started attempts)
          (push (format "%d %s did not start"
                        (- attempts started)
                        (if (= (- attempts started) 1)
                            "child"
                          "children"))
                details))
        (setq details (nreverse details)))
      (let ((line (niwa-view--operation-line
                   (if notep "\u25c7" "!")
                   face
                   "Sandbox:"
                   (string-join details " · ")
                   nil
                   'niwa-view-tool-metadata)))
        ;; A note annotates the call above it and must not read louder
        ;; than that call; a warning keeps its full-contrast detail.
        (when notep
          (put-text-property 0 (length line)
                             'font-lock-face 'niwa-view-tool-metadata line))
        line))))

(defun niwa-view--sandbox-unconfined-p (summary)
  "Return non-nil when sandbox SUMMARY says the call ran without confinement.
These are the rows whose disclosure reads `ran without confinement'; an
activity group shows the fact once on its header."
  (and summary
       (memq (plist-get summary :sandbox) '(off unavailable))
       (> (or (plist-get summary :started-count) 0) 0)))

(defun niwa-view--rendering-header-block (rendering)
  "Return RENDERING's header plus any durable sandbox disclosure."
  (let ((header (niwa-view--rendering-header-line rendering))
        (sandbox
         (niwa-view--sandbox-summary-line
          (plist-get rendering :sandbox-summary))))
    (let ((block (if sandbox (concat header "\n" sandbox) header)))
      ;; A collapsed group still projects the completions folded into it.
      (if-let* ((records (plist-get rendering :execution-completions)))
          (propertize block 'niwa-view-execution-completions records)
        block))))

(defvar niwa-view--rendering-indent ""
  "Line prefix applied to the rendering currently being inserted.
Bound to a non-empty string while a nested call row is inserted so its
header and body sit one level in from the block that ran it.")

(defun niwa-view--rendering-indent-blank ()
  "Return the current rendering indent as plain spaces.
A batch glyph belongs on the first line of a row only, so wrapped text
and body lines are inset by the same width without repeating it."
  (make-string (string-width niwa-view--rendering-indent) ?\s))

(defun niwa-view--apply-rendering-indent (start end)
  "Indent START..END by `niwa-view--rendering-indent' when it is set."
  (unless (string-empty-p niwa-view--rendering-indent)
    (add-text-properties
     start end
     `(line-prefix ,niwa-view--rendering-indent
       wrap-prefix ,(niwa-view--rendering-indent-blank)
       rear-nonsticky (line-prefix wrap-prefix)))))

(defun niwa-view--render-collapsed-header (rendering source)
  "Insert the collapsed header for RENDERING with SOURCE coordinates.
RENDERING is a rendering plist.  SOURCE is (DATA-START . DATA-END)."
  (let* ((vtype (or (plist-get rendering :vtype) 'tool-summary))
         (ins-start (point)))
    (niwa-view--insert-summary-region
     (niwa-view--rendering-header-block rendering)
     `(niwa-view-type ,vtype
       niwa-view-collapsed t
       niwa-view-source ,source
       niwa-view-source-key ,(niwa-view-disclosure-state-key
                                 source vtype)
       niwa-view-tool-use-id ,(plist-get rendering :tool-use-id)
       niwa-view-rendered t))
    (when (eq vtype 'agent-handle)
      (niwa-view--stamp-agent-handle ins-start (point) rendering))
    ;; A group header is generated from counts and tool names, so it has no
    ;; Markdown, links or paths to decorate.
    (unless (eq vtype 'tool-group)
      (niwa-view--decorate-markdown-in-range ins-start (point)))
    (niwa-view--apply-rendering-indent ins-start (point))))

(defun niwa-view--render-expanded-body (rendering source)
  "Insert the expanded body for RENDERING with SOURCE coordinates."
  (let* ((body (or (plist-get rendering :body) ""))
         (body-mode (plist-get rendering :body-mode))
         (vtype (or (plist-get rendering :vtype) 'tool-summary))
         (fontified (niwa-view--fontify-as body body-mode))
         (header-line (niwa-view--rendering-header-block
                       ;; Only an expanded group header points its marker down.
                       (if (eq vtype 'tool-group)
                           (plist-put (copy-sequence rendering) :disclosure-open t)
                         rendering)))
         (ins-start (point))
         body-start)
    (insert header-line "\n")
    (when (eq vtype 'agent-handle)
      (niwa-view--stamp-agent-handle ins-start (point) rendering))
    (setq body-start (copy-marker (point)))
    (insert fontified)
    (when (memq vtype '(tool-summary tool-child))
      (niwa-view-audit-insert-history-link rendering))
    (unless (eq (char-before) ?\n)
      (insert "\n"))
    (add-text-properties ins-start (point)
                         `(niwa-view-type ,vtype
                           niwa-view-collapsed nil
                           niwa-view-source ,source
                           niwa-view-source-key ,(niwa-view-disclosure-state-key
                                                     source vtype)
                           niwa-view-tool-use-id
                           ,(plist-get rendering :tool-use-id)
                           niwa-view-force-expanded
                           ,(and (plist-get rendering :force-expanded-p) t)
                           niwa-view-rendered t))
    ;; Stamp the body inset before decoration so a rendered table
    ;; subtracts it from its usable width, then again afterwards so
    ;; decoration-inserted text (panel labels, padding) is indented too.
    (let ((inset (concat (niwa-view--rendering-indent-blank) "    ")))
      (add-text-properties body-start (point)
                           `(line-prefix ,inset
                             wrap-prefix ,inset
                             rear-nonsticky (line-prefix wrap-prefix)))
      ;; Thinking uses the same fontification-only body as an explicit
      ;; disclosure expansion, preserving the exact reasoning payload.
      ;; A group's own rows follow and decorate themselves.
      (unless (memq vtype '(thinking-summary tool-group))
        (niwa-view--decorate-markdown-in-range ins-start (point)))
      (add-text-properties body-start (point)
                           `(line-prefix ,inset
                             wrap-prefix ,inset
                             rear-nonsticky (line-prefix wrap-prefix))))
    (niwa-view--apply-rendering-indent ins-start body-start)
    (set-marker body-start nil)
    ;; Nested call rows follow the body so a compound tool's children are
    ;; visible exactly while the block that ran them is expanded.
    (niwa-view--insert-child-calls rendering source)))

(defun niwa-view--insert-rendered-tool (rendering source)
  "Insert a rendered tool block honouring RENDERING's initial state.
SOURCE is (DATA-START . DATA-END) identifying the data-buffer segment.
When `:initially-collapsed-p' is nil the body is inserted expanded;
otherwise only the header is shown.

When RENDERING carries `:expandable-p' nil, insert a compact event line
with no source coordinates so expand/collapse commands cannot reveal
the raw tool segment.  When `:hidden-p' is non-nil, insert nothing."
  (unless (plist-get rendering :hidden-p)
    (let ((hook-audits (plist-get rendering :hook-audits)))
      (setq rendering
            (niwa-view-disclosure-apply-rendering-state rendering source))
      (if (and (plist-member rendering :expandable-p)
               (not (plist-get rendering :expandable-p)))
          (let ((ins-start (point)))
            (niwa-view--insert-summary-region
             (niwa-view--rendering-header-block
              (plist-put (copy-sequence rendering) :vtype 'tool-event))
             '(niwa-view-type tool-event
               niwa-view-rendered t))
            (niwa-view-render-add-display-properties
             ins-start (point) 'tool-event)
            (niwa-view--decorate-markdown-in-range ins-start (point)))
        (if (plist-member rendering :initially-collapsed-p)
            (if (plist-get rendering :initially-collapsed-p)
                (niwa-view--render-collapsed-header rendering source)
              ;; `:preview-body' replaces `:body' for this initial
              ;; expansion only.  A later explicit expansion goes
              ;; through the disclosure path, which re-invokes the
              ;; renderer and inserts the complete `:body'.
              (niwa-view--render-expanded-body
               (if-let* ((preview (plist-get rendering :preview-body)))
                   (plist-put (copy-sequence rendering) :body preview)
                 rendering)
               source))
          ;; Default: collapsed.
          (niwa-view--render-collapsed-header rendering source)))
      (when hook-audits
        (let ((audit-start (point)))
          (dolist (audit hook-audits)
            (niwa-view--insert-hook-audit-block
             audit (or (plist-get audit :source) source)))
          (niwa-view-render-add-display-properties
           audit-start (point) 'hook-audit))))))

(defconst niwa-view--child-call-indent "  "
  "Extra line prefix that sets a nested call row in from its own block.")

(defvar niwa-view--child-call-depth 0
  "Nesting depth of the child call currently being rendered.")

(defun niwa-view--child-call-rendering (child)
  "Return the rendering plist for nested call CHILD, or nil.

CHILD is one `:child-calls' entry.  The nested tool's own registered
renderer produces the row, which is why a nested Grep gets Grep's header
and `grep-mode' body without the compound tool formatting anything
itself.  Mailbox and completion children carry their prepared rendering.
Tool rows start collapsed, including failures.

A nested compound call keeps its own `:child-calls', so expanding it
inside a block or an activity group shows the calls it ran rather than
the text it returned.  A row's sandbox disclosure travels with it, so
folding a run into a group does not lose the boundary it ran with."
  (let* ((name (plist-get child :tool))
         (args (plist-get child :args))
         (data (niwa-view-segments-display-buffer))
         (direct (niwa-tool-render-data-direct-call
                  name (plist-get child :render-data)))
         (child-id (or (and (member name '("Bash" "Monitor"))
                            (stringp (plist-get child :id))
                            (plist-get child :id))
                       (and (member (plist-get direct :tool) '("Bash" "Monitor"))
                            (stringp (plist-get direct :id))
                            (plist-get direct :id))))
         (terminal (and child-id
                        (niwa-view--execution-terminal-render-data
                         data child-id
                         (plist-get (or (plist-get direct :render-data)
                                        (plist-get child :render-data))
                                    :execution-id))))
         (event (and (not terminal) child-id
                     (bound-and-true-p niwa-view--execution-events)
                     (hash-table-p niwa-view--execution-events)
                     (gethash child-id niwa-view--execution-events)))
         (progress (and (eq (plist-get event :type) 'progress)
                        (append (copy-sequence (plist-get event :facts))
                                (list :permission-approvals
                                      (plist-get event :permission-approvals)
                                      :status 'success :live-execution-p t
                                      :output-preview-truncated-p
                                      (plist-get event :output-preview-truncated-p)))))
         (latest (or terminal progress))
         (result (cond
                  ((plist-member latest :execution-output)
                   (or (plist-get latest :execution-output) ""))
                  (progress (or (plist-get event :output-tail) ""))
                  ((plist-member (plist-get direct :render-data)
                                 :execution-output)
                   (or (plist-get (plist-get direct :render-data)
                                  :execution-output) ""))
                  ((and (member name '("Bash" "Monitor"))
                        (plist-member (plist-get child :render-data)
                                      :execution-output))
                   (or (plist-get (plist-get child :render-data)
                                  :execution-output) ""))
                  (t (plist-get child :result))))
         (child-status (plist-get child :status))
         (render-data
          (cond ((and latest direct)
                 (plist-put
                  (copy-sequence (plist-get child :render-data)) :calls
                  (list (plist-put
                         (plist-put (copy-sequence direct)
                                    :render-data latest)
                         :status (plist-get latest :status)))))
                (latest latest)
                ((and (not (memq (plist-get (plist-get child :render-data)
                                            :state)
                                 '(completed interrupted lost archived)))
                      (memq child-status '(success error failed denied blocked)))
                 (plist-put (copy-sequence (plist-get child :render-data)) :status
                            (if (eq child-status 'success) 'success 'error)))
                (t (plist-get child :render-data))))
         (tool (and (stringp name) (niwa-tool-get name)))
         (rendering
          (if (memq (plist-get child :kind) '(mailbox completion))
              (plist-get child :rendering)
            (or (and tool
                     (niwa-view--invoke-renderer tool render-data args result))
                (niwa-view--generic-tool-rendering
                 name args result nil render-data)))))
    (when rendering
      (setq rendering (copy-sequence rendering))
      (dolist (cell (list (cons :vtype 'tool-child)
                          (cons :expandable-p t)
                          (cons :coalesce-key nil)
                          (cons :hook-audits nil)
                          (cons :force-expanded-p nil)
                          (cons :initially-collapsed-p t)))
        (setq rendering (plist-put rendering (car cell) (cdr cell))))
      (when child-id
        (setq rendering (plist-put rendering :tool-use-id child-id))
        (setq rendering (plist-put rendering :execution-id
                                  (or (plist-get latest :execution-id)
                                      (plist-get render-data :execution-id)
                                      (plist-get (plist-get direct :render-data)
                                                 :execution-id)))))
      (when (and (not latest) (memq child-status '(warning running cancelled))
                 (not (eq (niwa-view--rendering-status rendering) 'error)))
        (setq rendering (plist-put rendering :status child-status)))
      (when-let* ((summary
                   (or (plist-get render-data :sandbox-summary)
                       (plist-get (plist-get direct :render-data)
                                  :sandbox-summary)
                       (plist-get (plist-get child :render-data)
                                  :sandbox-summary))))
        (setq rendering
              (plist-put rendering :sandbox-summary (copy-tree summary))))
      (when-let* ((media (plist-get child :media)))
        (setq rendering
              (plist-put rendering :body
                         (concat (or (plist-get rendering :body) "")
                                 (format "\nMedia: %S" media)))))
      rendering)))

(defun niwa-view--child-call-state-key (child source)
  "Return the disclosure key for CHILD's row under SOURCE, or nil.
Grouped transcript rows keep their standalone identity.  Compound-tool
children use their id to discriminate one block's shared coordinates."
  (if-let* ((own-source (plist-get child :source)))
      (niwa-view-disclosure-state-key
       own-source (if (eq (plist-get child :kind) 'mailbox)
                      'tool-child
                    'tool-summary))
    (when-let* ((base (niwa-view-disclosure-state-key source 'tool-child))
                (id (plist-get child :id)))
      (append base (list id)))))

(defun niwa-view--child-call-prefixes (children)
  "Return one line prefix per entry in CHILDREN, or nil.

A `:batch' value marks the calls of one concurrent join.  When some call
ran in a batch every row gets a two-column glyph gutter so the join is
visible as a group and every marker still lines up; a block that ran
everything in sequence pays no gutter at all."
  (let ((batches (mapcar (lambda (child) (plist-get child :batch)) children))
        previous)
    (when (cl-some #'identity batches)
      (cl-loop
       for (batch next) on batches
       collect
       (concat
        niwa-view--child-call-indent
        (if (null batch)
            "  "
          (let ((first (not (equal batch previous)))
                (last (not (equal batch next))))
            (concat (cond ((and first last) "\u2500")
                          (first "\u250c")
                          (last "\u2514")
                          (t "\u251c"))
                    " "))))
       do (setq previous batch)))))

(defun niwa-view--insert-child-call-block
    (child source &optional collapsed indent)
  "Insert the nested call row for CHILD under SOURCE.

COLLAPSED is `derive' to use the remembered state, or a boolean to force
one.  INDENT overrides the row's line prefix, which is how a concurrent
batch keeps its glyph gutter across a toggle.  The row gets its own copy
of SOURCE's coordinates because section bounds compare source identity
with `eq', which is what separates one row from the next and from the
body of the block that ran them."
  (if (eq (plist-get child :kind) 'pending)
      ;; An in-flight call has no source or result: it is a running row
      ;; with no disclosure of its own.
      (let ((niwa-view--rendering-indent
             (concat niwa-view--rendering-indent
                     (or indent niwa-view--child-call-indent))))
        (niwa-view--insert-pending-tool-row (plist-get child :entry)))
    (niwa-view--insert-settled-child-call-block
     child source collapsed indent)))

(defun niwa-view--insert-settled-child-call-block
    (child source collapsed indent)
  "Insert CHILD's nested row under SOURCE; see the caller for COLLAPSED, INDENT."
  (when-let* ((rendering (niwa-view--child-call-rendering child))
              ((not (plist-get rendering :hidden-p)))
              (source (or (plist-get child :source) source))
              (source (and (consp source) (cons (car source) (cdr source)))))
    (let* ((key (niwa-view--child-call-state-key child source))
           (remembered (and (eq collapsed 'derive)
                            (niwa-view-disclosure-state-for-key key)))
           (mailbox-default-p
            (and (plist-get rendering :mailbox-text)
                 (eq collapsed 'derive) (not remembered)))
           (collapsed (cond
                       (remembered (cdr remembered))
                       ((eq collapsed 'derive)
                        (plist-get rendering :initially-collapsed-p))
                       (t collapsed)))
           (indent (or indent niwa-view--child-call-indent))
           (niwa-view--rendering-indent
            (concat niwa-view--rendering-indent indent))
           (niwa-view--child-call-depth (1+ niwa-view--child-call-depth))
           (start (point)))
      (cond
       ((plist-get rendering :completion-record)
        (let ((record (plist-get rendering :completion-record)))
          (niwa-view--insert-hook-audit-block record source)
          (add-text-properties
           start (point)
           `(niwa-view-type tool-child
                               niwa-view-source ,source
                               niwa-view-collapsed ,collapsed))
          (niwa-view--apply-rendering-indent start (point))))
       ((plist-get rendering :mailbox-text)
          (let ((text (plist-get rendering :mailbox-text))
                (niwa-view-mailbox-collapse-line-threshold
                 (cond (mailbox-default-p
                        niwa-view-mailbox-collapse-line-threshold)
                       (collapsed 0)
                       (t most-positive-fixnum))))
            (insert text "\n")
            (niwa-view--decorate-agent-result-blocks start (point))
            (niwa-view--decorate-agent-message-blocks start (point))
            ;; Notices default to collapsed independently of the line threshold.
            ;; An explicit or remembered expansion must override that default.
            (when (and (not mailbox-default-p) (not collapsed)
                       (get-text-property start 'niwa-view-collapsed))
              (save-excursion
                (goto-char start)
                (niwa-view-disclosure--toggle-mailbox)))
            (when mailbox-default-p
              (setq collapsed
                    (get-text-property
                     (or (text-property-any start (point)
                                            'niwa-view-type 'mailbox-delivery)
                         start)
                     'niwa-view-collapsed)))
            (add-text-properties
             start (point)
             `(niwa-view-type tool-child
                                 niwa-view-source ,source
                                 niwa-view-collapsed ,collapsed))))
       (collapsed
        (niwa-view--render-collapsed-header rendering source))
       (t
        (niwa-view--render-expanded-body rendering source)))
      ;; Descendants have their own source identity and disclosure state.
      ;; Stamp only this row's body, even when it contains a compound call.
      ;; Retain the full prefix so a row-local toggle keeps its parent inset.
      (add-text-properties
       start (next-single-property-change start 'niwa-view-source nil (point))
       `(niwa-view-tool-child ,child
         niwa-view-child-indent ,niwa-view--rendering-indent
         niwa-view-child-depth ,niwa-view--child-call-depth
         niwa-view-source-key ,key))
      (niwa-view-render-add-display-properties start (point) 'tool-child)
      (niwa-view-disclosure-record-state-for-key key collapsed))))

(defun niwa-view--insert-child-calls (rendering source)
  "Insert one row per nested call in RENDERING under SOURCE."
  (let* ((children (plist-get rendering :child-calls))
         (prefixes (niwa-view--child-call-prefixes children)))
    (dolist (child children)
      (niwa-view--insert-child-call-block
       child source 'derive (pop prefixes)))))

(defun niwa-view-render-child-calls-end (start limit source &optional parent-depth)
  "Return the end of the nested call rows that begin at START, before LIMIT.
Return START when no owned row begins there.  Only rows contained in SOURCE
belong to the section: an independent mailbox delivery can also have the
`tool-child' type.  With PARENT-DEPTH, stop before any row at that depth or
shallower, preserving siblings of a nested compound call."
  (let ((pos start))
    (while (and (< pos limit)
                (eq (get-text-property pos 'niwa-view-type) 'tool-child)
                (let ((child-source (get-text-property pos 'niwa-view-source)))
                  (and (consp child-source)
                       (<= (car source) (car child-source))
                       (<= (cdr child-source) (cdr source))))
                (or (null parent-depth)
                    (> (get-text-property pos 'niwa-view-child-depth)
                       parent-depth)))
      (setq pos (or (next-single-property-change
                     pos 'niwa-view-source nil limit)
                    limit)))
    pos))

(defun niwa-view-render-section-body-end (start limit)
  "Return the end of the section body in START..LIMIT.
Stops before any nested call rows so re-stamping a re-rendered section
does not overwrite the identity of the rows it owns."
  (let ((pos start))
    (while (and (< pos limit)
                (not (eq (get-text-property pos 'niwa-view-type)
                         'tool-child)))
      (setq pos (or (next-single-property-change
                     pos 'niwa-view-type nil limit)
                    limit)))
    pos))

(defun niwa-view-render-toggle-child-call ()
  "Toggle the nested call row at point."
  (let* ((bounds (niwa-view-disclosure-section-bounds))
         (start (car-safe bounds))
         (child (and start (get-text-property start 'niwa-view-tool-child)))
         (source (and start (get-text-property start 'niwa-view-source)))
         (collapsed (and start (get-text-property start 'niwa-view-collapsed)))
         (indent (and start (get-text-property start 'niwa-view-child-indent)))
         (depth (and start (get-text-property start 'niwa-view-child-depth)))
         (turn-id (and start (get-text-property start 'niwa-view-turn-id))))
    (unless (and bounds child)
      (niwa-diagnostic-user-error (niwa-diagnostic-scope (current-buffer)) "No collapsible section at point"))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char start)
        (delete-region start (niwa-view-render-child-calls-end
                              (cdr bounds) (point-max) source depth))
        (let ((niwa-view--child-call-depth (1- depth))
              (niwa-view--rendering-indent ""))
          (niwa-view--insert-child-call-block
           child source (not collapsed) indent))
        (when turn-id
          (put-text-property start (point) 'niwa-view-turn-id turn-id))))))


(defvar-local niwa-view--source-revisions nil
  "Bounded map of source ranges to disposable rendering identities.")

(defvar-local niwa-view--source-revision-tick nil
  "Character modification tick accounted for by source revision tracking.")

(defvar-local niwa-view--source-change-start nil
  "Beginning of an observed source edit awaiting its after-change hook.")

(defun niwa-view--source-before-change (beg _end)
  "Record an upcoming source modification at BEG.
Discard identities if an earlier character modification escaped the hooks."
  (when niwa-view--source-revisions
    (unless (eql niwa-view--source-revision-tick (buffer-chars-modified-tick))
      (clrhash niwa-view--source-revisions))
    (setq niwa-view--source-revision-tick (buffer-chars-modified-tick)
          niwa-view--source-change-start beg)))

(defun niwa-view--source-after-change (beg _end _old-length)
  "Account for a source modification beginning at BEG.
Character changes invalidate ranges at or after BEG.  Property-only changes
leave character identities intact; rendering keys inspect provenance directly."
  (when niwa-view--source-revisions
    (unless (eql niwa-view--source-revision-tick (buffer-chars-modified-tick))
      (if (null niwa-view--source-change-start)
          (clrhash niwa-view--source-revisions)
        (let ((start (min beg niwa-view--source-change-start)))
          (maphash (lambda (range _identity)
                     (when (> (cdr range) start)
                       (remhash range niwa-view--source-revisions)))
                   niwa-view--source-revisions))))
    (setq niwa-view--source-revision-tick (buffer-chars-modified-tick)
          niwa-view--source-change-start nil)))

(defun niwa-view--source-revision (start end)
  "Return a character identity for the current buffer's range START..END.
Any unobserved character modification, including one made with hooks inhibited,
discards identities before reuse.  No source text is copied or retained.
Property provenance is checked separately by the rendering key."
  (unless (and niwa-view--source-revisions
               (eql niwa-view--source-revision-tick (buffer-chars-modified-tick))
               (memq #'niwa-view--source-before-change before-change-functions)
               (memq #'niwa-view--source-after-change after-change-functions))
    (setq niwa-view--source-revisions (make-hash-table :test #'equal)
          niwa-view--source-revision-tick (buffer-chars-modified-tick)
          niwa-view--source-change-start nil)
    (add-hook 'before-change-functions #'niwa-view--source-before-change nil t)
    (add-hook 'after-change-functions #'niwa-view--source-after-change nil t))
  (let ((range (cons start end)))
    (or (gethash range niwa-view--source-revisions)
        (puthash range (make-symbol "source-revision")
                 niwa-view--source-revisions))))

(defun niwa-view--tool-source-properties (start end)
  "Return relevant provenance intervals in the source range START..END.
Inspect buffer properties directly without copying the payload."
  (let ((pos start) intervals)
    (while (< pos end)
      (let ((next (min (next-single-property-change pos 'gptel nil end)
                       (next-single-property-change pos 'niwa-render-data nil end)
                       (next-single-property-change pos 'niwa-hook-audit nil end))))
        (push (list (- pos start) (- next start)
                    (copy-tree (get-text-property pos 'gptel))
                    (copy-tree (get-text-property pos 'niwa-render-data))
                    (copy-tree (get-text-property pos 'niwa-hook-audit)))
              intervals)
        (setq pos next)))
    (nreverse intervals)))

(defvar niwa-view--tool-cache-used nil
  "Tool keys visited by the current complete projection, or nil.
Staged projection jobs retain this table across their callbacks.")

(defun niwa-view--sweep-tool-rendering-cache (used)
  "Retain only the current projection's USED tool keys and source identities.
Call after a successful complete projection, never for an incremental tail."
  (when (hash-table-p niwa-view--tool-rendering-cache)
    (maphash (lambda (key _rendering)
               (unless (gethash key used)
                 (remhash key niwa-view--tool-rendering-cache)))
             niwa-view--tool-rendering-cache))
  (let ((sources (make-hash-table :test #'eq)))
    (maphash (lambda (key _)
               (let* ((data (car key))
                      (ranges (or (gethash data sources)
                                  (puthash data (make-hash-table :test #'equal) sources))))
                 (puthash (cons (nth 1 key) (nth 2 key)) t ranges))) used)
    (maphash (lambda (data ranges)
               (when (buffer-live-p data)
                 (with-current-buffer data
                   (when (hash-table-p niwa-view--source-revisions)
                     (maphash (lambda (range _identity)
                                (unless (gethash range ranges)
                                  (remhash range niwa-view--source-revisions)))
                              niwa-view--source-revisions))))) sources)))

(defun niwa-view--tool-cache-key
    (data-buf seg-start seg-end collapsed-only)
  "Return a source-tracked key for DATA-BUF SEG-START..SEG-END rendering.
COLLAPSED-ONLY distinguishes header and body renderings.  Unchanged source
characters and provenance reuse the key without copying or hashing payloads.
Archived rows also depend on the live transcript's terminal completions."
  (let ((live-data niwa--data-buffer))
    (with-current-buffer data-buf
      (let ((start (niwa-view--source-position seg-start))
            (end (niwa-view--source-position seg-end)))
        (let ((key (list data-buf start end
                         (niwa-view--source-revision start end)
                         (niwa-view--session-render-state-fingerprint niwa--session)
                         (and collapsed-only t)
                         (niwa-view--tool-source-properties start end)
                         (and (buffer-live-p live-data)
                              (not (eq live-data data-buf))
                              (list live-data
                                    (with-current-buffer live-data
                                      (buffer-modified-tick)))))))
          (when niwa-view--tool-cache-used
            (puthash key t niwa-view--tool-cache-used))
          key)))))

(defun niwa-view--collapsed-rendering-p (rendering)
  "Return non-nil when RENDERING initially renders as a collapsed header."
  (and rendering
       (not (and (plist-member rendering :expandable-p)
                 (not (plist-get rendering :expandable-p))))
       (or (not (plist-member rendering :initially-collapsed-p))
           (plist-get rendering :initially-collapsed-p))))

(defun niwa-view--omit-rendering-body-for-cache (rendering)
  "Return RENDERING with its body omitted for collapsed-header caching.
Nested call rows go with the body: a collapsed block shows neither, and
retaining them would keep every child result in the header cache."
  (if (niwa-view--collapsed-rendering-p rendering)
      (plist-put (plist-put (copy-sequence rendering) :body nil)
                 :child-calls nil)
    rendering))

(defun niwa-view--execution-terminal-render-data
    (data-buf tool-use-id &optional yielded-id)
  "Return TOOL-USE-ID's latest completion in DATA-BUF or the live transcript.
An archived row still belongs to the live transcript: completion may have
arrived in an intermediate segment after the source row was archived.
YIELDED-ID limits older-segment inspection to yielded executions."
  (or (and (buffer-live-p niwa--data-buffer)
           (not (eq niwa--data-buffer data-buf))
           (niwa-execution-transcript-pending-render-data
            niwa--data-buffer tool-use-id))
      (and (buffer-live-p data-buf)
           (niwa-execution-transcript-pending-render-data
            data-buf tool-use-id))
      (when (and yielded-id (buffer-live-p niwa--data-buffer)
                 (not (eq niwa--data-buffer data-buf)))
        (when-let* ((session (buffer-local-value
                              'niwa--session niwa--data-buffer)))
          (catch 'found
            (dolist (descriptor (reverse (niwa-session-artifacts-transcript-segments
                                          session niwa--data-buffer
                                          niwa-view--agent-transcript-p)))
              (when (and (eq (plist-get descriptor :status) 'readable)
                         (not (plist-get descriptor :current-p)))
                (when-let* ((older (condition-case nil
                                      (niwa-session-artifacts-read-transcript-segment
                                       session descriptor)
                                    (error nil))))
                  (unwind-protect
                      (when-let* ((terminal
                                   (niwa-execution-transcript-pending-render-data
                                    older tool-use-id)))
                        (throw 'found terminal))
                    (kill-buffer older))))))))))

(defun niwa-view--render-tool-call (call data-buf &optional collapsed-only)
  "Render parsed CALL from DATA-BUF using the current view and tool registry.
COLLAPSED-ONLY requests summary-only renderer work.  Source preparation must
finish before this function invokes registered renderers or reads live events."
  (when call
    (let* ((niwa-tool-render-summary-only collapsed-only)
           (name (plist-get call :name))
           (args (plist-get call :args))
           (tool-use-id (plist-get call :tool-use-id))
           (event
            (and (bound-and-true-p niwa-view--execution-events)
                 (hash-table-p niwa-view--execution-events)
                 (gethash tool-use-id niwa-view--execution-events)))
           (call-render-data (plist-get call :render-data))
           (terminal-render-data
            (and (member name '("Bash" "Monitor"))
                 (niwa-view--execution-terminal-render-data
                  data-buf tool-use-id
                  (plist-get call-render-data :execution-id))))
           (event-type (plist-get event :type))
           (direct-child (niwa-tool-render-data-direct-call
                          name call-render-data))
           (direct-terminal
            (and (member (plist-get direct-child :tool) '("Bash" "Monitor"))
                 (stringp (plist-get direct-child :id))
                 (niwa-view--execution-terminal-render-data
                  data-buf (plist-get direct-child :id)
                  (plist-get (plist-get direct-child :render-data)
                             :execution-id))))
           (direct-event
            (and (not direct-terminal)
                 (stringp (plist-get direct-child :id))
                 (bound-and-true-p niwa-view--execution-events)
                 (hash-table-p niwa-view--execution-events)
                 (gethash (plist-get direct-child :id)
                          niwa-view--execution-events)))
           (direct-progress
            (and (eq (plist-get direct-event :type) 'progress)
                 (append (copy-sequence (plist-get direct-event :facts))
                         (list :permission-approvals
                               (plist-get direct-event :permission-approvals)
                               :status 'success :live-execution-p t
                               :output-preview-truncated-p
                               (plist-get direct-event :output-preview-truncated-p)))))
           (direct-latest (or direct-terminal direct-progress))
           (result
            (cond
             ((eq event-type 'progress)
              (or (plist-get event :output-tail) ""))
             ((plist-member terminal-render-data :execution-output)
              (or (plist-get terminal-render-data :execution-output) ""))
             ((plist-member direct-latest :execution-output)
              (or (plist-get direct-latest :execution-output) ""))
             (direct-progress
              (or (plist-get direct-event :output-tail) ""))
             ((plist-member call-render-data :execution-output)
              (or (plist-get call-render-data :execution-output) ""))
             ((plist-member (plist-get direct-child :render-data)
                            :execution-output)
              (or (plist-get (plist-get direct-child :render-data)
                             :execution-output) ""))
             (t (plist-get call :result))))
           (render-data
            (if (eq event-type 'progress)
                (let ((facts (copy-sequence (plist-get event :facts))))
                  (append
                   facts
                   (list :permission-approvals
                         (plist-get event :permission-approvals)
                         :status 'success
                         :live-execution-p t
                         :output-preview-truncated-p
                         (plist-get event :output-preview-truncated-p))))
              (or terminal-render-data call-render-data)))
           (tool (niwa-tool-get name))
           custom rendering)
      ;; Direct ToolCall rows unwrap the child without creating a separate view
      ;; row.  Reconcile its settled facts before that shortcut invokes Bash's
      ;; renderer, just as a separately rendered compound child would do.
      (when direct-latest
        (setq render-data
              (plist-put (copy-sequence render-data) :calls
                         (list (plist-put
                                (plist-put (copy-sequence direct-child)
                                           :render-data direct-latest)
                                :status (plist-get direct-latest :status))))))
      (setq custom (and tool (niwa-view--invoke-renderer
                              tool render-data args result)))
      (setq rendering (or custom
                          (niwa-view--generic-tool-rendering
                           name args result collapsed-only render-data)))
      (when-let* (((eq (plist-get rendering :vtype) 'agent-handle))
                  (path (plist-get render-data :path)))
        (setq rendering (plist-put rendering :agent-path path)))
      ;; A direct ToolCall renders its child as the outer row.  Its terminal
      ;; facts can arrive after the envelope, so retain the child's disclosure
      ;; and execution identity on that same row without changing the source ID.
      (when-let* ((summary (or (plist-get direct-latest :sandbox-summary)
                               (plist-get (plist-get direct-child :render-data)
                                          :sandbox-summary)
                               (plist-get call-render-data :sandbox-summary)
                               (plist-get render-data :sandbox-summary))))
        (setq rendering
              (plist-put rendering :sandbox-summary (copy-tree summary))))
      (when (member (plist-get direct-child :tool) '("Bash" "Monitor"))
        (when-let* ((id (or (plist-get direct-latest :execution-id)
                           (plist-get (plist-get direct-child :render-data)
                                      :execution-id))))
          (setq rendering (plist-put rendering :execution-id id))))
      (when tool-use-id
        (setq rendering
              (plist-put rendering :tool-use-id tool-use-id)))
      (when-let* ((audits
                   (niwa-view--merge-hook-audits
                    (plist-get call :hook-audits)
                    (plist-get rendering :hook-audits))))
        (setq rendering (plist-put rendering :hook-audits audits)))
      ;; Grouping needs the same normalized result and render data.  Carry
      ;; them with this one computation; the cache drops the payload below
      ;; so a collapsed header does not retain a large result indefinitely.
      (setq rendering
            (plist-put rendering :group-tool
                       (or (plist-get direct-child :tool) name)))
      (setq rendering
            (plist-put
             rendering :group-child
             (list :tool name :args args :result result :render-data render-data
                   :status (if (and direct-child (plist-get rendering :status))
                               (niwa-view--rendering-status rendering)
                             (niwa-view--tool-render-status
                              result render-data)))))
      (if collapsed-only
          (niwa-view--omit-rendering-body-for-cache rendering)
        rendering))))

(defun niwa-view--prepare-tool-segment (data-buf seg-start seg-end)
  "Prepare DATA-BUF's SEG-START..SEG-END without invoking a renderer.
Return a parsed call or request failure and whether the complete span can be
cached under its own source identity.  Partial spans keep their recovery path.
The returned data is temporary; it can contain large hidden result payloads."
  (let* ((text (niwa-view-render--parse-step
                (with-current-buffer data-buf
                  (buffer-substring seg-start seg-end))))
         (request-data (niwa-view-render--parse-step
                         (niwa-view--request-summary-render-data-from-text text))))
    (if (eq (plist-get request-data :outcome) 'error)
        (list :request-failure request-data :cacheable t)
      (let* ((raw (niwa-view-render--parse-step
                   (with-current-buffer data-buf
                     (niwa-view--tool-segment-text seg-start seg-end text))))
             ;; The key's revision covers the whole span, so a complete block
             ;; inside it -- followed by its merged hook audits -- is as
             ;; stable as the span.  Requiring equality left every audited
             ;; tool reparsed on each streamed update.
             (cacheable
              (niwa-view-render--parse-step
                (and (string-search raw text)
                     (or (niwa-view--complete-wrapped-tool-text-p raw)
                         (and (not (niwa-view--tool-wrapped-text-p raw))
                              (niwa-view--direct-tool-readable-text-p raw)))))))
        (list :call (niwa-view--tool-call-parse data-buf seg-start seg-end raw)
              :cacheable cacheable)))))

(defun niwa-view--render-prepared-tool (prepared data-buf &optional collapsed-only)
  "Render PREPARED source data using the current view's presentation state.
DATA-BUF supplies live execution metadata; COLLAPSED-ONLY requests summaries."
  (if-let* ((request-data (plist-get prepared :request-failure)))
      (let* ((backend (or (plist-get request-data :backend) "Provider"))
             (status (plist-get request-data :status))
             (type (plist-get request-data :error-type))
             (code (plist-get request-data :error-code))
             (error-data (plist-get request-data :error-data))
             (retry-errors (plist-get request-data :retry-errors))
             (retry-exhausted (plist-get request-data :retry-exhausted))
             (message-text (plist-get request-data :message))
             (extra-data (and (listp error-data)
                              (niwa-view--request-failure-extra-data
                               error-data message-text type code))))
        (list
         :header (concat backend " request failed"
                         (if type (format " · %s" type) "")
                         (if retry-exhausted " · retries exhausted" ""))
         :body
         (string-join
          (delq nil
                (list (and status (format "Status: %s" status))
                      (and type (format "Type: %s" type))
                      (and code (format "Code: %s" code))
                      (and extra-data (format "Provider data: %S" extra-data))
                      ""
                      (niwa-diagnostic-format
                       (niwa-diagnostic-scope data-buf) "%s"
                       (or message-text "Request failed"))
                      (and retry-exhausted
                           (format "Automatic retries exhausted%s."
                                   (if retry-errors
                                       (format " after %d attempts"
                                               (length retry-errors))
                                     "")))
                      (and retry-errors
                           (concat
                            "Attempt history (oldest first):\n"
                            (mapconcat
                             (lambda (attempt)
                               (format
                                "Attempt %s%s: %s"
                                (plist-get attempt :attempt)
                                (let ((details
                                       (delq nil
                                             (list (plist-get attempt :http-status)
                                                   (plist-get attempt :type)
                                                   (plist-get attempt :code)))))
                                  (if details
                                      (concat " · " (string-join details " · "))
                                    ""))
                                (plist-get attempt :message)))
                             retry-errors "\n")))
                      ""
                      (if (plist-get request-data :external-history)
                          "Retry forks native history before the failed prompt and replays it. Older sessions without answer coordinates reuse their native history, which may retain partial context. Failed partial output is removed; prior tool effects and the error disclosure remain."
                        "Retry replays the prepared request while it remains retained and the transcript is unchanged.")))
          "\n")
         :body-mode 'text-mode
         :vtype 'request-failure
         :failure-id (plist-get request-data :request-id)
         :failure-buffer data-buf
         :external-history (plist-get request-data :external-history)
         :status 'error
         :initially-collapsed-p nil))
    (niwa-view--render-tool-call (plist-get prepared :call) data-buf collapsed-only)))

(defun niwa-view--request-failure-extra-data (error-data message type code)
  "Return ERROR-DATA without the fields a failure block already shows.
A plist's :message, :type and :code equal to MESSAGE, TYPE or CODE are
dropped: the block states those on their own lines, and repeating them
under \"Provider data\" printed an agent's error twice.  Return nil when
nothing else remains, and other list data unchanged."
  (if (not (and (consp error-data) (proper-list-p error-data)
                (zerop (% (length error-data) 2))
                (cl-loop for key in error-data by #'cddr always (keywordp key))))
      error-data
    (cl-loop for (key value) on error-data by #'cddr
             unless (or (and (eq key :message) (equal value message))
                        (and (eq key :type) (equal value type))
                        (and (eq key :code) (equal value code)))
             append (list key value))))

(defun niwa-view--retire-tool-renderings (data-buf start end)
  "Discard renderings overlapping DATA-BUF START..END before a local refresh.
Local refreshes need this too: a full projection may not run again for a long
conversation, and patched agent handles otherwise retain every old body."
  (when (hash-table-p niwa-view--tool-rendering-cache)
    (maphash
     (lambda (key _rendering)
       (when (and (eq (car-safe key) data-buf)
                  (< (nth 1 key) end) (> (nth 2 key) start))
         (remhash key niwa-view--tool-rendering-cache)))
     niwa-view--tool-rendering-cache)))

(defun niwa-view--segment-rendering (data-buf seg-start seg-end
                                                 &optional collapsed-only)
  "Return rendering for DATA-BUF's SEG-START..SEG-END.
Provider-failure request summaries and tool segments are renderable.
Return nil only when the segment is malformed or unparseable.
COLLAPSED-ONLY requests a cached summary without retaining large bodies."
  (let* ((cache (and (hash-table-p niwa-view--tool-rendering-cache)
                     niwa-view--tool-rendering-cache))
         (key (and cache (niwa-view--tool-cache-key
                          data-buf seg-start seg-end collapsed-only))))
    (or (and key (gethash key cache))
        (let* ((prepared (or (and collapsed-only (bound-and-true-p niwa-view-prepare-enabled)
                                  (niwa-view-prepare-get data-buf seg-start seg-end key))
                             (niwa-view--prepare-tool-segment data-buf seg-start seg-end)))
               (rendering (or (plist-get prepared :pending)
                              (niwa-view--render-prepared-tool prepared data-buf collapsed-only))))
          ;; A live execution's header reads transient progress (elapsed time,
          ;; output) that the source-based key cannot see.  Caching it would
          ;; freeze one snapshot per key, and render paths would alternate.
          (when (and key rendering (plist-get prepared :cacheable)
                     (not (eq (plist-get rendering :status) 'running)))
            (puthash key (plist-put (copy-sequence rendering) :group-child nil) cache))
          rendering))))

(defvar-local niwa-view--tool-row-starts nil
  "Hash table from tool-use ID to a marker at its last found view row.
A hint only: `niwa-view--tool-row-region' revalidates it before use.")

(defconst niwa-view--tool-row-starts-limit 32
  "Maximum number of remembered tool row markers per view.")

(defun niwa-view--forget-tool-row-starts ()
  "Drop every remembered tool row marker in the current view."
  (when (hash-table-p niwa-view--tool-row-starts)
    (maphash (lambda (_id marker) (set-marker marker nil))
             niwa-view--tool-row-starts)
    (clrhash niwa-view--tool-row-starts)))

(defun niwa-view--tool-row-start-p (pos limit tool-use-id)
  "Return non-nil when TOOL-USE-ID's refreshable row starts at POS before LIMIT."
  (and (< pos limit)
       (consp (get-text-property pos 'niwa-view-source))
       (eq (get-text-property pos 'niwa-view-type) 'tool-summary)
       (equal (get-text-property pos 'niwa-view-tool-use-id) tool-use-id)))

(defun niwa-view--tool-row-start (tool-use-id limit)
  "Return the start of TOOL-USE-ID's refreshable row before LIMIT.
A projection shows each tool-use ID in at most one summary row.  Progress
refreshes the same row repeatedly, so a remembered start that still begins
that row spares scanning the transcript."
  (let* ((marker (and (hash-table-p niwa-view--tool-row-starts)
                      (gethash tool-use-id niwa-view--tool-row-starts)))
         (hint (and marker (marker-position marker))))
    (if (and hint
             (niwa-view--tool-row-start-p hint limit tool-use-id)
             (or (= hint (point-min))
                 (not (equal (get-text-property (1- hint) 'niwa-view-tool-use-id)
                             tool-use-id))))
        hint
      (when marker
        (set-marker marker nil)
        (remhash tool-use-id niwa-view--tool-row-starts))
      (let ((pos (point-min)) found)
        (while (and (< pos limit) (not found))
          (if (niwa-view--tool-row-start-p pos limit tool-use-id)
              (setq found pos)
            (setq pos (or (next-single-property-change
                           pos 'niwa-view-tool-use-id nil limit)
                          limit))))
        (when found
          (unless (hash-table-p niwa-view--tool-row-starts)
            (setq niwa-view--tool-row-starts (make-hash-table :test #'equal)))
          (when (>= (hash-table-count niwa-view--tool-row-starts)
                    niwa-view--tool-row-starts-limit)
            (niwa-view--forget-tool-row-starts))
          (puthash tool-use-id (copy-marker found) niwa-view--tool-row-starts))
        found))))

(defun niwa-view--tool-row-region (data-buffer tool-use-id)
  "Return the visible source-backed row for TOOL-USE-ID in DATA-BUFFER.
The result is `(VIEW-START VIEW-END SOURCE-BOUNDS)' or nil."
  (when-let* ((bounds
               (with-current-buffer data-buffer
                 (niwa-tool-render-data-segment-bounds tool-use-id)))
              ;; Yanking a tool row keeps its identity properties.  A draft is
              ;; never a refresh target, even if grouping hides the original row.
              (limit (if niwa-view--agent-transcript-p
                         (point-max)
                       (or (niwa-view-transcript-tail) (point-max))))
              (pos (niwa-view--tool-row-start tool-use-id limit)))
    (let ((source (get-text-property pos 'niwa-view-source))
          (next (or (next-single-property-change
                     pos 'niwa-view-tool-use-id nil limit)
                    limit)))
      ;; Expanded compound tools append child rows with their own IDs.
      ;; Replace the whole parent section, not just its text up to the
      ;; first child, or every progress refresh duplicates the children.
      (list pos (niwa-view-render-child-calls-end next limit source)
            ;; Restoring transcript properties can split the metadata into an
            ;; ignored run.  The projected source still owns the complete
            ;; canonical span.
            (if (and (markerp (car source))
                     (markerp (cdr source))
                     (eq (marker-buffer (car source)) data-buffer)
                     (eq (marker-buffer (cdr source)) data-buffer)
                     (<= (car source) (car bounds))
                     (<= (cdr bounds) (cdr source)))
                (cons (marker-position (car source))
                      (marker-position (cdr source)))
              bounds)))))

(defun niwa-view--refresh-tool-row (data-buffer tool-use-id)
  "Refresh only TOOL-USE-ID's visible row from DATA-BUFFER."
  (niwa-view-render-mutate
   (list 'tool-row tool-use-id)
   (lambda () (niwa-view-render--refresh-tool-row-now data-buffer tool-use-id))))

(defun niwa-view-render--refresh-tool-row-now (data-buffer tool-use-id)
  "Rediscover TOOL-USE-ID in DATA-BUFFER with projection ownership held."
  (let ((owner tool-use-id) region)
    ;; Compound children share the outer gptel segment.  Refresh that row for
    ;; both attended events and deferred updates when focus returns.
    (while (and (not region) owner)
      (setq region (niwa-view--tool-row-region data-buffer owner)
            owner (and (string-match "/[0-9]+\\'" owner)
                       (substring owner 0 (match-beginning 0)))))
    (when region
    (let* ((start (nth 0 region))
           (end (nth 1 region))
           (bounds (nth 2 region))
           (source (niwa-view-disclosure-source-range
                    data-buffer (car bounds) (cdr bounds)))
           (collapsed (get-text-property start 'niwa-view-collapsed))
           (previously-forced-p
            (get-text-property start 'niwa-view-force-expanded))
           (turn-id (get-text-property start 'niwa-view-turn-id)))
      ;; Rebase visible nested choices before replacing their source-backed row.
      (niwa-view-disclosure-capture-state start end)
      ;; Progress changes rendering even when its source text stays unchanged.
      (niwa-view--retire-tool-renderings data-buffer (car source) (cdr source))
      (when-let* ((rendering
                  (niwa-view--segment-rendering
                   data-buffer (car source) (cdr source))))
        ;; The source span can include following audits, but this replacement
        ;; owns only the tool and its nested calls.  Their separate audit and
        ;; completion rows remain outside START..END; reinserting them here
        ;; would add another copy on every progress event.
        (setq rendering (plist-put (copy-sequence rendering) :hook-audits nil))
        (unless (or (plist-get rendering :force-expanded-p)
                    previously-forced-p)
          (setq rendering
                (plist-put (copy-sequence rendering)
                           :initially-collapsed-p collapsed)))
        (niwa-view-call-preserving-user-view-state
         (lambda ()
           (niwa-view--call-with-render-boundaries-advancing
            (lambda ()
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (save-excursion
                  (goto-char start)
                  (delete-region start end)
                  (let ((insert-start (point)))
                    (niwa-view--insert-rendered-tool rendering source)
                    (niwa-view-render-add-display-properties
                     insert-start (point))
                    (when turn-id
                      (put-text-property
                       insert-start (point)
                       'niwa-view-turn-id turn-id))))))))
         ;; Only the row changes; the reader's position elsewhere is exact.
         (cons start end))
        t)))))

(provide 'niwa-view-tool-render)
;;; niwa-view-tool-render.el ends here
