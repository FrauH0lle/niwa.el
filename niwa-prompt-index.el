;;; niwa-prompt-index.el --- Session transcript prompt index -*- lexical-binding: t -*-

;;; Commentary:

;; Projects authored prompts and fork points from session transcripts.
;; Session artifacts loads this owner for its existing index entry points.

;;; Code:

(require 'cl-lib)
(require 'niwa-tool-render-data)
(require 'niwa-transcript-audit)
(require 'niwa-transcript-source)
(require 'niwa-structs)
(require 'niwa-utilities)

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-read-segment "niwa-session-artifacts" (session number))
(declare-function niwa-session-artifacts-segment-summary-bounds "niwa-session-artifacts" ())
(autoload 'niwa-session-artifacts-read-segment "niwa-session-artifacts")
(autoload 'niwa-session-artifacts-segment-summary-bounds "niwa-session-artifacts")

;; `niwa-transcript'
(declare-function niwa-transcript--org-block-depths-before
                  "niwa-transcript" (pos &optional start tool-depth summary-depth))
(declare-function niwa-transcript--render-data-block-at
                  "niwa-transcript" (position &optional limit literal))
(declare-function niwa-transcript--user-prompt-start
                  "niwa-transcript" (pos next prop &optional state))
(declare-function niwa-transcript-cache-create
                  "niwa-transcript" (&rest args))
(declare-function niwa-transcript-cache-restart
                  "niwa-transcript" (cache))
(declare-function niwa-transcript-cached-segments
                  "niwa-transcript" (cache &optional origin))
(declare-function niwa-transcript-leading-property
                  "niwa-transcript" (name))
(declare-function niwa-transcript-prompt-scan-state
                  "niwa-transcript" ())
(declare-function niwa-transcript-segments
                  "niwa-transcript" (start end))
(defvar niwa-transcript--prepared-audit-spans)
(autoload 'niwa-transcript--org-block-depths-before "niwa-transcript")
(autoload 'niwa-transcript--render-data-block-at "niwa-transcript")
(autoload 'niwa-transcript--user-prompt-start "niwa-transcript")
(autoload 'niwa-transcript-cache-create "niwa-transcript")
(autoload 'niwa-transcript-cached-segments "niwa-transcript")
(autoload 'niwa-transcript-leading-property "niwa-transcript")
(autoload 'niwa-transcript-prompt-scan-state "niwa-transcript")
(autoload 'niwa-transcript-segments "niwa-transcript")

;; `niwa-transcript-restore'
(declare-function niwa-transcript-restore-properties "niwa-transcript-restore" (&optional only-if-missing))
(autoload 'niwa-transcript-restore-properties "niwa-transcript-restore")

;; `org'
(defvar org-agenda-file-menu-enabled)

;;; Prompt index (used by the rewind picker)

(defun niwa-session-artifacts-content-start (buffer)
  "Return the first buffer position past BUFFER's leading metadata.
Skips an initial org property drawer (:PROPERTIES: ... :END:) so the
prompt walker does not treat the drawer as a user prompt.  Returns
`point-min' when no drawer is present."
  (with-current-buffer buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (if (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
            (if (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                (progn (forward-line 1) (point))
              (point-min))
          (point-min))))))

(defun niwa-session-artifacts-collect-prompts (buffer &optional preview-length)
  "Return indexed user prompt plists for BUFFER.

A user prompt is a nil-`gptel' text-property region with
non-whitespace content that is not gptel's org tool/reasoning
scaffolding.  Turns are numbered 1, 2, ... in document order.  Used at
save time to refresh the live segment's entry in
`niwa-session-prompt-index'.

Skips the initial org property drawer (via
`niwa-session-artifacts-content-start') and any content inside
`#+begin_summary' / `#+end_summary' blocks (the compaction summary
has its body stripped of the `gptel' property but is not a user
prompt).  Also skips unpropertized gptel org tool/reasoning block glue and
structural mailbox deliveries using the canonical transcript codec.
PREVIEW-LENGTH defaults to 80 display characters."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (save-restriction
          (widen)
          (let* ((content-start
                  (niwa-session-artifacts-content-start buffer))
                (turn 0)
                ;; Ditto: one forward pass over the block-depth prefix.
                (scan-state (niwa-transcript-prompt-scan-state))
                (results nil))
            (dolist (seg (niwa-transcript-segments
                          content-start
                          (point-max)))
              (pcase-let ((`(,type ,seg-start ,seg-end) seg))
                (when (eq type 'user)
                  (when-let* ((prompt-start
                               (niwa-transcript--user-prompt-start
                                (max seg-start content-start) seg-end nil
                                scan-state)))
                    ;; Only the first nonblank line is kept, so find it in
                    ;; the buffer: copying each prompt whole cost megabytes
                    ;; per save on a long segment.
                    (let ((preview
                           (save-excursion
                             (goto-char prompt-start)
                             (when (re-search-forward "[^[:space:]]" seg-end t)
                               (buffer-substring-no-properties
                                (match-beginning 0)
                                (min seg-end (line-end-position)))))))
                      (when preview
                        (cl-incf turn)
                        (push (niwa-session-artifacts--prompt-record
                               turn prompt-start
                               (niwa--truncate-display
                                preview (or preview-length 80) "..."))
                              results)))))))
            (nreverse results)))))))

(defun niwa-session-artifacts--prompt-record (turn prompt-start preview)
  "Return the index plist of prompt TURN starting at PROMPT-START.
PREVIEW is its display line."
  (list :turn turn :pos prompt-start :preview preview))

;;;; Incremental prompt index

(cl-defstruct (niwa-session-artifacts--prompt-cache
               (:constructor niwa-session-artifacts--prompt-cache-create)
               (:copier nil))
  "Derived prompt-index state of one transcript buffer, never persisted.
SOURCE proves which suffix changed and SEGMENTS is the parser cache.  ORIGIN
is the body origin of the last derivation, nil before the first.  UNITS holds
one vector per user segment and SPANS every audit span, with positions
relative to ORIGIN.  UNRESOLVED is the earliest relative audit opener that no
span consumed: a later close could still pair it.  FRONTIER is
\(POSITION TOOL SUMMARY), relative to ORIGIN: the org block depths counted up
to a position near the
buffer end, so the next prompt scan resumes there instead of at the last
prompt.  PREFIX is the text before ORIGIN, including its properties."
  source segments origin units spans unresolved frontier prefix)

(defvar-local niwa-session-artifacts--prompt-cache nil
  "Incremental prompt-index state of the current transcript buffer.")

(defun niwa-session-artifacts--prompt-cache-eligible-p ()
  "Return non-nil when the current buffer is accessible whole."
  (and (= (point-min) 1) (= (point-max) (1+ (buffer-size)))))

(defun niwa-session-artifacts--prompt-cache-ensure (origin)
  "Return the current buffer's prompt cache, creating it for ORIGIN."
  (let ((cache niwa-session-artifacts--prompt-cache))
    (unless (and cache
                 (eq (marker-buffer
                      (niwa-transcript-source-origin
                       (niwa-session-artifacts--prompt-cache-source cache)))
                     (current-buffer)))
      (setq cache (niwa-session-artifacts--prompt-cache-create
                   :source (niwa-transcript-source-create origin)
                   :segments (niwa-transcript-cache-create))
            niwa-session-artifacts--prompt-cache cache))
    cache))

(defun niwa-session-artifacts--audit-spans-cached (cache origin d full prefix)
  "Return (SPANS . UNRESOLVED) for the current buffer without changing CACHE.
SPANS are every audit span relative to ORIGIN, as one whole-buffer scan finds
them.  D is the first dirty offset relative to ORIGIN, nil when the source is
unchanged; FULL forces a complete scan.  PREFIX is the text before ORIGIN.
A cached span ending at or before the
restart point is reused: its end is where the sequential scan stood, so
scanning on from there is the scan the whole buffer would have run.  The
restart point is the first change or the earliest opener without a span,
whichever comes first, because a later close can pair such an opener."
  (let* ((spans (niwa-session-artifacts--prompt-cache-spans cache))
         (unresolved (niwa-session-artifacts--prompt-cache-unresolved cache))
         ;; Metadata edits before the origin leave the change proof clean, so
         ;; compare the pre-origin text itself: only a change there can add,
         ;; move or remove spans in it.
         (full (or full
                   (not (equal-including-properties
                         prefix
                         (niwa-session-artifacts--prompt-cache-prefix cache)))
                   ;; Offsets of pre-origin records drift with the origin.
                   (and d (or (< (or (caar spans) 0) 0)
                              (< (or unresolved 0) 0)))))
         (old-p (and (not full) (niwa-session-artifacts--prompt-cache-origin cache)))
         (old (and old-p spans)))
    (if (and old-p (null d))
        (cons spans unresolved)
      (let* ((limit (if old-p (min d (or unresolved d)) (- (point-min) origin)))
             (reused (cl-loop for span in old
                              while (<= (nth 1 span) limit)
                              collect span))
             ;; Rescan only from just before the first change, far enough
             ;; back to catch an opener it completes, and no later than
             ;; the first span the change crosses or the last one reused.
             ;; Everything between holds no opener outside a reused span:
             ;; unresolved ones bound LIMIT above.
             (from (max (point-min)
                        (min (max (if reused
                                      (+ origin (nth 1 (car (last reused))))
                                    (point-min))
                                  (- (+ origin limit)
                                     (1- (length niwa--hook-audit-open))))
                             (if-let* ((next (nth (length reused) old)))
                                 (+ origin (nth 0 next))
                               (point-max)))))
             (fresh (niwa-transcript-audit-buffer-spans nil from (point-max)))
             (rest fresh)
             first-open)
        (save-excursion
          (goto-char from)
          (while (and (not first-open)
                      (search-forward niwa--hook-audit-open nil t))
            (let ((open (match-beginning 0)))
              (while (and rest (<= (plist-get (car rest) :end) open))
                (pop rest))
              (unless (and rest (<= (plist-get (car rest) :start) open))
                (setq first-open (- open origin))))))
        (cons (append reused
                      (mapcar (lambda (span)
                                (list (- (plist-get span :start) origin)
                                      (- (plist-get span :end) origin)
                                      (plist-get span :record)))
                              fresh))
              first-open)))))

(defun niwa-session-artifacts--depth-lookahead-closed-p (from to)
  "Return non-nil when no block marker in FROM..TO reads text at or past TO.
The org block depths counted up to TO are then fixed by the text before TO.
Two kinds of marker decide from text after themselves: `#+begin_tool' (the
serialized plist after it, past any whitespace, makes it structural) and a
render-data marker (a complete block is skipped as a whole, an incomplete one
is not).  Both are judged with the depth counter's own grammar: leading
whitespace allowed, any trailing text after the render-data marker."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char from)
      (let ((closed t)
            (regexp (concat "^[ \t]*#\\+begin_tool\\b"
                            "\\|^" (regexp-quote niwa-tool-render-data-open))))
        (while (and closed (< (point) to) (re-search-forward regexp to t))
          (let ((marker (match-beginning 0)))
            (if (eq (char-after marker) ?<)
                (let ((block (niwa-transcript--render-data-block-at
                              marker nil)))
                  (if (and block (<= (cadr block) to))
                      (goto-char (max (point) (cadr block)))
                    (setq closed nil)))
              (when (>= (save-excursion
                          (goto-char marker)
                          (forward-line 1)
                          (skip-chars-forward " \t\n")
                          (forward-char (min 1 (- (point-max) (point))))
                          ;; The discriminator's `\s-*' takes any whitespace
                          ;; syntax, newlines included.
                          (skip-syntax-forward "-")
                          (+ (point) 6))
                        to)
                (setq closed nil)))))
        closed))))

(defun niwa-session-artifacts--prompt-units (cache origin d full)
  "Return user-segment units, reusing the unchanged prefix from CACHE.
Each unit is [START END STATE-POS TOOL SUMMARY PROMPT PREVIEW] with positions
relative to ORIGIN: the segment, the prompt scan state after it, and the
prompt start and preview, nil when it holds no prompt.  D and FULL are as for
`niwa-session-artifacts--audit-spans-cached'.  A unit is reused when its
segment ends at or before the parser cache's restart point for the change:
segments before it are then as parsed before, and the scan state carried out
of it depends only on earlier text.  Return `mismatch' when a reused unit
does not line up with the fresh segments, so the caller derives everything."
  (let* ((segments (niwa-session-artifacts--prompt-cache-segments cache))
         (old-p (and (not full)
                     (niwa-session-artifacts--prompt-cache-origin cache)))
         (user (cl-loop for seg in (niwa-transcript-cached-segments
                                    segments origin)
                        when (and (eq (car seg) 'user) (> (nth 2 seg) origin))
                        collect seg))
         ;; The parser re-parsed from its restart point in current
         ;; coordinates, after rebasing over any leading metadata change.
         (previous (and old-p (niwa-session-artifacts--prompt-cache-frontier
                               cache)))
         (restart-limit (and d (min d (- (or (niwa-transcript-cache-restart
                                              segments)
                                             (point-min))
                                         origin))))
         ;; Saved scan states count block depth through markers that read
         ;; ahead.  Only text proven closed (the frontier) may anchor them
         ;; after a change; with no proof every unit is derived again.
         (limit (cond ((not old-p) -1)
                      ((null d) most-positive-fixnum)
                      ((null previous) -1)
                      (t (min restart-limit (car previous)))))
         (frontier (and previous restart-limit
                        (<= (car previous) restart-limit)
                        previous))
         (remaining (and old-p
                         (cl-loop for unit in (niwa-session-artifacts--prompt-cache-units cache)
                                  while (<= (aref unit 1) limit)
                                  collect unit)))
         units state result)
    (setq result
     (catch 'mismatch
      (dolist (seg user)
        (let ((unit (car remaining))
              (start (- (nth 1 seg) origin))
              (end (- (nth 2 seg) origin)))
          (cond
           ((and unit (= (aref unit 0) start) (= (aref unit 1) end))
            (setq remaining (cdr remaining)
                  state (list (+ origin (aref unit 2)) (aref unit 3) (aref unit 4)))
            (push unit units))
           (remaining (throw 'mismatch 'mismatch))
           (t
            (let* ((pos (max (nth 1 seg) origin))
                   (prompt-start nil)
                   (preview nil))
              ;; Resume at the depth frontier when it is nearer than the
              ;; last prompt's state: the body between is unchanged.
              (when (and frontier
                         (<= (+ origin (car frontier)) pos)
                         (or (null state)
                             (> (+ origin (car frontier)) (nth 0 state))))
                (setq state (list (+ origin (car frontier))
                                  (nth 1 frontier) (nth 2 frontier))))
              (setq state (or state (niwa-transcript-prompt-scan-state))
                    prompt-start (niwa-transcript--user-prompt-start
                                  pos (nth 2 seg) nil state))
              (when prompt-start
                (setq preview
                      (save-excursion
                        (goto-char prompt-start)
                        (when (re-search-forward "[^[:space:]]" (nth 2 seg) t)
                          (buffer-substring-no-properties
                           (match-beginning 0)
                           (min (nth 2 seg) (line-end-position)))))))
              (push (vector start end
                            (- (nth 0 state) origin) (nth 1 state) (nth 2 state)
                            (and preview (- prompt-start origin))
                            (and preview
                                 (niwa--truncate-display preview 80 "...")))
                    units))))))
      (if remaining 'mismatch (nreverse units))))
    (if (eq result 'mismatch)
        'mismatch
      (cons result
            (let* ((target (save-excursion
                             (goto-char (point-max))
                             (line-beginning-position)))
                   (candidates
                    (delq nil
                          (list state
                                (and frontier
                                     (list (+ origin (car frontier))
                                           (nth 1 frontier)
                                           (nth 2 frontier))))))
                   (base (car (sort (copy-sequence
                                     (or (cl-remove-if
                                          (lambda (candidate)
                                            (> (nth 0 candidate) target))
                                          candidates)
                                         candidates
                                         (list (niwa-transcript-prompt-scan-state))))
                                    (lambda (a b) (> (nth 0 a) (nth 0 b))))))
                   (position (max target (nth 0 base)))
                   (depths (if (> position (nth 0 base))
                               (niwa-transcript--org-block-depths-before
                                position (nth 0 base) (nth 1 base) (nth 2 base))
                             (cons (nth 1 base) (nth 2 base)))))
              ;; Trust the depths only when no marker counted on the way
              ;; reads text that can still change: fail closed otherwise.
              (and (niwa-session-artifacts--depth-lookahead-closed-p
                    (min position
                         (if frontier (+ origin (car frontier)) (point-min)))
                    position)
                   (list (- position origin) (car depths) (cdr depths))))))))

(defun niwa-session-artifacts--prompts-from-units (units origin)
  "Return the prompt index plists for UNITS at ORIGIN, as a whole scan builds."
  (let ((turn 0) results)
    (dolist (unit units)
      (when-let* ((relative (aref unit 5)))
        (cl-incf turn)
        (push (niwa-session-artifacts--prompt-record
               turn (+ origin relative) (aref unit 6))
              results)))
    (nreverse results)))

(defun niwa-session-artifacts--prompt-derive (buffer)
  "Return (PROMPTS . FORK-POINTS) of BUFFER, as the uncached scans find them.
An accessible-whole buffer derives only what changed since the last call, by
the proof its change source holds; any doubt derives everything.  A narrowed
buffer is scanned in full, since its fork points are restriction-relative."
  (with-current-buffer buffer
    (if (not (niwa-session-artifacts--prompt-cache-eligible-p))
        (cons (niwa-session-artifacts-collect-prompts buffer)
              (niwa-session-artifacts-fork-point-spans buffer))
      (let* ((origin (niwa-session-artifacts-content-start buffer))
             (cache (niwa-session-artifacts--prompt-cache-ensure origin))
             (source (niwa-session-artifacts--prompt-cache-source cache))
             (marker (niwa-transcript-source-origin source))
             (derived (niwa-session-artifacts--prompt-cache-origin cache))
             (d (if derived (niwa-transcript-source-change source) 0))
             (full (or (null derived) (eql d 0)
                       (not (eql (marker-position marker) origin)))))
        (when full
          (set-marker marker origin)
          (setq d 0))
        (let* ((prefix (buffer-substring (point-min) origin))
               (audit (niwa-session-artifacts--audit-spans-cached
                       cache origin d full prefix))
               ;; The parser's neighbor repair reads the same fork-point
               ;; spans: hand it the cached ones instead of a whole scan.
               (prepared
                (cons t (cl-loop for span in (car audit)
                                 when (eq (plist-get (nth 2 span) :type)
                                          'fork-point)
                                 collect (list :record (nth 2 span)
                                               :start (+ origin (nth 0 span))
                                               :end (+ origin (nth 1 span))))))
               (derived-units
                (let ((niwa-transcript--prepared-audit-spans prepared))
                  (niwa-session-artifacts--prompt-units
                   cache origin d full)))
               (derived-units
                (if (eq derived-units 'mismatch)
                    (let ((niwa-transcript--prepared-audit-spans prepared))
                      (niwa-session-artifacts--prompt-units
                       cache origin 0 t))
                  derived-units))
               (units (car derived-units)))
          (setf (niwa-session-artifacts--prompt-cache-origin cache) origin
                (niwa-session-artifacts--prompt-cache-units cache) units
                (niwa-session-artifacts--prompt-cache-spans cache) (car audit)
                (niwa-session-artifacts--prompt-cache-prefix cache) prefix
                (niwa-session-artifacts--prompt-cache-unresolved cache) (cdr audit)
                (niwa-session-artifacts--prompt-cache-frontier cache)
                (cdr derived-units))
          (niwa-transcript-source-accept source)
          (cons (niwa-session-artifacts--prompts-from-units
                 units origin)
                (cl-loop for span in (car audit)
                         when (eq (plist-get (nth 2 span) :type) 'fork-point)
                         collect (append
                                  (copy-sequence (nth 2 span))
                                  (list :record-start (+ origin (nth 0 span))
                                        :transcript-cutoff
                                        (+ origin (nth 1 span)))))))))))

(defun niwa-session-artifacts--prompt-count-in-text (text)
  "Return the number of user prompt markers detected in TEXT."
  (if (string-empty-p (or text ""))
    0
    (with-temp-buffer
      (let ((org-agenda-file-menu-enabled nil))
        (org-mode))
      (insert text)
      (length (niwa-session-artifacts-collect-prompts
               (current-buffer))))))

(defun niwa-session-artifacts--segment-tail-prompt-count ()
  "Return the copied-tail prompt count recorded on the current segment."
  (if (derived-mode-p 'org-mode)
      (max 0 (string-to-number
              (or (niwa-transcript-leading-property
                   "NIWA_SEGMENT_TAIL_PROMPTS")
                  "0")))
    0))

(defun niwa-session-artifacts--new-fork-point-id (session)
  "Return a fresh stable fork-point identity for SESSION."
  (substring
   (secure-hash
    'sha256
    (format "%S"
            (list (niwa-session-session-id session)
                  (current-time)
                  (random most-positive-fixnum)
                  (emacs-pid))))
   0 32))

(defun niwa-session-artifacts-fork-point-spans (buffer)
  "Return durable fork-point records and source spans from BUFFER.
Respect BUFFER's current restriction and record provenance."
  (with-current-buffer buffer
    (mapcar
     (lambda (span)
       (append (copy-sequence (plist-get span :record))
               (list :record-start (plist-get span :start)
                     :transcript-cutoff (plist-get span :end))))
     (niwa-transcript-audit-buffer-spans
      'fork-point (point-min) (point-max)))))

(defun niwa-session-artifacts-fork-point-at-source
    (buffer source-start source-end &optional fork-points)
  "Return the stable fork point inside BUFFER source bounds.

SOURCE-START and SOURCE-END must bound one rendered assistant turn.
FORK-POINTS, when supplied, is a projection-scoped vector of provenance-checked
records in source order, built under the projection's source restriction."
  (let* ((points (or fork-points
                     (vconcat (niwa-session-artifacts-fork-point-spans buffer))))
         (low 0)
         (high (length points)))
    ;; Find the first record at or after this turn's start.  Audit spans are
    ;; disjoint and ordered, so if it ends after this turn no later one fits.
    (while (< low high)
      (let ((middle (/ (+ low high) 2)))
        (if (< (plist-get (aref points middle) :record-start) source-start)
            (setq low (1+ middle))
          (setq high middle))))
    (when (< low (length points))
      (let ((record (aref points low)))
        (when (<= (plist-get record :transcript-cutoff) source-end)
          record)))))

(defun niwa-session-artifacts--prompt-fork-point
    (prompt next-position fork-points)
  "Return PROMPT's fork point before NEXT-POSITION from vector FORK-POINTS.
FORK-POINTS ascend by `:record-start', so the first one after the prompt is
the only candidate: later ones start later still."
  (let ((low 0)
        (high (length fork-points))
        (position (plist-get prompt :pos)))
    (while (< low high)
      (let ((middle (/ (+ low high) 2)))
        (if (> (plist-get (aref fork-points middle) :record-start) position)
            (setq high middle)
          (setq low (1+ middle)))))
    (when (< low (length fork-points))
      (let ((fork-point (aref fork-points low)))
        (and (or (null next-position)
                 (< (plist-get fork-point :record-start) next-position))
             fork-point)))))

(defun niwa-session-artifacts--continued-prompt (session)
  "Return SESSION's latest prompt entry when its turn continues in this segment.
Auto-compaction during a turn carries the turn into a new segment, so its
response ends in a segment that has no prompt of its own and its entry in the
earlier segment never receives a fork point there."
  (let* ((segment (or (niwa-session-current-segment session) 1))
         (index (niwa-session-prompt-index session))
         latest)
    (unless (cdr (assoc segment index))
      (dolist (entry index)
        (when (< (car entry) segment)
          (dolist (prompt (cdr entry))
            (when (or (null latest)
                      (> (plist-get prompt :cum-turn)
                         (plist-get latest :cum-turn)))
              (setq latest prompt)))))
      (and latest (not (plist-get latest :fork-point-id)) latest))))

(defun niwa-session-artifacts--continuation-start (buffer)
  "Return where a turn continued into BUFFER's segment resumes.
That is after the inherited compaction summary, or at the last prompt the
compaction copied forward."
  (with-current-buffer buffer
    (let ((tail (niwa-session-artifacts--segment-tail-prompt-count)))
      (or (and (> tail 0)
               (plist-get (nth (1- tail)
                               (niwa-session-artifacts-collect-prompts
                                buffer))
                          :pos))
          (plist-get (niwa-session-artifacts-segment-summary-bounds) :end)
          (point-min)))))

(defun niwa-session-artifacts--record-continuation
    (session buffer fork-points)
  "Record the continued turn's fork point from FORK-POINTS in SESSION's index.
The entry gains `:continuation' naming the current segment, where in BUFFER
the turn resumes, and its fork point, so a completed turn that spans an
auto-compaction is still indexed.  Other consumers keep the entry unchanged."
  (when-let* ((prompt (niwa-session-artifacts--continued-prompt session))
              ((not (plist-get prompt :continuation)))
              (segment (or (niwa-session-current-segment session) 1))
              (fork-point
               (cl-find-if (lambda (candidate)
                             (and (eql (plist-get candidate :cum-turn)
                                       (plist-get prompt :cum-turn))
                                  (eql (plist-get candidate :segment) segment)))
                           fork-points)))
    (plist-put prompt :continuation
               (list :segment segment
                     :start (niwa-session-artifacts--continuation-start
                             buffer)
                     :fork-point-id (plist-get fork-point :fork-point-id)
                     :transcript-cutoff
                     (plist-get fork-point :transcript-cutoff)))))

(defun niwa-session-artifacts-update-prompt-index (session buffer)
  "Refresh the live segment's prompt list in SESSION from BUFFER's contents.

Operates only on the current segment; previous (finalized) segments
keep their pre-recorded entries.  Idempotent -- safe to call on every
save.

Each prompt plist gets a `:cum-turn' field equal to the prompt's
sequence number across the entire session.  Prompts copied forward as
compaction tail are skipped because they are already indexed in the
predecessor segment.  The cumulative
number is what `:file-snapshots' is keyed by, so the restore plan
can map a picker selection to the checkpoint from immediately before
that prompt's model turn."
  (let* ((current-seg (or (niwa-session-current-segment session) 1))
         (index       (niwa-session-prompt-index session))
         (cell        (assoc current-seg index))
         (offset
          (cl-loop for (seg . prompts) in index
                   when (< seg current-seg)
                   sum (length prompts)))
         (derived     (niwa-session-artifacts--prompt-derive buffer))
         (raw-all     (car derived))
         (tail-count  (with-current-buffer buffer
                        (niwa-session-artifacts--segment-tail-prompt-count)))
         (skip-count  (min tail-count (length raw-all)))
         (raw         (nthcdr skip-count raw-all))
         (fork-points (cdr derived))
         (fork-vector (vconcat fork-points))
         (with-cum    (cl-loop for remaining on raw
                               for p = (car remaining)
                               for next-position =
                               (plist-get (cadr remaining) :pos)
                               for turn from 1
                               collect
                               (let ((copy (copy-sequence p)))
                                 (plist-put copy :turn turn)
                                 (plist-put copy :file-turn
                                            (+ skip-count turn))
                                 (plist-put copy :cum-turn (+ offset turn))
                                 (when-let* ((fork-point
                                              (niwa-session-artifacts--prompt-fork-point
                                               p next-position fork-vector)))
                                   (dolist (key '(:fork-point-id
                                                  :transcript-cutoff))
                                     (plist-put copy key
                                                (plist-get fork-point key))))
                                 copy))))
    (if cell
        (setcdr cell with-cum)
      (setf (niwa-session-prompt-index session)
            (cons (cons current-seg with-cum) index)))
    (unless with-cum
      (niwa-session-artifacts--record-continuation
       session buffer fork-points))))

(defun niwa-session-artifacts--ensure-latest-fork-point
    (session buffer)
  "Attach a durable fork-point marker to SESSION's latest response in BUFFER.
A turn continued into this segment by auto-compaction gets its marker here
too, after the response it finished with."
  (let* ((segment (or (niwa-session-current-segment session) 1))
         (own (car (last (cdr (assoc
                               segment
                               (niwa-session-prompt-index session))))))
         (continued
          (unless own
            (when-let* ((prompt (niwa-session-artifacts--continued-prompt
                                 session))
                        ((not (plist-get prompt :continuation)))
                        ;; Only the turn settling now continues here.
                        (request (with-current-buffer buffer
                                   (bound-and-true-p niwa--current-request)))
                        ((eql (niwa-request-turn request)
                              (plist-get prompt :cum-turn))))
              prompt)))
         (prompt (or own continued)))
    (when (and prompt (not (plist-get prompt :fork-point-id)))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (if continued
                         (niwa-session-artifacts--continuation-start buffer)
                       (plist-get prompt :pos)))
          (when (text-property-search-forward 'gptel 'response t)
            (goto-char (point-max))
            (insert
             (niwa--format-hook-audit-record
              (list :type 'fork-point
                    :fork-point-id
                    (niwa-session-artifacts--new-fork-point-id session)
                    :segment segment
                    :turn (plist-get prompt :turn)
                    :file-turn (plist-get prompt :file-turn)
                    :cum-turn (plist-get prompt :cum-turn)
                    :captured-file-turn (plist-get prompt :cum-turn))))
            t))))))

(defun niwa-session-artifacts--newer-prompt-p (candidate incumbent)
  "Return non-nil when CANDIDATE is newer than INCUMBENT."
  (or (null incumbent)
      (> (plist-get candidate :cum-turn)
         (plist-get incumbent :cum-turn))))

(defun niwa-session-artifacts--latest-user-message-from-index (index)
  "Return the newest non-empty prompt preview from prompt INDEX, or nil."
  (let (best)
    (dolist (entry index)
      (let ((segment (car entry)))
        (dolist (prompt (cdr entry))
          (when (consp prompt)
            (let ((preview (plist-get prompt :preview)))
              (when (and (stringp preview)
                         (not (string-empty-p (string-trim preview))))
                (let ((candidate (copy-sequence prompt)))
                  (plist-put candidate :segment segment)
                  (when (niwa-session-artifacts--newer-prompt-p
                         candidate best)
                    (setq best candidate)))))))))
    (plist-get best :preview)))


;;
(provide 'niwa-prompt-index)
;;; niwa-prompt-index.el ends here
