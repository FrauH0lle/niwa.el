;;; niwa-view-mailbox.el --- Mailbox delivery presentation for views -*- lexical-binding: t -*-

;;; Commentary:

;; Presents mailbox deliveries (`<agent-message>' and `<agent-result>'
;; records) in view transcripts: delivery cards with sender attribution,
;; background-event notices, collapsed long bodies and the gutter.
;; `niwa-transcript' recognizes the canonical records; this module decides
;; how each is shown.  `niwa-view-render' requires it and calls it while
;; projecting turns; fold application after projection stays with the
;; renderer (ADR 0119).

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'niwa-utilities)
(require 'niwa-view-tool-render)

;; `niwa-tool-exec'
(declare-function niwa-tool-exec-completion "niwa-tool-exec"
                  (termination outcome))
(autoload 'niwa-tool-exec-completion "niwa-tool-exec")

;; `niwa-transcript'
(declare-function niwa-transcript--mailbox-any-block-at-point
		  "niwa-transcript" (limit))
(declare-function niwa-transcript--mailbox-find-close
		  "niwa-transcript" (open-regexp close-tag limit))

;; `niwa-view-agent'
(declare-function niwa-view--insert-attribution
                  "niwa-view-agent"
                  (agent-path))
(defvar niwa-view--agent-path)

;; `niwa-view-disclosure'
(declare-function niwa-view-disclosure-collapse-mailbox-body
                  "niwa-view-disclosure" (start end))
(declare-function niwa-view-disclosure-mailbox-hint
                  "niwa-view-disclosure" (line-count))
(declare-function niwa-view-disclosure-mailbox-line-count
                  "niwa-view-disclosure" (start end))

;; `niwa-view-render'
(declare-function niwa-view--debug-log "niwa-view-render" (event &rest data))


;;
;;; Customization

(defcustom niwa-view-mailbox-collapse-line-threshold 0
  "Mailbox delivery bodies longer than this many lines start collapsed.
Shorter bodies render fully expanded."
  :type 'integer
  :group 'niwa)


;;
;;; Delivery cards

(defun niwa-view--style-mailbox-body (start end)
  "Style mailbox body text between START and END.
Adds a display-only gutter to payload lines and returns END.
Wrapped text aligns after the gutter.  Leading/trailing blank structural lines
are left bare, while blank lines between payload lines keep the gutter."
  (let ((end-marker (copy-marker end t))
        lines
        first
        last
        index)
    (save-excursion
      (goto-char start)
      (setq index 0)
      (while (< (point) (marker-position end-marker))
        (let* ((line-start (point))
               (line-end (min (line-end-position)
                              (marker-position end-marker)))
               (nonempty
                (string-match-p
                 "\\S-"
                 (buffer-substring-no-properties line-start line-end))))
          (push (cons (copy-marker line-start) nonempty) lines)
          (when nonempty
            (unless first
              (setq first index))
            (setq last index))
          (setq index (1+ index))
          (forward-line 1)))
      (when first
        (setq lines (nreverse lines))
        (cl-loop for line in lines
                 for n from 0
                 when (and (>= n first) (<= n last))
                 do
                 (goto-char (car line))
                 (add-text-properties
                  (point) (min (1+ (line-end-position)) end)
                  `(line-prefix
                    ,(concat (niwa-view--rendering-indent-blank)
                             (propertize "    │ " 'font-lock-face
                                         'niwa-view-mailbox-gutter))
                    wrap-prefix
                    ,(concat (niwa-view--rendering-indent-blank) "      ")
                    rear-nonsticky (line-prefix wrap-prefix)))
                 (when (cdr line)
                   (put-text-property
                    (point) (line-end-position)
                    'font-lock-face 'niwa-view-mailbox-body)))))
    (dolist (line lines)
      (set-marker (car line) nil))
    (prog1 (marker-position end-marker)
      (set-marker end-marker nil))))

(defun niwa-view--mail-attribute (open-tag attribute)
  "Return unescaped ATTRIBUTE from mailbox OPEN-TAG, or nil."
  (save-match-data
    (when (string-match (concat "\\s-" (regexp-quote attribute)
                                "=\"\\([^\"]*\\)\"") open-tag)
      (let ((text (match-string 1 open-tag)))
        (dolist (entity '(("&quot;" . "\"") ("&apos;" . "'") ("&lt;" . "<")
                          ("&gt;" . ">") ("&amp;" . "&")))
          (setq text (string-replace (car entity) (cdr entity) text)))
        text))))

(defun niwa-view--mail-notice (open-tag kind sender body)
  "Return (LABEL . FACE) for a background mail notice, or nil.
OPEN-TAG carries trusted metadata; BODY supplies only the event preview.
KIND and SENDER identify the mailbox record.  Payload text never determines
whether a record is a terminal event."
  (let* ((windows (niwa-view--windows 'geometry))
         (width (max 20 (- (if windows
                              (apply #'min (mapcar #'window-body-width windows))
                            (window-body-width))
                          4)))
         (type (niwa-view--mail-attribute open-tag "type"))
         (description (niwa-view--mail-attribute open-tag "description"))
         (termination (niwa-view--mail-attribute open-tag "termination"))
         (outcome (niwa-view--mail-attribute open-tag "outcome"))
         (exit (niwa-view--mail-attribute open-tag "exit_code"))
         (completion (niwa-tool-exec-completion termination outcome))
         (verb (cdr completion))
         (label
          (cond
           ((eq kind 'agent-result)
            (format "Agent %s %s" sender
                    (cond ((equal outcome "completed") "finished")
                          ((equal outcome "errored") "failed")
                          ((equal outcome "interrupted") "interrupted")
                          (t "incomplete"))))
           ((equal type "MONITOR")
            (format "Monitor event: %s · %s"
                    (car (split-string (string-trim body) "\n"))
                    (or description "monitor")))
           ((equal type "MONITOR_TERMINAL")
            (let ((count (niwa-view--mail-attribute open-tag "event_count")))
              (format "Monitor \"%s\" %s%s" (or description "monitor") verb
                      (if count (format " · %s event%s" count
                                        (if (equal count "1") "" "s")) ""))))
           ((and (equal type "EXECUTION")
                 (equal (niwa-view--mail-attribute open-tag "kind") "eval"))
            (format "Background Eval %s%s" verb
                    (if exit (format " (exit code %s)" exit) "")))
           ((equal type "EXECUTION")
            (let* ((prefix "Background command \"")
                   (suffix (format "\" %s%s" verb
                                   (if exit (format " (exit code %s)" exit) "")))
                   (subject
                    (or (and description (not (string-empty-p description)) description)
                        (niwa-view--mail-attribute open-tag "command") "command")))
              (concat prefix
                      (niwa--truncate-display
                       (replace-regexp-in-string "[\n\r\t]+" " " subject)
                       (max 1 (- width (string-width prefix)
                                 (string-width suffix))) "…")
                      suffix))))))
    (when label
      (cons (niwa--truncate-display
             (replace-regexp-in-string "[\n\r\t]+" " " label)
             width "…")
            (if (eq (car completion) 'failed)
                'error 'niwa-view-tool-metadata)))))

(defun niwa-view--own-completion-mail-p (open-tag sender)
  "Return non-nil when OPEN-TAG from SENDER completes this transcript's own work.
Its completion line is the breadcrumb this transcript already recorded when
the work ended, so the delivery adds no second line.  A child agent's
forwarded completion has no local breadcrumb and keeps its notice."
  (and (member (niwa-view--mail-attribute open-tag "type")
               '("EXECUTION" "MONITOR_TERMINAL"))
       (equal sender (or niwa-view--agent-path "/root"))))

(defun niwa-view--decorate-mailbox-block
    (open-regex close-tag start end &optional kind)
  "Replace OPEN-REGEX/CLOSE-TAG regions from START to END with mailbox cards.
KIND identifies the mailbox block flavor.  Shared engine for
`<agent-message>' and `<agent-result>'
rendering.  OPEN-REGEX must capture the canonical sender path in match group
1.  Bodies between the matched open and close tags are preserved verbatim;
background notices summarize structured opener attributes.
Notice bodies start collapsed without a hint.  Ordinary message bodies
exceeding `niwa-view-mailbox-collapse-line-threshold' start collapsed
with a `[N lines collapsed]' hint.  The `mailbox-delivery' vtype wires
both kinds to the same disclosure controls."
  (save-excursion
    (let ((end-marker (copy-marker end t)))
      (unwind-protect
          (progn
            (goto-char start)
            (while
                (let (close)
                  (while
                      (and
                       (re-search-forward
                        open-regex (marker-position end-marker) t)
                       (not
                        (setq close
                              (save-match-data
                                (save-excursion
                                  (niwa-transcript--mailbox-find-close
                                   open-regex close-tag
                                   (marker-position end-marker))))))))
                  close)
              (let* ((open-start (match-beginning 0))
                     (open-end (match-end 0))
                     (sender (match-string-no-properties 1))
                     (own (and (not (eq kind 'agent-result))
                               (niwa-view--own-completion-mail-p
                                (match-string-no-properties 0) sender)))
                     (notice
                      (unless own
                        (niwa-view--mail-notice
                         (match-string-no-properties 0) kind sender
                         (buffer-substring-no-properties
                          open-end
                          (car (save-excursion
                                 (goto-char open-end)
                                 (niwa-transcript--mailbox-find-close
                                  open-regex close-tag end-marker)))))))
                     (attribution (niwa-view--insert-attribution sender))
                     (inhibit-read-only t))
                (if own
                    (let ((close (save-excursion
                                   (goto-char open-end)
                                   (niwa-transcript--mailbox-find-close
                                    open-regex close-tag
                                    (marker-position end-marker)))))
                      (goto-char (if close (cdr close) open-end))
                      (when (eq (char-after) ?\n) (forward-char 1))
                      (delete-region open-start (point)))
                (delete-region open-start open-end)
                (goto-char open-start)
                (let ((card-start (point))
                      (card-id (gensym "niwa-view-mailbox-")))
                  (insert "  ")
                  (insert (propertize
                           (if notice "● " "✉ ")
                           'font-lock-face
                           (if notice (cdr notice) 'niwa-view-mailbox-header)
                           'niwa-view-mailbox t))
                  (let ((label (propertize
                                (if notice (car notice) "message ")
                                'font-lock-face 'niwa-view-mailbox-header
                                'niwa-view-mailbox t)))
                    (insert (if (and notice (eq kind 'agent-result))
                                (niwa-view--buttonize-agent-header-label label sender)
                              label)))
                  (unless notice (insert attribution))
                  (insert "\n")
                  (niwa-view--apply-rendering-indent card-start (point))
                  (let ((body-start (point)))
                    (when-let* ((close
                                 (niwa-transcript--mailbox-find-close
                                  open-regex close-tag
                                  (marker-position end-marker))))
                      (let* ((body-end (car close))
                             (close-end (cdr close))
                             (body-line-count
                              (niwa-view-disclosure-mailbox-line-count
                               body-start body-end))
                             (long-body
                              (or (and notice t)
                                  (> body-line-count
                                     niwa-view-mailbox-collapse-line-threshold))))
                        (niwa-view--debug-log
                         'mailbox-decorate
                         :kind kind
                         :sender sender
                         :open-start open-start
                         :body-start body-start
                         :body-end body-end
                         :body-lines body-line-count
                         :long-body long-body
                         :preview
                         (replace-regexp-in-string
                          "\n" "\\\\n"
                          (buffer-substring-no-properties
                           body-start
                           (min body-end (+ body-start 120)))
                          t t))
                        (let ((styled-end
                               (niwa-view--style-mailbox-body
                                body-start body-end)))
                          (setq close-end (+ close-end
                                             (- styled-end body-end))
                                body-end styled-end))
                        (when long-body
                          (let* ((hint
                                  (propertize
                                   (if notice ""
                                     (niwa-view-disclosure-mailbox-hint
                                      body-line-count))
                                   'font-lock-face
                                   'niwa-view-attribution
                                   'niwa-view-mailbox-hint t))
                                 (hint-len (length hint)))
                            (goto-char body-start)
                            (when (eq (char-before) ?\n)
                              (backward-char))
                            (insert hint)
                            ;; The header inset predates the hint; a wrapped
                            ;; hint in a nested row must keep it too.
                            (niwa-view--apply-rendering-indent
                             (- (point) hint-len) (point))
                            (setq body-start (+ body-start hint-len)
                                  body-end (+ body-end hint-len)
                                  close-end (+ close-end hint-len)))
                          (add-text-properties
                           body-start body-end
                           (list 'niwa-view-mailbox-body t
                                 'niwa-view-type 'mailbox-delivery
                                 'niwa-view-collapsed t))
                          (niwa-view-disclosure-collapse-mailbox-body
                           body-start body-end))
                        (unless long-body
                          (add-text-properties
                           body-start body-end
                           '(niwa-view-mailbox-body t)))
                        (delete-region body-end close-end)
                        (goto-char body-end)
                        (remove-text-properties
                         card-start (point)
                         '(niwa-view-source nil
                           niwa-view-source-key nil
                           niwa-view-agent-handle-p nil
                           niwa-view-agent-status nil))
                        (remove-text-properties
                         body-start (point)
                         '(niwa-view-agent-path nil))
                        (add-text-properties
                         card-start (point)
                         (list 'niwa-view-type 'mailbox-delivery
                               'niwa-view-mailbox-card card-id
                               'niwa-view-mailbox-kind
                               (or kind 'agent-message)
                               'niwa-view-mailbox-agent-path sender
                               'niwa-view-mailbox-notice (and notice t)
                               'niwa-view-collapsed long-body))))))))))
        (set-marker end-marker nil)))))

(defun niwa-view--decorate-agent-result-blocks (start end)
  "Render agent result blocks from START to END as mailbox cards.
Delegates to `niwa-view--decorate-mailbox-block' so
`<agent-message>' and `<agent-result>' render uniformly: same
header, same collapse threshold, and the same vtype tag for downstream
TAB toggling."
  (niwa-view--decorate-mailbox-block
   "<agent-result\\s-+[^>]*sender=\"\\([^\"]+\\)\"[^>]*>"
   "</agent-result>"
   start end
   'agent-result))

(defun niwa-view--decorate-agent-message-blocks (start end)
  "Decorate canonical `<agent-message sender=PATH>' blocks from START to END.
Delegates to `niwa-view--decorate-mailbox-block' so the body
collapse threshold, click gating, and vtype tag are uniform with
`<agent-result>' rendering.

Multiple `<agent-message>' blocks in one user turn produce one mailbox
card each, in source order.  Non-matching prose in the same turn
remains as ordinary user text.

Point is restored with an advancing marker: callers sit at the end of the
block and must stay after its rendered card to stamp and append past it."
  (let ((return (copy-marker (point) t)))
    (unwind-protect
        (niwa-view--decorate-mailbox-block
         "<agent-message\\s-+[^>]*sender=\"\\([^\"]+\\)\"[^>]*>"
         "</agent-message>" start end 'agent-message)
      (goto-char return)
      (set-marker return nil))))

(defun niwa-view--mailbox-only-text-p (text)
  "Return non-nil if TEXT is only mailbox delivery blocks.

Pure mailbox turns are injected as user-role messages in the data
buffer for gptel, but the view must not render them as `You' turns."
  (and (stringp text)
       (with-temp-buffer
         (insert text)
         (goto-char (point-min))
         (let ((found nil)
               (ok t))
           (while (and ok (not (eobp)))
             (skip-chars-forward " \t\r\n")
             (if (eobp)
                 nil
               (if-let* ((block (niwa-transcript--mailbox-any-block-at-point
                                  (point-max))))
                   (progn
                     (setq found t)
                     (goto-char (plist-get block :close-end)))
                 (setq ok nil))))
           (and found ok)))))

(provide 'niwa-view-mailbox)
;;; niwa-view-mailbox.el ends here
