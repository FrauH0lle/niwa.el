;;; test-niwa-view-mailbox.el --- Mailbox presentation interface tests -*- lexical-binding: t -*-

;;; Commentary:

;; One presentation interface for mailbox deliveries: classification, range
;; decoration, and the same cards in every delivery shape.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-disclosure)
(require 'niwa-view-mailbox)
(require 'niwa-view-redraw)
(require 'niwa-view-render)
(require 'niwa-view-stream)
(require 'niwa-view-tool-render)

(defconst niwa-view-mailbox-test--message
  "<agent-message type=\"MAIL\" sender=\"/root/a\">\nhello body\n</agent-message>\n"
  "An ordinary message from another agent.")

(defconst niwa-view-mailbox-test--result
  (concat "<agent-result sender=\"/root/w\" recipient=\"/root\" outcome=\"completed\">\n"
          "result body\n</agent-result>\n")
  "A child agent's result.")

(defconst niwa-view-mailbox-test--own
  (concat "<agent-message type=\"EXECUTION\" sender=\"/root\" execution_id=\"e1\" "
          "outcome=\"success\" exit_code=\"0\">\nterminal output\n</agent-message>\n")
  "The root transcript's own background completion.")

(defun niwa-view-mailbox-test--cards (text)
  "Return the mailbox card header lines in TEXT, in order."
  (seq-filter (lambda (line) (string-match-p "^ *[✉●] " line))
              (mapcar #'string-trim-right (split-string text "\n"))))

(defun niwa-view-mailbox-test--view-text ()
  "Return the current view's transcript text."
  (buffer-substring-no-properties (point-min) (niwa-view-transcript-tail)))

(niwa-deftest niwa-view-mailbox-delivery ()
  ,test
  (test)
  :doc "ordinary mail is a message; results and background events are notices"
  (with-temp-buffer
    (dolist (case '(("<agent-message type=\"MAIL\" sender=\"/root/a\">" agent-message "/root/a" message)
                    ("<agent-message sender=\"/root\">" agent-message "/root" message)
                    ("<agent-result sender=\"/root/w\" outcome=\"completed\">" agent-result "/root/w" notice)
                    ("<agent-result sender=\"/root\" type=\"EXECUTION\">" agent-result "/root" notice)
                    ("<agent-message type=\"MONITOR\" sender=\"/root\">" agent-message "/root" notice)
                    ("<agent-message type=\"EXECUTION\" sender=\"/root/w\">" agent-message "/root/w" notice)))
      (should (eq (nth 3 case)
                  (niwa-view-mailbox-delivery (nth 0 case) (nth 1 case) (nth 2 case))))))
  :doc "the receiving transcript's own completions are suppressed in root and agent views"
  (dolist (owner '(nil "/root/worker"))
    (dolist (type '("EXECUTION" "MONITOR_TERMINAL"))
      (with-temp-buffer
        (setq-local niwa-view--agent-path owner)
        (let ((sender (or owner "/root")))
          (should (eq 'suppressed
                      (niwa-view-mailbox-delivery
                       (format "<agent-message type=\"%s\" sender=\"%s\">" type sender)
                       'agent-message sender)))
          ;; Another transcript's completion keeps its notice.
          (should (eq 'notice
                      (niwa-view-mailbox-delivery
                       (format "<agent-message type=\"%s\" sender=\"/root/other\">" type)
                       'agent-message "/root/other"))))))))

(niwa-deftest niwa-view-mailbox-decorate ()
  ,test
  (test)
  :doc "an integer END still covers deliveries after a rewritten result"
  (with-temp-buffer
    (insert niwa-view-mailbox-test--result niwa-view-mailbox-test--message)
    (niwa-view-mailbox-decorate (point-min) (point-max))
    (should (equal '("● Agent /root/w finished"
                     "✉ message from /root/a [1 line collapsed]")
                   (mapcar #'string-trim
                           (niwa-view-mailbox-test--cards (buffer-string)))))
    (should-not (string-match-p "<agent-" (buffer-string))))
  :doc "deliveries after END stay untouched when an earlier card shortens the range"
  (with-temp-buffer
    (insert "<agent-result sender=\"/root/w\" outcome=\"completed\" padding=\""
            (make-string 120 ?x) "\">\nresult body\n</agent-result>\n"
            niwa-view-mailbox-test--message)
    (let ((end (point))
          (outside "<agent-message sender=\"/root/b\">\nOUT\n</agent-message>\n"))
      (insert outside)
      (niwa-view-mailbox-decorate (point-min) end)
      (should (string-suffix-p outside (buffer-string)))
      (should (equal 2 (length (niwa-view-mailbox-test--cards (buffer-string)))))))
  :doc "prose around deliveries stays, own completions vanish, point stays after the cards"
  (with-temp-buffer
    (insert "before\n" niwa-view-mailbox-test--message "middle\n"
            niwa-view-mailbox-test--own niwa-view-mailbox-test--result)
    (let ((end (point)))
      (insert "TAIL\n")
      (goto-char end)
      (niwa-view-mailbox-decorate (point-min) end))
    (should (looking-at "TAIL"))
    (let ((text (buffer-string)))
      (should (string-match-p
               "\\`before\n +✉ message from /root/a[^\n]*\n+middle\n +● Agent /root/w finished\n+TAIL\n\\'"
               (replace-regexp-in-string "\n *[^\n]*body\n" "\n" text)))
      (should-not (string-match-p "terminal output\\|<agent-" text)))))

(niwa-deftest niwa-view-mailbox-decorate/shapes ()
  ,test
  (test)
  :doc "mixed, pure, standalone and grouped deliveries show the same cards, live and rebuilt"
  (let ((records (concat niwa-view-mailbox-test--message
                         niwa-view-mailbox-test--own
                         niwa-view-mailbox-test--result))
        (expected nil))
    (dolist (shape '(mixed pure standalone grouped))
      (niwa-view-test--with-buffers
        (pcase shape
          ('mixed
           (niwa-view-test--insert-data data-buf (concat "Please look\n" records "thanks\n") nil))
          ('pure
           (niwa-view-test--insert-data data-buf records nil))
          ('standalone
           (niwa-view-test--insert-data data-buf "go\n" nil)
           (niwa-view-test--insert-data data-buf "Working.\n" 'response)
           (niwa-view-test--insert-data data-buf records nil))
          ('grouped
           (niwa-view-test--insert-data data-buf "go\n" nil)
           (dotimes (i 2)
             (niwa-view-test--insert-data
              data-buf
              (format "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n" i i)
              `(tool . ,(format "call_%d" i)))
             (when (= i 0)
               (niwa-view-test--insert-data data-buf records nil)))))
        (niwa-view-test--insert-data data-buf "Done.\n" 'response)
        (with-current-buffer data-buf
          (niwa-view-stream-render-response (point-min) (point-max)))
        (with-current-buffer view-buf
          (dolist (rebuild '(nil t))
            (when rebuild (niwa-view--full-rerender))
            (let* ((text (if (eq shape 'grouped)
                             ;; Open group children render through the
                             ;; production child-row path.
                             (let ((inhibit-read-only t)
                                   (start (marker-position niwa-view--status-marker)))
                               (goto-char start)
                               (dolist (seg (with-current-buffer data-buf
                                              (niwa-transcript-segments (point-min) (point-max))))
                                 (when (eq (car seg) 'mailbox)
                                   (when-let* ((entry (niwa-view--mailbox-activity-entry seg data-buf))
                                               (child (niwa-view--tool-group-child entry data-buf 0)))
                                     (niwa-view--insert-child-call-block
                                      child (plist-get child :source) 'derive ""))))
                               (prog1 (buffer-substring-no-properties start (point))
                                 (delete-region start (point))))
                           (niwa-view-mailbox-test--view-text)))
                   (cards (mapcar #'string-trim (niwa-view-mailbox-test--cards text))))
              (ert-info ((format "%s rebuild=%s\n%s" shape rebuild text))
                (should-not (string-match-p "terminal output\\|<agent-" text))
                (if expected
                    (should (equal expected cards))
                  (should (equal 2 (length cards)))
                  (setq expected cards))))))))))

(provide 'test-niwa-view-mailbox)
;;; test-niwa-view-mailbox.el ends here
