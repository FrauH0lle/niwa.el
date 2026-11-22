;;; niwa-collaboration-wake-ups.el --- Pending wake-ups in browser rooms -*- lexical-binding: t; -*-

;;; Commentary:
;; Project committed session wake-ups to each admitted peer at its room tier.

;;; Code:
(require 'niwa-schedule)
(require 'niwa-structs)
(require 'seq)
(require 'subr-x)

;; `niwa-collaboration'
(declare-function niwa-collaboration--room-list "niwa-collaboration" ())
(declare-function niwa-collaboration--send-guarded "niwa-collaboration" (room peer frame))

(defun niwa-collaboration--schedule-time-fields (due now)
  "Return browser timing fields for epoch seconds DUE relative to NOW."
  (let* ((system-time-locale "C")
         (pattern (cond ((= (time-to-days now) (time-to-days due)) "%H:%M")
                        ((equal (format-time-string "%Y" now) (format-time-string "%Y" due)) "%e %b %H:%M")
                        (t "%e %b %Y %H:%M"))))
    (list :due due :short (string-trim-left (format-time-string pattern due))
          :time (format-time-string "%F %H:%M %Z (UTC%z)" due))))

(defun niwa-collaboration--send-wake-ups (room peer guest)
  "Send ROOM's pending wake-ups to PEER at the admitted GUEST's tier."
  (when-let* ((session (plist-get room :session))
              (workspace (niwa-session-workspace session))
              (id (niwa-session-session-id session)))
    (let* ((now (funcall niwa-schedule-clock))
           (rows (mapcar
                  (lambda (entry)
                    (append (niwa-collaboration--schedule-time-fields (plist-get entry :due) now)
                            (when (plist-get guest :writable) (list :note (plist-get entry :note)))))
                  (niwa-schedule-pending-snapshot workspace id))))
      ;; 25 notes of 4000 chars fit a 1 MiB frame even at six JSON bytes per char.
      (cl-loop for rest on (or (seq-partition rows 25) '(nil)) do
               (niwa-collaboration--send-guarded
                room peer (list :t "pending-wake-ups" :records (vconcat (car rest))
                                :final (if (cdr rest) :json-false t)))))))

(defun niwa-collaboration--pending-wake-ups-changed (root session-id)
  "Publish pending wake-ups when ROOT's SESSION-ID commits a change."
  (when session-id
    (dolist (room (niwa-collaboration--room-list))
      (when-let* ((session (plist-get room :session))
                  ((equal session-id (niwa-session-session-id session)))
                  (workspace (niwa-session-workspace session))
                  ((equal root (directory-file-name (niwa-workspace-root workspace)))))
        (maphash (lambda (peer guest) (niwa-collaboration--send-wake-ups room peer guest))
                 (plist-get room :guests))))))

(add-hook 'niwa-schedule-pending-changed-functions #'niwa-collaboration--pending-wake-ups-changed)

(provide 'niwa-collaboration-wake-ups)
;;; niwa-collaboration-wake-ups.el ends here
