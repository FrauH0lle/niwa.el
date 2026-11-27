;;; niwa-session-diagnostics.el --- Best-effort session diagnostic appends -*- lexical-binding: t -*-

;;; Commentary:

;; Collect and retry diagnostic appends behind the session lease proof.

;;; Code:

(require 'cl-lib)
(require 'niwa-session-control-fs)
(require 'niwa-session-durability)
(require 'niwa-structs)
(require 'niwa-utilities)

(autoload 'niwa-session-codec-portable-authority-p "niwa-session-codec")
(autoload 'niwa-session-publication--artifact-for-session "niwa-session-publication")
(autoload 'niwa-session-publication--publish-critical-batches "niwa-session-publication")
(autoload 'niwa-session-recovery-refresh "niwa-session-recovery")

(defvar niwa-session-publication--diagnostic-batch nil
  "The session, state and collected appends of a diagnostic batch.

The value is (SESSION STATE . APPENDS).  `open' means SESSION's lease can
carry diagnostics: each append is collected, newest first, as (PATH .
CONTENT), and the batch writes them together when it ends.  `unavailable'
means the batch established that SESSION's lease cannot carry diagnostics
now, so its appends decline without re-testing.")

(defvar niwa-session-publication--diagnostic-backlog
  (make-hash-table :test #'eq :weakness 'key)
  "Session to (IN-FLIGHT . APPENDS): its diagnostics not yet on the target.
APPENDS, oldest first, are appends a program did not write and appends
collected while an earlier program was still running; the next batch
writes them first.")

(defconst niwa-session-publication--diagnostic-backlog-max-bytes
  (* 8 1024 1024)
  "Largest diagnostic backlog kept for a session whose appends keep failing.")

(defun niwa-session-publication--diagnostic-ready-p (session)
  "Return non-nil when SESSION's lease can carry diagnostics right now.

Its owned lease must also be one whose committed bytes this client knows, so
each append can prove ownership in its own program; a renewal supplies them."
  (niwa-session-recovery-refresh session)
  (not (or (niwa-session-pending-publication session)
           (niwa-session-publication-active-p session)
           (not (or (and (niwa-session-durability-lease-owned-p session)
                         (plist-get (niwa-session-lease session) :bytes))
                    (niwa-session-durability-lease-renew session)))
           (not (niwa-session-durability-lease-owned-p session)))))

(defun niwa-session-publication-call-with-diagnostic-batch (session function)
  "Call FUNCTION with SESSION's diagnostic appends sharing one admission.

The recovery refresh and lease check run once, and each append then proves
ownership inside its own target program.  Diagnostics used to reserve the
lease around the appends -- a renewal, a reservation write, and a release
write around one or a few appends of best-effort data nothing reads live --
which cost several control programs per flush point on a remote target.

Publication is held active while FUNCTION runs, so a critical publisher
reached from a sentinel during an append queues instead of nesting inside
it; its batches are published through the ordinary reserved path once
FUNCTION returns.

The appends are written together once FUNCTION returns, as one program
that `niwa-session-publication--write-diagnostics' runs without the
editor waiting for it where the target allows.  An append it does not
write stays in the session's backlog for the next batch."
  (cond
   ((eq (car-safe niwa-session-publication--diagnostic-batch) session)
    (funcall function))
   ;; A session that does not publish through the lease writes its
   ;; diagnostics directly, so there is no lease to prove.
   ((not (niwa-session-codec-portable-authority-p session))
    (funcall function))
   (t
    (niwa-session-durability-with-transaction
      (if (not (niwa-session-publication--diagnostic-ready-p session))
          (let ((niwa-session-publication--diagnostic-batch
                 (list session 'unavailable)))
            (funcall function))
        (let ((batch (list session 'open)))
          (prog1 (niwa-session-durability-call-with-held-lease
                  session
                  (lambda ()
                    (prog1 (let ((niwa-session-publication--diagnostic-batch batch))
                             (funcall function))
                      ;; Started inside the hold: where the program runs
                      ;; synchronously, a critical publisher reached from a
                      ;; sentinel meanwhile still queues instead of nesting.
                      (niwa-session-publication--write-diagnostics
                       session (reverse (cddr batch))))))
            ;; A critical publisher may have queued meanwhile.  It retains
            ;; precedence.
            (when (niwa-session-publication-queue session)
              (niwa-session-publication--publish-critical-batches
               session nil)))))))))

(defun niwa-session-publication--write-diagnostics (session appends)
  "Write SESSION's diagnostic backlog, then APPENDS, as one program.

Each (PATH . CONTENT) is an optional append behind the lease proof, so one
unwritable log does not stop the others.  On a target that allows a direct
pipe nothing waits for the program; an append it did not write, and every
append collected while it runs, joins the backlog the next batch writes
first, which keeps each log in order.  The backlog is bounded: past
`niwa-session-publication--diagnostic-backlog-max-bytes' the oldest
appends are dropped with a warning.

Where the editor is about to exit (`niwa-session-control-fs--async-wait'),
an append still in flight is waited for first, so what it leaves and APPENDS
are written now instead of queueing for a batch that never comes."
  (let ((state (or (gethash session niwa-session-publication--diagnostic-backlog)
                   (puthash session (cons nil nil)
                            niwa-session-publication--diagnostic-backlog)))
        entries)
    (when (and (car state) niwa-session-control-fs--async-wait)
      (niwa-session-control-fs-wait-async))
    (setq entries (append (cdr state) appends))
    (cond
     ((null entries))
     ((car state) (setcdr state entries))
     (t
      (setcar state t)
      (setcdr state nil)
      (niwa-session-durability-run-owned-program-async
       session
       (mapcar (lambda (entry)
                 (list :op 'append
                       :path (niwa-session-control-fs-physical-path (car entry))
                       :content (cdr entry)
                       :optional t))
               entries)
       (lambda (results error)
         (when error
           (niwa--warn-once
            (niwa-diagnostic-scope session)
            (list 'diagnostic-append (niwa-session-save-path session))
            "Diagnostic append failed, retaining for retry: %s"
            (error-message-string error)))
         (cl-loop for entry in entries
                  for result in results
                  when (eq 'failed (plist-get result :status))
                  do (niwa--warn-once
                      (niwa-diagnostic-scope session)
                      (list 'diagnostic-append (car entry))
                      "Diagnostic append failed for %s, retaining for retry: %s"
                      (car entry)
                      (or (plist-get result :diagnostic) "target refused the append")))
         (let ((unwritten (cl-loop for entry in entries
                                   for index from 0
                                   unless (eq 'ok (plist-get (nth index results) :status))
                                   collect entry)))
           (setcar state nil)
           (setcdr state (niwa-session-publication--bound-backlog
                          session (append unwritten (cdr state)))))))))))

(defun niwa-session-publication-diagnostic-backlog-p (session)
  "Return non-nil when SESSION has diagnostics waiting for the next batch."
  (and (cdr (gethash session niwa-session-publication--diagnostic-backlog)) t))

(defun niwa-session-publication--bound-backlog (session entries)
  "Return SESSION's diagnostic backlog ENTRIES, oldest dropped past its bound."
  (let ((bytes 0) kept)
    (dolist (entry (reverse entries))
      (setq bytes (+ bytes (string-bytes (cdr entry))))
      (if (<= bytes niwa-session-publication--diagnostic-backlog-max-bytes)
          (push entry kept)
        (niwa--warn-once
         (niwa-diagnostic-scope session)
         (list 'diagnostic-backlog (niwa-session-save-path session))
         "Dropping diagnostics that could not be written to %s"
         (niwa-session-save-path session))))
    kept))

(defun niwa-session-publication-append-diagnostic (session path content)
  "Append diagnostic CONTENT to PATH for portable project SESSION.

Return nil when the lease cannot carry a diagnostic right now -- a critical
publication is active or pending, renewal is momentarily unavailable, or the
lease is waiting to be reacquired -- so the caller retains CONTENT for a
later retry.  A diagnostic is best-effort data nothing reads live, so an
unavailable moment is a quiet retry, not an error echoed per attempt.
Diagnostic failure never creates critical pending publication.

Otherwise the append is taken, returning non-nil: it is written with the
rest of its batch as one pinned program carrying only the deltas, behind an
ownership proof in the same program, and an append that program does not
write is retried with the next batch.  Republishing the whole file per
flush was quadratic in stream size and dominated remote-save allocations
once a log grew to megabytes."
  (unless (and (stringp path) (stringp content))
    (error "Diagnostic publication requires string path and content"))
  (let ((batch niwa-session-publication--diagnostic-batch))
    (cond
     ((not (eq (car-safe batch) session))
      (let (appended)
        (niwa-session-publication-call-with-diagnostic-batch
         session
         (lambda ()
           (setq appended
                 (niwa-session-publication-append-diagnostic
                  session path content))))
        appended))
     ((eq (cadr batch) 'unavailable) nil)
     (t
      ;; Validate that the path stays inside the session before any
      ;; target I/O; :content is not staged, only checked for shape.
      (niwa-session-publication--artifact-for-session
       session (list :path path :content content))
      (setcdr (cdr batch) (cons (cons path content) (cddr batch)))
      t))))

(provide 'niwa-session-diagnostics)
;;; niwa-session-diagnostics.el ends here
