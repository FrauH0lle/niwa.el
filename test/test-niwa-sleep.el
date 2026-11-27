;;; test-niwa-sleep.el --- System sleep inhibitor tests -*- lexical-binding: t -*-

;;; Commentary:

;; Covers reason collection and the inhibitor process lifecycle.  A stand-in
;; command records the announced reasons in a file and then blocks on stdin
;; like the real inhibitor, so no test takes a real system sleep lock.

;;; Code:

(require 'niwa-sleep)
(require 'niwa-execution)
(require 'niwa-transport)
(require 'niwa-turn)
(require 'niwa-sandbox)
(require 'niwa-session-persistence)
(require 'niwa-schedule)
(require 'niwa-collaboration)
(require 'niwa-execution-test-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-execution-test-helpers"))
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defmacro test-niwa-sleep--with-state (&rest body)
  "Run BODY with inhibition enabled and private inhibitor state.
Everything is released afterwards."
  (declare (indent 0) (debug (body)))
  `(let ((niwa-sleep--timer nil)
         (niwa-sleep--process nil)
         (niwa-sleep--held-reasons nil)
         (niwa-sleep--failed nil)
         (niwa--warn-once-table (make-hash-table :test #'equal))
         (niwa-sleep-inhibit t)
         (niwa-sleep-inhibit-command niwa-sleep-inhibit-command))
     (unwind-protect (progn ,@body)
       (niwa-sleep-stop))))

(defun test-niwa-sleep--recording-command (file)
  "Return an inhibitor stand-in that writes its reasons to FILE."
  (list "sh" "-c" "echo \"$1\" > \"$2.tmp\" && mv \"$2.tmp\" \"$2\"; exec cat"
        "inhibitor" "%s" file))

(defun test-niwa-sleep--wait (predicate)
  "Wait up to five seconds for PREDICATE, then assert it."
  (let ((deadline (+ (float-time) 5)))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (should (funcall predicate))))

(defun test-niwa-sleep--announced (file)
  "Return the reasons the stand-in wrote to FILE, or nil."
  (and (file-exists-p file)
       (string-trim (with-temp-buffer
                      (insert-file-contents file)
                      (buffer-string)))))

(niwa-deftest niwa-sleep--reasons ()
  ,test
  (test)
  :doc "is empty when nothing runs"
  (cl-letf (((symbol-function 'niwa-busy-p) #'ignore)
            ((symbol-function 'niwa-execution-working-p) #'ignore)
            ((symbol-function 'niwa-collaboration-sleep-reasons) #'ignore))
    (should-not (niwa-sleep--reasons)))

  :doc "names a running turn, a live command and collaboration in order"
  (with-temp-buffer
    (setq-local niwa--turn-settlements-pending '(settlement))
    (cl-letf (((symbol-function 'niwa-execution-working-p) #'always)
              ((symbol-function 'niwa-collaboration-sleep-reasons)
               (lambda (now)
                 (should (floatp now))
                 (list "collaboration share active"))))
      (should (equal '("request running" "command running"
                       "collaboration share active")
                     (niwa-sleep--reasons)))))

  :doc "a failing reason source counts as no reason and warns"
  (let ((niwa--warn-once-table (make-hash-table :test #'equal))
        diagnostics)
    (cl-letf (((symbol-function 'niwa-busy-p) #'always)
              ((symbol-function 'niwa-execution-working-p)
               (lambda () (error "Registry corrupt"))))
      (niwa-test--with-captured-diagnostics diagnostics
        (should-not (niwa-sleep--reasons))))
    (should (string-match-p "Cannot determine whether to block system sleep: \
Registry corrupt"
                            diagnostics))))

(niwa-deftest niwa-sleep--reconcile ()
  ,test
  (test)
  :doc "holds, replaces and releases the inhibitor as reasons change"
  (test-niwa-sleep--with-state
    (let* ((directory (make-temp-file "niwa-sleep-" t))
           (file (file-name-concat directory "why"))
           (reasons '("request running"))
           (sentinel (symbol-function 'niwa-sleep--sentinel))
           sentinel-processes
           first)
      (unwind-protect
          (cl-letf (((symbol-function 'niwa-sleep--reasons)
                     (lambda () reasons))
                    ((symbol-function 'niwa-sleep--sentinel)
                     (lambda (process event)
                       (push process sentinel-processes)
                       (funcall sentinel process event))))
            (setq niwa-sleep-inhibit-command
                  (test-niwa-sleep--recording-command file))
            (niwa-sleep--reconcile)
            (setq first niwa-sleep--process)
            (should (process-live-p first))
            (should (timerp niwa-sleep--timer))
            (test-niwa-sleep--wait
             (lambda () (equal "request running"
                               (test-niwa-sleep--announced file))))
            ;; An unchanged reason set keeps the same process.
            (niwa-sleep--reconcile)
            (should (eq first niwa-sleep--process))
            (delete-file file)
            (setq reasons '("request running" "command running"))
            (niwa-sleep--reconcile)
            (should (process-live-p niwa-sleep--process))
            (should-not (eq first niwa-sleep--process))
            (should-not (process-live-p first))
            (test-niwa-sleep--wait
             (lambda () (equal "request running, command running"
                               (test-niwa-sleep--announced file))))
            ;; The replaced process's sentinel must not disown or fail its
            ;; successor.
            (test-niwa-sleep--wait
             (lambda () (memq first sentinel-processes)))
            (should (process-live-p niwa-sleep--process))
            (should-not niwa-sleep--failed)
            (should (equal reasons niwa-sleep--held-reasons))
            (let ((held niwa-sleep--process))
              (setq reasons nil)
              (niwa-sleep--reconcile)
              (should-not niwa-sleep--process)
              (should-not niwa-sleep--held-reasons)
              (should-not niwa-sleep--timer)
              (should-not (process-live-p held))))
        (delete-directory directory t))))

  :doc "releases a held inhibitor once inhibition is disabled"
  (test-niwa-sleep--with-state
    (cl-letf (((symbol-function 'niwa-sleep--reasons)
               (lambda () '("request running"))))
      (setq niwa-sleep-inhibit-command '("cat"))
      (niwa-sleep--reconcile)
      (let ((held niwa-sleep--process))
        (should (process-live-p held))
        (setq niwa-sleep-inhibit nil)
        (niwa-sleep--reconcile)
        (should-not niwa-sleep--process)
        (should-not niwa-sleep--timer)
        (should-not (process-live-p held)))))

  :doc "a failing inhibitor warns once and is retried only in a new episode"
  (test-niwa-sleep--with-state
    (let ((reasons '("request running"))
          diagnostics)
      (cl-letf (((symbol-function 'niwa-sleep--reasons)
                 (lambda () reasons)))
        (setq niwa-sleep-inhibit-command
              '("sh" "-c" "echo access denied; exit 1"))
        (niwa-test--with-captured-diagnostics diagnostics
          (niwa-sleep--reconcile)
          (test-niwa-sleep--wait (lambda () niwa-sleep--failed)))
        (should (string-match-p "Cannot block system sleep: .*access denied"
                                diagnostics))
        (should-not niwa-sleep--process)
        (niwa-sleep--reconcile)
        (should-not niwa-sleep--process)
        (should (timerp niwa-sleep--timer))
        (setq reasons nil)
        (niwa-sleep--reconcile)
        (should-not niwa-sleep--failed)
        (should-not niwa-sleep--timer)
        (setq reasons '("request running")
              niwa-sleep-inhibit-command '("cat"))
        (niwa-sleep--reconcile)
        (should (process-live-p niwa-sleep--process)))))

  :doc "a missing inhibitor program warns instead of signaling"
  (test-niwa-sleep--with-state
    (let (diagnostics)
      (cl-letf (((symbol-function 'niwa-sleep--reasons)
                 (lambda () '("request running"))))
        (setq niwa-sleep-inhibit-command
              (list (file-name-concat temporary-file-directory
                                      "niwa-no-such-inhibitor")))
        (niwa-test--with-captured-diagnostics diagnostics
          (niwa-sleep--reconcile))
        (should (string-match-p "Cannot block system sleep" diagnostics))
        (should niwa-sleep--failed)
        (should-not niwa-sleep--process)))))

(niwa-deftest niwa-sleep-update ()
  ,test
  (test)
  :doc "does nothing while inhibition is disabled"
  (test-niwa-sleep--with-state
    (setq niwa-sleep-inhibit nil)
    (niwa-sleep-update 'session 'buffer)
    (should-not niwa-sleep--timer))

  :doc "coalesces repeated calls into one pending evaluation"
  (test-niwa-sleep--with-state
    (setq niwa-sleep-inhibit-command '("cat"))
    (niwa-sleep-update)
    (let ((first niwa-sleep--timer))
      (niwa-sleep-update)
      (should (timerp niwa-sleep--timer))
      (should-not (memq first timer-list))
      (should (memq niwa-sleep--timer timer-list))))

  :doc "leaves a held inhibitor to the poll"
  (test-niwa-sleep--with-state
    (cl-letf (((symbol-function 'niwa-sleep--reasons)
               (lambda () '("request running"))))
      (setq niwa-sleep-inhibit-command '("cat"))
      (niwa-sleep--reconcile)
      (let ((poll niwa-sleep--timer))
        (niwa-sleep-update)
        (should (eq poll niwa-sleep--timer))
        (should (memq poll timer-list)))))

  :doc "a request from inside a TRAMP handler is held, not discarded"
  (test-niwa-sleep--with-state
    (let ((held nil))
      (cl-letf (((symbol-function 'niwa-transport-nested-p) #'always))
        (let ((niwa-transport--held-timers nil))
          (niwa-sleep-update)
          (setq held niwa-transport--held-timers)))
      (should (equal (list niwa-sleep--timer) held))
      (should-not (memq niwa-sleep--timer timer-list))
      (setq niwa-sleep--timer nil)))

  :doc "superseded or stopped held timers fire without effect"
  (test-niwa-sleep--with-state
    (let ((reconciled 0) held)
      (cl-letf (((symbol-function 'niwa-transport-nested-p) #'always)
                ((symbol-function 'niwa-sleep--reconcile)
                 (lambda () (cl-incf reconciled))))
        (let ((niwa-transport--held-timers nil))
          (niwa-sleep-update)
          (niwa-sleep-update)
          (setq held niwa-transport--held-timers))
        (should (= 2 (length held)))
        ;; Held timers fire even after cancellation: only the current runs.
        (dolist (timer held) (apply (timer--function timer) (timer--args timer)))
        (should (= 1 reconciled))
        (let ((niwa-transport--held-timers nil))
          (niwa-sleep-update)
          (setq held niwa-transport--held-timers))
        (niwa-sleep-stop)
        (dolist (timer held) (apply (timer--function timer) (timer--args timer)))
        (should (= 1 reconciled))))))

(niwa-deftest niwa-sleep-stop ()
  ,test
  (test)
  :doc "releases the inhibitor and cancels the poll"
  (test-niwa-sleep--with-state
    (cl-letf (((symbol-function 'niwa-sleep--reasons)
               (lambda () '("command running"))))
      (setq niwa-sleep-inhibit-command '("cat"))
      (niwa-sleep--reconcile)
      (let ((held niwa-sleep--process)
            (timer niwa-sleep--timer))
        (niwa-sleep-stop)
        (should-not niwa-sleep--process)
        (should-not niwa-sleep--timer)
        (should-not (memq timer timer-list))
        (should-not (process-live-p held))))))

(niwa-deftest niwa-sleep--set-inhibit ()
  ,test
  (test)
  :doc "Customize applies the switch to the current work at once"
  (test-niwa-sleep--with-state
    (cl-letf (((symbol-function 'niwa-sleep--reasons)
               (lambda () '("request running"))))
      (setq niwa-sleep-inhibit nil
            niwa-sleep-inhibit-command '("cat"))
      (funcall (get 'niwa-sleep-inhibit 'custom-set)
               'niwa-sleep-inhibit t)
      (should niwa-sleep-inhibit)
      (should (timerp niwa-sleep--timer))
      (niwa-sleep--reconcile)
      (let ((held niwa-sleep--process))
        (should (process-live-p held))
        (funcall (get 'niwa-sleep-inhibit 'custom-set)
                 'niwa-sleep-inhibit nil)
        (should-not niwa-sleep-inhibit)
        (should-not niwa-sleep--process)
        (should-not niwa-sleep--timer)
        (should-not (process-live-p held))))))


(niwa-deftest niwa-sleep-monitor-reason ()
  ,test
  (test)
  :doc "a live Monitor alone holds the inhibitor and ending it releases"
  (let* ((root (make-temp-file "niwa-sleep-monitor-" t))
         (session (test-niwa-execution--session root))
         (niwa-sandbox-mode 'off))
    (unwind-protect
        (cl-letf (((symbol-function 'niwa-busy-p) #'ignore)
                  ((symbol-function 'niwa-collaboration-sleep-reasons) #'ignore)
                  ((symbol-function 'niwa-schedule-sleep-reasons) #'ignore))
          (should-not (niwa-sleep--reasons))
          (niwa-execution-start-bash
           #'ignore :kind 'monitor :description "watch" :session session
           :owner "/root" :owner-context session
           :command '("sh" "-c" "sleep 30") :workdir root
           :writable-roots (list root)
           :artifact-directory (file-name-concat root "artifacts"))
          (should-not (niwa-execution-working-p))
          (should (equal '("monitor watching") (niwa-sleep--reasons)))
          (niwa-execution-teardown-session session)
          (should-not (niwa-sleep--reasons)))
      (niwa-execution-teardown-session session)
      (delete-directory root t))))

(niwa-deftest niwa-schedule-sleep-reasons ()
  ,test
  (test)
  :doc "a session wake-up holds inside the horizon, not outside, not when nil"
  (let* ((now 1000000.0)
         (niwa-schedule-clock (lambda () now))
         (niwa-schedule--buffers nil)
         (niwa-schedule--workspaces nil)
         (niwa-schedule--closed (list (cons "/tmp/s" (+ now 3600)))))
    (should (string-prefix-p "wake-up due "
                             (car (niwa-schedule-sleep-reasons 7200))))
    (should-not (niwa-schedule-sleep-reasons 1800))
    (should-not (niwa-schedule-sleep-reasons nil)))

  :doc "a workspace schedule never holds, however near"
  (let* ((now 1000000.0)
         (niwa-schedule-clock (lambda () now))
         (niwa-schedule--buffers nil)
         (niwa-schedule--closed nil)
         (niwa-schedule--workspaces (list (cons "/tmp/w" (+ now 60)))))
    (should-not (niwa-schedule-sleep-reasons 86400))))

(niwa-deftest niwa-schedule--arm-sleep-horizon ()
  ,test
  (test)
  :doc "a wake-up entering the horizon triggers an update with no other reason"
  (let* ((now 1000000.0)
         (niwa-schedule-clock (lambda () now))
         (niwa-schedule--buffers nil)
         (niwa-schedule--workspaces nil)
         (niwa-schedule--horizon-timer nil)
         (niwa-sleep-inhibit t)
         (niwa-sleep-wake-up-horizon 3600)
         (niwa-schedule--closed (list (cons "/tmp/s" (+ now 7200))))
         updates)
    (cl-letf (((symbol-function 'niwa-sleep-update)
               (lambda (&rest _) (push t updates))))
      (unwind-protect
          (progn
            (niwa-schedule--arm-sleep-horizon)
            (should (timerp niwa-schedule--horizon-timer))
            (should (< (abs (- 3600 (float-time
                                     (time-subtract
                                      (timer--time niwa-schedule--horizon-timer)
                                      nil))))
                       5))
            (should-not updates)
            ;; Already inside the horizon: update at once, no timer.
            (setq niwa-schedule--closed (list (cons "/tmp/s" (+ now 60))))
            (niwa-schedule--arm-sleep-horizon)
            (should updates)
            (should-not niwa-schedule--horizon-timer))
        (when (timerp niwa-schedule--horizon-timer)
          (cancel-timer niwa-schedule--horizon-timer)))))

  :doc "setting the horizon through Customize re-arms and updates at once"
  (let* ((now 1000000.0)
         (niwa-schedule-clock (lambda () now))
         (niwa-schedule--buffers nil)
         (niwa-schedule--workspaces nil)
         (niwa-schedule--horizon-timer nil)
         (niwa-sleep-inhibit t)
         (niwa-sleep-wake-up-horizon nil)
         (niwa-schedule--closed (list (cons "/tmp/s" (+ now 60))))
         updates)
    (cl-letf (((symbol-function 'niwa-sleep-update)
               (lambda (&rest _) (push t updates))))
      (unwind-protect
          (progn
            (customize-set-variable 'niwa-sleep-wake-up-horizon 3600)
            (should (= 3600 niwa-sleep-wake-up-horizon))
            ;; Armed (inside the horizon) plus the setter's own update.
            (should (= 2 (length updates))))
        (when (timerp niwa-schedule--horizon-timer)
          (cancel-timer niwa-schedule--horizon-timer)))))

  :doc "idle stays idle: no timer when inhibition is off or nothing is pending"
  (let ((niwa-schedule--buffers nil)
        (niwa-schedule--workspaces nil)
        (niwa-schedule--horizon-timer nil)
        (niwa-sleep-wake-up-horizon 3600)
        (niwa-schedule--closed (list (cons "/tmp/s" (+ (float-time) 7200)))))
    (let ((niwa-sleep-inhibit nil))
      (niwa-schedule--arm-sleep-horizon)
      (should-not niwa-schedule--horizon-timer))
    (let ((niwa-sleep-inhibit t) (niwa-schedule--closed nil))
      (niwa-schedule--arm-sleep-horizon)
      (should-not niwa-schedule--horizon-timer))))

(niwa-deftest niwa-sleep-inhibit-horizon-toggle ()
  ,test
  (test)
  :doc "toggling inhibition arms then cancels the horizon timer for a pending wake-up"
  (let* ((now 1000000.0)
         (niwa-schedule-clock (lambda () now))
         (niwa-schedule--buffers nil)
         (niwa-schedule--workspaces nil)
         (niwa-schedule--retrying nil)
         (niwa-schedule--horizon-timer nil)
         (niwa-sleep-inhibit nil)
         (niwa-sleep-wake-up-horizon (* 6 3600))
         (niwa-schedule--closed (list (cons "/tmp/s" (+ now (* 8 3600))))))
    (cl-letf (((symbol-function 'niwa-sleep-update) #'ignore))
      (unwind-protect
          (progn
            (customize-set-variable 'niwa-sleep-inhibit t)
            (should (timerp niwa-schedule--horizon-timer))
            (customize-set-variable 'niwa-sleep-inhibit nil)
            (should-not niwa-schedule--horizon-timer))
        (setq niwa-sleep-inhibit nil)
        (when (timerp niwa-schedule--horizon-timer)
          (cancel-timer niwa-schedule--horizon-timer))))))

(niwa-deftest niwa-schedule-reopen-retry-sleep ()
  ,test
  (test)
  :doc "a reopen retry never holds sleep; verified recovery within the horizon does"
  (let* ((now 1000000.0)
         (niwa-schedule-clock (lambda () now))
         (niwa-schedule--buffers nil)
         (niwa-schedule--workspaces nil)
         (niwa-schedule--retrying nil)
         (niwa-schedule--closed nil)
         (niwa--warn-once-table (make-hash-table :test #'equal))
         (stored (- now 10))
         diagnostics)
    (cl-letf (((symbol-function 'niwa-schedule--stored-due)
               (lambda (_directory) stored))
              ((symbol-function 'niwa-schedule--open-p) #'ignore)
              ((symbol-function 'niwa-session-persistence-restore)
               (lambda (_directory) (error "Store unreadable"))))
      (niwa-test--with-captured-diagnostics diagnostics
        (dotimes (_ 3)
          (let ((entry (car niwa-schedule--closed)))
            (setq niwa-schedule--closed (delq entry niwa-schedule--closed))
            (niwa-schedule--wake-closed "/tmp/s" now))
          (should (= 1 (length niwa-schedule--closed)))
          (should-not (niwa-schedule-sleep-reasons 86400))))
      ;; Recovery: the store reads and the wake-up is 30 minutes away.
      (setq stored (+ now 1800))
      (let ((entry (car niwa-schedule--closed)))
        (setq niwa-schedule--closed (delq entry niwa-schedule--closed))
        (niwa-schedule--wake-closed "/tmp/s" now))
      (should-not niwa-schedule--retrying)
      (should (niwa-schedule-sleep-reasons 3600))
      (should-not (niwa-schedule-sleep-reasons 600)))))

(provide 'test-niwa-sleep)
;;; test-niwa-sleep.el ends here
