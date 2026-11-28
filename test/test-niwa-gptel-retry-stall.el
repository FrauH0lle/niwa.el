;;; test-niwa-gptel-retry-stall.el --- Stalled provider stream tests -*- lexical-binding: t -*-

;;; Commentary:
;; Real curl against a local server whose stream stops making semantic
;; progress while it keeps the connection alive, with real-clock timers.

;;; Code:

(require 'gptel-retry-helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "gptel-retry-helpers"))

(defun niwa-retry-stall-test--wait (predicate seconds)
  "Accept process output until PREDICATE holds or SECONDS pass."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (< (float-time) deadline) (not (funcall predicate)))
      (accept-process-output nil 0.01))
    (funcall predicate)))

(ert-deftest niwa-gptel-retry-stall/keepalives-after-first-delta-retry ()
  "Keep-alives after a first delta, or no headers at all, are a stall.
The request is retried once without the abandoned delta, and the watch ends."
  (dolist (keepalive (cons nil niwa-retry-test--keepalives))
    (ert-info ((format "keep-alive %S" keepalive))
      (niwa-retry-test--with-http
          (list (if keepalive
                    (niwa-retry-test--stream niwa-retry-test--first-delta nil keepalive)
                  #'ignore)
                niwa-retry-test--success)
        (niwa-retry-test--with-real-clock 0.4
          (gptel--fsm-transition fsm)
          (niwa-retry-test--drain-native))
        (should (= 2 (length requests)))
        (should (eq 'DONE (gptel-fsm-state fsm)))
        (should (string-prefix-p "Recovered answer" (with-current-buffer data-buf (buffer-string))))
        (should-not (string-search "First sentence" (with-current-buffer data-buf (buffer-string))))
        (should (equal '("stream_stalled")
                       (mapcar (lambda (error) (plist-get error :code))
                               (plist-get (gptel-fsm-info fsm) :niwa-retry-errors))))
        (should-not (niwa-retry-test--stall-timers))
        (should-not niwa-gptel-retry--states)))))

(ert-deftest niwa-gptel-retry-stall/slow-progress-is-not-a-stall ()
  "Text or tool-argument deltas spaced under the timeout keep a long stream."
  (dolist (delta '("event: response.output_text.delta\ndata: {\"delta\":\"more \"}\n\n"
                   "event: response.function_call_arguments.delta\ndata: {\"delta\":\"{\\\"a\"}\n\n"))
    (ert-info ((format "delta %S" delta))
      (niwa-retry-test--with-http
          (list (niwa-retry-test--stream
                 niwa-retry-test--first-delta
                 (append (apply #'append (make-list 8 (list delta (car niwa-retry-test--keepalives))))
                         (list niwa-retry-test--success))
                 nil 0.1))
        (niwa-retry-test--with-real-clock 0.4
          (let ((start (float-time)))
            (gptel--fsm-transition fsm)
            (niwa-retry-test--drain-native)
            ;; The stream outlasted the timeout several times over.
            (should (> (- (float-time) start) 1.2))))
        (should (= 1 (length requests)))
        (should (eq 'DONE (gptel-fsm-state fsm)))
        (should-not (plist-get (gptel-fsm-info fsm) :niwa-retry-errors))
        (should-not (niwa-retry-test--stall-timers))))))

(ert-deftest niwa-gptel-retry-stall/long-tool-is-not-a-stall ()
  "A tool running longer than the timeout is not provider silence."
  (let ((tool-response
         (concat "event: response.output_item.done\n"
                 "data: {\"item\":{\"type\":\"function_call\",\"call_id\":\"call-1\",\"name\":\"Slow\",\"arguments\":\"{}\"}}\n\n"
                 "event: response.completed\ndata: {\"response\":{\"status\":\"completed\"}}\n\n"))
        (tool-timers nil) (watched-during-tool nil))
    (niwa-retry-test--with-http (list tool-response niwa-retry-test--success)
      (unwind-protect
          (niwa-retry-test--with-real-clock 0.4
            (plist-put (gptel-fsm-info fsm) :tools
                       (list (gptel-make-tool
                              :name "Slow" :description "Outlasts the stall timeout"
                              :args nil :async t
                              :function
                              (lambda (callback)
                                (push (run-at-time 0.6 nil (lambda ()
                                                             (push (niwa-retry-test--stall-timers)
                                                                   watched-during-tool)))
                                      tool-timers)
                                (push (run-at-time 1.2 nil callback "Done") tool-timers)))))
            (gptel--fsm-transition fsm)
            (niwa-retry-test--drain-native))
        (mapc #'cancel-timer tool-timers))
      (should (= 2 (length requests)))
      (should (eq 'DONE (gptel-fsm-state fsm)))
      (should (equal '(nil) watched-during-tool))
      (should-not (plist-get (gptel-fsm-info fsm) :niwa-retry-errors))
      (should-not (niwa-retry-test--stall-timers)))))

(ert-deftest niwa-gptel-retry-stall/disabled-retries-end-at-first-stall ()
  "Without recovery, the first stall ends the request honestly."
  (let ((niwa-gptel-retry-max-retries 0))
    (niwa-retry-test--with-http
        (list (niwa-retry-test--stream niwa-retry-test--first-delta nil
                                       (car niwa-retry-test--keepalives))
              niwa-retry-test--success)
      (niwa-retry-test--with-real-clock 0.4
        (gptel--fsm-transition fsm)
        (niwa-retry-test--drain-native))
      (should (= 1 (length requests)))
      (should (eq 'ERRS (gptel-fsm-state fsm)))
      (should-not gptel--request-alist)
      (should-not (niwa-retry-test--stall-timers)))))

(ert-deftest niwa-gptel-retry-stall/nil-timeout-disables-detection ()
  "Negative control: with detection off, the stream waits as before."
  (niwa-retry-test--with-http
      (list (niwa-retry-test--stream niwa-retry-test--first-delta nil
                                     (car niwa-retry-test--keepalives))
            niwa-retry-test--success)
    (niwa-retry-test--with-real-clock nil
      (gptel--fsm-transition fsm)
      (should-not (niwa-retry-stall-test--wait (lambda () (= 2 (length requests))) 1.5)))
    (should (eq 'TYPE (gptel-fsm-state fsm)))
    (should (string-search "First sentence" (with-current-buffer data-buf (buffer-string))))
    (should-not (niwa-retry-test--stall-timers))))

(defun niwa-retry-stall-test--orphans ()
  "Return live gptel curl processes that gptel never registered."
  (seq-filter (lambda (process)
                (and (string-prefix-p "gptel-curl" (process-name process))
                     (process-live-p process)
                     (not (assq process gptel--request-alist))))
              (process-list)))

(defmacro niwa-retry-stall-test--around-config (function &rest body)
  "Run BODY with FUNCTION around `gptel-curl--get-config', innermost.
The production capture advice still runs first, as it does in use."
  (declare (indent 1) (debug t))
  `(let ((advice ,function))
     (advice-add 'gptel-curl--get-config :around advice '((depth . 100)))
     (unwind-protect (progn ,@body)
       (advice-remove 'gptel-curl--get-config advice))))

(ert-deftest niwa-gptel-retry-stall/interrupted-dispatch-retires-curl ()
  "A throw after curl starts but before gptel registers it leaves no orphan.
`condition-case' cannot see a throw; the sample is retried, not stranded."
  (niwa-retry-test--with-http (list niwa-retry-test--success)
    (let ((calls 0))
      (niwa-retry-test--with-real-clock 0.4
        (niwa-retry-stall-test--around-config
            (lambda (get-config &rest args)
              (when (= 1 (cl-incf calls)) (throw 'niwa-test-unwind nil))
              (apply get-config args))
          (catch 'niwa-test-unwind (gptel--fsm-transition fsm))
          (should (= 1 calls))
          (should-not (niwa-retry-stall-test--orphans))
          (niwa-retry-test--drain-native))))
    (should (= 1 (length requests)))
    (should (eq 'DONE (gptel-fsm-state fsm)))
    (should (equal '("request_not_sent")
                   (mapcar (lambda (error) (plist-get error :code))
                           (plist-get (gptel-fsm-info fsm) :niwa-retry-errors))))
    (should-not (niwa-retry-stall-test--orphans))
    (should-not (niwa-retry-test--stall-timers))))

(ert-deftest niwa-gptel-retry-stall/suspended-dispatch-stalls ()
  "A request write suspended past the timeout is a stall without a transport.
The half-fed curl is deleted and the sample retried while the write waits."
  (niwa-retry-test--with-http (list niwa-retry-test--success)
    (let ((calls 0)
          (orphans-after-stall 'unset))
      (niwa-retry-test--with-real-clock 0.4
        (niwa-retry-stall-test--around-config
            (lambda (get-config &rest args)
              (when (= 1 (cl-incf calls))
                ;; A nested wait, as under a filter's own event loop.
                (niwa-retry-stall-test--wait
                 (lambda () (eq 'MRETRY (gptel-fsm-state fsm))) 5)
                (setq orphans-after-stall (niwa-retry-stall-test--orphans)))
              (apply get-config args))
          ;; The resumed write finds its curl deleted.
          (should (condition-case nil (progn (gptel--fsm-transition fsm) nil) (error t)))
          (niwa-retry-test--drain-native)))
      (should-not orphans-after-stall))
    (should (= 1 (length requests)))
    (should (eq 'DONE (gptel-fsm-state fsm)))
    (should (equal '("stream_stalled")
                   (mapcar (lambda (error) (plist-get error :code))
                           (plist-get (gptel-fsm-info fsm) :niwa-retry-errors))))
    (should-not (niwa-retry-stall-test--orphans))))

(ert-deftest niwa-gptel-retry-stall/stall-spares-another-unregistered-request ()
  "A stall deletes only its own half-fed curl, not another request's.
Request B starts and is still being written inside A's suspended write when
A stalls; B then completes once."
  (niwa-retry-test--with-http (list niwa-retry-test--success niwa-retry-test--success)
    (let* ((calls 0) (b-done 0) (b-process nil) (b-alive-after-stall 'unset)
           (b-buf (generate-new-buffer " *retry-b*"))
           (b-fsm (gptel-make-fsm
                   :state 'WAIT
                   :handlers `((DONE ,(lambda (_machine) (cl-incf b-done))))
                   :info (list :buffer b-buf :backend backend :model 'test-model :stream t
                               :position (with-current-buffer b-buf (point-max-marker))
                               :data '(:model "test-model" :input [] :stream t)
                               :callback #'gptel-curl--stream-insert-response))))
      (unwind-protect
          (niwa-retry-test--with-real-clock 0.4
            (niwa-retry-stall-test--around-config
                (lambda (get-config &rest args)
                  (pcase (cl-incf calls)
                    (1 (run-at-time 0.05 nil #'gptel-curl-get-response b-fsm)
                       (niwa-retry-stall-test--wait (lambda () (= 1 b-done)) 5))
                    (2 (setq b-process (get-buffer-process (current-buffer)))
                       (niwa-retry-stall-test--wait
                        (lambda () (eq 'MRETRY (gptel-fsm-state fsm))) 5)
                       (setq b-alive-after-stall (and (process-live-p b-process) t))))
                  (apply get-config args))
              (should (condition-case nil (progn (gptel--fsm-transition fsm) nil) (error t)))
              (niwa-retry-test--drain-native)))
        (kill-buffer b-buf))
      (should (eq t b-alive-after-stall))
      (should (= 1 b-done))
      (should (eq 'DONE (gptel-fsm-state b-fsm)))
      (should (= 2 (length requests)))
      (should (eq 'DONE (gptel-fsm-state fsm)))
      (should-not (niwa-retry-stall-test--orphans)))))

(ert-deftest niwa-gptel-retry-stall/interrupt-during-write-retires-curl ()
  "Interrupting a request still being written deletes its curl at once.
The write resumes against a dead process, and nothing is retried."
  (niwa-retry-test--with-http (list niwa-retry-test--success)
    (let ((calls 0) (process nil) (alive-before-resume 'unset))
      (niwa-retry-test--with-real-clock 5
        (niwa-retry-stall-test--around-config
            (lambda (get-config &rest args)
              (when (= 1 (cl-incf calls))
                (setq process (get-buffer-process (current-buffer)))
                (run-at-time 0.05 nil #'niwa-request-cancel request)
                (niwa-retry-stall-test--wait (lambda () (eq 'ABRT (gptel-fsm-state fsm))) 5)
                (setq alive-before-resume (process-live-p process)))
              (apply get-config args))
          (should (condition-case nil (progn (gptel--fsm-transition fsm) nil) (error t)))
          (should-not (niwa-retry-stall-test--wait (lambda () requests) 1.5))))
      (should (processp process))
      (should-not alive-before-resume)
      (should (= 1 calls))
      (should (eq 'ABRT (gptel-fsm-state fsm)))
      (should-not (niwa-retry-stall-test--orphans))
      (should-not niwa-gptel-retry--states))))

(ert-deftest niwa-gptel-retry-stall/superseded-check-keeps-successor ()
  "Arm A, arm B, deliver A: B stays STATE's cancellable timer.
A cancelled timer held by TRAMP still fires after its replacement exists."
  (niwa-retry-test--with-http nil
    (let ((niwa-provider-stall-timeout 300)
          (state (list :fsm fsm))
          (info (list :stream t)))
      (niwa-gptel-retry--watch state info)
      (let ((a (pop scheduled)))
        (niwa-gptel-retry--watch state info)
        (let ((b (plist-get state :stall-timer)))
          (should (memq b timer-list))
          (apply (nth 1 a) (nth 2 a))
          (should (eq b (plist-get state :stall-timer)))
          (niwa-gptel-retry--unwatch state)
          (should-not (memq b timer-list)))))))

(provide 'test-niwa-gptel-retry-stall)
;;; test-niwa-gptel-retry-stall.el ends here
