;;; test-niwa-permission-queue-entry.el --- Permission entry normalization table -*- lexical-binding: t -*-

;;; Commentary:

;; Characterization table for permission entries as the queue admits them.
;; Each row submits an entry through a real producer route (direct queue
;; admission, the execution adapter, the tool request boundary, or both
;; chained) and reads every consumer of the entry's derived facts: the policy
;; re-evaluation, the reviewer's tool name and Eval mode, the card's live-Eval
;; warning, the diagnostic Eval mode and the one-shot restrictions.  It must
;; pass unchanged when entry construction changes shape.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-permission-queue-test-support
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "niwa-permission-queue-test-support"))
(require 'niwa-permission-review)
(require 'niwa-tool-exec-permission)
(require 'niwa-tool-permission)
(require 'niwa-hooks)

(defconst pqe--rows
  ;; (LABEL ROUTE ONE-SHOT-P ARGS ENTRY EXPECTED)
  ;; EXPECTED: (TOOL DECISION REVIEW-MODE LIVE-WARNING LOG-MODE ONCE-ONLY)
  '(("exec live Eval" exec nil (:expression "(+ 1 2)")
     (:kind eval :mode "live" :eval-mode live :expression "(+ 1 2)")
     ("Eval" ask live t "live" nil))
    ("exec fresh Eval" exec nil (:mode "fresh" :expression "(+ 1 2)")
     (:kind eval :mode "fresh" :eval-mode fresh :expression "(+ 1 2)")
     ("Eval" ask fresh nil "fresh" nil))
    ("exec sandbox Eval" exec nil (:mode "sandbox" :expression "(+ 1 2)")
     (:kind eval :mode "sandbox" :eval-mode sandbox :expression "(+ 1 2)")
     ("Eval" allow sandbox nil "sandbox" nil))
    ("exec escalated fresh Eval" exec nil
     (:mode "fresh" :expression "t" :sandbox_permissions "require_escalated")
     (:kind sandbox :tool-name "Eval" :detail "t" :sandbox-permissions require-escalated)
     ("Eval" ask fresh nil "fresh" nil))
    ("exec Bash" exec nil (:command "ls")
     (:kind bash :command "ls")
     ("Bash" allow nil nil nil nil))
    ("exec one-shot Bash" exec t (:command "touch x")
     (:kind bash :command "touch x" :include-always t)
     ("Bash" ask nil nil nil t))
    ("exec through tool boundary, one-shot live Eval" exec-tool t (:expression "t")
     (:kind eval :mode "live" :eval-mode live :expression "t" :include-always t)
     ("Eval" ask live t "live" t))
    ("queue hook card for sandbox Eval" queue nil nil
     (:kind generic :tool-name "Eval" :args (:mode "sandbox" :expression "t"))
     ("Eval" ask sandbox nil "sandbox" nil))
    ("queue hook card for default Eval" queue nil nil
     (:kind generic :tool-name "Eval" :args (:expression "t"))
     ("Eval" ask live t "live" nil))
    ("queue Eval without tool name" queue nil nil
     (:kind eval :mode "live" :expression "t")
     ("Eval" ask live t "live" nil))
    ("queue Bash without tool name" queue nil nil
     (:kind bash :command "touch x")
     ("Bash" ask nil nil nil nil))
    ("tool boundary one-shot Read" tool t nil
     (:kind generic :tool-name "Read" :specifier-key :path :specifier-value "/tmp/x")
     ("Read" ask nil nil nil t))))

(defun pqe--submit (route one-shot-p args entry session)
  "Submit ENTRY with ARGS by ROUTE for SESSION, ONE-SHOT-P as the request says."
  (let* ((entry (append (list :origin "/root" :callback #'ignore) (copy-tree entry)))
         (tool (or (plist-get entry :tool-name)
                   (pcase (plist-get entry :kind) ('bash "Bash") (_ "Eval"))))
         (context (list :session session :buffer (current-buffer) :args args
                        :one-shot-mutations-p one-shot-p))
         (permission-context
          (niwa-permission--invocation-context
           :tool-name tool :args args :session session :buffer (current-buffer)
           :one-shot-mutations-p one-shot-p
           :permission-request
           (and (eq route 'exec-tool)
                (lambda (entry queue-session settle)
                  (niwa-tool-permission--request
                   context entry queue-session
                   (lambda (_updated outcome) (funcall settle outcome))))))))
    (pcase route
      ('queue (niwa-permission-request entry session))
      ('tool (niwa-tool-permission--request context entry session #'ignore))
      (_ (niwa-tool-exec-permission--request-permission
          entry permission-context session)))))

(niwa-deftest niwa-permission-queue-entry ()
  ,test
  (test)
  :doc "every producer route yields the same consumer facts"
  (dolist (row pqe--rows)
    (pcase-let ((`(,label ,route ,one-shot-p ,args ,entry ,expected) row))
      (ert-info (label)
        (niwa-pq-test--with-queue (session _data _view)
          (let ((niwa-permission-reviewer 'user)
                (niwa-permission-rules nil))
            (cl-letf (((symbol-function 'niwa-hooks-run-event)
                       (lambda (_event _payload callback &rest _)
                         (funcall callback nil))))
              (pqe--submit route one-shot-p args entry session))
            (let* ((queued (car (niwa-permission-queue--get session)))
                   (evidence (niwa-permission-review--evidence queued)))
              (should queued)
              (should
               (equal expected
                      (list
                       (plist-get (niwa-permission-review--context queued) :tool-name)
                       (niwa-permission-queue--reevaluate queued)
                       (plist-get evidence :eval-mode)
                       (and (string-match-p
                             "Live Eval"
                             (niwa-permission--execution-warnings queued))
                            t)
                       (plist-get (niwa-permission-queue--log-props queued)
                                  :eval-mode)
                       (plist-get queued :once-only))))
              (when one-shot-p
                (should-not (plist-get queued :include-always))
                (should-not (plist-get queued :reusable-operation-p))))
            (niwa-permission-queue-abort-all session))))))
  :doc "one-shot entries capture the side buffer and collapse reusable outcomes"
  (let ((side-buffer (generate-new-buffer " *niwa-side-permission*"))
        delivered)
    (unwind-protect
        (let ((entry (niwa-permission-queue-entry
                      (list :kind 'permission :mutation-p t
                            :callback (lambda (outcome) (push outcome delivered)))
                      t side-buffer)))
          (should (eq side-buffer (plist-get entry :data-buffer)))
          (should (plist-get entry :once-only))
          (should-not (plist-member entry :shared-workspace-active-p))
          (funcall (plist-get entry :callback) 'allow-session)
          (funcall (plist-get entry :callback) 'deny-session)
          (should (equal '(deny-once allow-once) delivered)))
      (kill-buffer side-buffer)))
  :doc "normalizing again changes nothing"
  (let ((entry (niwa-permission-queue-entry '(:kind generic :tool-name "Eval"))))
    (should (equal entry (niwa-permission-queue-entry entry))))
  :doc "invalid Eval arguments go to the user without automatic review"
  (dolist (row '(((:mode "bogus" :expression "t") nil)
                 ((:mode "live" :reset t :expression "t") nil)
                 ((:mode "live" :expression "t") t)))
    (niwa-pq-test--with-queue (session _data view)
      (let ((niwa-permission-reviewer 'auto) reviewed)
        ;; Records eligibility and skips the provider call; review itself is
        ;; covered in test-niwa-permission-review.el.
        (cl-letf (((symbol-function 'niwa-permission-review-start)
                   (lambda (_entry fallback) (setq reviewed t) (funcall fallback))))
          (niwa-pq-test--request
           (list :kind 'generic :tool-name "Eval" :origin "/root"
                 :args (car row) :callback #'ignore)
           session))
        (should (eq (cadr row) reviewed))
        (let ((card (plist-get (niwa-pq-test--card view) :entry)))
          (should (eq (if (cadr row) nil 'invalid-eval)
                      (plist-get card :review-fallback))))
        (niwa-permission-queue-abort-all session)))))

(provide 'test-niwa-permission-queue-entry)
;;; test-niwa-permission-queue-entry.el ends here
