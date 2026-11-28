;;; test-niwa-view-disclosure-live.el --- Disclosure toggles in a live turn -*- lexical-binding: t -*-

;;; Commentary:

;; Toggling a row of a streaming turn keeps the reader where they were.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))
(require 'niwa-view)
(require 'niwa-view-disclosure)
(require 'niwa-view-redraw)
(require 'niwa-view-render)
(require 'niwa-view-stream)

(defun niwa-view-disclosure-live-test--line ()
  "Return the text of the current line."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(niwa-deftest niwa-view-toggle-section/live-group-child ()
  ,test
  (test)
  :doc "expanding a grouped row while a call still runs keeps point on it and window start"
  (niwa-view-test--with-buffers
    (let (data-turn-start)
      (niwa-view-test--insert-data
       data-buf
       (mapconcat (lambda (i) (format "prompt line %d" i)) (number-sequence 1 60) "\n")
       nil)
      (niwa-view-test--insert-data data-buf "\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max))))
      (niwa-view-test--insert-data data-buf "Working.\n" 'response)
      (dotimes (i 4)
        (niwa-view-test--insert-data
         data-buf
         (format "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n" i i)
         `(tool . ,(format "call_%d" i))))
      (niwa-view-test--insert-data data-buf "More.\n" 'response)
      (with-current-buffer view-buf
        (set-window-buffer (selected-window) view-buf)
        (niwa-view--full-rerender)
        (setq niwa-view--data-turn-start data-turn-start)
        (save-excursion
          (goto-char (point-min))
          (search-forward "Assistant")
          (niwa-view-stream-set-in-flight-turn-start (match-beginning 0)))
        (unwind-protect
            (progn
              ;; One call is still running below the settled group.
              (setq niwa-view--pending-tool-calls
                    (list (list (niwa-view--pending-tool-claim-key
                                 '(:name "Bash" :args (:command "sleep 9")))
                                "Bash" "sleep 9")))
              (niwa-view-render-live-update data-buf)
              (goto-char (point-min))
              (search-forward "read 4 files")
              (goto-char (match-beginning 0))
              (niwa-view-toggle-section)
              (goto-char (point-min))
              (search-forward "f1.el")
              (goto-char (match-beginning 0))
              (should (eq 'tool-child (get-text-property (point) 'niwa-view-type)))
              (let ((row (niwa-view-disclosure-live-test--line))
                    (start (save-excursion (forward-line -2) (point))))
                (set-window-start (selected-window) start)
                (set-window-point (selected-window) (point))
                (niwa-view-toggle-section)
                (should (string-search "content 1" (buffer-string)))
                ;; The running call, drawn only by the live render, survives.
                (should (= 1 (niwa-view-test--count-substring
                              "sleep 9" (buffer-substring-no-properties
                                         (point-min) (niwa-view-transcript-tail)))))
                (should (equal row (niwa-view-disclosure-live-test--line)))
                (should (eq 'tool-child (get-text-property (point) 'niwa-view-type)))
                (should (= start (window-start)))))
          (setq niwa-view--pending-tool-calls nil)))))
  :doc "a row in the running call's own group keeps point, and the running call is redrawn"
  (niwa-view-test--with-buffers
    (let (data-turn-start)
      (niwa-view-test--insert-data data-buf "go\n" nil)
      (with-current-buffer data-buf
        (setq data-turn-start (copy-marker (point-max))))
      (niwa-view-test--insert-data data-buf "Working.\n" 'response)
      (dotimes (i 4)
        (niwa-view-test--insert-data
         data-buf
         (format "(:name \"Read\" :args (:file_path \"/tmp/f%d.el\"))\n\ncontent %d\n" i i)
         `(tool . ,(format "call_%d" i))))
      (with-current-buffer view-buf
        (set-window-buffer (selected-window) view-buf)
        (niwa-view--full-rerender)
        (setq niwa-view--data-turn-start data-turn-start)
        (save-excursion
          (goto-char (point-min))
          (search-forward "Assistant")
          (niwa-view-stream-set-in-flight-turn-start (match-beginning 0)))
        (unwind-protect
            (progn
              (setq niwa-view--pending-tool-calls
                    (list (list (niwa-view--pending-tool-claim-key
                                 '(:name "Bash" :args (:command "sleep 9")))
                                "Bash" "sleep 9")))
              (niwa-view-render-live-update data-buf)
              (goto-char (point-min))
              (search-forward "1 running")
              (beginning-of-line)
              (niwa-view-toggle-section)
              (goto-char (point-min))
              (search-forward "f1.el")
              (goto-char (match-beginning 0))
              (should (eq 'tool-child (get-text-property (point) 'niwa-view-type)))
              (let ((row (niwa-view-disclosure-live-test--line))
                    (redraws 0)
                    (redraw (symbol-function 'niwa-view--render-live-region)))
                ;; The queued live redraw still runs, after point is restored.
                (cl-letf (((symbol-function 'niwa-view--render-live-region)
                           (lambda (&rest args)
                             (cl-incf redraws)
                             (apply redraw args))))
                  (niwa-view-toggle-section))
                (should (= 1 redraws))
                (should (string-search "content 1" (buffer-string)))
                (should (= 1 (niwa-view-test--count-substring
                              "sleep 9" (buffer-substring-no-properties
                                         (point-min) (niwa-view-transcript-tail)))))
                (should (equal row (niwa-view-disclosure-live-test--line)))))
          (setq niwa-view--pending-tool-calls nil))))))

(provide 'test-niwa-view-disclosure-live)
;;; test-niwa-view-disclosure-live.el ends here
