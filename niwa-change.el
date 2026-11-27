;;; niwa-change.el --- Checked file-change transactions -*- lexical-binding: t; -*-

;;; Commentary:

;; The one owner of a checked file change, shared by ApplyPatch, note
;; changes, memory decisions and Rewind.  Snapshots contain :path, :exists,
;; literal :bytes, and :mode, read through a pin that refuses any symlink
;; component.
;;
;; `niwa-change-prepare' merges a recorded before/after change, or its
;; reverse, into current bytes without writing: later edits outside the
;; changed line runs survive; overlapping edits produce per-hunk conflicts.
;;
;; `niwa-change-commit' writes final replacements as one transaction: it
;; rechecks each file just before its pinned write or delete, synchronizes
;; visiting buffers inside change groups, and on failure rolls files and
;; buffers back.  It returns a receipt; `niwa-change-rollback' undoes a
;; committed receipt with the same checks, so a later failure in the caller
;; (Rewind's publication) restores exactly what the transaction wrote.
;; Rollback restores only the complete recorded after-state and preserves
;; any intervening edit, reporting the recovery as incomplete instead.
;; Plans, prompts and the modified-buffer policy stay with the callers.

;;; Code:

(require 'cl-lib)
(require 'niwa-pin)

;; `niwa-instruction-edit'
(autoload 'niwa-instruction-replace-buffer-contents "niwa-instruction-edit")

;; `niwa-instruction-registry'
(declare-function niwa--instruction-alist-value
                  "niwa-instruction-registry" ())
(declare-function niwa--set-instruction-alist-value
                  "niwa-instruction-registry" (value))

;; `niwa-note-source'
(declare-function niwa--mark-buffer-source-missing
                  "niwa-note-source" (buffer))

(define-error 'niwa-change-partial-rollback
  "File change rollback incomplete")

(defun niwa-change-snapshot (path)
  "Return PATH's pinned snapshot: :path, :exists, literal :bytes and :mode."
  (if (not (file-exists-p path))
      (list :path path :exists nil :bytes nil :mode nil)
    (niwa-pin-with-file path
      (let ((pinned (niwa-pin-open-path path)))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally pinned)
          (list :path path :exists t :bytes (buffer-string) :mode (file-modes pinned)))))))

(defun niwa-change-text (snapshot)
  "Decode SNAPSHOT's bytes as reading its path would; return (TEXT . CODING).
Coding cookies and name-based coding rules apply; the file is not reread."
  (with-temp-buffer
    (insert (or (plist-get snapshot :bytes) ""))
    (let ((last-coding-system-used nil))
      (decode-coding-inserted-region (point-min) (point-max) (plist-get snapshot :path))
      (cons (buffer-string) last-coding-system-used))))

(defun niwa-change-same-p (left right)
  "Return non-nil when LEFT and RIGHT have identical path, bytes and mode."
  (and left right
       (cl-every (lambda (key) (equal (plist-get left key) (plist-get right key)))
                 '(:path :exists :bytes :mode))))

(defun niwa-change--runs (before after)
  "Return zero-based line runs changing literal bytes BEFORE to AFTER.
Each run is (OLD-START OLD-END NEW-START NEW-END), with exclusive ends."
  (let ((old (make-temp-file "niwa-change-old-"))
        (new (make-temp-file "niwa-change-new-"))
        (output (generate-new-buffer " *niwa change diff*"))
        (default-directory temporary-file-directory)
        (coding-system-for-read 'no-conversion)
        (coding-system-for-write 'no-conversion)
        runs)
    (unwind-protect
        (progn
          (write-region before nil old nil 'silent)
          (write-region after nil new nil 'silent)
          ;; Interactive diff helpers run user major-mode hooks and shell
          ;; commands.  This is an exact byte comparison, not a diff view.
          (let ((status (call-process "diff" nil output nil "-a" "-U0" "--" old new)))
            (unless (memq status '(0 1))
              (error "Diff failed: %s" status)))
          (with-current-buffer output
            (goto-char (point-min))
            (while (re-search-forward
                    "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? +\\+\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? @@"
                    nil t)
              (let* ((old-count (if (match-string 2)
                                    (string-to-number (match-string 2)) 1))
                     (new-count (if (match-string 4)
                                    (string-to-number (match-string 4)) 1))
                     (old-start (- (string-to-number (match-string 1))
                                   (if (zerop old-count) 0 1)))
                     (new-start (- (string-to-number (match-string 3))
                                   (if (zerop new-count) 0 1))))
                (push (list old-start (+ old-start old-count)
                            new-start (+ new-start new-count)) runs))))
          (unless (or runs (equal before after))
            (error "Diff produced no changed line runs"))
          (nreverse runs))
      (delete-file old)
      (delete-file new)
      (kill-buffer output))))

(defun niwa-change-overlap-p (start end other-start other-end)
  "Return non-nil when START..END overlaps OTHER-START..OTHER-END.
Empty runs touch both adjacent lines: an insertion at the same gap, or a
replacement reaching a deleted run's gap, must not be guessed through."
  (if (or (= start end) (= other-start other-end))
      (and (<= start other-end) (<= other-start end))
    (and (< start other-end) (< other-start end))))

(defun niwa-change-map-extents (from to extents)
  "Map zero-based line EXTENTS of literal bytes FROM into TO with one diff.
Each extent is (START . END), END exclusive.  Return one plist per extent:
:start and :end shifted by the runs changed before them, and :touched, the
\(START . END) runs of TO replacing FROM lines that overlap the extent.  An
empty run touches both adjacent lines."
  (let ((runs (unless (equal from to) (niwa-change--runs from to))))
    (mapcar
     (lambda (extent)
       (let ((start (car extent)) (end (cdr extent))
             (start-offset 0) (end-offset 0) touched)
         (pcase-dolist (`(,a ,b ,c ,d) runs)
           (when (<= b start) (cl-incf start-offset (- (- d c) (- b a))))
           (when (<= b end) (cl-incf end-offset (- (- d c) (- b a))))
           (when (niwa-change-overlap-p start end a b) (push (cons c d) touched)))
         (list :start (+ start start-offset) :end (+ end end-offset)
               :touched (nreverse touched))))
     extents)))

(defun niwa-change--line-offsets (bytes)
  "Return a vector of zero-based line starts and the final end in BYTES."
  (let ((offsets (list 0)) (position 0))
    (while (string-match "\n" bytes position)
      (setq position (match-end 0))
      (push position offsets))
    (unless (= position (length bytes)) (push (length bytes) offsets))
    (vconcat (nreverse offsets))))

(defun niwa-change--merge (base proposed current path)
  "Prepare BASE to PROPOSED over CURRENT literal bytes at PATH.
Return :bytes or :conflicts, plus zero-based current-coordinate :ranges."
  (let ((changes (niwa-change--runs base proposed))
        (intervening (niwa-change--runs base current))
        (source-offsets (niwa-change--line-offsets proposed))
        (current-offsets (niwa-change--line-offsets current))
        edits conflicts ranges)
    (dolist (change changes)
      (pcase-let ((`(,start ,end ,new-start ,new-end) change)
                  (offset 0)
                  (end-offset 0)
                  (touched nil))
        (dolist (other intervening)
          (pcase-let ((`(,a ,b ,c ,d) other))
            (when (niwa-change-overlap-p start end a b)
              (push other touched))
            (when (<= b start)
              (setq offset (+ offset (- (- d c) (- b a)))))
            (when (<= b end)
              (setq end-offset (+ end-offset (- (- d c) (- b a)))))))
        (cond
         ((and (= (length touched) 1)
               (= start (nth 0 (car touched)))
               (= end (nth 1 (car touched)))
               (equal (substring proposed (aref source-offsets new-start)
                                 (aref source-offsets new-end))
                      (substring current (aref current-offsets (nth 2 (car touched)))
                                 (aref current-offsets (nth 3 (car touched))))))
          (push (list (nth 2 (car touched)) (nth 3 (car touched))) ranges))
         (touched
            (let ((first (1+ (apply #'min (mapcar (lambda (run) (nth 2 run)) touched))))
                  (last (apply #'max (mapcar (lambda (run) (nth 3 run)) touched))))
              (push (list (min (+ start offset) (1- first))
                          (max (+ end end-offset) last)) ranges)
              (push (list :path path :start-line first :end-line (max first last)
                          :base-start-line (1+ start) :base-end-line (max (1+ start) end))
                    conflicts)))
         (t
          (push (list (+ start offset) (+ end offset)) ranges)
          (push (list (+ start offset) (+ end offset) new-start new-end) edits)))))
    (if conflicts
        (list :conflicts (nreverse conflicts) :ranges (nreverse ranges))
      (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert current)
          ;; Descending coordinates preserve the positions of earlier runs.
          (dolist (edit edits)
            (pcase-let ((`(,start ,end ,new-start ,new-end) edit))
              (delete-region (1+ (aref current-offsets start))
                             (1+ (aref current-offsets end)))
              (goto-char (1+ (aref current-offsets start)))
              (insert (substring proposed (aref source-offsets new-start)
                                 (aref source-offsets new-end)))))
          (list :bytes (buffer-string) :ranges (nreverse ranges))))))

(defun niwa-change-prepare (before after current &optional reverse strict)
  "Prepare a BEFORE/AFTER recorded change against CURRENT, without writing.
REVERSE prepares its revert.  STRICT requires the entire base snapshot to
match, as transaction rollback does.  Otherwise only overlapping line edits
conflict; unrelated current edits and mode changes survive.  File creation,
deletion and binary changes require the whole base to match.

Return (:before CURRENT :after SNAPSHOT), or (:conflicts PLISTS).  Conflicts
contain :path, :start-line and :end-line (one-based current coordinates).
Text merges also return :ranges, zero-based current line runs for all hunks.
The caller must collect conflicts before writing and recheck each prepared
:before snapshot at mutation time.  No partial merged text is returned."
  (let* ((base (if reverse after before))
         (target (if reverse before after))
         (path (plist-get base :path))
         (conflict (list :conflicts
                         (list (list :path path :start-line 1 :end-line 1)))))
    (unless (and (equal path (plist-get target :path))
                 (equal path (plist-get current :path)))
      (error "Change snapshots name different files"))
    (cond
     ((niwa-change-same-p base current)
      (list :before current :after target))
     ((or strict
          (not (cl-every (lambda (snapshot) (plist-get snapshot :exists))
                         (list base target current)))
          (cl-some (lambda (snapshot)
                     (string-search (unibyte-string 0) (plist-get snapshot :bytes)))
                   (list base target current)))
      conflict)
     ((and (not (equal (plist-get base :mode) (plist-get target :mode)))
           (not (equal (plist-get base :mode) (plist-get current :mode))))
      conflict)
     (t
      (let ((merged (niwa-change--merge
                     (plist-get base :bytes) (plist-get target :bytes)
                     (plist-get current :bytes) path)))
        (if (plist-get merged :conflicts) merged
          (list :before current
                :ranges (plist-get merged :ranges)
                :after (list :path path :exists t :bytes (plist-get merged :bytes)
                             :mode (if (equal (plist-get base :mode) (plist-get target :mode))
                                       (plist-get current :mode)
                                     (plist-get target :mode))))))))))

(defun niwa-change--visiting-buffer (path)
  "Return PATH's visiting buffer normalized to its base buffer.
Preserve `find-buffer-visiting' file identity and alias matching."
  (when-let* ((buffer (find-buffer-visiting path)))
    (or (buffer-base-buffer buffer) buffer)))

(defun niwa-change-assert-saved (changes)
  "Signal when a visited file in CHANGES has unsaved edits."
  (dolist (change changes)
    (when-let* ((buffer (niwa-change--visiting-buffer (plist-get change :path)))
                ((buffer-modified-p buffer)))
      (error "File has unsaved buffer changes: %s" (plist-get change :path)))))

(defun niwa-change-missing-parent-directories (path)
  "Return missing parent directories for PATH, outermost first."
  (let ((directory (file-name-directory path)) missing)
    (while (and directory (not (file-exists-p directory)))
      (push directory missing)
      (setq directory
            (file-name-directory (directory-file-name directory))))
    missing))

(defun niwa-change--assert-current (currentp)
  "Signal unless CURRENTP, when non-nil, still confirms the caller's authority."
  (when (and currentp (not (funcall currentp)))
    (error "Patch ownership is unavailable")))

(defun niwa-change--buffer-snapshot (buffer)
  "Return BUFFER's restorable state: text, point, overlays and notes."
  (with-current-buffer buffer
    (let ((minimum (point-min))
          (maximum (point-max))
          (position (point)))
      (save-restriction
        (widen)
        (let* ((overlays (append (car (overlay-lists))
                                 (cdr (overlay-lists))))
               (entry (assq buffer (niwa--instruction-alist-value))))
          (list :buffer buffer
                :group (prepare-change-group)
                :minimum minimum
                :maximum maximum
                :point position
                :modified (buffer-modified-p)
                :modtime (visited-file-modtime)
                :file-current (verify-visited-file-modtime buffer)
                :content (buffer-substring (point-min) (point-max))
                :instruction-present (and entry t)
                :instructions (and entry (copy-sequence (cdr entry)))
                :overlays
                (mapcar
                 (lambda (overlay)
                   (list overlay (overlay-start overlay)
                         (overlay-end overlay)
                         (copy-sequence (overlay-properties overlay))))
                 overlays)))))))

(defun niwa-change--restore-buffer (snapshot)
  "Restore a live buffer from SNAPSHOT of `niwa-change--buffer-snapshot'."
  (when-let* ((buffer (plist-get snapshot :buffer))
              ((buffer-live-p buffer)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (let ((inhibit-modification-hooks t)
              (inhibit-read-only t))
          (unless (equal (buffer-string) (plist-get snapshot :content))
            (erase-buffer)
            (insert (plist-get snapshot :content))))
        (let* ((saved-overlays (plist-get snapshot :overlays))
               (originals (mapcar #'car saved-overlays))
               (current (append (car (overlay-lists))
                                (cdr (overlay-lists)))))
          (dolist (overlay current)
            (unless (memq overlay originals)
              (delete-overlay overlay)))
          (dolist (saved saved-overlays)
            (let ((overlay (car saved))
                  (properties (nth 3 saved)))
              (let ((current (overlay-properties overlay)))
                (while current
                  (overlay-put overlay (pop current) nil)
                  (pop current)))
              (move-overlay overlay (nth 1 saved) (nth 2 saved) buffer)
              (while properties
                (overlay-put overlay (pop properties) (pop properties)))))
          (let ((alist (niwa--instruction-alist-value)))
            (if (plist-get snapshot :instruction-present)
                (setf (alist-get buffer alist)
                      (plist-get snapshot :instructions))
              (setq alist (assq-delete-all buffer alist)))
            (niwa--set-instruction-alist-value alist))
          (narrow-to-region (plist-get snapshot :minimum)
                            (plist-get snapshot :maximum))
          (goto-char (plist-get snapshot :point))
          (set-visited-file-modtime (plist-get snapshot :modtime))
          (set-buffer-modified-p (plist-get snapshot :modified)))))))

(defun niwa-change-rollback (receipt &optional cause)
  "Roll back RECEIPT's committed files and visiting buffers.
A file is restored only while it still holds the after-state the
transaction wrote, and a buffer only while it holds the text the
transaction gave it; an intervening edit is preserved.  A deleted file
returns with its recorded bytes and mode.  A directory the transaction
created is removed under its pinned parent only while empty and still the
same directory; one whose identity cannot be established is left and
reported.  Return nil after complete recovery;
otherwise signal `niwa-change-partial-rollback' with CAUSE and the paths
left unrestored, including buffers that could not be restored, after
attempting every other path."
  (let* ((inhibit-quit t)
         (currentp (plist-get receipt :currentp))
         (mutate (plist-get receipt :mutate))
         (expected (plist-get receipt :expected))
         (buffers (plist-get receipt :buffers))
         failures)
    ;; A failed pinned write names the parent directories it could not remove.
    (when (eq (car-safe cause) 'niwa-pin-refused)
      (dolist (directory (nth 2 cause))
        (push (cons directory cause) failures)))
    (dolist (snapshot (plist-get receipt :snapshots))
      (let* ((path (plist-get snapshot :path))
             (after (cl-find path expected :test #'equal
                             :key (lambda (item) (plist-get item :path)))))
        (when after
          (condition-case err
              (let ((current (niwa-change-snapshot path)))
                (unless (niwa-change-same-p snapshot current)
                  (niwa-change--assert-current currentp)
                  (unless (niwa-change-same-p after current)
                    (error "File changed after the transaction write: %s" path))
                  (cond (mutate (funcall mutate after snapshot))
                        ((plist-get snapshot :exists)
                         (niwa-pin-write-file-atomically
                          path (plist-get snapshot :bytes) 'no-conversion
                          (plist-get snapshot :mode) (plist-get receipt :keep-links)))
                        (t (niwa-pin-delete-file path (plist-get receipt :keep-links))))))
            (error (push (cons path err) failures))))))
    (dolist (snapshot buffers)
      (when-let* ((buffer (plist-get snapshot :buffer)) ((buffer-live-p buffer)))
        (let ((path (plist-get snapshot :path))
              (text (with-current-buffer buffer
                      (save-restriction
                        (widen)
                        (buffer-substring-no-properties (point-min) (point-max))))))
          (when (or (assoc path failures)
                    (not (or (equal text (plist-get snapshot :content))
                             (equal text (plist-get snapshot :expected-content)))))
            (plist-put snapshot :preserve t)
            (unless (assoc path failures)
              (push (cons path '(error "Buffer changed during the transaction")) failures))))))
    ;; A committed transaction has accepted its groups; only a failing commit
    ;; still holds them.
    (dolist (snapshot (reverse buffers))
      (when-let* ((group (plist-get snapshot :group)))
        (condition-case err
            (with-current-buffer (plist-get snapshot :buffer)
              (let ((inhibit-modification-hooks t)
                    (inhibit-read-only t))
                (if (plist-get snapshot :preserve)
                    (progn (accept-change-group group) (set-buffer-modified-p t))
                  (cancel-change-group group))))
          (error (plist-put snapshot :preserve t)
                 (push (cons (plist-get snapshot :path) err) failures)))))
    (dolist (snapshot buffers)
      (unless (plist-get snapshot :preserve)
        (condition-case err (niwa-change--restore-buffer snapshot)
          (error (push (cons (plist-get snapshot :path) err) failures)))))
    (pcase-dolist (`(,directory ,device ,inode) (reverse (plist-get receipt :directories)))
      (condition-case failure
          (progn
            (niwa-change--assert-current currentp)
            (niwa-pin-remove-directory directory device inode))
        (error (push (cons directory failure) failures))))
    (when failures
      (signal 'niwa-change-partial-rollback
              (list cause (delete-dups (mapcar #'car (nreverse failures))))))))

(cl-defun niwa-change-commit (changes &key currentp mutate batch keep-links)
  "Commit CHANGES as one filesystem and visited-buffer transaction.
Each change has :path and :action `write' or `delete'.  A write supplies
literal :bytes or :content text, and optionally :mode; otherwise it keeps
the current mode, and a new file gets the default modes.  A change may
include :before, a snapshot whose bytes/absence/mode must still match
before any write.  Each file is rechecked just before its own write.
CURRENTP, when supplied, must authorize writes, synchronization, and
rollback mutations.  MUTATE, when supplied, replaces the default pinned
filesystem mutation.  It receives before/after snapshots and must check
freshness and fence the replacement on the target; the same callback
handles rollback.  BATCH, when non-nil, checks and applies the ordered
lists of before/after snapshots in one target program instead of
individual forward mutations.  Buffer synchronization stays here.
KEEP-LINKS makes the default pinned writer refuse to replace or remove a
symlink at a written or deleted path, including during rollback.

Return a receipt for `niwa-change-rollback'.  On failure, roll back as it
does and re-signal; an incomplete rollback signals
`niwa-change-partial-rollback' instead."
  (require 'niwa-overlays)
  (let ((owner-current-p currentp)
        (checks (delete-dups (delq nil (mapcar (lambda (change) (plist-get change :resource-check))
                                               changes)))))
    (when checks
      (setq currentp (lambda ()
                       (and (or (null owner-current-p) (funcall owner-current-p))
                            (cl-every #'funcall checks))))))
  (dolist (change changes)
    (when-let* (((eq (plist-get change :action) 'write))
                (buffer (niwa-change--visiting-buffer (plist-get change :path)))
                ((buffer-local-value 'buffer-read-only buffer)))
      (error "Buffer is read-only: %s" (buffer-name buffer))))
  (niwa-change--assert-current currentp)
  (let* ((snapshots (mapcar (lambda (change) (niwa-change-snapshot (plist-get change :path)))
                            changes))
         expected batch-expected created-directories
         (buffer-snapshots
          (delq nil
                (mapcar
                 (lambda (change)
                   (when-let* ((path (plist-get change :path))
                               (buffer (niwa-change--visiting-buffer path)))
                     (append (niwa-change--buffer-snapshot buffer)
                             (list :path path
                                   :expected-content
                                   (when (eq (plist-get change :action) 'write)
                                     (or (plist-get change :content)
                                         (car (niwa-change-text
                                               (list :path path :bytes (plist-get change :bytes))))))))))
                 changes)))
         (receipt (lambda ()
                    (list :snapshots snapshots :expected expected
                          :buffers buffer-snapshots :directories created-directories
                          :currentp currentp :mutate mutate :keep-links keep-links))))
    (cl-loop for change in changes for snapshot in snapshots do
             (when (and (plist-member change :before)
                        (not (niwa-change-same-p (plist-get change :before) snapshot)))
               (error "File changed before the transaction: %s" (plist-get change :path))))
    (dolist (snapshot buffer-snapshots)
      (when-let* ((group (plist-get snapshot :group)))
        (with-current-buffer (plist-get snapshot :buffer)
          (activate-change-group group))))
    (condition-case err
        (progn
          (cl-loop
           for change in changes for snapshot in snapshots do
           (niwa-change--assert-current currentp)
           (unless (niwa-change-same-p snapshot (niwa-change-snapshot (plist-get change :path)))
             (error "File changed before its transaction write: %s" (plist-get change :path)))
           (let* ((path (plist-get change :path))
                  (write (eq (plist-get change :action) 'write))
                  (after (list :path path :exists write
                               :bytes (when write
                                        (if (plist-member change :bytes) (plist-get change :bytes)
                                          (encode-coding-string (plist-get change :content) 'utf-8-unix)))
                               :mode (when write
                                       (or (plist-get change :mode) (plist-get snapshot :mode)
                                           (default-file-modes))))))
             (if batch (push after batch-expected)
               (push after expected)
               (cond (mutate (funcall mutate snapshot after))
                     (write (setq created-directories
                                  (append created-directories
                                          (niwa-pin-write-file-atomically
                                           path (plist-get after :bytes) 'no-conversion
                                           (plist-get after :mode) keep-links))))
                     (t (niwa-pin-delete-file path keep-links))))))
          (when batch
            (niwa-change--assert-current currentp)
            (setq expected (nreverse batch-expected))
            (funcall batch snapshots expected))
          (dolist (snapshot buffer-snapshots)
            (niwa-change--assert-current currentp)
            (with-current-buffer (plist-get snapshot :buffer)
              (if-let* ((text (plist-get snapshot :expected-content)))
                  (save-restriction
                    (widen)
                    (let ((source (generate-new-buffer " *niwa change sync*")))
                      (unwind-protect
                          (progn
                            (with-current-buffer source (insert text))
                            (set-visited-file-modtime)
                            (niwa-instruction-replace-buffer-contents source)
                            (unless (equal (buffer-substring-no-properties (point-min) (point-max)) text)
                              (error "Buffer changed during the transaction: %s"
                                     (plist-get snapshot :path)))
                            (set-visited-file-modtime)
                            (set-buffer-modified-p nil))
                        (kill-buffer source))))
                (niwa--mark-buffer-source-missing (current-buffer))
                (set-visited-file-modtime 0))))
          (niwa-change--assert-current currentp)
          (dolist (after expected)
            (unless (niwa-change-same-p after (niwa-change-snapshot (plist-get after :path)))
              (error "File changed during the transaction: %s" (plist-get after :path))))
          (dolist (snapshot buffer-snapshots)
            (when-let* ((group (plist-get snapshot :group)))
              (with-current-buffer (plist-get snapshot :buffer)
                (accept-change-group group))
              (plist-put snapshot :group nil)
              ;; Unwind a failed commit exactly; a later receipt rollback
              ;; should keep a formerly current buffer current on disk.
              (when (plist-get snapshot :file-current)
                (plist-put snapshot :modtime nil))))
          (funcall receipt))
      ((error quit)
       (niwa-change-rollback (funcall receipt) err)
       (signal (car err) (cdr err))))))

(provide 'niwa-change)
;;; niwa-change.el ends here
