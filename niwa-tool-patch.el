;;; niwa-tool-patch.el -- Patch-oriented file mutation -*- lexical-binding: t -*-

;;; Commentary:

;; Implements the ApplyPatch tool.  One Codex-style patch proposal may add,
;; update, delete, or move multiple files.  Parsing and filesystem validation
;; happen before any write; selected changes are then committed as one
;; rollback-backed transaction.
;;
;; A remote HTML artifact's files are patched at shared://ALIAS/files/PATH.
;; They are content at the artifact's origin, not local files: the handler
;; fetches each one with its hash before planning, hunks apply to that
;; content, and the commit sends every result back in one request that the
;; origin carries out only if every hash still matches, so the patch lands
;; whole or not at all.  A patch touches the files of one remote artifact or
;; local files, never both.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'niwa-tool-registry)
  (require 'subr-x))

(require 'niwa-change)
(require 'niwa-edit-diagnostics)
(require 'niwa-execution-target)
(require 'niwa-file-state)
(require 'niwa-pin)
(require 'niwa-resource)
(require 'niwa-theme-faces)
(require 'niwa-utilities)

;; `diff'
(declare-function diff-no-select "diff"
                  (old new &optional switches no-async buf))

;; `niwa-artifact-backend'
(declare-function niwa-artifact-backend-call "niwa-artifact-backend"
                  (workspace args callback &optional authorize commit))
(declare-function niwa-artifact-backend-registered-p "niwa-artifact-backend"
                  (workspace id))
(autoload 'niwa-artifact-backend-call "niwa-artifact-backend")
(autoload 'niwa-artifact-backend-registered-p "niwa-artifact-backend")

;; `niwa-artifact-store'
(declare-function niwa-artifact-store-note-writes
                  "niwa-artifact-store" (session changes &optional request))
(autoload 'niwa-artifact-store-note-writes "niwa-artifact-store")

;; `niwa-note-change'
(autoload 'niwa-note-prepare-change "niwa-note-change")

;; `niwa-patch-review'
(declare-function niwa-patch-review-start
                  "niwa-patch-review" (proposal callback data-buffer))
(autoload 'niwa-patch-review-start "niwa-patch-review")

;; `niwa-permission-mode'
(declare-function niwa-permission-mode-effective
                  "niwa-permission-mode"
                  (&optional session data-buffer surface-buffer))
(autoload 'niwa-permission-mode-effective "niwa-permission-mode")

;; `niwa-pipeline'
(declare-function niwa-pipeline-canonical-path
                  "niwa-pipeline" (path))
(autoload 'niwa-pipeline-canonical-path "niwa-pipeline")
(defvar niwa-pipeline--auto-apply-edit-p)
(defvar niwa-pipeline--handler-active-p)

;; `niwa-session-persistence'
(declare-function niwa-session-persistence-shallow-ensure-files
                  "niwa-session-persistence" (session buffer))

;; `niwa-tool-editing'
(declare-function niwa-tool-editing-actor "niwa-tool-editing" ())
(autoload 'niwa-tool-editing-actor "niwa-tool-editing")

(defvar niwa-tool-patch-prepared-proposal nil)

(defvar niwa-tool-patch--remote nil
  "The remote files of the proposal being planned, or nil.
A plist: `:files' is a hash table from shared://ALIAS/files/PATH address
to (:text TEXT :hash HASH), or (:binary t :data BASE64 :hash HASH) for
content that is not text; an address without an entry was not fetched,
as it is created.  `:workspace', `:actor' and `:authorize' are the
workspace, attribution and authority the files are written with.")

(define-error 'niwa-tool-patch-remote-stale
  "Remote file changed since it was read")

(defconst niwa-tool-patch--begin "*** Begin Patch")
(defconst niwa-tool-patch--end "*** End Patch")
(defconst niwa-tool-patch--add "*** Add File: ")
(defconst niwa-tool-patch--delete "*** Delete File: ")
(defconst niwa-tool-patch--update "*** Update File: ")
(defconst niwa-tool-patch--move "*** Move to: ")
(defconst niwa-tool-patch--eof "*** End of File")


(defun niwa-tool-patch-resource-address-p (value)
  "Return non-nil when VALUE is a resource-address-shaped operand."
  (and (stringp value)
       (niwa-resource-address-like-p value)))

(defun niwa-tool-patch-physical-path (operation)
  "Return the resolved filesystem path for OPERATION's source."
  (or (plist-get operation :physical-path)
      (plist-get operation :path)))

(defun niwa-tool-patch-remote-p (path)
  "Return (ALIAS . FILE) when PATH is a remote artifact's file address."
  (and (stringp path) (string-prefix-p "shared://" path)
       (niwa-resource-remote-file path)))

(defun niwa-tool-patch--physical-move-path (operation)
  "Return the resolved filesystem path for OPERATION's destination."
  (or (plist-get operation :physical-move-path)
      (plist-get operation :move-path)))

(defun niwa-tool-patch-sanitize-error (message proposal)
  "Replace private resource paths in MESSAGE with authored operands.
PROPOSAL is the prepared proposal whose physical fields are private."
  (dolist (operation (and proposal (plist-get proposal :operations)) message)
    (dolist (entry '((:permission-path :rel-path :resource-path-p)
                     (:permission-move-path :move-rel-path :resource-move-path-p)
                     (:physical-path :rel-path :resource-path-p)
                     (:physical-move-path :move-rel-path
                                          :resource-move-path-p)))
      (let ((physical (plist-get operation (nth 0 entry)))
            (authored (plist-get operation (nth 1 entry)))
            (resource-p (plist-get operation (nth 2 entry))))
        (when (and resource-p physical authored)
          (setq message
                (replace-regexp-in-string
                 (regexp-quote physical) authored message t t)))))))

(defun niwa-tool-patch--ensure-work-session ()
  "Return the current durable session for a session-owned work operand."
  (let ((session (and (boundp 'niwa--session) niwa--session))
        (request (and (boundp 'niwa--current-request)
                      niwa--current-request)))
    (unless session
      (error "Session working files require a session (work://)"))
    (when (and request (niwa-request-ephemeral-p request))
      (error "Ephemeral requests cannot write session working files (work://)"))
    (unless (niwa-session-save-path session)
      (require 'niwa-session-persistence)
      (unless (niwa-session-persistence-shallow-ensure-files
               session (current-buffer))
        (error "Could not create storage for session working files (work://)")))
    (make-directory
     (file-name-concat (niwa-session-save-path session) "local") t)
    session))

(defun niwa-tool-patch-prepare-resources (proposal)
  "Prepare resource operands in PROPOSAL once.

The authored operands stay in the regular operation fields.  Resolved
filesystem targets are kept in private fields consumed by the existing patch
transaction, so review and result data continue to use authored addresses.
Preparation leaves local sessions untouched until permission and plan checks
have completed."
  (let (operands (session (bound-and-true-p niwa--session)) (session-only-p t))
    (dolist (operation (plist-get proposal :operations))
      (dolist (address (delq nil (list (plist-get operation :rel-path)
                                       (plist-get operation :move-rel-path))))
        (unless (niwa-resource-session-work-p address)
          (setq session-only-p nil)))
      (dolist (entry '((:rel-path :physical-path :resource-attempt-path
                        :resource-path-p)
                       (:move-rel-path :physical-move-path
                                       :resource-attempt-move-path
                                       :resource-move-path-p)))
        (let ((address (plist-get operation (nth 0 entry))))
          (when (niwa-tool-patch-resource-address-p address)
            (push (list operation address (nth 1 entry) (nth 2 entry)
                        (nth 3 entry))
                  operands)))))
    (unless (and operands session-only-p)
      (setq session-only-p nil))
    (let ((remote (cl-loop for operation in (plist-get proposal :operations)
                           append (mapcar #'niwa-tool-patch-remote-p
                                          (delq nil (list (plist-get operation :rel-path)
                                                          (plist-get operation :move-rel-path)))))))
      (when (and (memq nil remote) (delq nil remote))
        (error "A patch of a remote artifact's files cannot change other files; split it into separate patches"))
      ;; Its origin writes them at once, so all of them land or none.
      (when (cdr (delete-dups (mapcar #'car (remq nil remote))))
        (error "A patch changes the files of one remote artifact; split it into one patch per artifact")))
    (dolist (entry operands)
      (pcase-let ((`(,operation ,address ,physical-key ,attempt-key ,resource-key)
                   entry))
        (let* ((attempt
                (niwa-resource-prepare
                 'apply-patch address
                 (list :session session
                       :workspace (and session
                                       (niwa-session-workspace session)))))
               (remote-p (niwa-tool-patch-remote-p address)))
          ;; A remote file has no backing path to resolve: its address is
          ;; what planning and the commit work with, also in place of the
          ;; operand's path the pipeline canonicalized as a local one.
          (plist-put operation attempt-key (unless remote-p attempt))
          (when remote-p (plist-put operation physical-key address))
          (plist-put operation
                     (if (eq physical-key :physical-path)
                         :resource-check-path :resource-check-move-path)
                     (niwa-resource-attempt-write-check attempt))
          (plist-put operation resource-key t)
          (plist-put operation
                     (if (eq physical-key :physical-path)
                         :permission-path :permission-move-path)
                     (unless remote-p
                       (niwa-resource-attempt-write-path attempt))))))
    (plist-put proposal :session-only-p session-only-p)))

(defun niwa-tool-patch--materialize-resources (proposal)
  "Materialize prepared resource attempts in PROPOSAL after authorization."
  (let (session-work-p)
    (dolist (operation (plist-get proposal :operations))
      (when (or (niwa-resource-session-work-p (plist-get operation :rel-path))
                (niwa-resource-session-work-p (plist-get operation :move-rel-path)))
        (setq session-work-p t)))
    (when session-work-p
      (niwa-tool-patch--ensure-work-session))
    (dolist (operation (plist-get proposal :operations))
      (dolist (entry '((:resource-attempt-path :physical-path)
                       (:resource-attempt-move-path :physical-move-path)))
        (when-let* ((attempt (plist-get operation (car entry))))
          (let (physical)
            (niwa-resource-execute
             attempt (lambda (path _authored) (setq physical path)))
            (unless physical
              (error "Writable resource owner is unavailable: %s"
                     (plist-get operation (if (eq (car entry) :resource-attempt-path)
                                              :rel-path :move-rel-path))))
            (plist-put operation (nth 1 entry) physical)
            (plist-put operation (car entry) nil)))))
    proposal))

(defun niwa-tool-patch--operation-marker-p (line)
  "Return non-nil when LINE begins a patch file operation."
  (or (string-prefix-p niwa-tool-patch--add line)
      (string-prefix-p niwa-tool-patch--delete line)
      (string-prefix-p niwa-tool-patch--update line)))

(defun niwa-tool-patch--marker-path (line marker line-number)
  "Return the path following MARKER in LINE at LINE-NUMBER."
  (let ((path (substring line (length marker))))
    (when (string-empty-p path)
      (error "Invalid patch at line %d: Missing file path" line-number))
    path))

(defun niwa-tool-patch-parse-update-lines (lines first-line)
  "Parse update payload LINES beginning at FIRST-LINE into hunks.
A hunk's `@@' anchor is either a context line or a bare positive line
number; the latter becomes `:line-hint' and only breaks ties between
otherwise ambiguous matches.  A hunk made of context lines alone is a
valid locator: it pins the position of the hunks after it and applies
as a no-op.  Each hunk records the patch line of its `@@' marker (or
first content line) as `:at-line' for error reporting."
  (let (hunks current)
    (cl-labels
        ((new-hunk (anchor at-line)
           (let ((numeric (and anchor
                               (string-match-p "\\`[1-9][0-9]*\\'" anchor))))
             (list :context (unless numeric anchor)
                   :line-hint (and numeric (string-to-number anchor))
                   :at-line at-line
                   :old-lines nil :new-lines nil
                   :diff-lines nil :eof nil :selected t)))
         (ensure-current (at-line)
           (unless current
             (setq current (new-hunk nil at-line))))
         (finish-current ()
           (when current
             (unless (plist-get current :diff-lines)
               (error "Invalid patch at line %d: Update hunk is empty"
                      (plist-get current :at-line)))
             (setq current
                   (plist-put current :old-lines
                              (nreverse (plist-get current :old-lines))))
             (setq current
                   (plist-put current :new-lines
                              (nreverse (plist-get current :new-lines))))
             (setq current
                   (plist-put current :diff-lines
                              (nreverse (plist-get current :diff-lines))))
             (push current hunks)
             (setq current nil))))
      (cl-loop for line in lines
               for line-number from first-line
               ;; Codex matches update markers after stripping trailing
               ;; whitespace; content lines stay raw.
               for marker = (string-trim-right line)
               do
               (cond
                ((or (string= marker "@@")
                     (string-prefix-p "@@ " marker))
                 (finish-current)
                 (setq current
                       (new-hunk (unless (string= marker "@@")
                                   (substring marker 3))
                                 line-number)))
                ((string= marker niwa-tool-patch--eof)
                 (ensure-current line-number)
                 (setq current (plist-put current :eof t)))
                ((string-empty-p line)
                 ;; Codex leniency: a bare empty line is an empty context
                 ;; line whose space marker the model dropped.
                 (ensure-current line-number)
                 (push " " (plist-get current :diff-lines))
                 (push "" (plist-get current :old-lines))
                 (push "" (plist-get current :new-lines)))
                ((memq (aref line 0) '(?\s ?+ ?-))
                 (ensure-current line-number)
                 (let ((text (substring line 1)))
                   (push line (plist-get current :diff-lines))
                   (pcase (aref line 0)
                     (?\s
                      (push text (plist-get current :old-lines))
                      (push text (plist-get current :new-lines)))
                     (?+
                      (push text (plist-get current :new-lines)))
                     (?-
                      (push text (plist-get current :old-lines))))))
                (t
                 (error "Invalid patch at line %d: Invalid update line"
                        line-number))))
      (finish-current))
    (nreverse hunks)))

(defun niwa-tool-patch-hunk-changes-p (hunk)
  "Return non-nil when HUNK adds or removes at least one line.
A hunk without changes is a locator: it anchors the position of the
hunks after it and applies as a no-op."
  (cl-some (lambda (line)
             (memq (aref line 0) '(?+ ?-)))
           (plist-get hunk :diff-lines)))

(defun niwa-tool-patch-parse (patch &optional root)
  "Parse PATCH relative to ROOT and return a patch proposal plist.
Like Codex, structural markers are matched after trimming surrounding
whitespace, one `*** Environment ID:' line after the opening marker is
tolerated, and content lines are kept raw."
  (unless (stringp patch)
    (error "Parameter patch is required"))
  (let* ((root (file-name-as-directory (expand-file-name (or root default-directory))))
         (target
          (or (and (boundp 'niwa--session)
                   niwa--session
                   (niwa-session-execution-target niwa--session))
              (niwa-execution-target-create root)))
         (resolve
          (lambda (path)
            (if (niwa-tool-patch-resource-address-p path)
                path
              (niwa-pipeline-canonical-path
               (niwa-execution-target-expand-path target path root)))))
         (normalized (replace-regexp-in-string "\r\n?" "\n" patch))
         (lines (vconcat (split-string (string-trim normalized) "\n" nil)))
         (count (length lines))
         (index 1)
         operations)
    (unless (and (>= count 3)
                 (string= (string-trim (aref lines 0))
                          niwa-tool-patch--begin))
      (error "Invalid patch: The first line must be '%s'"
             niwa-tool-patch--begin))
    (unless (string= (string-trim (aref lines (1- count)))
                     niwa-tool-patch--end)
      (error "Invalid patch: The last line must be '%s'"
             niwa-tool-patch--end))
    (when (string-prefix-p "*** Environment ID: "
                           (string-trim (aref lines 1)))
      (setq index 2))
    (while (< index (1- count))
      (let* ((line (string-trim (aref lines index)))
             (line-number (1+ index)))
        (cond
         ((string-prefix-p niwa-tool-patch--add line)
          (let ((rel-path (niwa-tool-patch--marker-path
                           line niwa-tool-patch--add line-number))
                added)
            (setq index (1+ index))
            (while (and (< index (1- count))
                        (not (niwa-tool-patch--operation-marker-p
                              (string-trim (aref lines index)))))
              (let ((added-line (aref lines index)))
                (unless (string-prefix-p "+" added-line)
                  (error "Invalid patch at line %d: Add lines must start with '+'"
                         (1+ index)))
                (push (substring added-line 1) added))
              (setq index (1+ index)))
            (unless added
              (error "Invalid patch at line %d: Add File requires content"
                     line-number))
            (push (list :kind 'add :path (funcall resolve rel-path)
                        :rel-path rel-path
                        :content (concat (string-join (nreverse added) "\n") "\n")
                        :selected t)
                  operations)))
         ((string-prefix-p niwa-tool-patch--delete line)
          (let ((rel-path (niwa-tool-patch--marker-path
                           line niwa-tool-patch--delete line-number)))
            (push (list :kind 'delete :path (funcall resolve rel-path)
                        :rel-path rel-path :selected t)
                  operations)
            (setq index (1+ index))))
         ((string-prefix-p niwa-tool-patch--update line)
          (let* ((rel-path (niwa-tool-patch--marker-path
                            line niwa-tool-patch--update line-number))
                 (path (funcall resolve rel-path))
                 move-rel payload)
            (setq index (1+ index))
            (when (and (< index (1- count))
                       (string-prefix-p niwa-tool-patch--move
                                        (string-trim-right
                                         (aref lines index))))
              (setq move-rel
                    (niwa-tool-patch--marker-path
                     (string-trim-right (aref lines index))
                     niwa-tool-patch--move (1+ index)))
              (setq index (1+ index)))
            (let ((payload-first-line (1+ index)))
              ;; Update payload markers only tolerate trailing
              ;; whitespace, like Codex: an indented header inside an
              ;; update body stays an ordinary context line.
              (while (and (< index (1- count))
                          (not (niwa-tool-patch--operation-marker-p
                                (string-trim-right (aref lines index)))))
                (push (aref lines index) payload)
                (setq index (1+ index)))
              (setq payload (nreverse payload))
              (when (and (null move-rel) (null payload))
                (error "Invalid patch at line %d: Update File requires a hunk"
                       line-number))
              (let ((hunks (and payload
                                (niwa-tool-patch-parse-update-lines
                                 payload payload-first-line))))
                ;; Locator hunks are valid, but an Update built from
                ;; them alone would rewrite the file byte-identically;
                ;; a Move without changes is still a real operation.
                (when (and (null move-rel) hunks
                           (not (cl-some #'niwa-tool-patch-hunk-changes-p
                                         hunks)))
                  (error "Invalid patch at line %d: Update for %s has no changes"
                         line-number rel-path))
                (push (list :kind (if move-rel 'move 'update)
                            :path path :rel-path rel-path
                            :move-path (and move-rel (funcall resolve move-rel))
                            :move-rel-path move-rel
                            :hunks hunks
                            :selected t)
                      operations)))))
         (t
          (error "Invalid patch at line %d: Expected a file operation"
                 line-number)))))
    (unless operations
      (error "Invalid patch: At least one file operation is required"))
    (setq operations (nreverse operations))
    (cl-loop for operation in operations for operation-index from 1 do
             (cl-loop for hunk in (plist-get operation :hunks)
                      for hunk-index from 1 do
                      (plist-put hunk :operation-index operation-index)
                      (plist-put hunk :hunk-index hunk-index)))
    (list :patch normalized :root root :operations operations)))

(defun niwa-tool-patch--read-file (path &optional with-coding)
  "Return text contents of regular file PATH.
When WITH-CODING is non-nil, return (CONTENTS . CODING-SYSTEM).  A remote
artifact's file is read from `niwa-tool-patch--remote'."
  (if-let* ((_ (niwa-tool-patch-remote-p path))
            (entry (niwa-tool-patch--remote-entry path)))
      (progn
        (when (plist-get entry :binary)
          (error "Cannot patch a file that is not text: %s" path))
        (if with-coding
            (cons (plist-get entry :text) 'utf-8-unix)
          (plist-get entry :text)))
    (unless (file-exists-p path)
      (error "File does not exist: %s" path))
    (when (file-directory-p path)
      (error "Cannot patch a directory: %s" path))
    (with-temp-buffer
      (niwa-pin-with-file path
        (niwa-pin-insert-file-contents path))
      (let ((content (buffer-string)))
        (if with-coding
            (cons content last-coding-system-used)
          content)))))

(defun niwa-tool-patch--remote-entry (address)
  "Return the fetched remote file at ADDRESS, or signal that it does not exist."
  (or (when-let* ((files (plist-get niwa-tool-patch--remote :files)))
        (gethash address files))
      (error "File does not exist: %s" address)))

(defun niwa-tool-patch--remote-change (path &optional absent)
  "Return the remote fields of a planned change to PATH, nil for a local one.
They are `:remote' (ALIAS . FILE) and the `:hash' the content was read
with, none for a change creating the file, which ABSENT marks.
`:origin' is `niwa-tool-patch--remote'."
  (when-let* ((remote (niwa-tool-patch-remote-p path)))
    (let ((entry (unless absent (niwa-tool-patch--remote-entry path))))
      (list :remote remote :hash (plist-get entry :hash)
            :origin niwa-tool-patch--remote))))

(defun niwa-tool-patch--remote-operands (proposal)
  "Return the remote file addresses PROPOSAL reads before planning.
Added files and move destinations are created, so they are not read."
  (delete-dups
   (cl-loop for operation in (plist-get proposal :operations)
            for path = (plist-get operation :rel-path)
            when (and (not (eq (plist-get operation :kind) 'add))
                      (niwa-tool-patch-remote-p path))
            collect path)))

(defun niwa-tool-patch--fetch-remote (proposal workspace callback)
  "Read PROPOSAL's remote files from WORKSPACE's artifact origins.
Record them on PROPOSAL, then call CALLBACK with nil, or with the error
condition of the first file that could not be read."
  (let* ((addresses (niwa-tool-patch--remote-operands proposal))
         (files (make-hash-table :test #'equal))
         (pending (length addresses))
         (settled nil))
    (plist-put (plist-get proposal :remote-artifact) :files files)
    (cl-flet ((finish (failure)
                (unless settled
                  (when (or failure (zerop pending))
                    (setq settled t)
                    (funcall callback failure)))))
      (if (null addresses)
          (finish nil)
        (dolist (address addresses)
          (pcase-let ((`(,alias . ,file) (niwa-tool-patch-remote-p address)))
            (if (not (niwa-artifact-backend-registered-p workspace alias))
                (finish (list 'error (format "Remote artifact not attached: %s; Read shared:// to list this project's artifacts"
                                             address)))
              (niwa-artifact-backend-call
               workspace (list :action "file" :id alias :file file)
               (lambda (reply)
                 (if-let* ((message (plist-get reply :error)))
                     (finish (list 'error (format "Cannot read %s: %s" address message)))
                   (let ((result (plist-get reply :result)))
                     (puthash address
                              (if (stringp (plist-get result :text))
                                  (list :text (plist-get result :text) :hash (plist-get result :hash))
                                (list :binary t :data (plist-get result :data)
                                      :hash (plist-get result :hash)))
                              files)
                     (cl-decf pending)
                     (finish nil))))))))))))

(defun niwa-tool-patch--remote-request (changes)
  "Return the origin request carrying out the planned remote CHANGES.
They change files of one remote artifact, all in this one request."
  (list :action "write-files" :id (car (plist-get (car changes) :remote))
        :changes (vconcat
                  (mapcar (lambda (change)
                            (append (list :file (cdr (plist-get change :remote)))
                                    (when-let* ((hash (plist-get change :hash)))
                                      (list :hash hash))
                                    (if (eq (plist-get change :action) 'delete)
                                        (list :delete t)
                                      (list :text (plist-get change :content)))))
                          changes))))

(defun niwa-tool-patch--remote-refusal (changes reply)
  "Return the error condition of the origin's REPLY refusing remote CHANGES."
  (let* ((change (cl-find (plist-get reply :file) changes
                          :key (lambda (change) (cdr (plist-get change :remote)))
                          :test #'equal))
         (address (plist-get change :path)))
    (cond
     ((not (and change (equal (plist-get reply :code) "stale")))
      (list 'error (plist-get reply :error)))
     ;; A creation is refused when the file exists, as locally.
     ((and (eq (plist-get change :action) 'write) (null (plist-get change :hash)))
      (list 'error (format "Cannot add existing file: %s" address)))
     (t (list 'niwa-tool-patch-remote-stale
              (format "%s changed at its origin since it was read; nothing was applied.  Read it again and redo the patch"
                      address))))))

(defun niwa-tool-patch--commit-remote (changes continuation fail)
  "Write remote CHANGES at their origin, then call CONTINUATION.
They go in one request naming the hash each file was read with, and the
origin checks every hash before it changes any file, so the patch lands
whole or not at all.  On a refusal FAIL receives an error condition,
`niwa-tool-patch-remote-stale' for a file changed since it was read."
  (let ((origin (plist-get (car changes) :origin)))
    (niwa-artifact-backend-call
     (plist-get origin :workspace)
     (append (niwa-tool-patch--remote-request changes)
             (list :actor (plist-get origin :actor)))
     (lambda (reply)
       (if (plist-get reply :error)
           (funcall fail (niwa-tool-patch--remote-refusal changes reply))
         (funcall continuation)))
     (plist-get origin :authorize))))

(defun niwa-tool-patch--lines (text)
  "Split TEXT into (LINE . ENDING) pairs, retaining a final empty line."
  (let ((start 0) lines)
    (while (string-match "\r?\n" text start)
      (push (cons (substring text start (match-beginning 0))
                  (match-string 0 text)) lines)
      (setq start (match-end 0)))
    (nreverse (cons (cons (substring text start) nil) lines))))

(defun niwa-tool-patch-content-lines (content)
  "Return logical diff lines for complete file CONTENT."
  (unless (string-empty-p content)
    (let ((lines (mapcar #'car (niwa-tool-patch--lines content))))
      (if (string-suffix-p "\n" content) (butlast lines) lines))))

(defconst niwa-tool-patch--ascii-folds
  (append
   (mapcar (lambda (char) (cons char ?-))
           '(#x2010 #x2011 #x2012 #x2013 #x2014 #x2015 #x2212))
   (mapcar (lambda (char) (cons char ?'))
           '(#x2018 #x2019 #x201A #x201B))
   (mapcar (lambda (char) (cons char ?\"))
           '(#x201C #x201D #x201E #x201F))
   (mapcar (lambda (char) (cons char ?\s))
           '(#xA0 #x2002 #x2003 #x2004 #x2005 #x2006 #x2007
             #x2008 #x2009 #x200A #x202F #x205F #x3000)))
  "Typographic punctuation folded to ASCII during fuzzy hunk matching.
Dash and hyphen codepoints fold to `-', curly quotes to their ASCII
quotes, and exotic spaces to a plain space, mirroring Codex.")

(defun niwa-tool-patch--rstrip (line)
  "Return LINE without trailing horizontal whitespace."
  (string-trim-right line "[ \t]+"))

(defun niwa-tool-patch--normalize-line (line)
  "Return LINE trimmed, with typographic punctuation folded to ASCII."
  (string-trim
   (mapconcat (lambda (char)
                (char-to-string
                 (alist-get char niwa-tool-patch--ascii-folds char)))
              line "")))

(defconst niwa-tool-patch--match-passes
  (list (cons #'identity nil)
        (cons #'niwa-tool-patch--rstrip 'trailing-whitespace)
        (cons #'string-trim 'whitespace)
        (cons #'niwa-tool-patch--normalize-line 'punctuation))
  "Line normalizers with their pass labels, in decreasing strictness.
The label nil marks an exact match; fuzzy labels are recorded on the
hunk and surfaced in the review and the tool result.")

(defun niwa-tool-patch-match-pass-description (pass)
  "Return the human-readable description of fuzzy match PASS."
  (pcase pass
    ('trailing-whitespace "ignoring trailing whitespace")
    ('whitespace "ignoring surrounding whitespace")
    ('punctuation "normalizing typographic punctuation")))

(defun niwa-tool-patch--sequence-match-p (norm-lines norm-pattern start)
  "Return non-nil when NORM-PATTERN matches NORM-LINES at START.
NORM-LINES is a vector and NORM-PATTERN a list; both hold lines already
normalized by the active matching pass."
  (and (<= (+ start (length norm-pattern)) (length norm-lines))
       (cl-loop for expected in norm-pattern
                for index from start
                always (string= (aref norm-lines index) expected))))

(defun niwa-tool-patch--candidate-starts
    (lines hunk normalize &optional ignore-eof)
  "Return candidate match starts for HUNK in LINES under NORMALIZE.
IGNORE-EOF drops the end-of-file anchoring requirement of an
`*** End of File' hunk."
  (let* ((pattern (plist-get hunk :old-lines))
         (pattern-length (length pattern))
         (context (plist-get hunk :context))
         (limit (- (length lines) pattern-length))
         starts)
    (when (>= limit 0)
      ;; Normalizing is the dominant cost of a matching pass, so every
      ;; line is normalized exactly once per call instead of once per
      ;; candidate start.
      (let ((norm-lines (vconcat (mapcar normalize lines)))
            (norm-pattern (mapcar normalize pattern))
            (norm-context (and context (funcall normalize context)))
            ;; The @@ context anchor must appear strictly before the
            ;; candidate; track it incrementally as starts advance.
            (context-seen (null context))
            (eof-end (and (plist-get hunk :eof)
                          (not ignore-eof)
                          (if (string-empty-p (car (last lines)))
                              (1- (length lines))
                            (length lines)))))
        (dotimes (start (1+ limit))
          ;; An insertion must immediately follow its anchor; for a
          ;; nonempty pattern, any position below the anchor is eligible.
          (when (and context (or (not context-seen) (null pattern)))
            (setq context-seen
                  (and (> start 0)
                       (string-prefix-p norm-context
                                        (aref norm-lines (1- start))))))
          (when (and context-seen
                     (niwa-tool-patch--sequence-match-p
                      norm-lines norm-pattern start)
                     (or (not eof-end)
                         (= (+ start pattern-length) eof-end)))
            (push start starts)))))
    (nreverse starts)))

(defun niwa-tool-patch--mismatch-detail (lines hunk path current-lines)
  "Describe HUNK's mismatch in CURRENT-LINES using baseline LINES and PATH.
Only exact substring evidence supplies a file-line hint.  A sequence or anchor
failure must not suggest that a complete line is merely a fragment.
CURRENT-LINES supplies anchor eligibility after earlier pending edits."
  (let* ((normalized (make-hash-table :test #'equal))
         (anchor (and (plist-get hunk :context)
                      (niwa-tool-patch--normalize-line
                       (plist-get hunk :context))))
         (anchor-position (and (null anchor) 0)))
    (dolist (line current-lines)
      (let ((folded (niwa-tool-patch--normalize-line line)))
        (puthash folded t normalized)))
    (when anchor
      (cl-loop for line in current-lines for number from 1
               when (string-prefix-p
                     anchor (niwa-tool-patch--normalize-line line))
               do (setq anchor-position number) and return nil))
    (when anchor-position
      (when-let* ((diff-line
                   (cl-find-if
                    (lambda (line)
                      (and (memq (aref line 0) '(?\s ?-))
                           (not (gethash
                                 (niwa-tool-patch--normalize-line
                                  (substring line 1)) normalized))))
                    (plist-get hunk :diff-lines))))
        (let* ((text (substring diff-line 1))
               (kind (if (eq (aref diff-line 0) ?-) "removal" "context"))
               (print-escape-control-characters t)
               (excerpt (prin1-to-string
                         (if (> (length text) 120)
                             (concat (substring text 0 120) "...")
                           text)))
               candidates)
          (unless (string-empty-p text)
            (cl-loop for line in lines for number from 1
                     when (string-search text line)
                     do (push (cons number line) candidates)
                     until (> (length candidates) 1)))
          (concat
           (format "; %s line %s" kind excerpt)
           (if (and (= (length candidates) 1)
                    (not (equal (niwa-tool-patch--normalize-line text)
                                (niwa-tool-patch--normalize-line
                                 (cdar candidates))))
                    (or (null anchor)
                        (when-let* ((current-position
                                     (cl-position-if
                                      (lambda (line) (string-search text line))
                                      current-lines)))
                          (> (1+ current-position) anchor-position))))
               (let* ((candidate (car candidates))
                      (relation
                       (cond ((string-prefix-p text (cdr candidate)) "a prefix")
                             ((string-suffix-p text (cdr candidate)) "a suffix")
                             (t "a middle substring"))))
                 (format
                  (concat " is only %s of file line %d. Context and removal lines"
                          " must be whole lines; Read(file_path=%S, offset=%d, limit=1)")
                  relation (car candidate) path (car candidate)))
             " was not found as a whole line")))))))

(defun niwa-tool-patch--match-start
    (lines hunk path &optional after line-offset baseline-lines)
  "Return the unique start for HUNK in LINES from PATH.
Matching passes decrease in strictness: exact, ignoring trailing
whitespace, ignoring surrounding whitespace, then folding typographic
punctuation to ASCII.  An `*** End of File' hunk is first anchored to
the end of LINES, then retried unanchored.  Unlike Codex, every pass
still requires a unique match.  Ties are broken in two steps: when
AFTER is non-nil, the candidates at or after that line index are
preferred, since hunks arrive in file order; a remaining tie is then
settled by HUNK's `:line-hint', the bare line number of a numeric `@@'
anchor, which selects the nearest candidate.  LINE-OFFSET translates
that baseline hint into the current LINES coordinates.  BASELINE-LINES
supplies original file coordinates for failure hints after earlier edits.
Neither step can empty the candidate set, so a stale hint never rejects
a hunk on its own.  The winning pass label is recorded on HUNK as `:match-pass'
(nil for an exact match)."
  (let (starts pass)
    (cl-loop
     for ignore-eof in (if (plist-get hunk :eof) '(nil t) '(nil))
     do (cl-loop for (normalize . label) in niwa-tool-patch--match-passes
                 do (setq starts (niwa-tool-patch--candidate-starts
                                  lines hunk normalize ignore-eof)
                          pass label)
                 until starts)
     until starts)
    (when (and after (> (length starts) 1))
      (let ((ordered (seq-filter (lambda (start) (>= start after)) starts)))
        (when ordered
          (setq starts ordered))))
    ;; ponytail: nearest candidate wins with no distance ceiling; add a
    ;; window if a far-off hint is ever seen picking the wrong region.
    (when-let* ((hint (and (> (length starts) 1) (plist-get hunk :line-hint))))
      (let* ((target (+ (1- hint) (or line-offset 0)))
             (distance (lambda (start) (abs (- start target))))
             (best (apply #'min (mapcar distance starts)))
             (nearest (seq-filter (lambda (start)
                                    (= best (funcall distance start)))
                                  starts)))
        (when (= 1 (length nearest))
          (setq starts nearest))))
    (plist-put hunk :match-pass (and starts pass))
    (let ((label (if-let* ((operation (plist-get hunk :operation-index)))
                     (format "Patch operation %d, hunk %d" operation
                             (plist-get hunk :hunk-index))
                   "Patch hunk"))
          (where (if-let* ((at-line (plist-get hunk :at-line)))
                     (format " (patch line %d)" at-line)
                   "")))
      (pcase (length starts)
        (0 (error "%s%s does not match %s%s" label where path
                  (or (niwa-tool-patch--mismatch-detail
                       (or baseline-lines lines) hunk path lines) "")))
        (1 (car starts))
        (_ (error "%s%s is ambiguous in %s; add distinguishing context or use @@ N from Read"
                  label where path))))))

(defun niwa-tool-patch--section-label (lines start)
  "Return a diff-style section label for a hunk at START in LINES.
Like diff's function context: the closest line at or above START whose
first character is not whitespace or a closing bracket."
  (cl-loop for index downfrom start to 0
           for line = (nth index lines)
           when (and (> (length line) 0)
                     (not (memq (aref line 0) '(?\s ?\t ?\) ?\] ?\}))))
           return (niwa--truncate-display line 48 "…")))

(defun niwa-tool-patch-annotate-line-numbers (proposal)
  "Attach display start lines and section labels to hunks in PROPOSAL.
Old and new file positions are computed against the captured baseline
content.  A hunk that no longer matches uniquely keeps nil positions
and renders without gutter numbers."
  (dolist (operation (plist-get proposal :operations))
    (when (memq (plist-get operation :kind) '(update move))
      (let* ((content (plist-get operation :baseline-content))
             (lines (and content (mapcar #'car (niwa-tool-patch--lines content))))
             (offset 0)
             (after 0))
        (dolist (hunk (plist-get operation :hunks))
          (let ((start (and lines
                            (ignore-errors
                              (niwa-tool-patch--match-start
                               lines hunk
                               (niwa-tool-patch-physical-path operation)
                               after)))))
            (plist-put hunk :old-start (and start (1+ start)))
            (plist-put hunk :new-start (and start (+ start 1 offset)))
            (plist-put hunk :section
                       (and start
                            (niwa-tool-patch--section-label lines start)))
            (when start
              (setq after (+ start (length (plist-get hunk :old-lines))))
              (cl-incf offset
                       (- (length (plist-get hunk :new-lines))
                          (length (plist-get hunk :old-lines)))))))))))

(defun niwa-tool-patch--hunk-replacement (lines start hunk eol)
  "Replace HUNK at START in paired LINES, using EOL for unmapped additions."
  (let ((remaining (nthcdr start lines))
        (diff (plist-get hunk :diff-lines)) replacement)
    (while diff
      (if (= (aref (car diff) 0) ?\s)
          (progn (push (pop remaining) replacement) (pop diff))
        (let (added endings)
          (while (and diff (/= (aref (car diff) 0) ?\s))
            (let ((line (pop diff)))
              (if (= (aref line 0) ?-)
                  (push (cdr (pop remaining)) endings)
                (push (substring line 1) added))))
          (let ((unterminated (and endings (null (car endings)))))
            (setq endings (nreverse endings)
                  added (nreverse added))
            (while added
              (let ((text (pop added)))
                (push (cons text (unless (and unterminated (null added))
                                   (or (pop endings) eol)))
                      replacement)))))))
    (nreverse replacement)))

(defun niwa-tool-patch-apply-hunks (content hunks path)
  "Return CONTENT with selected HUNKS applied for PATH.
Replacements inherit the removed lines' endings within each change run;
extra lines use the majority ending (LF on ties).  Deselected hunks still
advance the order-disambiguation cursor when they match, so a later
hunk that previewed cleanly stays unambiguous after a deselection."
  (let* ((records (niwa-tool-patch--lines content))
         (crlf (cl-count "\r\n" records :key #'cdr :test #'equal))
         (eol (if (> (* 2 crlf) (1- (length records))) "\r\n" "\n"))
         (baseline-lines (mapcar #'car records))
         (lines baseline-lines)
         (after 0)
         (line-offset 0))
    (dolist (hunk hunks)
      (if (plist-get hunk :selected)
          (let* ((start (niwa-tool-patch--match-start
                         lines hunk path after line-offset baseline-lines))
                 (old-count (length (plist-get hunk :old-lines)))
                 (replacement (niwa-tool-patch--hunk-replacement
                               records start hunk eol)))
            (setq records
                  (append (seq-take records start)
                          replacement
                          (nthcdr (+ start old-count) records))
                  lines (mapcar #'car records))
            (setq after (+ start (length replacement)))
            (cl-incf line-offset (- (length replacement) old-count)))
        (when-let* ((start (ignore-errors
                             (niwa-tool-patch--match-start
                              lines hunk path after line-offset))))
          (setq after (+ start (length (plist-get hunk :old-lines)))))))
    (with-temp-buffer
      (cl-loop for ((text . ending) . rest) on records
               do (insert text (or ending (and rest eol) "")))
      (buffer-string))))

(defun niwa-tool-patch--diff-update-lines (buffer)
  "Return BUFFER's unified diff output as update payload lines.
Unified headers become the bare `@@' this engine matches by context,
and diff's file headers and no-newline notices are dropped.  Content
lines always carry a marker in column zero, so a payload line that
begins a header can only be a header."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let (lines)
      (when (re-search-forward "^@@" nil t)
        (beginning-of-line)
        (while (and (not (eobp))
                    (not (looking-at-p "^Diff finished")))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (cond
             ((string-prefix-p "@@" line) (push "@@" lines))
             ((string-prefix-p "\\" line))
             (t (push line lines))))
          (forward-line 1)))
      ;; `diff-no-select' separates the hunks from its completion notice
      ;; with a blank line, which the payload parser would otherwise read
      ;; as an empty context line.
      (while (and lines (string-empty-p (car lines)))
        (pop lines))
      (nreverse lines))))

(defun niwa-tool-patch--diff-side (text)
  "Return TEXT as the exact bytes to diff for hunk derivation.
Both sides are reduced to their logical lines and given one final
newline, because a hunk cannot represent whether a file ends with one:
without this, a trailing-newline difference alone would derive a hunk
whose replacement re-joins to the text it claims to change."
  (if-let* ((lines (niwa-tool-patch-content-lines text)))
      (concat (string-join lines "\n") "\n")
    ""))

(defun niwa-tool-patch-hunks-from-content (baseline content)
  "Return update hunks that turn BASELINE into CONTENT.
Both contents are reduced to logical lines and diffed with three lines
of context, then parsed into this engine's hunk shape.  A derived hunk
therefore matches BASELINE by construction, and its context is
well-formed without anyone having written diff markers by hand.
Returns nil when the two contents hold the same lines."
  (require 'diff)
  (let ((baseline-buffer (generate-new-buffer " *niwa-patch-baseline*"))
        (content-buffer (generate-new-buffer " *niwa-patch-content*"))
        (diff-buffer (generate-new-buffer " *niwa-patch-diff*")))
    (unwind-protect
        (progn
          (with-current-buffer baseline-buffer
            (insert (niwa-tool-patch--diff-side baseline)))
          (with-current-buffer content-buffer
            (insert (niwa-tool-patch--diff-side content)))
          (diff-no-select baseline-buffer content-buffer "-U3" t diff-buffer)
          (when-let* ((lines (niwa-tool-patch--diff-update-lines
                              diff-buffer)))
            (niwa-tool-patch-parse-update-lines lines 1)))
      (kill-buffer baseline-buffer)
      (kill-buffer content-buffer)
      (kill-buffer diff-buffer))))

(defun niwa-tool-patch--affected-paths (operation)
  "Return all filesystem paths touched by OPERATION."
  (delq nil (list (niwa-tool-patch-physical-path operation)
                  (niwa-tool-patch--physical-move-path operation))))

(defun niwa-tool-patch--path-state (path)
  "Return the exact filesystem state needed to revalidate PATH."
  (cond
   ((niwa-tool-patch-remote-p path)
    (let ((entry (when-let* ((files (plist-get niwa-tool-patch--remote :files)))
                   (gethash path files))))
      (cond ((null entry) '(missing))
            ((plist-get entry :binary) (list 'binary (plist-get entry :hash)))
            (t (list 'file (plist-get entry :text))))))
   ((file-symlink-p path) (list 'symlink (file-symlink-p path)))
   ((not (file-exists-p path)) '(missing))
   ((file-directory-p path) '(directory))
   (t (list 'file (niwa-tool-patch--read-file path)))))

(defun niwa-tool-patch-capture-baseline (proposal)
  "Capture every affected path in PROPOSAL for later stale detection."
  (let ((niwa-tool-patch--remote (plist-get proposal :remote-artifact))
        baseline)
    (dolist (operation (plist-get proposal :operations))
      (let ((paths (niwa-tool-patch--affected-paths operation))
            (relative (delq nil
                            (list (plist-get operation :rel-path)
                                  (plist-get operation :move-rel-path)))))
        (cl-mapc
         (lambda (path rel-path)
           (let ((state (niwa-tool-patch--path-state path)))
             (when (and (equal path
                              (niwa-tool-patch-physical-path operation))
                        (eq (car state) 'file))
               (plist-put operation :baseline-content (cadr state)))
             (push (list :path path :rel-path rel-path :state state)
                   baseline)))
         paths relative)))
    (plist-put proposal :baseline (nreverse baseline))))

(defun niwa-tool-patch--selected-paths (proposal)
  "Return filesystem paths selected in PROPOSAL."
  (let (paths)
    (dolist (operation (plist-get proposal :operations))
      (when (if (eq (plist-get operation :kind) 'update)
                (cl-some (lambda (hunk)
                           (and (plist-get hunk :selected)
                                (niwa-tool-patch-hunk-changes-p hunk)))
                         (plist-get operation :hunks))
              (plist-get operation :selected))
        (setq paths (append paths
                            (niwa-tool-patch--affected-paths operation)))))
    paths))

(defun niwa-tool-patch-assert-baseline (proposal)
  "Signal when a selected path in PROPOSAL changed since review began.
A remote artifact's file is checked by its origin when it is written."
  (dolist (path (cl-remove-if #'niwa-tool-patch-remote-p
                              (niwa-tool-patch--selected-paths proposal)))
    (let ((entry (seq-find (lambda (item)
                             (equal path (plist-get item :path)))
                           (plist-get proposal :baseline))))
      (unless (and entry
                   (equal (plist-get entry :state)
                          (niwa-tool-patch--path-state path)))
        (error "File changed during review: %s"
               (or (plist-get entry :rel-path) path))))))

(defun niwa-tool-patch--validate-distinct-paths (operations)
  "Reject overlapping file paths across OPERATIONS."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (operation operations)
      (dolist (path (niwa-tool-patch--affected-paths operation))
        ;; ponytail: repeated-path operations are intentionally rejected until
        ;; a real patch needs sequential virtual-file semantics.
        (when (gethash path seen)
          (error "Patch touches path more than once: %s" path))
        (puthash path t seen)))))

(defun niwa-tool-patch-planned-changes (proposal)
  "Validate PROPOSAL and return its selected filesystem changes.
Prepared note reviews select changes against their captured source snapshots."
  (let* ((operations (plist-get proposal :operations))
         (snapshots (plist-get proposal :prepared-snapshots))
         (read-source
          (lambda (path)
            (if-let* ((snapshot (cl-find path snapshots :test #'equal
                                        :key (lambda (item) (plist-get item :path)))))
                (progn
                  (unless (plist-get snapshot :exists)
                    (error "File does not exist: %s" path))
                  (let* ((last-coding-system-used nil)
                         (text (decode-coding-string (plist-get snapshot :bytes) 'undecided)))
                    (cons text last-coding-system-used)))
              (niwa-tool-patch--read-file path t))))
         (niwa-tool-patch--remote (plist-get proposal :remote-artifact))
         (checks (cl-mapcan (lambda (operation)
                             (delq nil (list (plist-get operation :resource-check-path)
                                            (plist-get operation :resource-check-move-path))))
                           (plist-get proposal :operations)))
         changes)
    (niwa-tool-patch--validate-distinct-paths operations)
    (mapc #'funcall checks)
    (dolist (operation operations)
      (pcase (plist-get operation :kind)
        ('add
         (let ((path (niwa-tool-patch-physical-path operation)))
           (when (plist-get operation :selected)
             ;; A remote file's origin refuses to create an existing one.
             (when (and (not snapshots)
                        (not (niwa-tool-patch-remote-p path)) (file-exists-p path))
               (error "Cannot add existing file: %s" path))
             (push (append (list :action 'write :path path)
                           (and (plist-get operation :resource-path-p)
                                '(:resource-p t))
                           (niwa-tool-patch--remote-change path t)
                           (list :content (plist-get operation :content)))
                   changes))))
        ('delete
         (let ((path (niwa-tool-patch-physical-path operation)))
           (when (plist-get operation :selected)
             (if (niwa-tool-patch-remote-p path)
                 (niwa-tool-patch--remote-entry path)
               (funcall read-source path))
             (push (append (list :action 'delete :path path)
                           (and (plist-get operation :resource-path-p)
                                '(:resource-p t))
                           (niwa-tool-patch--remote-change path))
                   changes))))
        ('update
         (let ((path (niwa-tool-patch-physical-path operation))
               (hunks (plist-get operation :hunks)))
           ;; Pass every hunk: deselected ones advance the ordering
           ;; cursor inside `niwa-tool-patch-apply-hunks' so the
           ;; apply-time disambiguation window matches the preview's.
           ;; Selected locator hunks alone plan no write: they change
           ;; nothing, so rewriting the file byte-identically would
           ;; only bump its mtime.
           (when (cl-some (lambda (hunk)
                            (and (plist-get hunk :selected)
                                 (niwa-tool-patch-hunk-changes-p hunk)))
                          hunks)
             (pcase-let* ((`(,content . ,coding)
                           (funcall read-source path))
                          (updated
                           (niwa-tool-patch-apply-hunks
                            content hunks (or (plist-get operation :rel-path) path))))
               (push (append (list :action 'write :path path)
                             (and (plist-get operation :resource-path-p)
                                  '(:resource-p t))
                             (niwa-tool-patch--remote-change path)
                             (list :content updated
                                   :bytes
                                   (encode-coding-string updated coding)))
                     changes)))))
        ('move
         (let ((path (niwa-tool-patch-physical-path operation))
               (destination
                (niwa-tool-patch--physical-move-path operation)))
           (when (plist-get operation :selected)
             (pcase-let ((`(,content . ,coding)
                          (funcall read-source path)))
               (when (and (not snapshots)
                          (not (niwa-tool-patch-remote-p destination))
                          (file-exists-p destination))
                 (error "Cannot move to existing file: %s" destination))
               (push (append (list :action 'delete :path path)
                             (and (plist-get operation :resource-path-p)
                                  '(:resource-p t))
                             (niwa-tool-patch--remote-change path))
                     changes)
               (push (append (list :action 'write :path destination)
                             (and (plist-get operation :resource-move-path-p)
                                  '(:resource-p t))
                             (niwa-tool-patch--remote-change destination t)
                             (let ((updated
                                    (if-let* ((hunks
                                               (plist-get operation :hunks)))
                                        (niwa-tool-patch-apply-hunks
                                         content hunks (or (plist-get operation :rel-path) path))
                                      content)))
                               (append
                                (unless (niwa-tool-patch-remote-p path)
                                  (list :mode (if snapshots
                                                  (plist-get (cl-find path snapshots :test #'equal
                                                                      :key (lambda (item) (plist-get item :path)))
                                                             :mode)
                                                (file-modes path))))
                                (list :content updated
                                      :bytes
                                      (encode-coding-string updated coding)))))
                     changes)))))))
    ;; Every change of a proposal commits together, so one merged freshness
    ;; check guards each change; commit folds the shared closure into `currentp'.
    (when checks
      (let ((check (lambda () (cl-every #'funcall checks))))
        (dolist (change changes) (plist-put change :resource-check check))))
    (nreverse changes)))

(defun niwa-tool-patch-apply (data-buffer changes continuation &optional fail)
  "Apply CHANGES for DATA-BUFFER, then call CONTINUATION.
Refresh file tracking immediately and diagnostics before continuation.
Changes to a remote artifact's files are written at its origin, later;
when they cannot be, FAIL receives the error condition, which otherwise
signals here.  A remote change has no local file tracking or diagnostics."
  (if (plist-get (car changes) :remote)
      (niwa-tool-patch--commit-remote
       changes continuation (or fail (lambda (err) (signal (car err) (cdr err)))))
    (niwa-tool-patch--apply-local data-buffer changes continuation)))

(defun niwa-tool-patch--apply-local (data-buffer changes continuation)
  "Apply local CHANGES as `niwa-tool-patch-apply' does for DATA-BUFFER.
Call CONTINUATION after diagnostics."
  (niwa-change-assert-saved changes)
  (dolist (change changes)
    (unless (plist-get change :resource-p)
      (niwa-edit-diagnostics-before-edit
       data-buffer (plist-get change :path))))
  (niwa-change-commit changes)
  (when-let* ((session (and (buffer-live-p data-buffer)
                            (buffer-local-value 'niwa--session
                                                data-buffer))))
    (dolist (change changes)
      (unless (plist-get change :resource-p)
        (ignore-errors
          (niwa-session-record-file-access
           session (plist-get change :path) 'modify))))
    ;; The write is committed; bookkeeping failure must not fail the tool.
    (condition-case err
        (niwa-artifact-store-note-writes
         session changes (buffer-local-value 'niwa--current-request data-buffer))
      (error (niwa-diagnostic-message
              (niwa-diagnostic-scope session)
              "artifact store bookkeeping failed: %s"
              (error-message-string err)))))
  (cl-labels
      ((finish (remaining)
         (let ((remaining
                (cl-remove-if (lambda (change) (plist-get change :resource-p))
                              remaining)))
           (if remaining
               (niwa-edit-diagnostics-after-edit
                data-buffer (plist-get (car remaining) :path)
                (lambda () (finish (cdr remaining))))
             (funcall continuation)))))
    (finish changes)))

(defun niwa-tool-patch--selected-file-data (operation)
  "Return persisted per-file data for selected OPERATION changes."
  (let* ((kind (plist-get operation :kind))
         ;; Locator hunks are positioning aids, not changes; the
         ;; persisted diff shows only what the patch did.
         (hunks (pcase kind
                  ('update
                   (seq-filter (lambda (hunk)
                                 (and (plist-get hunk :selected)
                                      (niwa-tool-patch-hunk-changes-p
                                       hunk)))
                               (plist-get operation :hunks)))
                  ('move
                   (and (plist-get operation :selected)
                        (seq-filter #'niwa-tool-patch-hunk-changes-p
                                    (plist-get operation :hunks))))))
         (selected-p (if (eq kind 'update)
                         hunks
                       (plist-get operation :selected)))
         lines)
    (when selected-p
      (setq lines
            (pcase kind
              ('add
               (mapcar (lambda (line) (concat "+" line))
                       (niwa-tool-patch-content-lines
                        (plist-get operation :content))))
              ('delete
               (mapcar (lambda (line) (concat "-" line))
                       (niwa-tool-patch-content-lines
                        (plist-get operation :baseline-content))))
              ((or 'update 'move)
               (cl-mapcan
                (lambda (hunk)
                  (cons (if-let* ((context (plist-get hunk :context)))
                            (concat "@@ " context)
                          "@@")
                        (copy-sequence (plist-get hunk :diff-lines))))
                hunks))))
      (list :path (plist-get operation :rel-path)
            :move-path (plist-get operation :move-rel-path)
            :kind kind
            :added (cl-count-if (lambda (line) (string-prefix-p "+" line))
                                lines)
            :deleted (cl-count-if (lambda (line) (string-prefix-p "-" line))
                                  lines)
            :diff (string-join lines "\n")))))

(defun niwa-tool-patch-revised-count (proposal)
  "Return how many of PROPOSAL's applied changes the user revised."
  (let ((count 0))
    (dolist (operation (plist-get proposal :operations) count)
      (pcase (plist-get operation :kind)
        ('update
         (dolist (hunk (plist-get operation :hunks))
           (when (and (plist-get hunk :modified)
                      (plist-get hunk :selected))
             (cl-incf count))))
        ('move
         (when (plist-get operation :selected)
           (dolist (hunk (plist-get operation :hunks))
             (when (plist-get hunk :modified)
               (cl-incf count)))))
        ('add
         (when (and (plist-get operation :selected)
                    (plist-get operation :modified))
           (cl-incf count)))))))

(defun niwa-tool-patch-result (proposal changes &optional reviewed)
  "Return the aggregate handler result for PROPOSAL and CHANGES.
Rejected and feedback detail lines double as render-data notes so the
settled transcript badge stays expandable without applied diffs.
REVIEWED marks a result settled through the interactive review; its
transcript row then opens on the first changes so the user sees what
the review just applied."
  (let (details notes)
    (cl-flet ((note (line)
                (push line details)
                (push line notes))
              ;; A user revision replaces the file's whole derived hunk
              ;; set, so it is reported once for the file: hunk indices
              ;; would not refer to anything the model wrote.
              (revision (operation)
                (when-let* ((revised
                             (cl-remove-if-not
                              (lambda (hunk)
                                (and (plist-get hunk :modified)
                                     (plist-get hunk :selected)))
                              (plist-get operation :hunks))))
                  (push (format
                         (concat
                          "User edited during review (authoritative):"
                          " %s (whole file revised)\nThe user rewrote this"
                          " file's change before approving it; the applied"
                          " content below is what they chose.  Treat it as"
                          " intentional -- do not revert or"
                          " \"correct\" it.\n%s")
                         (plist-get operation :rel-path)
                         (string-join
                          (mapcar (lambda (hunk)
                                    (string-join
                                     (plist-get hunk :diff-lines) "\n"))
                                  revised)
                          "\n"))
                        details))))
      (when-let* ((feedback (plist-get proposal :feedback)))
        (note (format "Feedback (whole patch): %s" feedback)))
      (dolist (operation (plist-get proposal :operations))
        (if (eq (plist-get operation :kind) 'update)
            (progn
              (revision operation)
              ;; Locator hunks are invisible here: they carry no change
              ;; to report, and skipping them keeps hunk numbers aligned
              ;; with the review's rows.
              (cl-loop for hunk in (seq-filter
                                    #'niwa-tool-patch-hunk-changes-p
                                    (plist-get operation :hunks))
                       for number from 1
                       do
                       (when (plist-get hunk :selected)
                         (push (format "Applied: %s hunk %d"
                                       (plist-get operation :rel-path) number)
                               details)
                         (when-let* ((pass (plist-get hunk :match-pass)))
                           (note (format "Fuzzy: %s hunk %d matched while %s"
                                         (plist-get operation :rel-path) number
                                         (niwa-tool-patch-match-pass-description
                                          pass)))))
                       (unless (plist-get hunk :selected)
                         (note (format "Rejected: %s hunk %d"
                                       (plist-get operation :rel-path) number)))
                       (when-let* ((feedback (plist-get hunk :feedback)))
                         (note (format "Feedback: %s hunk %d: %s"
                                       (plist-get operation :rel-path) number
                                       feedback)))))
          (if (plist-get operation :selected)
              (progn
                (when (plist-get operation :modified)
                  (push (format
                         (concat
                          "User edited during review (authoritative):"
                          " %s\nThe user revised this new file's content"
                          " before approving it; the applied content is"
                          " what they chose.  Treat it as intentional --"
                          " do not revert or \"correct\" it.")
                         (plist-get operation :rel-path))
                        details))
                (revision operation)
                (push (format "Applied: %s%s"
                              (plist-get operation :rel-path)
                              (if-let* ((destination
                                         (plist-get operation
                                                    :move-rel-path)))
                                  (format " -> %s" destination)
                                ""))
                      details)
                (when-let* ((pass (cl-some
                                   (lambda (hunk)
                                     (plist-get hunk :match-pass))
                                   (plist-get operation :hunks))))
                  (note (format "Fuzzy: %s matched while %s"
                                (plist-get operation :rel-path)
                                (niwa-tool-patch-match-pass-description
                                 pass)))))
            (note (format "Rejected: %s" (plist-get operation :rel-path)))))
        (when-let* ((feedback (plist-get operation :feedback)))
          (note (format "Feedback: %s: %s"
                        (plist-get operation :rel-path) feedback)))))
    (let ((stats (niwa-tool-patch-proposal-stats proposal)))
      (list :result (string-join
                     (cons (if changes
                               (let ((revised (niwa-tool-patch-revised-count
                                               proposal)))
                                 (format "Applied patch: %d changes%s"
                                         (plist-get stats :selected)
                                         (if (> revised 0)
                                             (format " (%d revised by the user during review)"
                                                     revised)
                                           "")))
                             "Patch revision requested")
                           (nreverse details))
                     "\n")
            :render-data
            (list :kind 'patch
                  :applied (if changes (plist-get stats :selected) 0)
                  :total (plist-get stats :total)
                  :comments (plist-get stats :comments)
                  :reviewed (and reviewed t)
                  :notes (nreverse notes)
                  :files
                  (delq nil
                        (mapcar (lambda (operation)
                                  (when-let* ((data (niwa-tool-patch--selected-file-data
                                                     operation)))
                                    (niwa-tool-patch--remote-written-size data changes)))
                                (plist-get proposal :operations))))))))

(defun niwa-tool-patch--remote-written-size (data changes)
  "Return per-file DATA with the `:size' its remote write among CHANGES had.
The origin stores the text it is sent as UTF-8, so the size is known
here without asking it.  DATA for a local file is returned unchanged."
  (let* ((path (or (plist-get data :move-path) (plist-get data :path)))
         (change (cl-find-if (lambda (change)
                               (and (plist-get change :remote)
                                    (eq (plist-get change :action) 'write)
                                    (equal path (plist-get change :path))))
                             changes)))
    (if change
        (append data (list :size (string-bytes (encode-coding-string
                                                (plist-get change :content) 'utf-8-unix))))
      data)))

(defun niwa-tool-patch-hunk-counts (hunk)
  "Return (ADDED . DELETED) line counts for HUNK."
  (let ((added 0) (deleted 0))
    (dolist (line (plist-get hunk :diff-lines))
      (pcase (aref line 0)
        (?+ (cl-incf added))
        (?- (cl-incf deleted))))
    (cons added deleted)))

(defun niwa-tool-patch-operation-stats (operation)
  "Return selection and line-count stats for OPERATION.
The plist carries :selected and :total change counts plus :added and
:deleted line counts.  Update line counts cover selected hunks only;
whole-operation kinds always report their full size."
  (let ((selected-p (if (plist-get operation :selected) 1 0)))
    (pcase (plist-get operation :kind)
      ('add
       (list :selected selected-p :total 1
             :added (length (niwa-tool-patch-content-lines
                             (plist-get operation :content)))
             :deleted 0))
      ('delete
       (list :selected selected-p :total 1
             :added 0
             :deleted (length (niwa-tool-patch-content-lines
                               (or (plist-get operation :baseline-content)
                                   "")))))
      ('move
       (let ((added 0) (deleted 0))
         (dolist (hunk (plist-get operation :hunks))
           (pcase-let ((`(,a . ,d) (niwa-tool-patch-hunk-counts hunk)))
             (cl-incf added a)
             (cl-incf deleted d)))
         (list :selected selected-p :total 1 :added added :deleted deleted)))
      ('update
       ;; Locator hunks position other hunks; they are not changes.
       (let ((selected 0) (total 0) (added 0) (deleted 0))
         (dolist (hunk (plist-get operation :hunks))
           (when (niwa-tool-patch-hunk-changes-p hunk)
             (cl-incf total)
             (when (plist-get hunk :selected)
               (cl-incf selected)
               (pcase-let ((`(,a . ,d)
                            (niwa-tool-patch-hunk-counts hunk)))
                 (cl-incf added a)
                 (cl-incf deleted d)))))
         (list :selected selected :total total
               :added added :deleted deleted))))))

(defun niwa-tool-patch-proposal-stats (proposal)
  "Return aggregate selection stats for PROPOSAL.
Line counts include selected changes only.  The plist carries
:selected, :total, :added, :deleted, :files-selected, and :comments."
  (let ((selected 0) (total 0) (added 0) (deleted 0)
        (files-selected 0)
        (comments (if (plist-get proposal :feedback) 1 0)))
    (dolist (operation (plist-get proposal :operations))
      (let* ((stats (niwa-tool-patch-operation-stats operation))
             (op-selected (plist-get stats :selected)))
        (cl-incf selected op-selected)
        (cl-incf total (plist-get stats :total))
        (when (> op-selected 0)
          (cl-incf files-selected)
          (cl-incf added (plist-get stats :added))
          (cl-incf deleted (plist-get stats :deleted)))
        (cl-incf comments
                 (+ (if (plist-get operation :feedback) 1 0)
                    (cl-count-if (lambda (hunk) (plist-get hunk :feedback))
                                 (plist-get operation :hunks))))))
    (list :selected selected :total total :added added :deleted deleted
          :files-selected files-selected :comments comments)))

(defun niwa-tool-patch-status (operation)
  "Return the display status character for OPERATION."
  (pcase (plist-get operation :kind)
    ('add "A")
    ('delete "D")
    ('move "R")
    (_ "M")))

(defun niwa-tool-patch-kind-face (kind)
  "Return the status-letter face for operation KIND."
  (pcase kind
    ('add 'success)
    ('delete 'error)
    ('move 'font-lock-keyword-face)
    (_ 'font-lock-function-name-face)))

(defface niwa-patch-review-added
  '((t :inherit diff-added :extend t))
  "Face for added lines in an ApplyPatch review.
Foreground comes from the theme's `diff-added'.  `niwa--derive-theme-faces'
keeps that face's background when it is visible and blends one from its
colour otherwise, so themes that style diffs with foreground colours
only still get a whole-line tint."
  :group 'niwa)

(defface niwa-patch-review-removed
  '((t :inherit diff-removed :extend t))
  "Face for removed lines in an ApplyPatch review.
Derived like `niwa-patch-review-added', from `diff-removed'."
  :group 'niwa)

(pcase-dolist (`(,face ,diff ,fallback)
               '((niwa-patch-review-added diff-added success)
                 (niwa-patch-review-removed diff-removed error)))
  (niwa--derive-theme-face
   face
   (lambda (foreground background)
     (list :background
           (niwa--panel-background
            ;; `diff-mode' may not be loaded when the theme changes.
            (and (facep diff) (face-background diff nil t))
            (or (and (facep diff) (face-foreground diff nil t))
                (face-foreground fallback nil t)
                foreground)
            background 0.15)))))

(with-eval-after-load 'magit-diff
  ;; Magit's diff faces are usually themed more carefully than diff-mode's;
  ;; prefer them when magit is actually loaded.
  (when (facep 'magit-diff-added)
    (set-face-attribute 'niwa-patch-review-added nil
                        :inherit 'magit-diff-added))
  (when (facep 'magit-diff-removed)
    (set-face-attribute 'niwa-patch-review-removed nil
                        :inherit 'magit-diff-removed)))

(defun niwa-tool-patch--fontify-diff (diff)
  "Return DIFF fontified line-wise with the review's diff faces.
Each line's newline carries the face too, so the `:extend' background
tint reaches the window edge like in the live review.  Every line is
marked linkify-exempt: diff content is verbatim and must never grow
file buttons, rewritten links, or inline images."
  (string-remove-suffix
   "\n"
   (mapconcat
    (lambda (line)
      (let ((face (pcase (and (> (length line) 0) (aref line 0))
                    (?+ 'niwa-patch-review-added)
                    (?- 'niwa-patch-review-removed)
                    (?@ 'diff-hunk-header))))
        (apply #'propertize (concat line "\n")
               'niwa-view-no-linkify t
               (and face (list 'font-lock-face face)))))
    (split-string diff "\n") "")))

(defun niwa-tool-patch--effective-mode ()
  "Return the active permission mode for patch review."
  (let ((mode (niwa-permission-mode-effective
               (and (boundp 'niwa--session) niwa--session)
               (current-buffer))))
    (cond
     ((eq mode 'full-auto) mode)
     ((and (boundp 'niwa--current-request)
           niwa--current-request
           (niwa-request-one-shot-mutations-p niwa--current-request))
      'ask)
     ((and (boundp 'niwa-pipeline--auto-apply-edit-p)
           niwa-pipeline--auto-apply-edit-p)
      'edits)
     (t mode))))

(defun niwa-tool-patch-handler (callback args)
  "Handle an ApplyPatch call with CALLBACK and ARGS.
A patch of a remote artifact's files first reads them from the origin.
Applied without review, a write refused because a file changed meanwhile
is retried once on the current content, where a hunk that no longer
matches fails as it does for a local file."
  (let ((data-buffer (current-buffer))
        (mode (niwa-tool-patch--effective-mode))
        proposal)
    (cl-labels
        ((fail (err)
           (funcall callback
                    (list :result
                          (format "Error: %s"
                                  (niwa-tool-patch-sanitize-error
                                   (niwa-resource-error-message err) proposal))
                          :status 'error)))
         (fetch (then)
           (niwa-tool-patch--fetch-remote
            proposal (plist-get (plist-get proposal :remote-artifact) :workspace)
            (lambda (failure)
              (if failure
                  (fail failure)
                (with-current-buffer (if (buffer-live-p data-buffer) data-buffer
                                       (current-buffer))
                  (funcall then))))))
         (proceed (retries)
           (condition-case err
               (progn
                 (niwa-tool-patch-capture-baseline proposal)
                 (let ((changes (niwa-tool-patch-planned-changes proposal)))
                   (niwa-tool-patch-assert-baseline proposal)
                   (cond
                    ((and niwa--session
                          (niwa-session-note-conversation niwa--session)
                          (not (cl-some
                                (lambda (operation)
                                  (or (niwa-tool-patch-resource-address-p
                                       (plist-get operation :rel-path))
                                      (and (plist-get operation :move-rel-path)
                                           (niwa-tool-patch-resource-address-p
                                            (plist-get operation :move-rel-path)))))
                                (plist-get proposal :operations))))
                     (funcall callback (niwa-note-prepare-change proposal changes data-buffer)))
                    ((memq mode '(edits full-auto))
                       (let ((result (niwa-tool-patch-result proposal changes)))
                         (niwa-tool-patch-apply
                          data-buffer changes (lambda () (funcall callback result))
                          (lambda (err)
                            (if (and (> retries 0)
                                     (eq (car err) 'niwa-tool-patch-remote-stale))
                                (fetch (lambda () (proceed (1- retries))))
                              (fail err))))))
                    (t
                     (require 'niwa-patch-review)
                     (niwa-patch-review-start proposal callback data-buffer)))))
             (error (fail err)))))
      (condition-case err
          (let ((prepared niwa-tool-patch-prepared-proposal))
            (setq proposal (or prepared
                               (niwa-tool-patch-parse (plist-get args :patch))))
            (unless prepared
              (niwa-tool-patch-prepare-resources proposal))
            (niwa-tool-patch--materialize-resources proposal)
            (if (niwa-tool-patch-remote-p
                 (plist-get (car (plist-get proposal :operations)) :rel-path))
                (progn
                  (plist-put proposal :remote-artifact
                             (list :workspace (niwa-tool-patch--workspace)
                                   :actor (niwa-tool-editing-actor)
                                   :authorize niwa-pipeline--handler-active-p))
                  (fetch (lambda () (proceed 1))))
              (proceed 0)))
        (error (fail err))))))

(defun niwa-tool-patch--workspace ()
  "Return the current session's workspace, which remote files belong to."
  (or (and (boundp 'niwa--session) niwa--session
           (niwa-session-workspace niwa--session))
      (error "Remote artifact files require a session")))

(defun niwa-tool-patch--get-paths (args)
  "Return every path affected by the patch in ARGS."
  (niwa-tool-patch-get-paths-from-proposal
   (niwa-tool-patch-parse (plist-get args :patch))))

(defun niwa-tool-patch-get-paths-from-proposal (proposal)
  "Return the filesystem paths whose write policy governs prepared PROPOSAL.

Ordinary operands contribute their paths.  Authored resource operands are
already authorized by their resource attempt, so they contribute only the
backing path preparation recorded for a writable resource, if any."
  (delete-dups
   (cl-mapcan
    (lambda (operation)
      (delq nil
            (cl-mapcar
             (lambda (address path permission)
               (if (niwa-tool-patch-resource-address-p address)
                   permission
                 path))
             (list (plist-get operation :rel-path)
                   (plist-get operation :move-rel-path))
             (list (niwa-tool-patch-physical-path operation)
                   (niwa-tool-patch--physical-move-path operation))
             (list (plist-get operation :permission-path)
                   (plist-get operation :permission-move-path)))))
    (plist-get proposal :operations))))

(defun niwa-tool-patch--file-heading (file)
  "Return the one-line heading for per-file render data FILE."
  (let ((path (if-let* ((destination (plist-get file :move-path)))
                  (format "%s → %s" (plist-get file :path) destination)
                (plist-get file :path))))
    (concat
     (propertize (niwa-tool-patch-status file)
                 'font-lock-face
                 (niwa-tool-patch-kind-face (plist-get file :kind)))
     " " path " · "
     (propertize (format "+%d" (plist-get file :added))
                 'font-lock-face 'niwa-view-tool-diff-added)
     " "
     (propertize (format "−%d" (plist-get file :deleted))
                 'font-lock-face 'niwa-view-tool-diff-removed))))

(defun niwa-tool-patch--diff-hunks (diff)
  "Split DIFF into per-change chunks on `@@' hunk headers.
A diff without hunk headers -- a whole added or deleted file -- is one
chunk."
  (if (string-match-p "^@@" diff)
      (let (chunks current)
        (dolist (line (split-string diff "\n"))
          (when (and current (string-prefix-p "@@" line))
            (push (string-join (nreverse current) "\n") chunks)
            (setq current nil))
          (push line current))
        (when current
          (push (string-join (nreverse current) "\n") chunks))
        (nreverse chunks))
    (list diff)))

(defconst niwa-tool-patch--preview-change-limit 2
  "How many leading changes a reviewed patch row shows expanded.")

(defconst niwa-tool-patch--preview-line-limit 20
  "Maximum diff lines one previewed change may occupy.")

(defun niwa-tool-patch--preview-body (render-data)
  "Return the leading-changes preview body for RENDER-DATA.
Shows the first `niwa-tool-patch--preview-change-limit' changes with
their file headings and a trailing count of everything omitted."
  (let ((budget niwa-tool-patch--preview-change-limit)
        (shown 0)
        (total 0)
        blocks)
    (dolist (file (plist-get render-data :files))
      (let* ((diff (plist-get file :diff))
             (hunks (unless (string-empty-p diff)
                      (niwa-tool-patch--diff-hunks diff))))
        (cl-incf total (length hunks))
        (when (and hunks (> budget 0))
          (let ((take (min budget (length hunks))))
            (push (string-join
                   (cons
                    (niwa-tool-patch--file-heading file)
                    (mapcar
                     (lambda (hunk)
                       (let ((lines (split-string hunk "\n")))
                         (niwa-tool-patch--fontify-diff
                          (if (<= (length lines)
                                  niwa-tool-patch--preview-line-limit)
                              hunk
                            (concat
                             (string-join
                              (seq-take
                               lines niwa-tool-patch--preview-line-limit)
                              "\n")
                             "\n…")))))
                     (seq-take hunks take)))
                   "\n")
                  blocks)
            (cl-incf shown take)
            (cl-decf budget take)))))
    (string-join
     (delq nil
           (list (string-join (nreverse blocks) "\n\n")
                 (when (> total shown)
                   (propertize
                    (format "… %d more change%s"
                            (- total shown)
                            (if (= (- total shown) 1) "" "s"))
                    'font-lock-face 'shadow))))
     "\n")))

(defun niwa-tool-patch--render
    (name _args _result render-data)
  "Render an ApplyPatch result named NAME from RENDER-DATA.
A reviewed, applied patch renders initially expanded on a preview of
its first changes; collapsing and re-expanding shows the complete
diff."
  (when (eq (plist-get render-data :kind) 'patch)
    (let* ((files (plist-get render-data :files))
           (added (cl-loop for file in files sum (plist-get file :added)))
           (deleted (cl-loop for file in files sum (plist-get file :deleted)))
           (file-count (length files))
           (applied (or (plist-get render-data :applied) 0))
           (total (or (plist-get render-data :total) applied))
           (comments (or (plist-get render-data :comments) 0)))
      ;; "Name: arg (+N -M)" is the view's tool summary grammar; the
      ;; trailing count group picks up the green/red summary faces.
      (append
       (list :header
             (concat
              (or name "ApplyPatch") ": "
              (if (zerop applied)
                  "revision requested"
                (format "%d %s · %s"
                        file-count (if (= file-count 1) "file" "files")
                        (if (= applied total)
                            (format "%d change%s" applied
                                    (if (= applied 1) "" "s"))
                          (format "%d/%d changes" applied total))))
              (when (> comments 0)
                (format " · %d comment%s sent"
                        comments (if (= comments 1) "" "s")))
              (unless (zerop applied)
                (format " (+%d -%d)" added deleted)))
             :body
             (string-join
              (delq nil
                    (list
                     (and files
                          (mapconcat
                           (lambda (file)
                             (string-join
                              (delq nil
                                    (list
                                     (niwa-tool-patch--file-heading file)
                                     (unless (string-empty-p
                                              (plist-get file :diff))
                                       (niwa-tool-patch--fontify-diff
                                        (plist-get file :diff)))))
                              "\n"))
                           files "\n\n"))
                     (when-let* ((notes (plist-get render-data :notes)))
                       (propertize (string-join notes "\n")
                                   'font-lock-face 'shadow))))
              "\n\n")
             :expandable-p t)
       (if (and (plist-get render-data :reviewed) (> applied 0))
           (list :initially-collapsed-p nil
                 :preview-body (niwa-tool-patch--preview-body render-data))
         (list :initially-collapsed-p t))))))

(provide 'niwa-tool-patch)
;;; niwa-tool-patch.el ends here
