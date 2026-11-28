;;; niwa-plugin-registry.el -- Plugin discovery and trust -*- lexical-binding: t -*-

;;; Commentary:

;; Discovers Codex-style manifests, validates plugin-relative paths, and owns
;; workspace-local enablement plus executable-hook consent state.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'niwa-structs)

(declare-function niwa-session-control-fs-run-program
                  "niwa-session-control-fs" (operations &optional lock-directory))
(declare-function niwa-session-control-fs-program-value
                  "niwa-session-control-fs" (result))
(declare-function niwa-session-control-fs-make-directory
                  "niwa-session-control-fs" (path &optional parents))
(declare-function niwa-session-control-fs-physical-path
                  "niwa-session-control-fs" (path))
(declare-function niwa-session-control-fs-call-with-locks
                  "niwa-session-control-fs" (paths function))
(autoload 'niwa-session-control-fs-call-with-locks "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-run-program "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-program-value "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-make-directory "niwa-session-control-fs")
(autoload 'niwa-session-control-fs-physical-path "niwa-session-control-fs")

;; `niwa-cockpit'
(declare-function niwa-cockpit-context-session
                  "niwa-cockpit" (&optional context))
(declare-function niwa-cockpit-current-context
                  "niwa-cockpit" ())
(autoload 'niwa-cockpit-context-session "niwa-cockpit")
(autoload 'niwa-cockpit-current-context "niwa-cockpit")

;; `niwa-hooks'
(declare-function niwa-hooks-invalidate-config "niwa-hooks" ())
(declare-function niwa-hooks-rules-script-hashes "niwa-hooks"
                  (rules root &optional plugin))
(declare-function niwa-hooks-read-config-file
                  "niwa-hooks" (file &optional content))

;; `niwa-session-artifacts'
(declare-function niwa-session-artifacts-call-fenced
                  "niwa-session-artifacts" (session function))
(autoload 'niwa-session-artifacts-call-fenced "niwa-session-artifacts")
(declare-function niwa-session-artifacts-fenced-results
                  "niwa-session-artifacts" (proof results))
(autoload 'niwa-session-artifacts-fenced-results "niwa-session-artifacts")

;; `niwa-utilities'
(declare-function niwa--write-file-atomically
                  "niwa-utilities" (path content &optional coding mode))
(autoload 'niwa--write-file-atomically "niwa-utilities")

;; `niwa-workspace'
(declare-function niwa-workspace-state-dir
                  "niwa-workspace" (workspace))
(autoload 'niwa-workspace-state-dir "niwa-workspace")

;; `subr'
(defvar read-eval)

(defcustom niwa-plugin-extra-roots nil
  "Additional local Codex plugin roots to scan.
Each entry may be a direct plugin root or a directory containing plugin
roots below it."
  :type '(repeat directory)
  :group 'niwa)

(defconst niwa-plugins--default-install-directory
  (file-name-concat "~" ".agents" "plugins")
  "Default value for `niwa-plugin-install-directory'.")

(defcustom niwa-plugin-install-directory
  niwa-plugins--default-install-directory
  "Directory where `/plugin install' writes new plugins."
  :type 'directory
  :group 'niwa)

(defcustom niwa-plugins-update-check-interval nil
  "Days between background checks for upstream plugin updates, or nil.
Nil (the default) never checks.  A number enables an asynchronous
`git ls-remote' against each managed plugin's origin once that many
days have passed since the last check, started on an idle timer after
the registry is first used.  A check only notifies; it never updates a
plugin.  The cockpit's \"Check for updates\" action ignores this setting."
  :type '(choice (const :tag "Never check" nil)
                 (number :tag "Days between checks"))
  :group 'niwa)


;;
;;; Structs

(cl-defstruct (niwa-plugin (:constructor niwa-plugin--create))
  "Installed Codex-style plugin manifest."
  name
  version
  description
  root
  skills-dir
  hooks-file
  hooks
  shadowed
  enabled-p
  hooks-enabled-p)

(cl-defstruct (niwa-plugin-error (:constructor niwa-plugin-error--create))
  "Plugin root whose manifest metadata could not be used."
  name
  root
  manifest
  message)

;;
;;; Paths

(defun niwa-plugins--normalize-directory (directory)
  "Return DIRECTORY as an expanded directory name."
  (file-name-as-directory
   (expand-file-name (substitute-in-file-name directory))))

(defun niwa-plugins-dir ()
  "Return the global directory where new plugins are installed."
  (niwa-plugins--normalize-directory niwa-plugin-install-directory))

(defun niwa-plugins--global-niwa-dir ()
  "Return the global niwa-specific plugin directory."
  (niwa-plugins--normalize-directory
   (file-name-concat niwa-user-dir "plugins")))

(defun niwa-plugins-state-file (&optional workspace)
  "Return the persistent plugin state file for WORKSPACE.
Return nil when WORKSPACE is nil."
  (when workspace
    (file-name-concat (niwa-workspace-state-dir workspace) "state" "plugins.eld")))

(defun niwa-plugins--project-state-file (workspace)
  "Return WORKSPACE's top-level plugin state file.
A project may track it; older clients write it as their only state file."
  (file-name-concat (niwa-workspace-state-dir workspace) "plugins.eld"))

(defun niwa-plugins--workspace-plugins-dir (workspace resource-dir)
  "Return WORKSPACE plugin directory under RESOURCE-DIR."
  (file-name-concat (niwa-workspace-root workspace)
                    resource-dir
                    "plugins"))

(defun niwa-plugins-plugin-data-dir (plugin-name &optional workspace)
  "Return persistent data directory for PLUGIN-NAME in WORKSPACE."
  (unless workspace
    (error "No workspace for plugin data"))
  (file-name-concat (niwa-workspace-state-dir workspace)
                    "state" "plugin-data"
                    plugin-name))


;;
;;; Manifest discovery

(defun niwa-plugins-manifest-file (root)
  "Return plugin manifest path under ROOT."
  (file-name-concat root ".codex-plugin" "plugin.json"))

(defun niwa-plugins--string-or-nil (value)
  "Return VALUE if it is a non-empty string, else nil."
  (and (stringp value)
       (not (string-empty-p value))
       value))

(defun niwa-plugins--json-string (key alist)
  "Return string field named by symbol KEY from parsed JSON ALIST."
  (niwa-plugins--string-or-nil
   (alist-get key alist)))

(defun niwa-plugins--safe-name-p (name)
  "Return non-nil when NAME is safe for plugin identifiers."
  (and (stringp name)
       (string-match-p "\\`[A-Za-z0-9_.-]+\\'" name)
       (not (member name '("." ".." "user" "project" "managed" "bundled")))))

(defun niwa-plugins--path-in-root-p (path root)
  "Return non-nil when PATH stays inside ROOT."
  (let ((path (file-truename path))
        (root (file-name-as-directory (file-truename root))))
    (string-prefix-p root (file-name-as-directory path))))

(defun niwa-plugins--resolve-manifest-path (root path)
  "Resolve manifest PATH relative to plugin ROOT."
  (when-let* ((path (niwa-plugins--string-or-nil path))
              ((not (file-name-absolute-p path)))
              (expanded (expand-file-name path root))
              ((not (file-symlink-p expanded)))
              ((niwa-plugins--path-in-root-p expanded root)))
    expanded))

(defun niwa-plugins--normalize-manifest-hooks (root value missing)
  "Normalize manifest hook VALUE for plugin ROOT.
Return a list of plists, each with a `:file' path."
  (cond
   ((eq value missing)
    (when-let* ((default (niwa-plugins--resolve-manifest-path
                          root "hooks/hooks.json"))
                ((file-readable-p default)))
      (list (list :file default))))
   ((stringp value)
    (when-let* ((file (niwa-plugins--resolve-manifest-path root value)))
      (list (list :file file))))))

(defun niwa-plugins--first-hook-file (hooks)
  "Return the first file path in normalized HOOKS."
  (cl-some (lambda (entry) (plist-get entry :file)) hooks))

(defun niwa-plugins--read-manifest-result (root &optional workspace)
  "Read plugin manifest under ROOT.
Return a `niwa-plugin' or `niwa-plugin-error', or nil when ROOT
does not contain a readable Codex plugin manifest."
  (let ((file (niwa-plugins-manifest-file root)))
    (when (file-readable-p file)
      (let* ((root (file-name-as-directory (expand-file-name root)))
             (fallback-name
              (file-name-nondirectory (directory-file-name root))))
        (condition-case err
            (let* ((json (with-temp-buffer
                           (insert-file-contents file)
                           (json-parse-buffer
                            :object-type 'alist
                            :array-type 'list
                            :null-object nil
                            :false-object nil)))
                   (name (or (niwa-plugins--json-string 'name json)
                             fallback-name))
                   (missing (make-symbol "missing"))
                   (skills (niwa-plugins--json-string 'skills json))
                   (hooks (niwa-plugins--normalize-manifest-hooks
                           root
                           (alist-get 'hooks json missing)
                           missing)))
              (if (niwa-plugins--safe-name-p name)
                  (niwa-plugin--create
                   :name name
                   :version (niwa-plugins--json-string 'version json)
                   :description (niwa-plugins--json-string
                                 'description json)
                   :root root
                   :skills-dir (niwa-plugins--resolve-manifest-path
                                root skills)
                   :hooks-file (niwa-plugins--first-hook-file hooks)
                   :hooks hooks
                   :enabled-p (niwa-plugins--state-enabled-p
                               name workspace root)
                   :hooks-enabled-p (niwa-plugins--state-hooks-enabled-p
                                     name workspace root hooks))
                (niwa-plugin-error--create
                 :name name
                 :root root
                 :manifest file
                 :message (format "Unsafe plugin name: %s" name))))
          (error
           (niwa-plugin-error--create
            :name fallback-name
            :root root
            :manifest file
            :message (error-message-string err))))))))

(defun niwa-plugins-read-manifest (root &optional workspace)
  "Read plugin manifest under ROOT.
Return nil when ROOT does not contain a usable Codex plugin manifest."
  (let ((entry (niwa-plugins--read-manifest-result root workspace)))
    (and (niwa-plugin-p entry) entry)))

(defun niwa-plugins--canonical-root (root)
  "Return deterministic directory form for plugin ROOT."
  (file-name-as-directory (expand-file-name root)))

(defconst niwa-plugins-staging-prefix ".niwa-staging-"
  "Filename prefix marking an in-flight plugin clone.
A clone is staged beside its destination so publishing it is a
same-filesystem rename, which puts it inside the tree discovery walks.
Discovery skips these names so a half-built clone is never adopted.")

(defun niwa-plugins-staging-name-p (path)
  "Return non-nil when PATH names an in-flight plugin clone."
  (string-prefix-p niwa-plugins-staging-prefix
                   (file-name-nondirectory (directory-file-name path))))

(defun niwa-plugins--collect-roots-under (root)
  "Return plugin roots at or below ROOT.
Do not descend into a directory once it is recognized as a plugin root."
  (let (roots)
    (cl-labels
        ((walk (dir)
           (when (file-directory-p dir)
             (let ((dir (niwa-plugins--canonical-root dir)))
               (cond
                ((file-readable-p (niwa-plugins-manifest-file dir))
                 (push dir roots))
                (t
                 (dolist (entry (directory-files
                                  dir t directory-files-no-dot-files-regexp))
                   (when (and (file-directory-p entry)
                              (not (file-symlink-p entry))
                              (not (niwa-plugins-staging-name-p entry)))
                     (walk entry)))))))))
      (walk root))
    (nreverse roots)))

(defun niwa-plugins--root-sources (&optional workspace)
  "Return plugin root containers in precedence order for WORKSPACE."
  (append
   (when workspace
     (list (niwa-plugins--workspace-plugins-dir workspace ".niwa")
           (niwa-plugins--workspace-plugins-dir workspace ".agents")))
   (list (niwa-plugins--global-niwa-dir)
         (niwa-plugins-dir))
   niwa-plugin-extra-roots))

(defun niwa-plugins--plugin-roots (&optional workspace)
  "Return deduplicated plugin roots in precedence order for WORKSPACE."
  (let ((seen (make-hash-table :test #'equal))
        roots)
    (dolist (root (niwa-plugins--root-sources workspace))
      (dolist (plugin-root (niwa-plugins--collect-roots-under root))
        (let ((key (file-name-as-directory (file-truename plugin-root))))
          (unless (gethash key seen)
            (puthash key t seen)
            (push plugin-root roots)))))
    (nreverse roots)))

(defun niwa-plugins--select-duplicate-names (plugins)
  "Select highest-precedence entries from PLUGINS.
Shadowed entries are retained in the winner's `shadowed' slot."
  (let ((by-name (make-hash-table :test #'equal))
        kept)
    (dolist (plugin plugins)
      (push plugin (gethash (niwa-plugin-name plugin) by-name)))
    (maphash
     (lambda (_name matches)
       (let* ((matches (nreverse matches))
              (winner (car matches)))
         (setf (niwa-plugin-shadowed winner) (cdr matches))
         (push winner kept)))
     by-name)
    (sort kept
          (lambda (a b)
            (string< (niwa-plugin-name a)
                     (niwa-plugin-name b))))))

(defun niwa-plugins--collect (&optional workspace)
  "Return discovered plugin metadata for WORKSPACE."
  (niwa-plugins--schedule-update-check)
  (let (plugins errors)
    (dolist (root (niwa-plugins--plugin-roots workspace))
      (let ((item (niwa-plugins--read-manifest-result root workspace)))
        (cond
         ((niwa-plugin-p item) (push item plugins))
         ((niwa-plugin-error-p item) (push item errors)))))
    (let* ((winners (niwa-plugins--select-duplicate-names
                     (nreverse plugins)))
           (errors (nreverse errors))
           (enabled-count
            (cl-count-if
             (lambda (plugin)
               (niwa-plugins-enabled-p plugin workspace))
             winners)))
      (list :winners winners
            :errors errors
            :enabled-count enabled-count
            :total-count (length winners)
            :error-count (length errors)))))

(defun niwa-plugins-list (&optional workspace)
  "Return installed Codex-style plugins.
State slots are resolved for WORKSPACE when provided."
  (plist-get (niwa-plugins--collect workspace) :winners))

(defun niwa-plugins-item-name (item)
  "Return display name for plugin cockpit ITEM."
  (cond
   ((niwa-plugin-p item) (niwa-plugin-name item))
   ((niwa-plugin-error-p item)
    (or (niwa-plugin-error-name item) "metadata error"))))

(defun niwa-plugins-items (&optional workspace)
  "Return plugin cockpit items for WORKSPACE.
Items include usable plugin manifests and visible metadata errors."
  (let ((collection (niwa-plugins--collect workspace)))
    (sort (append (plist-get collection :winners)
                  (plist-get collection :errors))
          (lambda (a b)
            (string< (niwa-plugins-item-name a)
                     (niwa-plugins-item-name b))))))

(defun niwa-plugins-find (name &optional workspace)
  "Return installed plugin named NAME, or nil."
  (cl-find name (niwa-plugins-list workspace)
           :key #'niwa-plugin-name :test #'equal))


;;
;;; Persistent state

(defconst niwa-plugins--consent-keys
  '(:hooks-enabled :hooks-fingerprint :hooks-scripts)
  "Plugin state keys that enable executable hooks or approve their scripts.
No workspace file supplies them: a copy or restore of a workspace must
not carry consent.  They live in the user's consent store, see
`niwa-plugins--user-consent'.")

(defun niwa-plugins--consent-file ()
  "Return the user's plugin hook consent store, beside project trust."
  (file-name-concat niwa-user-dir "plugin-consent.eld"))

(defun niwa-plugins--workspace-key (workspace)
  "Return WORKSPACE's physical identity in the consent store.
The root's symlinks are resolved, on the target for a remote root, so a
retargeted link names a different checkout.  A remote identity keeps its
TRAMP prefix, so consent is per client machine and per alias."
  (let ((root (directory-file-name (expand-file-name (niwa-workspace-root workspace)))))
    (concat (or (file-remote-p root) "") (file-local-name (file-truename root)))))

(defun niwa-plugins--consent-source (source workspace)
  "Return SOURCE's physical identity in WORKSPACE, links resolved.
A source relative to the workspace resolves against its root, so a link
inside the workspace retargeted to another plugin names a different
source; activation keeps the portable relative path."
  (if (stringp source)
      (file-truename (expand-file-name source (niwa-workspace-root workspace)))
    source))

(defvar niwa-plugins--warned-consent nil
  "The unreadable consent store already reported, if any.")

(defvar niwa-plugins--reviewed-binding nil
  "The (NAME ROOT KEY SOURCE) a consent write was reviewed for.
NAME is the plugin, ROOT its reviewed root spelling, KEY and SOURCE the
physical identities `niwa-plugins--consent-binding' captured.
Bound by the enable commands around their write, so the consent store
records the physical identities the user approved, not the ones links
resolve to when the write lands.  An entry naming another source is
keyed as usual.")

(defun niwa-plugins--consent-binding (plugin workspace)
  "Return PLUGIN's physical consent identity in WORKSPACE as (KEY . SOURCE)."
  (cons (niwa-plugins--workspace-key workspace)
        (niwa-plugins--consent-source (niwa-plugin-root plugin) workspace)))

(defun niwa-plugins--reviewed (plugin consent)
  "Return the `niwa-plugins--reviewed-binding' for PLUGIN's CONSENT snapshot."
  (cons (niwa-plugin-name plugin)
        (cons (niwa-plugin-root plugin)
              (let ((binding (plist-get consent :binding)))
                (list (car binding) (cdr binding))))))

(defun niwa-plugins--binding-unchanged-p (plugin consent workspace)
  "Return non-nil unless PLUGIN's identity in WORKSPACE moved since CONSENT.
Otherwise report that the reviewed hook approval was not applied."
  (or (not (plist-get consent :fingerprint))
      (equal (plist-get consent :binding)
             (niwa-plugins--consent-binding plugin workspace))
      (progn
        (message "Plugin %s changed location during review; hook approval was not applied"
                 (niwa-plugin-name plugin))
        nil)))

(defun niwa-plugins--read-consent ()
  "Return the records in the user's consent store.
A missing store is created empty first, so the protected-path mount
covers it before any confined child starts."
  (let ((file (niwa-plugins--consent-file)))
    (unless (file-exists-p file)
      (ignore-errors
        (make-directory niwa-user-dir t)
        (with-file-modes #o600
          (write-region "nil\n" nil file nil 'silent nil 'excl))))
    ;; Unreadable consent authorizes nothing; the bytes stay for repair.
    (and (file-readable-p file)
         (condition-case err
             (niwa-plugins--parse-strict
              (with-temp-buffer (insert-file-contents file) (buffer-string)) file)
           (user-error
            (unless (equal niwa-plugins--warned-consent file)
              (setq niwa-plugins--warned-consent file)
              (display-warning 'niwa (concat (error-message-string err)
                                             "; plugin hooks stay off until then")))
            nil)))))

(defun niwa-plugins--user-consent (state workspace)
  "Return STATE with WORKSPACE's hook consent taken only from the user's store.
Consent in any workspace file, including one written by older versions,
is ignored.  A record applies to the workspace, plugin and source it was
given for; the registry also compares its fingerprint and scripts."
  (let ((records (niwa-plugins--read-consent))
        (key (niwa-plugins--workspace-key workspace)))
    (mapcar (lambda (entry)
              (let* ((plist (niwa-plugins--keys (cdr entry)))
                     (record (cl-find-if
                              (lambda (record)
                                (and (equal key (plist-get record :workspace))
                                     (equal (car entry) (plist-get record :plugin))
                                     (equal (niwa-plugins--consent-source
                                             (plist-get plist :source-root) workspace)
                                            (plist-get record :source))))
                              records)))
                (cons (car entry)
                      (append plist (niwa-plugins--keys record t)))))
            state)))

(defun niwa-plugins--store-consent (workspace old new)
  "Record in the user's store the WORKSPACE hook consent NEW changed from OLD.
The read, merge and replacement run under the store directory's lock,
as project trust writes do, and touch only the plugins this write
changed, so another client's consent or revocation survives."
  (let* ((current (niwa-plugins--workspace-key workspace))
         (reviewed niwa-plugins--reviewed-binding)
         (consent (lambda (state name &optional after)
                    (let ((plist (cdr (assoc name state))))
                      ;; The reviewed plugin's approval binds what was reviewed.
                      (if (and after (equal name (car reviewed))
                               (niwa-plugins-same-root-p
                                (nth 1 reviewed) (plist-get plist :source-root)
                                workspace))
                          (list (nth 2 reviewed) (nth 3 reviewed)
                                (niwa-plugins--keys plist t))
                        (list current
                              (niwa-plugins--consent-source
                               (plist-get plist :source-root) workspace)
                              (niwa-plugins--keys plist t))))))
         (changes (cl-loop for name in (delete-dups (mapcar #'car (append old new)))
                           for after = (funcall consent new name t)
                           unless (equal after (funcall consent old name))
                           collect (cons name after))))
    (when changes
      (make-directory niwa-user-dir t)
      (niwa-session-control-fs-call-with-locks
       (list (directory-file-name (expand-file-name niwa-user-dir)))
       (lambda (_lock)
         (let ((records (cl-remove-if
                         (lambda (record)
                           (let ((change (assoc (plist-get record :plugin) changes)))
                             (and change
                                  (equal (nth 1 change) (plist-get record :workspace)))))
                         (let ((file (niwa-plugins--consent-file)))
                           (and (file-exists-p file)
                                (niwa-plugins--parse-strict
                                 (with-temp-buffer (insert-file-contents file)
                                                   (buffer-string))
                                 file)))))
               (print-length nil) (print-level nil))
           (pcase-dolist (`(,name ,key ,source ,plist) changes)
             ;; A revocation is kept too, so it reads back as written.
             (when plist
               (push (append (list :workspace key :plugin name :source source) plist)
                     records)))
           (niwa--write-file-atomically (niwa-plugins--consent-file)
                                        (prin1-to-string records))))))))

(defun niwa-plugins--parse-strict (text file)
  "Return the plugin state alist TEXT holds, read from FILE.
Blank TEXT is a valid empty state.  Anything else that is not one list of
entries signals naming FILE, so an update never replaces state it could
not read."
  (if (string-blank-p text)
      nil
    (car (or (condition-case nil
                 (let* ((read-eval nil)
                        (form (read-from-string text))
                        (state (car form)))
                   (and (listp state)
                        (string-blank-p (replace-regexp-in-string
                                         "^[ \t]*;.*$" "" (substring text (cdr form))))
                        (cl-every #'consp state)
                        (list state)))
               (error nil))
             (user-error "Plugin state %s cannot be read; repair or remove it"
                         file)))))

(defun niwa-plugins--parse-state (text)
  "Return the plugin state alist TEXT holds, or nil when it is unreadable."
  (condition-case nil
      (let* ((read-eval nil)
             (state (car (read-from-string text))))
        (and (listp state) state))
    (error nil)))

(defun niwa-plugins--read-file (file)
  "Return the plugin state alist in FILE, or nil."
  (when (file-readable-p file)
    (niwa-plugins--parse-state
     (with-temp-buffer (insert-file-contents file) (buffer-string)))))

(defun niwa-plugins--keys (plist &optional consent)
  "Return PLIST's hook consent keys when CONSENT, else its other keys."
  (cl-loop for (key value) on plist by #'cddr
           when (eq (and consent t) (and (memq key niwa-plugins--consent-keys) t))
           append (list key value)))

(defvar niwa-plugins--tracked-cache (make-hash-table :test #'equal)
  "Recent Git answers for project plugin files, by path.")

(defun niwa-plugins--tracking (workspace file &optional fresh)
  "Return `tracked', `untracked' or `unknown' for WORKSPACE's plugin FILE.
`unknown' means Git could not answer; a file is `untracked' outside any
repository only when no `.git' entry exists above it, since a broken
worktree pointer also fails as \"not a git repository\".  FRESH skips the
cache, as a destructive adoption must."
  (let ((mtime (file-attribute-modification-time (file-attributes file)))
        (cached (gethash file niwa-plugins--tracked-cache)))
    ;; shortcut: a 2 s answer keyed by FILE's mtime spares repeated reads a Git
    ;; call; an index change shows within it, and always before an adoption.
    (if (and (not fresh) cached (equal mtime (car cached))
             (< (float-time (time-since (nth 1 cached))) 2))
        (nth 2 cached)
      (let ((answer
             (with-temp-buffer
               (let* ((default-directory
                       (file-name-as-directory (niwa-workspace-root workspace)))
                      (status (condition-case nil
                                  (process-file "git" nil t nil "ls-files" "-z" "--"
                                                (file-local-name file))
                                (error nil))))
                 (cond
                  ((eq status 0) (if (> (buffer-size) 0) 'tracked 'untracked))
                  ((and (integerp status)
                        (not (locate-dominating-file default-directory ".git")))
                   'untracked)
                  (t 'unknown))))))
        (if (eq answer 'unknown)
            (remhash file niwa-plugins--tracked-cache)
          (puthash file (list mtime (current-time) answer) niwa-plugins--tracked-cache))
        answer))))

(defun niwa-plugins--merge-legacy (base legacy local)
  "Return LOCAL with LEGACY's activation changes since BASE, never consent."
  (let ((result (copy-tree local)))
    (dolist (name (delete-dups (mapcar #'car (append base legacy))) result)
      (let ((old (assoc name legacy)))
        (unless (equal (niwa-plugins--keys (cdr old))
                       (niwa-plugins--keys (cdr (assoc name base))))
          (let ((consent (niwa-plugins--keys (cdr (assoc name result)) t)))
            (setq result (assoc-delete-all name result))
            (when old
              (setq result (append result
                                   (list (cons name (append (niwa-plugins--keys (cdr old))
                                                            consent))))))))))))

(defun niwa-plugins--adopt-legacy (workspace &optional proof)
  "Move WORKSPACE's untracked top-level plugin state into `state/'.
Nothing proves who wrote it, so its activation changes since the last
adopted file apply, but never its hook consent: the user consents once
more.  A concurrent change leaves the file for the next read.  A remote
session's lease PROOF leads the locked program, so only the lease owner
migrates and deletes shared files."
  (let* ((legacy (niwa-plugins--project-state-file workspace))
         (local (niwa-plugins-state-file workspace))
         (base (file-name-concat (file-name-directory local) "plugins-legacy.eld"))
         (reads (niwa-session-control-fs-run-program
                 (mapcar (lambda (path)
                           (list :op 'read :path path :coding 'no-conversion :optional t))
                         (list legacy local base))))
         (bytes (mapcar (lambda (result)
                          (and (eq (plist-get result :status) 'ok)
                               (niwa-session-control-fs-program-value result)))
                        reads))
         ;; An unreadable input refuses the adoption and stays as it is.
         (state (cl-mapcar (lambda (data file)
                             (and data (niwa-plugins--parse-strict
                                        (decode-coding-string data 'utf-8-unix) file)))
                           bytes (list legacy local base))))
    ;; Recheck the index right before anything destructive.
    (when (and (car state) (eq 'untracked (niwa-plugins--tracking workspace legacy t)))
      (let ((results
             (niwa-session-artifacts-fenced-results
              proof
              (niwa-session-control-fs-run-program
               (append
                proof
                (list (list :op 'make-directory
                          :path (directory-file-name (file-name-directory local))
                          :optional t)
                    (if (nth 1 bytes)
                        (list :op 'verify-hash :path local
                              :content (secure-hash 'sha256 (nth 1 bytes)))
                      (list :op 'absent :path local))
                    (list :op 'verify-hash :path legacy
                          :content (secure-hash 'sha256 (car bytes)))
                    ;; State, then base, then removal: after a crash at any
                    ;; point the next read merges the same changes again.
                    (list :op 'write :path local
                          :content (niwa-plugins--state-text
                                    (niwa-plugins--merge-legacy
                                     (nth 2 state) (car state) (nth 1 state)))
                          :coding 'utf-8-unix)
                    (list :op 'write :path base :content (car bytes)
                          :coding 'no-conversion)
                    (list :op 'delete-file :path legacy)))
               (niwa-plugins--lock-directory workspace)))))
        (unless (cl-some (lambda (result)
                           (memq (plist-get result :status) '(mismatch absent conflict)))
                         (seq-take (cdr results) 2))
          (mapc #'niwa-session-control-fs-program-value (nthcdr 3 results))
          t)))))

(defun niwa-plugins--read-state (&optional workspace)
  "Read persistent plugin state for WORKSPACE.
A tracked top-level file supplies activation, otherwise the file under
`state/' does.  Hook consent comes from the user's store only, see
`niwa-plugins--user-consent'.  An untracked top-level file is an older client's
and is adopted first; until that succeeds it lists plugins without
enabling hooks.  Remotely, only an authorized update adopts, see
`niwa-plugins--change-state': reading proves no lease.  When Git cannot
say, the file is left alone and read as tracked."
  (when workspace
    (let ((project (niwa-plugins--project-state-file workspace))
          (local (niwa-plugins-state-file workspace)))
      (niwa-plugins--user-consent
       (cond
        ((not (file-exists-p project)) (niwa-plugins--read-file local))
        ((not (eq 'untracked (niwa-plugins--tracking workspace project)))
         (niwa-plugins--read-file project))
        ((and (not (file-remote-p local))
              (ignore-errors (niwa-plugins--adopt-legacy workspace)))
         (niwa-plugins--read-file local))
        (t (or (niwa-plugins--read-file local)
               (niwa-plugins--read-file project))))
       workspace))))

(defun niwa-plugins--state-session (workspace)
  "Return the live session authorized to mutate WORKSPACE plugin state."
  (let ((session
         (or niwa--session
             (ignore-errors
               (niwa-cockpit-context-session
                (niwa-cockpit-current-context))))))
    (and session
         (eq workspace (niwa-session-workspace session))
         session)))

(defun niwa-plugins--state-text (state)
  "Return the durable plugin STATE file contents."
  (with-temp-buffer
    (insert ";; Niwa plugin state\n")
    (insert ";; Auto-generated, safe to edit\n\n")
    (pp state (current-buffer))
    (buffer-string)))

(defun niwa-plugins--lock-directory (workspace)
  "Return the lock serializing WORKSPACE's plugin state writers.
The project permission store's writers hold the same `.niwa/' lock."
  (niwa-session-control-fs-physical-path
   (directory-file-name (niwa-workspace-state-dir workspace))))

(defun niwa-plugins--try-update (workspace function local project)
  "Apply FUNCTION once to WORKSPACE's stored plugin state.
Return non-nil when the result was written.
LOCAL is the state file; PROJECT, when non-nil, the tracked activation file.
The write proves the bytes read are unchanged in one program under the
`.niwa/' lock; a remote write also runs inside its session's fence."
  (let* ((files (delq nil (list local project)))
         (bytes (mapcar (lambda (result)
                          (and (eq (plist-get result :status) 'ok)
                               (niwa-session-control-fs-program-value result)))
                        (niwa-session-control-fs-run-program
                         (mapcar (lambda (file)
                                   (list :op 'read :path file :coding 'no-conversion
                                         :optional t))
                                 files))))
         ;; Unreadable state refuses the update rather than reading as empty.
         (parsed (cl-mapcar (lambda (data file)
                              (and data (niwa-plugins--parse-strict
                                         (decode-coding-string data 'utf-8-unix) file)))
                            bytes files))
         (activation (lambda (state)
                       (mapcar (lambda (entry)
                                 (cons (car entry) (niwa-plugins--keys (cdr entry))))
                               state)))
         (old (niwa-plugins--user-consent (if project (nth 1 parsed) (car parsed))
                                          workspace))
         (new (funcall function (copy-tree old)))
         (writes
          ;; Workspace files hold activation only; consent goes to the
          ;; user's store once this write lands.
          (if project
              ;; Unchanged tracked activation is not rewritten.
              (and (not (equal (funcall activation new)
                               (funcall activation (nth 1 parsed))))
                   (list (cons project (niwa-plugins--state-text
                                        (funcall activation new)))))
            (list (cons local (niwa-plugins--state-text (funcall activation new))))))
         (program
          (cons (list :op 'make-directory
                      :path (directory-file-name (file-name-directory local))
                      :optional t)
                (cl-loop for file in files for data in bytes
                        ;; A written file read absent is created exclusively
                        ;; below, so a creator outside the lock keeps its file
                        ;; and the conflict rereads it, as the permission
                        ;; store does.
                        unless (and (not data) (assoc file writes))
                        collect (if data
                                    (list :op 'verify-hash :path file
                                          :content (secure-hash 'sha256 data))
                                  (list :op 'absent :path file)))))
         (proved (lambda (results)
                   (not (cl-some (lambda (result)
                                   (memq (plist-get result :status)
                                         '(mismatch absent conflict)))
                                 (cdr results))))))
    (cl-flet ((write (&optional proof)
                ;; The lock opens `.niwa/', so the state directory must exist.
                (niwa-session-control-fs-make-directory
                 (directory-file-name (file-name-directory local)) t)
                ;; Each file is a whole record, read as nil when truncated, so
                ;; the program's rename-into-place write matters.  A remote
                ;; session's lease PROOF leads it, checked under the lock.
                (let ((results (niwa-session-artifacts-fenced-results
                                proof
                                (niwa-session-control-fs-run-program
                                 (append proof program
                                         (mapcar (lambda (write)
                                                   (list :op (if (nth (cl-position (car write) files
                                                                                   :test #'equal)
                                                                      bytes)
                                                                 'write
                                                               'create)
                                                         :path (car write)
                                                         :content (cdr write)
                                                         :coding 'utf-8-unix))
                                                 writes))
                                 (niwa-plugins--lock-directory workspace)))))
                  (when (funcall proved results)
                    (mapc #'niwa-session-control-fs-program-value
                          (nthcdr (length program) results))
                    t))))
      (when (if (file-remote-p local)
                ;; The session fences the target; the program's lock and proof
                ;; exclude every other writer of these shared files.
                (niwa-session-artifacts-call-fenced
                 (or (niwa-plugins--state-session workspace)
                     (user-error "Remote plugin state requires its live session"))
                 #'write)
              (write))
        (niwa-plugins--store-consent workspace old new)
        t))))

(defun niwa-plugins--change-state (workspace function)
  "Apply FUNCTION to WORKSPACE's plugin state alist and persist the result.
The state is read once and written only if unchanged since.  On a conflict
FUNCTION applies again to what is stored now, up to three times, so a
concurrent adoption or consent change survives.  With a tracked top-level
file, activation goes there and hook consent stays under `state/'."
  (let ((local (or (niwa-plugins-state-file workspace)
                   (error "No workspace for plugin state")))
        (project (niwa-plugins--project-state-file workspace)))
    ;; Adopts an untracked legacy file first, so it is not written around.
    ;; Remotely the adoption migrates and deletes shared files, so it runs
    ;; inside the session's fence with the lease proof; reads never adopt.
    (if (not (file-remote-p local))
        (niwa-plugins--read-state workspace)
      (when (and (file-exists-p project)
                 (eq 'untracked (niwa-plugins--tracking workspace project)))
        (let ((session (or (niwa-plugins--state-session workspace)
                           (user-error "Remote plugin state requires its live session"))))
          (ignore-errors
            (niwa-session-artifacts-call-fenced
             session (lambda (&optional proof) (niwa-plugins--adopt-legacy workspace proof)))))))
    ;; A local program pins parents and locks `.niwa/', so both must exist.
    ;; Remote writes create nothing before publication.
    (unless (file-remote-p local)
      (niwa-session-control-fs-make-directory
       (directory-file-name (file-name-directory local)) t))
    (let ((project (and (file-exists-p project)
                        (not (eq 'untracked (niwa-plugins--tracking workspace project)))
                        project)))
      (unless (cl-loop repeat 3
                       thereis (niwa-plugins--try-update workspace function local project))
        (user-error "Plugin state changed during each of three attempts; try again")))
    ;; Enablement and consent decide which plugin hooks resolve, so every
    ;; state write leaves the memoized hook configuration stale.  Guarded
    ;; rather than required: hooks depends on plugins, not the other way.
    (when (fboundp 'niwa-hooks-invalidate-config)
      (niwa-hooks-invalidate-config))))

(defun niwa-plugins--write-state (state &optional workspace)
  "Replace WORKSPACE's plugin state with STATE."
  (niwa-plugins--change-state workspace (lambda (_) state)))

(defun niwa-plugins--state-plist (name &optional workspace)
  "Return persisted state plist for plugin NAME in WORKSPACE."
  (cdr (assoc name (niwa-plugins--read-state workspace))))

(defun niwa-plugins-source-root (root &optional workspace)
  "Return the durable source identity for ROOT in WORKSPACE.
Project sources use workspace-relative paths; other sources stay absolute."
  (when root
    (let* ((root (file-name-as-directory (expand-file-name root)))
           (workspace-root
            (and workspace
                 (file-name-as-directory
                  (expand-file-name (niwa-workspace-root workspace)))))
           (relative
            (and workspace-root
                 (equal (file-remote-p root)
                        (file-remote-p workspace-root))
                 (file-relative-name root workspace-root))))
      (if (and relative
               (not (file-name-absolute-p relative))
               (not (equal relative ".."))
               (not (string-prefix-p "../" relative)))
          relative
        root))))

(defun niwa-plugins-managed-roots ()
  "Return directories whose plugins niwa may update or remove."
  (delete-dups
   (mapcar #'niwa-plugins-source-root
           (list (niwa-plugins--global-niwa-dir)
                 (niwa-plugins-dir)))))

(defun niwa-plugins-same-root-p (a b &optional workspace)
  "Return non-nil when source roots A and B identify the same directory.
Relative identities are requalified through the live WORKSPACE root."
  (when (and a b)
    (let ((base (and workspace (niwa-workspace-root workspace))))
      (equal (file-name-as-directory (expand-file-name a base))
             (file-name-as-directory (expand-file-name b base))))))

(defun niwa-plugins--hook-rules-from-file (file)
  "Return normalized hook rules from FILE."
  (when (and (file-readable-p file)
             (or (fboundp 'niwa-hooks-read-config-file)
                 (require 'niwa-hooks nil t)))
    (niwa-hooks-read-config-file file)))

(defun niwa-plugins--hook-rules-from-hooks (hooks)
  "Return normalized hook rules declared by HOOKS."
  (let (rules)
    (dolist (entry hooks rules)
      (when-let* ((file (plist-get entry :file)))
        (setq rules
              (append rules
                      (niwa-plugins--hook-rules-from-file file)))))))

(defun niwa-plugins--hook-surface-from-hooks (hooks &optional workspace)
  "Return the hook consent surface declared by HOOKS in WORKSPACE."
  (let (items)
    (dolist (entry hooks)
      (when-let* ((file (plist-get entry :file)))
        (let ((source (niwa-plugins-source-root file workspace)))
          (push (list source) items)
          (dolist (rule (niwa-plugins--hook-rules-from-file file))
            (let ((event (symbol-name (car rule))))
              (dolist (group (cdr-safe rule))
                (let ((matcher (plist-get group :matcher)))
                  (dolist (handler (plist-get group :hooks))
                    (pcase (plist-get handler :type)
                      ('command
                       (push (list source event matcher 'command
                                   (plist-get handler :command))
                             items))
                      ('elisp
                       (push (list source event matcher 'elisp
                                   (plist-get handler :function))
                             items)))))))))))
    (sort (delete-dups items)
          (lambda (a b)
            (string< (prin1-to-string a)
                     (prin1-to-string b))))))

(defun niwa-plugins--hook-fingerprint-from-hooks
    (hooks &optional workspace)
  "Return a consent fingerprint for normalized plugin HOOKS in WORKSPACE."
  (when-let* ((surface (niwa-plugins--hook-surface-from-hooks
                        hooks workspace)))
    (secure-hash 'sha256 (prin1-to-string surface))))

(defun niwa-plugins--hook-fingerprint (plugin &optional workspace)
  "Return the current hook consent fingerprint for PLUGIN in WORKSPACE."
  (niwa-plugins--hook-fingerprint-from-hooks
   (niwa-plugin-hooks plugin)
   workspace))

(defun niwa-plugins--state-enabled-p (name &optional workspace root)
  "Return non-nil when plugin NAME from ROOT is enabled in WORKSPACE."
  (let ((state (niwa-plugins--state-plist name workspace)))
    (and (plist-get state :enabled)
         (niwa-plugins-same-root-p
          root
          (plist-get state :source-root)
          workspace))))

(defun niwa-plugins--state-hooks-enabled-p
    (name &optional workspace root hooks)
  "Return non-nil when plugin NAME from ROOT has hooks enabled in WORKSPACE."
  (let* ((state (niwa-plugins--state-plist name workspace))
         (fingerprint (niwa-plugins--hook-fingerprint-from-hooks
                       hooks workspace)))
    (and fingerprint
         (plist-get state :hooks-enabled)
         (niwa-plugins--state-enabled-p name workspace root)
         (equal fingerprint (plist-get state :hooks-fingerprint)))))

(defun niwa-plugins-enabled-p (plugin &optional workspace)
  "Return non-nil when PLUGIN is enabled in WORKSPACE."
  (niwa-plugins--state-enabled-p
   (niwa-plugin-name plugin)
   workspace
   (niwa-plugin-root plugin)))

(defun niwa-plugins-hooks-enabled-p (plugin &optional workspace)
  "Return non-nil when hooks are enabled for PLUGIN in WORKSPACE."
  (niwa-plugins--state-hooks-enabled-p
   (niwa-plugin-name plugin)
   workspace
   (niwa-plugin-root plugin)
   (niwa-plugin-hooks plugin)))

(defun niwa-plugins--write-state-entry (name plist &optional workspace)
  "Persist plugin NAME state PLIST for WORKSPACE, leaving other entries."
  (niwa-plugins--change-state
   workspace
   (lambda (state)
     (sort (cons (cons name plist) (assoc-delete-all name state))
           (lambda (a b) (string< (car a) (car b)))))))

(defun niwa-plugins-remove-state-for-source
    (name root &optional workspace)
  "Remove NAME state only when it points at source ROOT."
  (when-let* ((entry (assoc name (niwa-plugins--read-state workspace)))
              ((niwa-plugins-same-root-p
                root (plist-get (cdr entry) :source-root) workspace)))
    (niwa-plugins--change-state
     workspace
     (lambda (state)
       (let ((entry (assoc name state)))
         (if (and entry (niwa-plugins-same-root-p
                         root (plist-get (cdr entry) :source-root) workspace))
             (assoc-delete-all name state)
           state))))))

(defun niwa-plugins-transfer-state-entry
    (old-name new-name root &optional workspace)
  "Move OLD-NAME's state to NEW-NAME in WORKSPACE if it belongs to ROOT.
The entry moves as stored when written, so a change made meanwhile, such
as a hook revocation during an update, survives."
  (let ((source (niwa-plugins-source-root root workspace)))
    (niwa-plugins--change-state
     workspace
     (lambda (state)
       (let ((entry (assoc old-name state)))
         (if (not (and entry
                       (or (null (plist-get (cdr entry) :source-root))
                           (niwa-plugins-same-root-p
                            root (plist-get (cdr entry) :source-root) workspace))))
             state
           (sort (cons (cons new-name (plist-put (copy-sequence (cdr entry))
                                                 :source-root source))
                       (assoc-delete-all new-name (assoc-delete-all old-name state)))
                 (lambda (a b) (string< (car a) (car b))))))))))

(defun niwa-plugins--change-entry (name workspace function)
  "Replace NAME's state in WORKSPACE with FUNCTION of the stored entry.
FUNCTION receives a copy of the entry read inside the write, or nil."
  (niwa-plugins--change-state
   workspace
   (lambda (state)
     (sort (cons (cons name (funcall function (copy-sequence (cdr (assoc name state)))))
                 (assoc-delete-all name state))
           (lambda (a b) (string< (car a) (car b)))))))

(defun niwa-plugins-enabled (&optional workspace)
  "Return installed plugins enabled in WORKSPACE."
  (cl-remove-if-not
   (lambda (plugin) (niwa-plugins-enabled-p plugin workspace))
   (plist-get (niwa-plugins--collect workspace) :winners)))

(defun niwa-plugins-count-label (&optional workspace)
  "Return enabled/total plugin count label for WORKSPACE."
  (if workspace
      (let ((collection (niwa-plugins--collect workspace)))
        (format "%d/%d"
                (plist-get collection :enabled-count)
                (plist-get collection :total-count)))
    "0/0"))

(defun niwa-plugins-state-entry (plugin &optional workspace)
  "Return persisted state plist for PLUGIN in WORKSPACE, with defaults applied."
  (let* ((state (copy-sequence
                 (or (niwa-plugins--state-plist
                      (niwa-plugin-name plugin) workspace)
                     '(:enabled nil :hooks-enabled nil)))))
    (setq state
          (plist-put state
                     :source-root
                     (niwa-plugins-source-root
                      (niwa-plugin-root plugin)
                      workspace)))
    state))

(defun niwa-plugins--hook-rules (plugin)
  "Return normalized hook rules declared by PLUGIN."
  (niwa-plugins--hook-rules-from-hooks
   (niwa-plugin-hooks plugin)))

(defun niwa-plugins--hook-script-hashes (plugin)
  "Return the (FILE . DIGEST) scripts PLUGIN's hook commands name."
  (when (require 'niwa-hooks nil t)
    (niwa-hooks-rules-script-hashes
     (niwa-plugins--hook-rules plugin) (niwa-plugin-root plugin) t)))

(defun niwa-plugins-hook-rule-events (plugin)
  "Return event names declared by PLUGIN hooks."
  (sort
   (delete-dups
    (delq nil
          (mapcar (lambda (rule)
                    (and (consp rule)
                         (symbol-name (car rule))))
                  (niwa-plugins--hook-rules plugin))))
   #'string<))

(defun niwa-plugins--hook-rule-handlers (plugin)
  "Return concise handler descriptions declared by PLUGIN hooks."
  (let (handlers)
    (dolist (rule (niwa-plugins--hook-rules plugin))
      (dolist (group (cdr-safe rule))
        (let ((scope (format "%s [%s]"
                             (car rule)
                             (or (plist-get group :matcher) "*"))))
          (dolist (handler (plist-get group :hooks))
            (pcase (plist-get handler :type)
              ('command
               (push (format "%s: command %s"
                             scope (plist-get handler :command))
                     handlers))
              ('elisp
               (push (format "%s: elisp %s"
                             scope (plist-get handler :function))
                     handlers)))))))
    (sort (delete-dups handlers) #'string<)))

(defun niwa-plugins-hook-consent-summary (plugin &optional workspace)
  "Return a concise hook consent summary for PLUGIN in WORKSPACE."
  (let ((events (niwa-plugins-hook-rule-events plugin))
        (handlers (niwa-plugins--hook-rule-handlers plugin))
        (skills (niwa-plugins-skill-count plugin)))
    (string-join
     (delq nil
           (list
            (format "Enable plugin %s hooks?" (niwa-plugin-name plugin))
            (format "Version: %s"
                    (or (niwa-plugin-version plugin) "unspecified"))
            (format "Source: %s" (niwa-plugin-root plugin))
            (when (> skills 0)
              (format "Skills: %d from %s"
                      skills
                      (niwa-plugin-skills-dir plugin)))
            (when events
              (format "Events: %s" (string-join events ", ")))
            (when handlers
              (format "Handlers: %s" (string-join handlers ", ")))
            (when workspace
              (format "Runtime data: %s"
                      (niwa-plugins-plugin-data-dir
                       (niwa-plugin-name plugin) workspace)))))
     "\n")))

(defun niwa-plugins--ensure-hook-consent (plugin &optional workspace)
  "Return PLUGIN's approved hook snapshot in WORKSPACE, or nil if refused.
Ask again when the handlers or the content of a script they name
changed since the consent recorded in WORKSPACE.  The snapshot, a plist
of `:fingerprint', `:scripts' and the physical `:binding', is what was
compared or shown, so a change after that stays unconsented."
  (let* ((binding (niwa-plugins--consent-binding plugin workspace))
         (fingerprint (niwa-plugins--hook-fingerprint plugin workspace))
         (state (niwa-plugins--state-plist
                 (niwa-plugin-name plugin) workspace))
         (scripts (and fingerprint
                       (niwa-plugins--hook-script-hashes plugin)))
         (unconsented (and fingerprint
                           (cl-set-difference
                            scripts
                            (plist-get state :hooks-scripts)
                            :test #'equal))))
    (and (or (not fingerprint)
             (and (equal fingerprint (plist-get state :hooks-fingerprint))
                  (not unconsented))
             (yes-or-no-p
              (concat (niwa-plugins-hook-consent-summary plugin workspace)
                      (when unconsented
                        (format "\nNew or changed scripts: %s"
                                (mapconcat #'car unconsented ", ")))
                      " ")))
         (list :fingerprint fingerprint :scripts scripts :binding binding))))

(defun niwa-plugins--ensure-source-switch-consent
    (plugin &optional workspace)
  "Return non-nil when PLUGIN may replace an active source binding."
  (let* ((state (niwa-plugins--state-plist
                 (niwa-plugin-name plugin) workspace))
         (source-root (plist-get state :source-root)))
    (or (not (plist-get state :enabled))
        (niwa-plugins-same-root-p
         (niwa-plugin-root plugin) source-root workspace)
        (yes-or-no-p
         (format "Switch plugin %s activation from %s to %s? "
                 (niwa-plugin-name plugin)
                 source-root
                 (niwa-plugin-root plugin))))))

(defun niwa-plugins--write-enabled-state
    (plugin consent &optional workspace)
  "Persist PLUGIN as enabled in WORKSPACE, with hooks per CONSENT.
CONSENT is the snapshot `niwa-plugins--ensure-hook-consent' approved;
hooks are enabled when it has a fingerprint, recording exactly its
fingerprint and script hashes for its reviewed binding."
  (let ((fingerprint (plist-get consent :fingerprint))
        (niwa-plugins--reviewed-binding
         (niwa-plugins--reviewed plugin consent)))
    (niwa-plugins--write-state-entry
     (niwa-plugin-name plugin)
     (list :enabled t
           :hooks-enabled (and fingerprint t)
           :source-root (niwa-plugins-source-root
                         (niwa-plugin-root plugin)
                         workspace)
           :hooks-fingerprint fingerprint
           :hooks-scripts (and fingerprint (plist-get consent :scripts)))
     workspace)))

(defun niwa-plugins-enable (plugin-name &optional workspace)
  "Persist PLUGIN-NAME as enabled in WORKSPACE."
  (when-let* ((plugin (niwa-plugins-find plugin-name workspace))
              ((niwa-plugins--ensure-source-switch-consent plugin workspace))
              (consent (niwa-plugins--ensure-hook-consent plugin workspace)))
    (niwa-plugins--write-enabled-state plugin consent workspace)
    (niwa-plugins--binding-unchanged-p plugin consent workspace)))

(defun niwa-plugins-disable (plugin-name &optional workspace)
  "Persist PLUGIN-NAME as disabled in WORKSPACE."
  (let ((plugin (niwa-plugins-find plugin-name workspace)))
    (niwa-plugins--write-state-entry
     plugin-name
     (list :enabled nil
           :hooks-enabled nil
           :source-root (and plugin
                             (niwa-plugins-source-root
                              (niwa-plugin-root plugin)
                              workspace))
           :hooks-fingerprint nil)
     workspace)))

(defun niwa-plugins-enable-hooks (plugin-name &optional workspace)
  "Persist hooks as enabled for PLUGIN-NAME in WORKSPACE.
Return non-nil only when the consent landed on the plugin it was given for."
  (when-let* ((plugin (niwa-plugins-find plugin-name workspace)))
    (when-let* (((niwa-plugin-hooks plugin))
                ((niwa-plugins-enabled-p plugin workspace))
                (consent (niwa-plugins--ensure-hook-consent plugin workspace))
                (root (niwa-plugin-root plugin)))
      (let ((niwa-plugins--reviewed-binding
             (niwa-plugins--reviewed plugin consent))
            applied)
        ;; Only while still enabled from this source: a disable or a source
        ;; switch meanwhile wins, and the consent is not reported as given.
        (niwa-plugins--change-entry
         plugin-name workspace
         (lambda (entry)
           (setq applied (and (plist-get entry :enabled)
                              (niwa-plugins-same-root-p
                               root (plist-get entry :source-root) workspace)))
           (if (not applied)
               entry
             (let ((fingerprint (plist-get consent :fingerprint)))
               (thread-first entry
                             (plist-put :hooks-enabled (and fingerprint t))
                             (plist-put :hooks-fingerprint fingerprint)
                             (plist-put :hooks-scripts
                                        (and fingerprint (plist-get consent :scripts))))))))
        (and applied
             (niwa-plugins--binding-unchanged-p plugin consent workspace))))))

(defun niwa-plugins-disable-hooks (plugin-name &optional workspace)
  "Persist hooks as disabled for PLUGIN-NAME in WORKSPACE."
  (let ((root (when-let* ((plugin (niwa-plugins-find plugin-name workspace)))
                (niwa-plugin-root plugin))))
    ;; Only the hook flag changes, on the entry as stored when written, and
    ;; only while it still names this source: never rewrite :source-root.
    (niwa-plugins--change-entry
     plugin-name workspace
     (lambda (entry)
       (if (and entry root (plist-get entry :source-root)
                (not (niwa-plugins-same-root-p
                      root (plist-get entry :source-root) workspace)))
           entry
         (plist-put entry :hooks-enabled nil))))))


;;
;;; Registry summaries

(defun niwa-plugins-active-shadowed-source
    (plugin &optional workspace)
  "Return shadowed active source for PLUGIN in WORKSPACE, or nil."
  (let ((state (niwa-plugins--state-plist
                (niwa-plugin-name plugin) workspace)))
    (and (plist-get state :enabled)
         (not (niwa-plugins-same-root-p
               (niwa-plugin-root plugin)
               (plist-get state :source-root)
               workspace))
         (cl-find-if
          (lambda (shadow)
            (niwa-plugins-same-root-p
             (niwa-plugin-root shadow)
             (plist-get state :source-root)
             workspace))
          (niwa-plugin-shadowed plugin)))))

(defun niwa-plugins-skill-count (plugin)
  "Return number of SKILL.md files exposed by PLUGIN."
  (if-let* ((dir (niwa-plugin-skills-dir plugin))
            ((file-directory-p dir)))
      (length (directory-files-recursively dir "\\`SKILL\\.md\\'"))
    0))

(defun niwa-plugins-hooks-stale-p (plugin &optional workspace)
  "Return non-nil when PLUGIN hook consent is stale in WORKSPACE."
  (let* ((state (niwa-plugins--state-plist
                 (niwa-plugin-name plugin) workspace))
         (fingerprint (niwa-plugins--hook-fingerprint plugin workspace)))
    (and fingerprint
         (plist-get state :hooks-enabled)
         (niwa-plugins-same-root-p
          (niwa-plugin-root plugin)
          (plist-get state :source-root)
          workspace)
         (not (equal fingerprint
                     (plist-get state :hooks-fingerprint))))))

(defun niwa-plugins-hooks-status (plugin &optional workspace)
  "Return user-facing hook status for PLUGIN in WORKSPACE."
  (cond
   ((not (niwa-plugin-hooks plugin)) "none")
   ((niwa-plugins-hooks-enabled-p plugin workspace) "on")
   ((niwa-plugins-hooks-stale-p plugin workspace) "needs-consent")
   (t "off")))

(defun niwa-plugins-pending-consent (&optional workspace)
  "Return enabled plugins whose executable hooks need consent in WORKSPACE."
  (cl-remove-if-not
   (lambda (plugin) (niwa-plugins-hooks-stale-p plugin workspace))
   (niwa-plugins-enabled workspace)))


;;
;;; Update-check state

;; The check itself runs in `niwa-plugin-lifecycle'; this section owns the
;; setting, the idle scheduling and the cached results.  The state is global
;; (managed plugins are global), so it lives beside the other user-level
;; files rather than in a workspace's `plugins.eld'.

(declare-function niwa-plugins-check-updates
                  "niwa-plugin-lifecycle" (&optional on-demand done))
(autoload 'niwa-plugins-check-updates "niwa-plugin-lifecycle")

(defconst niwa-plugins--update-check-idle-delay 30
  "Seconds of idle time before a scheduled update check starts.")

(defvar niwa-plugins--update-cache :unloaded
  "Cached update-check state plist, or `:unloaded' before the first read.
Holds `:last-check' (`float-time') and `:plugins', an alist of source
root to a plist of `:local', `:remote', `:error' and `:notified'.")

(defvar niwa-plugins--update-check-timer nil
  "Pending idle timer for a scheduled update check, or nil.")

(defvar niwa-plugins--update-check-busy nil
  "Non-nil while an update check is running.")

(defun niwa-plugins--update-state-file ()
  "Return the global update-check state file."
  (file-name-concat niwa-user-dir "plugin-update-check.eld"))

(defun niwa-plugins--update-state ()
  "Return the update-check state, reading it once per session."
  (when (eq niwa-plugins--update-cache :unloaded)
    (setq niwa-plugins--update-cache
          (let ((file (niwa-plugins--update-state-file)))
            (and (file-readable-p file)
                 (condition-case nil
                     (with-temp-buffer
                       (insert-file-contents file)
                       (let ((read-eval nil))
                         (let ((state (read (current-buffer))))
                           (and (listp state) state))))
                   (error nil))))))
  niwa-plugins--update-cache)

(defun niwa-plugins--update-save (state)
  "Cache and persist update-check STATE."
  (setq niwa-plugins--update-cache state)
  (niwa--write-file-atomically
   (niwa-plugins--update-state-file)
   (with-temp-buffer
     (insert ";; Niwa plugin update-check state\n")
     (insert ";; Auto-generated, safe to delete\n\n")
     (pp state (current-buffer))
     (buffer-string))))

(defun niwa-plugins--update-entry-for-root (root)
  "Return the cached update-check plist for source ROOT, or nil."
  (cdr (assoc (niwa-plugins-source-root root)
              (plist-get (niwa-plugins--update-state) :plugins))))

(defun niwa-plugins--update-set-entry (root plist)
  "Cache PLIST for source ROOT, keeping its `:notified' commit."
  (let* ((state (copy-tree (niwa-plugins--update-state)))
         (key (niwa-plugins-source-root root))
         (plugins (assoc-delete-all key (plist-get state :plugins))))
    (when-let* (((not (plist-member plist :notified)))
                (notified
                 (plist-get (niwa-plugins--update-entry-for-root root)
                            :notified)))
      (setq plist (plist-put (copy-sequence plist) :notified notified)))
    (niwa-plugins--update-save
     (plist-put state :plugins (cons (cons key plist) plugins)))))

(defun niwa-plugins--update-mark-notified (root)
  "Record that the cached upstream commit of ROOT was announced."
  (when-let* ((entry (niwa-plugins--update-entry-for-root root)))
    (niwa-plugins--update-set-entry
     root (plist-put (copy-sequence entry) :notified
                     (plist-get entry :remote)))))

(defun niwa-plugins-forget-update (root)
  "Drop the cached update-check result for ROOT after it was updated."
  (when (niwa-plugins--update-entry-for-root root)
    (let ((state (copy-tree (niwa-plugins--update-state))))
      (niwa-plugins--update-save
       (plist-put state :plugins
                  (assoc-delete-all (niwa-plugins-source-root root)
                                    (plist-get state :plugins)))))))

(defun niwa-plugins-update-available-p (plugin)
  "Return the upstream commit when the cache says PLUGIN has an update."
  (let ((entry (niwa-plugins--update-entry-for-root
                (niwa-plugin-root plugin))))
    (and entry
         (not (plist-get entry :error))
         (plist-get entry :remote)
         (not (equal (plist-get entry :remote) (plist-get entry :local)))
         (plist-get entry :remote))))

(defun niwa-plugins-update-check-error (plugin)
  "Return the cached update-check failure message for PLUGIN, or nil."
  (plist-get (niwa-plugins--update-entry-for-root
              (niwa-plugin-root plugin))
             :error))

(defun niwa-plugins--update-check-due-p ()
  "Return non-nil when the interval setting asks for a check now."
  (and (numberp niwa-plugins-update-check-interval)
       (> niwa-plugins-update-check-interval 0)
       (let ((last (plist-get (niwa-plugins--update-state) :last-check)))
         (or (not (numberp last))
             (>= (- (float-time) last)
                 (* niwa-plugins-update-check-interval 86400))))))

(defun niwa-plugins--run-scheduled-update-check ()
  "Start the check an idle timer was set for, if it is still due."
  (setq niwa-plugins--update-check-timer nil)
  (when (niwa-plugins--update-check-due-p)
    (niwa-plugins-check-updates)))

(defun niwa-plugins--schedule-update-check ()
  "Schedule an idle update check when the interval says one is due.
Does nothing with the default nil interval, while a check is pending or
running, and never touches the network itself."
  (when (and (not niwa-plugins--update-check-timer)
             (not niwa-plugins--update-check-busy)
             (niwa-plugins--update-check-due-p))
    (setq niwa-plugins--update-check-timer
          (run-with-idle-timer
           niwa-plugins--update-check-idle-delay nil
           #'niwa-plugins--run-scheduled-update-check))))



(provide 'niwa-plugin-registry)

;;; niwa-plugin-registry.el ends here
