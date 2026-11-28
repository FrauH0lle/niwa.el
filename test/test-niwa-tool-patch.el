;;; test-niwa-tool-patch.el --- Tests for ApplyPatch -*- lexical-binding: t -*-

;;; Commentary:

;; Integration tests for the patch-oriented filesystem mutation tool.

;;; Code:

(require 'niwa-tool-patch-registration)

(require 'gptel-request)
(require 'niwa-pipeline)
(require 'niwa-reminders)
(require 'niwa-tool-patch)
(require 'niwa-instruction-edit)
(require 'niwa-change)
(require 'niwa-view)
(require 'niwa-view-interaction)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
            "helpers"))
(require 'niwa-session-persistence)

(defun test-niwa-tool-patch--read-bytes (path)
  "Read literal bytes from PATH for coding and rollback assertions."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (buffer-string)))

(defvar niwa-session--read-only-mode nil)

(niwa-deftest niwa-tool-patch-handler
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "niwa-apply-patch-" t)))
           (update-path (file-name-concat root "update.txt"))
           (delete-path (file-name-concat root "delete.txt"))
           (move-path (file-name-concat root "move.txt"))
           (moved-path (file-name-concat root "moved.txt"))
           (added-path (file-name-concat root "nested" "new.txt"))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "patch"
                       :file-cache (niwa-test-file-cache-create)))
           (session (niwa-session--create
                     :name "patch" :workspace workspace
                     :working-directory root :permission-mode 'edits))
           (buffer (generate-new-buffer " *niwa-apply-patch*"))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Update File: update.txt"
                     "@@"
                     " alpha"
                     "-old"
                     "+new"
                     " omega"
                     "*** Add File: nested/new.txt"
                     "+created"
                     "*** Delete File: delete.txt"
                     "*** Update File: move.txt"
                     "*** Move to: moved.txt"
                     "@@"
                     "-before move"
                     "+after move"
                     "*** End Patch")
                   "\n"))
           result)
   :before-each
   (progn
     (with-temp-file update-path (insert "alpha\nold\nomega\n"))
     (with-temp-file delete-path (insert "delete me\n"))
     (with-temp-file move-path (insert "before move\n"))
     (set-file-modes move-path #o751)
     (with-current-buffer buffer
       (setq-local default-directory root
                   niwa--workspace workspace
                   niwa--session session)))
   :after-each
   (progn
     (when (buffer-live-p buffer) (kill-buffer buffer))
     (when (file-directory-p root) (delete-directory root t))))
  ,test
  (test)
  :doc "Auto mode applies every operation and returns one aggregate result"
  (progn
    (with-current-buffer buffer
      (niwa-tool-patch-handler
       (lambda (value) (setq result value))
       (list :patch patch)))
    (should (equal "alpha\nnew\nomega\n"
                   (niwa-test--read-file update-path)))
    (should (equal "created\n"
                   (niwa-test--read-file added-path)))
    (should-not (file-exists-p delete-path))
    (should-not (file-exists-p move-path))
    (should (equal "after move\n"
                   (niwa-test--read-file moved-path)))
    (should (= #o751 (file-modes moved-path)))
    (should (equal "alpha\nnew\nomega\n"
                   (niwa-file-state-content
                    (niwa-file-cache-get
                     (niwa-workspace-file-cache workspace) update-path))))
    (should (string-prefix-p "Applied patch: 4 changes" (plist-get result :result)))
    (should (eq 'patch (plist-get (plist-get result :render-data) :kind))))

  :doc "Growing an unterminated final line preserves EOF and mixed line endings"
  (progn
    (let ((coding-system-for-write 'no-conversion))
      (with-temp-file update-path (insert "first\r\nsecond\nthird\r\nold")))
    (with-current-buffer buffer
      (niwa-tool-patch-handler
       (lambda (value) (setq result value))
       '(:patch "*** Begin Patch\n*** Update File: update.txt\n@@\n-old\n+A\n+B\n*** End Patch")))
    (should (equal "first\r\nsecond\nthird\r\nA\r\nB"
                   (test-niwa-tool-patch--read-bytes update-path))))

  :doc "Direct path authority auto-applies while the session remains in ask mode"
  (let ((niwa-pipeline--auto-apply-edit-p t))
    (setf (niwa-session-permission-mode session) 'ask)
    (with-current-buffer buffer
      (niwa-tool-patch-handler
       (lambda (value) (setq result value))
       (list :patch patch)))
    (should result)
    (should (equal "alpha\nnew\nomega\n"
                   (niwa-test--read-file update-path))))

  :doc "A long-line fragment identifies its file line without applying earlier operations"
  (let* ((fragment "- [ ] T-049 Add the fixture country")
         (original (concat "heading\n" fragment " " (make-string 260 ?x) "\nnext\n")))
    (with-temp-file update-path (insert original))
    (with-current-buffer buffer
      (niwa-tool-patch-handler
       (lambda (value) (setq result value))
       (list :patch
             (string-join
              (list "*** Begin Patch"
                    "*** Add File: nested/new.txt" "+created"
                    "*** Update File: delete.txt" "@@" "-delete me" "+changed"
                    "*** Update File: update.txt" "@@"
                    (concat "-" fragment) "+replacement"
                    "*** End Patch")
              "\n"))))
    (should (eq 'error (plist-get result :status)))
    (let ((text (plist-get result :result)))
      (should (string-match-p "operation 3.*hunk 1.*patch line 9" text))
      (should (string-match-p "removal line.*prefix of file line 2" text))
      (should (string-match-p "whole lines" text))
      (should (string-match-p "Read.*update.txt.*offset.*2" text)))
    (should-not (file-exists-p added-path))
    (should (equal "delete me\n" (niwa-test--read-file delete-path)))
    (should (equal original (niwa-test--read-file update-path))))

  :doc "Context and removal fragments distinguish prefix, suffix and middle substrings"
  (let* ((line (concat "task prefix " (make-string 260 ?x) " middle marker task suffix"))
         (original (concat "heading\n" line "\nold\n")))
    (with-temp-file update-path (insert original))
    (dolist (case '(("task prefix" "prefix")
                    ("task suffix" "suffix")
                    ("middle marker" "middle substring")))
      (dolist (context-p '(nil t))
        (with-current-buffer buffer
          (niwa-tool-patch-handler
           (lambda (value) (setq result value))
           (list :patch
                 (format
                  "*** Begin Patch\n*** Update File: update.txt\n@@\n%s%s%s\n+new\n*** End Patch"
                  (if context-p " " "-") (car case)
                  (if context-p "\n-old" "")))))
        (should (eq 'error (plist-get result :status)))
        (should (string-match-p
                 (format "%s line.*%s of file line 2"
                         (if context-p "context" "removal") (cadr case))
                 (plist-get result :result)))
        (should (equal original (niwa-test--read-file update-path))))))

  :doc "Multiple candidates, sequence failures and absent anchors do not guess a file line"
  (dolist (case '(("prefix shared\nother shared\n" "@@\n-shared\n+new")
                  ("first\nsecond\n" "@@\n-second\n-first\n+new")
                  ("needle trailing\n" "@@ absent\n-needle\n+new")
                  ("needle trailing\nanchor\n" "@@ anchor\n-needle\n+new")
                  ("anchor\nprefix needle suffix\n"
                   "@@\n-anchor\n+other\n@@ anchor\n-needle\n+new" 6)
                  ("anchor\nprefix needle suffix\nanchor\nold\n"
                   "@@ 1\n-anchor\n+other\n@@ anchor\n-needle\n+new" 6)
                  ("anchor\nprefix needle suffix\nold\n"
                   "@@ 3\n-old\n+needle\n@@ anchor\n-needle\n-prefix needle suffix\n+new" 6)
                  ("needle\nother\n"
                   "@@\n-needle\n+different\n@@\n-needle\n+new" 6)
                  ("needle  \nother\n"
                   "@@\n-needle\n+different\n@@\n-needle\n+new" 6)
                  ("first\nsecond\n" "@@\n-missing\n+new")
                  ("first\nsecond\n" "@@\n \n-second\n+new")))
    (with-temp-file update-path (insert (car case)))
    (with-current-buffer buffer
      (niwa-tool-patch-handler
       (lambda (value) (setq result value))
       (list :patch
             (format "*** Begin Patch\n*** Update File: update.txt\n%s\n*** End Patch"
                     (cadr case)))))
    (should (eq 'error (plist-get result :status)))
    (should (string-match-p
             (format "patch line %d.*does not match" (or (nth 2 case) 3))
             (plist-get result :result)))
    (should-not (string-match-p "file line\\|Read(" (plist-get result :result)))
    (should (equal (car case) (niwa-test--read-file update-path))))

  :doc "Escaped excerpts stay bounded even when an unmatched line is very long"
  (let ((fragment (concat "quoted \\\"\t" (make-string 400 ?\"))))
    (with-temp-file update-path (insert fragment " trailing\n"))
    (with-current-buffer buffer
      (niwa-tool-patch-handler
       (lambda (value) (setq result value))
       (list :patch
             (format "*** Begin Patch\n*** Update File: update.txt\n@@\n-%s\n+new\n*** End Patch"
                     fragment))))
    (let ((text (plist-get result :result)))
      (should (eq 'error (plist-get result :status)))
      (should (< (length text) 700))
      (should (string-match-p (regexp-quote "\\11") text))
      (should-not (string-match-p "[\t\n]" text))
      (should (string-match-p "prefix of file line 1" text))))

  :doc "File hints use baseline coordinates after an earlier hunk adds lines"
  (dolist (anchor '(nil "anchor"))
    (let ((original (concat "alpha\n" (and anchor "anchor\n")
                            "prefix target suffix\n")))
      (with-temp-file update-path (insert original))
      (with-current-buffer buffer
        (niwa-tool-patch-handler
         (lambda (value) (setq result value))
         (list :patch
               (string-join
                (list "*** Begin Patch" "*** Update File: update.txt"
                      "@@" "-alpha" "+alpha" "+extra" "+extra"
                      (if anchor "@@ anchor" "@@")
                      "-target" "+new" "*** End Patch") "\n"))))
      (should (eq 'error (plist-get result :status)))
      (should (string-match-p
               (format "hunk 2.*patch line 8.*file line %d" (if anchor 3 2))
               (plist-get result :result)))
      (should (equal original (niwa-test--read-file update-path))))))

(niwa-deftest niwa-tool-patch-resource-handler
  (:vars* ((root (file-name-as-directory
                  (make-temp-file "niwa-apply-patch-resource-" t)))
           (workspace (niwa-workspace--create
                       :type 'file :id root :root root :name "patch-resource"
                       :file-cache (niwa-test-file-cache-create)))
           (session (let ((session (niwa-session-create
                                    "patch-resource" workspace root "patch-resource-id")))
                      (setf (niwa-session-permission-mode session) 'edits)
                      session))
           (buffer (generate-new-buffer " *niwa-apply-patch-resource*"))
           (patch (string-join
                   '("*** Begin Patch"
                     "*** Add File: work://notes/new.txt"
                     "+created"
                     "*** End Patch")
                   "\n"))
           result)
   :after-each
   (progn
     (when (buffer-live-p buffer) (kill-buffer buffer))
     (when-let* ((save-path (niwa-session-save-path session)))
       (niwa-session-persistence-lock-release save-path session))
     (when (file-directory-p root) (delete-directory root t))))
  ,test
  (test)
  :doc "Local resources materialize a durable session on first write"
  (progn
    (with-current-buffer buffer
      (setq-local default-directory root
                  niwa--workspace workspace
                  niwa--session session)
      (niwa-tool-patch-handler
       (lambda (value) (setq result value))
       (list :patch patch)))
    (let* ((save-path (niwa-session-save-path session))
           (local-path (file-name-concat save-path "local" "notes" "new.txt")))
      (should save-path)
      (should (niwa-session-durability-lease-owned-p session))
      (should (equal "created\n"
                     (niwa-test--read-file local-path)))
      (should (string-match-p "work://notes/new.txt"
                              (plist-get result :result)))
      (should (= 0 (hash-table-count (niwa-session-touched-files session))))))

  :doc "Mismatch hints name the authored resource without its private backing directory"
  (progn
    (with-current-buffer buffer
      (setq-local default-directory root
                  niwa--workspace workspace
                  niwa--session session)
      (niwa-tool-patch-handler
       (lambda (value) (setq result value))
       (list :patch patch))
      (niwa-tool-patch-handler
       (lambda (value) (setq result value))
       '(:patch "*** Begin Patch\n*** Update File: work://notes/new.txt\n@@\n-creat\n+new\n*** End Patch")))
    (should (eq 'error (plist-get result :status)))
    (let ((text (plist-get result :result)))
      (should (string-match-p "operation 1.*hunk 1.*patch line 3" text))
      (should (string-match-p "prefix of file line 1" text))
      (should (string-match-p "Read(file_path=\"work://notes/new.txt\"" text))
      (should-not (string-match-p (regexp-quote root) text))
      (should-not (string-match-p (regexp-quote (niwa-session-save-path session)) text)))))

(niwa-deftest niwa-tool-patch-mixed-move-locality
  (:doc "Tracks only the ordinary operand of mixed local moves")
  ,test
  (test)
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-apply-patch-mixed-move-" t)))
         (workspace (niwa-workspace--create
                     :type 'file :id root :root root :name "mixed-move"
                     :file-cache (niwa-test-file-cache-create)))
         (session (let ((session (niwa-session-create
                                  "mixed-move" workspace root "mixed-move-id")))
                    (setf (niwa-session-permission-mode session) 'edits)
                    session))
         (buffer (generate-new-buffer " *niwa-apply-patch-mixed-move*"))
         (local-source "work://notes/from-local.txt")
         (local-target "work://notes/to-local.txt")
         (ordinary-source (file-name-concat root "from-ordinary.txt"))
         (ordinary-target (file-name-concat root "to-ordinary.txt"))
         result)
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local default-directory root
                        niwa--workspace workspace
                        niwa--session session)
            (niwa-session-persistence-shallow-ensure-files
             session buffer))
          (let ((local-root (file-name-concat
                             (niwa-session-save-path session) "local")))
            (make-directory (file-name-concat local-root "notes") t)
            (with-temp-file (file-name-concat local-root "notes" "from-local.txt")
              (insert "local source\n")))
          (with-current-buffer buffer
            (let ((niwa-pipeline--auto-apply-edit-p t))
              (niwa-tool-patch-handler
               (lambda (value) (setq result value))
               (list :patch
                     (string-join
                      (list "*** Begin Patch"
                            (concat "*** Update File: " local-source)
                            (concat "*** Move to: "
                                    (file-name-nondirectory ordinary-target))
                            "*** End Patch")
                      "\n")))
              (should result))
            (should (file-exists-p ordinary-target))
            (should-not (file-exists-p
                         (file-name-concat
                          (niwa-session-save-path session) "local"
                          "notes" "from-local.txt")))
            (should (gethash ordinary-target
                             (niwa-session-touched-files session)))
            (should-not (gethash
                         (file-name-concat
                          (niwa-session-save-path session) "local"
                          "notes" "from-local.txt")
                         (niwa-session-touched-files session)))
            (with-temp-file ordinary-source (insert "ordinary source\n"))
            (niwa-tool-patch-handler
             (lambda (value) (setq result value))
             (list :patch
                   (string-join
                    (list "*** Begin Patch"
                          (concat "*** Update File: "
                                  (file-name-nondirectory ordinary-source))
                          (concat "*** Move to: " local-target)
                          "*** End Patch")
                    "\n")))
            (should result)
            (should-not (file-exists-p ordinary-source))
            (should (file-exists-p
                     (file-name-concat
                      (niwa-session-save-path session) "local"
                      "notes" "to-local.txt")))
            (should (gethash ordinary-source
                             (niwa-session-touched-files session)))
            (should-not (gethash
                         (file-name-concat
                          (niwa-session-save-path session) "local"
                          "notes" "to-local.txt")
                         (niwa-session-touched-files session)))))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (when-let* ((save-path (niwa-session-save-path session)))
        (niwa-session-persistence-lock-release save-path session))
      (when (file-directory-p root) (delete-directory root t)))))

(niwa-deftest niwa-tool-patch-annotate-line-numbers
  (:doc "Computes old and new start lines against the captured baseline")
  ,test
  (test)
  (let* ((proposal (niwa-tool-patch-parse
                    (string-join
                     '("*** Begin Patch"
                       "*** Update File: one.txt"
                       "@@ a"
                       "-old1"
                       "+new1"
                       "+extra"
                       "@@ d"
                       "-old2"
                       "+new2"
                       "*** End Patch")
                     "\n")
                    "/tmp/niwa-annotate"))
         (operation (car (plist-get proposal :operations))))
    (plist-put operation :baseline-content "a\nold1\nc\nd\nold2\n")
    (niwa-tool-patch-annotate-line-numbers proposal)
    (let ((hunks (plist-get operation :hunks)))
      (should (equal 2 (plist-get (car hunks) :old-start)))
      (should (equal 2 (plist-get (car hunks) :new-start)))
      (should (equal "old1" (plist-get (car hunks) :section)))
      (should (equal 5 (plist-get (cadr hunks) :old-start)))
      (should (equal 6 (plist-get (cadr hunks) :new-start)))))

  :doc "Later hunks annotate after earlier ones, resolving repeats"
  (let* ((proposal (niwa-tool-patch-parse
                    (string-join
                     '("*** Begin Patch"
                       "*** Update File: one.txt"
                       "@@"
                       "-a"
                       "+A"
                       "@@"
                       "-x"
                       "+X"
                       "*** End Patch")
                     "\n")
                    "/tmp/niwa-annotate"))
         (operation (car (plist-get proposal :operations))))
    (plist-put operation :baseline-content "x\na\nx\nb\n")
    (niwa-tool-patch-annotate-line-numbers proposal)
    (should (equal 2 (plist-get (car (plist-get operation :hunks))
                                :old-start)))
    (should (equal 3 (plist-get (cadr (plist-get operation :hunks))
                                :old-start))))

  :doc "A hunk that no longer matches keeps nil display positions"
  (let* ((proposal (niwa-tool-patch-parse
                    (string-join
                     '("*** Begin Patch"
                       "*** Update File: one.txt"
                       "@@"
                       "-gone"
                       "+new"
                       "*** End Patch")
                     "\n")
                    "/tmp/niwa-annotate"))
         (operation (car (plist-get proposal :operations))))
    (plist-put operation :baseline-content "other\n")
    (niwa-tool-patch-annotate-line-numbers proposal)
    (let ((hunk (car (plist-get operation :hunks))))
      (should-not (plist-get hunk :old-start))
      (should-not (plist-get hunk :new-start))
      (should-not (plist-get hunk :section)))))

(niwa-deftest niwa-tool-patch-apply-hunks ()
  ,test
  (test)
  :doc "Falls back to trailing-whitespace matching"
  (let ((hunk '(:old-lines ("old") :new-lines ("new") :selected t
                :diff-lines ("-old" "+new"))))
    (should (equal "before  \nnew\n"
                   (niwa-tool-patch-apply-hunks
                    "before  \nold  \n" (list hunk) "file.txt"))))

  :doc "Rejects ambiguous matches"
  (let ((hunk '(:old-lines ("same") :new-lines ("new") :selected t
                :diff-lines ("-same" "+new"))))
    (should-error
     (niwa-tool-patch-apply-hunks
      "same\nmiddle\nsame\n" (list hunk) "file.txt")
     :type 'error))

  :doc "Pure insertions follow their anchor and reject duplicate anchors"
  (let ((hunks (niwa-tool-patch-parse-update-lines '("@@ def bar" "+new") 1)))
    (should (equal "def bar():\nnew\nbody\ntail\n"
                   (niwa-tool-patch-apply-hunks "def bar():\nbody\ntail\n" hunks "f")))
    (should (string-match-p
             "ambiguous.*distinguishing context"
             (error-message-string
              (should-error (niwa-tool-patch-apply-hunks
                             "def bar1():\ndef bar2():\n" hunks "f"))))))

  :doc "A numeric hint chooses the closest matching line"
  (let ((hunk '(:old-lines ("same") :new-lines ("new") :selected t
                :line-hint 3 :diff-lines ("-same" "+new"))))
    (should (equal "same\nmiddle\nnew\n"
                   (niwa-tool-patch-apply-hunks
                    "same\nmiddle\nsame\n" (list hunk) "file.txt"))))

  :doc "Keeps baseline line hints stable after earlier line-count changes"
  (let* ((content "d1\nd2\nd3\nd4\nsep\nx\na\nb\nc\nd\nx\n")
         (hunks (list
                 '(:selected t
                   :old-lines ("d1" "d2" "d3" "d4")
                   :new-lines nil
                   :diff-lines ("-d1" "-d2" "-d3" "-d4"))
                 '(:selected t
                   :old-lines ("x")
                   :new-lines ("changed")
                   :line-hint 6
                   :diff-lines ("-x" "+changed"))))
         (hinted (cadr hunks))
         (operation (list :kind 'update :path "file.txt"
                          :baseline-content content :hunks hunks)))
    (niwa-tool-patch-annotate-line-numbers
     (list :operations (list operation)))
    (should (= 6 (plist-get hinted :old-start)))
    (should (equal "sep\nchanged\na\nb\nc\nd\nx\n"
                   (niwa-tool-patch-apply-hunks
                    content hunks "file.txt"))))

  :doc "Applies later hunks after earlier ones, resolving repeats"
  (let ((hunks '((:old-lines ("a") :selected t
                  :diff-lines ("-a" "+A"))
                 (:old-lines ("x") :selected t
                  :diff-lines ("-x" "+X")))))
    (should (equal "x\nA\nX\nb\n"
                   (niwa-tool-patch-apply-hunks
                    "x\na\nx\nb\n" hunks "file.txt"))))

  :doc "A deselected hunk still advances the disambiguation cursor"
  (let ((hunks '((:old-lines ("a") :selected nil
                  :diff-lines ("-a" "+A"))
                 (:old-lines ("x") :selected t
                  :diff-lines ("-x" "+X")))))
    (should (equal "x\na\nX\nb\n"
                   (niwa-tool-patch-apply-hunks
                    "x\na\nx\nb\n" hunks "file.txt"))))

  :doc "Preserves CRLF line endings"
  (let ((hunk '(:old-lines ("old") :new-lines ("new") :selected t
                :diff-lines ("-old" "+new"))))
    (should (equal "a\r\nnew\r\nb\r\n"
                   (niwa-tool-patch-apply-hunks
                    "a\r\nold\r\nb\r\n" (list hunk) "file.txt"))))

  :doc "Maps removed endings within each run and uses the majority for extras"
  (let ((hunk '(:old-lines ("a" "b" "keep" "c") :selected t
                :diff-lines ("-a" "-b" "+A" "+B" "+extra" " keep" "-c" "+C"))))
    (should (equal "A\nB\r\nextra\r\nkeep\r\nC\r\nend\r"
                   (niwa-tool-patch-apply-hunks
                    "a\nb\r\nkeep\r\nc\r\nend\r" (list hunk) "file.txt"))))

  :doc "Keeps fuzzy-matched context byte-identical"
  ;; The pattern ASCII-fies the file's typographic dash and drops the
  ;; deleted line's indentation; the applied result must keep the
  ;; context line verbatim while the change still lands.
  (let ((hunk '(:old-lines ("keep - this" "old") :selected t
                :diff-lines (" keep - this" "-old" "+new"))))
    (should (equal "keep \u2013 this\nnew\ntail\n"
                   (niwa-tool-patch-apply-hunks
                    "keep \u2013 this\n  old\ntail\n"
                    (list hunk) "file.txt"))))

  :doc "A locator hunk changes nothing and pins the hunk after it"
  ;; Without the locator the "x" hunk would be ambiguous; matching the
  ;; locator advances the cursor past the first "x".
  (let ((hunks '((:old-lines ("a" "x") :new-lines ("a" "x") :selected t
                  :diff-lines (" a" " x"))
                 (:old-lines ("x") :selected t
                  :diff-lines ("-x" "+X")))))
    (should (equal "x\na\nx\nX\n"
                   (niwa-tool-patch-apply-hunks
                    "x\na\nx\nx\n" hunks "file.txt"))))

  :doc "A locator lets a prefix anchor name its own enclosing line"
  (let ((hunks
         (niwa-tool-patch-parse-update-lines
          '("@@" " def load_config(path):"
            "@@ def load_config" " old" "-value = 1" "+value = 2")
          2)))
    (should (equal "def load_config(path):\nold\nvalue = 2\n"
                   (niwa-tool-patch-apply-hunks
                    "def load_config(path):\nold\nvalue = 1\n"
                    hunks "config.py")))))

(niwa-deftest niwa-tool-patch-hunks-from-content ()
  ,test
  (test)
  :doc "Returns nil for identical contents"
  (should-not (niwa-tool-patch-hunks-from-content "a\nb\n" "a\nb\n"))

  :doc "Derives one hunk per changed region, each with its own context"
  (let* ((baseline (concat (string-join
                            (list "one" "two" "three" "four" "five" "six"
                                  "seven" "eight" "nine" "ten" "eleven"
                                  "twelve")
                            "\n")
                           "\n"))
         (content (replace-regexp-in-string
                   "^two$" "TWO"
                   (replace-regexp-in-string "^eleven$" "ELEVEN" baseline)))
         (hunks (niwa-tool-patch-hunks-from-content baseline content)))
    (should (= 2 (length hunks)))
    (should (member "-two" (plist-get (nth 0 hunks) :diff-lines)))
    (should (member "+TWO" (plist-get (nth 0 hunks) :diff-lines)))
    (should (member "+ELEVEN" (plist-get (nth 1 hunks) :diff-lines)))
    (should (plist-get (nth 0 hunks) :selected))
    ;; Three lines of context on each side, so neither hunk claims the
    ;; whole file and both stay independently selectable.
    (should (member " five" (plist-get (nth 0 hunks) :diff-lines)))
    (should-not (member " eleven" (plist-get (nth 0 hunks) :diff-lines)))
    (should (equal content
                   (niwa-tool-patch-apply-hunks
                    baseline hunks "file.txt"))))

  :doc "Derives a change on the first line"
  (let ((hunks (niwa-tool-patch-hunks-from-content
                "first\nsecond\n" "FIRST\nsecond\n")))
    (should (= 1 (length hunks)))
    (should (equal '("first" "second")
                   (plist-get (car hunks) :old-lines)))
    (should (equal '("FIRST" "second")
                   (plist-get (car hunks) :new-lines))))

  :doc "Derives an appended trailing line"
  (let ((hunks (niwa-tool-patch-hunks-from-content
                "first\n" "first\nsecond\n")))
    (should (= 1 (length hunks)))
    (should (equal '("first") (plist-get (car hunks) :old-lines)))
    (should (equal '("first" "second") (plist-get (car hunks) :new-lines))))

  :doc "Derives a deleted trailing line without a no-newline marker"
  (let ((hunks (niwa-tool-patch-hunks-from-content
                "first\nsecond\n" "first\n")))
    (should (= 1 (length hunks)))
    (should (equal '("first" "second") (plist-get (car hunks) :old-lines)))
    (should (equal '("first") (plist-get (car hunks) :new-lines))))

  :doc "Derived hunks reproduce the content they were derived from"
  (let* ((baseline (string-join
                    (list "alpha" "beta" "gamma" "delta" "epsilon" "zeta"
                          "eta" "theta" "iota" "kappa")
                    "\n"))
         (content (string-join
                   (list "ALPHA" "beta" "gamma" "delta" "epsilon" "zeta"
                         "eta" "theta" "iota" "KAPPA" "extra")
                   "\n"))
         (hunks (niwa-tool-patch-hunks-from-content
                 (concat baseline "\n") (concat content "\n"))))
    (should (equal (concat content "\n")
                   (niwa-tool-patch-apply-hunks
                    (concat baseline "\n") hunks "file.txt"))))

  :doc "Normalizes CRLF input and reapplies with CRLF preserved"
  (let ((hunks (niwa-tool-patch-hunks-from-content
                "one\r\ntwo\r\n" "one\r\nTWO\r\n")))
    (should (= 1 (length hunks)))
    (should (equal '("one" "two") (plist-get (car hunks) :old-lines)))
    (should (equal "one\r\nTWO\r\n"
                   (niwa-tool-patch-apply-hunks
                    "one\r\ntwo\r\n" hunks "file.txt"))))

  :doc "Ignores a trailing-newline-only difference in either direction"
  (should-not (niwa-tool-patch-hunks-from-content "a\nb\n" "a\nb"))
  (should-not (niwa-tool-patch-hunks-from-content "a\nb" "a\nb\n"))

  :doc "Derives no phantom last-line hunk from an unterminated baseline"
  (let* ((baseline (string-join
                    (list "l1" "l2" "l3" "l4" "l5" "l6" "l7" "l8" "l9" "l10")
                    "\n"))
         (content (replace-regexp-in-string "\\`l1$" "L1" baseline))
         (hunks (niwa-tool-patch-hunks-from-content baseline content)))
    (should (= 1 (length hunks)))
    (should-not (member "-l10" (plist-get (car hunks) :diff-lines))))

  :doc "Derives a whole-file replacement"
  (let ((hunks (niwa-tool-patch-hunks-from-content "old\n" "new\n")))
    (should (= 1 (length hunks)))
    (should (equal "new\n"
                   (niwa-tool-patch-apply-hunks
                    "old\n" hunks "file.txt")))))

(niwa-deftest niwa-tool-patch-apply/refuses-unsaved
  (:doc "Refuses to overwrite an affected buffer with unsaved edits")
  ,test
  (test)
  (let* ((root (file-name-as-directory
                (make-temp-file "niwa-patch-buffer-" t)))
         (path (file-name-concat root "visited.txt"))
         (alias (file-name-concat root "visited-alias.txt"))
         buffer
         continued)
    (unwind-protect
        (progn
          (with-temp-file path (insert "disk\n"))
          (make-symbolic-link path alias)
          (setq buffer (find-file-noselect alias))
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert "unsaved\n"))
          (should-error
           (niwa-tool-patch-apply
            nil (list (list :action 'write :path path :content "patch\n"))
            (lambda () (setq continued t)))
           :type 'error)
          (should-not continued)
          (should (buffer-modified-p buffer))
          (should (equal "disk\n"
                         (niwa-test--read-file path)))
          (should (equal "disk\nunsaved\n"
                         (with-current-buffer buffer (buffer-string)))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-buffer-modified-p nil))
        (kill-buffer buffer))
      (when (file-directory-p root) (delete-directory root t)))))

(niwa-deftest niwa-tool-patch-register
  (:before-each (niwa-tool-clear-registry)
   :after-each (niwa-tool-clear-registry))
  ,test
  (test)
  :doc "Registers one reviewed aggregate edit tool with one patch argument"
  (progn
    (niwa-tool-patch-register)
    (let ((tool (niwa-tool-get "ApplyPatch")))
      (should tool)
      (should (equal '(edit reviewed-edit) (niwa-tool-groups tool)))
      (should (niwa-tool-snapshot-p tool))
      (should (functionp (niwa-tool-get-paths tool)))
      (should (equal '((patch string :required
                              "A complete *** Begin Patch / *** End Patch patch."))
                     (niwa-tool-args tool)))
      (dolist (text '("Standalone or sticky Plan mode"
                      "permits only session-owned `work://` descendants"
                      "Shared, memory, ordinary, malformed, and root-only endpoints are denied before materialization"
                      "Requests carrying inherited Plan limits follow the same rule"
                      "Outside Plan mode, resource and ordinary paths may share one atomic proposal"))
        (should (string-match-p
                 (mapconcat #'regexp-quote (split-string text " " t)
                            "[[:space:]]+")
                 (niwa-tool-prompt tool)))))))

;; Small direct suites keep each helper's contract visible; the integration
;; suites above exercise their composition and filesystem behavior.

(niwa-deftest niwa-tool-patch--operation-marker-p
  (:doc "Recognizes file-operation markers only") ,test (test)
  (should (niwa-tool-patch--operation-marker-p "*** Add File: a"))
  (should-not (niwa-tool-patch--operation-marker-p "@@ context")))

(niwa-deftest niwa-tool-patch--marker-path
  (:doc "Extracts and requires a marker path") ,test (test)
  (should (equal "a" (niwa-tool-patch--marker-path
                      "*** Add File: a" "*** Add File: " 1)))
  (should-error (niwa-tool-patch--marker-path
                 "*** Add File: " "*** Add File: " 1)))

(niwa-deftest niwa-tool-patch-parse-update-lines
  (:doc "Parses selected update hunks") ,test (test)
  (let ((hunk (car (niwa-tool-patch-parse-update-lines
                    '("@@ here" "-old" "+new") 2))))
    (should (equal '("old") (plist-get hunk :old-lines)))
    (should (equal '("new") (plist-get hunk :new-lines)))
    (should (plist-get hunk :selected)))

  :doc "treats a bare empty line as an empty context line"
  (let ((hunk (car (niwa-tool-patch-parse-update-lines
                    '("@@" "-old" "" "+new") 2))))
    (should (equal '("old" "") (plist-get hunk :old-lines)))
    (should (equal '("" "new") (plist-get hunk :new-lines)))
    (should (equal '("-old" " " "+new") (plist-get hunk :diff-lines))))

  :doc "matches @@ and End of File markers despite trailing whitespace"
  (let ((hunk (car (niwa-tool-patch-parse-update-lines
                    '("@@ here  " "-old" "+new" "*** End of File  ") 2))))
    (should (equal "here" (plist-get hunk :context)))
    (should (plist-get hunk :eof)))

  :doc "reads a bare positive integer anchor as a line hint"
  (let ((hunk (car (niwa-tool-patch-parse-update-lines
                    '("@@ 412" "-old" "+new") 2))))
    (should (= 412 (plist-get hunk :line-hint)))
    (should-not (plist-get hunk :context)))

  :doc "keeps a textual anchor as context with no line hint"
  (let ((hunk (car (niwa-tool-patch-parse-update-lines
                    '("@@ here" "-old" "+new") 2))))
    (should (equal "here" (plist-get hunk :context)))
    (should-not (plist-get hunk :line-hint)))

  :doc "leaves zero and zero-padded anchors as context"
  (dolist (anchor '("0" "007"))
    (let ((hunk (car (niwa-tool-patch-parse-update-lines
                      (list (concat "@@ " anchor) "-old" "+new") 2))))
      (should (equal anchor (plist-get hunk :context)))
      (should-not (plist-get hunk :line-hint))))

  :doc "accepts a context-only hunk as a locator"
  (let ((hunk (car (niwa-tool-patch-parse-update-lines
                    '("@@" " kept" " also kept") 2))))
    (should (equal '("kept" "also kept") (plist-get hunk :old-lines)))
    (should (equal (plist-get hunk :old-lines) (plist-get hunk :new-lines)))
    (should-not (niwa-tool-patch-hunk-changes-p hunk)))

  :doc "parses a locator section before an anchored change"
  (let ((hunks (niwa-tool-patch-parse-update-lines
                '("@@ outer" " scope line" "@@" " context" "+new") 2)))
    (should (= 2 (length hunks)))
    (should-not (niwa-tool-patch-hunk-changes-p (car hunks)))
    (should (niwa-tool-patch-hunk-changes-p (cadr hunks))))

  :doc "records each hunk's @@ marker line as :at-line"
  (let ((hunks (niwa-tool-patch-parse-update-lines
                '("@@" "-old" "+new" "@@" " kept") 3)))
    (should (equal '(3 6) (mapcar (lambda (hunk) (plist-get hunk :at-line))
                                  hunks))))

  :doc "rejects a hunk with no lines at its own @@ line"
  (let ((err (should-error (niwa-tool-patch-parse-update-lines
                            '("@@" "-old" "+new" "@@" "@@" " kept") 3)
                           :type 'error)))
    (should (string-match-p "line 6.*hunk is empty"
                            (error-message-string err)))))

(niwa-deftest niwa-tool-patch-hunk-changes-p
  (:doc "Distinguishes changing hunks from locators") ,test (test)
  (should (niwa-tool-patch-hunk-changes-p '(:diff-lines ("-old" "+new"))))
  (should (niwa-tool-patch-hunk-changes-p '(:diff-lines (" kept" "+new"))))
  (should-not (niwa-tool-patch-hunk-changes-p '(:diff-lines (" kept" " ")))))

(niwa-deftest niwa-tool-patch-parse
  (:doc "Parses a complete multi-operation envelope") ,test (test)
  (let ((proposal (niwa-tool-patch-parse
                   "*** Begin Patch\n*** Add File: a\n+x\n*** Delete File: b\n*** End Patch"
                   "/tmp/")))
    (should (equal '(add delete)
                   (mapcar (lambda (op) (plist-get op :kind))
                           (plist-get proposal :operations)))))

  :doc "tolerates whitespace around markers and an Environment ID line"
  (let ((proposal (niwa-tool-patch-parse
                   (concat "  *** Begin Patch  \n"
                           "*** Environment ID: env-42\n"
                           "  *** Update File: a  \n"
                           "@@\n"
                           "-old\n"
                           "+new\n"
                           "*** End Patch  ")
                   "/tmp/")))
    (should (equal '(update)
                   (mapcar (lambda (op) (plist-get op :kind))
                           (plist-get proposal :operations))))
    (should (equal "a" (plist-get (car (plist-get proposal :operations))
                                  :rel-path))))

  :doc "an indented header inside an update body stays a context line"
  (let* ((proposal (niwa-tool-patch-parse
                    (concat "*** Begin Patch\n"
                            "*** Update File: a\n"
                            "@@\n"
                            "-old\n"
                            "+new\n"
                            " *** Delete File: b\n"
                            "*** End Patch")
                    "/tmp/"))
         (hunk (car (plist-get (car (plist-get proposal :operations))
                               :hunks))))
    (should (= 1 (length (plist-get proposal :operations))))
    (should (member "*** Delete File: b" (plist-get hunk :old-lines))))

  :doc "interprets absolute patch paths in the remote target domain"
  (let* ((proposal
          (niwa-tool-patch-parse
           (concat "*** Begin Patch\n"
                   "*** Update File: /etc/app.conf\n"
                   "*** Move to: /var/lib/app.conf\n"
                   "@@\n-old\n+new\n"
                   "*** End Patch")
           "/ssh:user@host:/srv/project/"))
         (operation (car (plist-get proposal :operations))))
    (should (equal "/ssh:user@host:/etc/app.conf"
                   (plist-get operation :path)))
    (should (equal "/ssh:user@host:/var/lib/app.conf"
                   (plist-get operation :move-path))))

  :doc "rejects patch paths that explicitly name another target"
  (should-error
   (niwa-tool-patch-parse
    (concat "*** Begin Patch\n"
            "*** Add File: /ssh:user@other:/tmp/a\n"
            "+x\n"
            "*** End Patch")
    "/ssh:user@host:/srv/project/")
   :type 'niwa-execution-target-error)

  :doc "accepts locator hunks in an update that also changes something"
  (let* ((proposal (niwa-tool-patch-parse
                    (concat "*** Begin Patch\n"
                            "*** Update File: a\n"
                            "@@\n"
                            " scope line\n"
                            "@@\n"
                            "-old\n"
                            "+new\n"
                            "*** End Patch")
                    "/tmp/"))
         (hunks (plist-get (car (plist-get proposal :operations)) :hunks)))
    (should (= 2 (length hunks))))

  :doc "rejects an update whose hunks are all locators"
  (let ((err (should-error
              (niwa-tool-patch-parse
               (concat "*** Begin Patch\n"
                       "*** Update File: a\n"
                       "@@\n"
                       " only context\n"
                       "*** End Patch")
               "/tmp/")
              :type 'error)))
    (should (string-match-p "Update for a has no changes"
                            (error-message-string err))))

  :doc "a move with locator hunks alone stays a real operation"
  (let ((proposal (niwa-tool-patch-parse
                   (concat "*** Begin Patch\n"
                           "*** Update File: a\n"
                           "*** Move to: b\n"
                           "@@\n"
                           " kept\n"
                           "*** End Patch")
                   "/tmp/")))
    (should (eq 'move (plist-get (car (plist-get proposal :operations))
                                 :kind)))))

(niwa-deftest niwa-tool-patch--read-file
  (:doc "Reads regular files and rejects missing paths") ,test (test)
  (let ((path (make-temp-file "niwa-patch-read-")))
    (unwind-protect
        (progn (with-temp-file path (insert "text"))
               (should (equal "text" (niwa-tool-patch--read-file path))))
      (delete-file path))))

(niwa-deftest niwa-tool-patch--lines
  (:doc "Pairs logical lines with their actual endings") ,test (test)
  (should (equal '(("a\rb" . "\r\n") ("")) (niwa-tool-patch--lines "a\rb\r\n"))))

(niwa-deftest niwa-tool-patch-content-lines
  (:doc "Removes one file terminator while retaining trailing blank lines")
  ,test (test)
  (should (equal '("a" "")
                 (niwa-tool-patch-content-lines "a\n\n")))
  :doc "Represents an empty file with no changed lines"
  (should-not (niwa-tool-patch-content-lines "")))

(niwa-deftest niwa-tool-patch--hunk-replacement
  (:doc "Takes context from the file and additions from the patch")
  ,test (test)
  (should (equal '(("A" . "\r\n") ("new" . "\n"))
                 (niwa-tool-patch--hunk-replacement
                  (niwa-tool-patch--lines "A\r\nold\ntail") 0
                  '(:diff-lines (" A" "-old" "+new")) "\r\n"))))

(niwa-deftest niwa-tool-patch--section-label
  (:doc "Returns the closest unindented line at or above the start")
  ,test (test)
  (should (equal "(defun foo ()"
                 (niwa-tool-patch--section-label
                  '("(defun foo ()" "  body" "  more") 2)))
  (should-not (niwa-tool-patch--section-label '("  a" "  b") 1))
  :doc "skips closing brackets and truncates long labels"
  (should (equal "top"
                 (niwa-tool-patch--section-label '("top" ")" "  x") 2)))
  (should (= 48 (length (niwa-tool-patch--section-label
                         (list (make-string 80 ?y)) 0)))))

(niwa-deftest niwa-tool-patch-match-pass-description
  (:doc "Describes fuzzy passes and stays nil for exact matches")
  ,test (test)
  (should (equal "ignoring surrounding whitespace"
                 (niwa-tool-patch-match-pass-description 'whitespace)))
  (should (equal "ignoring trailing whitespace"
                 (niwa-tool-patch-match-pass-description
                  'trailing-whitespace)))
  (should (equal "normalizing typographic punctuation"
                 (niwa-tool-patch-match-pass-description 'punctuation)))
  (should-not (niwa-tool-patch-match-pass-description nil)))

(niwa-deftest niwa-tool-patch--fontify-diff
  (:doc "Fontifies diff lines whole-line and marks them linkify-exempt")
  ,test (test)
  ;; The renderer runs whenever a patch applies; the review UI it shares
  ;; these faces with loads only when the user actually reviews one.
  (should (facep 'niwa-patch-review-added))
  (should (facep 'niwa-patch-review-removed))
  (let ((body (niwa-tool-patch--fontify-diff "@@ ctx\n-x\n+y\n z")))
    (should (equal "@@ ctx\n-x\n+y\n z" body))
    (should (eq 'diff-hunk-header (get-text-property 0 'font-lock-face body)))
    (let ((removed (string-search "-x" body))
          (added (string-search "+y" body)))
      (should (eq 'niwa-patch-review-removed
                  (get-text-property removed 'font-lock-face body)))
      ;; The newline after a removed line carries the face for :extend.
      (should (eq 'niwa-patch-review-removed
                  (get-text-property (+ removed 2) 'font-lock-face body)))
      (should (eq 'niwa-patch-review-added
                  (get-text-property added 'font-lock-face body))))
    (should (get-text-property 0 'niwa-view-no-linkify body))
    (should (get-text-property (string-search " z" body)
                               'niwa-view-no-linkify body))))

(niwa-deftest niwa-tool-patch-kind-face
  (:doc "Maps operation kinds to status-letter faces")
  ,test (test)
  (should (eq 'success (niwa-tool-patch-kind-face 'add)))
  (should (eq 'error (niwa-tool-patch-kind-face 'delete)))
  (should (eq 'font-lock-keyword-face
              (niwa-tool-patch-kind-face 'move)))
  (should (eq 'font-lock-function-name-face
              (niwa-tool-patch-kind-face 'update))))

(niwa-deftest niwa-tool-patch--sequence-match-p
  (:doc "Matches pre-normalized pattern lines against the line vector")
  ,test (test)
  (should (niwa-tool-patch--sequence-match-p ["a" "b"] '("b") 1))
  (should-not (niwa-tool-patch--sequence-match-p ["a" "b"] '("a") 1))
  :doc "rejects a pattern running past the end of the lines"
  (should-not (niwa-tool-patch--sequence-match-p ["a"] '("a" "b") 0)))

(niwa-deftest niwa-tool-patch--normalize-line
  (:doc "Folds typographic punctuation to ASCII and trims") ,test (test)
  (should (equal "a - \"b\" 'c'"
                 (niwa-tool-patch--normalize-line
                  " a \u2013 \u201Cb\u201D \u2018c\u2019 ")))
  (should (equal "plain" (niwa-tool-patch--normalize-line "plain"))))

(niwa-deftest niwa-tool-patch--candidate-starts
  (:doc "Returns every valid hunk start") ,test (test)
  (should (equal '(0 2) (niwa-tool-patch--candidate-starts
                         '("a" "b" "a") '(:old-lines ("a")) #'identity)))
  :doc "ignore-eof drops the end-of-file anchor"
  (should-not (niwa-tool-patch--candidate-starts
               '("a" "b" "") '(:old-lines ("a") :eof t) #'identity))
  (should (equal '(0) (niwa-tool-patch--candidate-starts
                       '("a" "b" "") '(:old-lines ("a") :eof t)
                       #'identity t)))

  :doc "a context anchor matches a full line or its prefix"
  (should (equal '(2) (niwa-tool-patch--candidate-starts
                       '("body" "def load_config(path):" "body")
                       '(:old-lines ("body") :context "def load_config")
                       #'identity)))
  (should (equal '(2) (niwa-tool-patch--candidate-starts
                       '("body" "def load_config(path):" "body")
                       '(:old-lines ("body")
                         :context "def load_config(path):")
                       #'identity)))

  :doc "a context anchor gates strictly before the hunk"
  (should-not (niwa-tool-patch--candidate-starts
               '("def load_config(path):" "tail")
               '(:old-lines ("def load_config(path):")
                 :context "def load_config")
               #'identity)))

(niwa-deftest niwa-tool-patch--match-start
  (:doc "Requires one unambiguous hunk start") ,test (test)
  (should (= 1 (niwa-tool-patch--match-start
                '("a" "b") '(:old-lines ("b")) "x")))
  :doc "records the winning pass on the hunk"
  (let ((exact (list :old-lines '("a")))
        (fuzzy (list :old-lines '("indented"))))
    (niwa-tool-patch--match-start '("a" "b") exact "x")
    (should-not (plist-get exact :match-pass))
    (niwa-tool-patch--match-start '("  indented  ") fuzzy "x")
    (should (eq 'whitespace (plist-get fuzzy :match-pass))))
  :doc "falls through surrounding-whitespace and punctuation passes"
  (should (= 0 (niwa-tool-patch--match-start
                '("  indented  ") '(:old-lines ("indented")) "x")))
  (should (= 0 (niwa-tool-patch--match-start
                '("say \u201Chi\u201D \u2013 loud")
                '(:old-lines ("say \"hi\" - loud")) "x")))
  :doc "an end-of-file hunk anchors to the end, then retries unanchored"
  (should (= 2 (niwa-tool-patch--match-start
                '("a" "b" "a" "") '(:old-lines ("a") :eof t) "x")))
  (should (= 0 (niwa-tool-patch--match-start
                '("a" "b" "c" "") '(:old-lines ("a") :eof t) "x")))
  :doc "hunk order disambiguates an otherwise ambiguous match"
  (should-error (niwa-tool-patch--match-start
                 '("x" "a" "x") '(:old-lines ("x")) "p")
                :type 'error)
  (should (= 2 (niwa-tool-patch--match-start
                '("x" "a" "x") '(:old-lines ("x")) "p" 2)))
  (should-error (niwa-tool-patch--match-start
                 '("x" "a" "x" "b" "x") '(:old-lines ("x")) "p" 1)
                :type 'error)

  :doc "a line hint settles an otherwise ambiguous match"
  (should (= 2 (niwa-tool-patch--match-start
                '("x" "a" "x") '(:old-lines ("x") :line-hint 3) "p")))
  (should (= 0 (niwa-tool-patch--match-start
                '("x" "a" "x") '(:old-lines ("x") :line-hint 1) "p")))

  :doc "equidistant candidates stay ambiguous"
  (should-error (niwa-tool-patch--match-start
                 '("x" "a" "x") '(:old-lines ("x") :line-hint 2) "p")
                :type 'error)

  :doc "match failures name the offending hunk's patch line"
  (dolist (case '((("a") (:old-lines ("missing") :at-line 17)
                   "line 17.*does not match")
                  (("x" "a" "x") (:old-lines ("x") :at-line 23)
                   "line 23.*ambiguous")))
    (let ((err (should-error
                (niwa-tool-patch--match-start
                 (nth 0 case) (nth 1 case) "p")
                :type 'error)))
      (should (string-match-p (nth 2 case) (error-message-string err)))))

  :doc "a stale hint never rejects an unambiguous match"
  (should (= 1 (niwa-tool-patch--match-start
                '("a" "b") '(:old-lines ("b") :line-hint 900) "x")))

  :doc "hunk order narrows first, the hint settles what it leaves"
  (should (= 4 (niwa-tool-patch--match-start
                '("x" "a" "x" "b" "x") '(:old-lines ("x") :line-hint 5) "p" 1)))

  :doc "hunk order excludes earlier matches before applying the hint"
  (should (= 2 (niwa-tool-patch--match-start
                '("x" "a" "x" "b" "x") '(:old-lines ("x") :line-hint 1) "p" 1))))

(niwa-deftest niwa-tool-patch--affected-paths
  (:doc "Includes both source and move destination") ,test (test)
  (should (equal '("a" "b")
                 (niwa-tool-patch--affected-paths
                  '(:path "a" :move-path "b")))))

(niwa-deftest niwa-tool-patch--path-state
  (:doc "Distinguishes missing and regular-file state") ,test (test)
  (let ((path (make-temp-file "niwa-patch-state-")))
    (unwind-protect
        (progn (with-temp-file path (insert "x"))
               (should (equal '(file "x")
                              (niwa-tool-patch--path-state path))))
      (delete-file path))))

(niwa-deftest niwa-tool-patch-capture-baseline
  (:doc "Stores source content on the proposal and operation") ,test (test)
  (let ((path (make-temp-file "niwa-patch-baseline-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "x"))
          (let* ((operation (list :kind 'delete :path path :rel-path "x"))
                 (proposal (list :operations (list operation))))
            (niwa-tool-patch-capture-baseline proposal)
            (should (equal "x" (plist-get operation :baseline-content)))
            (should (= 1 (length (plist-get proposal :baseline))))))
      (delete-file path))))

(niwa-deftest niwa-tool-patch--selected-paths
  (:doc "Returns paths only for selected changes") ,test (test)
  (should (equal '("a")
                 (niwa-tool-patch--selected-paths
                  '(:operations ((:kind add :path "a" :selected t)
                                 (:kind delete :path "b" :selected nil))))))
  :doc "Ignores a selected locator when every changing hunk is rejected"
  (should-not
   (niwa-tool-patch--selected-paths
    '(:operations
      ((:kind update :path "a"
        :hunks ((:selected t :diff-lines (" context"))
                (:selected nil :diff-lines ("-old" "+new")))))))))

(niwa-deftest niwa-tool-patch-assert-baseline
  (:doc "Accepts an unchanged selected baseline") ,test (test)
  (let ((path (make-temp-file "niwa-patch-assert-")))
    (unwind-protect
        (progn
          (with-temp-file path (insert "x"))
          (let ((proposal (list :operations
                                (list (list :kind 'delete :path path
                                            :rel-path "x" :selected t)))))
            (niwa-tool-patch-capture-baseline proposal)
            (should-not (niwa-tool-patch-assert-baseline proposal))))
      (delete-file path))))

(niwa-deftest niwa-tool-patch--validate-distinct-paths
  (:doc "Rejects repeated affected paths") ,test (test)
  (should-error
   (niwa-tool-patch--validate-distinct-paths
    '((:path "a") (:path "a")))))

(niwa-deftest niwa-tool-patch-planned-changes
  (:doc "Plans selected Add writes without touching disk") ,test (test)
  (let ((path (make-temp-name
               (expand-file-name "niwa-patch-plan-" temporary-file-directory))))
    (should (equal (list (list :action 'write :path path :content "x\n"))
                   (niwa-tool-patch-planned-changes
                    (list :operations
                          (list (list :kind 'add :path path :content "x\n"
                                      :selected t)))))))
  :doc "Preserves a changed file's non-default coding in final bytes"
  (let* ((path (make-temp-file "niwa-patch-plan-coding-" nil ".el"))
         (destination (concat path ".moved"))
         (original
          ";; -*- coding: iso-latin-1 -*-\r\nold: \u00e4\nkeep: \r\u00f6\n")
         (expected
          ";; -*- coding: iso-latin-1 -*-\r\nnew: \u00e4\nkeep: \r\u00f6\n"))
    (unwind-protect
        (progn
          (let ((coding-system-for-write 'iso-latin-1))
            (with-temp-file path (insert original)))
          (let* ((hunk (list :selected t
                             :old-lines '("old: \u00e4")
                             :diff-lines
                             '("-old: \u00e4" "+new: \u00e4")))
                 (operation (list :kind 'update :path path
                                  :hunks (list hunk)))
                 (changes
                  (niwa-tool-patch-planned-changes
                   (list :operations (list operation)))))
            (niwa-change-commit changes)
            (should
             (equal (encode-coding-string expected 'iso-latin-1)
                    (test-niwa-tool-patch--read-bytes path))))
            (setq changes
                  (niwa-tool-patch-planned-changes
                   (list :operations
                         (list (list :kind 'move :path path
                                     :move-path destination :selected t)))))
            (niwa-change-commit changes)
            (should-not (file-exists-p path))
            (should
             (equal (encode-coding-string expected 'iso-latin-1)
                    (test-niwa-tool-patch--read-bytes destination)))))
      (when (file-exists-p path) (delete-file path))
      (when (file-exists-p destination) (delete-file destination))))

(niwa-deftest niwa-tool-patch-apply
  (:doc "Commits changes before invoking its continuation") ,test (test)
  (let ((path (make-temp-file "niwa-patch-apply-")) called)
    (unwind-protect
        (progn
          (niwa-tool-patch-apply
           nil (list (list :action 'write :path path :content "x"))
           (lambda () (setq called t)))
          (should called)
          (should (equal "x" (niwa-tool-patch--read-file path))))
      (delete-file path)))

  :doc "A change hook on A that edits B during A's sync rolls back and keeps B's draft"
  (let* ((root (make-temp-file "niwa-patch-sync-guard-" t))
         (paths (mapcar (lambda (leaf) (file-name-concat root leaf)) '("a" "b")))
         buffers typed)
    (unwind-protect
        (progn
          (dolist (path paths) (write-region "old\n" nil path nil 'silent))
          (setq buffers (mapcar #'find-file-noselect paths))
          (with-current-buffer (car buffers)
            (add-hook 'after-change-functions
                      (lambda (&rest _)
                        (unless typed
                          (setq typed t)
                          (with-current-buffer (cadr buffers)
                            (goto-char (point-max))
                            (insert "draft\n"))))
                      nil t))
          (should-error
           (niwa-tool-patch-apply
            nil (mapcar (lambda (path) (list :action 'write :path path :content "new\n")) paths)
            #'ignore))
          (should typed)
          (dolist (path paths) (should (equal "old\n" (niwa-tool-patch--read-file path))))
          (with-current-buffer (car buffers) (should (equal "old\n" (buffer-string))))
          (with-current-buffer (cadr buffers)
            (should (buffer-modified-p))
            (should (equal "old\ndraft\n" (buffer-string)))))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory root t)))

  :doc "Records store writes as artifact versions of the attached session"
  (let* ((root (file-name-as-directory (make-temp-file "niwa-patch-store-" t)))
         (workspace (niwa-workspace--create :type 'project :id "w"
                                               :root root :name "w"))
         (path (file-name-concat root ".niwa/artifacts/flow/index.html"))
         (one (niwa-session--create :workspace workspace :session-id "one"))
         (two (niwa-session--create :workspace workspace :session-id "two"))
         (data (generate-new-buffer " *patch-store-data*")))
    (unwind-protect
        (cl-flet ((write (session content)
                    (with-current-buffer data
                      (setq-local niwa--session session))
                    (niwa-tool-patch-apply
                     data (list (list :action 'write :path path :content content))
                     #'ignore)))
          (write one "<p>one</p>\n")
          (write two "<p>two</p>\n")
          (should (equal '("flow") (niwa-session-attached-artifacts one)))
          (should (equal '("flow") (niwa-session-attached-artifacts two)))
          (should (equal '("one" "two")
                         (mapcar (lambda (row) (plist-get row :session))
                                 (niwa-artifact-store-versions workspace "flow"))))
          ;; A hunk planned against the copy session one read no longer
          ;; matches after session two's write: the model rereads instead
          ;; of overwriting.
          (should-error
           (niwa-tool-patch-apply-hunks
            (niwa-tool-patch--read-file path)
            '((:old-lines ("<p>one</p>") :new-lines ("<p>three</p>") :selected t
               :diff-lines ("-<p>one</p>" "+<p>three</p>")))
            path)))
      (kill-buffer data)
      (delete-directory root t))))

(niwa-deftest niwa-tool-patch--selected-file-data
  (:doc "Builds a selected-only Update file block") ,test (test)
  (let ((file (niwa-tool-patch--selected-file-data
               '(:kind update :rel-path "a"
                 :hunks ((:selected t :diff-lines ("-x" "+y"))
                         (:selected nil :diff-lines ("-z" "+q")))))))
    (should (equal "a" (plist-get file :path)))
    (should (= 1 (plist-get file :added)))
    (should (= 1 (plist-get file :deleted)))
    (should (string-search "+y" (plist-get file :diff)))
    (should-not (string-search "+q" (plist-get file :diff))))
  :doc "Builds Add and Delete file blocks from their complete contents"
  (let ((added (niwa-tool-patch--selected-file-data
                '(:kind add :rel-path "a" :selected t :content "x\n\n")))
        (deleted (niwa-tool-patch--selected-file-data
                  '(:kind delete :rel-path "b" :selected t
                    :baseline-content "old\n\n"))))
    (should (equal "+x\n+" (plist-get added :diff)))
    (should (= 2 (plist-get added :added)))
    (should (equal "-old\n-" (plist-get deleted :diff)))
    (should (= 2 (plist-get deleted :deleted))))
  :doc "Keeps a selected move even when it has no content hunks"
  (let ((file (niwa-tool-patch--selected-file-data
               '(:kind move :rel-path "a" :move-rel-path "b"
                 :selected t :hunks nil))))
    (should (equal "b" (plist-get file :move-path)))
    (should (string-empty-p (plist-get file :diff))))
  :doc "Reports no deleted lines for an empty deleted file"
  (let ((file (niwa-tool-patch--selected-file-data
               '(:kind delete :rel-path "empty" :selected t
                 :baseline-content ""))))
    (should (= 0 (plist-get file :deleted)))
    (should (string-empty-p (plist-get file :diff))))
  :doc "Omits locator hunks from the persisted diff"
  (let ((file (niwa-tool-patch--selected-file-data
               '(:kind update :rel-path "a"
                 :hunks ((:selected t :context "scope"
                          :diff-lines (" locator line"))
                         (:selected t :diff-lines ("-x" "+y")))))))
    (should-not (string-search "locator line" (plist-get file :diff)))
    (should (string-search "+y" (plist-get file :diff)))))

(niwa-deftest niwa-tool-patch-result
  (:doc "Reports applied and rejected choices") ,test (test)
  (let ((text (plist-get
               (niwa-tool-patch-result
                '(:operations ((:kind update :rel-path "a"
                                :hunks ((:selected t :diff-lines ("-x" "+y"))
                                        (:selected nil :diff-lines ("-z" "+q"))))))
                '((:action write)))
               :result)))
    (should (string-search "Applied: a hunk 1" text))
    (should (string-search "Rejected: a hunk 2" text)))

  :doc "Marks a reviewed settlement in its render data"
  (let ((proposal '(:operations ((:kind update :rel-path "a"
                                  :hunks ((:selected t
                                           :diff-lines ("-x" "+y"))))))))
    (should (plist-get
             (plist-get (niwa-tool-patch-result proposal '((:action write)) t)
                        :render-data)
             :reviewed))
    (should-not (plist-get
                 (plist-get (niwa-tool-patch-result proposal '((:action write)))
                            :render-data)
                 :reviewed)))

  :doc "Reports fuzzy matches for applied hunks as result notes"
  (let ((result (niwa-tool-patch-result
                 '(:operations
                   ((:kind update :rel-path "a"
                     :hunks ((:selected t :match-pass whitespace
                              :diff-lines ("-x" "+y"))))))
                 '((:action write)))))
    (should (string-search
             "Fuzzy: a hunk 1 matched while ignoring surrounding whitespace"
             (plist-get result :result)))
    (should (member
             "Fuzzy: a hunk 1 matched while ignoring surrounding whitespace"
             (plist-get (plist-get result :render-data) :notes))))

  :doc "Reports only selected edited hunks as authoritative applied content"
  (let ((text
         (plist-get
          (niwa-tool-patch-result
           '(:operations
             ((:kind update :rel-path "a"
               :hunks ((:selected t :modified t
                        :diff-lines ("-applied" "+chosen"))
                       (:selected nil :modified t
                        :diff-lines ("-rejected" "+discarded"))))))
           '((:action write)))
          :result)))
    (should (string-search "+chosen" text))
    (should-not (string-search "+discarded" text)))

  :doc "Skips locator hunks so hunk numbers name changes only"
  (let ((text (plist-get
               (niwa-tool-patch-result
                '(:operations ((:kind update :rel-path "a"
                                :hunks ((:selected t :diff-lines (" locator"))
                                        (:selected t
                                         :diff-lines ("-x" "+y"))))))
                '((:action write)))
               :result)))
    (should (string-search "Applied: a hunk 1" text))
    (should-not (string-search "hunk 2" text))))

(niwa-deftest niwa-tool-patch-hunk-counts
  (:doc "Counts added and deleted diff lines in one hunk") ,test (test)
  (should (equal '(1 . 2)
                 (niwa-tool-patch-hunk-counts
                  '(:diff-lines ("-x" " keep" "-y" "+z"))))))

(niwa-deftest niwa-tool-patch-operation-stats
  (:doc "Counts selected Update hunks and their lines only") ,test (test)
  (should (equal '(:selected 1 :total 2 :added 1 :deleted 1)
                 (niwa-tool-patch-operation-stats
                  '(:kind update
                    :hunks ((:selected t :diff-lines ("-x" "+y"))
                            (:selected nil :diff-lines ("-a" "+b")))))))
  :doc "Reports the full size of whole-operation kinds"
  (should (equal '(:selected 0 :total 1 :added 2 :deleted 0)
                 (niwa-tool-patch-operation-stats
                  '(:kind add :selected nil :content "new\n\n"))))
  (should (equal '(:selected 1 :total 1 :added 0 :deleted 2)
                 (niwa-tool-patch-operation-stats
                  '(:kind delete :selected t :baseline-content "old\n\n"))))
  (should (equal '(:selected 1 :total 1 :added 1 :deleted 1)
                 (niwa-tool-patch-operation-stats
                  '(:kind move :selected t
                    :hunks ((:diff-lines ("-x" "+y")))))))
  :doc "Locator hunks count as neither total nor selected changes"
  (should (equal '(:selected 1 :total 1 :added 1 :deleted 1)
                 (niwa-tool-patch-operation-stats
                  '(:kind update
                    :hunks ((:selected t :diff-lines (" kept"))
                            (:selected t :diff-lines ("-x" "+y"))))))))

(niwa-deftest niwa-tool-patch-proposal-stats
  (:doc "Aggregates selected changes, files, and comments") ,test (test)
  (should (equal '(:selected 2 :total 4 :added 2 :deleted 1
                   :files-selected 2 :comments 2)
                 (niwa-tool-patch-proposal-stats
                  '(:operations
                    ((:kind update
                      :hunks ((:selected t :diff-lines ("-x" "+y"))
                              (:selected nil :feedback "no"
                               :diff-lines ("-a" "+b"))))
                     (:kind add :selected t :content "new\n")
                     (:kind delete :selected nil :feedback "keep"
                      :baseline-content "old\n"))))))
  :doc "Counts whole-patch feedback as one comment"
  (should (equal '(:selected 0 :total 0 :added 0 :deleted 0
                   :files-selected 0 :comments 1)
                 (niwa-tool-patch-proposal-stats
                  '(:feedback "split it" :operations nil))))
  :doc "Counts changing Update hunks and whole operations, excluding locators"
  (dolist (entry
           '((2 :operations ((:kind update
                              :hunks ((:selected t :diff-lines ("-x" "+y"))
                                      (:selected nil :diff-lines ("-a" "+b"))))
                             (:kind move :selected t)))
             (1 :operations ((:kind update
                              :hunks ((:selected t :diff-lines (" kept"))
                                      (:selected t :diff-lines ("-x" "+y"))))))))
    (should (= (car entry)
               (plist-get (niwa-tool-patch-proposal-stats (cdr entry))
                          :selected)))))

(niwa-deftest niwa-tool-patch-status
  (:doc "Maps operation kinds to display status") ,test (test)
  (should (equal "R" (niwa-tool-patch-status '(:kind move)))))

(niwa-deftest niwa-tool-patch--effective-mode
  (:doc "Direct edit authority overrides the ambient mode") ,test (test)
  (let ((niwa-pipeline--auto-apply-edit-p t)
        (niwa-permission-mode 'ask))
    (should (eq 'edits (niwa-tool-patch--effective-mode))))
  :doc "One-shot requests force review over direct edit authority"
  (let ((niwa--current-request
         (niwa-request--create :one-shot-mutations-p t))
        (niwa-pipeline--auto-apply-edit-p t)
        (niwa-permission-mode 'edits))
    (should (eq 'ask (niwa-tool-patch--effective-mode))))
  :doc "Full Access bypasses side-conversation patch review"
  (let ((niwa--current-request (niwa-request--create :one-shot-mutations-p t))
        (niwa-permission-mode 'full-auto))
    (should (eq 'full-auto (niwa-tool-patch--effective-mode)))))

(niwa-deftest niwa-tool-patch--get-paths
  (:doc "Extracts every source and destination path") ,test (test)
  (let ((default-directory temporary-file-directory))
    (should (= 2 (length
                  (niwa-tool-patch--get-paths
                   '(:patch "*** Begin Patch\n*** Update File: a\n*** Move to: b\n*** End Patch"))))))
  :doc "Omits local operands but keeps ordinary paths in mixed proposals"
  (let ((default-directory temporary-file-directory))
    (should (equal (list (expand-file-name "ordinary.txt"))
                   (niwa-tool-patch--get-paths
                    '(:patch "*** Begin Patch\n*** Add File: work://scratch.txt\n+x\n*** Delete File: ordinary.txt\n*** End Patch"))))))

(niwa-deftest niwa-tool-patch--render
  (:doc "Produces one collapsible aggregate with per-file diff blocks") ,test (test)
  (let ((rendered (niwa-tool-patch--render
                   "ApplyPatch" nil nil
                   '(:kind patch
                     :applied 3 :total 3 :comments 0
                     :files ((:path "a" :kind update :added 1 :deleted 1
                              :diff "@@\n-x\n+y")
                             (:path "b" :kind add :added 2 :deleted 0
                              :diff "+one\n+two"))))))
    (should (equal "ApplyPatch: 2 files · 3 changes (+3 -1)"
                   (plist-get rendered :header)))
    (should (string-search "M a · +1 −1" (plist-get rendered :body)))
    (should (string-search "A b · +2 −0" (plist-get rendered :body)))
    (should (string-search "+two" (plist-get rendered :body)))
    (let ((body (plist-get rendered :body)))
      ;; Diff content is linkify-exempt; the per-file header line is not.
      (should (get-text-property (string-search "+two" body)
                                 'niwa-view-no-linkify body))
      (should-not (get-text-property (string-search "M a" body)
                                     'niwa-view-no-linkify body)))
    (should (plist-get rendered :expandable-p)))

  :doc "Reports partial application and sent comments"
  (let ((rendered (niwa-tool-patch--render
                   "ApplyPatch" nil nil
                   '(:kind patch
                     :applied 1 :total 3 :comments 2
                     :files ((:path "a" :kind update :added 1 :deleted 1
                              :diff "@@\n-x\n+y"))))))
    (should (equal "ApplyPatch: 1 file · 1/3 changes · 2 comments sent (+1 -1)"
                   (plist-get rendered :header))))

  :doc "Labels a comment-only settlement as a revision request"
  (let ((rendered (niwa-tool-patch--render
                   "ApplyPatch" nil nil
                   '(:kind patch :applied 0 :total 2 :comments 1
                     :files nil
                     :notes ("Rejected: a.el hunk 1"
                             "Feedback: a.el hunk 1: why")))))
    (should (equal "ApplyPatch: revision requested · 1 comment sent"
                   (plist-get rendered :header)))
    (should (equal "Rejected: a.el hunk 1\nFeedback: a.el hunk 1: why"
                   (plist-get rendered :body))))

  :doc "A reviewed applied patch opens on a leading-changes preview"
  (let ((rendered (niwa-tool-patch--render
                   "ApplyPatch" nil nil
                   '(:kind patch :reviewed t
                     :applied 3 :total 3 :comments 0
                     :files ((:path "a" :kind update :added 2 :deleted 1
                              :diff "@@ one\n-x\n+y\n@@ two\n+z\n@@ three\n+w"))))))
    (should (null (plist-get rendered :initially-collapsed-p)))
    (should (plist-member rendered :initially-collapsed-p))
    (let ((preview (plist-get rendered :preview-body)))
      (should (string-search "M a · +2 −1" preview))
      (should (string-search "+y" preview))
      (should (string-search "+z" preview))
      (should-not (string-search "+w" preview))
      (should (string-search "… 1 more change" preview)))
    ;; The complete diff stays in :body for explicit expansion.
    (should (string-search "+w" (plist-get rendered :body))))

  :doc "An unreviewed or comment-only patch stays collapsed"
  (dolist (render-data
           '((:kind patch :applied 1 :total 1 :comments 0
              :files ((:path "a" :kind update :added 1 :deleted 0
                       :diff "@@\n+x")))
             (:kind patch :reviewed t :applied 0 :total 1 :comments 1
              :files nil :notes ("Rejected: a hunk 1"))))
    (let ((rendered (niwa-tool-patch--render
                     "ApplyPatch" nil nil render-data)))
      (should (plist-get rendered :initially-collapsed-p))
      (should-not (plist-get rendered :preview-body)))))

(niwa-deftest niwa-tool-patch--diff-hunks
  (:doc "Splits a diff into chunks on hunk headers") ,test (test)
  (should (equal '("@@ one\n-x\n+y" "@@ two\n+z")
                 (niwa-tool-patch--diff-hunks "@@ one\n-x\n+y\n@@ two\n+z")))

  :doc "A headerless diff is one chunk"
  (should (equal '("+one\n+two")
                 (niwa-tool-patch--diff-hunks "+one\n+two"))))

(niwa-deftest niwa-tool-patch--preview-body
  (:doc "Caps the preview at the change limit across files") ,test (test)
  (let ((preview (niwa-tool-patch--preview-body
                  '(:files ((:path "a" :kind update :added 1 :deleted 0
                             :diff "@@ one\n+x")
                            (:path "b" :kind update :added 2 :deleted 0
                             :diff "@@ two\n+y\n@@ three\n+z"))))))
    (should (string-search "M a" preview))
    (should (string-search "M b" preview))
    (should (string-search "+y" preview))
    (should-not (string-search "+z" preview))
    (should (string-search "… 1 more change" preview)))

  :doc "Truncates an oversized change to the line limit"
  (let* ((niwa-tool-patch--preview-line-limit 20)
         (lines (mapconcat (lambda (i) (format "+line %d" i))
                           (number-sequence 1 30) "\n"))
         (preview (niwa-tool-patch--preview-body
                   `(:files ((:path "big" :kind add :added 30 :deleted 0
                              :diff ,lines))))))
    (should (string-search "+line 20" preview))
    (should-not (string-search "+line 21" preview))
    (should (string-search "…" preview))))

(provide 'test-niwa-tool-patch)
;;; test-niwa-tool-patch.el ends here
