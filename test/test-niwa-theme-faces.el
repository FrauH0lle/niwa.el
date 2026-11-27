;;; test-niwa-theme-faces.el --- Tests for niwa-theme-faces.el -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'niwa-theme-faces)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(defface test-niwa-theme-faces--face
  '((t :inherit shadow))
  "Throwaway face for the theme-derivation cases."
  :group 'niwa)

(defun test-niwa-theme-faces--hex-rgb (name &optional _frame)
  "Resolve NAME as a #rrggbb triplet the way a graphic frame would.
Batch Emacs resolves color names through the terminal color map, where
a colour like #bbc2cf comes back as pure white and every blend measured
from it is meaningless."
  (if (and (stringp name) (string-prefix-p "#" name) (= (length name) 7))
      (list (/ (string-to-number (substring name 1 3) 16) 255.0)
            (/ (string-to-number (substring name 3 5) 16) 255.0)
            (/ (string-to-number (substring name 5 7) 16) 255.0))
    (list 0.0 0.0 0.0)))

(defmacro test-niwa-theme-faces--with-gui-colors (cells &rest body)
  "Run BODY with graphic-frame color resolution and CELLS display colors."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'color-name-to-rgb)
              #'test-niwa-theme-faces--hex-rgb)
             ((symbol-function 'display-color-cells) (lambda (&rest _) ,cells)))
     ,@body))

(niwa-deftest niwa--contrast ()
  ,test
  (test)
  :doc "returns the full range between black and white"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (should (< 20.9 (niwa--contrast "#ffffff" "#000000") 21.1)))
  :doc "returns 1 for a colour against itself"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (should (< 0.99 (niwa--contrast "#51afef" "#51afef") 1.01)))
  :doc "is symmetric in its arguments"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (should (equal (niwa--contrast "#bbc2cf" "#282c34")
                   (niwa--contrast "#282c34" "#bbc2cf")))))

(niwa-deftest niwa--muted-color ()
  ,test
  (test)
  :doc "mutes below the foreground while clearing the contrast target"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (dolist (theme '(("#bbc2cf" "#282c34")   ; doom-one, dark
                     ("#ffffff" "#000000")   ; modus-vivendi, dark
                     ("#000000" "#ffffff")   ; default, light
                     ("#657b83" "#fdf6e3"))) ; solarized, light
      (pcase-let* ((`(,foreground ,background) theme)
                   (muted (niwa--muted-color foreground background)))
        (should (>= (niwa--contrast muted background)
                    niwa-muted-contrast))
        (should (< (niwa--contrast muted background)
                   (niwa--contrast foreground background))))))
  :doc "returns the foreground unchanged when no blend can clear the target"
  (test-niwa-theme-faces--with-gui-colors 16777216
    ;; 2.85:1 to begin with: there is no room below it.
    (should (equal "#999999" (niwa--muted-color "#999999" "#ffffff"))))
  :doc "follows `niwa-muted-contrast'"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (let ((dim (let ((niwa-muted-contrast 2.0))
                 (niwa--muted-color "#bbc2cf" "#282c34")))
          (bright (let ((niwa-muted-contrast 6.0))
                    (niwa--muted-color "#bbc2cf" "#282c34"))))
      (should (< (niwa--contrast dim "#282c34")
                 (niwa--contrast bright "#282c34"))))))

(niwa-deftest niwa--dim-color ()
  ,test
  (test)
  :doc "replaces a shadow below the contrast floor with one that clears it"
  (test-niwa-theme-faces--with-gui-colors 16777216
    ;; doom-one: the theme's own shadow is the failing control.
    (let ((dim (niwa--dim-color "#5b6268" "#bbc2cf" "#282c34")))
      (should (< (niwa--contrast "#5b6268" "#282c34") niwa-muted-contrast))
      (should (>= (niwa--contrast dim "#282c34") niwa-muted-contrast))))
  :doc "keeps a shadow that already clears the floor"
  (test-niwa-theme-faces--with-gui-colors 16777216
    ;; modus-vivendi and the stock light theme.
    (should-not (niwa--dim-color "#989898" "#ffffff" "#000000"))
    (should-not (niwa--dim-color "#7f7f7f" "#000000" "#ffffff")))
  :doc "derives a colour when the theme has no shadow colour"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (should (>= (niwa--contrast (niwa--dim-color nil "#bbc2cf" "#282c34")
                                "#282c34")
                niwa-muted-contrast))))

(niwa-deftest niwa--panel-background ()
  ,test
  (test)
  :doc "keeps a themed background that is visible"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (should (equal "#1e1e1e"
                   (niwa--panel-background "#1e1e1e" "#ffffff" "#000000"))))
  :doc "blends a visible one when the theme has none or a faint one"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (dolist (theme '((nil "#000000" "#ffffff")          ; default: unset
                     ("#23272e" "#bbc2cf" "#282c34")    ; doom-one: 1.07:1
                     (nil "#ffffff" "#000000")))
      (pcase-let* ((`(,themed ,foreground ,background) theme)
                   (panel (niwa--panel-background themed foreground background)))
        (when themed
          (should (< (niwa--contrast themed background) 1.1)))
        (should (< 1.1 (niwa--contrast panel background) 1.5)))))
  :doc "follows the intensity"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (should (< (niwa--contrast (niwa--panel-background nil "#98be65" "#282c34" 0.06)
                               "#282c34")
               (niwa--contrast (niwa--panel-background nil "#98be65" "#282c34" 0.15)
                               "#282c34")))))

(niwa-deftest niwa-state-cell ()
  ,test
  (test)
  :doc "faces a state by what it asks of the user, whatever its case"
  (pcase-dolist (`(,label . ,face)
                 '(("Running" . success) ("active" . success)
                   ("Blocked" . bold) ("paused" . bold) ("missing" . bold)
                   ("failed" . error)
                   ("Queued" . niwa-view-dim) ("discoverable" . niwa-view-dim)))
    (let ((cell (niwa-state-cell label)))
      (should (equal label cell))
      (should (eq face (get-text-property 0 'face cell)))))
  :doc "never colours a state as warning text"
  (dolist (label '("Blocked" "Waiting" "Stopping" "paused" "pending" "stale"))
    (should-not (eq 'warning (get-text-property 0 'face (niwa-state-cell label))))))

(niwa-deftest niwa-view-panel-dim
  (:vars ((default-background (face-attribute 'default :background nil))
          (default-foreground (face-attribute 'default :foreground nil))
          (shadow-foreground (face-attribute 'shadow :foreground nil))
          (niwa--theme-derived-faces
           (list (assq 'niwa-view-panel-dim niwa--theme-derived-faces))))
   :before-each
   (progn
     (set-face-attribute 'default nil
                         :foreground "#bbc2cf" :background "#282c34")
     (set-face-attribute 'shadow nil :foreground "#5b6268"))
   :after-each
   (progn
     (set-face-attribute 'default nil
                         :foreground default-foreground
                         :background default-background)
     (set-face-attribute 'shadow nil :foreground shadow-foreground)
     (put 'niwa-view-panel 'customized-face nil)
     (set-face-attribute 'niwa-view-panel nil :background 'unspecified)
     (set-face-attribute 'niwa-view-panel-dim nil :foreground 'unspecified)))
  ,test
  (test)
  :doc "measures against a panel background the user customized"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (let ((custom "#4a4f58"))
      (put 'niwa-view-panel 'customized-face `((t :background ,custom)))
      (set-face-attribute 'niwa-view-panel nil :background custom)
      (niwa--derive-theme-faces)
      ;; Control: the colour derived for the blended panel fails on this one.
      (should (< (niwa--contrast
                  (niwa--dim-color
                   "#5b6268" "#bbc2cf"
                   (niwa--panel-background nil "#bbc2cf" "#282c34"))
                  custom)
                 niwa-muted-contrast))
      (should (>= (niwa--contrast
                   (face-attribute 'niwa-view-panel-dim :foreground nil)
                   custom)
                  niwa-muted-contrast))))
  :doc "measures against the blended panel otherwise"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (niwa--derive-theme-faces)
    (should (equal (niwa--dim-color
                    "#5b6268" "#bbc2cf"
                    (niwa--panel-background nil "#bbc2cf" "#282c34"))
                   (face-attribute 'niwa-view-panel-dim :foreground nil)))))

(niwa-deftest niwa--face-user-styled-p
  (:after-each
   (progn
     (put 'test-niwa-theme-faces--face 'customized-face nil)
     (put 'test-niwa-theme-faces--face 'theme-face nil)))
  ,test
  (test)
  :doc "ignores a face carrying only its defface spec"
  (should-not (niwa--face-user-styled-p 'test-niwa-theme-faces--face))
  :doc "detects a user customization"
  (progn
    (put 'test-niwa-theme-faces--face
         'customized-face '((t :height 1.0)))
    (should (niwa--face-user-styled-p 'test-niwa-theme-faces--face)))
  :doc "detects a theme spec"
  (progn
    (put 'test-niwa-theme-faces--face
         'theme-face '((user ((t :height 1.0)))))
    (should (niwa--face-user-styled-p 'test-niwa-theme-faces--face))))

(niwa-deftest niwa--derive-theme-face
  (:vars ((default-background (face-attribute 'default :background nil))
          (default-foreground (face-attribute 'default :foreground nil))
          (niwa--theme-derived-faces nil))
   :before-each
   (set-face-attribute 'default nil
                       :foreground "#bbc2cf"
                       :background "#282c34")
   :after-each
   (progn
     (set-face-attribute 'default nil
                         :foreground default-foreground
                         :background default-background)
     (put 'test-niwa-theme-faces--face 'customized-face nil)
     (face-spec-set 'test-niwa-theme-faces--face
                    '((t :inherit shadow))
                    'face-defface-spec)))
  ,test
  (test)
  :doc "registers the face and derives it immediately"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (niwa--derive-theme-face
     'test-niwa-theme-faces--face
     (lambda (foreground background)
       (list :foreground (niwa--muted-color foreground background))))
    (should (equal '(test-niwa-theme-faces--face)
                   (mapcar #'car niwa--theme-derived-faces)))
    (should (equal "#808591"
                   (face-attribute 'test-niwa-theme-faces--face
                                   :foreground nil t))))
  :doc "replaces rather than duplicates an existing registration"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (niwa--derive-theme-face
     'test-niwa-theme-faces--face
     (lambda (&rest _) '(:foreground "#111111")))
    (niwa--derive-theme-face
     'test-niwa-theme-faces--face
     (lambda (&rest _) '(:foreground "#222222")))
    (should (= 1 (length niwa--theme-derived-faces)))
    (should (equal "#222222"
                   (face-attribute 'test-niwa-theme-faces--face
                                   :foreground nil t)))))

(niwa-deftest niwa--derive-theme-faces
  (:vars ((default-background (face-attribute 'default :background nil))
          (default-foreground (face-attribute 'default :foreground nil))
          (niwa--theme-derived-faces
           (list (cons 'test-niwa-theme-faces--face
                       (lambda (&rest _) '(:foreground "#123456"))))))
   :before-each
   (set-face-attribute 'default nil
                       :foreground "#bbc2cf"
                       :background "#282c34")
   :after-each
   (progn
     (set-face-attribute 'default nil
                         :foreground default-foreground
                         :background default-background)
     (put 'test-niwa-theme-faces--face 'customized-face nil)
     (face-spec-set 'test-niwa-theme-faces--face
                    '((t :inherit shadow))
                    'face-defface-spec)))
  ,test
  (test)
  :doc "applies every registered derivation"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (niwa--derive-theme-faces)
    (should (equal "#123456"
                   (face-attribute 'test-niwa-theme-faces--face
                                   :foreground nil t))))
  :doc "leaves a user-styled face alone"
  (test-niwa-theme-faces--with-gui-colors 16777216
    (put 'test-niwa-theme-faces--face
         'customized-face '((t :height 1.0)))
    (niwa--derive-theme-faces)
    (should (eq 'unspecified
                (face-attribute 'test-niwa-theme-faces--face
                                :foreground nil))))
  :doc "does nothing on a display whose colours cannot be measured"
  (test-niwa-theme-faces--with-gui-colors 8
    (niwa--derive-theme-faces)
    (should (eq 'unspecified
                (face-attribute 'test-niwa-theme-faces--face
                                :foreground nil)))))

(provide 'test-niwa-theme-faces)

;;; test-niwa-theme-faces.el ends here
