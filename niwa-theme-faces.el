;;; niwa-theme-faces.el --- Theme-derived faces -*- lexical-binding: t -*-

;;; Commentary:

;; Derives registered face attributes from the active theme and reapplies
;; them when themes or measurable frames change.

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'color)
(require 'niwa-utilities)

(defcustom niwa-muted-contrast 3.6
  "Minimum contrast ratio for muted text against the background.
Sits between the 3:1 floor for large text and the contrast of ordinary
prose, so text a face mutes reads as quiet without becoming unreadable.
A fixed blend cannot serve every theme: blending the foreground 45% into
the background yields 3.4:1 on a 7.8:1 theme but 6.3:1 on a 21:1 one and
2.0:1 on a 4:1 one, so `niwa--muted-color' searches for this target
instead."
  :type 'float
  :group 'niwa)

(defvar niwa--theme-derived-faces nil
  "Alist of (FACE . FUNCTION) for faces blended from the active theme.
FUNCTION is called with the default face's foreground and background and
returns a plist of face attributes to apply.")

(defun niwa--color-frame ()
  "Return a frame whose colors can be measured, or nil.
On a terminal `color-name-to-rgb' resolves through the terminal color
map, where a colour like #bbc2cf comes back as pure white, so a blend
measured there is meaningless."
  (seq-find (lambda (frame) (> (display-color-cells frame) 256))
            (frame-list)))

(defun niwa--contrast (color-a color-b)
  "Return the contrast ratio between COLOR-A and COLOR-B.
The Y component of CIE XYZ is relative luminance, so `color-srgb-to-xyz'
already does the work."
  (let* ((luminance
          (lambda (color)
            (nth 1 (apply #'color-srgb-to-xyz
                          (niwa--color-name-to-rgb color)))))
         (a (funcall luminance color-a))
         (b (funcall luminance color-b)))
    (/ (+ (max a b) 0.05) (+ (min a b) 0.05))))

(defun niwa--muted-color (foreground background)
  "Return FOREGROUND blended as far toward BACKGROUND as contrast allows.
The most muted blend still clearing `niwa-muted-contrast' wins.  On a
theme whose own foreground is already that low-contrast no blend clears
the target and FOREGROUND is returned unchanged, which is the honest
answer: there is no room below it."
  (or (cl-loop for percent downfrom 65 to 5 by 5
               for color = (niwa--tint foreground background
                                          (/ percent 100.0))
               when (>= (niwa--contrast color background)
                        niwa-muted-contrast)
               return color)
      foreground))

(defun niwa--dim-color (shadow foreground background)
  "Return a readable replacement for SHADOW, or nil when SHADOW is readable.
SHADOW is the theme's `shadow' foreground.  A theme that keeps it above
`niwa-muted-contrast' against BACKGROUND keeps its own colour; one that
does not (2.3:1 on doom-one) gets FOREGROUND muted to the target."
  (unless (and shadow
               (>= (niwa--contrast shadow background) niwa-muted-contrast))
    (niwa--muted-color foreground background)))

(defun niwa--panel-background (themed accent background &optional intensity)
  "Return a panel background that is visible against BACKGROUND.
THEMED is the theme's own background for the panel; it is kept when it
clears 1.1:1.  Otherwise BACKGROUND is tinted toward ACCENT by INTENSITY,
0.08 by default, which lands near the table stripe on every theme."
  (if (and (stringp themed) (>= (niwa--contrast themed background) 1.1))
      themed
    (niwa--tint background accent (or intensity 0.08))))

(defun niwa--face-user-styled-p (face)
  "Return non-nil when FACE carries a user or theme spec to defer to."
  (or (get face 'customized-face)
      (get face 'saved-face)
      (get face 'theme-face)))

(defun niwa--derive-theme-faces (&rest _)
  "Apply every derivation in `niwa--theme-derived-faces'.
Faces the user or a theme has styled are left alone, and nothing happens
at all on a display whose colors cannot be measured."
  (when-let* ((frame (niwa--color-frame)))
    ;; `niwa--tint' and `color-name-to-rgb' both resolve against the
    ;; selected frame, which in a daemon may be a terminal.
    (with-selected-frame frame
      (let ((foreground (face-foreground 'default frame t))
            (background (face-background 'default frame t)))
        (when (and foreground background)
          (pcase-dolist (`(,face . ,derive) niwa--theme-derived-faces)
            (when (and (facep face) (not (niwa--face-user-styled-p face)))
              (apply #'set-face-attribute face nil
                     (funcall derive foreground background)))))))))

(defun niwa--derive-theme-face (face derive)
  "Register FACE as derived from the active theme and derive it now.
DERIVE is called with the default face\='s foreground and background and
returns a plist of attributes for FACE."
  (setf (alist-get face niwa--theme-derived-faces) derive)
  (niwa--derive-theme-faces))

(defface niwa-view-dim
  '((t :inherit shadow))
  "Secondary text that must stay readable: metadata, hints and labels.
It is the theme's `shadow' wherever that clears `niwa-muted-contrast';
elsewhere `niwa--derive-theme-faces' replaces the foreground.  Rules and
borders, which are not text, keep `shadow' itself."
  :group 'niwa)

(niwa--derive-theme-face
 'niwa-view-dim
 (lambda (foreground background)
   (list :foreground (or (niwa--dim-color (face-foreground 'shadow nil t)
                                           foreground background)
                         'unspecified))))

(defface niwa-view-panel
  '((t :extend t))
  "Background of a card set apart from the transcript, such as a prompt.
The background is blended from the active theme by
`niwa--derive-theme-faces'."
  :group 'niwa)

(niwa--derive-theme-face
 'niwa-view-panel
 (lambda (foreground background)
   (list :background (niwa--panel-background nil foreground background))))

(defun niwa-state-cell (label)
  "Return state LABEL for a list row, faced by what the state asks of you.
Work in progress is `success', a state waiting on something is bold, a
failure is `error', and anything settled or idle is dimmed, so a list's
State column can be read without reading it."
  (propertize
   label 'face
   (pcase (downcase label)
     ((or "running" "watching" "active" "ready" "working") 'success)
     ((or "blocked" "waiting" "stopping" "paused" "pending" "missing" "stale")
      'bold)
     ((or "failed" "error") 'error)
     (_ 'niwa-view-dim))))

(defface niwa-view-panel-dim
  '((t :inherit niwa-view-dim))
  "Secondary text on a `niwa-view-panel' card.
`niwa-view-dim' is measured against the default background; on the
panel's tint the same colour falls under `niwa-muted-contrast' (3.2:1 on
doom-one).  This face is derived against the panel instead, and
`niwa--prompt-framed-body' substitutes it for dim text inside a card."
  :group 'niwa)

(niwa--derive-theme-face
 'niwa-view-panel-dim
 (lambda (foreground background)
   (list :foreground
         (or (niwa--dim-color
              (face-foreground 'shadow nil t) foreground
              ;; A panel the user or a theme styled is left alone by the
              ;; derivation, so measure against the background it has.
              (or (and (niwa--face-user-styled-p 'niwa-view-panel)
                       (face-background 'niwa-view-panel nil t))
                  (niwa--panel-background nil foreground background)))
             'unspecified))))

;; Themes reset frame attributes, so the blends have to be rewritten after
;; every change; a daemon has no measurable frame until one is created.
(add-hook 'enable-theme-functions #'niwa--derive-theme-faces)
(add-hook 'disable-theme-functions #'niwa--derive-theme-faces)
(add-hook 'after-make-frame-functions #'niwa--derive-theme-faces)


(provide 'niwa-theme-faces)

;;; niwa-theme-faces.el ends here
