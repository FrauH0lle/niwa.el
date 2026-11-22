---
name: "Make Niwa look good in vanilla Emacs and respect the active theme"
description: "Check view appearance in graphical emacs -Q, and prefer theme-derived colors for child-frame borders."
type: user
modified: 2026-11-21
---

# Make Niwa look good in vanilla Emacs and respect the active theme

**Stock appearance matters.** In the docked-composer discussion (November 20, 2026, turn 2), the user said the UI must look good in vanilla Emacs and asked to verify it under `emacs -Q`, not only under their themed setup. For view appearance changes, inspect the result in a separate graphical `emacs -Q` process. A stock appearance check does not replace a configured-environment check when the claim is about a particular theme, package, or configuration.

**Prefer theme-derived child-frame borders.** In the floating-composer discussion (November 19, 2026, turns 25, 29, and 32), the user preferred deriving child-frame border colors from the active theme rather than using fixed colors such as teal. The discussion covered the composer, note frame, and share panel. Apply this preference when choosing or revising those colors. It does not establish that existing borders already follow the theme, nor does it prohibit every fixed color elsewhere in the UI.
