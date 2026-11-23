---
name: "Keep package-specific configuration at the package boundary"
description: "Expose customization through the main package, and keep local Niwa-specific workarounds out of editor core when possible."
type: user
modified: 2026-11-23
---

# Keep package-specific configuration at the package boundary

- **Public customization:** User-facing keybinding customization should work from the main package's `use-package! … :config` block. Do not require users to discover internal module load order or configure a submodule to change advertised keybindings. The user also questioned an autoloaded `find-file-hook` because it removes user control over an important hook. Consider that control when proposing automatic global hooks. This does not require eagerly loading every optional module, prohibit autoloads, or select an activation mechanism. (User statements, 2026-11-14, `segment-0001.chat.org`, turn 4; the package was then referred to as mevedel.)
- **Local integration workarounds:** When a workaround is specific to Niwa and can be expressed in the user's Niwa configuration, prefer placing it there over teaching generic editor core about Niwa internals. The user asked, “Shouldnt we put the fix in the niwa config, if possible?” when a Niwa-specific window-closing workaround had been placed in Zenit core. This is a placement preference, not a rule against fixing a genuinely generic defect in core. (User statement, session `2026-11-22T13-23-95f54cd26f5e`, `segment-0001.chat.org`, turn 2.)
