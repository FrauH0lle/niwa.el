# Pending Plugin Hook Consent Interrupts Interactive Startup

Executable plugin hook consent is part of the plugin trust boundary, not just
plugin metadata. When an enabled plugin's hook surface changes, niwa keeps
plugin skills active but withholds executable hooks until the user reviews the
changed surface.

Consent identifies each executable handler by its source, event, matcher,
type, and command or function. Changing matcher scope therefore invalidates
the prior consent even when the handler itself is unchanged.

Consent also records the hash of each script the handlers' commands name, as
project hook trust does. A changed script does not withhold the whole plugin:
its handler alone is skipped with a warning until `/plugin hooks NAME on` asks
again. The command line alone does not show what runs, so approving it would
approve whatever a later pull puts in its script.

Interactive session startup or resume actively notifies the user when any
enabled plugin has pending hook consent. Because withheld hooks can make an
enabled plugin behave incorrectly, niwa reports this as a warning/message
and routes review through the plugin cockpit. Consent is reviewed one plugin
at a time; niwa does not offer a blanket approve-all action for multiple
executable hook surfaces.

Until the user approves the changed hook surface, the session continues with
those hooks withheld and the plugin cockpit continues to show the pending
consent state. Noninteractive and batch contexts do not block; they report the pending consent through user-visible warnings/messages instead.

Pending consent is user configuration and security state, not model context.
It is shown in the user-facing session/view surfaces and messages, not
injected as a model-visible reminder.

Consent is stored outside the workspace, in `plugin-consent.eld` under
`niwa-user-dir` beside project hook trust, keyed by workspace identity, plugin
and source, with the approved fingerprint and script hashes. Workspace files
hold activation only, so a copied or restored workspace carries no consent and
a revocation cannot be undone by restoring old state. Remote workspaces are
keyed by their TRAMP identity, so consent is per client machine.

## Decision history

Consent first lived in the workspace's plugin state file. A checkout or
restored workspace could then carry what looked like the user's approval, and
restoring old state undid a revocation. Signing that file with a per-user key
was considered and dropped; the user chose to move consent to user storage
(needs-user #10, 2026-11-28).
