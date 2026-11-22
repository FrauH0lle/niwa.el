---
name: "Exercise the failing order before claiming a timing fix"
description: "Force the suspected interleaving, test both reentry orders where relevant, and distinguish a demonstrated mechanism from the original incident."
type: feedback
modified: 2026-11-21
---

# Exercise the failing order before claiming a timing fix

An ordinary green replay does not show that a timing-sensitive failure was reached or fixed. Impose a reachable failing interleaving and compare the old and changed behavior under that schedule. Where callbacks can reenter, exercise both nesting orders: older work can resume after nested finalization and restore obsolete output or state. Use this when ordering is material to the claim, not as a requirement to force an artificial order in every regression.

A controlled reproduction can establish a mechanism with matching symptoms; it is not a trace of the original incident. Keep repository validation, isolated-checkout validation, and live deployment separate.

## Reentrant view rendering

On September 16, 2026, ordinary streaming and a live update nested inside settlement passed. Settlement injected during fontification inside an older live update did not: the older update resumed, duplicated response text, and recreated retained-tail state. The loaded native functions reproduced the injected failure in temporary buffers, so it was not confined to the scratch harness.

Compare authoritative transcript data with the rendered view before attributing duplicated display content to duplicated model output. Here the transcript contained one heading and HTML example, the view contained multiple copies, and a fresh render was correct. Historical render tracing was unavailable and the full original corruption was not reproduced, so the original event order remains unknown.

## Remote hook startup and stdin delivery

On September 18, 2026, a remote `PreToolUse` hook exceeded its configured 10-second timeout and the guarded Read never ran. Instrumented checks passed with both handlers, and a later uninstrumented replay also passed. A controlled test then coalesced the final TRAMP exec and JSON writes without changing payload bytes or order. A preceding shell read ahead and lost the hook's stdin on exec, reproducing the failure without establishing that the original stall used that same schedule.

Do not raise the timeout or weaken or delete the hook check to make this failure disappear. A larger timeout would have hidden the stdin race rather than resolved it. For current remote startup, readiness, timeout ownership, and cancellation, consult `docs/hooks.md` and the relevant source history; this case is evidence for a verification method, not a recipe or a diagnosis of every remote timeout.
