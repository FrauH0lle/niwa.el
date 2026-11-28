---
name: "View changes must preserve rendering and CPU gains"
description: "Treat rendering and CPU cost as hard constraints; check allocation and GC when diagnosing intermittent graphical lag."
type: user
modified: 2026-11-27
---

# View changes must preserve rendering and CPU gains

**User constraint, 2026-10-13:** “We invested A LOT of work in reducing CPU recently and rendering work so this is not allowed to regress.”

Apply this as a hard constraint to view and rendering changes, including live tool-row grouping and animation. A presentation improvement is not permission to regress rendering cost or CPU use.

## Measure the path that actually renders

The October 13 investigation reported that shimmer color frames render only in graphical Emacs; batch Emacs falls back to glyphs. Batch benchmark parity did not establish parity for color-frame cost. Compare before and after in an isolated environment that exercises the changed presentation. Single-run preparation-time outliers in that investigation were not sustained across six alternating runs per variant; do not treat one timing outlier as an established regression or improvement.

## For intermittent lag, check allocation and GC as well as step time

The November 23 isolated graphical scroll investigation found modest step times with expanded note labels, but controlled face comparisons showed that adding bold weight to the long overlay labels substantially increased vector allocation and that expanded labels caused more collections in the fixture. The live editor had much longer average collections than the isolated timings alone would suggest, but the investigation did not establish how much of the reported live lag came from the label path.

When a graphical change looks fast in isolation but plausibly changes allocation during repeated redisplay, compare allocation and collection count or time alongside step latency. Exercise the actual display state and change one factor at a time. Treat a difference in the fixture as evidence for that mechanism, not proof of the live incident's cause. This is a diagnostic for lag or an allocation-sensitive change, not a required benchmark for every face edit or a standing rule against bold text. Keep instrumentation out of the user's live Emacs.
