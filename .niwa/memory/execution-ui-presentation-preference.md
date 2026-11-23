---
name: "Background-work UI: visible, unified, and composer-stable"
description: "Make scheduled work discoverable without crowding the composer; keep running work unified and completed entries inspectable."
type: user
modified: 2026-11-23
---

# Execution and background-work UI preferences

## Minimal presentation and coherent grouping

In a 2026-09-27 preview comparison, the user favored a work-oriented default: a minimal command row over a result card with an output preview. A completion breadcrumb looked potentially useful but was not chosen over the minimal row.

On 2026-10-16, the user said the executions/Monitor UI felt awkward: clicking the row above the composer was not expected to open the cockpit, and executions and monitors belong to the same category.

On 2026-10-17, the user asked to unify background work in one area, **including agents**. The live panel should show what is actually running; finished items should leave it because completion has an event row in the conversation. Do not repeat header counts in the footer. Keep controls out of a busy header: the user accepted `[Manage]` in its own last row, with `[Show all N]` / `[Show fewer]` for overflow, and found the trailing dots in `Manage…` strange.

On 2026-10-18, the user qualified the removal rule: leave completed entries visible while expanded. After collapse, start a fresh removal grace period **only once point is outside the background area**. No exact duration was specified.

## Composer stability, interactive rows, and scheduled work

On 2026-10-17, the user said the main layout discomfort was status content growing and pushing the composer away. Prefer status content **below the composer**, while allowing the composer itself to grow during typing. Interactive rows must remain real buffer text so point can move to them and invoke their actions; the user rejected `after-string` for those rows.

Wake-ups and schedules should be discoverable in **both Emacs and the browser** so background work does not start by surprise. Balance this with the user's concern that the background-work panel above the composer was getting full. A small composer hint was the user's suggestion, not an approved widget or interaction design. (User statements, session `2026-11-21T22-17-7c90d5334396`, `segment-0001.chat.org`, turns 14–15.)

## Scope and unconfirmed proposals

The October 17 request for planning and mockups rather than code changes applied to that design phase, not all UI work. The following were not confirmed: one breadcrumb per background command regardless of delivery mechanism; the October 16 inline-expansion proposal; exact generic footer wording; removing `All…`; a repeated-Monitor-event collapse policy; a fixed panel height; and a marker-based composer layout.
