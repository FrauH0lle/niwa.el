---
name: "Finalize one coherent design before implementing"
description: "Use concrete mockups to settle design questions before editing code or prototypes; prefer one coherent design over phased versions."
type: feedback
modified: 2026-11-23
---

# Finalize one coherent design before implementing

Settle design questions with the user before editing code or prototypes, delegating edits or runs, or adding backlog entries. Present one coherent design rather than using v1/v2 phases or backlog entries to defer unresolved scope.

**Make the questions concrete.** Include one or more example mockups with each design-review question so the user can judge the proposed behavior. An inline mockup that shows what the user would see is part of discussion; editing a prototype file is not. Use examples to make real alternatives or tradeoffs visible without presenting them as approved designs.

This boundary does not apply once the user has asked for implementation or explicitly asked for a prototype. When the chosen workflow starts with a design grill, finish the grill before prototyping or implementing.

**Sources:** Floating-composer discussion, 2026-11-18: the user said, “Please don't start an implementation, we need to finalize the design first,” after a `.scratch` prototype was edited prematurely. Schedules discussion, 2026-11-19: the user asked for “one good, coherent design” and objected to adding work to a backlog they were trying to clean up. Schedule-visibility design review, session `2026-11-22T13-22-fe5c835dc36a`, `segment-0001.chat.org`, turn 3: the user asked for concrete example mockups with the questions.
