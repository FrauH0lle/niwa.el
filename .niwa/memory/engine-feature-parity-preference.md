---
name: "Aim for feature parity across Niwa engines"
description: "Consider both gptel and Claude Code/ACP when scoping shared features, even if the motivating report came from only one engine."
type: user
modified: 2026-11-28
---

# Aim for feature parity across Niwa engines

On 2026-11-27 (session `2026-11-27T00-08-0c3ae4823d6e`, turn 144), the user said, “I would argue for feature parity between the engines.” They wanted stall detection to cover both gptel and Claude Code/ACP even though the reported stall had not been observed on ACP.

When scoping a feature that serves the same user need on both engines, consider both from the start. A report from one engine identifies where the problem was seen, not necessarily where the feature should stop. Aim for equivalent user-facing behavior where applicable, not identical transport logic. If an engine cannot support the same behavior, explain the constraint and make the scope difference explicit.

This preference does not establish that a particular feature has shipped on either engine.
