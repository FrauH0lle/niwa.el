---
name: "Preserve agreed scope and distinguish it from the example"
description: "Carry agreed work and approved visuals forward; do not turn a single reported example into a silent scope limit."
type: feedback
modified: 2026-11-28
---

# Preserve agreed scope and distinguish it from the example

When converting an agreed design into a specification, PRD, implementation tickets, or code, preserve the agreed scope. Do not silently drop an item, relabel it as out of scope, demote a prerequisite to optional follow-up, or narrow an approved prototype's presentation.

## Preserve agreed verification work

On 2026-11-11 (`segment-0001.chat.org`, turn 51), the user noticed that a TRAMP store round-trip measurement the assistant had agreed to include as a ticket was moved to “Out of scope” in the specification. The assistant restored it to the ticket plan.

## Approved prototype visuals count as scope

On 2026-11-13 (`segment-0003.chat.org`, turns 245–246), the user asked why notes were not colored: “The demo we had in scratch used colors. I thought we would take them over?” The selected label look included per-author colors; reducing it to a grey label narrowed the selection without asking. Carry across the visual treatment of the chosen design as well as its named controls and behavior. This does not make every exploratory alternative a requirement; the user did not confirm that the separate ambient-underline look was also wanted.

## A reported example is not a scope limit

On 2026-11-27 (session `2026-11-27T00-08-0c3ae4823d6e`, turns 166–167), the assistant explained that it had scoped thumbnail work to PNG because the profile used a PNG. The user corrected it: “this should work for all supprted image formats.” When a report or profile uses one format, check which supported variants the requested behavior concerns before turning that fixture into the acceptance boundary. If a variant needs different handling or a justified exclusion, name it explicitly. The correction establishes the requested scope, not which formats the final implementation handles.

Reconcile specifications, tickets, and implementation against the agreement before presenting a handoff or claiming completion. When new evidence warrants a scope change, surface it and explain why rather than treating a rewrite as approval. Keep actual work items, dependencies, and current decisions in the tracker or maintained specification.
