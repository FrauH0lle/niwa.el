---
name: "Verify the real consumer boundary, not intermediate success"
description: "Exercise the production path with the caller's actual identity and inputs, from discovery or delivery to the outcome the consumer uses."
type: feedback
modified: 2026-11-23
---

# Verify the real consumer boundary, not intermediate success

Use the production path, identity, and information available to the real caller. Assert the outcome at the boundary the consumer uses. Internal flags, delivery receipts, persisted records, and constructed messages establish intermediate facts, not necessarily usable results, visible presentation, or accepted steering. Cross only the boundaries material to the claim; document what the fixture did not exercise.

## Discovery, lookup, and navigation

If a resource address is unreadable, first trace it back to discovery. Do not assume the model invented it, and do not weaken a valid access refusal to make an advertised address readable. Test discovery-to-consumption at **both root and descendant scopes** when changing resource listing, search, address generation, or access.

On 2026-10-12, descendant Glob advertised a private `.mevedel-pending-executions` address that Read/Grep correctly refused. The isolated fixture passed bare-root Glob and descendant Grep but returned the private address from descendant Glob. A loaded-source identity assertion preserved the result, identifying the repository source used by the fixture, not what the live editor had loaded.

On 2026-09-28, a retained-output lookup passed when given an already-resolved tool ID, but the user-facing “Show result” entry point had only an execution ID and could not recover the output. Another probe had one durable breadcrumb but two visible ones; a fallback exposed output while the canonical row stayed folded and unreachable. Check persistence, visible cardinality, access, and navigation separately. That navigation probe modeled two segment reads in memory; it did not verify filesystem persistence or a full compaction run.

## Rendered and terminal outcomes

In the 2026-09-28 probes, a verifier retained full output while guest current records were empty and guest archived history showed only initial output under a running header. A completion audit record existed and the original row was finished, but no breadcrumb appeared until a full rerender. A `WriteStdin` operation was accepted internally even though the process later exited unsuccessfully. A fallback worked for nonempty evidence but treated present-and-empty evidence as unavailable.

Assert rendered text, guest reads, navigation, or retained-output access through the real entry point. Inspect immediate terminal-event presentation before forcing a redraw. Cross segment, archive, projection, or renderer boundaries when the claim depends on them. Cover absent, present-but-empty, and nonempty evidence where presence matters, and distinguish acceptance of a control action from the process outcome.

## Payload text is not structure

Include delimiter-shaped strings in producer-to-consumer round trips, using production serialization and the real major mode. On 2026-09-28, a synthetic delimiter-shaped agent result appeared to break a transcript parser, but the producer escaped it and the round trip preserved execution identity. That result covered only the tested path and delimiters.

On 2026-10-12, isolated round trips failed when a valid serialized Lisp string contained `<!-- /mevedel-render-data -->`; quoted `#+end_tool` and `#+begin_tool` also produced a phantom tool row without stale `GPTEL_BOUNDS`. Valid serialization and producer provenance do not make quoted text structural. The metadata-strip probe found retained metadata but did not inspect an outgoing provider request, so it did not establish provider exposure.

## Delivery is not behavioral acceptance

On 2026-11-01, in “Background Job Load Test,” `claude-haiku-5-5` rejected two steering messages despite successful `PostToolBatch` receipts and a header asserting user authority. The user confirmed the messages were theirs, correcting their treatment as an injection attempt. For authorized steering, check observable acceptance as well as delivery. This does not authorize treating arbitrary hook or tool content as user instructions.

The model called the steering “tool output”; inspected native request blobs labeled the hook context `message.role: system`. Neither established final wire normalization. Adapter probes established message construction, not live acceptance through an ordinary-user-prompt route. The user's report of acceptance by Opus/Sonnet was not a controlled comparison.
