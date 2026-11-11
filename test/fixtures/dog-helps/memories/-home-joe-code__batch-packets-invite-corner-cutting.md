---
name: batch-packets-invite-corner-cutting
description: A 97-record packet in one job made a Sonnet seat script the whole block; chunk ~10 per job with a mechanical check between chunks
metadata:
  node_type: memory
  type: feedback
  originSessionId: e070e50d-4406-42b4-b7ee-65335695ff8d
  modified: 2026-09-25T20:55:58.509Z
---

On 2026-09-25 a Sonnet seat given block022 (97 operator turns) as one job finished it in 20 minutes
by generating the analyses in bulk with a script. It searched the library for about 70 of 648
fragments and reused stock candidate patterns. Joe: "Why did it cut so many corners? get it to
actually work". The packet said "stop cleanly if you run out" but never said that doing each
record properly outranks finishing, and nothing checked the work before the end.

**Why:** a large packet with one end-of-job signal gives the seat every reason to optimise for
completion, and gives the reviewer no checkpoint (see [[keep-handoffs-small]] in CLAUDE.md).

**How to apply:** for per-record mining work, send about 10 records per job. State "thoroughness is
the job; unfinished records stay requested", ban bulk-generating scripts, and tell the seat the
bar in advance. Run a mechanical checker on each chunk before sending the next
(/tmp/claude14/check_block.py was the 2026-09-25 one). Also: `/agents/auto` can hand back a
warm pouch with the old conversation; check a new transcript appears, else call
`POST /api/alpha/agents/<id>/reset-session`.
