---
name: kimi-bells-need-requisition
description: "A bell to a kimi-N seat without a \"Requisition: <M-*|E-*|T-*> — purpose\" line fails at once (invoke-error), with no work done"
metadata:
  node_type: memory
  type: reference
  originSessionId: c98c6e8c-c4bf-46af-937c-dcd03bd34bfc
  modified: 2026-11-10T22:17:43.805Z
---

A bell to a kimi-N seat must carry a line `Requisition: <M-*|E-*|T-*> — <one-line purpose>`, usually as the packet's first line. Without it the job goes `failed` within seconds (terminal-code invoke-error, "You can't use a Kimi seat without a requisition"). If you parked on that job, the park then wakes you at once with "(no summary)". The seat keeps one conversation per requisition target and compacts it when the target changes.

**How to apply:**
- Put the Requisition line in every Kimi packet.
- After dispatching, poll the job for ~10 s; a failure shows in `terminal-message`.

Episode 2026-11-10 (claude-9): the Euclid weak-model run packet to kimi-1 failed this way. It was resent with `Requisition: E-euclid-tutor — …`. Related: [[kimi-task-error-may-have-dispatched]], [[agency-send-pass-mode-work]].

Episode 2026-11-11 (claude-13): this applies to in-thread replies too. A Kimi seat's report arrived as a bell whose header said "your response is delivered back automatically"; my reply asked for follow-up work, went to kimi-1 as an auto-bellback without a Requisition line, and failed the same way ("(no summary)" on the park). When a reply to a Kimi seat asks for new work, send it as a fresh bell with a Requisition line, not as the in-thread reply.
