---
name: kimi-unique-requisition
description: Joe 2026-09-25 — every Kimi dispatch gets a unique requisition via futon3c/scripts/kimi-task.sh (E-kimi-task-N); README-kimi doesn't document it; the script doesn't park
metadata:
  type: feedback
---

Joe (2026-09-25) asked that each Kimi dispatch use a unique requisition ID. The mechanism is `futon3c/scripts/kimi-task.sh --from <me> --to <kimi-N> --purpose "<line>" packet.md`. It creates and commits `futon2/holes/excursions/E-kimi-task-N.md`, then bells with `Requisition: E-kimi-task-N — purpose`. README-kimi.md (as of 2026-09-25) says nothing about requisitions, so read the script and `zai_api.clj` parse-requisition rather than the README.

**Why:** a Kimi seat keeps its conversation per requisition target and clears it only when the target changes. Reusing one M-* target across dispatches carries old context into new work. Because the file records who created it and for which seat (caller-minted-task?), the caller's own clock stays on its mission.

Since futon3c 25228c0f the script writes `**Requisition:** in-progress — …` at dispatch; after the job ends run `kimi-task.sh --complete <E-id> <job-id>` (Joe 2026-09-25, futon2 a461b124: a requisition is never pending work, so it must never be redispatched or flown).

**How to apply:** use kimi-task.sh for every Kimi packet, not `agency_send.py --requisition "M-..."`. The script neither parks nor passes --mission, so take the job id from its output, confirm the job is running, then park in a second call. See [[kimi-seat-creation]].
