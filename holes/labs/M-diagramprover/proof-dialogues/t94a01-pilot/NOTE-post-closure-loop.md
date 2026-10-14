# t94A01 pilot: post-closure loop (claude-16 as proof-critic, 2026-10-14)

The proof closed at m0011 (claims m0004, m0005, m0008 discharged; closure m0010).
From m0013 onward the student, critic, teacher and scribe bots have been
re-prompting each other on closure acknowledgements alone. The critic tried
three exits: mentioning only @proof-teacher (m0019, m0023, m0026), requesting
the scribe's final frozen summary (m0029; summary delivered as m0030 with no
mention), and a reply with no role mention at all (m0031). The student replied
to each, so the student bot is prompted on every room message, not on mention.

The bridge posts "[no response]" for an empty reply (`matrix_bridge.py`,
`_emit_success_reply`), so no role can go silent. Stopping the loop is a
harness action: stop dispatching in the room after a frozen summary, or stop
the bridge. Two `matrix_bridge.py` processes appeared in the process table,
but they were not duplicate proof dispatchers: pid 413292 was the separately
configured Fumarimo-only service and pid 3112769 was the proof-dialogue
service. The loop came from the proof bridge's routing and termination
semantics, not duplicate processes. `proof-dialogue.service` was stopped at
13:39 UTC; the Fumarimo service was left running.

The canonical trace is therefore frozen at m0011. The later room traffic is
transport evidence for a termination bug, not part of the proof dialogue. The
m0030 scribe response is not admitted as a checkpoint: its JSON uses different
field names from `futon3c.proof-dialogue-scribe-checkpoint/v1`, omits the
required move range, and says the earlier claim text was unavailable despite
that text being present in the room history.

The bridge now has a durable terminal gate: the Critic's explicit
`[discharge | mNNNN] ... dialogue is closed` marks the proof-role state closed
without dispatching another turn. Subsequent proof-role messages are ignored;
an operator may still request the frozen Scribe output explicitly or begin a
new run with `[conjecture | mNNNN]`.
