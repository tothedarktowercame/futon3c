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
the bridge. Two `matrix_bridge.py` processes were running at 2026-10-14
(pids 413292 from Oct 09 and 3112769 from Oct 13), which may be doubling
dispatch; not touched, Joe's call.
