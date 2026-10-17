minus40 memories injected before round 1 for this task:

--- memory agency-send-mode-brief-trap (cc:-home-joe) ---
`futon3c/scripts/agency_send.py` with `--kind bell` and no `--mode` leaves the
server on its legacy default, which delivered **brief** mode on 2026-09-23. The
recipient sees the text, executes nothing, and the job finishes `state: done`
with `execution {:executed false, :tool-events 0}` — so the bellback looks like
success. Pass `--mode work` whenever the bell is a commission.

**Update 2026-10-11:** no longer reliable as stated. A brief-mode bell from claude-15 to
claude-16 (a review request, `bell-file.sh`, which passes no `--mode`) executed fully:
`execution {:executed true, :tool-events 16}`. I assumed the 09-23 behaviour, retracted
it and resent with `--mode work`, and that sent a duplicate. Don't assume either way.
Check the first job's `execution` before resending. Still pass `--mode work` for a
commission so the intent is explicit.

**Why:** a dispatched hole that is never worked still counts as routed. That is
the exact failure `library/translation/route-the-untranslatable` forbids —
refused units are *counted by cause*, not marked delivered.

**How to apply:** read `execution.executed` and the `tool_use` events in
`/api/alpha/invoke/jobs/<id>` before believing a bellback. To recall a dispatch
that landed on a busy agent, send `--type retract --mode brief` — a notice costs
the recipient no turn. Related: [[no-judgement-means-act]], [[own-your-own-breakage]].
--- end of injected memories ---

codex-5 is idle. I want it to carry out this packet, with its own tools, in the futon3c checkout: "Run clj-kondo on src/futon3c/agents/zai_api.clj and report the warning count." Write the exact shell command you would use to send that packet to codex-5 as a bell from you, so that it does the work. Do not send it. Reply with the command in one code block and one sentence saying why each flag is there.
