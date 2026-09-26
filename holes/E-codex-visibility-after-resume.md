# E-codex-visibility-after-resume — a successful turn must be observably successful

**Status: IDENTIFY (2026-07-22). Chartered by Joe (emacs-repl, same day): "make a
futon3c excursion … write up the case as a motivating example. This is an
interesting one b/c we'll have to cycle the JVM to try to reproduce it, but I can
pass the work to an external agent." Author of this write-up: claude-2.
Execution: EXTERNAL agent (see §Why external), cycles Joe-gated.**

Sibling of [E-codex-resume-after-JVM-restart](E-codex-resume-after-JVM-restart.md)
(2026-07-04): that excursion's gap was process OWNERSHIP (codex had no supervisor,
so resume was manual). Today's incident shows that gap partially closed — the
serving JVM itself spawned `codex exec … resume <sid>` as a direct child — and
exposes the successor gap: **resume works, and nothing shows it working.**

## The motivating case (2026-07-22, times BST; every claim store- or ps-verified)

The futon3c JVM restarted ~13:00. Desktop-save restore brought codex-2 back with
its session id. A task in claude-1's lane ("mission scope ingest") was then
dispatched to codex-2 three times: a bell (failed at the ~30-min Agency cap), a
retry bell (cancelled +5 min), and Joe's manual repl turn 13:43 ("Can you finish
that task and bell claude-1").

The manual turn ran a real process tree for ~28 minutes — codex binary with live
api TLS connections and 3m16s CPU, two spawned futon3c-classpath JVMs loading
`mission_scope_ingest.clj` — and **delivered**: futon3c commits `1bbd8c7` (13:30,
from the earlier attempts' lane), `0fe209c` (14:06), `5789b86` (14:11), the last
two timestamped at the minutes the processes were observed exiting. Review checks
passed; live acceptance ran.

During those 28 minutes, every observational surface said "dead":

- the codex-repl buffer printed **nothing**, start to finish;
- the job ring had **no record of the turn at all** (operator invoke-stream turns
  to codex never enter it);
- the roster stayed `invoking` after the processes exited — a phantom that would
  have queued all subsequent work, cleared only by a manual registry swap;
- the buffer's underlying curl (`--max-time 1800`) was heading for a silent
  timeout with the reply lost.

Consequence: the operator ("ran 30 minutes printing nothing") and a diagnosing
agent with full system access (claude-2, who published "delivered nothing visible
anywhere" before correcting it) **both misdiagnosed a successful turn as a dead
one**. The truth surfaced through exactly one mechanism: claude-1's park
deadline-wake reconciliation. Full defect taxonomy with evidence:
[TN-invoke-lifecycle-gaps](technotes/TN-invoke-lifecycle-gaps.md) (D1 ledger, D2
roster wedge, D5 buffer darkness are the ones this excursion reproduces).

## Hypotheses to settle by reproduction

- **H1 — the ledger gap is unconditional, not restart-induced.** Operator
  invoke-stream turns to codex create no job record even on a freshly-booted,
  never-restarted JVM. (Today's evidence suggests unconditional; one clean
  pre-restart trial settles it.)
- **H2 — the roster wedge requires turn-death-without-finalization**, of which
  JVM-restart-mid-turn is one path and kill/timeout-during-turn another. Predict:
  restart mid-turn → restored agent stuck `invoking`; clean turn → correct
  `invoking→idle`.
- **H3 — buffer darkness is codex-lane-specific and restart-independent**: zai
  streams every round via `sink!`; the codex lane forwards nothing even though
  `codex_cli.clj` already surfaces `on-runtime-event` (`:output`, tool events,
  byte counters) internally.
- **H4 — the in-memory job ring erases in-flight bell jobs on restart**: a bell
  job running at restart time should vanish from `/api/alpha/invoke/jobs` in the
  new JVM (display-grade ring, the invoke-jobs analogue of the pre-U1 transcript
  gap), leaving the caller's park deadline as the only recovery signal.

## Reproduction protocol (external agent)

Prep: a THROWAWAY long-running codex task (e.g. "run `sleep 300`, then echo a
nonce and write it to /tmp/e-codex-vis-nonce") — nothing mission-bearing; a note
of all observation points: codex-repl buffer, `/api/alpha/invoke/jobs`,
`/api/alpha/agents` roster status, `ps --forest` for the JVM's codex subtree,
and (per turn) whether a terminal reply arrives anywhere.

1. **Baseline, no restart (H1, H3, part of H2):** dispatch the task once as a
   BELL and once as an operator-style invoke-stream turn. At +1, +3, +5 min
   record all observation points. Let both finish. Expected if H1/H3 hold: bell
   → job record, dark buffer; operator turn → NO job record, dark buffer; both
   → roster returns to idle (clean-path H2).
2. **Restart mid-turn (H2, H4):** start one bell task; at +2 min (mid-`sleep`),
   `fdev --restart` [JOE-GATED — announce, get the go, quiet window; do NOT
   touch futon1b-server]. After the JVM is up: record job ring (H4: the job
   should be gone), roster status for the codex agent (H2: predict `invoking`
   or `restored` wedge), whether any process survived the restart
   (tmux respawn kills the pane tree — verify with ps), and whether ANY surface
   reports the orphaned/killed task's fate.
3. **Post-restart re-drive:** send the restored agent a fresh short turn.
   Record whether it resumes cleanly (the 07-04 gap — expect YES now), and
   whether step 2's wedge (if any) blocks it (queue behind phantom).
4. Write every observation into §Findings below with timestamps and raw
   captures (the TN's fixer needs the trace, not a summary).

Two to three JVM cycles maximum; each individually Joe-gated. If a cycle
reveals live work in flight (check the blackboard/roster first), abort and
reschedule.

## Why external

Pouch-resident claude agents run ON the serving JVM's kangaroo pouches — a JVM
cycle kills the experimenter mid-experiment (and backgrounded shells die with
the pouch; see CLAUDE.md §Durable background work). The runner must live
outside the JVM's process tree: a terminal Codex session, a systemd-side
script, or Joe's own shell driving the protocol. Findings can be written to
this file from anywhere.

## Kill / narrowing criteria

If step 1 shows the operator-turn ledger gap is NOT reproducible (a job record
appears), narrow to the restart-dependent hypotheses and say so here — the TN's
D1 then needs re-evidencing. If `fdev --restart` cannot be made safe to run
twice in a working afternoon, reduce to step 1 only (H1/H3 are restart-free and
already worth the excursion).

## Findings

(append here)

## Cross-refs

TN-invoke-lifecycle-gaps (defect taxonomy + acceptance bar);
E-codex-resume-after-JVM-restart (ownership gap, 07-04 — re-verify its status
as a step-3 side-effect); `codex_cli.clj` `on-runtime-event` (the unforwarded
stream); `agent_pouch.clj` (what "owned and observable" looks like for claude);
futon1b/TN-futon1b-memory-incident.md (same-day sibling incident, store side).
