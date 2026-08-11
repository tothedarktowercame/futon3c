# E-batch-operator-runbook — running A/B batches as ground-control's operator

**For the Opus batch-operator seat on Zone (claude-3). Written 2026-08-10 by
ams-claude-1 (Fable), who ran batch-1 by hand; this manual encodes that
night's lessons. Supervisor: claude-1 — you liaise with them after every
batch and whenever the ESCALATE rules below fire. Operator ≠ designer: you
run the ballet; Fable and Joe change the choreography.**

Read first: `batch-1-prereg.md` + `batch-1-report.md` (the shape of a batch),
`E-memory-whitepaper-v3-programme.md` §Phase-2 + §Frames + §Generic proof
plans (why the ballet is shaped this way), `E-ground-control-pass-to-zone.md`
§5 (operational cautions — all still binding).

## Your standing loop

**Dispatch** (batch open, after supervisor hands you a prereg):
`./scripts/batch1_pair.sh <pid>` per panel problem (clone/adapt per batch id;
branch names must be batch-qualified — known frames.bb gap). First pair
first; remaining pairs only after the first pair closes clean.

**Per completion bellback** (jobs arrive as turns; one job = one loop):
1. VERIFY: recompile in the frame worktree (`lake env lean <target>`;
   PATH needs `~/.elan/bin`); confirm frozen statement untouched; confirm
   live apm-lean clean (`git status` — you NEVER commit there);
   sorry-count grep (a comment saying "sorry" is not a sorry — check
   compile warnings, not grep alone).
2. INTERVIEW the seat immediately, `--mode brief` (work-mode marks
   no-execution interviews failed — cosmetic, but brief avoids it):
   what did you need; what did you search and miss (exact queries); what
   would have made this one-pass; (mem arms) honest counterfactual on any
   USED/surfaced memory; any glue that felt familiar.
3. SWEEP: `futon6/.venv/bin/python scripts/memory_outcome_sweeper.py`
   (idempotent; ctl arms legitimately produce nothing).
4. CLOSE the frame: receipts via `/tmp/receipts-for-job.clj <job-id>`
   (NEVER "latest receipt" — interleaved dispatches broke that once);
   ctl arms with no outcome half use `none:memory-channel-none`;
   interview slot gets the REAL interview job-id; axioms verbatim to a
   file; obstruction stated in the runner's own words.
5. FILE to the store (EDN POST, x-penholder api, session-id
   "vote-and-callback-pipeline", append-only, deterministic-ish ids):
   concept-votes (one per problem per concept; supplements for demand
   queries/specs/stepping-stones), glue observations, use adjudications
   (with twin timing from the job ledger), retrieval misses. Copy the
   existing rows' schemas exactly — grep tags :concept-vote :glue-census
   :use-adjudication :retrieval-miss for live examples.
6. TWIN-DIFF when a pair completes:
   `git -C apm-lean diff exp/<pid>-mem exp/<pid>-ctl -- <file>` into
   `data/experiment-frames/<batch>/twin-diff-<pid>.txt`.

**Batch close**: `frames.bb validate --batch <id>` must exit 0; then draft
the report scored against the prereg (batch-1-report.md is the template:
headline vs falsifier FIRST, stratum accounting, noise-floor read, residue
battery, recommendation) and BELL IT TO claude-1. You draft; Fable
verdicts. Do not push the report until Fable reviews.

## Hard rules (violations are incidents, not style points)

- One delivery channel per job: bellback only. NO parks on jobs (the
  double-harvest lesson). Parks are for concept callbacks, and only the
  supervisor completes concept deps.
- Fingerprint standard: a USED claim counts only when corroborated by the
  committed artifact. Self-report is design signal, never outcome data.
- Frozen statements are immutable; frames never touch live apm-lean; you
  never merge, never push to apm-lean, never edit futon3c src/, never
  restart services, never register agents, never eval via Drawbridge.
- Interviews while the seat is warm (immediately after its job).
- Every number you report: say what you CHECKED, not what you concluded.
- Read "empty" as a fact about your query first (the standing §5 caution).

## ESCALATE to claude-1 (bell, don't improvise) when:

- anything contradicts a filed verdict or the prereg's predictions;
- a frame won't close cleanly after one repair attempt;
- a runner touches anything outside its frame, or a frozen statement
  changes, or an axiom output shows anything beyond
  propext/Classical.choice/Quot.sound/sorryAx;
- a fix you need is not in this manual (the permission onion, the pouch
  twin, and the receipts bug were all novel-snag days — the rule is stop
  and bell, not improvise);
- batch report drafted (always); merge decisions (always — you prepare
  comparison tables, Fable adjudicates); anything touching the vote
  pipeline's build/callback side (always).

## Liaison rhythm

After each batch: your draft report + anomalies + proposed next-panel by
the fixed rule → bell to claude-1. Fable returns: verdicts, merges,
ladder-rung choice, prereg for the next wave. Between batches you may be
assigned: census re-runs, merge-table prep, scribe-pass triggering
(scribe output goes to claude-2 for review — never approve attachments
yourself; operator ≠ reviewer ≠ scribe).

**Retraction convention (2026-08-11):** the store is append-only, so a
retracted claim's original row still answers tag queries. Every retraction
therefore gets a companion RETRACTION-MARKER row carrying the ORIGINAL
row's tags plus :retraction, so naive tag queries surface the void notice
adjacent to the claim. Proposed by the operator; adopted.

**Escalation delivery (2026-08-11, learned at cost):** a reply inside a
bellback turn routes to auto-bellback — the void. An escalation exists
only when sent as an explicit bell to claude-1. Bell FIRST; the
in-turn reply is optional commentary. Three hours of a decisive-pair
halt were lost to this gap.

**Traffic discipline (2026-08-11, Joe's stop-the-line):** the target is
ONE bell per batch — the liaison report — plus true blockers. A bell is
warranted ONLY when you cannot proceed without a ruling (a BLOCKING
escalation: dispatch gated, frame won't close, verdict contradicted on
the critical path). Everything else — corrections to your own earlier
claims, status, findings that don't gate the next action, test
verifications, confirmations — goes into a running HELD-ITEMS section
of the draft report and arrives at batch close. If a held item later
becomes blocking, bell it then, with the accumulated context. No bell
is ever just "an update"; the supervisor's silence between batches is
the system working. (Today's calibration: of ~10 operator bells, three
were blocking; the rest were report material arriving early.)
