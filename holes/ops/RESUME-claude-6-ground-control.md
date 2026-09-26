# RESUME — ground control for the codex sorry loop (claude-6 role)

Written 2026-07-29 by claude-6 (Fable) at Joe's direction (Fable usage
96%). Audience: a Claude Opus session picking up this role. The
auto-memory index (MEMORY.md, loaded at your boot) carries the wider
context; THIS file is the operational core. Read these before acting:
`holes/missions/M-codex-sorry-loop.md` (esp. §Standing per-row
protocol, §Horizon), `holes/ops/claude-6.md` (the ⊸ register — read
the last 20 lines), `holes/labs/M-codex-sorry-loop/pilot-1-prereg.md`.

## Identity and comms

- **SUPERSEDED 2026-07-29 (Joe's call): the seat is `claude-9`, not
  `claude-6`.** A nick is a routing address, not a title — it resolves
  to `/tmp/futon-session-id-<nick>` → a resumed pouch, so it cannot be
  inherited by declaration. claude-6's file still points at the spent
  Fable session `d03fcca9`. If you succeed claude-9, do the same
  thing: keep YOUR OWN nick, and repoint the loop rather than the
  session file (two roster entries on one session-id = two pouches
  able to `--resume` one transcript).
- You are whichever claude-N seat you booted as. Your session-id (for
  parks) comes from `GET /api/alpha/agents` → agents.<your-nick>.
- Dispatch via `scripts/dispatch_with_recall.clj` or
  `scripts/agency_send.py` — ALWAYS `--from <your-nick>`. The cron's
  own dispatch seat is hardcoded at `scripts/codex_sorry_cron.py`
  `--from`; that one line IS the succession.
- The ⊸ register keeps its filename `holes/ops/claude-6.md` (role
  register, continuous); authorship does not — write receipts and
  `:resolved-by` under your real nick. See its header note.
- **Every dispatch gets a park** (CLAUDE.md protocol): deadline-ms is
  ABSOLUTE epoch-ms, ≥45 min; awaiting id comes FROM THE BELL/DISPATCH
  RESPONSE, never from a jobs-list lookup (stale-id incident 07-29).
- Read incoming bell headers for the reply-delivery contract
  (CLAUDE.md). Failure bellbacks are FULL wakes: resolve the queue row
  immediately (reset `:untouched` + attempt history for no-output
  failures) or the loop gate-closes indefinitely (15h incident 07-29).

## The loop (runs itself; you are the verification + mining stage)

Cron `30 * * * *` → `scripts/codex_sorry_cron.py` → dispatches ONE
queue row (`data/codex-sorry-queue.edn`) to an idle codex runner with
memory recall. Gates: usage <50% (local rate_limits scan), concurrency,
**verification backpressure** (never dispatches while a row is
unresolved — YOUR pace is the throttle), zai-live guard. Log:
`futon2/logs/codex-sorry.cron.log` (**timestamps UTC**, not BST).

**Per-row protocol at each completion bellback** (full version in the
mission doc — follow it exactly):
1. VERIFY yourself — `lake env lean <file>` in /home/joe/code/apm-lean;
   independent axiom check via a /tmp scratch copy + `#print axioms`
   (acceptance = axiom-clean: no sorryAx; discharged ≠ relocated);
   statement-integrity diff; hygiene (problems/ untouched except the
   target; ConstructionTargets/ ok).
2. RECEIPT outcome half: POST /api/alpha/evidence on :7073, penholder
   `api` (NOT your name — gate allows {joe, api}), author claude-6,
   `:event :memory-use :phase :outcome`, job-id, used/unused ids from
   the runner's Memory-usage section, `:memory-use/error-time` from
   the field log. Health-probe first; write ONCE; verify read-back;
   dedupe rule = earliest per job-id. Outcome classes seen so far:
   :solved :partial :blocked-frontier-named :placeholder-discharged
   (+ :divergence) :substrate-confounded :misdispatch.
3. HARVEST the runner session:
   `cd holes/labs/M-codex-sorry-loop && bb rollout_harvester.bb
   --session <runner session-id from roster> --commit
   --allow-nonfixture` (idempotent). Also harvest
   `futon3c/.state/error-recall/<row-id>.jsonl` into the receipt — note
   the path is under **futon3c**, not apm-lean (corrected 07-29 after
   looking for it in the wrong place; absence of the file is itself the
   evidence that error-recall was not invoked).
4. SCRIBE: bell codex-5 (pass N+1; drafts only, no store writes,
   FULL receipt ids in the bell, evidence ids per draft, inference
   marked). Park on it.
5. RESOLVE the queue row (status/outcome/commit/resolved-at) — this
   reopens the backpressure gate.
6. PROMOTE in batches of ~2 passes: copy
   `promote_scribe_pass_8_9.bb` pattern (deterministic ids, dry-run →
   --commit → idempotency re-run; 409 = success; missing-entity GETs
   take ~17s so 60s timeouts; mint terrain patterns with lexical
   trigger vocabulary when a new terrain shows; RAM caution header in
   the script is binding).
7. ⊸ LOG every wake to `holes/ops/claude-6.md` (register in its
   header; every line carries a checkable ref; MEASURE then write —
   never both in one command; violations counted honestly).
8. Report to Joe: lead with deltas; compact; honest negatives first.

## Live state (2026-07-29 ~14:00 BST)

- Queue 8/83 resolved (4 axiom-clean incl. YoungL2 file at TRUE zero
  + first problem-sorry a95A01; 2 partials; 1 blocked; 2 vacuous).
- Faithfulness ledger (labs/.../faithfulness-ledger.md): 2 entries, 2
  divergence classes; **25% vacuity rate** → S9 static scan (stub
  defs + unbound existentials) is proposed and NECESSARY — pending
  Joe's go; dispatch it as a codex packet when he says.
- Frontiers: 4 open + anchored (argument-principle family DEMAND 3 =
  top of queue; poisson-ae Carleson-anchored; 2 lemniscate), 1
  dormant (integral-minkowski, bypassed).
- Memory corpus: 45 codex-lane memories / 7 patterns; both
  vacuity-QA rules recallable. S8 error-time recall LIVE
  (scripts/error_recall.bb; first field row = 4 invocations).
- Store :7073: restarted 07-29 morning (heap was 8.2G); anon series
  since 1.66→3.26G over ~5h — watch at wakes; sheds-per-hour = early
  signal; NO restarts without Joe. Evidence for upstream: 2M Arrow
  Field retention (ops log 07-29).
- Known recall gaps (do NOT hotfix — versioned, claude-4's interface,
  cohort close): 36-term query semantics; template-boilerplate leak;
  vocabulary extension candidates (Laurent/poles; Riemann-Darboux
  covered by new pattern).

## Pending on Joe's desk (raise gently, don't nag)

S9 vacuity scan (necessary); M-xenotype-its charter confirmation
(holes/missions/M-xenotype-its.md, incl. YoungL2+Schwarz explanation
pilot); Henderson chat confirm (Wed 08-05 10:00, receipt in futon7
outbox — agenda incl. Arrow retention + XTDB issues corpus,
sidecar-indexing of it deferred until store headroom); Epyc/GPU
decision (spot-GPU GLM-Air pilot preregistration offered); scaffold
policy (157 rows EXCLUDED from queue, xenotype-demand-driven proposal
on the table).

## Adjacent lanes (not yours to drive, yours to coordinate with)

- claude-4 = M-zai-learning-loop ground control; cohort-2 counting
  boundary: codex rows NEVER in cohort-2 receipts (join-safe by
  :runner-model stratum). Sat 08-01: zai returns; a96A04 S6 expects
  the now-unconditional Young corollary; the recall-pipeline
  investigation is queued with them; bell them at cohort close.
- codex-4 = infra builder; codex-5 = scribe seat (sole); codex-6/7 =
  runners (cron picks).
- Whitepaper: `docs/retrieval-whitepaper.md` (Rob deliverable, send
  gate Joe) — §5 has the cross-model paragraphs; increment log at top.

## Non-negotiables (the spirit of the role)

Author ≠ reviewer: verify every runner claim yourself before
reporting. State what you checked. Honest classification beats good
news. The register catches YOU too (see the 07-29 correction entry).
Runners never hold pens. When in doubt: measure, then write.
