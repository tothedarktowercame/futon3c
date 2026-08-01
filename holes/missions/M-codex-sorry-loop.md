# M-codex-sorry-loop — the learning loop on a second runner and a second work class

Date: 2026-07-28
Status: **CHARTERED at Joe's direction** (this conversation: "produce a
codex-verbose version…, turn on memory use, add an outer loop for mining
the codex transcripts, and get codex set to work patching the sorries").
Window: **4 days** (the zai quota window; codex ≈97% free). Owner:
claude-6 (ground control for this lane; architecture, review, wiring).
Dual payoff: a demo that the memory system is runner-model-portable, and
material progress on Joe's prelim-completion project.

## Grounding facts (recon 2026-07-28, all verified live)

1. **codex-verbose already exists on disk.** The Codex CLI writes full
   rollout JSONL per session (`~/.codex/sessions/YYYY/MM/DD/rollout-
   <ts>-<session-uuid>.jsonl`), and the uuid IS the Agency session id
   (verified: codex-4's evidence session `019f8b63-a009-…` = rollout
   filename). Census of that session: 8,890 events — 1,818 reasoning
   items, 1,206 function_call(+output) pairs, 657 custom tool calls,
   396 messages, 273 patch_apply events, 85 turn contexts. Denser than
   zai's turn-rounds. S1 is therefore an **ingester**, not a capture
   system.
2. **The work supply is real**: apm-lean has 348 `sorry` occurrences in
   142 files; the curriculum lane already promoted "missing dependency →
   construction target" (first: local Young inequality — YoungL2.lean
   sits at 2 sorries, unblocks a95J08 + a96A04).
3. **Dispatch is runner-agnostic**: `dispatch_with_recall --to codex-N`
   works today; recall is pushed in the packet; runners touch no store.
4. **The scribe seat is already codex**; the witness (`lake env lean`)
   is model-blind; receipts are written by ground control (outcome half
   now unconditional, per claude-4's 07-28 protocol change).

## Experiment discipline (what keeps this the SAME experiment)

- **Separate preregistered lane** with a `:runner-model` stratum —
  codex rows never enter cohort-2 (zai prereg comparability preserved).
- **Seat separation by instance**: runner ≠ scribe ≠ reviewer (e.g.
  codex-4 runs, codex-5 scribes; claude-6 ground-controls and reviews).
- **Held-out set stays held out** (Berkeley; no exceptions for codex).
- **Cross-model transfer is a first-class measurement**: zai-mined
  memories offered to codex runners; used/cited receipts test that the
  corpus carries portable content, not model-idiosyncratic phrasing.
  This is the memory-system demo's sharpest claim.
- Receipts as in M-zai-learning-loop; `:recall-system` versioning
  unchanged; Ψ-v2/connectivity meters consume this lane's exhaust like
  any other.

## Slices

### S1 — Rollout harvester — **LANDED + REVIEWED 2026-07-28** [codex-4 `cd21c01`]

Owner review PASS, gates re-run: 86/86 turn rows on the fixture,
live-ingested with read-back, idempotency verified live by reviewer
(re-run: 0 written / 86 skipped); kondo 0/0, parens OK; row quality
spot-checked in the store (tool digests, seq ranges, flagged
truncation, 16KB cap held). **Environmental finding**: codex reasoning
is stored ENCRYPTED-only under the current config (fixture:
1,845/1,845 items encrypted, 0 summaries; confirmed across sessions) —
not a harvester defect; the harvester retains plaintext summaries when
present. **Mitigation applied (claude-6, 07-28)**:
`model_reasoning_summary = "detailed"` added to `~/.codex/config.toml`
(backup `config.toml.bak-20260728`) so pilot sessions carry plaintext
self-talk. Mining impact meanwhile: arc-lane intact (function_call
outputs + patch events), solve-lane intact (messages + commits);
trajectory-lane degraded until summaries flow.

Ingest codex rollout JSONL → turn-round-shaped evidence rows in :7073.
One evidence row per TURN (zai-round granularity; ~85/session, not
8,890), body = that turn's reasoning + messages + compact tool/patch
digest (long outputs truncated — store brown-out history respected).
Idempotent by construction (stable ids from session-uuid + turn seq;
re-ingest creates 0 rows). Dry-run-first contract: file output reviewed
before any `--commit` store write; batched writes, read-back verified.
Packet: `holes/CODEX-HANDOFF-s1-rollout-harvester.md`.

### S2 — Sorry census — **LANDED + REVIEWED 2026-07-28** [codex-5 `99cc014e`]

Owner review PASS: 348/348 reconciled at `26be1cb` (re-counted by
reviewer), ids unique, classes sum (23 missing-lemma / 168 hard-step /
157 scaffold / 0 statement-issue / 0 unclassified), spot-checks against
source pass, apm-lean tracked tree untouched (pre-existing untracked
`.bak` noted). Top-5: Young, Schwarz equality, lemniscate components,
Rouché transfer, radial ℝ³. Fourth-criterion probe (claude-6): only
Young has non-transcript store hits (incl. a `:pattern-outcome`
receipt) → order stands, Young first.

### S3 — Pilot row 1 (Young) — **DISPATCHED 2026-07-28** [→ codex-6]

Prereg written FIRST: `labs/M-codex-sorry-loop/pilot-1-prereg.md`.
Dry-run confirmed recall surfaces the zai-mined absence-memory
(`e-dfea2de9` stop-research-after-repeated-young-api-miss) + the Young
API map — the cross-model transfer test is live. **Misdispatch
incident (ground-control error, receipted honestly)**: first dispatch
(`invoke-…-228`) failed at CLI startup because claude-6's S1 config
append landed inside the `[tui.model_availability_nux]` TOML table,
breaking config parsing for all codex agents ~8 min; no collateral
(only this job failed); config restored from backup, key re-applied
top-level, validated with a TOML parser; outcome receipt half written
(`:classification :misdispatch`, id `0e003dd3`). Re-dispatched as
`invoke-1785240935276-230-26db063f` (running). Lesson → memory:
validate TOML appends with a parser; append lands in the last open
table.

Lexical census of all 348 sorries (hole-count-is-lexical precedent; no
lake builds): classify (missing-Mathlib-lemma / hard-proof-step /
statement-issue / other), extract missing-dependency statements,
cross-ref problem ids, rank by unblock value (Young first per the
curriculum lane), emit top-5 construction-target packets.
Packet: `holes/CODEX-HANDOFF-s2-sorry-census.md`.

### S3 — Pilot dispatches (after S2; prereg before first dispatch)

Preregister the pilot lane (problems = top construction targets;
fixed order; typed outcomes; `:runner-model :codex`; recall on;
receipts unconditional; pacing note for the ~30-min job cap). Dispatch
via `dispatch_with_recall --to codex-N`. Ground truth per the recipe:
witness re-run, commit + statement-integrity check by claude-6.

### S4 — Mining lane — **PASS 1 MINED, REVIEWED, PROMOTED 2026-07-28**

Scribe (codex-5) drafts reviewed PASS; owner promotion pass
(`promote_scribe_pass_1.bb`, dry-run then commit, deterministic ids,
idempotent — re-run 8/8 skipped): **8 memories live** (3 solve, 3 arc
→ `tactic-algebra-interference`; corollary-reduction strategy →
`proof-architecture`; trajectory + frontier →
`missing-dependency-protocol`), each with entry + `:memory/assert`
edge + pudding-warrant review entry, `:runner-model :codex`,
distills → the harvested t007 turn. Frontier promoted as
`:dormant-bypassed` with demand recomputed to 0 confirmed (Young
discharged; a96A04 via unconditional corollary); non-assert edge
proposals retained as body data; cross-memory connectivity via shared
endpoints (frontier + trajectory records carry e-dfea2de9 et al. as
endpoints — first codex-lane cross-links in the measured graph).
Verified visible at pattern endpoints via the recall query path.

Original scope (after S1 + first pilots):

Scribe pass (codex-5) over the ingested turn rows: all four lanes now
possible (solve / arc / trajectory / challenge — the rollout's
function_call_output + patch events are exactly arc-lane food). Typed
edges per claude-4's 07-28 scribe rule from day one — this lane should
be born with cross-links, not backfilled.

### S6 — Literature lane — **FIRST SESSION SOLVED 2026-07-28** (codex-6 `ce77d41`)

**YoungL2.lean at ZERO sorries, AXIOM-CLEAN** — verified independently
by the owner (lake exit 0 no warnings; `#print axioms` rerun on a
scratch copy: all four declarations `[propext, Classical.choice,
Quot.sound]`, no sorryAx; statements diff-verified). Route (a) — the
file's own elementary strategy — won; the integral-Minkowski frontier
was **bypassed, not built** (demand recompute due at S4 promotion; the
L2 contraction corollary is now unconditional, which is what a96A04
needed). Both row-1 statements upgraded relocated → discharged. 3/3
zai-mined memories used again (see ledger). Reasoning-summary config
fix: **ineffective for resumed sessions** (t008 still encrypted-only);
efficacy test moves to the next fresh codex session. First file of the
announcement campaign at true zero.

Original scope:

The deeper sorries sit at the standard→literature transition: rather
than rederiving, retrieve an existing proof and use it as scaffolding.
Frontier records gain an **anchor** field: `:mathlib` (resolve by
import), `:literature` (paper/proof located → scaffolded session),
`:novel` (genuinely open). Protocol per frontier: bounded probe (web +
Zulip archive + local corpora) → anchor recorded as a REFERENCE-type
memory (paper, section, proof technique, sketch) → the deep session's
packet carries the literature proof as its plan. First case validated
by probe 2026-07-28: integral Minkowski is ABSENT from Mathlib (sum
form only — confirms the runner) but has a classical short proof
(duality + Fubini + Hölder; Schep's paper is the cleanest scaffold) —
`:literature`-anchored. Corpus acquisition: leanprover-community
Zulip archive cloning to `~/code/corpora/leanprover-zulip-archive`
(bg-1785242612010-1). Rob's arXiv holding is the scale-up path — and
the concrete joint demo for the whitepaper conversation.

### S7 — The cron loop — **LIVE 2026-07-28** (codex-4 `eacf6c3`; owner review PASS; crontab installed)

Review: all gates re-verified in source + tests rerun (11 passed) +
own dry-run; queue seed verified (83 rows: 4 prereg + 5 missing-lemma
+ 74 hard-step file groups, zero scaffold); template carries
axiom-clean bar + literature probe + hygiene. Manual live cycle fired
cleanly: **row 1 of the cron lane = schwarz-equality-case → codex-7**
(job `invoke-1785246177365-240-fcde4301`), queue row atomically
updated. Crontab `30 * * * *` installed →
`futon2/logs/codex-sorry.cron.log`. Usage signal post-upgrade reads
0% of the new Pro-20x window. Throughput = min(hourly cadence,
ground-control verification) by design (backpressure gate).

Original design:

Continuous operation: `codex_sorry_cron.py` adapted from the zai
prover loop's skeleton (`apm_formal_zai_cron.py` — flock, fail-closed
gates, one dispatch/run). Gates: **usage < 50%** (local
`rate_limits.used_percent` from rollout token_count events — no API
key needed; fail-closed on absent/stale), concurrency (runner pool
excludes scribe + builder seats), **verification backpressure** (never
dispatch while a row awaits ground-truth — the loop cannot outrun
review), zai-live-session guard (cross-lane hygiene). Queue seeded
from the census (prereg rows 2–5 → missing-lemma by rank → hard-steps
by file; scaffold EXCLUDED pending policy). Packet template carries
the axiom-clean bar + the S6 literature protocol (bounded local Zulip
grep, anchor recorded either way). Cadence: `30 * * * *` (hourly at
half-past per Joe; the zai loop runs */15 — densifying is a later
operator call). Owner installs crontab after review + one manual
cycle. Packet: `holes/CODEX-HANDOFF-s7-codex-sorry-cron.md`.

### Standing per-row protocol (continuous mode, Joe's direction 2026-07-28)

Every cron-dispatched row, at the completion wake, ground control runs
the FULL loop — proofs are not the only product:

1. **Verify** (witness rerun, axiom check when any proof cites a
   sorried dep, statement diff, hygiene).
2. **Receipt** outcome half (unconditional, dedupe-aware).
3. **Harvest** the fresh rollout turn(s) (`--allow-nonfixture`);
   check reasoning-summary presence on fresh sessions.
4. **Scribe pass**: bell codex-5 with session ids + commits + named
   targets (drafts-only; the S4 packet form). Every session gets
   mined — solve, arc, trajectory, and frontier/anchor lanes,
   including literature anchors found by the row's Zulip probe.
5. **Promotion**: owner review of drafts, then batched promotion
   (`promote_scribe_pass_1.bb` pattern) — batch per ~2–3 rows or
   daily, under the standing RAM caution, typed edges + cross-links
   included. Queue row resolved only after receipt + harvest are done
   (the scribe/promotion may trail by one batch).

The mining is what compounds: rows 2+ should show the row-1 memories
surfacing via recall, and the meters (receipts → Ψ; cross-links →
connectivity) move with every batch.

6. **Failure bellbacks are wakes too (incident, 2026-07-29)**: a
   cron-row job FAILURE requires the same immediate treatment as a
   success — resolve the row at that wake (reset to `:untouched` with
   attempt history for substrate-confounded/no-output failures; typed
   failure outcome otherwise) + outcome receipt. An unresolved failed
   row gate-closes the whole loop (by design — the backpressure gate
   held 15h overnight rather than pile up; the gap was ground control
   treating the failure bellback as a no-action notification).
7. **Second-derivative register (installed 2026-07-28, Joe's
   direction)**: at every wake, ground control appends marker-fronted
   lines (`⊸fix ⊸miss ⊸win ⊸prop ⊸meter`, each with a checkable ref)
   to `holes/ops/claude-6.md` — the coordination analogue of the
   cohort ops log; the scribe meta-lane parses it deterministically.
   Unmarked load-bearing events are violations, counted at batch
   close. **Propagation rule: every park payload written by ground
   control ends with the ⊸-log instruction including this propagation
   rule itself** — the register survives context loss because the
   wake protocol carries its own reproduction.

### S5 — Meters (continuous)

Every pilot session feeds: receipts → the WS3 Ψ-v2 harness;
mined+wired memories → the WS2 connectivity meter; both re-read at
lane close alongside claude-4's cohort-2-close readings.

## Coordination

claude-4 informed (bell, 2026-07-28); reply resolved three items:

- **Young/S6 collision → PROCEED (decided 07-28).** Cohort-2's S6 row
  (Saturday, zai-5, a96A04) is blocked on exactly the Young L2 case
  this lane will patch first. Decision: patch before Saturday.
  claude-4's RESUME doc (`42e28e5`) makes S6 check YoungL2's state at
  dispatch and record the builder's `:runner-model`. The resulting
  chain — zai-mined absence-memory
  (`stop-research-after-repeated-young-api-miss`, use-factor 1.5) →
  codex-built construction → zai runner consumes it — is the full
  memory→construction→unblock arc, cross-model both directions, with
  provenance at every hop. That chain is the demo.
- **Cross-lane hygiene (binding, into the S3 pilot contract)**:
  construction lemmas live in `lib/` or repo root, never `problems/`;
  never touch a `problems/<id>/` dir while a zai session is live on it
  (Saturday morning especially).
- **Fourth ranking criterion (claude-4's, adopted at the S3-prereg
  layer)**: *downstream recall reachability* — prefer targets whose
  absence-memories already exist in the store, because those close the
  full chain AND their receipts feed Ψ-v2 calibration immediately. S2
  runs to its own 3-criterion prereg unmodified; claude-6 re-ranks
  with all four at S3 prereg, amendment dated. Young scores on all
  four.

The lane's exhaust lands in the same store under the same disciplines,
so all standing meters see it automatically. No other collisions from
the live side: store writes join-safe by stratum, scribe seat is
scheduling-only contention, cohort-2 counting boundary airtight.

**Standing RAM caution (post-OOM, 2026-07-28, Joe's direction) — goes
in every packet that touches the store**: :7073 OOM'd today
(~12:15–12:23 window; one receipt duplicated in flight). All store
writes: check health/vitality first, bounded batches with pacing,
write-once-verify-read-back, stop on 503 (never hammer a browning
store); prefer drafts-as-files with owner-gated promotion over direct
agent writes. Reads: bounded by id or `&limit`, no scans. Bulk ingest
is always a deliberate, explicitly-flagged act (`--allow-nonfixture`
exists for exactly this).

## Horizon (Joe, 2026-07-28): the announcement pairing

Endgame: **"apm-lean fully proved"** paired with the retrieval
whitepaper — the claim being *curriculum-level* (agentic systems pass
PhD prelims), not specific-problem heroics. Announcement gates:

1. **Axiom-clean zero** — no transitive `sorryAx` anywhere in the
   problem corpus (the discharged/relocated counting, corpus-wide).
2. **Statement-faithfulness audit** — every formal statement checked
   against the original exam text, as a reviewed lane (author ≠
   reviewer).
3. **Provenance ledger published in full** — including interventions
   and misdispatches; honesty is the moat.

**Autonomy claim, stated precisely (Joe's formulation)**: operationally
autonomous — zero human-authored Lean, zero human-in-terminal proof
sessions; every proof produced by dispatched agent sessions. Human role
= strategic (memory architecture, literature policy, casting,
promotion gates). Auditable from dispatch records + receipts.
**Demo design**: a fresh BPM problem dropped via API runs the entire
loop untouched (BPM stays held-out from mining — evaluation-only, so
the demo is uncontaminated by construction).

Campaign arithmetic (census `99cc014e`): 157 scaffold sorries =
archival POLICY decision (Joe), not proofs; 23 missing-lemma =
S6 literature lane, demand-ranked; 168 hard-steps = the grind where
the memory loop compounds. Multi-week, multi-window.

**Provenance-hygiene item**: runner commits currently carry Joe's git
authorship (observed on `8208ca7` etc.) — future runner sessions
should set a distinct commit author (e.g. `codex-runner-N`) so git
metadata matches the dispatch ledger; the receipts remain the ground
truth meanwhile.

## Out of scope

Live ranking changes (M-memory-retrieval Interface 1 unchanged);
zai-quota spending; held-out problems; mining zai cohort rows (that
remains claude-4's scribe scope).
