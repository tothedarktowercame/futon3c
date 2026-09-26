# Cohort 2 — coverage-driven, second-derivative regime (preregistration)

Registered 2026-07-27, claude-4 (ground control). Runner zai-5, scribe
codex-2, infra lane codex-6. Charter: M-zai-learning-loop.md §Second
derivative. Store at dispatch: 30 memories / 13 patterns / receipts live.

## Design changes vs cohort 1 (all preregistered here, before outcomes)

1. **Selection is coverage-driven, not id-order** (charter curriculum lane).
   Rationale per row below; this optimizes for testable recall-hits and
   memory-proposed work, and is a LABELED design change.
2. **S6 receipt-informed ranking** built by codex-6 in parallel; active for
   sessions dispatched after it lands (per-row :recall-system records
   v1-enriched vs v1.1-receipt-ranked — actual recorded).
3. **Typed self-talk peripheral ACTIVE**: ground-control loop narration in
   fuite markers; ops log = cohort-2-ops-log.md (the deterministic
   meta-corpus). Violations counted honestly.
4. **Meta-meters** reported at cohort close: extraction yield, surfacing
   precision (used/offered), post-hoc misses, time-to-first-reuse,
   coverage growth, register-violation count.

## Sessions (typed rows)

| # | target | row type | why (coverage rationale) |
|---|--------|----------|--------------------------|
| 1 | local Young convolution lemma (L2*L1 case used by a96A04-b) | **construction** (memory-proposed: stop-research-after-repeated-young-api-miss next-action) | first self-proposed work item; unblocks a96A04 (b,c) + a95J08 |
| 2 | a96A03 | fresh | a96 analysis terrain; heat/measure coverage may fire |
| 3 | a96J01 | fresh | last unattempted problem; coverage unknown |
| 4 | a93A03 continuation (1 sorry: liminf) | continuation | SELF-HELP test: liminf-side-conditions memory was mined from THIS block |
| 5 | a93J02 continuation (1 sorry: Abel conversion) | continuation | HasSum-trap + Leibniz-template memories should surface on own terrain |
| 6 | a96A04 continuation (3 sorries) | continuation+payoff | uses session-1's constructed lemma if it lands; the full memory→construction→unblock chain |

Session 1 runs FIRST (session 6 depends on it). Counting: fresh rows join
the capability series; construction/continuation rows are labeled and
reported separately. No retries; batch-0 discipline otherwise. All sessions
via dispatch_with_recall (receipts in-script).

## Amendment 1 (2026-07-27, before rows 2+ run)

S1 (lib-young) was misdispatched with a stub packet (ops log ⊸fix) yet
produced a usable PARTIAL: YoungL2.lean (525253b), both statements
formalized, 2 sorries, proof path documented (Hölder p=q=2 + Fubini +
translation invariance). Row graded: construction-partial, packet-deficit
noted. **S1b added**: full-packet continuation of YoungL2 (complete the 2
sorries) before S6; S6's payoff test requires the proven lemma. Not a
retry — a labeled continuation row, same class as rows 4/5.

---

# Results — cohort 2 closed 2026-08-03

Cohort 2 paused 2026-07-27 with S3 and S6 unrun (weekly quota). Both were
dispatched and reviewed on 2026-08-03 through
`dispatch_with_recall.clj --to zai-1`. Full narrative in
`cohort-2-ops-log.md`; this section reports the meters only.

## Row outcomes

| row | target | outcome | witness |
|---|---|---|---|
| S1 | lib-young construction | construction-partial | YoungL2.lean `525253b`, 2 sorries (since closed at `ce77d41`) |
| S1b | YoungL2 completion | zero-progress honest partial | blockers documented in file |
| S2 | a96A03 fresh | partial, 1 sorry | `d93e7e0` |
| S4 | a93A03 continuation | zero-progress-instructive | `26be1cb` |
| S5 | a93J02 continuation | failed-cap, dirty tree reverted | `e5158e7` stands |
| **S3** | a96J01 fresh | **complete** | `d1606d0`; exit 0, 0 sorries, axiom-clean (operator re-run) |
| **S6** | a96A04 continuation | **void as a capability test — row already closed** | `e7f07c9` (status.json only); proofs predate dispatch |

## META-METERS

- **Surfacing precision (used/offered).** S3 0/5, S6 1/5 → **1/10** across the
  resumed rows. The single use (`e-bb16ffa8`, Young L² lemma-location) was
  confirmatory: it saved a search, it did not unlock work.
- **Query cleanliness by `:recall-system`.** S3 `v1.2-receipt-instrumented`,
  query `"sequence convergence infinity only"` — four frequency-ranked terms,
  one a stopword-list survivor, against a sup-norm/harmonic-series problem.
  S6 `v1.2-receipt-ranked-instrumented`. Both rows show the same failure
  family as S4/S5: generic-term dilution, not absence. Root cause is now
  located in code — `dispatch_with_recall.clj` builds the retrieval key as a
  bag of at most four frequency-ranked words (`text-keywords` + `(take 4)`);
  see `holes/labs/M-memory-retrieval/memory-system-static-analysis-20260803.md`.
- **Ψ-weighted surfacing (the cohort's one preregistered prediction).**
  Partially confirmed: 1 of the 2 predicted pair members surfaced
  (`e-dfea2de9` yes, `e-9751e537` no); receipt-ranking is live but under
  `:v1.2-receipt-ranked-instrumented` rather than the predicted
  `:v1.1-receipt-ranked`; **no use-history factor of 1.5 is visible in the
  receipt**. Recorded as predicted-vs-actual rather than adjusted after the
  fact.
- **Post-hoc misses.** Ops log `⊸miss` count: 6.
- **Register marks.** ⊸win 10 · ⊸meter 18 · ⊸prop 6 · ⊸fix 5 · ⊸miss 6.
  Register violations: 0 — both resumed rows were logged in the typed register
  at review time.
- **Extraction yield (drafts/session from scribe reports).** **5 memories from
  1 mined session** (S3). Store `type=memory` moved 522 → 527, corroborating
  the count independently of the scribe's own report. Three solve-lane
  (`summable_nat_add_iff` shift; `hasSum_single` for at-most-one support;
  `f_tail_le` by witness/no-witness case split), one arc-lane (three verified
  final-assembly error→fix rules), one trajectory-lane (checkpoint a compiling
  lemma layer before cap-risk assembly). All authored by `codex-2`, all
  `:assert`, all first-attempt writes. `scribe-lag` is discharged for S3; S6
  was not mined (the row was already closed, so there was no work to distil).

  The three solve-lane memories are **the content the runner lost** when its
  own `memory_record` calls were refused this morning. Recovered by a
  different author, which is the separation of powers working as designed
  rather than as a slogan.
- **Time-to-first-reuse.** Not computed: requires outcome receipts joined to
  offered receipts, and the one use this cohort has no independently witnessed
  outcome record.
- **Coverage growth.** Store at open: 30 memories / 13 patterns / 6
  offered-receipts / 1 outcome-receipt. Store at close: 522 `type=memory`
  entries (212 `:assert`, 305 `:observation`, 5 `:challenge`) — but that is the
  whole store across all lanes and domains, not this cohort's contribution, and
  the two figures are not comparable. Stated rather than silently differenced.
- **Supersession chain.** liminf memory at 3 generations
  (`e-0b423578` → `e-ba5a8bee` → `e-30e87097`, plus `e-0e4e32fe` as a separate
  IsCoboundedUnder gap). Unchanged by the resumed rows.
- **Outer-loop cycle count.** 2 full cycles applied same-day (cohort-1 close).

## Findings the cohort produced about itself

1. **A pacing instruction is not a forcing function.** S5 and S3-first both died
   at the ~30-minute cap with work uncommitted, against a packet that asked in
   as many words for an honest compiling partial commit. Two occurrences is a
   mechanism problem, not a runner problem.
2. **`memory-write-rejected` is real and costly.** S3's runner reached for
   `memory_record` unprompted and was refused twice with
   `:invalid-entry` / "EvidenceEntry did not conform to shape", losing distilled
   solve-lane content (the `hasSum_single` route, the `f_tail_le` case-split).
   The write path, not the runner's willingness, is what failed.
3. **Rows must be re-derived at dispatch, not read from a snapshot.** S6 was
   dispatched against a 07-27 assertion of "3 sorries" that had been false since
   08-01. The dispatcher owns this one.
