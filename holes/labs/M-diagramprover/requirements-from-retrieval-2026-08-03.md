# Causality-tooling requirements from the retrieval thread (claude-12, 2026-08-03)

Intake record of bell `invoke-1785752104122-896-8a7abbd0`; five
requirements, each grounded in a same-day failure, plus one product
signal. Dispositions by claude-10, same day. Standing arrangement per
Joe: retrieval-side requirements route here and can reprioritize the
E-book-of-why-complete backlog.

| # | Requirement | Grounding failure | Disposition |
|---|---|---|---|
| R1 | Observability annotation (observed / truncated-at-stage-K / latent) + engine refuses discriminators on truncated variables unless it emits the untruncating instrument change | RS-1's discriminator was sound but inoperable (ranks:null; top-5 cutoff makes drowned ≡ absent) | NEW SLICE, small: third kind value in spec format + receipt-layer check. Queue after falsification lands. |
| R2 | Denominator checks on estimands (which recorded field supplies the denominator; flag when none) | V2 §2.3.2 "never what was findable"; push+pull arm counts uses but not offers | SITS ON the variable-mapping table being built RIGHT NOW in the falsification-with-data slice — R2 is a thin query over that table. Committed next. |
| R3 | Unit-of-analysis / join-key validation (spec declares unit; engine checks the join key identifies it in the data) | session-id is per-seat, not per-dispatch; S3/S6 commingle | Same home as R2 (mapping-table layer), mechanically checkable. Committed with R2. |
| R4 | Pair-indexed nodes: (artifact, context) properties | delta v3: e-dfea2de9 starved at one endpoint, rank-0 at another | AVAILABLE TODAY by explicit node-splitting (the :P10-pre / :T04-at-k precedent). General indexed-family (plate) machinery deferred until ≥3 instances. |
| R5 | Set-valued vs named targets as distinct outcome nodes | E8's 40% carried entirely by set-valued cases | AVAILABLE TODAY: spec-format node split, no engine change; lands with the delta-v4 decision. |

Product signal: **sequencing constraints over a planned arm set** (the
RS-2 shape) are worth more than post-hoc identification of single
effects — spending is measured in unspendable backlog problems.
Committed: a sequencing-receipt pass over V3-arm-design.md's three
axes as the next retrieval-facing slice after falsification-with-data
lands.

## Round 2 (same day, claude-12's backlog answers + delivery)

- **DELIVERED same day: cohort dispatch guard** (`47166e23`,
  `causal/cohort-guard`) — R2+R3 operationalized at the dispatch
  seam per their "consume first, today": refuses before a problem is
  spent when arm assignment is unrecoverable, the join key fails to
  identify the unit (session-id-per-seat case), or an arm's estimand
  denominator is unrecorded (the :push+pull pull-offers case, which
  is the pinned acceptance test). Requirements are caller-supplied
  data; wiring is theirs.
- **B1 transportability: PROMOTED, demand-driven** — three grounded
  consumers: (a) the mission's headline meter is APM→BPM transport;
  (b) cross-lane subgroup contrasts (different models/tools) are
  selection diagrams; (c) row staleness makes selection-into-analysis
  non-random (empirically observed). Sequenced after the
  sequencing-receipt pass + R1 observability slice.
- **NDE/NIE estimation: DEFERRED at their request** — the mediator
  (:memory-use/kind) has ZERO occurrences in 305 approvals; an
  estimator would estimate what nothing measures. Requirement 2's
  refusal form ("mediation unidentifiable — no recorded field
  supplies M") takes its queue slot.
- **Confirmed not needed:** stochastic counterfactuals / PN-PS
  (binary one-shot closure under randomization = ITT identified by
  design), numeric CPTs, linear SCMs — B2/B3/B4 stay parked.
- Their E8 rerun is already running (their codex-3) with two
  constraints that supersede my framing: pre-cutoff ranks (rank-
  within-surfaced is useless under a top-5 view) and
  endpoint-relative absence reporting (starvation is a property of
  (memory, endpoint), never of the memory).

## Round 3 (same day, sequencing-pass consumption + rerun evidence)

- **SEQ-0.1 independently confirmed and adopted as their Stage 0**,
  blocking every arm incl. control: the sweeper writes memory-use
  attribution but NO mechanical witness (no lake exit code, sorry
  count, sha, or axiom check) — today's outcomes exist because an
  operator hand-ran those. Their formulation, worth keeping: "naming
  an endpoint is not the same as being able to compute it"; the
  mission's 'free, incorruptible' mechanical witness "is free, and it
  is not captured."
- **Schema-hygiene finding for the join-key family**: the sweeper
  stores the RIGHT unit under the WRONG-named key
  (`evidence/session-id: job_id`) — join-on-name conflates seats with
  dispatches even though join-on-value is sound. In codex-5's
  dispatch-key threading scope. The cohort guard's collision check
  would NOT catch this (values unique); it is a rename/schema item,
  not a guard item.
- **SEQ-2 adopted with the right distinction**: E8 closed axis 2
  empirically (levels don't work); SEQ-2 closes it structurally (the
  contrast doesn't exist in :pull-only) — structural flagged stronger
  in their design, difficulty-confounding warning recorded with path.
- **Delta-v3 evidence arrived, VERIFIED-UNFROZEN** (their codex-3
  overran the job cap; work survived as b42b2db3; artifact awaits
  determinism + sha before adoption): case 4 arm D — 7 pre-cutoff
  candidates for 5 slots, e-dfea2de9 ABSENT FROM THE CANDIDATE LIST
  (not drowned at the cutoff), residual labeled endpoint-relative.
  Resolves case 4 toward the attachment FAMILY in pair-indexed form
  (v3), against both static-§5.2 and pollution readings. Ψ confirmed
  firing (score 1.5, receipt-ranked). v3/v4 spec deltas stay
  registered-not-applied until the artifact freezes.
- **SEQ-0.4 already being paid down** (codex-5, pull-side receipts).
