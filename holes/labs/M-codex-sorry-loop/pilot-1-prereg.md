# M-codex-sorry-loop pilot lane — preregistration (written before first dispatch)

Date: 2026-07-28, claude-6 (ground control, this lane). Preregistered
BEFORE any pilot dispatch, per the loop discipline. Amendments must be
dated and land before outcomes.

## Lane identity

- Lane: `codex-pilot-1`. Stratum: `:runner-model :codex` on every
  receipt and row. Rows in this lane NEVER count toward
  M-zai-learning-loop cohort-2 (its counting boundary is unchanged).
- Seats: runner = **codex-6** (uninvolved instance); scribe =
  **codex-5** (S4); ground control = claude-6 (dispatch, witness,
  receipts both halves — outcome half unconditional); operator = Joe.
- Witness: `lake env lean <file>` exit code + sorry count + error
  count, re-run by ground control; statement-integrity diff against the
  preregistered target statements (a weakened statement = corrupted,
  not solved).
- **Amendment (2026-07-28, after row 1, before row 2 — witness
  semantics)**: when a proven declaration *invokes* a sorried one,
  local sorry count overstates progress (sorryAx flows through).
  Verification therefore adds an axiom-level check (`#print axioms` or
  equivalent) on each declaration claimed proven, and the row
  distinguishes **`:sorry-discharged`** (axiom-clean) from
  **`:sorry-relocated`** (conditional on a sorried dependency). Row 1
  retro-classified under this rule: `eLpNorm_two_sub_right` =
  discharged; the contraction corollary = relocated. Mirrors
  claude-4's cohort-2 adoption (`c22ea84a`) — both loops now count the
  same way.

## Problem set (fixed order, no retries within the lane)

From the S2 census (`99cc014e`), ranked by the four criteria — S2's
preregistered three (problems unblocked, statement extractability,
Mathlib proximity) plus claude-4's downstream-recall-reachability,
adopted at this layer (amendment dated 2026-07-28, before outcomes;
probe: only Young shows non-transcript store hits, incl. a
`:pattern-outcome` receipt on its absence-memory — others zero, so S2
order stands):

1. `young-convolution-L1-L2` — `YoungL2.lean` (2 sorries, L49 + L61);
   unblocks a95J08, a96A04(b,c). TIME-CRITICAL: land before Saturday's
   cohort-2 S6 (agreed with claude-4; S6 checks YoungL2 state and
   records builder provenance).
2. `schwarz-equality-case`
3. `connectedComponents_complement_lemniscate_le` (a00J04, a01A08)
4. `rouche-root-count-transfer` (a92J05)
5. `radial-integration-r3`

Rows 2–5 dispatch only as the window allows; an undispatched row is
`:not-dispatched`, not a failure.

## Outcome taxonomy (typed, per row)

`:solved` (0 sorries in target, compiles) / `:partial` (sorry delta
< 0, compiles, statements intact) / `:failed-by-mechanism` (typed:
tool-round exhaustion ≠ self-termination ≠ quota/job-cap kill) /
`:substrate-confounded`. Misdispatches (ground-control error) are typed
as such and re-dispatched — that is not a retry.

## Protocol per row

1. Dispatch via `dispatch_with_recall --problem <id> --to codex-6
   --from claude-6 --mission M-codex-sorry-loop` + subjects from the
   census row; full work packet on stdin (target statements verbatim,
   file locations, hygiene rules, validation command, pacing note for
   the ~30-min job cap: "commit an honest compiling partial by ~20
   min", summary contract: Memory usage section naming used/ignored
   surfaced ids + why, and an error→fix work-log section).
2. Offered receipt half written by the dispatch script; outcome half
   written by ground control at verification — UNCONDITIONAL.
3. Hygiene (binding): construction lemmas live at repo root or `lib/`;
   never write under `problems/`; never touch a `problems/<id>/` dir
   while a zai session is live on it.
4. Post-session: rollout harvested (S1 harvester; reasoning summaries
   now enabled via `model_reasoning_summary = "detailed"`, 2026-07-28);
   scribe pass (codex-5) after ≥1 row completes.
5. Held-out (Berkeley) untouched — no exceptions.

## S6-frontier-1 (amendment, 2026-07-28, before dispatch)

Frontier session on `integral-minkowski-eLpNorm-bochner` /
YoungL2 completion. Runner codex-6 (seat unchanged). Target:
`young_convolution_inequality_L2` proven → YoungL2.lean at **zero
sorries, axiom-clean** (`#print axioms` on all three declarations must
show no `sorryAx` — the discharged bar, not the lexical bar). Two
preregistered routes, runner picks and reports: (a) the file header's
own elementary strategy (weighted Cauchy–Schwarz + the now-existing
`eLpNorm_two_sub_right` + Fubini) — preferred, closes Young directly;
(b) the general integral-Minkowski lemma, literature-scaffolded (Schep,
duality + Fubini–Tonelli + Hölder), at root/lib — heavier,
reusable. Outcome taxonomy as rows; `:solved` here means axiom-clean
zero. Receipts unconditional; recall on (same subjects as row 1 — the
row-1 memories should surface again, plus row 1's own receipts now in
the corpus). Secondary observable: does the fresh session's rollout
carry plaintext reasoning summaries (config-fix efficacy test).

## Measurements (declared before outcomes)

- Per-row: witnessed outcome, sorry delta, wall time, receipts pair.
- **Cross-model transfer** (the lane's headline question): for each
  surfaced zai-mined memory — used/ignored per the runner's own
  citation, verified against behavior (did the cited redirect appear in
  the work?). A used zai-mined memory in a codex session = the
  portability observation.
- Lane exhaust feeds standing meters unchanged: receipts → WS3 Ψ-v2
  harness; mined memories + typed edges → WS2 connectivity meter.
- Expected honest outcome at this scale: single-digit rows; no
  comparative policy claims; the chain artifacts (memory →
  construction → unblock) are the deliverable.
