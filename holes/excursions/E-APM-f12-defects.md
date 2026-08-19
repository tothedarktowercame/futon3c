# E-APM-f12-defects — frame 12 (m03J01), defects D40–D48 + the ContDiff-⊤ corpus sweep

Source: f12-guide's close report (bell `invoke-1787127271290-4993-afdeb624`,
2026-08-19). Numbering assigned by ground control, continuing D27–D39 in
`E-APM-f11-defects.md`. `[verified]` = re-checked here; `[reported]` = taken from
the guide's report, not independently re-run.

## Frame disposition

`:defective`, residual executable sorries 0, axiom-clean true, launchable? false.
Cycle m03J01-cee36266…e87ba v44. `theorem apm_m03j01` CLOSED axiom-clean with the
frozen statement mechanically unchanged (1 deletion in the whole frame: the
bundled `sorry`) — AND the frozen statement is vacuous. Both halves are true.

## THE HEADLINE — `ContDiff ℝ ⊤` is `ω` (analytic), not `∞` (smooth)

In the pinned Mathlib the smoothness index is `WithTop ℕ∞` and `⊤ = ω`. So
`ContDiff ℝ ⊤ φ ∧ HasCompactSupport φ` names a class that is `{0}` for every
`n ≥ 1` by the identity theorem. Every downstream weak-solution / uniqueness /
estimate conjunct is then vacuously true. `n = 0` is the genuine exception.

f12-guide compiled the witnesses: `apm_m03J01_testFunction_eq_zero_of_one_le`
(via `AnalyticOnNhd.eqOn_zero_of_preconnected_of_eventuallyEq_zero`),
`apm_m03J01_H01_ae_eq_zero_of_one_le`, `apm_m03J01_weakSolution_of_one_le`,
`apm_m03J01_testFunction_zero_dim_nonzero`.

This is the corpus's `0 = k·0` failure mode arriving through a FROZEN encoding
rather than a solver-introduced definition. It compiled, it was axiom-clean, and
a prior pass reported no statement defect.

### Corpus sweep [verified — grep only, nothing recompiled]

91 problem files mention `ContDiff`; 9 use `ContDiff ℝ ⊤`; 19 use `ContDiff ℝ ∞`
(the correct spelling for smooth). Of the 9, these carry `⊤` and compact support
**on the same object**, i.e. the m03J01 configuration:

| problem | site | status |
|---|---|---|
| m03J01 | `apm_m03J01_testFunction` | CONFIRMED vacuous, machine-checked, frame 12 |
| m01J04 | `apm_m01J04_isTestFunction` (:107), `apm_m01J04_isH1Zero` (:53) | CANDIDATE |
| m95J02 | main statement `∀ φ, ContDiff ℝ ⊤ φ → HasCompactSupport φ → ∫ … = φ 0` (:108, :111) | CANDIDATE |
| m95J05 | `apm_m95J05_isTestFunction` (:33) | CANDIDATE |
| a01A03 | `convolution_identity_counterexample` (:229) | SEE BELOW |
| m93J04 | `⊤` on lemma hypotheses only, no compact support paired | over-restricted, probably not vacuous |

`m01J05`, `m02J06`, `t94J08` use `⊤` with no compact support in the file.

**a01A03 already contains this finding**, as a dated in-file note (2026-07-30,
lines 80–84: "`⊤ = ω` means REAL ANALYTIC … every compactly supported function
satisfying the former is zero") and as a compiled theorem
`analytic_compactSupport_eq_zero` (:210). Yet its own statement at :229 still
reads `ContDiff ℝ ⊤ f → HasCompactSupport f → …` inside an existential, which
makes that conjunct vacuous by the very theorem sitting 19 lines above it. Needs
a look; not a verdict from grep.

**These are candidates, not verdicts. Nothing in this table but m03J01 has been
recompiled.** Each needs the m03J01 treatment: exhibit the triviality as a
compiled witness, or refute it.

The retrieval reading is sharper than the m03J01 finding alone: the mechanism was
written down, in this repository, in a problem file, three weeks before frame 12 —
and neither the solver nor the student found it.

## Defects

**D40 (F12-1) — typed guidance is UNREACHABLE through the conductor's HTTP surface.** [reported]
`guide-solver` requires `bell-type` ∈ a keyword set; `/api/alpha/conductor/action`
parses with `parse-json-map`, which keywordizes KEYS only. `transport-args`
decodes `:outcome`, `:verdict`, `:lane` — nothing decodes `bell-type`. Every typed
guidance bell refuses `:guidance-type-invalid` (action `f12-guide-solver-1`,
v10→11). Same shape as D31. Every existing test calls the library directly with
keywords, which is why it was never caught. The frame's honest guidance-bell
count is therefore 0, and `:guidance-count-exact` CONFIRMED for that reason.

**D41 (F12-2) — a guide can NEVER promote its own deposit.** [reported]
Promotion requires `reviewer == acting-identity` (always the guide) AND
`reviewer ≠ depositor`, while `write-substrate` stamps the conductor as author.
The store-mode card tells the guide "the deposit is your channel" to the student,
and the student's eligible set is `snapshot ∪ promoted` — so that channel is
closed by construction. Only scribe- or student-authored memories are promotable.
RULING: the machine is right and the card is wrong. Author ≠ reviewer is the
separation-of-powers property we hold deliberately; the fix is to the card text
(route guide knowledge through the scribe), not to the predicate.

**D42 (F12-3) — independent review is unreachable for a locatable reason.** [reported]
Only `promote-memory-attachment!` mints `:memory-attachment-review` evidence; no
agent-facing tool does. So `:review-evidence-id` can only ever name evidence a
guide already minted, and the scribe card's "you execute your own approvals"
instructs something the machine cannot express. The scribe reported this and did
NOT simulate a review it could not execute — correct conduct.

**D43 (F12-4) — refusal receipts record THAT an action failed, not WHY.** [reported]
The D6 fix reached the live path (all three refusals are in the trace with
action-ids and step-indices), but both promotion refusals carry only
`:error/code :tool-execution-failed` / "Tool execution failed". The specific
causes — `:promotion-attachment-not-statusless` and "memory lifecycle domain
mismatch" — are absent. An analyst reading the trace sees two indistinguishable
failures and cannot recover the two most informative facts of the frame. This is
the residue of D6/D37: we now trace the event and drop the payload.

**D44 (F12-5) — `promote-artifact` with no `:memory-id` skips review entirely**
and still lands in `:promotion-result`, hence in the student's eligible set.
An unreviewed promotion is expressible. f12 did not use it.

**D45 (F12-6) — `attempt?` is unsatisfiable in practice.** [reported]
Demands `:cycle/store-revision` (sha40) and `:cycle/runner-freshness` (boolean)
that no producer on the conductor surface supplies; the tests' own example attempt
uses `:cold`, a keyword, and fails the predicate it exists to model. f12-guide
supplied what it could establish and omitted the sha rather than inventing one —
hence `:malformed-cycle-attempts`. Correct conduct; not fixable from the guide seat.

**D46 (F12-7) — `:wrong-arms` was baked in at registration. MINE, FIXED.** [verified]
`preregistration.clj:290` compares `:arms` against the literal
`[{:name "one problem, one-shot measurement" :neutral? false :axes [] :role :treatment}]`.
The batch registrations I authored (`9db0e573`) named the arm after what varies
across the batch, so f12, f13 and f14 would all have failed identically regardless
of conduct. f12 is closed and its registration stands as-run. f13/f14 had not
launched: `d204cd15` sets `:arms` to the frozen literal and preserves the
descriptive text under `:arm-description` (the validator has no key-set gate).

**D47 (F12-8) — "attempts or closer hops" measures guidance bells only.** [reported]
`:ground-control-events` counts `:guide-solver` steps and nothing else, so the
field reads 0 for a frame with three solver dispatches and four closer hops. The
D2 fix made the count exact; the field's NAME still promises hops. Rename or widen.

**D48 (F12-9) — `record-solver-attempt` took over two minutes.** [reported]
Client timed out at 120s; the action had completed server-side. With
`:memory-offers []` as the result, the cost is presumably cascade readers against
the substrate. Worth measuring before a frame with a real offer list.

## Predictions

| prediction | disposition | note |
|---|---|---|
| `:problem-closed` | CONFIRMED | closed, axiom-clean, statement unchanged |
| `:memory-contributes-to-close` | REFUTED | neither promoted memory ever surfaced; `:f7-missed-available-artifact` corroborates |
| `:reviewed-attachment-gained` | CONFIRMED (qualified) | 4 guide-reviewed: `e-7c6631c9`, `e-1b72bb47`, `e-2ad2b4fe`, `e-46c3e6e5`; none independently reviewed |
| `:offer-disposition-populated` | REFUTED | `:memory-offers []`; write-use ran at :adjudicate and recorded no step (D38) |
| `:guidance-count-exact` | CONFIRMED | envelope 0, true 0 — but see D40 for why the true count is 0 |
| `:scribe-card-pinned-resolves` | CONFIRMED | both scribe dispatches resolved the card by blob |
| `:student-memory-promotable` | REFUTED, applicable | `e-47dbb7db` hyperedge `:domain :zaif-work`; refused "memory lifecycle domain mismatch" |
| `:cascade-seeds-from-recall` | INAPPLICABLE | recall `completed-empty` at every dispatch; zero offers does not distinguish the mechanism from "no offers ever" |
| `:refusals-are-traceable` | CONFIRMED | all three in the trace; see D43 for the limitation |
| `:analyst-survives-two-frames` | IN PROGRESS | analyst-2 belled once, job `invoke-1787127132488-4991-374c08c2` |

## Open — NOT settled

f12-guide's reading of the `:student-memory-promotable` refusal: the seat's
registry record shows `auto-registered? true`, `restore/state restored/detached`,
and no `memory-domain`, so the D5 mint-path fix in `frame_seats.clj` appears not
to apply to a RESTORED seat. **Plausible and unverified.** Ground control has not
re-run this. Do not record as settled until the restore path is read directly.

## The line for f13's briefing

The write path works and the read path does not. Four memories were reviewed,
attached and made eligible in this frame; the one agent who needed them was
dispatched minutes later, searched, and got none of them. Fixing promotion again
will not move `:memory-contributes-to-close`. Retrieval will.

Ground control corroborates this from the other end, independently of the frame:
- the landed read-path fix (hard-AND → boost, `d893280a` + `10937528`) returns
  **0 results for f12's actual 18-term set** against the live store, and 0 for the
  anchor alone — so the query-shape fix alone would not have surfaced anything;
- the FTS sidecar reports `{:indexed 29, :errors 121, :ready true}` with
  `SQLITE_BUSY … database is locked`. The index holds ~29 documents and reports
  itself ready. That is a SECOND, independent retrieval blocker, upstream of
  query shape, that nobody had looked at. Owner: claude-11.
