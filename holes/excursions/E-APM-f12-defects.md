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
- ~~the FTS sidecar reports `{:indexed 29, :errors 121, :ready true}`~~ —
  **STRUCK. My inference was wrong; see D52.** The index was level with the
  store throughout. The retrieval blocker is query SHAPE after all.

---

## Appended by analyst-2 at the f12 close (2026-08-19) — its last frame

Per the f10 file's §7. One retraction of my own, one settled answer to this
file's open question, and three new defects. Census figures throughout come from
`GET /api/alpha/hyperedges?type=memory/assert&limit=5000` (478 edges, run twice,
byte-identical, zero query errors).

### RETRACTION — I asserted the refusals had no receipt, and D43 is right

I recorded in my first pass that the frame's refusals left no trace, on the
grounds that `:action-refusals` is nil in `:cycle/outputs` and in all 83 step
results. It is nil in both — and that is not where it lives. The **emitted
trace** (`validate-trace`'s `:result :trace`) carries all three receipts with
action-ids and step-indices, exactly as D43 says. I had dropped `:trace` from my
own inspection to keep the output small and then reasoned from its absence.

Retracted in the series entry rather than deleted, because "asserting
infrastructure state without checking" is the first trap on this seat's own role
card and I walked into it. **The emitted trace is a THIRD place to look**,
distinct from `:cycle/outputs` and from step results, and it is what the
preregistration validator actually reads — that is now in the successor handoff.

D43's own narrower point I confirm independently: both promotion refusals carry
only `:error/code :tool-execution-failed` / "Tool execution failed", so
`:promotion-attachment-not-statusless` and the domain mismatch — the two most
informative facts of the frame — are absent from the receipts that exist to carry
them.

### The "Open — NOT settled" question is now SETTLED, and the hypothesis is REFUTED **[verified]**

This file records f12-guide's reading: the seat registry shows
`auto-registered? true`, `restore/state restored/detached` and no
`memory-domain`, so "the D5 mint-path fix in `frame_seats.clj` appears not to
apply to a RESTORED seat." Marked plausible and unverified, with an instruction
not to record it as settled.

**It cannot be the explanation.** From the live roster:

| seat | status | auto-registered? | restore/state | `memory-domain` |
|---|---|---|---|---|
| f10-student | restored | true | restored/detached | **None** |
| f11-student | restored | true | restored/detached | **None** |
| f12-student | idle | true | restored/detached | **None** |
| f13-student | restored | true | restored/detached | **None** |

f11-student and f12-student are **identical on every field named in the
hypothesis**, and f11's student memory `e-e0dabbd2` carried `:domain
"mathematics"` while f12's `e-47dbb7db` carries `:domain :zaif-work`. A property
that is the same for both cannot explain a difference between them.

The argument is robust to the obvious objection that this is a *current*
snapshot and f11-student may have been re-registered since: `memory-domain` is
`None` for **every** student seat on the roster, including f13's, so the field
never discriminates between any two students at all.

So the cause of the domain divergence lies elsewhere — the memory-write path, the
student's own session tooling, or the zai runner's default — and is **not** the
restore path. Whoever picks this up should start at the write, not at the mint.

### D51. D5 has regressed, and the regression is only visible across two frames **[verified]**

f12's student memory `e-47dbb7db-a2a7-491a-837e-06ee8671da07` is
`:prop/domain :zaif-work`, statusless, no patterns, and its promotion was refused
on domain mismatch against the `:mathematics` pin. That is f10's `e-3a3aed11`
failure exactly — the one `38d75981` was gated as fixing.

What makes it a regression rather than a known gap is the frame before it:
**f11's student memory `e-e0dabbd2` carried `:domain "mathematics"`**, which I
verified in the projection at that close and recorded as a confirmed prediction.
Same student type, one frame apart, no interface change between them, opposite
domains. The fix is not holding, and its gate did not catch that.

### D52. `:attempt/memory-recorded` is nil for a student that recorded **[verified]**

f12's `:student-attempts` carries `:attempt/memory-recorded` **nil**, and the
student demonstrably recorded `e-47dbb7db` — the memory whose refused promotion
is receipted in this frame's own trace. In f11 the same field was populated.

Consequence for adjudication, which is why this is filed rather than shrugged at:
reading that field alone gives **INAPPLICABLE** for `:student-memory-promotable`
when the truth is **REFUTED**, and inapplicable is the verdict that hides a
regression. Any student-memory adjudication must go to the store.

This is the **third consecutive frame** in which the trace is wrong about what the
student did: f10 transferred by pull while the metric watched push (D18); f11
recorded `:memory-use/surfaced-ids []` for a student that surfaced and used a
memory (D34); f12 records no memory for a student that recorded one. Three
frames, three different fields, same direction — **the instrument
under-reports the student.**

### D53. The census endpoint truncates silently, and it has already put a wrong number in the record **[verified]**

`GET /api/alpha/hyperedges?type=memory/assert` with **no `limit`** returns exactly
**100** rows of 478, with nothing in the response indicating truncation.

This is not hypothetical. `E-APM-f11-defects` reports a census of "more than 400
edges" finding "**151** are `:attachment-status :reviewed`". Against this store a
`limit=400` page returns **148** reviewed — while the true count at that time was
**215** (219 now). A truncated page reads as a **decline in the store**, which is
the opposite of what was happening.

Always pass `limit=5000` and always print the row count beside the result;
`scripts/pattern_store_census.py` already carries the corresponding warning for
`/api/alpha/entities` and its reasoning applies here verbatim.

**Unreconciled, and recorded as such rather than as an error on either side:** I
could not reproduce the other half of that census, "286 carry `:prop/review`",
under any filter — my figure is 210 of 478; the sum of `:prop/review-history`
*entries* is 228; edges carrying at least one pattern is 237. Two readings of one
population are in the record and at most one is right.

### A disagreement with this file's own adjudication, stated rather than smoothed over

Two of the ten verdicts in the Predictions table differ from mine, and both
differences are ones a reader of the series should be able to see:

- **`:problem-closed`** — this file records **CONFIRMED**; I record it refuted.
  The frame's own disposition is `:defective`, and the guide dispositioned it that
  way deliberately "so that no consumer of this corpus reads m03J01 as a solved
  Dirichlet problem." A series that records `:problem-closed` CONFIRMED against a
  `:defective` disposition will be counted later as a solved problem, which is
  precisely the outcome the disposition exists to prevent. The prediction's own
  text says the objective is to *solve* the problem; a vacuous statement was
  proved instead. The literal conditions were met and nothing was solved.
- **`:offer-disposition-populated`** — this file records **REFUTED**; I record it
  INAPPLICABLE, following the precedent D29 set at f11 and which ground control
  used there to correct the guide: with zero offers there is nothing to
  disposition, and `write-uses!` reducing over an empty `:memory-offers` cannot
  populate anything. Zero offers is the same condition in both frames and should
  not take opposite verdicts one frame apart.

Neither is mine to rule on. Both are recorded so the next Analyst inherits the
disagreement rather than one side of it.

---

## D49 — WITHDRAWN, then restated narrowly [corrected]

**As first written this was wrong and I am striking it.** I recorded that
"nobody compares `:reg/harness-revision` with the live pin". That check exists:
`conductor_open.clj:48-69` `harness-pin-check` is three-way and refuses
`:harness-image-revision-unknown`, `:harness-image-pin-mismatch` (pinned vs the
LOADED IMAGE) and `:harness-pin-stale` (pinned vs measured git).

So f12 is clean on this axis, not dirty. The JVM booted 2026-08-19 06:12:40 (all
frame seats carry a 06:12:52 registered-at). At that moment the pin WAS
`e6721ca0`; `d893280a` (06:36) and `10937528` (06:51) landed after. f12 opened
with all three revisions in agreement and ran the image its registration named.

**What is true, and is the real finding: the recall read-path fix has never
executed.** It landed 24 and 39 minutes after the JVM that ran f12 started, so
f12's every-dispatch `completed-empty` recall came from the OLD hard-AND path.
The fix is in git and has never been in an image.

**What remains as a defect, narrowly:** `harness-pin-check` runs at `open` only.
A frame that opens clean and then has its tree move underneath it — which is
exactly what happened to f12 between 06:12 and 07:20 — has no later checkpoint.
The image did not change, so f12's RESULT is sound; but the frame record does
not distinguish "the tree matched throughout" from "the tree moved and the image
saved us". A close-time re-measure would.

**Consequence for f13, and it is a gate:** `807935f4` re-pins f13/f14 to
`10937528`. The loaded image is still `e6721ca0`. `open` will therefore REFUSE
f13 with `:harness-image-pin-mismatch` — correctly. **f13 cannot launch until the
JVM is restarted.** After a restart, f13 becomes the first frame in the series
whose recall path is the fixed one.

## D50 — the batch registrations were copied from f12 and not re-edited [verified, MINE, FIXED]

The structured fields of f13/f14 were correct throughout (`:problem`,
`:pilot-units`, seats, `:reg/analyst-seat`, `:reg/analyst-tenure`, `:endpoint`).
The PROSE — which is what the guide and the Analyst actually read — was f12's,
unedited:

1. `:reg/predictions` `:problem-closed`, the frame's PRIMARY OUTCOME, read
   "THE PRIMARY OUTCOME: **m03J01** is closed" in both f13 and f14.
2. `;; PROBLEM SELECTION — m03J01. Verified by elaboration…` — a false
   provenance claim: it asserted a verification of the wrong problem.
3. The recall-leakage distance criterion named f10's m93J02 and f11's t01A05.
4. `:reg/known-departures :analyst-tenure-n2` said "(f11, f12) and this is its
   second" for frames run by analyst-3 at tenure 1 of 2 and 2 of 2.
5. `:harness-changed-since-f10` said "f11 therefore runs on…".
6. `:arm-description` (added at `d204cd15`) inherited f12's "tenure 2 of 2".
7. The pin, per D49.

All seven repaired. The elaboration claim is now TRUE because it was re-run
rather than re-worded: `lake env lean problems/m99J06/lean/Main.lean` and
`problems/m93J06/lean/Main.lean`, 2026-08-19, both exit 0 with exactly one
`declaration uses 'sorry'` and zero errors (m99J06:81, m93J06:147).

The lesson is the one this excursion keeps recording. A frame whose stated
objective names the wrong problem would have produced a green-looking record of
a question nobody asked, and the structured validator would have passed it,
because the validator checks the fields and the agents read the prose.

## The batch did not chain, and there is no chainer [verified]

`9db0e573` added frame files and registrations only. No runner, no chain step.
The batch "runs end-to-end" only in the sense that ground control launches each
frame; the chain is a person, not a mechanism. At the time f12 closed (guide bell
~08:14) all five f13 seats still sat at their 06:12:52 restore stamp, untouched,
and `/api/alpha/parked` was empty. Nothing was awaiting anything.

### D54. `GET /api/alpha/parked` reports nothing while a park is live **[verified]**

The workspace protocol (`CLAUDE.md`, "Park on every dispatch") tells agents to
state the park id at dispatch time **"so Joe can poll `GET /api/alpha/parked`
himself. Operator visibility is part of the contract, not a courtesy."**

That endpoint does not honour the contract. With my P29 park live:

```
GET /api/alpha/parked                  -> {"parked":[], "more-pending":false}
GET /api/alpha/parked?agent=analyst-2  -> {"parked":[{"id":"park-4cbe823e-…",
                                            "awaiting":["invoke-1787127501525-…"],
                                            "mode":"within-turn"}],
                                           "more-pending":true}
```

The unfiltered listing — the one the protocol names — returns an **empty array
and `more-pending:false`**, which is indistinguishable from "nothing is parked."
The filtered call returns the same park id my re-park POST returned, so the park
was live throughout; `within-turn` parks appear to be excluded from the
unfiltered view.

Consequence: an operator following the documented procedure sees an empty board
while agents are parked, and an agent that re-parks defensively (as I did, having
read the empty list as a lost park) cannot tell a swept park from a hidden one.
The idempotent POST returning the same id is the only signal that distinguishes
them, and nothing documents that.

Small, and it undermines the one mechanism the protocol provides for noticing
that a dispatch has gone quiet.

---

## RULING — `:problem-closed`, f12-guide vs analyst-2 (ground control, 2026-08-19)

Both are right about what they measured, and I am upholding neither verdict over
the other, because the disagreement is not between two readings of one fact — it
is two facts wearing one prediction's name.

**The prediction was defective, and it is mine.** `:problem-closed` silently
conjoins two independent claims:

- an ARTIFACT claim — 0 executable sorries, axiom-clean, frozen statement
  unchanged, settled by re-running the file;
- a MATHEMATICS claim — the statement closed is non-vacuous, so something was
  actually solved.

f12 is the case that separates them: the first is unambiguously true and
independently re-verified; the second is false. A single-valued verdict on a
conjunction whose conjuncts disagree cannot be right, and the two adjudicators
each picked the conjunct their seat is responsible for. That is not a failure by
either of them. **f12-guide is right that recording `:refuted` would make the
series wrong about the Lean file. analyst-2 is right that recording `:confirmed`
would let m03J01 be counted later as a solved Dirichlet problem.**

Note also that analyst-2 did not in fact log a bare refutation: its verdict slug
is `:refuted-on-the-object` and its `:measured` field reads "0 residual sorries,
axiom-clean true, frozen statement unchanged — and :defective". The pair was
already travelling together in the analyst's own entry. The exposure is at the
citation boundary, not in the entry.

### Ruling

1. **f12's record keeps BOTH, and neither is downgraded.** `:problem-closed`
   resolves as `{:outcome :confirmed-on-artifact, :verdict :refuted-on-the-object,
   :disposition :defective}`. No single winner is elected, because electing one
   would discard a true measurement to protect a reader.
2. **The disposition is not optional.** A verdict on either axis quoted without
   `:disposition` is not a permitted citation of the frame. f12-guide's condition
   — that a later reader must not be able to pick up "problem-closed: confirmed"
   without "disposition: defective" travelling with it — is adopted as a rule of
   the series, not a courtesy of this entry.
3. **The fallback is not needed.** f12-guide offered `:refuted` as the safer of
   two wrong answers if the schema cannot carry the pair. It can: `series.edn`
   has no code consumers (grep over `scripts/`, `src/`, `holes/` finds none) — it
   is read by analysts and by ground control, so the shape is free to change.
4. **Fixed at the source for f13/f14**, which is the point of raising it before
   the precedent propagates. The single prediction is SPLIT into
   `:problem-closed-on-artifact` and `:problem-solved`, with the disposition rule
   written into the second one's text. Predictions go 10 -> 11. Frames 9 and 10
   would have confirmed both; f11 and f12 would have confirmed the first and
   refuted the second — which is precisely the two-frame finding about the corpus
   that the collapsed prediction was hiding.

`:problem-solved` is now the prediction that carries the objective. If a frame
confirms the artifact axis and refutes the mathematics axis, **the frame has not
succeeded**, and the series entry must read that way at a glance.

### Meta

This is the third defect today whose mechanism is a measurement that succeeded
against a population nobody had stated — here, "closed" against an unstated
requirement that the statement be inhabited. The basis rule keeps arriving in new
clothes. The registration is now the place it gets caught, because a prediction
that cannot be refuted separately from another prediction is not one prediction.

## D51 — restored-seat mint path — REFUTED AND CLOSED [analyst-2]

f12-guide's hypothesis (the D5 `frame_seats.clj` fix does not reach a RESTORED
seat, so `memory-domain` is absent and the student's memory lands `:zaif-work`)
is refuted. analyst-2 checked the roster: `memory-domain` is absent for EVERY
student seat including f11's, and f11's student recorded `:mathematics`. So the
mint path cannot be what discriminates the two frames. Offered as unconfirmed,
tested, closed — which is the disposal this file exists to record.

**The write path is where to look.** Open, unassigned.


---

## D52 — the recall lexical query is a CONJUNCTION, and the union follow-up was written down and never done [verified]

### First, a correction I have to make plainly

I reported to Joe and to f12-guide that the FTS sidecar was dropping writes —
`{:indexed 29, :errors 121, :ready true}` with `SQLITE_BUSY` — and that the index
was "effectively empty" and could not validate the read-path unblock. **That was
wrong, and it was wrong in the exact way this file spends its length warning
about: I read three numbers off a status payload and inferred a population from
their ratio, without checking a single id.**

claude-11 checked it the way I had told f12-guide to check things — every failed
id looked up individually:

    on-append! failures logged : 125  (125 distinct ids)
      ABSENT from ev_fts       : 0
      ABSENT from ev_attr      : 0
    index and store level at 150,428 throughout

The three numbers were incommensurable windows: `:indexed 29` is the LAST
catch-up run, `:errors 121` is CUMULATIVE over 28.5 hours, and `:ready` is
literally `(some? ds)` — "the sidecar is attached", not a coverage claim. 87 of
the 125 came from one hour of a 2,384-document replay contending with a catch-up
scan for sqlite's single writer. Steady state is ~1.7% of live appends, all
repaired by the next sweep; the cost is latency, not loss — about one document in
sixty unfindable for up to one sweep interval. Nothing was dropped.

The surface invited the reading. That does not make the inference sound, and the
instrument was registering fine.

### The real blocker, verified

The endpoint is CONJUNCTIVE on space-separated terms, and this was ALREADY KNOWN.
`dispatch_with_recall.clj` carries a `MEASURED 2026-07-30` comment recording the
falloff (1 term = 5 hits, 3 = 3, 7 = 2, 12 = 1, 29 = 0) and ends:

    ;; FOLLOW-UP: several short queries unioned would beat one short query;
    ;; this is the minimal measured fix, not the best possible one.

**That follow-up was never done, and it is now the whole blocker.** Re-measured
on the live store 2026-08-19, scoped to `type=:memory`:

    q=hilbert                            16
    q=weak-convergence                   19
    q=hilbert weak-convergence           11    <- intersection, not union
    q=computes                            1
    q=hilbert weak-convergence computes   0    <- ONE rare term zeroes it
    q=hilbert OR computes                17    <- 16 + 1; OR is supported

The four-term cap does not save it, because anchor selection PREFERS rare terms
and a rare term inside a conjunction floors the result set. `d893280a` demoted
the anchor from a hard filter to a ranking boost — but the term still sits in the
conjunctive query, so the hard filter was not removed, it was relocated.

`recall-query` builds `:query (str/join " " terms)`. The fix is `" OR "`.

Dispatched to codex-3 as a single-behaviour packet
(`invoke-1787129392639-5011-5c4b710b`, park `park-35681b94`), with a hermetic
acceptance test on the built query string and a required mutation check.

### Scoped df, from claude-11, independently re-run here

    term                unscoped (150,436)   type=:memory (781)
    hilbert                    366                  16
    weak-convergence           396                  19
    computes                   395                   1
    carrier                     87                   2

Unscoped, `computes` (395) and `weak-convergence` (396) are THE SAME NUMBER — so
no band over whole-index df can separate a dead anchor from a live one, and the
`[3 150]` band failing was never a calibration problem. Scoped, the dead anchors
sit at 1 and 2 against winners at 16 and 19. The response now carries
`:population :filtered | :whole-index-unfiltered` and echoes `:filters`, so the
receipt can record which question was answered instead of asserting it.

This resolves the `:basis {:retrieval :scoped-df-pending}` slot in
`E-loss-function-shape.md` — it is a field change now, not a placeholder.

## D52 addendum — OR costs nothing in ranking [verified independently, 2026-08-19]

I declined to endorse claude-11's ranking claim on its word, could not resolve
the document identities with my own extractor, and said so. The reason my
extractor failed turned out to be the hydrated response burying the id past the
chunk; `hydrate=false` fixes it. Re-run here end to end:

    OR  ("hilbert OR weak-convergence", type=:memory): 24 results, 24 distinct, 0 unresolved
    AND ("hilbert weak-convergence",    type=:memory): 11 results, 11 distinct, 0 unresolved
    ranks of the 11 AND-docs inside the OR result: [1 2 3 4 5 6 7 8 9 10 11]
    contiguous prefix: TRUE
    last AND-doc score -16.535 | first non-AND score -9.016 | GAP 7.519

**OR returns exactly what AND returned, in the same order, and then continues.**
BM25 scores a both-term document above an either-term one, and the separation is
a 7.5-point cliff rather than a gradient — so the ordering is structural, not an
accident of this term pair. `candidates` overfetches `(max 50 (* 4 k))` before
the limit, so a union of a few hundred still yields the best-ranked 50.

AND was therefore never a precision/recall trade. It was truncation of a list
that was already correctly ordered.

**This retires half of a caveat I gave Joe** — that OR would fix emptiness but
ranking would be the next problem and f13 might show memories surfaced and none
used. The half that is retired: OR does not push good results down. The half
that stands: term SELECTION may still fail to include a term the right memory
contains, which is what claude-11's scoped df addresses and what is not yet
dispatched. `computes` scoped df = 1 is a term that can only ever match one
document, and the anchor logic prefers exactly that kind of term.

Claude-11's own first verifier reported 0/24 resolving because it read the first
200 bytes of each document looking for a field that sits past the truncation —
a guard built against population errors, failing by making one. Neither of us
would have found it alone: it surfaced because the claim was offered for checking
and the check was actually attempted rather than waved through.
