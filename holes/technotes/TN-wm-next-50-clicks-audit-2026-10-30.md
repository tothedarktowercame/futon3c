# TN: independent audit of the 50-click campaign and forecast for the next 50

Technical note, 2026-10-30. Written from a cloud session against the public
futon2 record (`cffe497d`) and the mathlib4 fork branch `darktower` (`2b89e588`),
as a reading of `futon2/holes/labs/wm-contract/runs/campaign-50-2026-10-27/REPORT.md`.


Compiled 2026-10-30 from the public record only: futon2 `cffe497d`
(`holes/labs/wm-contract/runs/campaign-50-2026-10-27/{REPORT.md,clicks.tsv}`,
`E-wm-criteria-completeness.md`, GRID v1/v2, `criteria-completeness-deficits.tsv`,
the E3 discoveries, the September click-budget series) and the mathlib4 fork
branch `darktower` at `2b89e588` (`DarkTower/WarMachine/R20Step.lean`,
`R20Run.lean`, `OrdinaryAuthorRefusalCorrection.lean`, `SelfHealRoute.lean`).
Run records under `data/wm-runs/` and the operator notes under
`~/notes/wm-next/` are not in the public repos, so per-run phase timings and
author replies were not re-read; the per-click table is taken as given.

## 1. The campaign was two regimes, not one

| Regime | Clicks | Window | Ordinary | Self-heal + medic | Useful |
|---|---|---|---|---|---|
| A: self-heal loop | 1–35 | 10-27 13:26Z to 10-29 17:39Z (52 h) | 1 | 34 | 0 |
| B: one-attempt rule | 36–50 | 10-29 17:59Z to 10-30 04:44Z (11 h) | 10 | 5 | 2 (+1 partial) |

Regime A is 34 consecutive repair clicks on six injuries (2aef7e0c,
c5606345, ad038c48, 9c622038, e43a348c, a59c85a6). Sixteen were refused
before any repair was attempted. Regime B begins at the rule Joe set on
2026-10-29 (one self-heal attempt, then fix upstream directly). Everything
below forecasts from regime B, because regime A is the process that was
explicitly retired.

Regime B ordinary clicks (10): useful 2 (907fac83, 3589464e); partial 1
(b4e9ea0e, author commit landed, reviewer falsifier failed); guardrail
refusal 1 (fc2e27e3); stopped at a defect 6. Of the six stops, four were
machine defects on the ordinary path (answer delivery, hot-load, resume
protocol, author-answer validity), one was already-satisfied (04a84de2) and
one was an abstention on an invalid reading (f50d1626).

Overhead in regime B: 5 of 15 credits (33%) went to medic discharges and one
failed self-heal, roughly one medic click per machine defect found.

## 2. Base-rate forecast for 50 more clicks (no further process change)

| Quantity | Point estimate | Plausible range | Basis |
|---|---|---|---|
| Credits spent on medic/self-heal | 15 | 10–20 | 33% overhead in B; one discharge per defect |
| Ordinary clicks | 35 | 30–40 | remainder |
| Useful clicks (want reached, reviewed) | 7 | 2–14 | 2/10 in B; n is tiny, so the interval is wide |
| Partial (author commit, review failed) | 3 | 1–6 | 1/10 in B |
| Author claims an unrelated artifact | 3 | 1–7 | 1/10 in B; rises on missions with locatorless wants |
| New ordinary-path defects | 14–20 | 10–25 | 4–6 per 10 ordinary clicks in B; no decline visible yet in n=10 |
| Wall clock | 3–5 days | 2–7 days | B ran 15 clicks in 11 h, but every stop waited on a merge; fc2e27e3 alone ran 4 h |

The September budgets (eight grants, roughly 37 clicks) produced no reviewed
useful work at all, so 2 in 10 is already a step change. It is not yet a
rate: two successes do not distinguish 10% from 40%.

## 3. What the mechanism audit changes about that forecast

**The R20 β change is provably inert on the clicks that will actually run.**
`iteratePrecision_constant_F` (R20Step.lean, line 1435) shows that when every
policy has the same F, 16 iterations return the prior. `step_constant_F` lifts
that to the step. In `R20Run.lean`, `affectiveEvidence` equals 1 when
`betaPrior = betaPosterior`, so `valenceUpdate` returns `q` unchanged, and
`hespLevel` uses `identityB`, so `predict` returns it unchanged too. The
Clojure test `r20_reading_beta_test.clj` asserts exactly this (1.25 for 0, 1,
2 and 16 iterations). fc2e27e3 observed β prior = posterior = 1.25 with one
policy. Expect the same on most of the next 50: the reading-family route has
been one policy per family on every run cited. The pre-registered prediction
for click 51 ("posterior ≠ prior if the family has more than one policy")
is conditional on a case that has not yet occurred live. Treat β learning as
NOT-EXERCISED until a multi-policy family is observed, and do not read 50
clicks of "prior = posterior" as evidence either way.

**E2 (successor consumes the outcome) has never been observed live.**
Deficit E2-OUTER-LEARNING is open with only a typed deferral witnessed. Click
51 is the first click under the migrated Makeup pin that can consume a
pending observation (fc2e27e3's). If it records
`:pending-makeup-migration-required` or another deferral, the "streak"
criterion cannot be met by any later click until that is fixed. This is the
single most informative thing click 51 can tell you.

**E3 (do not re-pick a refused task on unchanged input) is implemented but
unwitnessed.** The selector re-picks the same target until it is reached by
design (M-reachable-from-boot was picked four times, 40/41/43/46, before it
closed; M-typed-memories was picked at 48 and 50). After fc2e27e3's refusal,
click 51 will most likely re-pick M-typed-memories on an unchanged Giraffe
field. Either T12/E3 fires (first live witness, good) or the author gets the
same prompt and has the same incentive to claim an existing artifact
(a repeat of click 50). Predict which before issuing.

**The guardrail-refusal pattern will grow, not shrink, as machine defects
thin out.** Click 50's author answered in 27 s by citing an existing commit.
That is a task-quality outcome: a mission with a locatorless or vague want
invites it. `OrdinaryAuthorRefusalCorrection.lean` models how such a terminal
is recorded as non-injury; it does not change what the author does. The fix
named in the report (existing-artifact claims must cite work on the selected
want) is not landed. Until it is, expect roughly one in ten ordinary clicks to
end this way, more once the easy ready missions are consumed.

**Self-heal remains unreliable and should stay at one attempt.** 22 of the 40
defects were in self-heal/medic itself; E4-SELF-HEAL-CASCADE is "MISSING
DESIGN; S4b 1/5". Three deficits are parked self-heal failures awaiting direct
fixes (E4-E43-CAUSE-SCOPE, E4-F7E97-ACCEPTANCE-SCOPE,
E4-ACCEPTANCE-PUBLICATION-ISOLATION). Nothing in the record suggests the
self-heal route would do better on the next 50 than it did on clicks 2–35.

**The binding constraint is fix throughput, not credits.** In regime B every
non-useful click was followed by a merge before the next click (clicks.tsv,
`operator_change_before_or_after`), all by one caller, codex-11; futon2 took
238 hand-authored commits on 2026-10-29. Fifty more clicks at that rate means
15–20 direct fixes plus medic discharges from the same seats. If those seats
are not available, the loop stalls regardless of credit.

## 4. The success criterion is a process discipline, not a probability

"Useful task execution and successor consumption without corrective
intervention" needs two consecutive ordinary clicks that both reach a want,
with the second consuming the first's outcome and no merge in between. Under
independence at 20% per click that pair would appear somewhere in 35 ordinary
clicks with probability around 75%. In practice it did not appear in regime B
because an intervention sat between every pair (both useful clicks, 46 and 49,
came immediately after a medic discharge or a reselection fix). To observe the
streak the operators must deliberately not merge after a useful click and
issue the successor on the unchanged machine. That should be written into the
next campaign's pre-registration, otherwise 50 more clicks will again meet the
criterion "twice, not as a streak".

## 5. Recommendations for the next grant

1. Charge medic/self-heal clicks to a separate pool, or state the expected
   30% overhead up front, so the ordinary count is the planned count.
2. Pre-register, per click, the three binary observables that the
   mechanism audit says are live: E2 consumption of the predecessor, E3/T12
   firing on a re-pick after refusal, and β posterior ≠ prior. Each is a
   first-witness event; the first 10 clicks will settle all three.
3. Declare a no-merge window after any useful click and issue the successor
   inside it, so the streak criterion is actually testable.
4. Land the author-route fix (claims must cite work on the selected want)
   before the campaign, not as a self-heal reaction to the next click 50.
5. Keep B1-TICKETS out of scope explicitly: the field is missions-only, so
   the next 50 can say nothing about ticket work however they go.
6. Track the ordinary-path defect rate per 10 clicks; the campaign is
   working when it falls below 3, and the honest stop condition is when it
   does not fall over 30 clicks.

## 7. Addendum (2026-10-30): one change made from here, no click spent

Of the levers in §5, one is mechanical, local to the ordinary route, and
testable without a click: **an existing-artifact claim must cite the
selected target before a reviewer is dispatched.** It is pushed as futon2
branch `claude/claim-cites-selected-target` (one commit on top of
`cffe497d`; the same commit is attached as
`TN-wm-next-50-clicks-audit-2026-10-30.patch` beside this note).

What it does, in `src/futon2/aif/full_loop_runner.clj`:

- `claimed-commit-cites-target` (pure): the claimed commit cites the target
  when its message contains the target id, or a changed file is the selected
  mission's source file by name, or a changed path carries the target id.
  It never approves; a citing commit still goes to the reviewer.
- `review-already-satisfied-claim!` runs that check after the completion
  source pin is verified and before the first `charge!`. A non-citing claim
  returns `:guardrail-refusal` / `:claimed-artifact-not-on-selected-target`
  with `:claim-refused-before-review? true` and the citation evidence under
  `:claim-citation`; no reviewer seat is dispatched or charged. The reviewer
  path is unchanged otherwise and now also carries `:claim-citation`.
- The author prompt's requirement 7 states the rule to the author.
- `stop_line/no-finding-required?` and
  `meta_terminal_injury/explicit-non-injury-kinds` treat the new kind like
  `:reviewed-claim-refused`: the author's answer is wrong, not the apparatus,
  so no stop-line finding and no injury.

Replay against click 50 (fc2e27e3): the claimed commit futon3c `7aac5ca07`
has message `inbox-zero: promote 1 path(s) for claude-16 / Mission:
M-giraffe` and one file, `holes/excursions/E-argument-game-search.md`; the
check refuses it with no dispatch. Against click 14's retained shape (the
claimed commit touches `holes/missions/M-recommendation-bindings.md`) the
claim still reaches review. Test:
`test/futon2/aif/author_claim_cites_target_test.clj`, 7 tests, 32
assertions, green on the resolved `:test` classpath in this container.

Effect on the §2 estimates: it does not raise the useful-click rate. It
converts each "author claims an unrelated artifact" click (estimated 3 of
the next 50) from a reviewer dispatch plus hours of wait into a
sub-second typed refusal, and removes that reviewer charge from the medic
and self-heal overhead line. The author still spent the click; the next
lever, which this change does not attempt, is the selector's E3 behaviour
on the re-pick that follows.

Lean first: `DarkTower/WarMachine/AlreadySatisfiedReview.lean` revision 2
(mathlib4 branch `claude/claim-cites-selected-target` off `darktower`
`2b89e588`) adds `Evidence.citesTarget` and `Result.claimNotOnTarget`.
`noncited_closes_without_review` and `close_eq_closeByReview_of_cites` are
what the Clojure conforms to, `alreadySatisfied_iff_reviewed` shows the
check never approves, `nonInjury` carries the classifier reading, and
`click50_refused_before_review` / `click50_revision1_reviewed` are the
witness pair with `click14_still_reviewed` as control. The module has no
imports and checks standalone under `leanprover/lean4:v4.31.0-rc1` (done
in this container; the full `lake build` of DarkTower was not run here).

## 8. Further no-click levers (proposal, not built)

Three more levers were found and written up as futon2
`holes/labs/wm-contract/PROPOSAL-no-click-efficiency-levers-2026-10-30.md`
on branch `claude/claim-cites-selected-target`, under the 2026-10-18 ground
rule that a Claude session proposes and Codex decides. In order of credits
saved: a read-only self-heal admission preview (16 of 35 self-heal clicks
were refused before any repair, after the ration was spent; a
specification question, SelfHealRoute.lean's open hole); exclusion of the
pending selection's target after a refused run on an unchanged field (GRID
v2 E3 outer case, which would change click 51's pre-registered selection);
and an executed-inspection requirement on already-satisfied claims,
record-only until Zai lane telemetry is confirmed.
