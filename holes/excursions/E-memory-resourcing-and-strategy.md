# E-memory-resourcing-and-strategy — sequencing V3 against live resources

**Opened 2026-08-01 by claude (Joe's strategy discussion, same day V2 was
pushed).** This doc owns the *resourcing and sequencing* strategy for the road
to V3: how the experimental programme, the instrument repairs, and the live
proof loop share the same runners, tokens, and problems. The claims and
experiment designs themselves stay in `E-memory-v3-programme.md`; the repair
and instrument detail stays in `E-memory-v3-staging.md`. Cross-reference,
don't duplicate.

**The write-up goal this strategy serves (Joe):** V3 should deliver a
*result*, not a catalogue of failures. Reproducing a 1955 finding and
reporting three falsified preregistrations is fine for a lab note; the white
paper needs a positive spine. The strategy below is built so that the largest
single piece of work — attempting the backlog — *is* the positive result's
apparatus.

---

## 1. Resources, as of 2026-08-01

| resource | state |
|---|---|
| zai usage | **unlocked again** (cohort-2 pause ended) |
| attempted-but-unsolved problems | **~7** — the hard residual |
| unattempted Lean backlog | **~356 problems** |
| codex lanes | most have touched this programme; freshness is a wasting asset (staging §H3b) |
| frozen-data experiments | E1 backfill, E7, E5 — no runners, no material token cost |
| E2 ablation | designed, validated, `apmablate` standing; parked on the `ValidatedTrace` refactor |
| repairs gate | **lifted** (staging §A) — recording-side repairs are shippable now |

## 2. The one irreversible mistake: spending the backlog uninstrumented

V2 was archaeology. Its major results were salvaged from fields that mostly
did not get written — `used-ids` on ~16% of outcome receipts, `surfacing-via`
on the final 6.57% of the corpus by elapsed time, `:rejection-reasons` on 7 of
129 — and staging §B2's verdict is permanent: without dispatch-time seed
capture there is *no historical replay, ever*.

**The 356-problem backlog is the one chance to run a prospective study
instead.** Problems can wait; dispatches, once made, are data kept or data
lost forever. Therefore:

> **No backlog dispatch until the recording-side repairs ship.** A1
> (`record-outcome!` + backfill from the ~130 job results on disk), B2
> (seed capture: candidate ids + scores + `index-as-of`), B4
> (`:memory-use/kind`), B5 (ladder rung fired), A4 (`surfacing-via` on all
> paths), A3 (lane routed through `review-attachment!`).

All six are recording-side, not behaviour-side — they change what is
*written*, not what the system *does* — so they contaminate no measurement.
A3 additionally un-confounds the zai lane (staging §C3's objection), which
matters below. Behaviour-side repairs (any term-selection rule, A2/A2b)
remain **not shippable** on current evidence; if they ever ship, they ship as
*arms*, not as defaults.

## 3. The headline result comes from the backlog itself

The backlog has to be attempted anyway — that is the loop's job. **Randomize
memory availability at dispatch and the naturalistic run is the experiment**:

| element | design |
|---|---|
| population | fresh backlog problems (never attempted; no exposure confound) |
| arms | randomized at dispatch (shape set by C1 — see §4) |
| DV | **one-shot closure** — binary, no adjudication needed |
| secondary endpoints | attempts-to-close; regulative-memory subgroup; stuck-7 unlocks |
| noise floor | supplied by randomization itself |
| cost of control arm | **deferral, not sacrifice** — a control problem that fails one-shot re-enters the queue with memory enabled |
| registration | DarkTower `ProspectiveRegistration` before the first dispatch |

The V3 headline this licenses: *"memory access raises the one-shot solution
rate from X% to Y% on a preregistered, randomized, fully-instrumented cohort
of fresh problems"* — a number with a causal warrant. This is rung 2 at
cohort scale, at near-zero marginal cost, because the work was going to be
done regardless.

**Null-risk, stated now.** The primary could come back flat. Two mitigations,
both structural: (a) preregister the secondary endpoints where V2 gives
strong priors (regulative memories used at 45–54% vs substitutive 15%), so a
flat primary still yields a differentiated result; (b) settle C1 *before*
registering the arms, so the treatment with the best chance of a real effect
is the one the cohort tests.

## 4. C1 shapes the arms — which is why E1's backfill goes first

If the attachment layer is confirmed as the bottleneck (C1), the interesting
treatment is not memory-on/off but **populated graph vs star-forest**. V2
§4.6 established that multi-attachment was always representable and never
written — the graph is unbuilt, not badly designed. Populating it is the
repair with the largest predicted effect, and the cohort can test that repair
as an arm rather than shipping it on faith.

**This is also the showcase for Rob's causal harness** (see the cover note,
`docs/retrieval-whitepaper-v2-cover-note-rob.md`): his model predicts the arm
effects *before* the cohort runs, the predictions are registered in
DarkTower, and the cohort confirms or refutes them. The "as above" makes its
first falsifiable prediction about the "so below."

## 5. Sequencing

| when | what | why ordered here |
|---|---|---|
| now, parallel | E1 attribution backfill (settles C1); E7 duplicate detection; E5 blind route-vocab | frozen-data; no runners; C1 gates the cohort's arm design |
| now | recording-side repairs A1, B2, B4, B5, A4, A3 | must precede any backlog dispatch (§2) |
| now | **reserve 2–3 never-exposed lanes for E2** | lane freshness only degrades; every backlog dispatch spends it |
| next | one bounded mining audit of the legacy meta-learning logs | informs the typed fields; see §7 |
| next | **encode the DAG + measurement/missingness layer + estimands in Rob's engine** | added 08-01 from their reply (`E-memory-causal-integration.md`): "could genuinely alter what E2 identifies"; also supplies the cohort's mediation secondaries |
| next | preregister the cohort (arms per C1; Rob's predictions if ready; **primary estimand = ITT of memory availability**) | the registration is the paper's spine |
| then | run the backlog as the cohort; zai + codex both draw from it | E4, E6, C7 collapse into subgroup contrasts of one design (§8) |
| meanwhile | E2 after the `ValidatedTrace` refactor **and the estimand encoding** | converts the 38% adjudication into measurement — as an ITT of single-memory availability, per their Q2 — and validates the rubric the cohort's receipts rely on |
| early, any time | the stuck-7, fully instrumented, new memory system on | case studies, not statistics (§6) |

## 6. The stuck seven are exhibits, not a sample

The ~7 attempted-unsolved problems are the a95J08 pattern: each is a
potential *"memory unlocked what three attempts couldn't"* narrative, and the
natural substrate for the route-relativity story (staging §G6). Run them
early with full instrumentation and the new memory system; write each up as
a case. n = 7 is exhibit material — do not let them near a rate claim.

## 7. Naturalistic learning ≠ unstructured logging

Joe's observation stands: meta-learning signals were previously "logged" and
plausibly never mined, and may have been mathematical or operational rather
than research-relevant. The V2 lesson is that retrospective prose-mining
yields 66% coverage with directional undercounts. So:

- **One bounded audit, not a salvage programme.** A single codex job: sample
  the legacy logs, classify signals mathematical / operational /
  research-relevant, report proportions. Purpose: inform what the typed
  receipt fields should capture. Nothing downstream builds on the audit.
- **The type must exist at dispatch time.** Every proof turn is a learning
  opportunity, but only the fields that already exist can catch what the
  turn drops. That is what B4/B5/B2 are *for*. "Each proof turn is a
  potential memory-improvement opportunity" is made true by instrumentation,
  not by intention.

## 8. Experiment consolidation

E4 (lane-scoped granularity), E6 (zai generality), and C7 (cross-model
transfer) should not run as standalone experiments: with zai unlocked and A3
shipped, **they are subgroup contrasts inside the cohort** — zai and codex
lanes drawing from the same randomized pool, `:memory-use/kind` recorded on
every surfacing. This takes the realistic experiment count down (programme
§4's bound check) while concentrating the paper's centre of gravity in one
large positive design instead of seven small ones.

## 9. The V3 write-up posture

**Spine: locate → test → repair → demonstrate.**

| section role | content | register |
|---|---|---|
| background (compressed) | V2's triangulation: how converging instrument failures located the attachment layer | one section; the 1955 reproduction and the falsified preregistrations become *method*, not headline |
| test | E1 backfill settles C1; E2 converts the 38% into measurement | rung 2 |
| repair | graph population (if C1 confirms); recording-side instrumentation | shipped as arms, not faith |
| **demonstrate** | **the prospective cohort result** | the number with the causal warrant |
| above/below | Rob's model's preregistered predictions vs cohort outcomes | the collaboration exhibit |

The insouciance problem is solved structurally, not tonally: the failure
catalogue stops being the story because the story now has a second act.

---

*Cross-references: claims and experiment designs — `E-memory-v3-programme.md`
(§2 claims C1–C8, §3 experiments, §4 sequencing state). Repairs, instruments,
ablation detail — `E-memory-v3-staging.md` (§A repairs, §B instruments, §H
the ablation). The Rob framing — `docs/retrieval-whitepaper-v2-cover-note-rob.md`.*
