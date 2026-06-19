# E-efe-education — the EFE learns its own delicacy (precision-learning over the star-map channel)

**Excursion** (E-prefix: a bounded scope-out, owned end-to-end by one agent — claude-1).
**Spawned:** 2026-06-08, from the star-map ⟷ live-WM EFE blend (`M-capability-star-map`, the blend design note).
**Status:** IDENTIFY (charter — concept + framing + safety; not yet building).

## The one-line idea (Joe, 2026-06-08)
> *"An applied-chaos concept where we tune the delicacy as we go … we've already leaned into the AIF
> self-extending the model, why not refine the tuning too?"*

Don't hand-pick the blend weights once and freeze them. **Let the EFE learn its own delicacy** — adapt the
star-map blend weight from the outcomes of the picks it influences.

## IDENTIFY — the gap
The blend (M-capability-star-map) wires the star-map signal into `compute-efe` additively, with **static,
hand-picked conservative weights** (applicability ~5, ascent ~6, body 3). But:
- We don't actually *know* the right delicacy a priori — "conservative" is a guess, not a measurement.
- The AIF stack already **self-extends the model** (structure-learning / niche-construction). The *tuning* is
  still exogenous and frozen. That's an asymmetry: the system learns *what to do* but not *how much to trust*
  its own new signal.

## The AIF framing (why this is native, not a bolt-on)
**The blend weight IS a precision.** In active inference, each signal channel enters the free energy weighted
by a precision (≈ inverse variance = confidence), and **precisions are learned from prediction-error** — this
is standard AIF (precision optimisation; the same machinery the WM judgement already exposes as
`precision-state`). So "tune the delicacy as we go" = **precision-learning over the star-map channel.** It is
the *same self-extension move* — applied to the precision rather than the structure. Reuses the
reliability-posteriors reframe of `[[project_bayesian_structure_learning]]` (a posterior over a precision,
updated by evidence), not a new mechanism.

## The mechanism (sketch — to be DERIVEd)
1. **Outcome signal.** Track the realised-vs-predicted outcome of **star-map-influenced** picks (picks whose
   ranking the ⭐ contribution actually changed). Prediction-error low ⇒ the star-map's ascent/applicability
   signal was warranted ⇒ evidence *for* raising its precision; high ⇒ evidence *against*. The deferred
   **peradam / mana** reward (M-capability-star-map design note, "later round") is a natural richer outcome
   signal — discharge/aliveness as the reward the precision learns from.
2. **Posterior over the precision.** A Beta/Gamma posterior over the star-map blend weight, updated from the
   outcome evidence (same shape as `[[project_bayesian_structure_learning]]`'s reliability posteriors). The
   static conservative weights from the blend round are its **prior**.
3. **Applied chaos = BOUNDED exploration.** Precision-learning has a chicken-and-egg: a low-precision star-map
   never gets its picks taken, so never earns evidence to rise. Controlled perturbation / annealing of the
   weight keeps generating evidence and escapes stuck-tuning — but *bounded*, decaying, never unbounded.

## Delicacy² — the safety (this is tuning the nervous system's tuning)
Non-negotiable, because a runaway tuner could destabilise the EFE:
- The tuner moves **only the soft star-map blend weight** — NEVER admissibility, the guardrails
  (`autonomous-admissible?` / `open-mission-with-holes?`), or any hard gate. Those stay exogenous.
- **Clamped range** — the weight can never exceed a ceiling (the star-map can nudge, never dominate / veto).
- **Slow adaptation** — small step size; the perturbation amplitude decays.
- **Observable trajectory** — the weight + its posterior + the perturbation are surfaced (alongside the ⭐) so
  the operator can watch the delicacy move.
- **Operator-consent on large shifts** — a tuning move past a threshold NAGs (consent-gate), per the
  WM-GUARDRAILS-SPEC posture. Default-on revert-to-prior kill-switch.
- Inherits "water doesn't flow uphill": the goal (the pre-registered ascent) is NOT something the tuner can
  change — only how much the ascent-credit is trusted.

## Sequencing (do NOT fold into the blend round)
1. **Blend round (in flight, codex-1):** static conservative weights + ⭐ + calibration. Ships first.
2. **This excursion:** makes those weights the learned prior + adds the precision-update loop + bounded chaos
   + the delicacy² safety. Refines from there; does not replace.
(The graph-extension to all grounded regions is the *other* follow-on; orthogonal to the tuning.)

## Success criteria (to be sharpened at VERIFY)
- The star-map precision **converges to a stable, justified value** that beats the static prior on an outcome
  metric (e.g. fewer pilot fork-resolutions / lower realised prediction-error on advanced missions) —
  **without ever destabilising** the EFE or breaching the clamp.
- The whole trajectory is **auditable** (weight + posterior + perturbation + ⭐ visible) and the operator
  stays in control (consent on large shifts; revert works).

## Relations
- `E-possible-world-regulator` (child, 2026-06-08) — the **instrument** this charter needs: a possible-world
  param-sweep harness scored by an Ashby-requisite-variety regulator (M-ffm business axis + C-pudding-prover
  useful-work axis), so the delicacy is tuned by discrimination, not by shooting in the dark. The harness
  *recommends*; live re-tuning stays here + at Joe's consent locus.
- `M-capability-star-map` — the blend this refines (the static weights = the prior).
- `[[project_bayesian_structure_learning]]` — the reliability-posterior machinery, reused.
- peradam / mana reward (deferred, M-capability-star-map design note) — the candidate richer outcome signal.
- `WM-GUARDRAILS-SPEC.md` — the consent/observability posture this inherits.
