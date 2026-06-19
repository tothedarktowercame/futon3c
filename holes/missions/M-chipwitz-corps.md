# Mission: ChipWitz Corps — the warranted-work control layer

**Status:** IDENTIFY (chartered 2026-06-11 from a live motivating example)
**Owner:** Joe + fable-1 (ground control); develops as-we-go alongside the WM pilot loop
**Cross-ref:** `futon0/holes/missions/M-futonzero-generative.md` (the evaluation
half rides FutonZero), `futon3/library/peripherals/pilot-plus-ground-control.flexiarg`,
`futon3/library/corps/` (the corps patterns), `futon3c/src/futon3c/aif/calibration.clj`
(the evidence reader the PXR stream joins)

## HEAD

A control layer for the agent corps in which **choice points are resolved by
finding warrants, not by asking the operator**. The warrants already exist —
they are the pattern library; they just need to be *found* at the moment of
choice. An agent that can do **warranted work** proceeds, logging the warrant
(PSR) and the outcome (PUR); FutonZero evaluates the PXR stream. An agent
that finds **no warrant** has located a genuine operator question — and,
simultaneously, a hole in the library worth mining.

## 1. IDENTIFY — the motivating example (verbatim-grade, 2026-06-11)

During WM pilot cycle 5, claude-3 surfaced a consent fork to Joe:

> **(A)** fly the guarded full discharge (`measure-never-target`) — fast,
> honest fallback, no measured pair; or **(B)** pivot to a partial-discharge
> advancement of `M-futonzero-generative` → the first measured pair + direct
> campaign progress. **My recommendation: (B).**
>
> joe: OK with B

Joe's post-hoc read: *"this does not seem like a real operator-needed choice
point to me — I just did what claude-3 recommended already."* Correct: the
choice was fully determined by standing direction (steer toward the
futonzero loop; the realised-semantics fork was resolved expressly to start
earning measured pairs). The consent gate fired on **form** (a fork exists)
rather than **substance** (is the fork undetermined by existing warrants?).
The "OK with B" turn was a policy lookup with the operator as the lookup
table.

The warrant for (B) existed and was findable: expected-information-gain
(prefer experiments where the model sticks its neck out — already written
down in the Bayesian-structure-learning frame). Under the ChipWitz layer,
claude-3 logs a PSR citing it and flies; Joe gets a notification, not a
question.

## 2. The protocol (sketch — DERIVE develops this)

1. **At a choice point**: run the circumstance through the warrant-finder —
   the cascade lane (`cascade-policy-for(ψ)` returns a scored,
   coherence-greedy pattern set; the WM charter's "policy network" row made
   real) or direct library search.
   - Warrant found → log **PSR** (pattern id, candidates considered,
     rationale, choice-point ref) and **proceed**.
   - No warrant above threshold → **ask the operator** — the ask is genuine,
     and it marks a library gap (feeds the sorry-mining chain).
2. **At cycle close**: log **PUR** (outcome, prediction-error vs what the
   pattern promised). Bounded work, bracketed by PXRs.
3. **FutonZero evaluates the PXR stream**: "can this agent do warranted
   work?" becomes measurable — PSR→PUR pairs with outcomes, one more
   evidence kind in the calibration reader (closure-folds are already
   PUR-grain; the PSR evidence-store already exists and feeds the WM's
   pattern `:activations-Nd`).

**The diagnostic inversion** (the charter's sharpest point): asking *despite*
a findable warrant is the smell; asking *without* one is legitimate and
productive. The operator-question rate stops being friction and becomes a
measurement of library coverage.

## 3. Guards (anti-laundering, inherited from the week's discipline)

- Agents cannot mint warrants for themselves mid-choice — the library
  accumulates through its own attested channels (minting is itself
  PXR-logged work).
- Ambiguity defaults to ASK: a warrant must *determine* the choice, not
  merely be consistent with it.
- Every warranted-proceed is a ledger record; the **autopen rate** is an
  operator-reviewable signal (if everything proceeds on warrant, the
  threshold is too loose — same erosion check as calibration's
  independent-pairs rule).
- Action-class boundaries unchanged: warrants cover *which/how within the
  autonomous envelope*, never whether outward/irreversible actions execute.

## Remaining work

- [ ] DERIVE: the warrant threshold — what cascade C-score / match quality
      counts as "found"? (The one knob: too loose re-creates autopen-creep,
      too tight re-creates rubber-stamp asks.)
- [ ] Typed PXR channel (sibling of `futon3c.aif.discipline-events`): PSR +
      PUR records with choice-point refs.
- [ ] Wire the choice-point check into the pilot flow (warrant-finder
      consultation before surfacing operator questions).
- [ ] `:pxr` evidence kind in `futon3c.aif.calibration` (FutonZero
      evaluation of warranted work).
- [ ] No-warrant asks → sorry-mining hook (library gap → registry candidate).
- [ ] Relation to `library/corps/` patterns: name which corps disciplines
      the layer operationalizes.
