# F1b-D: what admitting the flight's observation requires, by the definition

claude-11, 2026-09-26. Discovery, read-only.

Revisions read:

| what | revision |
|---|---|
| code | futon2 5d8105f2 |
| SPEC-F | futon2 b03017a5 |
| observation_admission.clj | 059cf10f, 3f601f60 |
| TokenObservation.lean | mathlib4 5fe0371169 |
| A-S.md | kimi-3's spec, 2026-09-24 (the hole H-A packet) |

**Neither (a) nor (b) as posed: the definition puts admission on the
reference label, not on the observation.**
- **o is the recorded verdict.** In `tokenLikelihood r s o`, o is what the
  observation channel **reports**, and s is the true state.
- **Measured A's rates are counted by comparing** those recorded verdicts with
  independently established reference truth.
- **Admission, blinded review included, is how the reference truth** that
  measures A is made. It is not a step the observation passes through.

So the flight's mechanical check results are o **as they stand**: recorded
verdicts on the checked tokens. They are scored with the A measured for
that check channel. F1a-1 applied `admit` to o at the packet's direction,
and that is the category error. `admit`'s refusal there is correct about
`admit`, but it was the wrong gate for o. F1b-I corrects the entry's
typing: one behaviour, below.

## 1. What each source says

**TokenObservation.lean** (mathlib4 5fe0371169):
- `tokenLikelihood (r : AdjudicationRates V) (s o : Finset V)` is the product
  over **every** v ∈ V of, when v ∈ s, `1 − falseNeg v` if v ∈ o else
  `falseNeg v`, and when v ∉ s, `falsePos v` if v ∈ o else `1 − falsePos v`.
- `falseNeg v` is "the probability that an established token v is **missed**";
  `falsePos v` is "the probability that a non-established token v is
  **reported**" (the `AdjudicationRates` docstring).
- o is therefore the set of tokens the channel **reported**, and s the set
  that is established. The ns docstring, design P5: "One observation follows
  each application … checkable tokens … are observed exactly …; tokens needing
  judgement carry an adjudication error rate".

**How the rates are measured.**
- `observation-rates/rates-by-class`: "Each label is {:token-class c :recorded r
  :admitted a} where r is the **recorded verdict** … and a is the **admitted
  reference label**". The rate compares r against a.
- `check-error-rates` (ns docstring; A-S.md §1) counts per check **kind**
  (`:test :grep :validator :layout`) over ledger rows whose truth was
  "established **independently of the check**" by a constructed bad case, a
  later review, or an independent recomputation. A row whose truth came from
  the check itself is excluded.

**What admission is for.**
- The `observation-admission` ns docstring (059cf10f): four steps "that keep
  blinding by construction and turn a reviewed adjudication into an admitted
  token label".
- 3f601f60's message: the "admitted record carries recorded verdict **for
  rates**".
- The `admit` docstring: the recorded verdict "is carried AFTER admission so
  rates (S-3) can compare it with the admitted label; the observer never saw
  it".

The observer is blinded **from the recorded verdict**, so the admitted label
is a truth independent of the observation. That is exactly what the rate
estimate needs: a reference that is not the check's own output. Review is
part of establishing that reference, so it belongs to the **measurement of A**.

**SPEC-F** §1: "Admission additionally requires: … measured-A provenance (A10);
the executed-policy/action→occurrence→observation join; unique occurrence
identities; …". This is admission of the **step** into the prefix (D's
admission). It asks for the observation to be joined to the occurrence, and
for A to have measured provenance. It does not ask for the observation to be
an adjudicated label. The `:f-prefix` row (:280) wants the "exact observed
event with carrier/context".

## 2. The two readings, settled

**(a) "a mechanical check is a different channel whose A is unmeasured".**
Half right, and it is the half that matters.
- The flight's o is on the mechanical check channel. But that is the channel
  measured A **is** measured for: check-error-rates counts the checks' own
  verdicts against independent truth, per check kind.
- So o is not on a different channel from A. The condition is that the rates
  applied must be those of the **same check channel** that produced o: the
  class or kind whose check the flight ran (`observation-checks/checks` by
  `:class`).
- No reviewer seat is needed for o.

**(b) "review is only a blinding safeguard, so a mechanical check can be
admitted with `:mechanical` provenance".** Wrong in its consequence. Review
**is** what makes A's rates apply: it makes the reference label independent.
But that applies to the **reference**, not to o. Giving `admit` a
`:mechanical` form would turn a check's own verdict into a reference label.
That is the self-truthed row A-S §1 excludes: "A row whose truth came from
the check itself is excluded". It must not be written.

## 3. Partial o, and the qualified universe

**Partial o.** `tokenLikelihood` ranges over all of V. A token v ∉ o reads as
"not reported", which for an established token is a miss (`falseNeg`), not
"not checked". So a partial observation **cannot** be put into o over the
full universe: the unchecked tokens would be scored as reported absent.
- The consistent encoding restricts V to the checked tokens. Because the
  kernel is a product of independent per-token factors, each summing to 1
  over o-membership, the unchecked tokens marginalise out.
- The code already has V as the rates' keys (`cascade-free-energy`: `universe
  (set (keys rates))`, `obs` intersected with it), so a rates map over the
  checked tokens is that restriction.
- **The Lean does not state it.** `tokenLikelihood_colsum` is the whole-V
  column sum, and `MixtureJointSeparationWitness` has a per-token `marginalMiss`,
  but there is no theorem that marginalising the unobserved tokens gives
  `tokenLikelihood` over the observed subtype. That lemma is the one text to
  write, a small ⟨1⟩2 Lean row (`tokenLikelihood_restrict`), so the
  restriction conforms to the definition rather than to a comment.

**The qualified universe.** The run record's `:domain-inputs`
(`token-belief-carry/token-universe`) gives `[target token]` pairs over the
problem's facts, want and interpretations. F1a-1 used the flight's view (the
want source's universe plus the wants). With the qualified universe:
- `:unobserved` grows to every fact, guard and produces token the step did
  not check.
- Under the restriction above, `:unobserved` is a record of what was
  marginalised, not an input to the likelihood.
- s (sPrev and q) stays over the full qualified universe. The likelihood
  reads s only on the checked tokens.

So the qualified universe changes the size of `:unobserved` and makes its
tokens joinable to sPrev's carrier. Using it (qualifying the flight's tokens
by `(:target f)`) is F1b's join, not the observation's content.

## 4. F1b-I: one behaviour

**Retype the flight's `:observation` entry (futon2 `flight/step-observation`)
as the recorded observation on the check channel, with no admission applied
to it.** Concretely:
- **Status.** `{:schema :wm/step-observation-v2 :status :observed}` when any
  token was checked, `{:status :nothing-observed}` otherwise.
- **o.** `:o` is the set of checked tokens whose verdict is true (reported),
  and `:checked` is the set of checked tokens (V for this step), so o ⊆
  checked. Each checked token carries its channel under `:channel {token
  class}`, the class whose check ran (from the attempt's
  `:check :class`, or the locator's class for the after-observation).
- **`:unobserved`** holds the universe tokens not checked (marginalised,
  never absent).
- **Drop the `admit` calls and the `:admission` / `:candidate-labels` /
  `:review-required` typing.** `admit` stays what it is, the reference-label
  maker for rate measurement.

The rest of F1a-1 stands: its write site, its universe record, and "unobserved
is never absent".

**Wire test.**
- First layer: one enacted click through `run!` gives `:observation
  {:status :observed}` with `:t` ∈ `:o`, `:t` ∈ `:checked`, and `:u` ∈
  `:unobserved`. No `:admission` key.
- Bad case: a check that reports false puts the token in `:checked` but not
  in `:o` (reported absent, a scored observation). An unchecked token is in
  neither.

**Map.** `:flight-run`'s declared write is unchanged (`[:observation {:record
:enactment-entry}]`).

**What F1b then needs, in the steps after F1b-I:**
1. **The step's A** is the measured rates of the channels in `:channel`,
   restricted to `:checked`. That is write 2 (claude-12's, the measured-A
   version at `cascade-decision-admitted`), which must record rates per
   **class**, the key the flight's checks carry.
2. **The step admission** (SPEC-F's D): the join occurrence → `:o` (already
   on one entry), continuity of sPrev, and unique occurrence ids.
3. **`tokenLikelihood_restrict`** (Lean, ⟨1⟩2), so scoring `:o` against s on
   `:checked` is the definition's likelihood.

## Not done

- No code, no registry edit, nothing run.
- **Not checked:**
  - whether the per-class labels `sourced-rates` reads at the live R5 call
    (`:subjects observation-labels`) come from the same checks the flight runs
    (`observation-checks/checks` by class), i.e. whether the recorded verdicts
    measured and the flight's o are one channel **operationally**, not just by
    name. `check-error-rates` keys by kind and `observation-rates` by class,
    and the join between them is a fixture (`check-ledger-classification`);
  - whether `tokenLikelihood_restrict` is provable as stated (it should be
    from the product form, but no proof was attempted).
- **The map step of F1a-1 is still waiting:** `wm-flight-wiring.edn` is dirty
  at 15:08Z; the poll continues.
