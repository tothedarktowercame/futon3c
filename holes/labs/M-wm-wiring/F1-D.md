# F1-D: which F `:policy-free-energy` means, and what F1 is

claude-11, 2026-09-26. Discovery, read-only.

Revisions read:

| what | revision |
|---|---|
| code | futon2 6a10b2f0 |
| registry | futon2 0a7ea5f2, row `:policy-free-energy` |
| Lean | mathlib4 e58b8d6e7d, `PolicyVariationalFreeEnergy.lean` |
| SPEC-F | futon2 b03017a5 |
| PROOF-2-F discovery | futon2 8a70c7f5 |

**Outcome: F1 is a component (a ⟨1⟩2 row), not a one-behaviour packet and not
Joe's.** Exactly one candidate computes the registry's F: the observed-prefix
arithmetic in `policy_prefix_evidence`. Nothing produces its input, the
admitted observed prefix of an executed policy, so "supply it on the live
path" is not one behaviour: the component is the prefix producer. The other
live candidate computes a different quantity, which Joe's 2026-09-21 H4
rulings already set aside. So there is nothing left for Joe to choose.

## 1. The definition

**The registry row** (`:formal` and `:latex`, futon2 b1baa026):

    F(π) := E_{Q(s|π)}[ln Q(s|π) − ln P(o,s|π)]  ≥  −ln P(o|π),
    P(o|π) = Σ_s P(o|s) P(s|π);  equality iff Q(s|π) = P(s|o,π);  +∞ when Q
    puts mass where P(o,s|π) = 0.
    (Parr 2022 B.1–B.2; B.9 is its use in π = σ(ln E − F − G).)

**Lean** (`PolicyVariationalFreeEnergy.variationalFreeEnergy lik prior q`):
the sum over q's support of `q s·(ln q s − ln (lik s·prior s))`, ⊤ on a
support violation. `lik s = P(o|s)` for the **fixed observed outcome**,
`prior s = P(s|π)`, `q = Q(s|π)`. It is stated over an arbitrary finite state
type; the docstring says "instantiating S with a trajectory type gives the
multi-step case". `vfe_posterior_eq` (:296) proves F = −ln P(o|π) at the exact
posterior.

**The time index is fixed by Joe's rulings, not by the formula.** On
2026-09-21 Joe issued six rulings settling F's semantics as an
**observed-prefix** quantity (H4). The record is the wording correction in
PROOF-2-F-discovery-2026-09-24.md §"Wording correction". SPEC-F §1 writes
the definition out:

    pᵢ = predictedState Bᵢ sPrevᵢ;  fᵢ = variationalFreeEnergy (fun x => Aᵢ x oᵢ) pᵢ qᵢ
    Fprefix(π) = Σ_{i<n} fᵢ   (n > 0 = observed history length of the SAME policy,
                               not the future horizon; an unweighted sum in nats;
                               qᵢ = exactUpdate Aᵢ Bᵢ oᵢ sPrevᵢ)

So the o in F(π) is the observation that followed an **executed** step of π,
and P(s|π) is the prediction through that step's transition, not a rollout
into the future.

## 2. The two live candidates, against that definition

### A. `policy_prefix_evidence/evaluate-synthetic` (policy_prefix_evidence.clj:17-54)

**What it computes.** For each admitted step of one policy, in order:
`prediction = transition · q`. Then `om/query :condition` with the observed
event gives `p = Σ_s prediction(s)·P(o|s)`, the exact posterior `w/p`, and
`:f = surprisal p = −ln p` (observation_model.clj :300-311, with p = 0 giving
`:contradiction` and `##Inf`). The posterior becomes the next step's q, and
`total = Σ :f`.

**Against the definition: it is the registry's F.**
- Each step's `−ln p` equals `variationalFreeEnergy (P(o|·)) prediction q` at
  the exact q (`vfe_posterior_eq`).
- The sum over the executed prefix is SPEC-F's Fprefix.
- The chaining `sPrev_(i+1) = qᵢ` is the loop's `recur (:posterior result)`.

**What it lacks.** Its input is synthetic by construction ("This result grants
no live authority"). `production-ranked` (:56-70) never calls it; for every
entry it removes `:f` and stamps `:not-supplied`. **No producer of an admitted
prefix exists.**

### B. `cascade_free_energy/policy-free-energy`, called from `efe/rank-cascade-actions` (efe.clj:1193)

**What it computes.** `F = −ln Σ_s Q_τ(s|π)·A(o|s)`, where:
- `Q_τ` is π's rollout from the current **q0** forward τ steps (the horizon);
- `A` is `token-likelihood` over the adjudication rates;
- `o` is the cascade spec's `:evidence` set, "the observed evidence tokens the
  cascade decision already carries", i.e. **the present observation**.

It too cites `vfe_posterior_eq`, as the exact-posterior value of B.2
(cascade_free_energy.clj ns docstring).

**Against the definition: not the registry's F.** It is B.2's arithmetic with
the wrong P(s|π) and the wrong o. It scores what is observed **now** against
where π would put the state **τ steps from now**. That is a prospective
quantity over a future horizon, not the evidence for π from π's own executed
history.
- SPEC-F :67 says so directly: "reviving it is not H4 supply".
- Under identity A it was `##Inf` for every effectful policy, which is why it
  was `:computed-not-attached` before acc4f3c4 switched it off. It was not a
  usable F set aside (PROOF-2-F discovery §4).
- Measured A (H-A-CONSUMER-I, 7ba427ab) would make it finite, and still the
  wrong quantity.

### C. The dark readback (`war_machine/f-pi-dark-readback` → `policy_free_energy/f-pi-vector`)

No production caller (the registry row's `:live-status`, RC8.5). It is not a
supplier and is not considered further.

## 3. What "D's admission" is, and whether it still applies

The stamp's `:pending-dependency :d-conditioning-consumption-and-policy-prefix-admission`
names PROOF-2's **clause 2, D** (PROOF-2-STRATEGY-draft :26): "compatible
prior, admitted observation, exact update, retained q, consumed posterior".

**It is not a gate PROOF-2a lifted.** It is the definition's own input.
Fprefix is defined over an admitted, occurrence-identified executed history,
with qᵢ retained. SPEC-F §1's admission list names what that requires:
- measured-A provenance;
- the executed-policy → occurrence → observation join;
- unique occurrence ids;
- `sPrev_(i+1) = qᵢ` on adjacent steps.

**What PROOF-2a changes is where the join could come from.** The flight now
writes enactment records: the candidate enacted, its attempts in order, the
W_c verdict, and `:publication-observed`. `enactment-habit/policy-key-for`
joins such a record to its policy key (H-E-I, 47842175). That is the nearest
existing carrier of "policy π executed, and then this was observed".

**What it does not carry:**
- the observation over the model's outcome carrier O;
- the step's transition row (B version) and prior;
- the A version;
- the exact posterior q.

Those four are exactly what fᵢ needs. So the prefix producer can be built on
the flight's records, but the records do not yet supply it.

## 4. What supplying F would change in each reader of the stamp

`production-ranked` accepts exactly three prefix statuses, `#{:computed
:not-supplied :zero-support}` (policy.clj:237-239 throws `:invalid-policy-prefix`
on any other), and the readers already type all three. **No reader needs
retyping.** F1's change is on the producer side. The readers, and what each
does when F is present versus absent:

| reader | absent (today, `:not-supplied`) | present |
|---|---|---|
| `cascade_selection.clj:112`, the −F term in `ln E − F − G/β` | term is **0.0** | −F from the prefix total (`:computed`) |
| `cascade_selection.clj:88, :100`, finite-candidate admission | admitted as finite | `:computed` finite; `:zero-support` (a contradiction on the prefix) gets probability exactly 0 |
| `cascade_selection.clj:182-183, :200, :226`, law receipt | `{:status :absent :reason :not-supplied}` | F's value and `:f-status :computed` |
| `cascade_selection.clj:255, :284-285`, contribution attribution | F contributes 0 | F's share, in the declared tie order `[:habit :free-energy :G]` |
| `policy.clj:236` `selection-candidate` (reads `:f-prefix`, takes f from it), `:255`, `:297` | f is `{:status :not-supplied :value nil}` | `{:status :computed :value total :presence :prefix-receipt}` |
| `efe.clj:1339-1344`, certificate `:f` | `{:status :not-supplied :source :policy-prefix-evidence …}` | unchanged: `efe` never supplies the prefix F; the certificate's `:f` stays the prospective slot's record |
| run record's per-tick F-absence stamp (f39bf0a1) | written every tick | absent, or per policy |

**Switching `:f-prefix-production?` off instead** would make efe call
candidate B again. It would attach `:f {:status :computed}` (or
`:computed-not-attached` when non-finite) into the certificate, while
`production-ranked` still removes `:f` and stamps `:not-supplied`. **The
selection would see nothing different**, and the certificate would record a
quantity the rulings set aside. That flag is not F1.

## 5. F1, as a component (⟨1⟩2 row)

The component is PROOF-2-F discovery §5 and SPEC-F, restated for PROOF-2a's
flight:

1. **The conditioning step producer.** After a flight enacts policy π, bind
   into one immutable, deduplicated step:
   - π's key (`policy-key-for`);
   - the enactment occurrence;
   - the observed outcome on O;
   - the measured-A version, and B's version and transition row;
   - sPrev;
   - the exact posterior q = `exactUpdate A B o sPrev`.

   This is the work: the flight records have the first two, and the other
   four are not written anywhere.
2. **Admission.** Order and admit π's steps (unique occurrence ids, adjacent
   continuity) at `[:decision :selection-certificate :token-belief-input
   :observation-updates]`. No synthetic evaluator or hand-built prefix may
   authorize them.
3. **The consumer: the one-behaviour change, once 1 and 2 exist.**
   `production-ranked` takes the admitted steps per candidate and runs the
   existing arithmetic (`evaluate-synthetic`'s loop on admitted input). It
   writes `:f-prefix {:status :computed :f total :steps … :posterior q}`, or
   `:zero-support`. When a candidate has no admitted steps it stays
   `:not-supplied` as the typed absence. That is correct for a policy never
   executed (SPEC-L's cold start: a new policy does not borrow another's
   history).
4. **Wire tests.**
   - *First layer:* a two-step admitted prefix for π gives
     `:f-prefix {:status :computed}` whose `:f` equals Σ −ln pᵢ, and whose
     steps chain qᵢ → sPrev_(i+1).
   - *Second layer:* `cascade_selection/selection-posterior` over two candidates
     that differ only in F ranks the lower-F one higher by exactly exp(−ΔF),
     and the law receipt carries F `:computed`.
   - *Bad cases* (SPEC-F): a duplicate or mismatched occurrence; a prefix
     joined to another policy; identity or unmeasured A; a missing q; a
     two-step total equated to its last summand.

**It closes** the hole `[[R2 R8] [R4 R8] [R6 R8] [R8 R6] [R8 R3]]`. The
exception is R8→R3, which also needs C-R3s: `:state-belief-update` has no
code.

**Registry, for the row's owner:**
- `:code` should name `policy_prefix_evidence` (the arithmetic) and, once it
  exists, the producer. The dark-readback sites should come off the row.
- `cascade_free_energy/policy-free-energy` should be recorded as the
  prospective quantity the rulings set aside, not as this row's site.

## Not done

- No code, no registry edit, no Lean build, nothing run.
- Not checked:
  - whether a flight enactment record's publication observation can be
    expressed on the model's O carrier (the first missing field in §3);
  - the text of Joe's six H4 rulings themselves: this rests on the
    2026-09-24 record of them and on SPEC-F.
