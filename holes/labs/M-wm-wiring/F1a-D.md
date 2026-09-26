# F1a-D: the conditioning step's fields, where each exists at HEAD

claude-11, 2026-09-26. Discovery, read-only. This covers the F1a producer from
F1-D (futon3c 507acc5c), §5.1.

**Revision read:** futon2 987bb867. Every "file:var" below was read this
session at that revision. "Written" means a value is written into a record at
HEAD. "Computable" means an existing function would produce it from values
in scope; the table keeps the two apart.

**Definition.** The step's fields come from SPEC-F §1 (futon2
b03017a5), which implements Joe's 2026-09-21 H4 rulings. I rely on the
2026-09-24 record of those rulings (PROOF-2-F-discovery, wording
correction), not on their text, as F1-D did. The per-step quantity is

    fᵢ = variationalFreeEnergy (fun x => Aᵢ x oᵢ) (predictedState Bᵢ sPrevᵢ) qᵢ,
    qᵢ = exactUpdate Aᵢ Bᵢ oᵢ sPrevᵢ.

## One-line answer

**F1a cannot be written as one behaviour, but O is not missing.** O exists in
the code: it is the admitted token observation. What F1a lacks are two
values, written at two different sites:
- **the o after the enacted step, over O:** the flight's after-observation,
  not yet admitted;
- **the measured-A version the decision used:** the tick's decision path
  scores with a class-emission observation model, and records no token
  rates.

## The table

| field | definition (SPEC-F / Lean) | exists at HEAD? where | same object the definition needs? | producer's site |
|---|---|---|---|---|
| **π's key** | the executed policy's identity; F is over the history of the SAME π | **written**: the `:increment` receipt's `:policy-key` (`enactment-habit/increment`, via `policy-key-for`). `flight-runner/wc-verdict-fn` returns it as `:increment`, and `flight/run!` merges that into the flight record's `:enactments` entry | **yes**: `[mission, ordered pattern ids, semilattice]`, the key selection's E is keyed on. Absent (typed) when no checker is configured or `policy-key-for` refuses | none needed. Its presence depends on the W_c checker being configured |
| **enactment occurrence** | a unique occurrence id for the executed step, joined to π and its observation | **written**: `flight-runner/enact-fn`'s record `{:schema :wm/enactment-v1 :flight … :click …}`, at `<record-dir>/<flight>-<click>.edn` | **yes**, as the pair `(flight, click)`. H-E-D's A4 ("no record id") still holds: the id is the pair, not a field | none needed. F1b's admission reads `[:flight :click]` as the occurrence id |
| **oᵢ, the observed outcome on O** | the event observed after the executed step, on the outcome carrier A is indexed over: `Aᵢ x oᵢ` | **partly written, not admitted, not all of O** (see §O):<br>• `flight/record-click`'s `:after {token bool}` is written per click by `flight/run!`, after `enact-fn` and `wc-fn`. It covers the **want** tokens only (their locators)<br>• `enact-fn`'s `:attempts` carry each produced token's `:check :result :observed`<br>• `:publication-observed` (`flight-runner/observe-publication-fn`) is a **different alphabet** | `:after` and the attempt checks are elements of O, restricted to part of the universe. They have not been through `observation-admission/admit`. `:publication-observed` and the W_c verdict are **not** elements of O | **`flight/run!`**, where `enacted` and `after` are both in scope. Admit `after` (plus the attempts' checks) through `observation-admission/admit` over `token-belief-carry/token-universe`, and write it on the enactments entry. **Off the tick's path** (flight process) |
| **Aᵢ, the measured-A version** | the observation kernel with measured-A provenance (SPEC-F A10): `TokenObservation.tokenLikelihood rates s o` | **computable, not written for the decision.** `observation-rates/sourced-rates` → `token-likelihood-rates` reads measured cells first (H-A-CONSUMER-I, 7ba427ab). But the rates and their `:rates-provenance {:basis :measurement …}` are passed only by `war-machine/cascade-lane` (the R5 lane). The decision the flight enacts comes from `war-machine/cascade-decision-admitted`, whose `efe/rank-actions` opts carry `:observation-model class-model` (a class-emission model) and **no** `:adjudication-rates`, so the decision's certificate records no token rates | the lane's rates are the right object; the class model is a different object. SPEC-F's A is the token kernel; F-discovery §3 already called the class model "synthetic, uncalibrated" | **`war-machine/cascade-decision-admitted`**, where the decision is sealed: compute `sourced-rates` over the problem's locators and write `{:rates … :basis … :measurement … :labels-digest …}` beside the selection certificate. **Touches the tick's path** |
| **Bᵢ's version and the transition row** | `predictedState Bᵢ sPrevᵢ` for the enacted step | **B's content written, the row computable.** The run record's `[:decision :selection-certificate :token-belief-stage :domain-inputs]` (`token-belief-carry/legacy-stage`) records each problem's `:interpretations`, whose `:transition :produces` is B. The row is `cascade-model-manifest/transition-row pattern state` (a function). A version digest exists as `policy-precision-carry/model-identity` → `:model-id`, but it digests the **scoring** model (the class model, policies, schedules), not B | B's content yes; the `:model-id` is not B's version | no new write for the content. B's version = a digest of the enacted candidate's interpretations, computable in the producer |
| **sPrevᵢ** | the belief the step starts from (the first step: an explicitly admitted boundary belief) | **written**: `[:decision :initial-belief-receipt :value]` (`scoring-input-receipts/initial-belief` → `cascade-model-manifest/observed-belief`: a point mass on the target-qualified true facts), repeated as the token-belief stage's `:continuation-belief` | **yes, same carrier** (token states over the token universe). `r1-belief-carry` (the R1 node packet) is the **entity** belief (`belief/reconcile-belief-carry`), a different carrier, so it is **not** sPrev | none needed for step 1; admission (F1b) decides it is the boundary. For i > 1, sPrev = qᵢ₋₁ (the chain) |
| **qᵢ, the exact posterior** | `exactUpdate Aᵢ Bᵢ oᵢ sPrevᵢ` | **not written**: the stage's `:observation-updates []`, `:conditioning-status :not-wired`/`:awaiting-observation-admission` | n/a | **computable with existing functions, no new arithmetic.** `cascade-model-manifest/exact-update` (Lean `ExactBeliefTrajectory.exactUpdate`), with `likelihood-of = (fn [s o] (token-likelihood rates s o))` and the prior pushed by `transition-row` over sPrev. That is the composition the retired `token-belief-at` performs. `exact-belief-core/condition-predicted` is its numeric core. P(oᵢ) for fᵢ is the same weights' sum |

## §O: what O is, and why `:publication-observed` is not in it

In the code, O is the token observation carrier: TokenObservation.lean's
`tokenLikelihood (r : AdjudicationRates V) (s o : Finset V)`, where both the
state and the observation are sets of tokens. Its runtime form is
`observation-admission` ("observation admission records for the token
observation carrier (P5, TokenObservation.lean)"): per token of
`token-belief-carry/token-universe`, an admitted label `:present` or `:absent`.
Measured A is indexed over the same alphabet: `rates-by-class` reads labels
carrying `:token-class`, a recorded verdict and the admitted reference label,
and `token-likelihood-rates` gives per-token `{:false-neg :false-pos}`.

- **`:publication-observed`** is one judgement about whether a repair
  obligation's discharge receipt reached the store
  (`observe-publication-fn`). It is not a token of any cascade problem's
  universe, so it is a different alphabet.
- **The W_c verdict** is a vector of check failures about the enactment's
  conformance. It is also not an element of O.
- **The flight's `:after {token bool}` and the attempts' checks** are
  elements of O, restricted to the want and produced tokens. That is the
  right alphabet but not all of the universe, and they are unadmitted.

**The Lean does not fix O.** `variationalFreeEnergy` and `exactUpdate` are
generic in S and O. The model does: SPEC-F's Aᵢ is the measured kernel, and
the measured kernel is the token kernel. So there is one reading, not two:
O is the admitted token observation. What has to be written is `oᵢ` as a
set of `:present` tokens over the problem's universe, via
`observation-admission/admit`. For a universe token not observed after the
step, the typed position is to record it as unobserved: "an unobserved
token is never an absent one" (observation_admission ns docstring). How
`tokenLikelihood` treats a partially observed o is then a question for F1b's
admission. It is not decided here.

## So F1a is

**Two writes at two sites, then composition:**

1. **In the flight, at `flight/run!`:** admit and write oᵢ (the after-
   observation and the attempts' checks) on the enactments entry. This is off
   the tick's path.
2. **In the tick, at `war-machine/cascade-decision-admitted`:** write the
   measured-A version the step's likelihood will use. This is on the tick's
   path. It is a write only: the decision still scores with the class model,
   and whether it should score with the token kernel is a separate question
   (the "two A's" noted above).
3. **qᵢ and fᵢ need no new arithmetic,** only the existing
   `exact-update` / `transition-row` / `token-likelihood` composition. It
   belongs in F1b/F1c, where the admitted steps are consumed.

π's key, the occurrence and sPrev are already written.

## Not done

- No code, no registry edit, nothing run, no generator.
- **Not checked:**
  - whether `tokenLikelihood` is defined for an o over part of the universe;
  - whether `observation-admission/admit` can take a mechanical check's
    `{:observed bool}` as an adjudication (it expects an adjudication and a
    review), i.e. whether the flight's checks qualify as admitted labels or
    need the review step;
  - whether any production run record at HEAD carries the `:increment`
    receipt with a `:policy-key` (this depends on the checker being
    configured in the driver).
- The H4 rulings' text: relied on the 2026-09-24 record, as F1-D did.
