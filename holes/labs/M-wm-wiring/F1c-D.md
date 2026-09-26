# F1c-D: the conditioning step, joined, admitted per policy, consumed

claude-11, 2026-09-26. Discovery, read-only.

Revisions read: futon2 7bdf382c, and SPEC-F b03017a5. Every "file:var" below
was read this session. This is the spec for the packets after F1b-I (futon2
c37926bc; futon3c 36b7471d).

## Answers in brief

- **The join sites.** There are two, joined by the run id. **`flight/run!`**
  has the enactment entry: o, the `:increment` receipt's policy key, and
  `:click-id`. **The click's run record** (`flight-runner/enact-fn`'s
  `:fetch-run-record`, keyed by the same `:click-id`) has the decision's
  measured A, sPrev and B. No single site has all six fields today.
- **The channel classes match by name,** but the sources do not show they are
  the same channel. The flight's checks (`observation-checks/checks`) and
  measured A's `:classes` (the contract's `class-of`) are both `:C3 :C4 :C5
  :C6 :C8`. That the recorded verdicts behind a class's measured rates come
  from **that class's check function** is not written anywhere, so it is a
  ⟨1⟩2 row (§2). At HEAD the decision's measured A is always the typed
  absence, because no labels reach it.

## 1. The join

**Flight record** (`flight/run!`). Each `:enactments` entry carries
`:click-id` (the run id the click function returned), `:record-path` (the
enactment record), `:publication-observed`, and `:observation` (v2: `:o
:checked :channel :unobserved :universe`). When a checker is configured it
also carries `wc-verdict-fn`'s `{:wc … :increment {:record-id … :policy-key
[mission pattern-ids semilattice] :delta …}}`.

**Enactment record** (`flight-runner/enact-fn`, at
`<record-dir>/<flight>-<click>.edn`): `:flight :click :candidate
:decision-candidate :attempts :grain :publication-observed`.

**Run record** (the click's, fetched by `:click-id`; `enact-fn` and
`wc-verdict-fn` hold `:fetch-run-record`). Under `:decision`:
- `:measured-a`: `war-machine/measured-a-version`, i.e. `{:schema
  :wm/measured-a-v1 :rates-sha … :source … :classes […]}`, or `{:status :absent
  :reason :no-measured-rates | :sourcing-refused}`;
- `:initial-belief-receipt :value`: sPrev, a point mass `{state 1}` over
  target-qualified token states (`cascade-model-manifest/observed-belief`,
  derivation `:target-qualified-true-facts-point-mass-v1`);
- `[:selection-certificate :token-belief-stage :domain-inputs]`: each
  problem's `:interpretations`, i.e. B's content;
- `[:selection-certificate :candidates]` and `:candidate-derivations`: the
  chosen candidate's precedence.

**No single site has all of them.**
- `flight/run!` has the entry, o and the policy key, but not the run record.
- `enact-fn` has the run record and the enactment, but not o, because the
  after-observation is taken later in `run!`.

The join id is **`:click-id` = the run id**. The producer site is
`flight/run!` right after the observation write, with the run record fetched
by `:click-id`. That needs `run!` to take the fetcher `enact-fn` already
takes, as an optional opt that is typed absent when missing.

**Is the occurrence unique?** Yes. `run!` enacts at most once per click, and
the click id is the tick's run id, unique per tick and so unique across
flights. `(flight, click)` is therefore unique, and `:click-id` alone would
do. SPEC-F's duplicate-occurrence bad case can be pinned on it.

**Adjacent continuity (sPrev₍ᵢ₊₁₎ = qᵢ).** The next click's
`:initial-belief-receipt` is **re-initialised** from fresh target facts every
click (`scoring-input-receipts/initial-belief`). It is not qᵢ. So the prefix's
sPrev for step i+1 must be the chain's own qᵢ, and only step 1 takes the run
record's initial belief (SPEC-F: "Initial sPrev is an explicitly admitted
boundary belief"). Where the next click's fresh point mass differs from qᵢ,
that is D's continuity question, recorded, not overwritten.

## 2. The channel match

**By name, they match.**
- `observation-checks/checks` = `{:C3 check-path-exists :C4 check-decl-in-file
  :C5 check-registry-entry :C6 check-witness-reference :C8 check-registered-run}`.
- `enact-fn`'s default check runs `(get checks/checks (:class check))`.
- The observation contract's `:production-path {:classes #{:C3 :C4 :C5 :C6
  :C8}}` names the same functions under `:checks-implemented`.
- `measured-a-version`'s `:classes` are `class-of` values from
  `sourced-rates`, i.e. contract classes.

**Operationally, not established.**
- `measured-a-version`'s own docstring: "The decision calls cascade-lane
  WITHOUT :observation-labels today … no admitted labels are in scope at
  this site unless the caller declares them". So at HEAD every decision
  writes `{:status :absent :reason :no-measured-rates}`.
- The labels, when supplied, are `{:token-class c :recorded r :admitted a}`
  (`observation-rates/rates-by-class`).
- `check-error-rates` counts per check **kind** (`:test :grep :validator
  :layout`), mapped to classes by a fixture (`check-ledger-classification`).

Nothing states that a class c label's `:recorded` verdict was produced by
`(checks/checks c)` at a declared revision. **⟨1⟩2 row:** write that class
c's A is counted from recorded verdicts of `observation-checks/checks c`, and
that every label names the check function and revision that produced its
`:recorded`. Until then, a consumer cannot show that o's channel is the one
its A measures. The row is a definition, not a ruling.

**A token whose channel class has no measured cell.**
`observation-rates/token-likelihood-rates` gives an unmeasured
`:checkable` class the zero kernel `{:false-neg 0 :false-pos 0 :measurement
:absent}`. A `:judgement` or unknown class gets the typed `:unsupported-class`
or `:unknown-class` refusal. (C2's `kernelSupply` was not read this session.)

SPEC-F's admission list fixes the outcome. It requires "measured-A
provenance (A10)", and the bad cases include "identity-default or unmeasured
A". So:
- **The step is not admissible.** Its record carries the typed refusal (e.g.
  `{:status :refused :kind :unmeasured-a :tokens […]}`).
- **Continuity ends the admitted prefix at that step.** No later step can
  chain, because its sPrev would be a q that was never admitted.
- **The policy's F** is `:computed` over the admitted steps before it,
  `:not-supplied` if none were admitted, and `:zero-support` **only** for a
  contradiction (P(oᵢ) = 0, `observation_model.clj` `:condition`'s
  `:contradiction` and `production-ranked`'s third status). It is not a
  per-step absence inside a computed total.

## 3. The posterior: the call, and what the records lack

`cascade-model-manifest/exact-update` takes `(likelihood-of prior-pushed o)`.
`token-likelihood` takes `(rates state obs)`, ranges over **all rate keys**,
and **refuses** a state or observation token with no rate entry (its
docstring). So the call, with the records' shapes, is:

```clojure
(let [V   (qualify (:target f) (:checked obs))          ; #{[target token] …}
      o   (qualify (:target f) (:o obs))
      r   (select-keys rates V)                         ; rates keyed [target token]
      lik (fn [s o] (token-likelihood r (set/intersection s V) o))   ; C5's restriction
      pushed (rollout (constantly precedence) s-prev 1)]              ; B via transition-row
  (exact-update lik pushed o))
```

- Here sPrev is the run record's `{state 1}`, with states over the qualified
  universe.
- The intersection of s with V is what the restriction lemma (C5,
  `tokenLikelihood_restrict`) licenses. Without it, `token-likelihood`
  refuses every unchecked state token.
- P(oᵢ), and so fᵢ = −ln P(oᵢ), is the same weighted sum (the numeric core is
  `exact-belief-core/condition-predicted`).

**What the records lack for the call to be made from them alone:**
1. **The rates value.** `measured-a` records `:rates-sha` and `:classes`, not
   the rates, and the labels are not on the record. The step cannot be
   recomputed from the run record. F1a-2's write must carry the qualified
   rates value, or the labels plus the contract revision.
2. **o and V qualified by target.** The flight's `:o`/`:checked` are
   unqualified. `(:target f)` qualifies them; the producer does it.
3. **Which B.** The enacted candidate's `:precedence` (the enactment's
   `:candidate`, looked up in the run record's candidates) and its patterns'
   `:transition` from `:domain-inputs`. B is the **model's** prediction for
   the policy's step (every pattern applies), not the realised outcome: a
   failed attempt shows up in o, not in B.
4. **C5's lemma**, for the restriction to be the definition's likelihood.

## 4. Where admitted steps live

The slot is SPEC-F's `[:decision :selection-certificate :token-belief-input
:observation-updates]`. Today it is written by
`token-belief-predecessor/input-receipt` (v3) through
`conditioned-trajectory/intake`:
- the entries are policy-qualified `{:tau :receipt :observation :pre-belief
  :predicted-belief :post-belief :status :consumed :channel :policy}`;
- `:conditioning-status` is `:observed-initialization` when some entry is
  `:updated`, else `:not-run`;
- they serve **next-selection initialisation from signed checks** (D v1),
  and are empty on every production record read by the F discovery.

**`production-ranked` already has them in scope.** war_machine passes
`(select-keys token-belief-input [:conditioning-status :reason
:observation-updates])` as its `conditioning` argument. It stamps
`:not-supplied` for every entry regardless.

F1c-I makes it read `(:observation-updates conditioning)`:
- group the admitted entries by `:policy`;
- match each ranked entry's policy key: the candidate's key, `[mission,
  ordered pattern ids, semilattice]` (`enactment-habit/policy-key-for`'s
  scheme);
- **with admitted steps**, write `:f-prefix {:status :computed :f Σfᵢ :steps
  …}`, or `:zero-support` on a contradiction step;
- **without**, keep `:not-supplied`: a never-executed policy borrows no
  history (SPEC-L).

The existing `evaluate-synthetic` loop runs `om/query` over an
observation-model. The token kernel is not an om model, so F1c-I sums the
admitted steps' own fᵢ (computed by the producer with `exact-update` and
P(o)). It also re-checks the chain qᵢ = sPrev₍ᵢ₊₁₎ rather than re-running the
arithmetic in a second place.

## 5. The packets

**Prerequisite (F1a-2b, one behaviour, tick):** `measured-a-version` writes
the qualified rates value (or the labels + contract sha) beside
`:rates-sha`, so the step's Aᵢ is on the record (§3.1).

**F1b-join-I: the step producer.** One behaviour, at `flight/run!` after the
observation write.
- Fetch the click's run record by `:click-id` (an optional `:fetch-run-record`
  opt, the one `enact-fn` takes; typed absent when missing).
- Bind `{:policy-key (from :increment) :occurrence [flight click] :o :checked
  :channel :a {:rates-sha … :classes …} :b {:precedence … :interpretations-sha …}
  :s-prev :q :p-o :f}` as `:step` on the entry, by the call in §3.
- Or write a typed refusal instead: `:unmeasured-a` (no measured cell for a
  checked token's class), `:policy-key-absent`, `:run-record-absent`,
  `:zero-support` (P(o) = 0).
- *Wire test (first layer):* one enacted click with a fixture run record
  carrying measured rates. `:step`'s q equals `exact-update` recomputed from
  the fixture, and f = −ln P(o).
- *Bad cases pinned here:* identity or unmeasured A (a zero-kernel class
  gives `:unmeasured-a`, not a step); a missing q (no `:step` without q).

**F1b-admit-I: admission per policy.** One behaviour, in the tick. The flight
hands its ordered steps to the next click through `flight/judge-opts`. The
tick admits them into `:token-belief-input :observation-updates`, as entries
with `:policy`, where `token-belief-predecessor/input-receipt` builds the
input (war_machine, `token-belief-input`). Checks:
- unique occurrence;
- same policy key along a prefix;
- sPrev₍ᵢ₊₁₎ = qᵢ;
- measured-A provenance present.

A step failing a check is recorded refused, and the prefix ends there.
- *Wire test:* two steps of one policy, chained, are admitted as two
  entries; a broken chain is refused and the prefix is one step.
- *Bad cases:* duplicate or mismatched occurrence; a prefix joined to
  another policy.

**F1c-I: the consumer.** One behaviour, `production-ranked` as in §4.
- *Wire test:* a candidate with two admitted steps gets `:f-prefix
  {:status :computed :f (+ f1 f2)}`; a candidate with none keeps
  `:not-supplied`; a contradiction step gives `:zero-support`.
- *Bad case:* a two-step total equated to its last summand. The test
  asserts f = f1 + f2 with f1 ≠ 0.

So F1 after F1a-1/F1a-2/F1b-I is **four behaviours**: F1a-2b, F1b-join-I,
F1b-admit-I and F1c-I. Two ⟨1⟩2 rows sit beside them: C5 (the restriction
lemma, already dispatched) and the channel-identity definition (§2).

## Not done

- No code, no registry edit, nothing run, no click.
- **Not read:** C2's `kernelSupply`; `policy/apply-observations`'s body (the
  v3 update that writes today's entries); how a ranked entry's policy key
  is computed on the ranked side (the key match F1c-I needs: the increment
  receipt has it for the enacted candidate; for every ranked candidate it
  must come from the candidate's precedence, `cascade-prior`'s scheme, not
  checked).
- **Not checked:** whether any production run record carries a measured A
  (the docstring says none can today).
