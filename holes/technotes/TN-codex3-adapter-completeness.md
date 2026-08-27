# TN-codex3-adapter-completeness — specification of the model/runtime seam

Codex-3, 2026-08-27. Specification only. No code or running coordinator was
changed. This is intentionally general; codex-2's concurrent work on the three
cross-transition trace observations is one future instance of this
specification, not its definition.

## Result

The proposed three obligations identify the right problem, but totality plus
no-strays plus groundedness is not sufficient. It would accept both of these
bad systems:

- a watchdog producer exists and has a model type, but no coordinator calls
  it;
- a trace projector exists and covers every model type, but frame admission or
  certification never consumes its output.

It also overstates “exactly one”: one committed transition may legitimately
produce a successor-durability observation, a progress observation, and a
delivery observation. Conversely, one model observation type may have several
platform-specific producers. The requirement is an explicit, checked
refinement relation, not necessarily a bijection.

The adapter-completeness claim needs four obligations and two separate
soundness conditions:

1. **Model coverage:** every modelled observation obligation has at least one
   live producer for every live variant to which the obligation applies.
2. **Live closure:** every relevant live effect/observation producer is related
   to one or more declared model obligations, with multiplicity explicit and
   ambiguity rejected.
3. **Mandatory mediation:** every relevant execution path constructs, invokes,
   persists, and—where the observation is a gate input—consumes the related
   adapter. Mere module existence does not count.
4. **Runtime identity and freshness:** the producer and consumer checked are
   the ones loaded and used by the admitted process, under the same schema and
   policy versions as the model.

The two soundness conditions are:

5. **Witness fidelity:** an observation is derived from the same immutable
   input or committed transition witness on which the machine acted, not
   recomputed from a parallel view.
6. **Closed construction:** the inventory of live variants and paths comes
   from constructors/effect boundaries that the machine must use. A list that
   can omit an implementation while that implementation still runs is
   documentation, not grounding.

Obligations 1–4 are the decidable completeness check. Conditions 5–6 state
what makes its result meaningful. Parts of them are mechanically checkable;
the remaining runtime, compiler, filesystem, and cryptographic assumptions
form the trusted base and must be named in the certificate.

## Formal shape

For an adapter class `K`, define finite, generated sets at a version `v`:

- `Mᵥ(K)`: model obligations/types declared by the Lean emitter;
- `Lᵥ(K)`: live adapter instances/variants admitted by closed constructors;
- `Pᵥ(K)`: relevant live execution paths/effect boundaries;
- `Rᵥ(K) ⊆ Lᵥ(K) × Mᵥ(K)`: declared refinement relation;
- `usesᵥ ⊆ Pᵥ(K) × Lᵥ(K)`: adapters mandatorily used by each path;
- `requiresᵥ(m, p)`: model obligation `m` applies to path `p`;
- `consumedᵥ(p, m)`: the produced observation is persisted and, if `m` is a
  gate, its verdict controls the protected effect.

Adapter completeness is:

```text
ModelCoverage(v, K) :=
  ∀ p ∈ Pᵥ(K), ∀ m ∈ Mᵥ(K),
    requiresᵥ(m, p) → ∃ l ∈ Lᵥ(K), usesᵥ(p, l) ∧ Rᵥ(l, m)

LiveClosure(v, K) :=
  ∀ p ∈ Pᵥ(K), ∀ l ∈ Lᵥ(K),
    usesᵥ(p, l) → ∃ nonempty S ⊆ Mᵥ(K),
      declaredRefinementSet(l) = S ∧ ∀ m ∈ S, Rᵥ(l, m)

MandatoryMediation(v, K) :=
  ∀ p,m, requiresᵥ(m,p) →
    produced(p,m) ∧ persisted(p,m) ∧ consumedᵥ(p,m)

RuntimeCoherent(v, K) :=
  modelDigest = generatedSchemaDigest = constructorInventoryDigest
  ∧ admittedRuntimeDigest = loadedProducerConsumerDigest
  ∧ observationSchemaVersion = policyVersion = admittedVersion

AdapterComplete(v, K) :=
  ModelCoverage ∧ LiveClosure ∧ MandatoryMediation ∧ RuntimeCoherent
```

`LiveClosure` does not allow an adapter to claim an arbitrary nonempty set. Its
refinement set is part of generated adapter metadata and each member has a
field/transition refinement obligation. If policy requires a unique owner for
an effect, add `UniqueOwner(K, m, p)` for that class; do not impose uniqueness
where fan-out is intended.

The useful meta-theorem is then conditional:

> For an admitted version `v`, if all relevant effects are constructible only
> through the closed constructors, the admitted runtime matches `v`, and each
> registered adapter satisfies its witness-refinement obligation, then every
> model-required observation is produced and consumed on every applicable live
> path, and every relevant live adapter is accounted for by the model.

This is not one theorem proving facts about an open Clojure process. It is a
Lean theorem over emitted finite inventories plus build/admission checks that
establish its premises for a concrete runtime. Calling only the Lean part “the
meta-theorem” would conceal the premise most likely to fail.

## Grounding: one mechanism is not enough

### 1. Source → loaded runtime (M7)

Ground the live inventory in the admitted process, not Git:

- build emits a manifest of namespaces/resources/Vars that implement protected
  adapters and consumers, with content or compiled-definition digests;
- loading/reloading registers the actual Var roots and resource digests under
  a runtime epoch;
- admission asks the JVM for that registry and compares it with the emitted
  manifest and model/schema digest;
- any reload of a protected namespace invalidates the admission certificate;
- mint/dispatch refuses an absent, stale, duplicate, or mismatched runtime
  epoch.

A Git SHA alone is insufficient: a dirty canonical checkout, partial reload,
or altered resource can disagree with it. A registry populated by a separate
manual call is also insufficient. Registration must occur as part of loading
the adapter/consumer definition, and protected execution must require the
resulting runtime capability.

This still trusts JVM introspection, digest implementation, classloader
identity, and the claim that all protected execution goes through the checked
Vars. Those are explicit trusted-base assumptions, not Lean conclusions.

### 2. Module → machine invocation (watchdog)

Ground this at construction. A runnable coordinator must not be constructible
from a tick function alone. Its constructor requires a supervision bundle
containing the watchdog, durable-disable authority, progress cursor source,
clock/deadline policy, and violation sink. The scheduler invokes the bundle as
part of every lifecycle path. Recovery reconstructs the same typed bundle from
the durable adapter registry.

The closed inventory is the set of constructors registered with the durable
coordinator, and the guarded execution entry is private behind the returned
capability. Build checking enumerates constructor descriptors; admission
constructs each enabled coordinator and verifies the bundle/version in its
runtime descriptor. A public/raw alternate start or direct tick entry makes
closed construction false and must fail the check.

This is why “watchdog function exists” is not totality. Totality is: every
constructible running coordinator path necessarily invokes one.

### 3. Machine → emitted trace

Ground observations in the transition/event algebra. Each protected effect
commits a typed event through the same transaction boundary that authorises or
records the effect. The trace is a total projection over those event variants;
required cross-transition observations are not optional caller arguments.
Certification/admission invokes the checker and consumes its verdict.

Generation should yield all of the following from one Lean declaration:

- JSON schema/type tag for each observation;
- Clojure event/projection descriptor;
- required/applicability relation;
- checker input field and policy version;
- completeness fixture that enumerates all required types.

The Clojure event algebra is closed at the protected append function: unknown
event types and missing required projections are rejected. The checker rejects
empty required observation collections when applicability says they should be
nonempty. The certification transition requires a checker receipt bound to the
trace digest.

This gives the required falsifiability result. At `ae438faa`, Lean added
`TraceProgressObservation`, `TraceSuccessorObservation`, and
`TraceDeliveryObservation`, and emitted policies requiring them. The then-live
`campaign_trace.clj` emitted none of the three, `from-durable-state` projected
none, and no production path invoked the checker. Therefore:

- `ModelCoverage` fails for all three model types;
- `MandatoryMediation` fails because there is no production or consumption;
- the generated model/schema digest cannot equal the old trace adapter
  descriptor.

The build must fail before merge if generation and Clojure adapter descriptors
are checked together. If incompatible commits somehow coexist, admission must
fail before a frame is minted or certified. A formulation that accepts
`ae438faa` with the pre-existing `campaign_trace.clj` is rejected by this spec.

### 4. Request → persisted authority (M3)

Ground this in one generated authority schema and an opaque constructor:

- the controller request is decoded into a generated authority value;
- persistence accepts that value, not an arbitrary `select-keys` map;
- consumers receive an authority capability or load the exact persisted value
  by digest;
- each field used by a policy is declared in the generated dependency set;
- construction fails when a required request field is absent, and persistence
  round-trips every declared field without loss or addition;
- the protected action accepts only the persisted authority digest/capability.

The live set is grounded by consumers declaring field dependencies through the
generated accessor/capability. Direct map access to controller requests or
handwritten authority projection must be unavailable at protected boundaries,
or detected as a forbidden stray path. Merely generating a nicer
`authority-fields` list repeats the defect one level higher.

For the holdout specifically, completeness of field transport is necessary but
does not fix the policy's shelf-derived domain. The common pre-serve gate and
depositor-truth predicate remain separate requirements.

## Where it is checked

Both build and admission are required because they establish different
premises.

### Build check

The build regenerates model types, schemas, adapter descriptors, field
dependencies, applicability, and refinement sets; compares them with every
closed constructor/event variant; compiles the Lean decidable check; and runs
negative fixtures that remove one model type, producer descriptor, invocation,
consumer, or authority field. This catches source-level incompleteness,
including `ae438faa` against the old Clojure tree.

Expected cost: schema generation and finite-set comparison should be below a
second or a few seconds; Clojure compilation/tests remain ordinary namespace
tests; the Lean checker build is seconds to tens of seconds depending on cache.
It belongs in the relevant build/CI gate, not every two-second tick.

### Admission check

Before mint/dispatch (and again after a protected reload), admission compares
the loaded runtime registry and digests with the build certificate, constructs
enabled adapter bundles, runs a small typed canary through producer → persist →
project → checker/consumer, and binds the result to a runtime epoch. This
catches committed-but-not-loaded code, stale generated resources, missing
runtime wiring, and checker non-invocation.

Expected cost: digest/registry checks are milliseconds; local canaries should
be seconds. Any external network/compiler action should use pinned local
fixtures at admission, not spend a real frame. Per transition, only capability,
epoch, schema, event append, and checker-receipt references are checked—bounded
local overhead, not a full rebuild.

Build-only passes stale runtimes. Admission-only can bless source omissions if
its expected inventory is derived from the same incomplete runtime. The two
certificates must cross-bind independently generated model/build and observed
runtime facts.

## Completeness is not truthfulness

An adapter can be complete and lie. For example, a producer can set
`semanticCursorAdvanced := true` by comparing a separately reread projection
while the machine acted on a stale in-memory state. All types, calls, and
consumer gates are present; the observation is false.

Truthfulness requires a **witness-refinement contract** for each relation
`R(l,m)`:

- observation fields are projections of the exact immutable request,
  pre-state, post-state, result, and effect receipts at the transition commit;
- the event records their digests/versions and transaction or tick id;
- controller-derived facts cannot be supplied by the role or recomputed from a
  later mutable slot;
- booleans that summarise evidence retain links to the underlying witness;
- the protected effect and observation are ordered in one transaction/protocol
  (pre-effect gate or append-before-successor as appropriate);
- independent replay from the durable event history yields the same
  observation, and disagreement fails closed.

Prefer constructors that make false combinations unrepresentable: derive
`predecessorPersisted` from a successful append receipt rather than accept a
Boolean; derive loaded digest from runtime registration rather than accept a
caller string; derive delivery from an inbox/push receipt rather than accept
`"delivered"`. Where the external world prevents atomicity, use an intent,
effect receipt, and reconciliation state instead of an asserted success flag.

The meta-theorem can prove that every required observation crosses the seam. It
cannot by itself prove filesystem durability, cryptographic collision
resistance, clock correctness, honest external services, correct model
semantics, or correct refinement functions. Those are the theorem's premises
and trusted base. Mutation/property/replay tests test them; they do not turn
them into logical consequences of completeness.

A green completeness certificate over a lying producer is indeed worse than an
absent producer if operators treat it as truth. Certificates must therefore
report completeness and witness-refinement status separately; admission
requires both, and diagnostics say which failed.

## Defect coverage

The current `TN-apm-defect-register.md` contains **26 ids**, not 24: D1–D5
(5), A1–A8 (8), M1–M10 (10), and C1–C3 (3). Its summary still says 24. The map
below covers all 26 rather than silently choosing two to omit.

### Would have caught, given the relevant model declaration

- **D1, D2:** `TraceSuccessorObservation` plus mandatory append/consume would
  reject successor creation without a complete persisted predecessor. It does
  not provide retention (D4).
- **A1:** `TraceProgressObservation` plus mandatory coordinator construction
  would reject a watchdog that exists but is not invoked. This detects and
  halts a stall; it does not make progress inevitable.
- **M1, M3:** generated authority dependencies and mandatory serving capability
  would reject the dropped holdout field and an unmediated search carrier.
- **M6:** this is the direct carrier/model coverage obligation. A registered
  search carrier outside the model fails live closure; a model carrier without
  a producer fails model coverage.
- **M7:** runtime identity/freshness rejects a committed fix that is not loaded,
  or a loaded definition that differs from the admitted build.
- **M8:** build/admission cross-binding rejects emitter/contract/validator/
  runtime version or schema drift and rejects a checker that is never consumed.
- **C1:** `TraceDeliveryObservation` plus a delivery effect constructor would
  reject polling-only status labelled `delivered`, if delivery is in the
  declared protected effect inventory.

`ae438faa` is the decisive example spanning **A1, D1/D2, and C1**: all three
model observation types are present while their producers and checker
invocation are absent, so the completeness check fails.

### Would catch only after the model/inventory states the missing property

- **A2:** first-tick failure/no alarm is caught only if terminal-failure halt
  and notification/record consumption are declared obligations.
- **A3–A6:** stop, tick claim, quiescence, and reconciled-status paths must first
  be modelled as protected lifecycle effects. Completeness then catches raw
  stop/status paths, but does not invent the correct lifecycle.
- **A7, A8:** completeness can require every tick/launch path to produce and
  consume typed failure observations. It cannot prevent callback exceptions or
  decide retryability without a fault model.
- **M4:** it catches late/unconsumed gates only if timing relative to the first
  information-revealing/irreversible effect is part of `requires` and the
  refinement contract.
- **M5:** it can require an attribution producer and budget consumer, but the
  apparatus/student/mixed semantics and evidence must be modelled first.
- **M10:** it can close a generated error-code/effect-boundary inventory, but
  the taxonomy and recovery semantics must first exist.
- **C3:** it catches conflation only if execution outcome and policy verdict are
  distinct modelled fields/types and all consumers are required to use them.

### Would not have caught by adapter completeness alone

- **D3:** mutable-slot temporal ambiguity can be faithfully and completely
  adapted; append-only event identity is a storage/observation semantics
  requirement.
- **D4:** a referenced event can later expire after a complete truthful trace;
  retention/resolvability is a durability property.
- **D5:** consuming the wrong attempt budget can be completely adapted to a
  model that specifies the wrong policy.
- **M2:** shelf-derived holdout can be perfectly adapted while the model's
  domain is wrong. Depositor-truth is a substantive policy correction.
- **M9:** refusal to void a closed frame is domain semantics, not an omitted
  adapter, unless a retroactive-disposition transition is first specified.
- **C2:** a headless fork acting under the parker's identity is primarily an
  identity/authority and delivery-topology defect. Completeness helps only
  after session identity and allowed wake executor are modelled as protected
  adapter facts; it does not discover that policy.

Thus adapter completeness would have caught the four motivating omissions and
several siblings, but it is not a universal defect theorem. It prevents the
machine from claiming that a declared model/runtime seam is wired when it is
not. It does not prove that the model asks for the right thing, that the
producer tells the truth, that evidence remains durable, or that external
effects succeed.

## Acceptance criteria for an implementation

1. The check fails `ae438faa` paired with the pre-existing
   `campaign_trace.clj`, naming all three missing producers and missing checker
   consumption.
2. Removing any generated authority field used by an access policy fails the
   build; loading an older authority adapter fails admission.
3. Defining but not installing the watchdog fails constructor coverage;
   installing but not invoking/consuming it fails mandatory mediation.
4. Adding a new serving carrier without a refinement registration fails; a
   registration that does not receive/use the common serve capability fails
   the guarded-path check.
5. A committed-but-not-loaded producer fails runtime coherence.
6. A complete producer fed from a different transaction/projection is reported
   as witness-refinement failure even when adapter completeness is green.
7. Build and admission certificates enumerate model obligations, live
   variants, execution paths, relation edges, producer/consumer digests,
   applicability, runtime epoch, and trusted-base assumptions; an empty
   required set is never silently accepted.

Only after these pass may the system say: “for this admitted runtime version,
every declared model observation is produced and consumed on every applicable
machine path, and every protected live adapter is represented in the model.”
It must not shorten that to “the trace is true” or “the model is complete.”
