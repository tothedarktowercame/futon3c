# M-first-flights — we have flight data (Orville); we need the data shapes (Wilbur)

Date: 2026-06-11
Status: VERIFY (2026-06-11; IDENTIFY → MAP → DERIVE → ARGUE → VERIFY-in-progress same evening). VERIFY = mockups + day-in-the-life narrative (Joe verifies interactively), then the logic model, then piecewise build with claude-3 stepping.
Owners: fable-2 (mint, human-cockpit standard), claude-3 (the flight half),
fable-1 (machine-trace production sites; schema and logic model).

Cross-references:

- `futon5a/holes/missions/M-war-machine-wiring.md` — the bridge mission. This
  mission is about what the trace of that bridge should look like.
- `README-pilot.md`, `war_machine_pilot.clj`, `repl_trace.clj` — the
  machine-pilot apparatus whose records are the evidence here.
- `futon4/dev/mission-mode.el` — the proof-theoretic anatomy the flight record
  should converge on.
- `futon4/dev/arxana-essays-twoup.el`, `arxana-window-constraints.el` — the
  invariant-backed operator cockpit (commits 481cd06…9985bdc, 2026-06-11).
- Memories `project_arxana_is_wm_for_joe`, `project_substrate_metric`.

## HEAD

Observed live on 2026-06-11, Joe reading the flight-mode panel on the Stack
HUD:

> "whereas the mission-mode buffers are approaching a functional programming
> (Scratch) or proof-theoretic (futon6) anatomy, the flight-mode buffer is
> just a list of numbers (and nulls)."

The flights are real and recorded; the records are lists of numbers and nulls.
Reshape the data before attempting the longer flight.

The model here is the Wright brothers' actual victory order. Kitty Hawk (1903)
was preceded by the wind tunnel (1901): when Lilienthal's lift tables failed
them, they did not build a bigger engine — they reshaped the data, and
controlled flight followed. In AIF terms: a flight record with anatomy is what
lets the forward model learn from a flight. A record reading "prediction
-4.9225 / realised -4.9225 / error 0.0000" in proposal-mode taught the model
nothing. The mission is satisfied when records carry their derivations, so
error has structure to propagate into.

## 1. IDENTIFY — the tension

The record in question (flight `live-d2068cc1`, complete, verbatim):

```
field-read       present                    215 ranked actions
velocity         present                    {:type :address-sorry, :target "sorry/r3d-per-entity-attribution"}
attribution      present                    :pilot-autonomous
prediction       present                    -4.9225
counterfactual   ghost                      :null
begin-state      ghost                      :null
act+witness      absent (proposal-mode)     :null
measurement      untagged                   realised -4.9225 | error 0.0000
out-of-band      none                       :null
self-record      present                    γ frame, 1 turn-record(s)
```

What we have: flights happen and leave records. The loop anatomy is present —
field-read, velocity, gated act, measurement, self-record — and the ghosts are
typed slots, not prose. This is real data from a real instrument.

What we lack: the records are judgments without derivations. The display
asymmetry has a cause and a useful split. The cause is that display quality
tracks the expected reader: mission-mode grew binders and scopes because a
human reads it and pushed back every time it flattened; flight-mode never had
a human reader, so the buffer stayed residue. The split is between rendering
debt (structure exists but isn't shown — "215 ranked actions" is a count
standing in for a typed ranked list that exists as data) and substrate debt
(-4.9225 is genuinely a bare scalar; no renderer can show the geometry of a
number that has none). The substrate debt is the thin-scalar bottleneck and
belongs to M-substrate-metric, not this mission. This mission stops at: the
record carries the derivation the substrate can already produce.

There is a third debt, and it is why this mission matters from the
ground-control seat: return-channel debt. Flight records exist and nothing
trains on them — FutonZero's own caveat is that realized-G does not yet
train the gradient prior (the loop's R2), so today's records are
write-only: produced, rendered, re-read by people, never consumed by the
learner they are nominally for. The rendering and substrate debts explain
why the records are hard to read; the return-channel debt is why fixing
readability alone would be satisficing — this mission could complete fully
while FutonZero gets no closer to its loop. Boundary: building the
training loop is FutonZero v2's work, not this mission's; this mission's
obligation is that the records be consumable by that loop — typed grains,
admissibility tags, grounds in the data — so that when R2 arrives it reads
flights, not prose.

The lineage claim (Joe): "I have been piloting a hypertext-based version of
the War Machine since 2005 — Arxana was first created then — now with agent
support and invariant-backed fidelity." The human cockpit is twenty-one years
old; the machine seat is the younger sibling. The two ascents agree on the
loop anatomy (read field → focus → gated write → trace); this mission brings
the younger ascent's trace up to the older one's representational standard.

This is a mission rather than a task because the unknown is the
specification: what is the term calculus for a flight record? The candidate
ingredients exist (BHK arrows keyed by endpoints, sorry as typed hole,
mission-mode's binder taxonomy) but their composition is not designed.

## 2. MAP — production sites, consumers, ingredients (fable-1)

Production sites:

- `repl_trace.clj` `turn-record` normalises a flight into the record. It is a
  whitelist (absent-unless-supplied — the ghost discipline already lives
  here) and was extended five times this week with tags (`:independent?`,
  `:evidence-ref`, `:realised-source`, `:realised-read`,
  `:predicted-constant`, `:field-delta`). The calculus half-exists as tags
  awaiting composition into terms.
- `war_machine_pilot.clj` `begin-live-cycle!` and `close-live-cycle!` produce
  the lifecycle. The close computes realised values, tags, and grounds — the
  derivations exist at this site and are thrown away after the judgment is
  recorded.
- `scripts/flight_scope_view.bb`, the projector. Key finding: several organ
  judgments ("absent (proposal-mode)", "untagged") are computed at render
  time by projector logic. Judgment-without-derivation is partly a projector
  artifact; the shape spec should move these judgments into the record with
  their grounds, so the renderer renders instead of adjudicating.

Consumers who must keep reading after the reshape: `flight-mode.el` (the
panel), `futon3c.aif.calibration` (the evidence reader — backfill
compatibility is its requirement), `repl_spec_verify.clj` (the V1–V5 checker
— the logic model should extend it, not duplicate it), the futon0 rollout
ledger, and the piano roll / VSATARCS evidence kinds.

Calculus ingredients, located:

1. BHK arrows (futon3a): endpoint-keyed, payload as construction evidence,
   CH2 discharge events. Measurement-as-discharge already has its rule here —
   "no payload, no discharge" is enforced (close throws on `:executed?`
   without a witness).
2. `sorrys.edn` schema v2 `:kind`: the typed-hole vocabulary. A ghost is a
   sorry in the standing sense — type without term, context fixed by the
   run-id.
3. The mission-mode binder taxonomy (which grew this very day — the inventory
   must be checked against the live detector, not hand-frozen).
4. The admissibility tags (`:realised-source`, `:realised-read`,
   `:independent?`) are already judgment slots with enforcement; the spec's
   job is to make each carry its ground.

Handoff boundaries: the EDN shape spec and the logic model stay owner-side
(fable-1). The renderer elaboration, projector rewrite, and substrate-2
round-trip ingest are bellable builds once the spec exists.

## 3. DERIVE — requirements from the pilot's seat

Per Joe's steer, the requirements come from what claude-3 sees as the WM
pilot, not from the apparatus inventory. Two passes converged: claude-3's
written account of the flight as flown, and four synchronous whistle salvos
(fable-1 asking, claude-3 answering, Joe watching the HUD; the full salvo
transcript is in git history at f10793a). The combined findings:

### 3.1 The flight as flown

Reading the field, the pilot does not read 215 entries. The actual read order
per entry is: rank and G-total; action type (which decides measurability —
sorries vanish when discharged, missions persist); target; G-constant (the
dual-prediction gap shows how discriminating a pair would be);
open-hole-count (the cap check); guardrail classification and rule; the
rationale line. Weight and structural-pressure are always skipped, as is the
long tail unless hunting a class.

Choosing a target involves five checks, run in order before trusting it: the
cap check; the pilot-closeable-hole check (grep the mission doc for unchecked
boxes, then read the matched lines and classify each hole as bounded-doable,
operator-gated, or standing-task); the warrant check (proto-PSR — if a
warrant is findable, name it and fly); the operator-relevance check (if the
choice is genuinely Joe's, take the safe default and queue it); the guardrail
class.

Between choosing and closing, the pilot preps the change uncommitted (so the
begin-to-commit window stays short), begins with the chosen target, commits
and captures the sha and the commit time, computes a settle threshold of
commit time plus about 85 seconds (one scan duration — gating on "after the
commit" alone can read a scan that started before the edit), forces a scan,
and then polls until two scans with distinct as-of timestamps, both past the
threshold, agree on the target's G within epsilon 0.005. That two-scan
agreement is what "settled" means. The pair is then judged: clean, null,
fallback, transient, or confounded (was the drift mine or independent?).
Almost none of this reaches the record: the settle comparison, the threshold
arithmetic, the hole classifications, and the warrant reasoning all live in
pilot context and are discarded.

Re-reading old records during the stock-takes exposed three recurring
failures. A 0.0000 error could mean a true null or a censored fabrication —
same number, opposite meanings, recoverable only by re-reading prose. The
grounds for excluding a pair (the transient window, the stale-begin drift)
lived only in prose, and an amendment had to reconstruct them by hand. And
the warrant for a velocity is recorded nowhere — the cycle-5 "was that
escalation correct?" dispute could not be checked against any record.

### 3.2 The requirements register (R0–R10)

R0. Capability preservation: every field of the current record maps into the
new shape; nothing is silently dropped. (The matrix is below.)

R1. Measurement is a discharge judgment carrying its evidence, not a scalar:
predicted (scaled) and predicted-constant (first-class, because the
constant-vs-scaled comparison must compose), realised, the witness, and the
error with its interpretation class (clean / null / fallback / transient /
confound). An excluded or invalid measurement is a discharge without payload
— a typed open sorry, never a fabricated 0.0.

R2. Ghosts are typed sorries that distinguish why they are absent:
`:proposal-mode` (typed absence by design), `:not-yet`, `:excluded-confound`.
The act organ has three states, not two: executed (with witness), proposal,
and refused — the refusal carrying its finding as payload (the Turn-2
teleport refusal drove three live fixes and has no frame at all). Abandoned
partials carry a reason class (superseded / operator-redirect / stale) and a
superseded-by link, and must be distinguishable from a crashed record.

R3. The measurement window is first-class: begin time, action commit, settle
threshold, the two confirming scans with their as-of stamps, epsilon, and the
observed agreement. "Settled with no two-scan witness" becomes a schema
violation, not a vigilance failure.

R4. The field-read carries (or links) its ranked list as bound entities, not
a count. The first RET-descend shows the decision neighbourhood — the chosen
velocity in full, the few neighbours actually weighed and rejected with their
reject-reasons, the warrant, and a cap-band marker explaining why most of the
field is equivalent. The raw dT is one more descend away.

R5. Judgments carry their grounds, and the renderer is a pure function of the
record. Nothing is adjudicated at render time.

R6. Backfill: the existing twenty records stay readable — representable but
distinguishable, flagged as derivation-thin rather than silently upgraded.

R7. The warrant is recorded: the determined-versus-undetermined judgment and
what determined it (standing contract, doc next-step, pattern, operator
direction). This is the judgment ChipWitz rests on, and it currently lives in
no record.

R8. The witness carries its ground: the ref, who verified it, and what the
verification was (for a codex build: that the gates were re-run before the
sha was accepted).

R9. The per-target verification is recorded: holes found, each hole's
classification, and the evidence lines — the grep-and-read work done every
flight and currently discarded.

R10. Typed inter-flight links: applies-lesson-of, re-measures, confounded-by,
supersedes (and a weak cites-finding). Each is taken from a real case; each
is currently prose and unqueryable. With them the piano roll becomes a graph.

R11. Every flight record projects to a training example for the gradient
prior (the learner's requirement, from the whistle review against
futonzero-alphazero.md): (state-ref, the candidate set with chosen and
rejected-with-reasons, predicted-G of both kinds with their g-grain,
realized-G, and a validity mask). Two warrants. First, the confound typing
gains a mechanical consumer: the mask is DERIVED from the typed verdicts
already required (R1's interpretation class, R3's window witness) — never
authored, so no record can declare itself trainable; confounded and
censored pairs mask out of training, which is the anti-laundering
discipline applied to the reward path. Second, this is R4's real warrant:
the decision neighbourhood — chosen plus rejected-with-reasons — is the
futonic analog of MCTS visit counts, the improved policy distribution the
prior should distill. RET-descend was only ever the display of it.

R12. In-flight operator steering is recorded (from claude-3's pilot's-seat
read of the schema, 2026-06-12): when the operator steers mid-flight with
content — not a begin-time direction, not an abandon-redirect — the steer
enters the record as a content-bearing event, so a re-read sees a
co-piloted flight rather than a solo one. The canonical case is the DERIVE
flight itself, reshaped live by four whistle salvos. Co-pilotedness is
derived from the presence of steer events, never authored as a flag (the
R11 discipline again).

### 3.3 Capability preservation matrix

| current row | current value | typed home |
|---|---|---|
| field-read | "215 ranked actions" | linked ranked-dT entity (R4) |
| velocity | the action map | already a term; keep verbatim |
| attribution | `:pilot-autonomous` | typed judgment (R5) |
| prediction | -4.9225 | discharge/have, scaled (R1) |
| counterfactual | null | discharge/have-constant, or ghost `:not-yet` (R1, R2) |
| begin-state | null | the window's begin state (R3) |
| act+witness | "absent (proposal-mode)" | ghost `:proposal-mode` (R2) |
| measurement | realised + error | discharge/want + window grounds + interpretation class (R1, R3) |
| out-of-band | null | ghost with its class (R2) |
| self-record | γ frame | the reflexive slot; keep |

A flight that taught the model nothing (a true proposal-mode null) and a
flight that lied to it (a censored 0.0) must be distinguishable in the data.
That distinction is the point: error needs structure to propagate into.

## 4. ARGUE — why this design is right, not just workable

The design being argued: a flight record is a derivation. Each organ carries
its judgment and the ground for that judgment; absences are typed sorries;
the measurement is a discharge with evidence; flights link to flights. The
register R0–R10 is the contract.

### Pattern cross-reference (with PSR catch-up)

Nine library patterns bear on the design; six of them were minted from the
very flights this mission wants to reshape, which is the strongest available
evidence that the requirements are real.

- `realtime/structured-events-only`: the oldest applicable rule — the trace
  must be typed events, not prose. The current record half-obeys it (typed
  slots) and half-defies it (the grounds live in prose). The design finishes
  the job. PSR: chosen as the base discipline for the whole record shape.
- `structure/two-projections-of-one-quantity`: a flight is one event; the γ
  frame, the panel rendering, and the substrate-2 ingest are three
  projections of it. The schema is the shared quantity; renderers and stores
  must be pure projections (R5). PSR: chosen to justify "the renderer never
  adjudicates."
- `aif/no-self-certification`: a verdict may only be moved by evidence its
  maker did not manufacture. Applied inward (R1, R3): a record cannot claim
  "settled" or "measured" without carrying the witness for the claim. PSR:
  chosen as the schema's enforcement principle — the logic model rejects
  unwitnessed tags structurally.
- `aif/measurement-window-hygiene`: the settle window (threshold, two scans,
  epsilon) becomes R3's first-class object. PSR: the pattern's own cases
  (the transient spike, the stale begin) are the schema's adversarial tests.
- `aif/two-layer-calibration`: the record must keep dynamics evidence and
  value evidence distinguishable; the interpretation-class field (R1) is
  where that distinction lives per pair.
- `futon-theory/crime-relocates-to-a-scarcer-witness`: each laundering fix
  pushed the crime toward scarcer witnesses; this design is the limit move —
  the witness is pushed INTO the record itself, so the cheapest place to
  check a claim is the claim's own row.
- `collaboration-coherence/determined-fork-proto-psr`: R7 records the
  warrant. The pattern says determined forks should be flown on a named
  warrant; the schema makes the naming durable and queryable.
- `futon-theory/honest-map-over-flattering-counter`: R6's backfill rule —
  old records flagged derivation-thin rather than silently upgraded — is
  this pattern applied to data migration.
- `peripherals/pilot-plus-ground-control`: the roles that produce and gate
  these records; R8's witness-ground (who verified, what was re-run) encodes
  the three-way review into the record.

### Theoretical coherence

IDENTIFY anchored the mission in two claims: that error needs structure to
propagate into (the AIF reading), and that a flight record should be a term
in a calculus (the proof-theoretic reading). The design serves both with one
move, because a derivation IS the structure error propagates through, and a
record whose judgments carry grounds IS a proof term with named inference
steps. The ghosts-as-sorries identification is not an analogy: a ghost slot
satisfies the standing definition of a sorry exactly (type without term,
context fixed). The theory has not shifted; it has converged from two sides.

### Trade-offs

- Records get heavier. Mitigated by linking rather than inlining (the raw dT
  is referenced, not embedded; R4) — and the heaviest grounds (the settle
  window) are a handful of timestamps and one epsilon.
- Writing costs move to close-time. Mitigated by the MAP finding: the
  grounds are already computed at close-time today; the change is persisting
  instead of discarding them.
- Two reader paths during the backfill window (derivation-thin vs full).
  Accepted deliberately (R6) — the alternative, silent upgrade, is the exact
  laundering shape this stack exists to refuse.
- The scalar stays a scalar. The design does not give -4.9225 geometry; that
  is M-substrate-metric's work, and pretending otherwise here would re-scope
  the bottleneck this mission explicitly fenced out.

### Generalization

The shape is not flight-specific. "Judgment with ground, absence as typed
sorry, outcome as witnessed discharge" applies to any instrumented loop
turn: the daily scan's probes (where the 無 resolution already demands
term-provenance), codex build gates (R8 is already their record in
miniature), and the human cockpit's own operations. If the schema works for
flights, the same calculus is the candidate for every trace in the stack —
which is why getting it right at the smallest grain matters.

### Plain-language argument

Every flight already does careful work to check its own results — but it
throws the checking away and keeps only the conclusions. So when we re-read
an old flight, we can see what it decided but not why, and two opposite
situations can look identical. The fix is to make every record keep its
receipts: what was checked, against what, with what result. Then a reader —
human or machine — can tell a real result from an accident without taking
anyone's word for it.

### Exit check

The design is inevitable in the required sense: every requirement traces to
a named failure that actually happened, every ingredient already exists and
is enforced somewhere in the stack, and the only new thing is their
composition. The plain-language argument stands alone. ARGUE is satisfied;
VERIFY's hooks are the logic model (exit-2) with the stale-begin confound as
its canonical adversarial case.

## 5. VERIFY (in progress) — mockups + narrative first, logic model second

Per Joe: a logic model is one way to do VERIFY, not the only way. The plan
here is reader-verification first, machine-verification second:

1. A narrative walkthrough — "a day in the life of a War Machine pilot" —
   with mockups of real flights rendered in the new shape:
   `first-flights-day-in-the-life.md` (this directory). Convention: values
   the current records contain are verbatim; values only the new shape
   would record are marked `~` illustrative — those are exactly what we
   currently discard. Joe verifies interactively: each beat should read
   without prose archaeology, the verbatim/illustrative split should be
   honest, and anything still missing on a re-read is a new requirement.
2. The logic model (extending repl_spec_verify V1–V5), with the
   stale-begin confound as the canonical adversarial record.
3. Build piece by piece, claude-3 stepping through the realised model
   after each piece (the seat that specified the shape checks the shape).

First VERIFY finding (V-f1, from Joe's read of the Morning beat): the
field-read must not be a temperature gauge. Its typed home has three
layers — the gauge (ranked list, linked), the decision neighbourhood
(candidates weighed, reject-reasons), and per candidate an optional
pattern CASCADE (psi, patterns, C — the M-memes-arrows sense; thickness is
information: thin says plain code, thick says pattern territory). Choice
points (A or B) are first-class, carrying the warrant that determined them
or the queue-entry when undetermined. This refines R4 and R7; the
data-shape diagrams are in the day-in-the-life doc.

Second VERIFY finding (V-f2, Joe): G is defined over policies, not
actions — the field's per-action temperatures are G(s, a), a one-step
approximation the charter itself calls "one-step-biased." The schema
therefore types every prediction with its approximation grain
(`g-grain: one-step-action` today; policy-grade is a typed ghost blocked
by the rollout-engine excursion), records the velocity as a policy
PREFIX (the pilot's intended plan sketch, not just the first action),
and notes that the cascade is the policy-shaped object in the current
stack. Refines R1 and the velocity row of the preservation matrix;
boundary unchanged (building policy-G is the rollout engine's charter,
not this mission's).

## 5. Consequence if the shapes land

The piano roll changes type, from numeric tape to proof script — a sequence
of terms with open sorries. Flights compose the way mission documents
compose; the agreement between the two cockpits becomes checkable rather
than rhetorical; and the forward model gets structure for its error to
propagate into.

## 6. Scope

In: the data shape for flight records (flight-as-derivation); the render lane
(flight-mode rows elaborated with the mission-mode anatomy, RET descending
into structure); backfill compatibility for the existing records.

Out: the ground metric behind the EFE scalar (M-substrate-metric); aif2
tension-proposer wiring (M-aif2); any autonomy change at the consent gate —
proposal-mode stays a typed absence, this mission makes it renderable, not
fillable.

## 7. Exit conditions (phased, per Joe's V-f2 compromise)

The mission exits in two phases, matching the two grains of G: the action
grain we can record honestly today, and the policy grain that waits on the
rollout engine. Phase A is this mission's commitment; Phase B is its
standing obligation, armed when the substrate catches up.

Phase A — actions (one-step grain, buildable now):

1. A written shape spec (EDN schema) for flight-as-derivation, with the
   capability preservation matrix discipline and every prediction typed
   `g-grain: one-step-action`.
2. A logic model before code, extending repl_spec_verify's V1–V5: a
   conforming witness record passes; per-invariant adversarial records are
   caught — including "judgment without derivation" (the stale-begin
   confound flight is the canonical case) and "mixed grains" (a record
   claiming policy-grade G without the rollout engine must be rejected).
3. flight-mode renders one real flight in the new shape: ghosts as typed
   holes, measurement as discharge with linked evidence, RET-descend on
   the field-read (gauge → neighbourhood → cascade where present).
4. One round-trip: a flight record ingested into substrate-2, browsable
   from the human cockpit like an essay.
5. The loop-closing demonstration (G-REWARD-shaped, where exits 1–4 are
   G-SIM-shaped): run the calibration lane over the twenty backfilled
   records plus the conforming witness flight and exhibit one number that
   moves — minimally, calibration error stratified by interpretation
   class, computed from the records themselves (clean-class error beside
   pooled error; fallback and confound classes visibly masked out by the
   R11-derived validity mask, with counts). The demonstration must be one
   that fails on derivation-thin records — today the classes live in
   prose, so today this stratification cannot be computed at all. This is
   deliberately minimal: not "train the prior" (FutonZero v2's work), just
   one demonstrated consumption proving the typing pays rent in the
   learner.
6. Joe flies one session reading the new instrument beside the old; the
   operator's verdict is the Phase-A gate.

Phase B — policies (opens when the rollout engine lands; not before):

6. The prediction organ's policy-grade slot fills: G(s, π) over the
   pilot's plan sketch, with the one-step numbers retained beside it so
   the two grains calibrate against each other on the same flights.
7. Plan-vs-realised: the velocity organ's policy prefix is compared, step
   by step, against the flight as actually flown — deviations typed
   (improvised / blocked / redirected), because the deviations are where
   the policy model learns.
8. The cascade scored as a policy: at least one flight whose candidate
   cascade carries a rollout-grade G, closing the loop between the
   M-memes-arrows cascade object and the AIF policy object.
9. Joe's second side-by-side verdict, on policy-grade records — the
   Phase-B gate.

## Checkpoints

Checkpoint 0 (2026-06-11): mission minted at IDENTIFY from the live
flight-mode reading; tension recorded with verbatim evidence; the two-debt
split and the M-substrate-metric boundary made explicit; lineage claim
recorded; exit conditions drafted.

Checkpoint 1 (2026-06-11, late): MAP — three production sites located, with
the central finding that the derivations exist at close time and are
discarded; five consumers named; four calculus ingredients located. Handoff
boundaries set.

Checkpoint 2 (2026-06-11, late evening): DERIVE — requirements gathered from
the pilot's seat by two convergent passes (the written account and four
whistle salvos), reconciled into the single register R0–R10. Every
requirement traces to a named case from the twenty flights. The DERIVE
flight self-demonstrated the stale-begin confound, giving exit-2 its
canonical test case.

Checkpoint 3 (2026-06-11, night): style normalization — the document
rewritten in plain markdown at Joe's direction ("the usual style is just
plain markdown"); content preserved, the salvo transcript condensed into
§3.1 (full transcript in git history at f10793a).

Next: the schema draft against R0–R10 (fable-1, owner-side), the logic
model, then the pilot flies the conforming witness flight.

Checkpoint 4 (2026-06-11, night): ARGUE — per mission-lifecycle.md: nine
library patterns cross-referenced with PSRs (six of them minted from these
very flights), theoretical coherence confirmed (the two IDENTIFY readings
converge on one move), four trade-offs accepted explicitly, generalization
noted (the shape is the candidate for every trace in the stack), and the
plain-language argument written. Exit criterion met: every requirement
traces to a real failure; the only new thing is the composition.

Checkpoint 5 (2026-06-11, night): VERIFY findings V-f1 (field-read =
gauge + neighbourhood + cascade; choice points first-class) and V-f2
(G is over policies — predictions typed by grain, velocity as policy
prefix, cascade as the policy-shaped object) folded in from Joe's
interactive read of the day-in-the-life narrative. Exit conditions split
into Phase A (actions, now) and Phase B (policies, armed by the rollout
engine) per Joe's compromise.

Checkpoint 6 (2026-06-11, close of the verify-read): operator verdict on
the narrative half of VERIFY — accepted as Phase-A preparation that
properly acknowledges Phase B. Joe's standing course-holding criterion,
recorded: do not drift from the vision of a real policy-grade G(s, π);
build progressively toward milestones of that nature, with fable-2's
review (the return-channel debt, R11, the loop-closing exit) taken into
account. Incoming modular lane: Joe and fable-2 will explore different
ways of SAMPLING CASCADES. The receiving seam already exists in the
register: V-f1's per-candidate cascade slot. One schema consequence,
recorded now so the seam stays clean: the cascade field carries sampler
provenance ({psi, patterns, C, sampler}) — different sampling methods must
be distinguishable in the data, the same anti-mixing discipline as
g-grain, so sampled cascades can be compared rather than silently pooled.
Remaining VERIFY: the logic model (machine half). Then the Phase-A build.

Checkpoint 7 (2026-06-12): Phase-A exit 1 drafted — the shape spec at
`holes/specs/flight.spec.edn` (v0.1, parses, check-parens OK). The
calculus is one rule applied everywhere: every organ slot is a term with
its ground or a typed sorry with its kind, nothing in between. The ten
display organs become thirteen schema cells — the three new ones (warrant,
verification, window) are exactly the discarded work R7, R9, and R3
rescue. G values wear their grain (V-f2); cascades wear their sampler
(checkpoint 6); the R11 training projection is specified as a pure
function of the record with the mask derived from class + window, never
authored; the preservation matrix (R0) maps every current row to its
schema path; and eight invariants F1–F8 are stated with their adversarial
records, ready to become the exit-2 logic model. Awaiting claude-3's
pilot's-seat read (the seat that specified the shape checks the shape),
then exit 2.

Checkpoint 8 (2026-06-12): the pilot's-seat read of the schema (claude-3,
one whistle round-trip) returned one fix, one honesty edit, and one new
requirement — all folded, spec bumped to v0.2. The fix: the warrant cell
was too ref-centric; real determined forks often resolve by a synthesis of
weak signals, and forcing those into a ref would make the pilot invent
one — so `:pilot-synthesis` is a warrant kind whose ground is the recorded
reasoning itself. The honesty edit: the window's scans are close-time
labour (driving ticks, polling to settle) and the neighbourhood's
reject-reasons carry an articulation cost — performed work being kept, not
data lying free; the projector rewrite must not assume otherwise. The new
requirement is R12 (in-flight operator steering as content-bearing events;
co-pilotedness derived, never authored; canonical case = the DERIVE flight
itself), with invariant F9 added for the exit-2 logic model. The pilot is
ready to fly the conforming witness flight. Next: exit 2, the logic model
over F1–F9.

Checkpoint 9 (2026-06-12): Phase-A exit 2 done — the logic model at
`scripts/flight_spec_verify.clj` (companion of repl_spec_verify.clj: that
one checks the operator's steps V1–V5 over a trace; this one checks one
step's record F1–F9). Self-test: the hand-authored witness conforms with
mask-in; the derivation-thin backfill record conforms-as-thin with
mask-out (the exit-5 demonstration's seed); nine adversarials — each from
a real failure among the first twenty flights — are each caught by the
invariant that names them, 9/9. The model earned its keep on run 1: the
mask's class+window rule alone let the censored-0.0 adversarial mask IN
(a fabricated-but-consistent settled window passes R3 while F3 catches
the exact-copy fraud), so the projection rule now additionally requires
full conformance — no record failing any invariant may train. Spec bumped
to v0.3 with the finding. Witness EDN at
`holes/specs/traces/flight-witness-model.edn` (the file-arg path verified
against it); it stands in only until claude-3's real conforming witness
flight lands (belled, in progress). Gates: kondo 0/0, check-parens OK,
self-test exit 0.

Checkpoint 10 (2026-06-12): the conforming witness flight is REAL and the
record CONFORMS — exit 2 met on real data. claude-3 flew live-957a4836
tight (18s begin window, the DERIVE confound's correction): closed
M-daily-scan §367 (the operator's 無 resolution recorded; act witness
futon7 729ee02), ohc 3→2, predicted −4.0791 vs realised −4.0397, error
0.0394 = the per-hole increment, two post-threshold scans agreeing within
7.6e-5, class :clean. The hand-recorded thirteen-cell record
(holes/specs/flight.witness.live-957a4836.edn, ea44a84) passes F1–F9
after one conformance pass: 9/9, derived mask IN, co-piloted TRUE (the
R12 steer = the flight-request bell itself). The run surfaced two
convergence findings, both folded: (1) the checker assumed an inline
window while the pilot read the spec layout as a thirteenth sibling cell
— the pilot's reading matched checkpoint 7's own words, so the checker
now resolves both and the spec disambiguates (v0.4); (2) the record's
out-of-band vocabulary drifted (:event/:ts vs :type/:at) and two
timestamps were placeholders — fixed against the stores verbatim
(discipline-events.edn merge at 10:02:02.743Z; the steer = the bell job's
created-at 09:54:30.935Z). Honest extra from the pilot: the begin sat at
the continuity point (scaled == constant there), so this clean pair is
increment-clean but MODEL-INDIFFERENT — discriminating pairs need
off-continuity begins. The adversarial twin (live-c1028918, the DERIVE
stale-begin confound) is named in the witness's typed links: exit 2 holds
both witnesses from real flights. Phase-A remaining: exits 3 (render
lane), 4 (substrate-2 round-trip), 5 (the loop-closing stratification),
6 (Joe's side-by-side); build-order step 2 (close-live-cycle! persists
the grounds) ends hand-recording.

Checkpoint 11 (2026-06-12): build-order step 2 landed — close-live-cycle!
persists the flight record; no pilot ever hand-records again. New pure ns
`futon3c.aif.flight-record` (compose + write `<run-id>.flight.edn` beside
the γ frame); the composition rule is the mission's discipline made code:
apparatus facts become terms with grounds, pilot judgments pass through
the new `:flight` opt (neighbourhood, warrant, verification, window,
class, plan-sketch, steers, links), and absences become typed sorries —
nothing fabricated: no guessed class (only the censored fallback derives
mechanically, plus :transient from the read protocol), no invented
warrant, no verification asserted on the pilot's behalf; a record missing
a pilot judgment fails the verifier honestly. begin-live-cycle! now
records :begin-at and :scan-as-of (the stale-begin confound is checkable
only if these are data), and the merge event shares ONE instant between
discipline-events.edn and the record's oob organ (the exact-timestamp
lesson from the witness conformance pass). Recording change only — no
behavior change. Gates: 36 tests / 191 assertions across flight-record +
the four pilot suites, 0 failures; kondo 0/0; check-parens OK; hot-loaded
into the serving JVM via Drawbridge (defonce-safe). Next: claude-3 steps
it live (the seat that specified checks the realised model), then the
render lane (exit 3).

Checkpoint 12 (2026-06-12): build-order step 2 STEPPED live by claude-3 —
the seat that specified the schema checks the realised model. This flight
(run-id live-df706c45) is the first record close-live-cycle! auto-persisted
through the new :flight opt; no hand-recording. Flown at an OFF-CONTINUITY
begin (M-first-flights ohc 4: the state-sensitive model predicts -4.1206,
the state-blind constant a different value) so the pair DISCRIMINATES, and
as a clean :null — the act is this doc-edit, a VERIFY content-advance that
moves no [ ] hole, so the field holds and the state-sensitive model is
right to predict no-move while the constant misses. The discriminating
shape the continuity-point witness (live-957a4836) lacked. The persisted
<run-id>.flight.edn beside the γ frame is the evidence, run through
scripts/flight_spec_verify.clj and belled to fable-1 with the seat-read.

Checkpoint 13 (2026-06-12): the step-read's apparatus finding fixed — the
:flight opt contract is now symmetric and documented. claude-3's stepping
flight (live-df706c45, checkpoint 12) tripped the split contract: compose
wrapped :neighbourhood/:window but required :warrant/:verification to
arrive pre-wrapped, an undocumented asymmetry that cost the first
stepping pilot a failed first verify. Fix: cellify — each of :warrant /
:verification / :window accepts either the bare judgment (compose wraps,
deriving the ground ONLY from content the judgment already carries: the
ref for ref-kind warrants, the reasoning for :pilot-synthesis, the inline
evidence lines for verification) or a pre-wrapped cell passed through
untouched. Contract documented in the compose docstring. The stepping
flight itself is the prize: an OFF-CONTINUITY NULL (ohc 4, clean :null,
settled to 0.0) on which the state-sensitive model erred 7.65e-5 against
the constant model's 0.0396 — ~517x — so a null at an off-continuity
begin is the cheapest discriminating pair available under the clean-
target wall (pattern mint = the pilot's, with live-df706c45 and
live-957a4836 as the canonical contrast cases). And the loose begin
window (3.4 min, a file-race re-prep) was flagged in the record's own
window-ground and is checkable from :begin-at — the confound is data now,
not a story; step 2 doing its job on its first live use. Gates: 9 tests /
51 assertions; kondo 0/0; check-parens OK; Drawbridge reloaded; the
stepped record re-verifies CONFORMS with mask IN. Next: exit 3, the
render lane (bellable; claude-3 dispatches per single-dispatch-per-kind).
