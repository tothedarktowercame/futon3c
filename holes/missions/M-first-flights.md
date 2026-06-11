# M-first-flights — we have flight data (Orville); we need the data shapes (Wilbur)

Date: 2026-06-11
Status: IDENTIFY → MAP → DERIVE done (same evening); next is the schema draft.
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

Observed live on 2026-06-11, Joe reading the flight-mode panel on the Stack
HUD:

> "whereas the mission-mode buffers are approaching a functional programming
> (Scratch) or proof-theoretic (futon6) anatomy, the flight-mode buffer is
> just a list of numbers (and nulls)."

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

## 4. Consequence if the shapes land

The piano roll changes type, from numeric tape to proof script — a sequence
of terms with open sorries. Flights compose the way mission documents
compose; the agreement between the two cockpits becomes checkable rather
than rhetorical; and the forward model gets structure for its error to
propagate into.

## 5. Scope

In: the data shape for flight records (flight-as-derivation); the render lane
(flight-mode rows elaborated with the mission-mode anatomy, RET descending
into structure); backfill compatibility for the existing records.

Out: the ground metric behind the EFE scalar (M-substrate-metric); aif2
tension-proposer wiring (M-aif2); any autonomy change at the consent gate —
proposal-mode stays a typed absence, this mission makes it renderable, not
fillable.

## 6. Exit conditions

1. A written shape spec (EDN schema) for flight-as-derivation, with the
   capability preservation matrix discipline.
2. A logic model before code, extending repl_spec_verify's V1–V5: a
   conforming witness record passes; per-invariant adversarial records are
   caught — including "judgment without derivation," which must be
   representable but distinguishable. The canonical test case exists: the
   DERIVE flight itself hit the stale-begin confound it documents (begun
   before the writing; the field drifted; the pair was excluded in
   proposal-mode). A record carrying its confound-verdict-with-ground would
   have recorded that exclusion as data.
3. flight-mode renders one real flight in the new shape: ghosts as typed
   holes, measurement as discharge with linked evidence, RET-descend on the
   field-read.
4. One round-trip: a flight record ingested into substrate-2, browsable from
   the human cockpit like an essay.
5. Joe flies one session reading the new instrument beside the old; the
   operator's verdict is the gate.

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
