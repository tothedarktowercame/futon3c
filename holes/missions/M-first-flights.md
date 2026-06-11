# M-first-flights — we have flight data (Orville); we need the data shapes (Wilbur)

Date: 2026-06-11
Status: **IDENTIFY** (minted 2026-06-11, evening of the essays-cockpit repair session).
MAP not yet begun; this document records the tension, the lineage claim, and the
boundary against M-substrate-metric before either fades.

Cross-reference:
- `futon5a/holes/missions/M-war-machine-wiring.md` — the bridge mission (WM head ↔
  live-geometric-stack manifold). This mission is about what the *trace* of that
  bridge should look like.
- `README-pilot.md` + `src/futon3c/dev/war_machine_pilot.clj`, `repl_trace.clj` —
  the machine-pilot apparatus whose records are the Orville-half evidence.
- `futon4/dev/mission-mode.el` (14 binder types, scope overlays, verify-gates,
  certificates) — the proof-theoretic anatomy the flight record should converge on.
- `futon4/dev/arxana-essays-twoup.el` + `arxana-window-constraints.el` — the
  invariant-backed operator cockpit (commits `481cd06`…`9985bdc`, all 2026-06-11).
- Memory `project_arxana_is_wm_for_joe` / `project_substrate_metric` — the
  two-cockpits insight and the thin-scalar bottleneck this mission must NOT re-scope.

## HEAD (one line)

**The flights are real and recorded; the records are lists of numbers and nulls —
reshape the data before attempting the longer flight.**

- *Jazz-head reading:* the generative kernel is the **Wright brothers' actual
  victory order**. Kitty Hawk (1903) was preceded by the wind tunnel (1901): when
  Lilienthal's lift tables failed them, they did not build a bigger engine — they
  **reshaped the data** and the controlled flight followed. Orville's 12 seconds
  proved the data existed; Wilbur's 59 seconds that same morning proved what the
  reshaped tables bought. Everything in this mission is an improvisation over
  "wind tunnel before engine."
- *AIF reading:* a flight record with anatomy is what lets the forward model
  LEARN from a flight. `prediction -4.9225 / realised -4.9225 / error 0.0000`
  in proposal-mode is a flight that taught the model nothing — satisfaction
  conditions for this mission are records that carry their derivations, so error
  has structure to propagate into.

## IDENTIFY — the tension

**Observed live, 2026-06-11** (Joe, reading `*flight-mode*` on the Stack HUD):

> "whereas the mission-mode buffers are approaching a functional programming
> (Scratch) or proof-theoretic (futon6) anatomy, the flight-mode buffer is just
> a list of numbers (and nulls)."

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

**The Orville-half (what we have):** flights happen and leave records. The loop
anatomy is present — field-read, velocity, gated act (`act+witness absent
(proposal-mode)` is WM-I4 as a *typed absence*, not prose), measurement,
self-record. The ghosts are typed holes: the record carries what didn't happen
*as slots*. This is real data from a real instrument.

**The Wilbur-half (what we lack):** the records are judgments without
derivations. The display asymmetry has a cause and a split:

- **Cause: display quality tracks the expected reader.** Mission-mode grew
  binders/scopes/certificates because a human pilot reads it and pushed back
  every time it flattened (the Skolem audit was that pressure). Flight-mode
  never had a reader who needed structure — the machine pilot holds structure
  in context; the buffer is residue. A human-facing surface no human was
  piloting from fell between the two seats.
- **Debt 1 — rendering debt** (structure exists, unshown): `215 ranked actions`
  is a *count* standing in for a ranked, typed list that exists as data. The
  `velocity` row — the ONE structured row, and not coincidentally the one that
  names a sorry — is the existence proof that the record wants to be a term.
- **Debt 2 — substrate debt** (structure doesn't exist to render): `-4.9225`
  is genuinely a bare scalar; no renderer can show the geometry of a number
  that has none. This is the thin-scalar bottleneck (WR-21 / war-bulletin-10)
  surfacing for the first time as a *felt instrument failure* rather than a
  theoretical one. **Boundary: the fix is M-substrate-metric's charter, not
  this mission's.**

**The lineage claim (Joe, 2026-06-11):** *"I have been piloting a hypertext-based
version of the War Machine since 2005 — Arxana was first created then — now with
agent support and invariant-backed fidelity."* The two cockpits are not parallel
inventions: the human seat is twenty-one years old. What 2026 added is (a) agent
embodiment inside the human cockpit (smart cursor, A4 visible authorship,
futon-look summons-as-consent) and (b) invariant-backed fidelity — the operator's
envelope written down and Reazon-gated (two-up pair, solo windows, back-restores,
one count vocabulary; the operator-side homolog of `repl.spec.edn`). The machine
seat (WM pilot REPL, 2026) is the *younger* sibling. Mount Analogue reading: the
ascents agree on the loop anatomy (read field → focus → gated write → trace);
this mission is about bringing the younger ascent's TRACE up to the older
ascent's representational standard.

**Why this is a mission, not a task:** the unknown is the specification — what
IS the term calculus for a flight record? (Candidate ingredients exist: BHK
arrows keyed by (have, want) endpoints; sorry = typed hole; mission-mode's 14
binder types; flight-as-derivation with ghosts-as-sorries and
measurement-as-discharge. Their composition is not yet designed.)

## Consequence if the shapes land

The piano roll changes type: from numeric tape (sequence of scalars and nulls)
to **proof script** (sequence of terms with open sorries). Flights compose the
way mission documents compose; the §5-style agreement between the two cockpits
becomes checkable (a correspondence between two term calculi) rather than
rhetorical; and the forward model gets structure for its error to propagate
into.

## Scope

**In:**
- The data shape for flight records: flight-as-derivation. Typed rows; ghosts
  as first-class sorries (type without term, context-fixed); measurement as a
  discharge judgment carrying its evidence; field-read carrying (or linking)
  its ranked list as bound, scoped entities.
- The render lane: flight-mode rows elaborated with mission-mode's anatomy
  (binder overlays, certificate coloring); RET on a row descends into its
  structure — the Arxana RET-focuses-the-item invariant applied to the machine
  cockpit's trace.
- Backfill compatibility: existing records (the Orville data) must remain
  readable — the new shape is an enrichment, not a migration that orphans the
  first flights.

**Out:**
- The ground metric / geometry behind the EFE scalar — **M-substrate-metric**.
  This mission stops at "the record carries the derivation the substrate can
  already produce"; it does not deepen the substrate.
- aif2 tension-proposer wiring (M-aif2 INSTANTIATE owns that).
- Any autonomy change at the consent gate. `act+witness absent (proposal-mode)`
  stays a typed absence; this mission makes the absence *renderable*, not
  fillable.

## Exit conditions

1. A written shape spec (EDN schema or equivalent) for flight-as-derivation,
   with the Capability Preservation Matrix discipline: every field of the
   current record maps in, nothing silently dropped.
2. A logic model BEFORE code (Reazon or core.logic per substrate): conforming
   witness record passes; per-invariant adversarial records caught — including
   "judgment without derivation" (today's records must be *representable* but
   *distinguishable*).
3. `*flight-mode*` renders one real flight in the new shape with at least:
   ghosts-as-sorries rendered as typed holes, measurement-as-discharge with
   linked evidence, RET-descend on field-read.
4. One round-trip: a flight record ingested into the Arxana/substrate-2 side
   (the human cockpit can browse a machine flight the way it browses an essay).
5. Joe flies one session reading the new instrument and the old side by side —
   the operator's verdict is the gate (mission-close is Joe's call).

## Next (MAP)

Locate the exact production sites of the current record (`repl_trace.clj`,
flight-mode renderer), the consumers (piano roll, VSATARCS evidence-kinds), and
the candidate calculus ingredients (BHK arrow store endpoint-identity, sorrys.edn
schema v2 `:kind` field, mission-mode binder taxonomy). Decide Codex handoff
boundaries per the coding-handoff protocol (shape spec + logic model stay with
the owner; bulk renderer code is bellable).

### Checkpoint 0 — 2026-06-11
**What was done:** Mission minted at IDENTIFY from the live flight-mode reading.
Tension recorded with verbatim evidence; two-debt split (rendering vs substrate)
established with the M-substrate-metric boundary explicit; 2005 lineage claim
recorded; exit conditions drafted.
**Test state:** n/a (no code in this mission yet).
**Next:** MAP per above.
