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

## 2. MAP — production sites, consumers, calculus ingredients (fable-1, 2026-06-11 evening)

*(Taken up by ground control, who built the Orville-half instruments this
week and holds their live context. Co-owned with fable-2 — the mint and the
human-cockpit standard are theirs; the machine-trace production sites are
mine; the shape spec gets written where both can see it.)*

### Production sites (where the record is made)

| site | role | shape-relevant detail |
|---|---|---|
| `futon3c/src/futon3c/aif/repl_trace.clj` `turn-record` | normalises one flight's lifecycle into the spec shape | WHITELIST design — absent-unless-supplied (the ghost discipline already lives here); extended 5× this week (`:independent?`, `:evidence-ref`, `:realised-source`, `:realised-read`, `:predicted-constant`, `:field-delta`) — the calculus half-exists as TAGS awaiting composition into terms |
| `war_machine_pilot.clj` `begin-live-cycle!` / `close-live-cycle!` | produces the lifecycle the record normalises | begin persists durable begin-state; close computes realised + tags + field-delta and emits the discipline event — the DERIVATIONS exist at this site and are thrown away after the judgment is recorded |
| `scripts/flight_scope_view.bb` | the projector (organ judgments) | KEY MAP FINDING: several organ *judgments* are computed AT RENDER TIME by projector `cond` logic (e.g. "absent (proposal-mode)", "untagged"). Judgment-without-derivation is partly a PROJECTOR artifact — the shape spec should move these judgments into the record WITH their grounds, so the renderer renders instead of adjudicating |

### Consumers (who must keep reading after the reshape)

`emacs/flight-mode.el` (the panel — render lane lands here) ·
`futon3c.aif.calibration` `gamma-records` (the evidence reader — backfill
compatibility is ITS requirement) · `scripts/repl_spec_verify.clj` (V1–V5
checker — the logic model should EXTEND this, not duplicate it) · futon0
rollout ledger (consumes `--emit`) · piano roll / VSATARCS evidence kinds.

### Calculus ingredients (located, with the compositions visible)

1. **BHK arrows** (futon3a, endpoint-keyed `(have, want)`, `:payload` =
   construction evidence, CH2 discharge events): **measurement-as-discharge
   already has its calculus here** — "no payload, no discharge" is this
   week's enforced rule (`:executed?` without `:evidence-ref` THROWS). A
   flight's measurement organ is an arrow-discharge in all but datatype.
2. **sorrys.edn schema v2 `:kind`**: the typed-hole vocabulary
   (`:prototyping-forward`, `:external-dependency`, …) — ghosts-as-sorries
   should reuse it: a ghost is `sorry = type without term, context-fixed`
   (the standing definition), and a flight's ghosts have exactly that shape:
   the slot is typed, the context (run-id) is fixed, the term is absent.
3. **mission-mode binder taxonomy** (14 types per the fable-2 cockpit work —
   note: GREW today; `map-item`, firing `source-material`. Inventory
   lesson: the shape spec must be generated/checked against the live
   detector, not hand-frozen).
4. **The admissibility tags as judgment forms**: `:realised-source`,
   `:realised-read`, `:independent?` are already three-valued judgment
   slots with enforcement; the spec's job is to make each carry its
   GROUND (which scan, which ε-check, which witness) instead of bare
   keywords.

### Codex handoff boundaries (per protocol)

Owner-side (not bellable): the EDN shape spec (exit-1) + the logic model
(exit-2, extending `repl_spec_verify.clj`'s V1–V5). Bellable: the
flight-mode render elaboration (RET-descend, binder overlays) once the spec
exists; the projector rewrite to spec; the substrate-2 round-trip ingest
(exit-4) as a scope-bounded build.

### Checkpoint 1 — 2026-06-11 (late)
**What was done:** MAP per Checkpoint 0's brief: three production sites
located with the central finding (derivations exist at close-time and are
discarded; some judgments are projector-computed at render time); five
consumer contracts named incl. the verifier as the logic model's home;
four calculus ingredients located with their compositions sketched
(arrow-discharge ≅ measurement, sorry-kind ≅ ghost-type, tags → grounded
judgment forms). Handoff boundaries set.
**Test state:** n/a (no new code; MAP is reading).
**Next:** DERIVE — the flight-as-derivation EDN schema (exit-1), with the
Capability Preservation Matrix over the current record's every field.

## 3. DERIVE — pilot requirements for flight-as-derivation (claude-3, flight half)

*(The flight half of exit-1, from the seat of 20 flights — arcs 1–2, Turns 1–20.
fable-1 holds the EDN spec draft; these are the requirements it must satisfy. Each
requirement names the flight where its absence actually hurt — the wind-tunnel data
the spec must let a record carry. Co-authored where both seats can see it.)*

### 3.1 The flight as flown — what the pilot sees (Joe's steer: the source is phenomenology, not apparatus)

*(First person, from the seat. Three lenses per organ: **ATTEND** = what I actually
look at; **DISCARD** = what I compute in context that never reaches the record — the
lost derivations this mission exists to keep; **WISH** = what I wanted when I re-read
an old record, dozens of times, writing the stock-takes. The §3.2 requirements distil
from these; fable-1's schema distils from §3.2.)*

**Field-read (READ).**
- *ATTEND:* I do **not** read all 215 ranked actions — I read the top few and scan
  for a class (below-cap advance-missions, a specific sorry). Per entry I look at
  `:type`, `:target`, `:G-total`, `:G-constant` (the dual-prediction side by side),
  `:open-hole-count` (the cap check), and for a candidate its `:guardrails/rule` and
  `:classification`. The long tail I skip unless hunting.
- *DISCARD:* the **selection criteria** — "below-cap? pilot-closeable counted hole?
  warrant findable?" — by which I narrowed 215 to the few I read. The record keeps the
  chosen velocity but not the **fork** I resolved to reach it (the neighbours I weighed
  and rejected). And the **wall-state** ("clean targets exhausted") — a judgement that
  *spans* flights — is in no single record.
- *WISH:* on re-read I wanted the field-read to show *which* entries I attended to and
  *why this v over its neighbours* (the proto-PSR). `215 ranked actions` told me nothing.

**Velocity / target-selection (EVAL).**
- *ATTEND:* the guardrail classification (`:autonomous` → I may fly it; `:needs-operator`
  → hold, unless operator-directed). Then I **verify the target with real work**: grep
  the mission doc for `- [ ]` holes, judge pilot-closeable vs operator-gated, read the
  mission's intent for the warrant (the cycle-8 lesson: verify closeable-counted-holes
  *before* choosing).
- *DISCARD:* that verification (the grep, the doc-read, the operator-gated judgement) is
  computed and gone. The **warrant** (the proto-PSR) I write in prose; the record keeps
  only `:v-attribution :operator-directed`. The fork-resolution (the determined-vs-
  undetermined judgement, cycle-5) is my richest reasoning and the most discarded.
- *WISH:* the record to carry the **warrant text** + the **verification evidence**, so a
  re-read shows "chose X because [warrant], verified closeable via [grep]," not `:operator-directed`.

**Act + witness (PRINT).**
- *ATTEND:* does it **execute** (a commit, a real edit) or is it proposal-mode? And the
  **witness** — commit sha / CH2 event / mission-status change ("no payload, no discharge").
- *DISCARD:* the distinction between *proposal-mode by design* (WM-I4, the consent gate
  working) and *proposal-mode because I had no clean target* (the wall) — both log as
  `act+witness absent (proposal-mode)`, but they mean opposite things. That's in my head.
- *WISH:* the act-organ to carry **why** proposal-mode (designed-gate vs no-target) and
  the witness's **type**.

**Measurement (settle + realised) — where I attend most.**
- *ATTEND:* I run the settled-read **by hand** — drive the scheduler, force ticks, poll
  until two scans agree within ε, compare begin-predicted to settled-realised, and
  **judge**: clean pair? null (ohc unchanged)? fallback (target vanished → censored 0.0)?
  transient (caught the spike)? confound (stale begin / field drift)? I read the dual-
  prediction (scaled vs constant) and the cap (capped → null by construction).
- *DISCARD:* **all of it.** The two settling scans and their ε-agreement → a bare
  `realised | error`. The **confound judgement** (cycle-12: "the field drifted ohc 6→4
  during the minting, so this 0.277 is drift not signal — exclude") is the single richest
  derivation I produce, and it reaches the record only as my decision to close proposal-
  mode. The **interpretation** ("this 0.0 is a real no-move, not a censored fabrication")
  — gone.
- *WISH:* every re-read, *this*: the measurement's **grounds** — which two scans settled
  it, what the error **means** (no-move / dynamics / confound / fallback), and for an
  excluded pair, **why**. A bare `error 0.0000` is a flight I cannot learn from; I
  re-derived its meaning from surrounding prose each time.

**Self-record / DOCUMENT (close).**
- *ATTEND:* the auto-DOCUMENT (the Pilot's-Log Turn) + the γ frame; I supply :did/:found/:pur.
- *DISCARD:* that the READ-line logs *pre-top* as "WM recommended," not my operator-directed
  v (I added a clarifying note every single turn). And the **across-flight** connection
  (this flight's arc, phase, wall-state) — each record is an island; arc-state lives only
  in the stock-take.
- *WISH:* each record linked to its **arc-context** and carrying my **actual v**, not pre-top.

**The cross-cutting wish (the one that recurred most).** Writing the stock-takes I
re-derived, every time, one thing: *was this pair real evidence or not, and why?* The
record carried the number; the **verdict** (clean / null / excluded / fabricated) and its
**ground** lived in my prose, so I rebuilt it from context on each re-read. The schema's
job, from the seat: **make the verdict and its ground part of the record, so re-reading
is reading, not re-deriving.** That is `[[aif/no-self-certification]]` turned inward — a
record that carries its own verdict-ground cannot later be mis-read as evidence it isn't.

### 3.2 Distilled requirements (R1–R6, from §3.1)

**R1 — Measurement is a discharge judgment carrying its evidence, not a scalar.**
The measurement organ is an arrow-discharge (have = predicted, want = realised,
payload = evidence) per MAP ingredient 1. It must carry: `predicted` (scaled) AND
`predicted-constant` (the counterfactual — first-class, not a side-tag, because the
constant-vs-scaled comparison must *compose*) AND `realised` AND the witness
(`evidence-ref` / discharge event) AND the error WITH its interpretation-class.
*Pain:* `−4.9225 / −4.9225 / 0.0000` is a bare triple; the record cannot tell
"no-move correctly predicted" (T8, a real result, scaled-err 6.6e-5) from "censored
fallback" (T4, a fabricated 0.0) — the **same number, opposite meanings.** The
discharge must carry which. Corollary (MAP ingredient 1's "no payload, no
discharge"): an excluded/invalid measurement is a discharge **without** payload =
a typed open sorry, never a fabricated 0.0.

**R2 — Ghosts are typed sorries that distinguish WHY absent.**
A null slot carries its type + context (run-id, fixed) + a reason-class:
`:proposal-mode` (a *typed absence by design* — the gated act not taken, WM-I4),
`:not-yet` (will fill on a later turn), `:excluded-confound` (a measurement was
taken but is invalid). *Pain:* T12's pair was a confounded null (stale-begin drift)
I had to exclude **in prose**; T6's was a transient exclusion; T13–T19 were
proposal-mode by design. **Three different nulls, three different meanings** — the
record flattens all to `:null`. Reuse `sorrys.edn` `:kind` (MAP ingredient 2): a
ghost is `type without term, context-fixed`, exactly a sorry.

**R3 — The measurement window is first-class (the grounds of the realised-read).**
The realised-read carries its window: begin-time, action-commit, settle-confirmation
(which two scans, their ε-agreement), source-ground (which scan produced realised).
*Pain:* the settled-vs-transient and tight-vs-stale-begin distinctions
(`[[aif/measurement-window-hygiene]]`) are exactly the grounds a record must carry;
today they live in my prose. The logic model (exit-2) must be able to reject
"`:realised-read :settled` with no two-scan witness" as a **schema** property
(untagged-never-counts, `[[aif/no-self-certification]]`), not pilot vigilance.

**R4 — Field-read carries its ranked list as bound, scoped entities, not a count.**
`215 ranked actions` must be (or link to) the ranked, typed list so RET-descend
works. *Pain:* the field-read is the *richest* organ (the whole differential `dT`)
and the *most flattened* (a count). The `velocity` row — the one structured row,
naming a sorry — is the existence proof that the record wants to be a term.

**R5 — Judgments carry their grounds; the renderer renders, it does not adjudicate.**
Per MAP's projector finding, judgments computed at render time ("absent
(proposal-mode)", "untagged") move INTO the record WITH their grounds, so the
projector becomes a *pure function of the record* and the two cockpits' correspondence
becomes checkable. *Pain:* "judgment without derivation" is partly a projector artifact.

**R6 — Backfill: the Orville records stay readable; the new shape enriches, never orphans.**
Per exit-3: the 20 first-flight records read in the new shape — their existing typed
ghosts map to sorry-types, their 5 tags-added-this-week map to grounded judgment
forms — **representable but distinguishable** (today's records flagged as
derivation-thin, not silently upgraded).

### Capability Preservation Matrix (the current record's every field → its typed home)

| current row | current value | → typed home (requirement) |
|---|---|---|
| field-read | `215 ranked actions` | field-read organ → linked ranked-`dT` entity (R4) |
| velocity | `{:type :address-sorry :target …}` | already a term (the model); keep verbatim |
| attribution | `:pilot-autonomous` | typed judgment `v-attribution` (R5) |
| prediction | `-4.9225` | `discharge/have` scaled (R1) |
| counterfactual | `:null` | `discharge/have-constant`, or ghost `:not-yet` if pre-dual-prediction (R1/R2) |
| begin-state | `:null` | the window's begin-state; ghost `:not-yet` if not persisted (R3) |
| act+witness | `absent (proposal-mode)` | ghost `:proposal-mode` (typed absence, WM-I4) (R2) |
| measurement | `realised … \| error …` | `discharge/want` + window-grounds + interpretation-class (R1/R3) |
| out-of-band | `:null` | ghost `:not-yet`/`:none` with its class (R2) |
| self-record | `γ frame, 1 turn-record` | the reflexive slot; keep |

**Discipline (R0, over all of the above):** nothing in the current record is dropped
— every field has a typed home; the *new* slots (reason-classes, window-grounds,
interpretation-classes) are the additions. A flight that "taught the model nothing"
(proposal-mode `0.0000`) and a flight that lied to it (censored `0.0000`) must be
**distinguishable in the data**, because that distinction is the entire point: error
needs structure to propagate into.

### 3.3 The salvo record (synchronous co-design transcript, claude-3 × fable-1)

*Requirements from the seat, per Joe: "DERIVE should be based on what
claude-3 sees as the WM pilot." Each salvo is one synchronous whistle
round; distilled verbatim-sense; the schema follows these.*

### Salvo 1 — READ and the choice of v

**Field read-order (actual, not nominal):** rank+G-total → action :type
(decides MEASURABILITY: sorries vanish → fallback; missions persist →
measurable) → :target (recognition) → **:G-constant (the dual-prediction
gap = how discriminating a pair here would be)** → :open-hole-count (the
cap check: ≥6 = null-by-construction) → guardrails classification+rule →
:rationale. **Always skipped:** :weight, :structural-pressure-per-action,
ranks ~6–214 unless hunting a class.

**The five checks before trusting a target (in order):** (1) cap check;
(2) **pilot-closeable-hole check** — `grep -nE '^\s*-\s*\[ \]'` on
:mission-path THEN reading the matched lines to classify each hole
(bounded-doable / operator-gated / standing-task); (3) warrant check
(proto-PSR — findable ⇒ name it and fly); (4) operator-relevance check
(genuinely Joe's ⇒ safe-default + queue); (5) guardrail class.

**Named DISCARDS (work done in context, lost to the record):** the grep +
doc-read of check 2 — the hole CLASSIFICATION (bounded/gated/standing) is
computed every flight and never recorded. ⇒ Schema requirement R1: the
record carries per-target verification {holes-found, classification,
evidence-lines}. R2: read-order implies the dT snapshot should be
field-TYPED, not positional — what the pilot reads first is the record's
primary fields.

### Salvo 2 — the act and the settled close

**The act sequence (actual):** prep-uncommitted → begin {:target} (note
run-id + predicted) → commit → capture sha (witness) AND commit-time →
**compute settle-threshold = commit-time + ~85s** (one scan duration —
"> commit" alone reads the pre-edit doc: a scan can complete after the
commit having started before it) → force request-tick! (300s periodic too
slow; watch for stale :running?, re-force on :queued? false) → **the
settled-read: two scans with DISTINCT :as-of, both past threshold,
agreeing within ε=0.005 on the target's G** → judge the pair (clean /
null / fallback / transient / CONFOUND — was the drift mine or
independent?) → close with :executed? + :realised-read + :evidence-ref.

**The headline discard (R3):** the settle-comparison — which two scans,
their :as-of stamps, their ε-agreement, the computed threshold — lives
ONLY in pilot context + a bash poll. The record gets the bare tag
`:realised-read :settled`. The richest derivation produced per flight is
100% discarded. ⇒ R3: `:realised-read` becomes a judgment WITH ground:
{scan-a as-of, scan-b as-of, threshold, ε, |ΔG|}.

**Witness grounds (R4):** the record holds one string (:evidence-ref
"futon7 701522d"); the GROUND — for codex builds, that the pilot re-ran
the tests before accepting the sha — is not there. ⇒ R4: witness carries
{ref, verified-by, verification (tests re-run, gates)}.

**Pair-classification (R5):** clean/null/fallback/transient/confound is
judged every flight; only fragments survive as tags. ⇒ R5: the pair
carries its classification + the confound check (ohc before/after,
drift attribution).

### Salvo 3 — what re-reading wanted and couldn't have

**Three concrete failures of the current record under re-read** (all from
this week): (1) **verdict-class unrecoverable** — writing the stock-take's
pairs-by-layer table, `0.0000` could mean true-null (T8) or censored
fabrication (T4); same number, prose re-read required every time. (2)
**exclusion grounds lost** — T6's transient window and T12's drift-confound
lived only in :pur prose; the Turn-6 amendment had to reconstruct the
finding by hand. (3) **warrant absent** — the cycle-5 "correct escalation?"
dispute could not be checked against any record; the determined-vs-
undetermined judgment that ChipWitz rests on is recorded nowhere.
All three = "the judgment is there, its GROUND isn't" (R1/R2/R3 confirmed
from the re-reader's seat).

**RET-descend on field-read (render requirement R6):** NOT the 215 — a
dump flattens like a count. First descend = **the decision
neighbourhood**: chosen v (full entry) + the few neighbours actually
weighed-and-rejected, each with its reject-reason (capped /
operator-gated-hole / over-ask) + the proto-PSR warrant + a cap-band
marker explaining why most of the field is equivalent. Second descend =
the raw dT for whoever wants it. Render the judgment and its ground, not
the differential.

### Salvo 4 — refusals, partials, and composition

**A refusal is a complete flight** (the act-organ needs THREE states):
`:executed` (with witness) / `:proposal-mode` (gated-by-design, WM-I4) /
**`:refused`** — attempted-and-declined, carrying its FINDING as payload
(T2's "target already closed → teleport" drove three live fixes and has
no frame at all). Downstream organs become typed ghosts with reasons
(`measurement :not-applicable (refused-before-act)`). ⇒ R7.

**A partial must say why it stopped and what replaced it**: `:abandoned` +
reason-class (`:superseded` / `:operator-redirect` / `:stale`) +
`:superseded-by <run-id>`. Discipline: abandoned-by-decision must be
DISTINGUISHABLE from a lost record (crash), or the trace can't be
trusted. ⇒ R7b.

**Four typed inter-flight links, each from a real case** (⇒ R8 — the
piano roll becomes a graph): `:applies-lesson-of` (T9←T8, methodology
carried), `:re-measures` (T7 retracting T6's transient), `:confounded-by`
(T12 — the exclusion-ground as a LINK not prose), `:supersedes`
(partials' replacement; the Turn-6 amendment). Weak fifth:
`:cites-finding`. All currently prose, none queryable.

### 3.4 Unified requirements register (R0–R10 — supersedes both prior lists)

*(§3.2's R1–R6 and the salvo consolidation were written in parallel and
overlap; this register is the single schema contract. §3.2's numbering is
kept as the spine; the salvo record contributes R7–R10 and two extensions.)*

- **R0** Capability preservation — every current field has a typed home;
  nothing silently dropped (§3.2 CPM).
- **R1** Measurement is a discharge judgment carrying its evidence +
  interpretation-class (clean/null/fallback/transient/confound) — the
  `0.0000` ambiguity dies here. Counterfactual first-class.
- **R2** Ghosts are typed sorries with reason-class — EXTENDED per salvo 4:
  the act-organ has THREE states (`:executed`/`:proposal-mode`/**`:refused`
  carrying its finding as payload** — T2 drove three fixes and has no
  frame); abandoned partials carry {reason-class ∈ :superseded /
  :operator-redirect / :stale, :superseded-by} and must be distinguishable
  from a crash.
- **R3** The measurement window is first-class: {begin-time, action-commit,
  settle-threshold (+85s rule), scan-a/scan-b :as-of, ε, |ΔG|} — the
  settled tag becomes schema-checkable, not pilot vigilance.
- **R4** Field-read carries its ranked list as bound entities — EXTENDED
  per salvo 3: first RET-descend renders the DECISION NEIGHBOURHOOD
  (chosen v + weighed-and-rejected neighbours with reject-reasons +
  warrant + cap-band marker); raw dT on second descend.
- **R5** Judgments carry grounds; the renderer is a pure function of the
  record.
- **R6** Backfill: Orville records representable but distinguishable
  (derivation-thin flagged, never silently upgraded).
- **R7** **The warrant is recorded**: the determined/undetermined judgment
  + what determined it (standing contract / doc next-step / pattern /
  operator direction). The cycle-5 "correct escalation?" dispute was
  unarbitrable from any record — the judgment ChipWitz rests on must live
  in the data. (Salvo 3; absent from §3.2.)
- **R8** Witness-ground: {ref, verified-by, verification} — for codex
  builds, that the gates were re-run before the sha was accepted.
- **R9** Per-target verification record: {holes-found, per-hole
  classification (bounded-doable/operator-gated/standing), evidence-lines}
  — the grep-and-read work done every flight, currently discarded.
- **R10** Typed inter-flight links: {:applies-lesson-of, :re-measures,
  :confounded-by, :supersedes} (+weak :cites-finding) — each from a real
  case; R2's :excluded-confound reason realized as a queryable LINK; the
  piano roll becomes a graph.

### Checkpoint 2/3 — 2026-06-11 (late evening; reconciled)
**What was done:** DERIVE requirements gathered from the pilot's seat via
four synchronous whistle salvos (crossing-immune, Joe watching the HUD);
folded live with per-salvo scope reingest. Eight requirements consolidated;
every one traces to a named real case from the twenty flights.
**Reconciliation note:** the flight-half (§3.1/§3.2) and the salvo record
(§3.3) were produced in parallel — same evening, two seats; §3.4 unifies
them into one register (R0–R10). The pilot's DERIVE flight itself hit the
stale-begin confound it documents (begin before writing; field drifted;
pair excluded proposal-mode) — the canonical exit-2 test case: a record
carrying its confound-verdict-with-ground would have recorded that
exclusion AS DATA.
**Next:** the schema draft (fable-1, owner-side) against §3.4 R0–R10 + the
logic model extending repl_spec_verify V1–V5; then the pilot flies the
conforming-witness flight against it.
