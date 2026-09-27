# JOIN-TERM-D — does the field that credits a "declared" edge carry the edge's term?

E-kimi-task-81 (kimi-8, 2026-09-27), for claude-9, PROOF-2a ⟨2⟩2b. DISCOVERY ONLY.
Companion data: `JOIN-TERM-D.edn` (one entry per edge).

**Inputs pinned.** Join output `wm-vs-equation-dag.edn` at futon3c `090658ae`
(10 declared / 12 hole / 3 code-path / 15 none). Map read at the sha the join
recorded, `775aaa6d` (`git show`, not the working tree — the working-tree map is
another lane's edit). Registry read at `202b5a02` (`git show`, same reason).
Code read at futon2 HEAD; no file was edited.

**Question.** The join classes an edge `:declared` when ANY map field runs from
a box of the source node to a box of the importer node (`:fields-by-var`). It
never checks that the field carries the edge's term (`:symbols`). Below, each of
the 10 declared edges: the crediting field(s), what the writer writes and the
reader reads at their sites, and a verdict — `:carries`, `:does-not-carry`
(false credit), `:cannot-tell`.

## Verdict table

| Edge | Term(s) | Crediting field (by-var) | Verdict |
|---|---|---|---|
| CTAU-TOKEN→R5 | C_τ | `[:want {:record :cascade-spec}]` assemble-one → rank-cascade-actions | **does-not-carry** |
| R14→R6 | τ | `[:beta {:record :precision}]` advance → selection-posterior / select-action-cascades | **carries** (under the row's own τ:=β identity) |
| R17→R6 | E | `:enactment-records` fold → select-action-cascades | **carries** (map under-names the carrier) |
| R2→R3a | o | `:tick-observation` observe → channel-prediction-error | **carries** |
| R3a→R7 | ε | `[:error {:record :prediction-error}]` compute-prediction-error → weighted-error | **carries** |
| R5→R6 | G | `:controller-score` rank-cascade-actions → select-action-cascades | **carries** |
| R6→R16 | π, Q(π) | `[:beta {:record :precision}]` select-action-cascades → selection-posterior (pass-through) | **does-not-carry** |
| R6→R17 | π | `:candidate` select-action-cascades → increment | **carries** (π's identity, not the policy record) |
| R6→R4 | π, r | `:precedence-violations`, `:descent`, `:units` containment-order → order-use | **carries** for r; π not on these fields (rides the receipt) |
| R7→R3 | Π | `:weighted-error` weighted-error → r3d-aggregate-driver | **carries** (Π as a factor; `:precision` sits beside it in the same record) |

So claude-9's two suspected false credits are confirmed, and they are the only
two: 8 of 10 declared edges carry their term, 2 do not. If ⟨2⟩2b's acceptance
counts `:declared`, the count of 10 proves "8 wired edges + 2 same-node-pair
field coincidences".

## Per-edge evidence

### 1. CTAU-TOKEN→R5, C_τ — FALSE CREDIT
Field `[:want {:record :cascade-spec}]`, writer `construction-assemble-one`
(`cascade_problems.clj:142` assemble-one), reader `r4-kernel`
(`efe.clj:1066` rank-cascade-actions).
- Writer: `:cascade-spec {:want (set want) :c-schedule schedule :lam … :mu …}`
  (cascade_problems.clj:165-168). The map field names ONLY the `:want` slot.
- Reader: `want (:want spec-in)` (efe.clj:1117) — the raw want-token set.
- The term: C_τ is the step-indexed preference DISTRIBUTION
  `e^{u_τ(o)}/Z_τ` with `u_τ` summed from per-token weights λ/|W|, μ, and the
  schedule's placement (registry :preference-schedule, pinned registry line
  754ff). It is computed INSIDE R5's scorer by `preference-member` /
  `utility-weights` under `horizon-g-sparse*` (cascade_model_manifest.clj:735,
  :767, :809). What crosses the wire is one ingredient (the want set), not the
  term. The registry row's own `:live-status :map` already says this:
  `:declared [[:want {:record :cascade-spec}]]`,
  `:not-declared [:c-schedule :lam :mu :preference-member :horizon-g-sparse*]`.
- Term-carrying alternative: the WHOLE `:cascade-spec` record (want +
  c-schedule + lam + mu) is written by assemble-one and read by
  rank-cascade-actions — declarable as a record-level field, and then ALL of
  C_τ's ingredients cross. C_τ itself never crosses any wire; it is
  reconstructed at the reader. Under a strict "the term's value flows" reading
  the edge is unwired at map level; under an "all ingredients flow" reading it
  is declarable in one map edit (record-grain field).

### 2. R14→R6, τ — carries (by the declared identity)
Field `[:beta {:record :precision}]`, writer `r14-precision-carry`
(`policy_precision_carry.clj:80` advance), readers `r14-selection-posterior`
(`cascade_selection.clj:54`) and `r9-selection-law` (`policy.clj:300`).
- Writer writes `:beta beta :gamma (/ 1.0 beta) :tau beta` with
  `:tau-source :carry-beta` (policy_precision_carry.clj:97-99): the record's
  `:tau` slot IS β by construction.
- The registry :precision-carry row's `:formal` is exactly
  `tau := beta, beta > 0; gamma := 1/tau` at this boundary.
- Reader consumes `(:beta …)` and refuses non-positive β
  (cascade_selection.clj:74-75).
- So the value that arrives under the `:beta` field is τ's value under the
  row's own identity. Note the map ALSO has the field
  `[:tau {:record :precision}]` written by the same box — and NO box reads it.
  The term flows under the β name or not at all. Verdict `:carries`, but this
  is exactly the case a naive symbol→field-name matcher would score wrong in
  both directions (credit `:beta` for τ, miss the unread `:tau` field).

### 3. R17→R6, E — carries
Crediting fields: `:enactment-records` (`r7-fold` → `r7-selection`) and a
spurious `[:beta {:record :precision}]` (`r9-selection-law` →
`r14-selection-posterior`, both boxes shared with R6's var list).
- `enactment_habit.clj:99` fold: the fold state is E's carrier — counted
  W_c-passing records under `:enactment-records` plus the cascade-prior counts
  (`prior/observe-policy`); `masses` (line 119) is E over a menu.
- `policy.clj:365-387` select-action-cascades: receives the fold state as
  `(:enactment-fold opts)` and reads `(:enactment-records fold-state)`; "E
  comes from the ENACTMENT FOLD" (docstring, M-wm-wiring step 8).
- The value on the wire is the whole fold state; the map names one key of it.
  E's carrier crosses. `:carries`. (The `:beta` field on this edge is a
  by-var artifact of box-sharing, not evidence about E.)

### 4. R2→R3a, o — carries
`:tick-observation` (`r2-tick-observe` = observation.clj `observe` →
`r3a-channel-prediction-error`). channel-prediction-error
(free_energy.clj:282) reads the observed channel value out of obs's envelope
(`channel-source-status`, `:value`) and takes the error against it. The
fourteen-channel observation vector IS this row's o at channel grain (registry
:observe :code, RC8.1). `:carries`.

### 5. R3a→R7, ε — carries
`[:error {:record :prediction-error}]` (`r3a-prediction-error` =
compute-prediction-error → `r7-weighted-error`). Writer:
`:error <observed − predicted-mean>` (free_energy.clj:215). Reader:
`err (:error error-map)` (precision.clj:228). The exact slot. `:carries`.

### 6. R5→R6, G — carries
`:controller-score` (`r4-kernel` = rank-cascade-actions → `r9-selection-law`).
Writer's own docstring: "Each ranked entry carries `:G-efe` (=
`:G-cascade` = `:controller-score`)" (efe.clj:1094-1095); the map aliases the
var to R5's node, which is why the field credits R5 though the box is row 4.
Reader: select-action-cascades selects on `:controller-score` as G
(policy.clj docstring, "cascade candidates carrying G in :controller-score";
selection-posterior takes it as `:g`). `:carries`.

### 7. R6→R16, π and Q(π) — FALSE CREDIT
Sole crediting field: `[:beta {:record :precision}]`, a PASS-THROUGH on
`r9-selection-law` (`:passes`, literal-arg-key :beta →
`selection-posterior` arg 1). β is neither π nor Q(π). Confirmed false.
- π alternative: selection-posterior's OTHER argument, `:candidates`
  (policy.clj:404 and :445: `{:beta beta :candidates stratum-candidates}`) —
  the policy set with per-policy `:habit :f :g`. That is π entering the
  posterior. It is NOT a declared map field (the reader box's `:reads` names
  only `[:beta {:record :precision}]`). Declarable: add a `:candidates`
  field write on r9-selection-law and read on r14-selection-posterior.
- Q(π): the posterior is COMPUTED at the reader (selection-posterior is an
  R16 var). Nothing needs to carry Q(π) into R16; the registry edge's Q(π)
  term is satisfied internally at the consumer, not wired. If the join wants
  to honour the edge, the honest declaration is π via `:candidates`.

### 8. R6→R17, π — carries (as identity)
`:candidate` (`r9-selection-law` → `r7-increment`). Writer: ":candidate is the
CHOSEN …" `(:cascade-id chosen-entry)` (policy.clj:527, :563). Reader:
increment's `policy-key-for` keys the E count off `(:candidate enactment)`
(enactment_habit.clj:35-52). The chosen policy's identity crosses; the policy
record itself does not, and increment needs only the identity. `:carries`
(with the grain note: π as a name, not as an action sequence).

### 9. R6→R4, π and r — carries for r; π rides the receipt
Fields `:precedence-violations`, `:descent`, `:units`
(`r4-constructor` = construction.clj `containment-order` → `r4-order-use` =
efe.clj `order-use`).
- Writer's docstring: `:descent [[above below] …] ; r itself` — the
  containment order, with `:units` its carrier set (construction.clj:124-128).
  The registry independently says ":r (the containment order) enters R4
  through the :co-application-kernel row". `:descent` carries r exactly.
- Reader actually consumes these as slots of `[:construction-receipt :order]`
  on the action (efe.clj:1035): the map's three fields are components of the
  record that rides the `:construction-receipt` wire — which the ledger has
  WITNESSED (`[:construction-construct :r4-order-use :construction-receipt]`,
  witnessed-hermetically). So the fields' values genuinely flow, inside the
  receipt.
- π: the policy reaches order-use as the action argument itself
  (`(:precedence action)`), not as any of the three crediting fields. On the
  map, `:construction-receipt` is a declared read of r4-order-use, so the π
  half is declarable/already declared at record grain; the three crediting
  fields alone do not carry it. Edge verdict `:carries` on r; π is a
  same-wire passenger that a field-grain check would miss.

### 10. R7→R3, Π — carries (as a factor; explicit key beside it)
`:weighted-error` (`r7-weighted-error` → `r3-aggregate-driver`).
Writer: `:weighted-error (* err adaptive-π)` where `adaptive-π` is
precision-for of the R7 state, and `:precision adaptive-π` is assoced onto the
SAME record (precision.clj:229-232). Reader's contract: "channel-id →
{:weighted-error <num> :precision <num> ...}" (belief.clj:1148) — it reads
both. The credited field is the product ε·Π, not Π; but the whole error-map is
what crosses, and Π is an explicit key of it. `:carries` (the map under-names
the wire at field grain again: declaring `:precision` as a field on this edge
would make it exact).

## Item 2 — the two false credits

- **CTAU-TOKEN→R5**: no field carries C_τ; the whole `:cascade-spec` record
  carries all its ingredients and is declarable in one map edit. Under an
  ingredients reading: declarable. Under a strict term reading: really
  unwired — C_τ is born inside R5's scorer.
- **R6→R16**: π is declarable (`:candidates`, one map edit — writer has it at
  policy.clj:445, reader destructures it). Q(π) needs no wire (produced at the
  consumer). Currently unwired at map level.

## Item 3 — a mechanical rule the join could apply

Add a declared **symbol→field correspondence** and require it for `:declared`:

- **Where it lives.** A separate table is the honest home — say
  `holes/labs/M-wm-wiring/wm-term-fields.edn`, keyed
  `{registry-symbol → #{map-field}}`, with provenance per entry (who declared
  the correspondence, when, at what code sha). Not the registry row (the
  registry is futon2's theory document; field names are map vocabulary, and
  codex lanes edit both on different rhythms), and not the map box (a box
  doesn't know which registry symbols its fields witness). The registry row
  may keep a pointer, as :preference-schedule already does informally in
  `:live-status :map`.
- **Rule.** Edge e with symbols S is `:declared` only if for every s ∈ S some
  crediting field f ∈ fields-by-var(e) has s ∈ correspondence(f). If S has a
  symbol with NO correspondence entry at all, the edge is `:cannot-tell`
  (gap in the table, loudly) — never silently credited and never silently
  cleared. If every symbol has a correspondence but no crediting field
  matches, `:does-not-carry` (a new, real class: wired node pair, unwired
  term).
- **Failure behaviour on a missing correspondence:** the edge drops out of the
  `:declared` count and names the uncovered symbol; the acceptance count can
  never again conflate "some field crosses" with "the term crosses".
- **Identities** (case 2, τ:=β) need an explicit entry
  `{:tau ← [:beta {:record :precision}] :via :precision-carry-formal}` —
  the table, not the matcher, owns identity declarations.

Sketch against the 10 edges (with a table filled from this document):

| Edge | Passes? | Why |
|---|---|---|
| R2→R3a (o), R3a→R7 (ε), R5→R6 (G), R17→R6 (E), R6→R17 (π), R7→R3 (Π) | pass | direct correspondence entries |
| R14→R6 (τ) | pass | via the declared τ:=β identity entry |
| R6→R4 (r, π) | r passes (`:descent`); π needs the record-grain `:construction-receipt` entry | mixed |
| CTAU-TOKEN→R5 | fails → `:does-not-carry` (until the record-grain `:cascade-spec` entry is declared) | the current false credit |
| R6→R16 | fails → `:does-not-carry` for π until `:candidates` is declared; Q(π) needs a "produced-at-consumer" convention or stays uncovered | the current false credit |

Result under the rule: 7 clean passes, 1 mixed, 2 exposed — matching §1.

## Item 4 — does the same weakness affect the wire ledger?

No, with one caveat. `wm-wire-ledger.edn` is per FIELD: each entry is
`[writer-box reader-box field]` plus a status and the test that witnessed it
(e.g. `[:construction-assemble-one :r4-kernel [:want {:record :cascade-spec}]]`
:witnessed-hermetically; the three `[:beta {:record :precision}]` wires
:unverified). The ledger's claim is "this field's value reaches that reader" —
a claim the false credits do NOT falsify: `[:want …]` really does reach
rank-cascade-actions. The term-carrying weakness lives one level up, in the
join's step from "field flows" to "edge declared". Caveat: the ledger is the
place a term-correspondence check would get its ground truth, and 24 of its
173 wires are :unverified — including all three `[:beta {:record :precision}]`
wires, two of which are exactly the fields crediting the R14→R6 pass and the
R6→R16 false credit. A term-level rule built on unverified field evidence
inherits that gap.

## Premise check (for the plan log)

The packet's premise — "the join never checks that a crediting field carries
the edge's term" — is correct, and both named false credits reproduce. Two
refinements the premise did not state: (a) the OTHER 8 declared edges do
carry their terms, several only because the wire value is a whole record/state
the map names one key of (E, Π, r) — a field-grain rule must allow
record-grain correspondence entries or it will manufacture NEW false
negatives; (b) R14→R6's credit is right only under the registry's own τ:=β
identity, which lives in prose, not in any machine-checkable place.
