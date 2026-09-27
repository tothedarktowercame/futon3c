# 2B-NONE-D — why each of the 16 unclassified ⟨2⟩2b edges is unclassified

kimi-8, 2026-09-27 (E-kimi-task-79). Discovery only: no map, registry,
prover, ledger, coverage or plan edit. Machine-readable twin:
`2B-NONE-D.edn` (one entry per edge, plus the packet table and findings).

Read at:

| what | revision |
|---|---|
| map | futon3c `2db7d11c` (`wm-flight-wiring.edn`, via the committed join output) |
| registry | futon2 `ff84adad` (repo HEAD `202b5a026` did not touch `aif-equations.edn`) |
| code | futon2 HEAD `202b5a026` — every line number below re-verified there today |
| join | futon3c `9204f6ed` (JOIN-2B-I): 40 edges, `{:declared 9 :hole 12 :code-path-note 3 :none 16}` |

Prior readings cited and checked against HEAD: WM-SUPERSET-D(.edn),
WM-NOPATH-RECONCILE-D(.edn), WM-PROVER-POSITIONAL-D (E1–E7), the
PROOF-2a-PLAN ⟨2⟩2b LOG (WM-NODE-R13-I, WM-NODE-R4-I), RC7–RC10 in the
registry's own `:code` fields. Where a prior reading drifted, that is said
below.

**Cause vocabulary** (the requisition's): **(a)** registry `:code` names no
var the map boxes — a pointer correction; **(b)** the map has the boxes but
no declared field between them; **(c)** a prover/join limit, the form named;
**(d)** no code path — belongs in `:holes` with a ⟨2⟩2d packet; **(e)** the
edge itself looks wrong in the registry.

**Headline:** none of the 16 is (d) — every one has a live code path at
HEAD. Two are (e)-flagged registry decisions (`:interp`'s placement), one is
a join-composition limit, and the other thirteen split between pointer
corrections (seven), prover forms (four), and missing map declarations
(two). Ten packets cover all sixteen; the biggest shared wins are
**P1** (name `judge`: three edges) and **P2** (the T carrier: three edges).

## The 16

### `[:R1 :R3]` μ — cause (a), packet P1
By-var `:boxed-no-field`; R1 boxes-by-var `[:r1-belief-carry]`, R3
`[:r3-aggregate-driver :r3-apply-belief-events]`, fields-by-var `[]`.
Path (E1, re-read): `judge` (war_machine.clj:7110) carries μ_t =
`reconcile-belief-carry` (:7222) through the morning-brief fold and hands it
positionally to `apply-arena-belief-events` (:7369). The map **already
declares** the hop: `r7-fold-call` (judge's box) `:passes :loop-belief` to
`r3-apply-belief-events`. Missing: `judge` is not joinable to R1 — R1's
`:code` cites war_machine.clj:6969-6974, a **line span that has drifted**
(at HEAD it sits inside `candidate-want-progress`, defn :6959). **Packet
P1:** name `judge` in R1's `:code`; registry-only, one word.

### `[:R3 :R1]` s-next — cause (a), packet P1
Mirror image (E2): `belief'` → judge's `:belief` → trace `:mu-post` →
`(:mu-post prev-trace-record)` inline at :7222 → `reconcile-belief-carry`
arg 2. The map already declares `:carried-mu-post` from `r7-fold-call`
(writes + `:passes` to `r1-belief-carry`). Missing: R3's `:code` cites
war_machine.clj:6147-6190, also **drifted** (now
`observation-label-view`/`-inputs` :6143/:6150; the inner step is judge
:7271-7369). **Packet P1:** name `judge` in R3's `:code`.

### `[:R1 :R3a]` μ — cause (c), packet P1+
E3: `judge` → `belief/predict-observation` (arg 1, positional,
war_machine.clj:7271) → `(get predictions ch)` (variable key) →
`channel-prediction-error` arg 3 (:7284). The map's existing `:passes
:channel-prediction {:element-of {:returns-of "belief/predict-observation"}}`
resolves to box `r3a-predict-observation` — an **intra-R3a** field, so
nothing R1-side is credited. The judge→predict-observation hop has no
`:passes`. **Packet P1+:** P1's pointer **plus** one `:passes` on
`r7-fold-call` to `{:call "belief/predict-observation" :arg 1 :callee-box
:r3a-predict-observation}`.

### `[:R1 :R4]` μ — cause (a), packet P5
The two-beliefs finding (WM-SUPERSET-D 3, RC8.2) is now half-fixed in the
registry: R1's `:code` names the token belief's `stage`
(token_belief_carry.clj:55-69). Live path: `joint-q0 =
(:continuation-belief token-belief-input)` (war_machine.clj:6605) →
`{:cascade-belief joint-q0}` (:6807) → `q0 = (:cascade-belief state)`
(efe.clj:1115). Missing: no box has `stage` as its var, and no R4 row names
`rank-cascade-actions` (box `r4-kernel`). The entity-belief half stays by
design (RC8.2). **Packet P5:** box `stage` writing
`[:continuation-belief {:record :token-belief-input}]`; `r9-decision`
reads it and writes `[:cascade-belief {:record :rank-opts}]`; `r4-kernel`
reads it; name `rank-cascade-actions` in an R4 row.

### `[:R13 :R4]`, `[:R13 :CTAU-TOKEN]`, `[:R13 :CTAU-CLASS]` T — cause (c)/(b), packet P2
R13 is `:unboxed` by var on all three. The live T carrier at HEAD:
sources `:horizon-steps` (**declared**: `r13-sources-horizon` →
`construction-assemble`) → `assemble-one`'s base-problem
(cascade_problems.clj:164) → `[:horizon-steps {:record :cascade-problem}]`
→ `r13-family-parameters` (**declared**) → `cascade-decision-admitted`'s
`{T :horizon-steps beta :beta}` destructuring (war_machine.clj:6499) → rank
opts `:horizon-steps T` (:6809) → `(:horizon-steps opts)` (efe.clj:1114),
and for CTAU-CLASS into `class-observation-model`'s per-tau
`:class-preference` (:6800-6806, :6329-6330; scorer
cascade_observation_scoring.clj:144-160).
Two prover limits, both already on the record: the **map destructuring**
`{T :horizon-steps}` is not treated as a read (JOIN-GRAIN-D, recorded on the
`r13-family-parameters` box by R13-I), and `policy-depth/configured`
(policy_depth.clj:13, the cascade-lane R13 step :5909-5915) returns a
**`select-keys`** call. Plus pointers: T's `:code` names only the
non-cascade sites (RC8.4 stands; its war_machine span :6283-6285 has drifted
into `resolve-cascade-horizon`'s computed-rule literal); the live sites
(`resolve-cascade-horizon` :6261, `policy-depth/configured`/`anticipation`,
`assemble-one`, `cascade-family-parameters` :6343 — the RC10 set) are
unnamed. **Packet P2:** R13 pointer + the destructuring prover form (or
`:passes`) + `r9-decision :writes+`/`r4-kernel :reads+` `[:horizon-steps
{:record :rank-opts}]`; for CTAU-CLASS additionally boxes on
`class-observation-model` (`:writes [[:class-preference {:record
:observation-model}]]`) and the class scorer.

### `[:CTAU-TOKEN :R5]` C-tau — cause (b), packet P3
Live: `assemble-one` writes `:c-schedule` onto the `:cascade-spec`
(cascade_problems.clj:152-153, 166-168 — the **same returned literal** the
map already attributes `[:want {:record :cascade-spec}]` from); consumers
are the per-problem lane's R5 step (war_machine.clj:5905-5935) and
`constructed-candidate-g` (:6161). The boxes exist; the map simply declares
no `[:c-schedule {:record :cascade-spec}]` field. Var-grain noise: the
join's var regex misses `(assemble-one:` (colon after the name).
**Packet P3:** declare that field from `construction-assemble-one` to the
R5 scorer box (`r5-g-sparse-cert` or a new `live_c/preference-schedule`
box — both vars are named in the C-tau row).

### `[:R16 :R2]` u, world — cause (c), packet P10
E7 re-confirmed: u flows `selection-posterior` → `bayes-choice` →
`select-action-cascades` (writes `:candidate`, box `r9-selection-law`) →
`enact-fn` reads `:candidate` (**declared** by file) — but the
`observe-publication-fn` closure (flight_runner.clj:702, called :881-885) is
handed `flight` and `click`, never the enactment, and asks the **store**:
`(fetch-run-record (:click-id click))` → `(:repair/publication record)`.
u reaches o through the published run record; that is the equation's own
shape (`:world` exogenous), not a missing prover hop. The by-var miss on the
u half is punctuation: the `:u` row's `(select-action-cascades:` is
colon-missed. **Packet P10:** re-punctuate, and record the store
round-trip as a checked attribution on `:observe` (RC8.2 CODE-PATH-NOTE
style); optionally a writer box for the run-record publication so
`[:repair/publication {:record :run-record}]` becomes a field.

### `[:R2 :R4]`, `[:R2 :R6]` interp — cause (e), packet P6
The registry's `:interp` note itself says placement at R2 is "a choice …
movable by editing this one field", and that the R6 placement **removes both
edges**. Live carrier at HEAD: interpretations are declared source data —
cascade_sources.clj:290 `(assoc-in [:interpretations t] …)` from the
document's `:interpretation-receipts`; `merge-published`
(want_interpretation.clj:452, box `ask-merge-published`, `:writes
[:interpretations]`) is the merge point; consumers `assemble-one`
(`:interpretations`, declared), `construction-construct`
(`:interpretations`, declared), and `containment-order`
(construction.clj:113-230, box `r4-constructor`, R6 by var, order "derived
from the interpretations' produces/consumes"). But want_interpretation.clj
is named by no R2 row, so fields-by-file is empty, and the note's own story
(seat answer via `agency-answer-fn`) is not the live carrier. Recorded, not
decided. **Packet P6:** the registry decision first; if R2 stands, name the
carrier vars in an R2 row — the fields are otherwise already declared.

### `[:R2 :R7]` o, ref-label — cause (b), packet P7
A stamped store round-trip (the second `:rates` row, added 2026-09-26):
`loaded-check` (observation_checks.clj:551-610) stamps verdicts → subjects
copy the stamp → `observation_label_reader` matches exactly → label store →
`read-rates-inputs` (war_machine.clj:6147) → `sourced-rates` call
(:5974-5975). WM-SUPERSET-D's "to confirm" resolves: the counts come from
the admitted stamped labels, not directly from C8 registry entries. Boxes
exist by file both sides; no box for the label-store write/read, and R2's
named vars are not the stamping var. **Packet P7:** boxes
`r2-loaded-check` (writes the stamped verdict) and the label-store read,
one field on the label record; name `loaded-check` in the `:o` row.

### `[:R3a :R3]` ε — cause (c), packet P9
Every hop is **already declared at var grain**: `[:error {:record
:prediction-error}]` `r3a-prediction-error` → `r7-weighted-error`, then
`:weighted-error` `r7-weighted-error` → `r3-aggregate-driver`. ε never
reaches R3 unweighted (WM-SUPERSET-D §4), so no single-field declaration is
truthful — the join has no composition. **Packet P9:** a join/registry form
for checked composition (accept the two-edge path through R7 when both
fields are declared, or an attribution note on `:mu-next` that ε enters as
`:weighted-error` — the RC8.2 `:enters-through` shape, kept off the Lean
hint key). No code or map change.

### `[:R4 :R5]` A, Q(o|π) — cause (c), packet P8
E6 re-confirmed: `push-forward` is passed **as a function value** to
`predictive-steps` (cascade_model_manifest.clj:834;
conditioned_trajectory.clj:200-212 conses `{:tau … :belief …}` steps); `q =
(:belief step)` (:901, :1001) goes positionally into
`outcome-risk-pointwise` (:909). WM-NODE-R4-I's record stands: "E6, R4→R5,
`:via-param`, the prover form that does not exist". **Packet P8:** the E6
prover form (function-valued `:passes` + element-of-sequence read), or a
`conditioned_trajectory/predictive-steps` box writing `[:belief {:record
:trajectory-step}]` plus naming the file in the Q-o-pi row — the box alone
leaves the function-value hop uncredited, so the prover form is the honest
fix.

### `[:R4 :R7]` A (ζ) — cause (a), packet P4
RC9's pointer landed in `:zeta`'s `:code` (`likelihood_precision/tempered-rates`,
likelihood_precision.clj:143-182, called at cascade_model_manifest.clj:830)
— yet the edge is `:none` because (i) the join's var regex misses
`(tempered-rates:` (colon after the name) on the R7 side, while the `:A-tok`
row's `(tempered-rates,` (comma) credits the **same var to R4** — node
assignment by punctuation; (ii) no box has `tempered-rates` as its var.
Also re-confirmed: the call is **guarded** (:829-830) — at the declared
ζ = 1 the rates pass through byte-identical, so the wire is live but the
identity. Whether that counts as realised is a ruling. **Packet P4:**
re-punctuate, box `tempered-rates` with the guard recorded; failing the
ruling, a `:holes` entry with this as its packet.

### `[:R7 :R4]` rates — cause (a), packet P4
E5 re-confirmed, and this is the cheapest edge of the 16: the field
`[:adjudication-rates r6-cascade-lane → r4-kernel]` is **already declared**,
`r6-cascade-lane` is R7 by var, and only `r4-kernel`'s membership in
R4-by-var is missing — because `:A-tok`'s `:code` names `token-likelihood`,
which is **not the live consumer** on the sparse path (E5: its callers are
the non-sparse `horizon-g`, retired `token-belief-at`, and the F_π path).
The live consumer is `rank-cascade-actions` → `horizon-g-sparse*`
(`(get rates v)` per token, :1030/:1053). **Packet P4:** one sentence in
`:A-tok`'s `:code` naming the live sparse-path consumer; the edge flips to
`:declared` with no map or code change.

## Packets (10 for 16 edges)

| packet | edges | kind |
|---|---|---|
| P1 name `judge` (+1 `:passes` for R3a) | R1→R3, R3→R1, R1→R3a | registry pointer |
| P2 T carrier | R13→R4, R13→CTAU-TOKEN, R13→CTAU-CLASS | pointer + prover form + map |
| P3 `:c-schedule` field | CTAU-TOKEN→R5 | map field |
| P4 rates pointers | R7→R4, R4→R7 | registry pointer (+box, +ruling) |
| P5 token-belief wire | R1→R4 | box + field + pointer |
| P6 interp siting | R2→R4, R2→R6 | registry decision first |
| P7 label-store wire | R2→R7 | boxes + field |
| P8 E6 function value | R4→R5 | prover form |
| P9 ε composition | R3a→R3 | join form or attribution |
| P10 u/world attribution | R16→R2 | pointer + checked attribution |

## Findings beyond the per-edge answers

1. **JOIN-GRAIN, quantified:** the join's var regex
   `#"\(([a-z][a-z0-9-]*[a-z0-9][!?]?)[,) ]"` misses a var followed by a
   colon. Four rows lose vars this way — `:zeta` (`tempered-rates`, which
   `:A-tok` simultaneously credits to R4 via a comma: the same var lands on
   different nodes by punctuation), `:C-tau` (`assemble-one`), `:C-class`
   (`class-observation-model`), `:u` (`select-action-cascades`).
   `horizon-g-sparse*` never matches (`*` is not `[!?]`). JOIN-GRAIN-D was
   referenced by R13-I but no document exists; this is the sharpest single
   instance of it.
2. **Drifted spans:** three war_machine.clj line spans in the registry no
   longer land where cited — R1's 6969-6974 (now `candidate-want-progress`;
the carry is `judge` :7222), R3's 6147-6190 (now `observation-label-view`;
the inner step is :7271-7369), R13's 6283-6285 (now
`resolve-cascade-horizon`'s literal). Line spans in `:code` rot; var names
do not. This is exactly why P1/P2 are pointer packets.
3. **Two `rank-cascade-actions` vars exist at HEAD**: efe.clj:1066 (token
   scorer, box `r4-kernel`) and cascade_observation_scoring.clj:135 (class
   scorer). The join's `box-by-var` is keyed on the bare var name; boxing
   the class scorer (P2) collides under the current keying.
4. **WM-SUPERSET-D's two "to confirm" items resolve at HEAD**:
   `assemble-one` sets `:horizon-steps` from its `horizon` argument
   (cascade_problems.clj:164, from `assemble`'s sources read — already
   map-declared); the R2→R7 counts come from the label store's admitted
   stamped labels (war_machine.clj:6147, :5972-5975), not directly from C8
   registry entries.
5. **Premise check on the requisition's leads:** all four held. `[R1 R3]`/
   `[R3 R1]` are indeed carried by `:passes` on judge's box with R1/R3
   citing spans (P1). The R13 rows have no box and RC10's set is the right
   one (P2). `[R4 R7]` has RC9's pointer and is `:none` for a punctuation
   reason plus a missing box (P4). The 2026-09-26 rows (`:C-tau`, `:C-class`,
   the second `:rates`) account for CTAU-TOKEN→R5, R13→CTAU-*, and R2→R7
   exactly as suspected.

Not done: no map, registry, prover, ledger, coverage or plan edit; no
generators run; nothing loaded into any JVM.
