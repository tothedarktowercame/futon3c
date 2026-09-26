# CTAU-D — the C_τ family the machine consumes, and what says so

claude-2, 2026-09-26, on claude-8's requisition (PROOF-2a-PLAN ⟨1⟩ addendum of 16:25Z, Joe via codex-2).
Read-only discovery. Nothing was run, no code or registry edited. Companion:
futon2 `holes/labs/wm-contract/PREFERENCE-AUDIT.md` (part 2).

Sources read this session: futon2 `7f94e48c4` (line numbers below re-checked at that sha for
`class-observation-model`, `resolve-cascade-horizon`, `flight-assembly-input`; other sites were read
at `e2d52965f`/`6b33b006c`, same files, tracked tree unchanged in them), futon3c `df65eac9`
(map `08482abe`, ledger `68bc4d92` = 163 wires, 5 verified / 73 hermetic / 85 unverified, coverage
`d58b30dc`), mathlib4 `afe69a946c`.

## 0. The requisition's premise needs one correction

The requisition (and the 16:25Z paragraph in PROOF-2a-PLAN) describes ONE family: `preference-member`
at each τ, consumed by `horizon-g`'s `outcome-risk`. At HEAD there are TWO C_τ families on the live
path, consumed by different scorers, and `horizon-g` (the dense function) is on neither:

| | token C_τ | class C_τ |
|---|---|---|
| built by | `cascade-model-manifest/preference-member` (`cascade_model_manifest.clj:767`), from the spec's `:want`/`:evidence`, λ, μ, `:c-schedule` | `war_machine/class-observation-model` (`war_machine.clj:6202`): `joe-c` literal `{:focused 55/100 :related 35/100 :unrelated 5/100 :stop-the-line 5/100}`; `class-pref` = `joe-c` at τ = T, `{:ending/not-yet-evaluated 1}` at τ < T |
| consumed by | `efe/rank-cascade-actions` (`efe.clj:1221`) → `horizon-g-sparse-cert` → `horizon-g-sparse*` (`cascade_model_manifest.clj:809`; `preference-member` at `:862`, `:960`) with `outcome-risk-pointwise` (`:755`) | `cascade_observation_scoring` (`:144-160`): with `(= :class-emission (:kind model))` it reads `(get-in model [:class-preference tau])` and says in a comment that classes are NOT routed through `preference-member` |
| reached from | the per-problem lane's R5 step (`war_machine.clj:5905-5935`: `base-opts` carries no `:observation-model`, so `efe/rank-actions` takes `rank-cascade-actions`), which is also the constructor's `:evaluate-g` (`constructed-candidate-g`, `:6065`) | the tick's joint decision `cascade-decision` (`war_machine.clj:6683-6698`): `(efe/rank-actions … {:observation-model class-model …})`; `efe.clj:1111` `(if (contains? opts :observation-model) …)` selects the observation scorer |
| domain | subsets of the problem's token universe (`problem-tokens facts want interpretations`) | 5 classes: focused, related, unrelated, stop-the-line, not-yet-evaluated |
| Lean | `PolicyHorizon.horizonEFE … (C : ℕ → O → ℝ)` (`PolicyHorizon.lean:61`), risk `stepRisk :50`, step-indexing `fixture_stepIndexed_preference :306`; the token C law is `TokenPreference.lean` | none found (no Lean declaration mentions the classes); the code comment says "stipulated and held fixed" (PROOF-wm-works ⟨2⟩2, `PROOF-wm-works-2026-09-22.md:314-317`) |
| what the tick does with the other | live-C weights are derived and recorded but "no longer enter the score" (`war_machine.clj:6221-6230`, `:6540-6551`) | — |

So the ordering AR-40 measured (H-VALUE-G-D, `92beed80d`) is the token family through the lane R5 /
constructor; the joint selection uses the class family. The registry row below is written for the token
family, as requested, and a second row (`:class-preference-schedule`) is proposed for the class family;
the wiring assumptions cannot be reviewed without both. `horizon-g` itself is not consumed by either
(only `horizon-g-sparse*`, which is the "aligned Clojure" of `horizonEFE` per its docstring; the
equivalence to `horizon-g` is a test claim in the WIRE-4 tests, not read here).

## 1(a). The producer chain, hop by hop

**Hop 1 — mission text → outcomes → cascade spec: NOT a wired hop.** The registry says so itself:
`aif-equations.edn` `:mission-preference` `:not-realised` (`:583`): "THE EDGE R19->R5 IS A THEORY EDGE
THE CODE DOES NOT CARRY. The C that enters risk today is built at cascade_problems.clj:150-174 from the
target's :want plus live-c/preference-scales and live-c/preference-schedule -- not from the extracted
outcomes." Confirmed at HEAD:
- `futon2.wm.extract-outcomes` (`scripts/futon2/wm/extract_outcomes.clj`) has one src consumer,
  `served_by_reading.clj:30,119` (the R2 read step: outcomes with cued spans and served-by links, a receipt);
  none reaches `cascade_problems`.
- The flight's `:want` comes from `flight/source-wants` (`flight.clj:29-60`): `:checkbox` = the sources'
  `[:wants target]` (`mission_hole_wants`, unchecked `- [ ]` tasks), or `:a-exits` = the mission's
  completion criteria (`futon2.aif.mission-criteria`); `war_machine/flight-assembly-input` (`:6039`)
  writes them as `[:sources :wants target]`. Declared sources (`resources/wm/cascade-sources/*.edn`) are
  hand-written `:want`.
- λ, μ: `live-c/preference-scales` (`cascade_problems.clj:150`; declared per target, else the default).
  `:evidence` is not set on the assembled problem's `:cascade-spec` (`:167`): only `:want`, `:c-schedule`,
  `:lam`, `:mu`, `:preference-scales`; `efe/rank-cascade-actions` fills `:evidence #{}`, `:lam`/`:mu`
  default 1 (`efe.clj:1130-1135`).

**Hop 2 — spec → schedule.** `:c-schedule` = `(or (get-in sources [:preference-schedules target]) (live-c/preference-schedule {}))`
(`cascade_problems.clj:153`). `live-c/preference-schedule` (`live_c.clj:324`): a declaration WITHOUT
`:c-schedule` gives `{:placement {:value :every-step :status :defaulted :reason :schedule-not-declared}}`; a
declared one must be exactly `{:placement :terminal, :elsewhere :uniform-over-non-ruled-zero}` or it throws.
`cascade_sources/load-declared` sets `[:preference-schedules t]` from each source file (`cascade_sources.clj:267,287`).
At HEAD `resources/wm/cascade-sources/` has 5 files: four declare `:terminal`
(M-aif-policy-conditioned-eig, M-expressions-of-interest, M-f11-find-production-successor, M-wm-08-external-f2),
`T-repair-occ-444fb018.edn` has none (defaulted `:every-step`).
- **What production passes as the placement is per target and follows the declaration.** A flight target
  with a declared source gets `:terminal`. A flight target the sources never declared (e.g.
  M-autoclock-in in the record below: its interpretations live under `data/wm-interpretations/`, and there
  is no `resources/wm/cascade-sources/M-autoclock-in.edn`) has no `[:preference-schedules t]` entry, so
  `assemble-one` passes the DEFAULT: `:every-step`, `:defaulted`. This is a reading of the code; the run
  record does not state it (the record has no `:c-schedule` key; `grep` for `:c-schedule`, `:c-form`,
  `:placement :terminal|:every-step` in `data/wm-runs/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn`
  finds only `:placement :next-selection`).
- Mixed schedules across a tick's problems: `live-c/family-schedule` (`live_c.clj:337`) throws
  `:incommensurable-family`; `mission_hole_wants/merge-into-sources` (`:130-156`) adopts one schedule only when
  the declared ones agree exactly (`one-of`), else generates NO checkbox targets. With 4 terminal and 1
  defaulted declared at HEAD, `one-of` returns nil by the code's own definition (`distinct` over both).
  Not run; that is a consequence I read, not observed.
- nil schedule inside `preference-member`: `placement` nil is in the accepted set `#{nil :every-step :terminal}`
  (`:774-777`), `uniform?` false, so the member is the SAME weights at every τ (the constant case).
  `:every-step` does the same. Only `:terminal` makes τ < T uniform (`:weights {}`) and τ = T weighted.

**Hop 3 — spec → the distribution at τ.** `preference-member spec universe horizon tau` returns
`{:universe … :zeroed … :weights …}` (log weights; `utility-weights` `:735`: `w_v = λ/|want|` for wants (or the
`:weights` map if given), plus μ for evidence, 0 elsewhere); `member-log-probability` turns it into ln c(o).
Tokens outside want/evidence weigh 0, so intermediates are NEUTRAL in the weights but still enlarge Z
(`log-preference-fn` docstring): C is uniform over them, it does not penalise them.

**Hop 4 — distribution → risk.** `horizon-g-sparse*` (`:809`) evaluates per τ: with zero adjudication rates
`outcome-risk-pointwise q (fn [o] (point-c tau o))` and the recorded `:c-distribution (get members tau)`; with
non-zero rates the factorised closed form (WIRE-4). `:infinite` when q has mass on a zeroed outcome.
`horizon-g` (`:632`, the dense form with `(c-fn tau)` and `outcome-risk` `:600`) is the Lean-aligned reference;
no live caller was found for it in `src`/`scripts` (grep of `horizon-g[^-]` callers: none besides the sparse core's docs).

## 1(b). The three indices (τ, click, occurrence)

| index | what it counts | where it comes from | relation to the other two |
|---|---|---|---|
| prediction time τ | steps of ONE click's rollout, τ = 1…T; T is one common horizon per tick | `resolve-cascade-horizon` (`war_machine.clj:6162`): a declared `:horizon-steps` wins (lifted from a source file by `load-declared`, `cascade_sources.clj:302`), else the largest number of admitted interpretations, at least 1. In the record below T = 4 while 7 interpretations were admitted, and 4 of the 8 wants came out `:beyond-horizon`/`:no-producer`; `T-repair-occ-444fb018.edn:31` declares `:horizon-steps 4`, which is consistent with that value but I did not trace it to the record | τ indexes a rollout step (one pattern application in the candidate's order), NOT a click and NOT a lifecycle phase |
| click | one call of the runner inside a flight | `flight/run!` (`flight.clj:466`): `max-clicks`, driver default 4 (`flight_driver.clj:191,261`), the spike passes 1; a click advancing no want ends the flight (`flight.clj:8-12`) | each click is a fresh tick: fresh q0, fresh problem, fresh rollout from τ = 1. C_τ has no memory of earlier clicks: carried state is the flight's `:carried-wants` (wants unreached), not a preference |
| lifecycle occurrence | a declared process occurrence (DERIVE, VERIFY, … and repeated VERIFY→DERIVE) | DERIVE-ARGUE-C-realization-2026-09-09 §Relations (`:76-90`): "C_tau is a family indexed by declared process occurrences, not a discounted sum over ticks. A future policy model must supply an explicit mapping from predicted times to these occurrences before consuming this family as C_tau." | **τ → occurrence mapping: ABSENT** (no code, no Lean, no registry field). click → occurrence: ABSENT (nothing stamps a click with an occurrence in C). |

Consequence stated once: the schedule's `:terminal` puts the weight at τ = T, the last step of ONE click's
predicted rollout. It is not "the end of the flight" and not "the end of the lifecycle".

## 1(c). What the ledger and the map say, hop by hop

Classes: **declared** (the map has a box or field), **implemented** (site exists at HEAD), **tested**
(a test reads it: the map's test boxes or a warranted namespace), **live-consumed** (a run record shows
the value used). Ledger statuses are from `wm-wire-ledger.edn` at `68bc4d92`; there is no ledger row for
anything not on the map.

| hop | site | declared on the map? | implemented | tested / ledger | live-consumed |
|---|---|---|---|---|---|
| mission text → outcomes (extractor) | `extract_outcomes.clj` | row 2 boxes; `:mission-preference` registry row `:lean-status :closed` (Lean carrier only, reading rule unformalised) | yes | R2 wires, not this hop | edge R19→R5 not carried (registry `:not-realised`) |
| wants/λ/μ/schedule → cascade spec | `cascade_problems/assemble-one` (`:construction-assemble-one`) | box exists; the map declares `[:want {:record :cascade-spec}]` (wire `[:construction-assemble-one :r4-kernel [:want {:record :cascade-spec}]]`, coverage `:witness`, ledger UNVERIFIED). `:c-schedule`, `:lam`, `:mu`, `:preference-scales` are NOT fields on the map (grep of the map: none) | yes | no ledger row for the schedule | the record does not carry the schedule (see 1(a)) |
| schedule → preference-member | `cascade_model_manifest/preference-member` | **no box** | yes | tests exist in futon2 (not read), none joined to a wire | not recorded per run; `horizon-g-sparse-cert`'s certificate carries `:c-distribution (get members tau)` per step (`:905-913`) — I did not open a certificate |
| member → risk → G | `horizon-g-sparse-cert` (`:r5-g-sparse-cert`) | box with NO `:reads`/`:writes` (map comment: `:g` not declared, `:keys` destructuring); `horizon-g-sparse*` unboxed | yes | via `:r4-kernel` wires: `[:r4-kernel :r9-selection-law :controller-score]` `:witness`, ledger UNVERIFIED; `[:r6-cascade-lane :r4-kernel :adjudication-rates]` `:witness`, UNVERIFIED | G values appear in run records (e.g. `:g-of-best 22.99…` below) |
| class family: relation → class → class model | `focus_receipt/classify-target` (`:r9-classify-target`), `class-observation-model` (`:r9-class-model`, reads `:target-class`) | yes | yes | `[:r9-classify-target :r9-decision :class]` **VERIFIED**; `[:r9-decision :r9-class-model :target-class]` **VERIFIED**; `[:r9-embedding-neighbour :r9-classify-target :derived-via]` **VERIFIED** (`:conditional`); the `55/35/5/5` literal is inside `class-observation-model` and is not a field | not checked (no class record read) |
| class model → observation scorer | `cascade_observation_scoring` | **no box** (map comment `:512` names it as an unboxed writer) | yes | — | — |

So a truthful `:live-status` for the token family today is: **implemented and consumed by the per-problem lane
and the constructor; declared on the map only at the two ends (`:want` into `:r4-kernel`, `:controller-score`
out); the schedule, λ, μ, `preference-member` and the certificate's `:c-distribution` are not on the map; no ledger
row is VERIFIED for any hop of it.** For the class family: **implemented; the relation → class → class-model hops
are VERIFIED wires; the scorer that consumes the class C and the 55/35/5/5 literal are not on the map.**

## 1(d). Real construction receipt (assumption 3)

`data/wm-runs/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn` (untracked, `sha256 a8e04fb97e58…`, 8
`:kind :machine-constructed` receipts, target `M-autoclock-in`). First receipt:
`:moves [{:move-id :compose-by-need :value 1.25 :g-comparison {:delta 1.25 …} :parts {:pragmatic 1.25 :epistemic nil :cost 0.0 …}}]`,
`:g-of-best {:value 22.99316913014941}` over a shared 8-token universe, `:horizon 4`, `:family-searched 2`,
`:budget-used 1`, `:stop-reason :no-admitted-move`, `:order {:units` 7 unit applications with `:descent` of 2 pairs`}`,
`:unreached-wants` 4 (one `:no-producer`, three `:beyond-horizon`). Second receipt: `:value 0.5`, 1 unit, 7 wants
`:no-producer`.
What a MOVE is on the production path (`interpretation_construction.clj:220-224`, `construct`): ONE move,
`:compose-by-need`, proposing the WHOLE minimal-support family (`(support input)`: every reachable plan's minimal
support set, complete or partial) at once, `:cost move-cost` (default 1, `war_machine.clj:6150ff` says cost 1 made the
constructor decline M-aif-eig's plan; the value in the receipt is cost 0.0). It is not a single-pattern extension.
The move is taken when its value (best-G(current) − best-G(proposed) + epistemic − cost) is > 0 over one shared universe
(`construction.clj:18-24`, stop `:acting-worth-more` when ≤ 0). The stop-reason `:no-admitted-move` printed in the
receipt is after taking the one move (`:moves` has it); I did not establish why the constructor stops with that
reason after a taken move.
**Baseline pin (AR-40, H-VALUE-G-D `92beed80d`)**: reproduced G(plan) vs G(empty): 4.0256/8.0256 (taken),
7.7982/10.7982 (taken), 15.3434/10.7982 (NOT taken), 13.4876/15.9876 (taken). Its cause (partition function over
different universes) was in the constructor's `:evaluate-g`; `constructed-candidate-g` now passes a common `:universe`
(`war_machine.clj:6065-6090`, "H-VALUE-G-D (2026-09-25)" in `efe.clj:1155-1160`). The placement used by those four rows
is whatever their sources gave (`h-value-g-d-repro.clj:118`); the packet does not state it.

## 1(e). Draft registry row (for CTAU-I after review; NOT a registry edit)

```clojure
{:id :preference-schedule
 :latex "\\begin{aligned}
         G(\\pi) &= \\sum_{\\tau=1}^{T}\\Bigl(D_{\\mathrm{KL}}\\bigl[Q(o_\\tau\\mid\\pi)\\,\\Vert\\,C_\\tau\\bigr]+\\mathbb{E}_{Q(s_\\tau\\mid\\pi)}H[A(\\cdot\\mid s)]\\Bigr) \\\\
         C_\\tau(o) &= \\frac{e^{u_\\tau(o)}}{Z_\\tau},\\quad u_\\tau(o)=\\sum_{v\\in o} w^{\\tau}_v \\\\
         w^{\\tau}_v &= \\begin{cases}
            \\lambda/|W| \\text{ (or } w_v\\text{) for } v\\in W,\\ +\\mu \\text{ for } v\\in E,\\ 0 \\text{ else} & \\text{placement } \\mathtt{every\\text{-}step},\\ \\text{or } \\mathtt{terminal} \\text{ at } \\tau=T\\\\
            0\\ \\text{for all } v \\text{ (uniform over non-ruled-zero outcomes)} & \\mathtt{terminal},\\ \\tau<T
            \\end{cases}
         \\end{aligned}"
 :defines :C-tau :node :R19 :class :stack-defined
 :imports [:cascade-spec :want :horizon]
 :formal "C_tau := the distribution over subsets o of the problem's token universe that risk is taken against at prediction step tau of ONE click's rollout at horizon T. Weights are additive per token: lam/|want| for a want (or its declared :weights entry), plus mu for an evidence token, zero for any other token; outcomes in :zeroed carry exactly zero mass. The schedule's :placement is :every-step (the same C at every tau; also the default when a source declares none) or :terminal (uniform over non-ruled-zero outcomes before tau = T, the weighted C at tau = T). tau is a rollout step, not a click and not a lifecycle occurrence; the tau -> occurrence mapping is absent."
 :lean "DarkTower.WarMachine.PolicyHorizon.horizonEFE" :lean-status :closed
 :lean-at "mathlib4 afe69a946c DarkTower/WarMachine/PolicyHorizon.lean:61 (C : Nat -> O -> Real, step-indexed; stepRisk :50); fixture_stepIndexed_preference :306 (the ranking reverses against a constant C); the token law is DarkTower/WarMachine/TokenPreference.lean"
 :lean-note "Binds the step-indexed C_tau binder and the risk term. It does not bind the schedule's :placement or :elsewhere, the per-token weights from want/evidence/lam/mu, or preference-member; those are the Clojure's (cascade_model_manifest.clj:735 utility-weights, :767 preference-member). The dense Clojure horizon-g (:632) is the one that mirrors horizonEFE with :c-fn; the live scorer is horizon-g-sparse* (:809), the factorised form."
 :code "futon2 (7f94e48c4) src/futon2/aif/cascade_model_manifest.clj:767 preference-member, :735 utility-weights, :809 horizon-g-sparse*, :755 outcome-risk-pointwise; src/futon2/aif/live_c.clj:324 preference-schedule, :337 family-schedule; src/futon2/aif/cascade_problems.clj:150-168 assemble-one (:c-schedule, :lam, :mu, :want on :cascade-spec); src/futon2/aif/cascade_sources.clj:267,287 (the declared schedule per target); src/futon2/aif/efe.clj:1130-1161,1221 rank-cascade-actions -> horizon-g-sparse-cert. Consumers: the per-problem lane's R5 (war_machine.clj:5905-5935) and the constructor's :evaluate-g (constructed-candidate-g :6065). NOT consumed by the joint selection, which scores class C (see :class-preference-schedule)."
 :live-status {:implemented true
               :consumed-by [:lane-r5 :constructor-evaluate-g]
               :map {:declared [:want {:record :cascade-spec}] :not-declared [:c-schedule :lam :mu :preference-member :horizon-g-sparse*]}
               :ledger {:verified [] :note "no wire of this family is VERIFIED at ledger 68bc4d92; the ends are :witness/UNVERIFIED"}
               :recorded-per-run {:absent :run-record-carries-no-c-schedule}}
 :open [{:assumption "the production placement"
         :state "per target, from the declared source: :terminal for the four targets in resources/wm/cascade-sources/ that declare it; :every-step (defaulted, :schedule-not-declared) for a target with no source entry, which is what a flight over M-autoclock-in gets by the code as read. The run record does not state the placement. Not established: which placement the flights actually ran under."}
        {:assumption "the ordering under measured A at a stated placement, with ties and controls"
         :state "a PLAN-2a 2b/3b test, not run. No claim of an ordering (chain, detour, idle) is made or measured here."}
        {:assumption "the constructor's proposal grain and strict-improvement rule"
         :state "one :compose-by-need move proposing the whole minimal-support family at once (single move, not per-pattern extension); taken iff value = G(best current) - G(best proposed) + epistemic - cost > 0 over one shared universe. Receipt: data/wm-runs/tick-run-record-2026-09-26-flight-7f89646a-click-1.edn (sha256 a8e04fb97e58…), first machine-constructed receipt :value 1.25, :horizon 4, 4 wants unreached."}
        {:assumption "the horizon relative to the proposed chain"
         :state "T is one common declared-or-computed horizon per tick (resolve-cascade-horizon); :max-moves (construction budget, default 4) is a different quantity that also reads 4 in the receipt above. In that receipt T = 4 with 7 unit applications in the proposed order and 3 wants :beyond-horizon; the chain is longer than the horizon it is scored over."}]}
```

Proposed second row, class family (not requested; needed so the figure does not show the selecting C as absent):
`:class-preference-schedule` — `:formal`: C over `{focused .55, related .35, unrelated .05, stop-the-line .05}` at
τ = T and unit mass on `:ending/not-yet-evaluated` at τ < T (risk exactly 0 before the horizon); `:lean` absent (typed:
`{:absent :no-declaration}`, the classes are "stipulated and held fixed"); `:code` `war_machine.clj:6202-6230`,
`cascade_observation_scoring.clj:144-160`, `observation_model.clj:195-232`; `:live-status` relation → class →
class-model wires VERIFIED, scorer and literal unmapped; provenance `:synthetic :calibrated false` in the model itself;
`:open`: the class weights are Joe's 2026-09-22 ruling; that their interpretation (corpus relation rows, not judged
"necessary" vs "irrelevant") matches what he means is not established (see PREFERENCE-AUDIT rows 2, 3).

## 1(f). What this note does NOT claim

- no ordering of chain, detour and idle under any placement;
- that flights ran under `:terminal`: by the code as read, an undeclared target runs `:every-step`;
- that `horizon-g` (dense) is live: it is not called from `src`/`scripts` by my grep;
- that the extractor's outcomes reach G: the registry and the code both say they do not;
- that class or token weights reflect Joe's preferences beyond the two rulings quoted in the audit.
