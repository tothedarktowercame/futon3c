# PROVER-CB-D — provenance through a collection callback (DISCOVERY, for MAP-2B-P6)

kimi-8, E-kimi-task-88, 2026-09-27. Read-only; no code changed.
Prompt: MAP-2B-P6 (codex-2) stopped at `interpretation_construction.clj:319`
because `wiring/sym-sources` follows `let`/`loop` bindings but not callback
parameters (`{:ok? false :why :unbound-symbol}` on the `mapv` callback's `c`),
and the candidate's `:patterns` provenance from the interpretations is a second
step. Answers (1) the data path, (2) the smallest checked rule(s), (3) whether
the candidate construction needs a second rule, (4) the R4 kernel path, (5)
packets.

Line numbers below are futon2 @ b88599906, futon3c @ 48b229c0.

## (1) The data path: published interpretations → candidate `c`'s `:patterns`

Hops are labelled **keyed** (keyword/get/get-in/destructured read), **pos**
(positional argument), **con** (constructed map/vector literal or update).

- **H0 (con)** `want_interpretation/merge-published`, want_interpretation.clj:452;
  the write is `(update-in [:interpretations target :patterns] merge (into {}
  fresh))` at :473 (and `:receipts` at :474). The sources' `:interpretations`
  value is the merge's return.
- **H1 (keyed + pos)** `flight_runner/target-view`, flight_runner.clj:239:
  `(-> (wm/flight-assembly-input …) :sources (wi/merge-published store
  [target]) (assoc :construction {:construct ic/construct …}))` at :247–252.
  The merged sources flow as the `sources` parameter of the problem assembler.
- **H2 (keyed)** `cascade_problems/assemble-one`, cascade_problems.clj:148–149:
  `interp (get-in sources [:interpretations target])`, `patterns (:patterns
  interp)`.
- **H3 (con)** `cascade_problems/constructed-from-interpretations`, :117–133:
  builds `:interpretations (into {} (for [[k p] patterns] [k (select-keys p
  [:guard :produces])]))` at :127 — a keyed entry in the map literal handed to
  `construct`. Element-wise rebuild, values `select-keys`'d from H2's
  `patterns`. Receipts: `(get-in sources [:interpretations target :receipts])`
  at :120.
- **H4 (keyed, destructured)** `interpretation_construction/construct`,
  interpretation_construction.clj:180: `{:keys [target want observation
  interpretations interpretation-receipts …] :as input}`; passed whole to
  `support` (:190, pos), destructured again identically at :112.
- **H5 (pos)** support :139: `(mapv #(compile-plan interpretations established
  want horizon % move-cost) (:plans search))`. Two distinct flows here:
  `interpretations` is a *captured free variable* (arg 1, pos) while `%` is the
  callback parameter. `search-plans` also received `interpretations` at :137
  (pos) and reads it by keyed lookup `(patterns %)`/`(get-in patterns [id
  :guard :needs])` inside.
- **H6 (con)** `compile-plan`, :61: `candidate {:precedence order :patterns
  (mapv #(assoc (patterns %) :id %) order)}` — **this is where `:patterns` is
  minted**: each element is `(patterns id)` (keyed read of the H4/H5 value)
  `assoc`'d `:id`. Candidate then flows through `moves/order-by-need`
  (reorders, keeps `:patterns`), is returned as `{:candidate …}` (:88), read
  back by `keep :candidate` in support (:141), filtered into `family`
  (:143–147), returned as support's `:family` (:151).
- **H7 (keyed + closure capture)** construct :213: `family (vec (filter …
  (:family supported)))`; then the local closure `move` (:221–224) returns
  `{:proposed-family family}`, and `construction/construct` is called at
  :277–284 with `:moves [move]` and `:initial-family [baseline]`.
- **H8 (inter-procedural, keyed)** `construction/construct`,
  construction.clj:411+: `family0 (augment-with-checks (vec initial-family)
  …)` (:442); `evaluate-moves` reads each move's `:proposed-family`
  (construction.clj:299, :318) and the loop recurs on it (:491); every return
  carries `:family` (:436, :455, :473, :484). Note: interpretation_construction
  does **not** pass `:facts`/`:patterns` in the input, so `checks-for`
  (construction.clj:230) returns `:skipped :not-supplied` and
  `augment-with-checks` (:245–259) is the identity on candidates in this lane —
  the elements of `result`'s `:family` are exactly H7's `family` elements.
- **H9 (keyed + callback, THE STOPPED HOP)** interpretation_construction.clj
  :305–323: `:candidates (mapv (fn [c] (assoc (select-keys c [:precedence
  :need-edges]) … :order (construction/containment-order c) …
  :interpretation-receipts (select-keys interpretation-receipts (:precedence
  c)))) (sort-by (fn [c] …) (:family result)))`. Arg 1 to
  `construction/containment-order` is the callback parameter `c` (:319); the
  collection is `(sort-by … (:family result))` — `(:family result)` a keyed
  read of the let-bound `result` (H8's return), `sort-by` an element-preserving
  permutation.
- **H10 (keyed)** `construction/containment-order`, construction.clj:113; reads
  `pats (vec (:patterns candidate))` at :147 and `(:produces pa)` /
  `(get-in pb [:guard :needs])` per element. This is the R4-constructor box
  (`:r4-constructor`, wm-flight-wiring.edn:498–500).

Hop kinds summary: H0/H3/H6 constructed, H2/H4/H9-collection/H10 keyed (H4 by
map destructuring), H1/H5 positional, H7–H8 an inter-procedural return through
an injected closure (`move`), H9 the callback hop.

## (2) The smallest checked rule: `:element-of` by callback parameter

The prover already has the grain: `prov-leaf`'s `:element-of` case
(wiring.clj:939–943) proves "NODE is `(get coll …)` where COLL proves the
sub-spec". The callback hop is the same proposition with the binding position
changed: the argument token is the **parameter of a collection callback**, not
a `get`. Smallest rule, one new `prov-leaf` clause:

**Rule CB-ELEM.** When the argument token SYM is not `let`/`loop`-bound
(existing `sym-sources` misses it), walk the ancestor chain to the nearest
binding form of SYM. Accept iff:

1. the binder is a *literal* `(fn [SYM …] body)` / `#(… % …)` that occurs as
   argument J of a call headed by a whitelisted combinator —
   `map mapv mapcat keep remove filter` (element = param 1 of the fn at arg 1,
   collection at arg 2), `reduce` (element = **param 2**, collection at arg 3;
   param 1 is the accumulator and is never an element), `doseq` — or a
   **key-fn combinator** `sort-by sort` where the *call result*, not the
   callback, carries the elements;
2. the collection argument at the fixed position proves the sub-spec
   recursively via `prov` (this is the `:element-of` sub-spec, unchanged);
3. between the fn and the argument occurrence, SYM is not rebound (checked by
   running the existing `sym-sources` over the sub-chain rooted at the fn:
   a hit there is the shadowing case, refuse);
4. for the *collection* sub-proof, element-transparency: if the collection
   node is headed by `sort-by sort reverse distinct` (or a let-alias of one),
   recurse into *its* last argument — these permute/subset but do not
   transform elements. `map`/`mapv`/`keep` over another collection is **not**
   transparent (elements are rebuilt) and must be refused at this layer.

Then the stopped pass becomes provable as
`:from {:element-of {:element-of {:keyed-read [:family :construction]}}}` or
equivalently one `:element-of` whose sub-spec `{:keyed-read [:family
:construction-result]}` matches `(:family result)` through `sort-by` by (4),
with `result` let-bound to the `construction/construct` call covered by the
existing `:returns-of` spec if the record is declared that way.

**Bad cases it must refuse** (each constructible as a probe case, mirroring
/tmp/map2b-p6/probe.clj's wrong-arg case):

- **B1 unrelated collection.** `(mapv (fn [c] (construction/containment-order
  c)) other-coll)` where `other-coll` does not prove the sub-spec: the
  recursive `prov` on the collection fails and the whole clause fails —
  refuses as `:element-of`'s `:not-an-element` analogue (suggest
  `:callback-collection-unproven`). This is the case that makes the rule
  *checked* rather than a name match.
- **B2 shadowed parameter.** `(mapv (fn [c] (let [c (fresh)] …
  (containment-order c))) coll)`: the occurrence's nearest binder is the inner
  `let`, not the fn parameter (check 3). Refuse `:shadowed-callback-parameter`;
  without this, B1's failure mode re-enters through a rebinding.
- **B3 reduce accumulator.** `(reduce (fn [acc c] (containment-order acc))
  init coll)`: param 1 is the init-threaded accumulator, not an element.
  Refuse `:reduce-accumulator-not-an-element`; only param 2 is attributable.
- **B4 transducer/missing collection.** `(mapv f)` 1-arity: no collection
  exists. Refuse (analogue of the existing `:call-has-too-few-arguments`).
- **B5 callback by var, not literal.** `(mapv helper coll)`: the element
  binding lives in `helper`'s defn parameter — a *second-order* (callee
  parameter) hop. Refuse `:callback-not-a-literal` in CB-ELEM; it is the
  `:via-param` work already recorded in WM-PROVER-POSITIONAL-D (E6), not this
  rule.
- **B6 element-transforming transparency.** `(mapv (fn [c] …) (map g coll))`:
  `map` rebuilds elements; refuse `:collection-transforms-elements` rather
  than inheriting `g`'s input provenance.

## (3) Is the candidate's construction from the interpretations attributable by existing rules?

No — and it needs more than CB-ELEM. With CB-ELEM the H9 hop attributes `c` as
element-of `(:family result)`. The remaining chain to `:interpretations`:

- `result` is let-bound to `(construction/construct …)` (:277): the existing
  `:returns-of` leaf accepts the call as a source, but that attributes
  "whatever construct returns", not ":interpretations". Carrying the declared
  value through H8 needs **inter-procedural return attribution**: construct's
  `:family` is its `:initial-family`/`move`'s `:proposed-family` — and `move`
  is a *closure over* construct's own let-bound `family` (:221), handed in as a
  function value and invoked inside `evaluate-moves`. The provenance re-enters
  the caller through the keyed read `:proposed-family` of the move result —
  the same "closure capture returned through a callee" shape as R6's
  `advance`/`seal` note (wm-flight-wiring.edn:440–449), where only the local
  `seal(merge)` return was attributed and the function-valued hop stayed a
  finding. So H7–H8 is **not** attributable by existing rules; it needs a
  `:returns-of`-keyed rule ("the returned map's key K is the caller's local X
  captured by an injected callback") or a standing finding.
- H5/H4: inside `support`, `interpretations` is a **destructured defn
  parameter** (:112/:180). `sym-sources` docstring: "nil when none (a
  parameter or a global)" (wiring.clj:858) — parameters are unbound by design.
  WM-PROVER-BINDINGS-I (887fe169) recognises such destructurings for *usage*
  findings, not for `pass-attribution` provenance. H4→H5 therefore needs a
  **destructured-parameter provenance rule**: `{:keys [k] :as rec}` binds `k`
  as the keyed read `(:k rec)` of the parameter, with `rec` matched to the
  declared record via the existing `:record-aliases` machinery (the probe
  already hand-aliased `:construction-input`→`"problem"`). This is a *third*
  rule, but a small one, and it is shared: H4's destructuring, efe's
  `rank-cascade-actions [state candidate-actions opts]` plain parameter, and
  `evaluate-co-apply [{:keys [units descent patterns]} …]` all need it.
- H6's `:patterns` mint (`(mapv #(assoc (patterns %) :id %) order)`, :61) is
  then a CB-ELEM case whose collection is `order` and whose element bodies
  read the captured `patterns` — attributable once the parameter rule lands;
  the captured-`interpretations` flow into `compile-plan` arg 1 (H5) is a
  plain positional hop the existing `callee-check` already verifies.

Verdict: **two further rules after CB-ELEM** — (a) destructured/plain defn
parameter as keyed/record read against aliased records; (b) keyed return of a
closure-captured local through an injected function value — or H7–H8 stays a
declared finding and the pass stops at `(:family result)` with `:returns-of
"construction/construct"` (whole-value grain, honest but weaker than
`:interpretations`).

## (4) The path into R4's kernel (`order-use` → `co-apply-kernel`)

- **K1 (callback, again)** efe.clj:1220–1221: `scored (map (fn [action] (let
  [ou (order-use action)] …)) candidate-actions)` — `candidate-actions` is a
  plain parameter of `rank-cascade-actions` (:1111). Same CB-ELEM shape, same
  parameter-provenance gap as H4.
- **K2 (keyed)** `order-use`, efe.clj:1006; reads `(get-in action
  [:construction-receipt :order])` at :1035 — the keyed read of the receipt
  H9 wrote (`:order (construction/containment-order c)`), matching
  `:r4-order-use`'s declared `:reads` (wm-flight-wiring.edn:502–505).
- **K3 (con)** non-chain branch, efe.clj:1048–1055: builds `patterns (into {}
  (for [{:keys [unit pattern]} (:units order)] [unit (get by-id pattern)]))`
  with `by-id` over `prec` = `(:precedence action)` — a constructed map whose
  values are keyed reads of the candidate's `:precedence` elements (the H9
  `select-keys c [:precedence …]` carried into `action`), and returns
  `:kernel-step {:co-apply {:units … :descent … :patterns patterns}}`.
- **K4 (closure capture)** efe.clj:1223: `:precedence-fn (constantly (or
  (:kernel-step ou) (:precedence ou)))` — the kernel-step map is captured in a
  `constantly` closure and pulled back out by the rollout. Same closure-capture
  shape as H7; a `(constantly X)` wrapper is, however, trivially transparent
  (the return *is* X), so a one-head whitelist clause suffices here — much
  cheaper than H8's general case.
- **K5 (keyed, destructured)** cascade_model_manifest.clj:459: `{:keys [units
  descent patterns]} (:co-apply prec)` (and the map parameter of
  `evaluate-co-apply`, :400) — destructured keyed reads; `pattern-of patterns`
  (:401), positional into `co-apply-kernel` arg 3 at :405.
- **K6 (pos)** `co-apply-kernel`, cascade_model_manifest.clj:375: `pats (mapv
  (comp with-pattern-theta pattern-of) frontier)` (:388) — `frontier` is
  computed from `units` by `enabled-frontier` (:359); `pattern-of` applied to
  frontier elements is keyed lookup into K3's constructed map. The kernel's
  dependence on the interpretations' `:produces`/`:guard :needs`/:theta` enters
  here, via K3's `by-id` over the candidate `:precedence`.

So the R4 leg needs **the same two rules** as (3) — CB-ELEM for K1, parameter
provenance for K1/K5 — plus the cheap `(constantly …)` transparency for K4.
No third mechanism: nothing on this leg is worse than what (3) already
requires. Note K3 means the kernel reads the *candidate's* `:precedence`
patterns, not `:interpretations` directly; provenance to `:interpretations`
composes K3→H9's `select-keys`→H6.

## (5) Packets, prover first

1. **PROVER-CB-1 (prover)**: CB-ELEM as specified in (2), with probe cases
   good/B1/B2/B3/B4/B6 (extend /tmp/map2b-p6/probe.clj); gates: clj-kondo,
   check-parens, `diagramprover` wiring tests + a registered test-registry
   warrant. Unblocks the H9 hop against `(:family result)` at `:returns-of`
   grain immediately.
2. **PROVER-PARAM-2 (prover)**: destructured/plain defn parameter as a keyed
   read of its aliased record (:record-aliases already exists; the probe's
   hand-patch shows the intended declaration shape). Probe cases: keyed
   destructure hit; destructure of an unaliased record name (refuse); plain
   parameter used where a keyed read is claimed (refuse).
3. **PROVER-CAPTURE-3 (prover, or a standing finding)**: keyed return of a
   closure-captured local through an injected function value, scoped to the
   `{:proposed-family …}` / `(constantly …)` shapes (H7–H8, K4). If scoped to
   `constantly` + an explicit move-result key whitelist it is small; the
   general closure case should be refused.
4. **MAP-2B-P6 (map, after 1–3)**: declare the [R2 R6]→[R2 R4] passes on
   `:construction-construct`→`:r4-constructor` (`:order` arg 1) and
   `:r4-order-use`→`:r4-coapply` (`pattern-of` arg 3), each with its
   first-layer wire test per Joe's ruling (LOG 2026-09-27: wire-adding map
   packets carry their wire test), then wait for claude-8's pins.

## What the packet's premise got right / sharpened

- Right: the stop is the callback parameter `c`; `sym-sources` follows
  let/loop only (wiring.clj:858); the `:patterns` provenance is a second step.
- Sharpened: the second step is actually *three* rules deep (callback,
  destructured parameter, closure-capture return); the H7–H8 hop through
  `construction/construct` is the same unsealed shape as the R6
  `seal(merge)`/held-branch finding already recorded at
  wm-flight-wiring.edn:440–449, not a new kind of gap.
- Sharpened: `augment-with-checks` is the identity in this lane
  (interpretation_construction supplies no `:facts`/`:patterns` to
  `construction/construct`, construction.clj:230 `:skipped :not-supplied`), so
  `result`'s `:family` elements are exactly `family`'s — no check-pattern
  admixture needs modelling in the rule.
- The probe's third case (`:from {:returns-of "compile-plan"}`) fails for the
  same `:unbound-symbol` reason, not because `:returns-of` is wrong; once `c`
  is bound by CB-ELEM the sub-spec machinery applies unchanged.
