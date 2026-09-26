# WM-PROVER-POSITIONAL-D — the seven positional hops, read at their sites

claude-2, 2026-09-26. Discovery, read-only: no code, no prover edit, no map edit.

Read: futon2 at `3f7a5c85` (tracked tree clean), the seven forms of
`WM-SUPERSET-D.edn` (67db43d4), and `src/futon3c/diagramprover/wiring.clj` at
`29df1244` (`attributed-records`, `field-usage*`, `owner-map`, `return-maps`).
Nothing here was run through the prover; every claim about what the prover
does is from its source. Line numbers are futon2's at `3f7a5c85`.

## 0. The finding

In none of the seven is the value a field of a record that the callee then
binds to a key. Two things are true instead.

1. **The passed value is the whole argument, and it is unkeyed.** `belief` (μ)
   is a map keyed by entity id, `observation` (o) a map keyed by channel,
   `rates` a map keyed by token, `q` a map keyed by token-state. The callee
   forwards it to another callee, or reads it with a *variable* key
   (`(get obs channel)`, `(get rates v)`, `(get carried-mu-post eid prior)`).
   There is no literal keyword for the prover to find at either end. So a rule
   of the form "the nth argument is the local bound from `(:f r)`, and the
   callee's nth parameter is read as `(:f r)`" cannot hold as stated: the
   callee never reads `(:f r)`.
2. **Where a keyword does exist, it sits on the carrier around the hop**, not
   on the hop: `:belief` on the morning-brief fold's returned map, `:mu-post`
   on the trace record, `:belief` on the trajectory step, `:rates` in the map
   passed to the scorer. The prover already handles most of those (keyword
   call on a receiver, returned literal, destructure). What it lacks is the
   join between a keyed carrier and the unkeyed local that carries the value
   between two calls.

So the smallest truthful rule is not "attribute a field through a positional
argument". It is: **a `:passes` entry is evidence of occurrence, checked at both
ends**, letting the map name an unkeyed value as a port and the prover verify
that the call site really hands that value to that parameter and that the
parameter is really used (§2).

## 1. The seven, at their sites

Notation: arg positions are 1-based, as in WM-SUPERSET-D. "Immediate" means the
callee binds or destructures the parameter in its own parameter list or first
form.

### E1 R1 → R3 (μ)

All in `war_machine.clj`, function `judge` (`:6865`).

| hop | what is passed | from | to | callee end |
|---|---|---|---|---|
| a | μ_t, the return of `belief/reconcile-belief-carry` | the call, in the `if` inside the init of local `wm-belief-pre` (`:6972-6978`) | (returned; then arg 1 of `apply-morning-brief-events`, `:6983`) | `apply-morning-brief-events [belief-state consumed-ids unseen-events]` (`:845`): `belief-state` used in `(contains? belief-state (:entity-id %))` and forwarded to `apply-arena-belief-events`. **Not** destructured. |
| b | the same value, now under key `:belief` | `apply-morning-brief-events` returns a literal `{:belief … :applied … :held … :consumed-ids …}` (`:856`) | `(:belief morning-brief-fold)` at `:6989`, into local `wm-belief-after-brief`, which is the init of the inner loop's `belief` | **keyed both ends** (returned literal, keyword call on a receiver that is the record) |
| c | loop local `belief` | init `wm-belief-after-brief` (keyed read); recur `belief'` (`:7113-7115`, the return of the call below or `belief` unchanged) | arg 1 of `apply-arena-belief-events` (`:7114`) | `apply-arena-belief-events [belief-state events]` (`:837`): `belief-state` only forwarded, arg 1 of `belief/update-belief-batch` (`:434`, `[belief events opts]`), which forwards to `reduce … update-belief`. **Not** destructured; entity-id keys are read inside with a variable key. |

The hop-c local has two sources. On step 0 it is the keyed read; on every later
step it is `belief'`, the return of this very call. So "the nth argument is the
local bound from the keyed read" is true for the first micro-step only, and
false for the rest, which are R3's own output feeding R3 again. A rule that
checks provenance has to say so and not pass silently.

### E2 R3 → R1 (s_next)

| hop | what is passed | from | to | callee end |
|---|---|---|---|---|
| a | belief′, the return of `apply-arena-belief-events` | let-local `belief'` in the loop (`:7113`) | stored under `:belief` in the loop's terminal literal (`:7151`), destructured `{:keys [belief …]}` in the `let` that binds the loop's result (`:7018`) | keyed both ends, but the writer is a literal in return position **of a `loop`**, which `return-maps` does not enter (it handles `let`, `if`, `cond`, `case`, `when`, `do`, `->`; not `loop`) |
| b | `wm-belief` = `belief` (`:7166`) | judge's returned literal, `:belief` | `trace/trace-record` reads it, writes `:mu-post` (WM-SUPERSET-D hop 2, 3) | keyed |
| c | `(:mu-post prev-trace-record)` | the **argument expression itself** is a keyword call on the trace record (`:6976`) | arg 2 of `belief/reconcile-belief-carry` (`:510`, `[fresh-bootstrap carried-mu-post]`) | `carried-mu-post` used in `(seq carried-mu-post)` and `(get carried-mu-post eid prior)`. **Not** destructured. |

Hop c is the clean case: the argument is a keyed read written inline. The
prover already attributes that occurrence of `:mu-post` (a keyword call whose
receiver is the record, given an alias for `prev-trace-record`). What is
missing is only the callee end.

### E3 R1 → R3a (μ → g(μ))

| hop | what is passed | from | to | callee end |
|---|---|---|---|---|
| a | loop local `belief` (as E1-c) | `:7024` | arg 1 of `belief/predict-observation` (`belief.clj:1199`, three arities; the 3-arity `[belief entity-tags context]` is the one called) | forwarded to the 2-arity, then the 1-arity, then `predict-annotation-health belief` etc. Not destructured. |
| b | `predictions`, the return of `predict-observation` | let-local `predictions` (`:7024`) | `(get predictions ch)` is arg 3 of `fe/channel-prediction-error` (`:7037`) | `channel-prediction-error [obs channel prediction opts]` (`free_energy.clj:280`) forwards `prediction` to `compute-prediction-error [observed prediction opts]`, which calls `(prediction-member prediction :mean)` and `… :variance` (`:191-200`), a helper doing `(get prediction k)` |

The arg-3 expression `(get predictions ch)` has a **variable** key (`ch` ranges
over `belief/channels-with-likelihood`). It is an element of `predictions`, not
a field of a record. The `:mean` and `:variance` fields are then read through a
helper as a **keyword passed as an argument** (`:keyword-arg`, which the prover
does not classify), so field-level attribution of `:mean`/`:variance` at R3a is
out of reach of any rule that stays at the call-site argument.

### E4 R2 → R3a (o)

`observation` is let-bound to `(obs/observe scan-data)` (`:6909`), the return of
`observation.clj/observe`. It is arg 1 of `fe/channel-prediction-error`
(`:7037`). The callee's `obs` is passed on as arg 1 of `channel-source-status`
(`free_energy.clj:33`), which reads `(observation/observation-status obs
channel)`, `(contains? obs channel)`, `(get obs channel)`. Every read is under
the variable `channel`. Not destructured; no literal key anywhere on the path.

### E5 R7 → R4 (rates)

`rank-cascade-actions` (`efe.clj:1066`) reads `(:adjudication-rates opts)`
(`:1171`), makes local `rates` (`:1172-1181`: the zero kernel, the declared map,
or a typed refusal map), and passes it **inside a map literal**:
`{:rates rates :q0 … :horizon … :spec …}` (`:1222`) as the argument of
`cascade-model-manifest/horizon-g-sparse-cert`, and `{… :rates rates}` (`:1196`)
as the argument of `cascade-free-energy/policy-free-energy`.

- **Live R4 consumer** (`horizon-g-sparse*`, `manifest:808`): the parameter is
  destructured in its parameter list, `{:keys [rates q0 precedence-fn horizon
  spec …] :as m}`; `horizon-g-sparse-cert [m]` forwards `m` unchanged (arg 1).
  Per token it reads `{:keys [false-neg false-pos]} (get rates v)` (`:1030`,
  `:1053`). **Keyed at both ends**; the middle is one positional forward of a
  map argument.
- `token-likelihood [rates state obs]` (`:191`) is **not** called from
  `horizon-g-sparse*`. Its callers are `observation-distribution` (`:232`, via
  `(partial token-likelihood rates state)`, reached from `step-ambiguity`
  `:629` and so from the non-sparse `horizon-g` `:661`), the reference-only
  `token-belief-at` (`:1283`, marked retired from runtime), and
  `cascade_free_energy.clj:100`, `candidate-free-energy [rates precedence q0 tau
  obs]`, called from `policy-free-energy` after it destructures `rates` out of
  its argument map. That last chain is the F_π path, which the futon2 commit
  message for `3f7a5c85` calls not live. So WM-SUPERSET-D's third hop
  ("rank-cascade-actions → token-likelihood") is real code but on the F_π path,
  not on what produces the ranked G.
- `token-likelihood` reads `{:keys [false-neg false-pos]} (get rates v)`: a
  destructure of an element selected by a variable token `v`.

### E6 R4 → R5 (Q(o|π), A)

Inside `horizon-g-sparse*`:

- R4's output. `trajectory (delay (trajectory/predictive-steps push-forward
  precedence-fn q0 record?))` (`manifest:834`). `push-forward` (the R4 kernel
  step, `:438`) is passed as **a function value**, arg 1 `transition` of
  `conditioned_trajectory.clj/predictive-steps` (`:200`), and called there as
  `(transition (precedence-fn (dec tau)) q record?)`. The result is read
  `(:belief evaluated)`, and stored in a let-bound literal `step {:tau tau
  :belief next-q :node-evaluation …}` that is an element of the lazy sequence
  (`(cons step …)`, `:212`).
- R5's input. `step (nth @trajectory (dec tau))`, `q (:belief step)` (`:901`,
  `:1001`), then `q` is arg 1 of `outcome-risk-pointwise [q log-c-of]` (`:755`,
  `:909`) and of `(marginals q)` on the factorized branch. Not destructured;
  read as `(some (fn [[o p]] …) q)` and `(for [[o p] q …])`.

So the belief is **keyed** at every hop except two: the function value passed
as an argument (`push-forward` as `transition`), and `q` into
`outcome-risk-pointwise`. `outcome-risk` (`:600`) and `step-ambiguity` (`:612`)
are the non-sparse path's R5, called from `horizon-g` (`:660-661`) with
`predicted` and `rates q`.

### E7 R16 → R2 (u, world)

`enact-fn` (`flight_runner.clj:777`):

```clojure
pub (:publication-observed
      ((or publication-observation
           (observe-publication-fn {:fetch-run-record fetch-run-record
                                    :repair-id-fn repair-id-fn}))
       flight click))                                   ; :881-885
```

- What is passed positionally is `flight` and `click`, args 1 and 2 of the
  closure `observe-publication-fn` returns (`:702`, `(fn [flight click] …)`).
  **It is not the enactment.** The enactment record is built after this call
  and never handed to it.
- The callee reads `(:target flight)`, `(:click-id click)`, `(repair-id-fn
  flight click)`, and `(fetch-run-record (:click-id click))`: keyword calls
  whose receivers are the two parameters, named for the records. It then reads
  `(:repair/publication record)` off the fetched run record.
- So u reaches o here only **through the store**: enact-fn's action publishes
  (elsewhere), and the closure asks the world about the click by id. That is
  the equation's own shape (world exogenous), not a hop the prover is missing.
- The closure's result `{:publication-observed …}` is a keyed carrier back:
  written as a returned literal of the returned `fn`, read by a keyword call
  whose **receiver is a call expression**, then copied into the `record`
  literal under the same key.

## 2. The smallest rule

Do not add a rule that finds a field through an argument. Add an optional,
checked declaration on the **caller's** box:

```clojure
:passes
[{:value  :observation              ; the name the map gives the unkeyed value
  :from   {:returns-of "obs/observe" :bound-to observation}   ; or {:keyed-read [:mu-post :trace]}
                                                              ;  or {:literal-arg-key :rates}
  :to     {:call "fe/channel-prediction-error" :arg 1
           :callee-box :r3a-prediction-error}}]               ; the box whose site var is the callee
```

`:value` becomes a field (vertex) the map may put in the writer's `:writes` and
the reader's `:reads` like any other; the difference is that a verified
`:passes` entry counts as the **occurrence evidence** for both ends, in the
places where `vertex-occurs?` and `vertex-usage` look for a keyword now.

What the prover checks (each failure is a finding, and only for a box that
carries `:passes`):

- **C1 the call exists.** In the caller's site text (its `:var` scope) there is
  a call form whose head symbol is `:call` as written (a namespace alias is
  text, as it is for record names), and, for `partial`, a leading-argument
  position; the head may not be computed. Finding
  `:passes-call-not-found`.
- **C2 the argument is what the declaration says.** The nth argument
  expression E is, by one of:
  - a keyed read the prover already attributes (`(:f r)` with `r` the record or
    an alias; `(get r :f)`; `(get-in r [… :f])`);
  - a symbol whose binding, followed through `let`, `loop`, destructuring and
    `if`/`cond` branches (the same machinery as `return-maps`), reaches either
    that keyed read or a call to `:returns-of` (a returns-bind);
  - a map literal, possibly under `cond->`, whose key `:literal-arg-key` has as
    value such a symbol;
  - `(get local expr)` where `local` resolves as above (an **element**, typed
    `:element-of`).

  A loop local with several sources must have each source accepted, and a
  source that is the return of the same passing call is recorded as
  `:self-recurrent`, not passed silently (E1-c). Finding
  `:passes-arg-not-from-declared`, naming the expression found.
- **C3 the callee's parameter is used.** The callee var (the `:callee-box`'s
  site) has, in the arity matching the call's argument count, a plain-symbol
  parameter at position n, and that symbol occurs in the body outside the
  parameter list. Its use is classified (forwarded as an argument, receiver of
  a keyword call, receiver of `get`/`contains?`/`seq`). A destructured
  parameter is not a `:passes` target: the existing destructure rule already
  attributes it. Findings `:passes-param-missing`, `:passes-param-unused`,
  `:passes-arity-mismatch`.

Why this shape and not the packet's sketch: the sketch asserts the field at the
callee, and the callee has no field. This one asserts the value, checks the call
site, and checks that the parameter is used, so the declaration is verified at
both ends and asserts nothing the code does not show. The `:passes` key is
optional, so no map that lacks it changes; the findings are reachable only by a
box that adds one.

The cases it cannot carry, by design:

- **A hop through a function value** (`push-forward` as `transition`): the call
  in the callee has a parameter as its head. Doable as `:via-param` (check the
  caller passes the symbol at position n and the callee calls that parameter in
  head position), but that is a second primitive and is left out of the
  smallest rule.
- **A head that is computed** (`((or publication-observation (observe…)) flight
  click)`): C1 refuses it, deliberately.
- **A field-level read behind a variable key or a keyword-as-argument**
  (`:mean`/`:variance` through `prediction-member`): the rule stops at the
  whole value.

## 3. What it covers

| # | edge | the positional hops the rule attributes | what else the chain needs | verdict |
|---|---|---|---|---|
| E1 | R1→R3 | a (returns-bind + arg 1 to `apply-morning-brief-events`), c (arg 1 to `apply-arena-belief-events`, with `:self-recurrent` typed) | b is already keyed (returned literal + keyword call with alias) | **covered** |
| E2 | R3→R1 | c (inline keyed arg 2 to `reconcile-belief-carry`); a's returns-bind (`belief'`) | a's terminal literal is in a `loop`: a **keyed** prover form (`return-maps` through `loop`), not a positional one | **covered**, one keyed-form dependency |
| E3 | R1→R3a | a (arg 1 to `predict-observation`), b's returns-bind and `(get predictions ch)` as `:element-of` (arg 3) | `:mean`/`:variance` stay unattributable (keyword-arg through a helper) | **covered at whole-value grain**; the field grain would need a keyed carrier in the code, and the edge does not need it |
| E4 | R2→R3a | returns-bind of `observe` and arg 1 to `channel-prediction-error`, then arg 1 to `channel-source-status` | reads are under variable channel keys; nothing finer exists | **covered at whole-value grain** |
| E5 | R7→R4 | `:literal-arg-key :rates` (the map literal argument); on the F_π path `candidate-free-energy` arg 1 and `token-likelihood` arg 1 | the live consumer is already keyed (param destructure, per-token destructure) | **covered**; the third hop belongs to the F_π path, not the ranked G |
| E6 | R4→R5 | the second positional hop, `q` into `outcome-risk-pointwise` arg 1 (from `(:belief step)`, alias `step`) | the first positional hop is a function value (`transition`); the writer `step` literal sits in `(cons step …)`, not in return position | **half**: needs `:via-param` and a keyed form for a literal element of a returned sequence; a keyed carrier in the code is not required |
| E7 | R16→R2 | none: the call passes `flight` and `click`, not u | a computed call head; the edge runs through the store | **not covered, and not a positional hop** |

**Covered by the rule: 5 of 7** (E1 to E5). **One half** (E6). **One not
applicable** (E7). The five carry two unkeyed-value grains (E3, E4) and one
keyed-form dependency (E2).

**Which need a code change (a keyed carrier), not a prover rule.** None of the
seven strictly needs one. Two code changes would make attribution cheaper and
are offered, not required:

- E7: bind the closure's result to a local named for its record and read the
  key off it (`(let [publication (…)] (:publication-observed publication))`),
  so the existing alias rule sees the receiver; and correct WM-SUPERSET-D's
  hop 2, which says "the enactment".
- E3: return `{:mean m :variance v}` records from a named accessor rather than
  `(prediction-member prediction :mean)`, if `:mean`/`:variance` are ever to be
  declared fields.

**Corrections to WM-SUPERSET-D**, from the reading above:

1. E7 hop 2 passes `flight` and `click`, not the enactment. The enactment
   reaches R2 through `(fetch-run-record (:click-id click))`.
2. E5 hop 3: `token-likelihood` is on the F_π path (`policy-free-energy` →
   `candidate-free-energy`, `cascade_free_energy.clj:100`), and via
   `observation-distribution`, not from `horizon-g-sparse*`; the live R4
   consumer of `rates` reads `(get rates v)` directly (`manifest:1030`,
   `:1053`). `token-belief-at` (`:1283`) is retired from runtime.
3. E1 and E3 hop c/a: the loop local's second source is R3's own output, so
   the edge is true for the first micro-step and recurrent afterwards.

## 4. Test cases (a new `wm_positional_passes_test.clj`, fixtures as source strings)

Each is the shape of a real site, reduced.

1. **Arg with an inline keyed read** (E2-c): `(f fresh (:mu-post prev))` with
   `:passes {:from {:keyed-read [:mu-post :trace]} :to {:call "f" :arg 2}}`;
   callee `(defn f [a carried] (if (seq carried) (get carried k) a))`:
   accepted. With the call's arg 2 replaced by `(:other prev)`:
   `:passes-arg-not-from-declared`.
2. **Local bound from a keyed read** (E1-c init): `(let [b (:belief fold)]
   (g b events))`: accepted.
3. **Local bound from a call's return** (E4): `(let [o (obs/observe scan)]
   (h o))` with `:from {:returns-of "obs/observe" :bound-to o}`: accepted; the
   same with `o` bound from a different call: refused.
4. **Loop local, two sources** (E1-c): init keyed, recur `belief'` = return of
   the passing call: accepted with `:self-recurrent`; init keyed, recur an
   unrelated call: refused.
5. **Element of a bound local** (E3-b): `(let [ps (predict b)] (cpe o ch (get ps
   ch)))` with `:element-of`: accepted; `(get other ch)`: refused.
6. **Map-literal argument key** (E5): `(sparse (cond-> {:rates rates :q0 q}
   x (assoc :z 1)))` with `:literal-arg-key :rates`: accepted; key present but
   value a different symbol: refused.
7. **`partial`** (`token-likelihood`): `(partial tl rates state)` arg 1:
   accepted; arg 2 declared: refused.
8. **Callee end.** Parameter unused: `:passes-param-unused`; parameter missing
   in the matching arity (call with 3 args, callee 2-arity only):
   `:passes-arity-mismatch`; destructured parameter: routed to the existing
   destructure rule, not a finding; multi-arity callee: the arity matching the
   call's argument count is the one checked.
9. **Call not found**: a `:passes` naming a call that is not in the `:var`
   scope: `:passes-call-not-found`; a computed head `((or a (b)) x y)`:
   `:passes-call-not-found` with a note that the head is computed.
10. **Occurrence evidence**: a box declaring `:reads [:observation]` with a
    verified `:passes` is found in role `:reads`; the same box without the
    `:passes` is `:declaration-without-occurrence`; a stale `:passes` (the call
    edited away) turns the box back to that finding.
11. **No new finding for a map without `:passes`**: the seven existing
    diagramprover namespaces and the map test's 264 findings byte-identical.
12. **Unscoped neighbours keep their occurrences**: a keyword the `:passes`
    machinery does not touch is counted as before.

## 5. Not checked

- I did not run the prover on any of this and did not read `cascade_free_energy`'s
  `policy-free-energy` parameter list beyond `validate`'s destructure.
- I did not read `trace/trace-record` (it is WM-SUPERSET-D's hop 3 for E2; I
  took its keys from that file).
- `conditioned_trajectory/intake` (the conditioning read of `m`) was not read.
- The claim that the F_π path is not live is the futon2 commit message's, not
  something I traced.
- Whether a `:passes` entry should also be accepted by `trace-findings` (the map
  test's exemplar trace checks field-connected steps by shared field) is left
  open: the field-connected check compares raw `:writes`/`:reads` entries.
