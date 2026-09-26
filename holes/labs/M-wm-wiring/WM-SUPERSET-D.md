# WM-SUPERSET-D: what would make each registry edge declared

claude-11, 2026-09-26. Discovery, read-only. This covers the 33 registry
edges not declared at var grain.

Revisions read:

| what | revision |
|---|---|
| map | futon3c 47513216, read through git |
| registry | futon2 1b3ba281 |
| code | futon2 e7f8b931 (the files read are clean in the checkout) |
| join | `bb spike/wm_vs_equation_dag.bb 47513216`: declared 4, boxed-no-field 10, unboxed 23 at var grain |

`WM-SUPERSET-D.edn` holds the same content machine-readably: per edge its
class, hops, the boxes that exist and are missing, the map edit, and the
prover form each field needs. Packets can be generated from it.

## Counts

| class | edges | meaning |
|---|---|---|
| wire | 2 | same keyword on the same record, from a source site to a consumer site |
| scoped wire | 2 | same keyword carried across successive records (`{:record r}`) |
| hop chain | 12 | different keys, or positional hand-offs, with carriers between. **7** of them include a hop that passes no keyword (positional), which the textual prover cannot attribute |
| no path | 17 | the code does not carry the edge from the registry's `:code` sites |

So 16 edges have a data path in code. **9 are declarable with keyword fields
as the code stands**: the 2 wires, the 2 scoped wires, and 5 keyed hop
chains. Of those, `[:R3a :R3]` is declarable only as a composition through
R7's box, because eps never reaches R3 unweighted. The other **7 need a
positional-argument rule in the prover, or a keyed carrier in the code**,
before any map edit can make the join say `:declared`.

Of the 17 with no path:

| conformance | edges |
|---|---|
| `:not-realised`, already in the registry's `:holes` | `[R1 R17]`, `[R2 R17]` |
| `:path-dependent`, already in `:holes` | `[R6 R16]` |
| should be recorded (they are not in `:holes` today) | the other 14; see below |

## What the reading found beyond the per-edge detail

Four places where the registry's `:code` points at the wrong site, or at
nothing live. Each needs a registry correction before its packet can be
mechanical.

1. **R2's `o` for the tick is not in R2's `:code`.** `judge` computes
   `observation (obs/observe scan-data)`, i.e. `futon2.aif.observation/observe`,
   and route-tags it `:R2`. R2's `:code` names only the flight's token
   observation (`observation_checks/observe`, `flight_runner/observe-fn`,
   `observe-publication-fn`). The tick's belief loop reads the channel vector
   from `observation.clj`, which is the carrier mismatch the Lean triage also
   met (the `ObservationVector` versus `O` question).
2. **F_π is not computed on the live path.**
   `policy-prefix-evidence/production-ranked` removes `:f` from every ranked
   entry and stamps it `:f-prefix {:status :not-supplied :reason
   :no-admitted-policy-prefix}`. `selection-candidate` then reads F from that
   stamp. The registry's R8 sites (`f-pi-dark-readback`,
   `f-pi-posterior-opts`, `f-pi-vector`) have no caller at e7f8b931. Every
   edge into or out of F_π is therefore no-path, and the R8 row's
   `:realised-flag-gated` status should become `:holes` entries.
3. **Two beliefs.** The entity belief (`belief.clj`, carried as `:mu-post`) is
   R1's `:code`. The cascade path (R4/R5) reads `:cascade-belief joint-q0`,
   the token belief from `token-belief-carry`, which R1 does not name. `[R1
   R4]` is no-path at R1's named site, and would be a keyed wire on
   `:cascade-belief` once R1 names the token belief's site.
4. **R14 names only τ's consumers.** τ is produced by `precision_carry/advance`
   as `(:beta beta-state)`; R14's `:code` names only `selection-posterior` and
   `select-action-cascades`. Likewise R4's `:forward-model` `:code`
   (`forward_model/predict-multi-horizon`) is not on the cascade path: the live
   Q(o|π) is `cascade_model_manifest` `push-forward`/`evaluate-state`.

## Per node

Order: one packet per R-node, in pipeline order. Registry corrections come
first. Each packet creates the boxes at its own node's sites, and an edge is
declared in the later of its two endpoints' packets. "After" counts the
edges each packet adds to `:declared`, with keyed edges and positional edges
kept apart.

### 1. R2: new box `r2-tick-observe` (`observation.clj` `observe`)
| edge | class | source → consumer (keys) | boxes missing | map edit |
|---|---|---|---|---|
| R16→R2 (u, world) | hop chain, positional | `:enacted-steps` (r9-selection-law → r0-enact-step, on the map) → enactment passed positionally into observe-publication-fn → `:publication-observed` (on the map) | none | none possible with the current prover. `world` is exogenous; it enters as the published store |

After: 0 keyed, 1 positional.

### 2. R3a: new boxes `r3a-prediction-error` (`free_energy/compute-prediction-error`), `r3a-predict-observation`
| edge | class | keys | missing | edit |
|---|---|---|---|---|
| R2→R3a (o) | hop chain, positional | judge local `observation` → `channel-prediction-error` (arg 1) | `r2-tick-observe`, `r3a-prediction-error` | needs the registry fix above |

After: 0 keyed, 1 positional.

### 3. R7: new box `r7-weighted-error` (`precision/weighted-error`), optional `r7-check-error-rates`
| edge | class | keys | missing | edit |
|---|---|---|---|---|
| R3a→R7 (eps) | **wire** | `:error`. Written as a literal key `{:status :present :error …}` by compute-prediction-error; read as a keyword call `(:error error-map)` in weighted-error. `update-precision-state` reads it as a keyword argument, which the prover does not classify | both boxes | `r3a-prediction-error :writes [:error]`; `r7-weighted-error :reads [:error] :writes [:precision :weighted-error]`. `:error` is generic, so expect a standing finding |
| R2→R7 (o, ref-label) | hop chain | `:measurement`/`:counts` from `sourced-rates`, out of recorded check verdicts against reference labels | `r7-check-error-rates` | **to confirm**: that the counts come from the C8 registry entries. `:likelihood-precision`'s half has `:code nil` |

After: 1 or 2 keyed (the second pending the confirmation), 0 positional.

### 4. R3: new boxes `r3-aggregate-driver` (`belief/r3d-aggregate-driver`), `r3-apply-belief-events`, `morning-brief-fold`
| edge | class | keys | edit |
|---|---|---|---|
| R7→R3 (Pi) | **wire** | `:weighted-error`, added by weighted-error and read as a keyword call `(:weighted-error %)` in r3d-aggregate-driver | `r3-aggregate-driver :reads [:weighted-error] :writes [:driver]` |
| R3a→R3 (eps) | hop chain | `:error` → `:weighted-error`, through R7 | none extra; declared only if the join accepts a composition |

After: 1 keyed, plus 1 by composition.

### 5. R1: new boxes `r1-belief-carry` (`belief/reconcile-belief-carry`), `trace-record` (`trace.clj`)
| edge | class | hops | edit |
|---|---|---|---|
| R1→R3 (mu) | hop chain, positional | reconcile → local → `:belief` on the morning-brief fold (keyword call) → loop → `apply-arena-belief-events` (positional) | `morning-brief-fold :writes [[:belief {:record :morning-brief-fold}]]`; `r7-fold-call :reads+` the same |
| R3→R1 (s-next) | hop chain, positional | belief′ → `:belief` on judge's output → `trace-record` writes `:mu-post` (record `:trace`) → next tick's judge reads `(:mu-post prev-trace-record)` → reconcile (positional) | `trace-record :reads [[:belief {:record :judge-output}]] :writes [[:mu-post {:record :trace}]]`; `r7-fold-call :writes+ [[:belief {:record :judge-output}]] :reads+ [[:mu-post {:record :trace}]]` |
| R1→R3a (mu) | hop chain, positional | belief → `predict-observation` → predictions → `channel-prediction-error`, all positional | none possible |

After: 0 keyed, 3 positional. The keyed hops above make judge's own
carriers visible, but the end sites still receive mu positionally.

### 6. R13: new box `r13-family-params` (`war_machine/cascade-family-parameters`)
Declares nothing alone.

### 7. R4: new boxes `r4-token-likelihood`, `r4-evaluate-state`
| edge | class | keys | edit |
|---|---|---|---|
| R13→R4 (T) | **scoped wire** | `:horizon-steps`, on the cascade problem → family params → the rank opts | `construction-assemble-one :writes+ [[:horizon-steps {:record :cascade-problem}]]`; `r13-family-params` reads and re-writes it; `r9-decision :writes+ [[:horizon-steps {:record :rank-opts}]]`; `r4-kernel :reads+` it. **To confirm** where `assemble-one` sets it |
| R7→R4 (rates) | hop chain, positional | `:rates` (sourced-rates) → `:adjudication-rates` (lane → kernel, on the map) → token-likelihood, positional | `r6-sourced-rates :writes+ [:rates]`, `r6-cascade-lane :reads+ [:rates]`, `r4-token-likelihood :reads [:false-neg :false-pos]` |
| R2→R4 (interp) | hop chain | `:precedence`, construct → order-use → co-apply-kernel (last hop positional) | `construction-construct :writes+ [:precedence]`, `r4-order-use :reads+ [:precedence]`. `:interp` needs a `:code` first |

After: 2 keyed (T and interp), 1 positional (rates).

### 8. R5: new boxes `r5-outcome-risk`, `r5-step-ambiguity`
| edge | class | keys | edit |
|---|---|---|---|
| R4→R5 (Q-o-pi, A) | hop chain, positional | inside `cascade_model_manifest`: horizon-g-sparse-cert → push-forward q → outcome-risk / step-ambiguity | none possible |
| R19→R5 (C, U-t) | hop chain | C: `:want-span` → flight `:wants` → the cascade spec's `:want` (the map's scoped read, no writer yet) | `r9-decision :writes+ [[:want {:record :cascade-spec}]]`. U-t: no path (with-pair-overlap is an island) |

After: 1 keyed (C), 1 positional.

### 9. R14: new box `r14-beta-state` (`precision_carry/advance`)
Declares nothing alone.

### 10. R6: new box `r6-selection-posterior` (`cascade_selection/selection-posterior`)
| edge | class | keys | edit |
|---|---|---|---|
| R14→R6 (tau) | **scoped wire** | `:beta`: the beta-state (keyword call) → the selection opts (literal, destructured) → the posterior args (literal, destructured) | `r14-beta-state :writes [[:beta {:record :beta-state}]]`; `r9-decision` reads beta-state and writes selection-opts; `r9-selection-law` reads selection-opts and writes posterior-args; `r6-selection-posterior :reads` posterior-args |
| R2→R6 (interp) | hop chain | `:precedence` → containment-order | `r4-constructor :reads+ [:precedence]` (**to confirm** the key) |

After: 2 keyed.

### Totals after all ten packets
| | keyed | positional | not declarable |
|---|---|---|---|
| after the packets | 9 (with `[R3a R3]` by composition) | 7 (need a prover rule) | 17 (no path) |

Added to today's 4 declared, the keyed edges give 13 of 37.

## The no-path edges, and what to record for each

| edge | why no path | record as |
|---|---|---|
| R1→R17, R2→R17 | registry `:holes` | `:not-realised` (already) |
| R6→R16 | registry `:holes` | `:path-dependent` (already) |
| R8→R6, R2→R8, R4→R8, R6→R8 | F_π is not live (the `:f-prefix :not-supplied` stamp; the R8 sites are uncalled) | `:not-realised` |
| R8→R3, R2→R3, R4→R3 | `:state-belief-update` has `:code nil` | `:not-realised` |
| R4→R3a | `:state-prediction-error` has `:code nil` | `:not-realised` |
| R4→R7 | `:likelihood-precision` has `:code nil` (candidate site: `likelihood_precision/tempered-rates`) | pointer first, then re-read |
| R2→R1, R16→R1, R4→R1 | R1's site (`reconcile-belief-carry`) takes only the fresh belief and `:mu-post`; o, u, A and B reach mu only through R3's update | `:enters-through :s-next` on `:belief-state` |
| R1→R4 | the cascade path reads the token belief (`:cascade-belief`), not R1's entity belief | pointer (R1's second belief), then a keyed wire |
| R4→R2 | the flight's observation reads the world, not the model's A and B | by design (the row states the generative process) |

## Registry sites with no box

`:registry-no-site` is empty. The registry's `:code` names **48** (node,
file, var) pairs with no box on the map; the full list is in the EDN under
`:code-sites-without-box`. It was extracted mechanically from the `:code`
strings, so a few entries are keys rather than vars, e.g. `efe.clj
horizon-steps`. The largest groups:

| node | missing boxes |
|---|---|
| R1 | the five belief.clj entity-belief vars |
| R3 | `r3d-aggregate-driver`, `update-belief-batch`, `apply-arena-belief-events` |
| R3a | both free_energy.clj producers |
| R4 | `cascade_model_manifest`'s `token-likelihood`, `observation-distribution`, `evaluate-state`, `evaluate-co-apply`; `forward_model.clj` |
| R5 | `horizon-g` and its variants |
| R6/R16 | `cascade_selection.clj`'s `selection-posterior`, `bayes-choice`, `law-receipt`; `policy.clj`'s `selection-scores`, `softmax-weights` |
| R7 | `precision.clj`'s two fns; `rates-by-class`, `measured-cell` |
| R8 | the four f-pi sites, all uncalled |

## What this rests on, and what it does not

- **Read:** the join's output in full; the registry rows' `:code` and
  `:imports`; `judge`'s belief loop (war_machine.clj, the `judge` defn from
  :6865); `cascade-decision-admitted` and `cascade-family-parameters`;
  `select-action-cascades` and `selection-candidate` (policy.clj);
  `selection-posterior` (cascade_selection.clj); `production-ranked`;
  `weighted-error` and `r3d-aggregate-driver`; `compute-prediction-error`'s
  docstring; `sourced-rates`' return keys; `trace.clj`'s trace record keys;
  and wm-org-layer.edn for the call order.
- **To confirm before those packets run:**
  - where `assemble-one` sets `:horizon-steps` (R13→R4);
  - that `sourced-rates`' counts come from the C8 registry entries (R2→R7);
  - which key `containment-order` reads the patterns under (R2→R6);
  - the name of the defn that writes `:mu-post` (trace.clj :436-450).
- **Not done:** no map edit, no code, nothing run.
