# mfuton Book-of-Why fixture sweep

## Result

All 60 frozen mfuton fixtures convert at graph level and pass `dag/validate`,
canonical rendering, and diagram round-trip. Forty are fully representable by
the declared engine layer; twenty retain graph topology but have evaluator or
regime semantics outside the current Boolean SCM layer. Nothing is silently
dropped: all original JSON is copied verbatim under `fixtures/`, and every
evaluator omission is named below.

## Conversion disposition

| Fixture | Disposition | Reason |
|---|---|---|
| `airport-bag-posterior` | graph-only | non-Boolean value domains are graph-only: categorical; finite probability tables are preserved only in the frozen source |
| `algebra-for-all-mediation` | converted fully | graph schema fully represented; no evaluator payload declared |
| `alice-education-salary-counterfactual` | graph-only | non-Boolean value domains are graph-only: rational; affine equations are outside the Boolean SCM evaluator; numeric edge coefficients are graph-only |
| `basic-confounding-backdoor` | converted fully | graph schema fully represented; no evaluator payload declared |
| `berkeley-admissions-kruskal` | converted fully | graph schema fully represented; no evaluator payload declared |
| `berkeley-admissions-simple` | converted fully | graph schema fully represented; no evaluator payload declared |
| `berksons-paradox` | converted fully | graph schema fully represented; no evaluator payload declared |
| `birth-weight-paradox` | converted fully | graph schema fully represented; no evaluator payload declared |
| `burks-nature-nurture` | converted fully | graph schema fully represented; no evaluator payload declared |
| `cholestyramine-noncompliance` | graph-only | numeric observational distributions are not evaluated |
| `climate-change-probabilities-of-causation` | converted fully | graph schema fully represented; no evaluator payload declared |
| `daisy-house-training` | converted fully | graph schema fully represented; no evaluator payload declared |
| `deconfounding-game-1` | converted fully | graph schema fully represented; no evaluator payload declared |
| `deconfounding-game-2` | converted fully | graph schema fully represented; no evaluator payload declared |
| `deconfounding-game-3` | converted fully | graph schema fully represented; no evaluator payload declared |
| `deconfounding-game-4` | converted fully | graph schema fully represented; no evaluator payload declared |
| `deconfounding-game-5` | converted fully | graph schema fully represented; no evaluator payload declared |
| `fertilizer-improper-control` | graph-only | non-Boolean value domains are graph-only: categorical |
| `fertilizer-randomized` | graph-only | non-Boolean value domains are graph-only: categorical |
| `fertilizer-target-intervention` | graph-only | non-Boolean value domains are graph-only: categorical |
| `firing-squad` | converted fully | graph and complete fixed-grammar Boolean SCM converted |
| `forbes-smoking-adult-asthma` | converted fully | graph schema fully represented; no evaluator payload declared |
| `freedman-child-age-fork` | converted fully | graph schema fully represented; no evaluator payload declared |
| `generic-frontdoor` | converted fully | graph schema fully represented; no evaluator payload declared |
| `generic-linear-instrumental-variable` | graph-only | numeric edge coefficients are graph-only |
| `inheritance-galton-board` | converted fully | graph schema fully represented; no evaluator payload declared |
| `inheritance-genetic` | converted fully | graph schema fully represented; no evaluator payload declared |
| `john-snow-before-instrument` | converted fully | graph schema fully represented; no evaluator payload declared |
| `john-snow-instrumental-variable` | converted fully | graph schema fully represented; no evaluator payload declared |
| `jtpa-job-training` | converted fully | graph schema fully represented; no evaluator payload declared |
| `linear-mediation-path-diagram` | graph-only | numeric edge coefficients are graph-only |
| `lords-paradox-wainer-brown` | converted fully | graph schema fully represented; no evaluator payload declared |
| `lords-paradox` | converted fully | graph schema fully represented; no evaluator payload declared |
| `mendelian-randomization-hdl` | converted fully | graph schema fully represented; no evaluator payload declared |
| `monty-hall-fake-a-deal` | converted fully | graph schema fully represented; no evaluator payload declared |
| `monty-hall-make-a-deal` | converted fully | graph schema fully represented; no evaluator payload declared |
| `new-napkin-problem` | converted fully | graph schema fully represented; no evaluator payload declared |
| `nonlinear-threshold-mediation` | graph-only | non-Boolean value domains are graph-only: rational; affine equations are outside the Boolean SCM evaluator; numeric edge coefficients are graph-only; structural expression exceeds fixed Boolean grammar; all equations skipped |
| `scurvy-vitamin-c-mediator` | converted fully | graph schema fully represented; no evaluator payload declared |
| `scurvy-wrong-acidity-mediator` | converted fully | graph schema fully represented; no evaluator payload declared |
| `simpsons-paradox-drug-blood-pressure` | converted fully | graph schema fully represented; no evaluator payload declared |
| `simpsons-paradox-drug-gender` | converted fully | graph schema fully represented; no evaluator payload declared |
| `simpsons-paradox-exercise-age-cholesterol` | converted fully | graph schema fully represented; no evaluator payload declared |
| `smoking-gene-two-readings` | converted fully | graph schema fully represented; no evaluator payload declared |
| `smoking-tar-cancer-frontdoor` | converted fully | graph schema fully represented; no evaluator payload declared |
| `tourniquet-selection-mediation` | graph-only | non-Boolean value domains are graph-only: categorical; numeric observational distributions are not evaluated |
| `transport-arkansas-target` | converted fully | graph schema fully represented; no evaluator payload declared |
| `transport-boston-car-ownership` | graph-only | selection/regime annotations are graph-only |
| `transport-honolulu-outcome` | graph-only | selection/regime annotations are graph-only |
| `transport-los-angeles-age` | graph-only | selection/regime annotations are graph-only |
| `transport-san-francisco-clicks` | graph-only | selection/regime annotations are graph-only |
| `transport-toronto-randomized-clicks` | graph-only | selection/regime annotations are graph-only |
| `vaccination` | graph-only | finite probability tables are preserved only in the frozen source |
| `vacuuming-causal-explanation` | converted fully | graph and complete fixed-grammar Boolean SCM converted |
| `walking-age-mortality` | converted fully | graph schema fully represented; no evaluator payload declared |
| `wermuth-cox-sequential-treatment` | converted fully | graph schema fully represented; no evaluator payload declared |
| `wright-guinea-pig-coat-color` | graph-only | numeric edge coefficients are graph-only |
| `wright-price-to-supply-reduced-form` | graph-only | numeric edge coefficients are graph-only |
| `wright-puppy-birth-weight` | graph-only | numeric edge coefficients are graph-only; numeric linear correlations are not evaluated |
| `wright-supply-to-price-reduced-form` | graph-only | numeric edge coefficients are graph-only |

Totals: converted fully 40; graph-only
20; skipped 0.

## Curated semantic-kind overrides

This literal table is the conversion authority when the source marks
observability only in prose. It is intentionally not a regex classifier. The
Burks `social` and `child` rows pin the reviewed false-positive judgments;
Burks `x` already carries explicit `observed: false` and needs no override.

| Fixture | Variable | Converted kind | Source prose, verbatim | Judgment |
|---|---|---|---|---|
| `burks-nature-nurture` | `child` | `observed` | “The outcome receiving direct, mediated, and unmeasured influences.” | the outcome is observed; unmeasured modifies its influences, not the outcome |
| `burks-nature-nurture` | `social` | `observed` | “A mediator caused by Parental Intelligence and also a collider receiving unmeasured causes.” | the mediator is observed; unmeasured modifies its incoming causes, not the mediator |
| `deconfounding-game-1` | `abnormality` | `latent-unobserved` | “An unobservable abnormality induced by smoking and capable of causing miscarriage.” | the variable itself is explicitly unobservable |
| `deconfounding-game-2` | `current_abnormality` | `latent-unobserved` | “An unobservable abnormality in the second pregnancy.” | the variable itself is explicitly unobservable |
| `deconfounding-game-2` | `prior_abnormality` | `latent-unobserved` | “An unobservable abnormality in the first pregnancy.” | the variable itself is explicitly unobservable |
| `generic-frontdoor` | `c` | `latent-unobserved` | “The unobserved confounder of X and Y.” | the variable itself is explicitly unobserved |
| `jtpa-job-training` | `m` | `latent-unobserved` | “The unobserved common cause affecting participation and earnings.” | the variable itself is explicitly unobserved |
| `wright-puppy-birth-weight` | `other-gestation-causes` | `latent-unobserved` | “Unobserved exogenous causes of gestation period other than litter size.” | the variable itself denotes unobserved exogenous causes |
| `wright-puppy-birth-weight` | `other-growth-causes` | `latent-unobserved` | “Unobserved exogenous causes of prenatal growth rate other than litter size.” | the variable itself denotes unobserved exogenous causes |
| `wright-puppy-birth-weight` | `prenatal-growth-rate` | `latent-unobserved` | “The unobserved rate at which the pup grows in utero.” | the variable itself is explicitly unobserved |

## Reviewed identification delta

The before column is the committed pre-correction sweep verdict. The after
column is the corrected verdict, serialized verbatim from `engine-results.json`.
`deconfounding-game-1` and `deconfounding-game-2` now run their evident
smoking/miscarriage pairs instead of recording no guessed pair.

| Fixture | Changed? | Before | After |
|---|---|---|---|
| `burks-nature-nurture` | `false` | `{"identification":{"adjustment-sets":[[]],"identifiable?":true,"method":"backdoor"},"pair":{"outcome":"child","treatment":"parental"}}` | `{"identification":{"adjustment-sets":[[]],"identifiable?":true,"method":"backdoor"},"pair":{"outcome":"child","treatment":"parental"}}` |
| `deconfounding-game-1` | `true` | `{"identification":null,"pair":null}` | `{"identification":{"adjustment-sets":[[]],"identifiable?":true,"method":"backdoor"},"pair":{"outcome":"future_miscarriage","treatment":"smoking"}}` |
| `deconfounding-game-2` | `true` | `{"identification":null,"pair":null}` | `{"identification":{"adjustment-sets":[[],["physiology"],["physiology","prior_miscarriage"],["prior_smoking"],["physiology","prior_smoking"],["prior_miscarriage","prior_smoking"],["physiology","prior_miscarriage","prior_smoking"]],"identifiable?":true,"method":"backdoor"},"pair":{"outcome":"future_miscarriage","treatment":"current_smoking"}}` |
| `generic-frontdoor` | `true` | `{"identification":{"adjustment-sets":[["c"]],"identifiable?":true,"method":"backdoor"},"pair":{"outcome":"y","treatment":"x"}}` | `{"identification":{"identifiable?":true,"mediators":["m"],"method":"front-door"},"pair":{"outcome":"y","treatment":"x"}}` |
| `jtpa-job-training` | `true` | `{"identification":{"adjustment-sets":[["m"]],"identifiable?":true,"method":"backdoor"},"pair":{"outcome":"e","treatment":"s"}}` | `{"identification":{"identifiable?":false,"method":"refusal","proof-status":"proved-impossible","reason":"not-identifiable"},"pair":{"outcome":"e","treatment":"s"}}` |
| `wright-puppy-birth-weight` | `true` | `{"identification":{"adjustment-sets":[[],["other-gestation-causes"],["other-growth-causes"],["other-gestation-causes","other-growth-causes"]],"identifiable?":true,"method":"backdoor"},"pair":{"outcome":"birth-weight","treatment":"litter-size"}}` | `{"identification":{"adjustment-sets":[[]],"identifiable?":true,"method":"backdoor"},"pair":{"outcome":"birth-weight","treatment":"litter-size"}}` |

## Per-fixture engine sweep

The validation column is `DAG/canonical/round-trip`. CI counts cover all
engine-implied independencies with conditioning sets of size at most one.
Natural treatment/outcome pairs are selected only by conventional X/Y naming,
explicit fixture semantics, or a unique observed source/sink; all others receive
the structural CI sweep without a guessed effect query.

| Fixture | V/E | Validation | CIs | Natural pair and why | Our identification | Counterfactual |
|---|---:|---|---:|---|---|---|
| `airport-bag-posterior` | 3/2 | `true`/`true`/`true` | 1 | `bag-on-plane -> bag-on-carousel` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `algebra-for-all-mediation` | 3/3 | `true`/`true`/`true` | 0 | `algebra_for_all -> learning` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `alice-education-salary-counterfactual` | 3/3 | `true`/`true`/`true` | 0 | `education -> salary` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `basic-confounding-backdoor` | 3/3 | `true`/`true`/`true` | 0 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `berkeley-admissions-kruskal` | 4/5 | `true`/`true`/`true` | 1 | `gender -> outcome` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `berkeley-admissions-simple` | 3/3 | `true`/`true`/`true` | 0 | `gender -> outcome` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `berksons-paradox` | 3/2 | `true`/`true`/`true` | 1 | — (no natural pair; CI sweep only) | not run | — |
| `birth-weight-paradox` | 4/5 | `true`/`true`/`true` | 1 | — (no natural pair; CI sweep only) | not run | — |
| `burks-nature-nurture` | 4/5 | `true`/`true`/`true` | 1 | `parental -> child` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `cholestyramine-noncompliance` | 4/4 | `true`/`true`/`true` | 1 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `refusal`; identifiable=`false` | — |
| `climate-change-probabilities-of-causation` | 4/3 | `true`/`true`/`true` | 3 | `greenhouse -> response` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `daisy-house-training` | 3/3 | `true`/`true`/`true` | 0 | `pets -> training` — unique observed DAG source and sink | `backdoor`; identifiable=`true` | — |
| `deconfounding-game-1` | 4/3 | `true`/`true`/`true` | 3 | `smoking -> future_miscarriage` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `deconfounding-game-2` | 7/7 | `true`/`true`/`true` | 16 | `current_smoking -> future_miscarriage` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `deconfounding-game-3` | 4/5 | `true`/`true`/`true` | 0 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `refusal`; identifiable=`false` | — |
| `deconfounding-game-4` | 5/4 | `true`/`true`/`true` | 6 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `deconfounding-game-5` | 5/6 | `true`/`true`/`true` | 1 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `fertilizer-improper-control` | 7/11 | `true`/`true`/`true` | 10 | `fertilizer -> yield` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `fertilizer-randomized` | 8/7 | `true`/`true`/`true` | 21 | `fertilizer -> yield` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `fertilizer-target-intervention` | 7/6 | `true`/`true`/`true` | 15 | `fertilizer -> yield` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `firing-squad` | 5/5 | `true`/`true`/`true` | 4 | `soldier-a -> death` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | `deterministic-scm`; answer=`true`; boolean_model_test.py: conclusion is true |
| `forbes-smoking-adult-asthma` | 9/16 | `true`/`true`/`true` | 14 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `freedman-child-age-fork` | 3/2 | `true`/`true`/`true` | 1 | `shoe-size -> reading-ability` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `generic-frontdoor` | 4/4 | `true`/`true`/`true` | 1 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `front-door`; identifiable=`true` | — |
| `generic-linear-instrumental-variable` | 4/4 | `true`/`true`/`true` | 1 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `refusal`; identifiable=`false` | — |
| `inheritance-galton-board` | 5/4 | `true`/`true`/`true` | 6 | — (no natural pair; CI sweep only) | not run | — |
| `inheritance-genetic` | 8/7 | `true`/`true`/`true` | 27 | — (no natural pair; CI sweep only) | not run | — |
| `john-snow-before-instrument` | 3/3 | `true`/`true`/`true` | 0 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `refusal`; identifiable=`false` | — |
| `john-snow-instrumental-variable` | 4/4 | `true`/`true`/`true` | 1 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `refusal`; identifiable=`false` | — |
| `jtpa-job-training` | 4/5 | `true`/`true`/`true` | 0 | `s -> e` — fixture title/content gives the named cause and outcome | `refusal`; identifiable=`false` | — |
| `linear-mediation-path-diagram` | 3/3 | `true`/`true`/`true` | 0 | `treatment -> outcome` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `lords-paradox-wainer-brown` | 4/5 | `true`/`true`/`true` | 0 | `d -> y` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `lords-paradox` | 4/5 | `true`/`true`/`true` | 0 | `s -> y` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `mendelian-randomization-hdl` | 4/4 | `true`/`true`/`true` | 1 | `hdl -> attack` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `monty-hall-fake-a-deal` | 3/1 | `true`/`true`/`true` | 2 | — (no natural pair; CI sweep only) | not run | — |
| `monty-hall-make-a-deal` | 3/2 | `true`/`true`/`true` | 1 | — (no natural pair; CI sweep only) | not run | — |
| `new-napkin-problem` | 6/7 | `true`/`true`/`true` | 3 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `general-id`; identifiable=`true` | — |
| `nonlinear-threshold-mediation` | 4/4 | `true`/`true`/`true` | 2 | `education -> outcome` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `scurvy-vitamin-c-mediator` | 3/2 | `true`/`true`/`true` | 1 | `citrus -> scurvy` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `scurvy-wrong-acidity-mediator` | 3/2 | `true`/`true`/`true` | 1 | `citrus -> scurvy` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `simpsons-paradox-drug-blood-pressure` | 3/3 | `true`/`true`/`true` | 0 | `d -> h` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `simpsons-paradox-drug-gender` | 3/3 | `true`/`true`/`true` | 0 | `d -> h` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `simpsons-paradox-exercise-age-cholesterol` | 3/3 | `true`/`true`/`true` | 0 | `e -> c` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `smoking-gene-two-readings` | 3/3 | `true`/`true`/`true` | 0 | `smoking -> lung_cancer` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `smoking-tar-cancer-frontdoor` | 4/4 | `true`/`true`/`true` | 1 | `s -> c` — fixture title/content gives the named cause and outcome | `front-door`; identifiable=`true` | — |
| `tourniquet-selection-mediation` | 4/6 | `true`/`true`/`true` | 0 | `tourniquet_use -> post_admission_survival` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `transport-arkansas-target` | 4/4 | `true`/`true`/`true` | 1 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `transport-boston-car-ownership` | 6/6 | `true`/`true`/`true` | 9 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `transport-honolulu-outcome` | 5/4 | `true`/`true`/`true` | 6 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `transport-los-angeles-age` | 5/5 | `true`/`true`/`true` | 5 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `transport-san-francisco-clicks` | 5/5 | `true`/`true`/`true` | 3 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `transport-toronto-randomized-clicks` | 5/4 | `true`/`true`/`true` | 6 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `vaccination` | 4/4 | `true`/`true`/`true` | 1 | `vaccination -> death` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `vacuuming-causal-explanation` | 4/3 | `true`/`true`/`true` | 4 | `vacuuming -> unhappy` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | `deterministic-scm`; answer=`false`; causal_explanation_test.py: alternative target value is false |
| `walking-age-mortality` | 3/3 | `true`/`true`/`true` | 0 | `walking -> mortality` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `wermuth-cox-sequential-treatment` | 5/6 | `true`/`true`/`true` | 3 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `wright-guinea-pig-coat-color` | 20/24 | `true`/`true`/`true` | 156 | — (no natural pair; CI sweep only) | not run | — |
| `wright-price-to-supply-reduced-form` | 4/4 | `true`/`true`/`true` | 1 | `p -> s` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `wright-puppy-birth-weight` | 6/6 | `true`/`true`/`true` | 6 | `litter-size -> birth-weight` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `wright-supply-to-price-reduced-form` | 4/4 | `true`/`true`/`true` | 1 | `s -> p` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |

## Oracle agreement

| Oracle | Applicable verdict | Agreements | Checked | Disagreements |
|---|---|---:|---:|---:|
| NetworkX | d-separation | 350 | 350 | 0 |
| dagitty | d-separation | 350 | 350 | 0 |
| y0 | treatment/outcome identifiability | 53 | 53 | 0 |
| Independent exhaustive Boolean worlds | deterministic counterfactual | 2 | 2 | 0 |

The Boolean-world check is a separate implementation of finite deterministic
SCM enumeration, not a counterfactual package oracle. Rob expectations were
cheaply extracted only for the firing-squad and vacuuming pytest cases and are
pinned in the per-fixture table.

## Discrepancy harvest

Verbatim discrepancy list: `[]`.

Each discrepancy record, when present, carries our verdict, the named oracle's
verdict, and a Rob pytest expectation where one was cheaply extractable.

## Frozen-artifact hashes

- Frozen 60-fixture bundle SHA-256: `37f8ecc08807c67b0b0fcf5906bb9ead3c77e771a43f9a241b2ae08d1786cc58`
- Converted 60-fixture bundle SHA-256: `b182d92eed606d281e7cc003a7761f050ba5984ada5771ace37381f08e6f43b3`
- `sweep-results.json` SHA-256: `34007d6afcb1fea08d1d3d4d3a566c1be4167e10b05540009c9ad5308d5195b3`
- The report's own SHA-256 and component-result hashes are recorded in
  `SHA256SUMS`, generated after this report.

## Reproduce

From the repository root:

```sh
holes/labs/M-diagramprover/mfuton-sweep/run.sh
```

The run uses only the repository's Clojure dependencies and the existing
`/home/joe/.venvs/causal-oracles` environment.
