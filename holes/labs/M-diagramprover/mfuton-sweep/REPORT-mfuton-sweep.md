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
| `deconfounding-game-1` | 4/3 | `true`/`true`/`true` | 3 | — (no natural pair; CI sweep only) | not run | — |
| `deconfounding-game-2` | 7/7 | `true`/`true`/`true` | 16 | — (no natural pair; CI sweep only) | not run | — |
| `deconfounding-game-3` | 4/5 | `true`/`true`/`true` | 0 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `refusal`; identifiable=`false` | — |
| `deconfounding-game-4` | 5/4 | `true`/`true`/`true` | 6 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `deconfounding-game-5` | 5/6 | `true`/`true`/`true` | 1 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `fertilizer-improper-control` | 7/11 | `true`/`true`/`true` | 10 | `fertilizer -> yield` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `fertilizer-randomized` | 8/7 | `true`/`true`/`true` | 21 | `fertilizer -> yield` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `fertilizer-target-intervention` | 7/6 | `true`/`true`/`true` | 15 | `fertilizer -> yield` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `firing-squad` | 5/5 | `true`/`true`/`true` | 4 | `soldier-a -> death` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | `deterministic-scm`; answer=`true`; boolean_model_test.py: conclusion is true |
| `forbes-smoking-adult-asthma` | 9/16 | `true`/`true`/`true` | 14 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `freedman-child-age-fork` | 3/2 | `true`/`true`/`true` | 1 | `shoe-size -> reading-ability` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
| `generic-frontdoor` | 4/4 | `true`/`true`/`true` | 1 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `backdoor`; identifiable=`true` | — |
| `generic-linear-instrumental-variable` | 4/4 | `true`/`true`/`true` | 1 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `refusal`; identifiable=`false` | — |
| `inheritance-galton-board` | 5/4 | `true`/`true`/`true` | 6 | — (no natural pair; CI sweep only) | not run | — |
| `inheritance-genetic` | 8/7 | `true`/`true`/`true` | 27 | — (no natural pair; CI sweep only) | not run | — |
| `john-snow-before-instrument` | 3/3 | `true`/`true`/`true` | 0 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `refusal`; identifiable=`false` | — |
| `john-snow-instrumental-variable` | 4/4 | `true`/`true`/`true` | 1 | `x -> y` — fixture uses the conventional observed X/Y effect pair | `refusal`; identifiable=`false` | — |
| `jtpa-job-training` | 4/5 | `true`/`true`/`true` | 0 | `s -> e` — fixture title/content gives the named cause and outcome | `backdoor`; identifiable=`true` | — |
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
| y0 | treatment/outcome identifiability | 51 | 51 | 0 |
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
- Converted 60-fixture bundle SHA-256: `158fd8c760ba95ac8b1c0e7f22ed1eda480955c606089bfaea9016e6917a3928`
- `sweep-results.json` SHA-256: `40e767db5e5878ef858d31d963e57e06df7045be659a18d0c91189b50ff7b15a`
- The report's own SHA-256 and component-result hashes are recorded in
  `SHA256SUMS`, generated after this report.

## Reproduce

From the repository root:

```sh
holes/labs/M-diagramprover/mfuton-sweep/run.sh
```

The run uses only the repository's Clojure dependencies and the existing
`/home/joe/.venvs/causal-oracles` environment.
