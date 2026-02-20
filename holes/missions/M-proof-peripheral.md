# M-proof-peripheral: FrontierMath Preseason on the Proof Peripheral

## Status: IN PROGRESS

## Objective

Wire the proof peripheral for live use against FrontierMath open problems.
The proof infrastructure (9-phase cycle machine, ledger + DAG, canonical
statement management, G5-G0 gate checklist) already exists. This mission
adds the missing protocol pieces:

1. Blackboard observability for proof sessions
2. FrontierMath 5-mode protocol (SPEC/FALSIFY/CONSTRUCT/VERIFY/MAP)
3. TryHarder licensing to gate persistence loops
4. Mandatory FALSIFY-before-CONSTRUCT enforcement
5. State snapshot evidence emission

## Pilot Problems

Three FrontierMath open problems bootstrapped from futon6:

| ID     | Problem                                    | Domain              |
|--------|--------------------------------------------|---------------------|
| FM-001 | Ramsey Numbers for Book Graphs             | Combinatorics       |
| FM-002 | Ramsey-style Problem on 3-uniform Hypergraphs | Extremal combinatorics |
| FM-003 | Large Steiner Systems S(t,k,n) for t >= 3  | Design theory       |

All start in `:SPEC` mode with `falsify-completed? false`.

## What Was Built

### Blackboard render for `:proof`
- `format-proof-state` shows: problem ID, mode, phase, cycles, steps,
  current blocker, ledger summary (N items: X proved, Y open, Z partial),
  TryHarder license status
- Evidence emission on each blackboard projection (permanent record)

### TryHarder licensing
- `TryHarderLicense` Malli shape with bottleneck type, new lever, witness,
  kill condition, timebox
- `:tryharder-license` tool with `:create`, `:close`, `:list`, `:active` actions
- Active license tracked in proof state; closed licenses in log

### Mode tracking + FALSIFY gate
- `ProofMode` enum: `:SPEC :FALSIFY :CONSTRUCT :VERIFY :MAP`
- `proof-mode-transitions` enforces valid transitions (SPEC->FALSIFY->CONSTRUCT->...)
- `:proof-mode-set` tool blocks CONSTRUCT until falsify-completed? is true
- Leaving FALSIFY mode auto-marks falsify-completed

### State snapshot evidence
- `state-snapshot-fn` returns snapshot map on `:proof-save`
- Cycle machine emits as evidence entry tagged `[:proof :snapshot]`

### State initialization
- `state-init` loads proof mode, falsify status, and TryHarder log from
  persisted `data/proof-state/{problem-id}.edn`

## Files Modified

| File | Change |
|------|--------|
| `src/futon3c/blackboard.clj` | Added `:proof` render adaptor, `format-proof-state`, `format-ledger-summary` |
| `src/futon3c/peripheral/proof_shapes.clj` | Added ProofMode, BottleneckType, TryHarderLicense, proof-mode-transitions, updated ProofState |
| `src/futon3c/peripheral/proof_backend.clj` | Added tool-proof-mode-get, tool-proof-mode-set, tool-tryharder-license |
| `src/futon3c/peripheral/proof.clj` | Added new tools to setup-tools, state-snapshot-fn, autoconf-fn, enriched state-init |
| `test/futon3c/blackboard_test.clj` | 3 proof render tests |
| `data/proof-state/FM-001.edn` | Bootstrapped proof state |
| `data/proof-state/FM-002.edn` | Bootstrapped proof state |
| `data/proof-state/FM-003.edn` | Bootstrapped proof state |

## Completion Criteria

1. [x] Proof blackboard shows live state during sessions
2. [x] TryHarder licensing shape + backend tool
3. [x] FALSIFY-before-CONSTRUCT gate works
4. [x] All 3 FM problems bootstrapped as proof state
5. [x] State snapshot evidence on proof-save
6. [ ] One full SPEC -> FALSIFY -> CONSTRUCT -> VERIFY -> MAP cycle on FM-001
7. [ ] Evidence trail queryable via `tag=proof&tag=snapshot`

## Protocol Reference

- Source: `futon6/data/first-proof/M-frontiermath-preseason-mission.md`
- Problem templates: `futon6/data/first-proof/frontiermath-pilot/`
- Strategy requirements: SR-1 through SR-8 (in proof_shapes.clj docstrings)
- Cycle requirements: CR-1 through CR-8 (in proof_shapes.clj docstrings)
