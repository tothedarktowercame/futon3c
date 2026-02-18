# P7 stepper calibration report: `:corpus-check` vs CW→manifold framing error

Date: 2026-02-18
Repo: `futon3c`
Commit base: `1d17dd5`

## Scope

Calibrate whether the framing-first stepper catches First Proof P7's Class A
failure:
- wrong path: discharge finite CW realization, assume manifold upgrade
- official outcome: NO (obstruction), not conditional YES

Script under test: `scripts/proof-p7-stepper-golden-path.clj`

## Run

Command:

```bash
PATH=/tmp/futon3a-minilm-venv/bin:$PATH \
clojure -Sdeps '{:deps {futon3b/futon3b {:local/root "/home/joe/code/futon3b"}}}' \
  scripts/proof-p7-stepper-golden-path.clj
```

Observed framing checkpoint:
- `neighbors=35`
- `framing-hits=0`
- `hit-rate=0.00`
- discipline signal: `0/15`
- domain signal: `0/20`
- status: `UNRESOLVED — no corpus signal`

## Interpretation

Result: **RED** for current corpus.

The stepper behavior is still directionally correct:
- `L-preconditions` stays open
- `L-bridge` should not be promoted

But retrieval does not surface Novikov / assembly / surgery obstruction signal
from current data.

## Minimum seed for this failure mode

1. `arXiv:math/0504564` (Lueck 2002 survey; Novikov landscape)
2. `arXiv:1101.0469` (Bartels-Farrell-Lueck; Farrell-Jones for lattices)
3. `arXiv:2303.15765` (Davis-Lueck; Z/2 exclusion framing)

## Calibration verdict

`P7 = RED` until topology/surgery corpus is indexed and queries begin returning
materially relevant neighbors for CW→manifold obstruction checks.
