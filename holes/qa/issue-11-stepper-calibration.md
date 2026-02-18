# Issue #11 calibration report: `:corpus-check` vs P1 framing error

Date: 2026-02-17
Repo: `futon3c`
Issue: https://github.com/tothedarktowercame/futon3c/issues/11
Commit base: `12005ba`

## Scope

Calibrate whether `:corpus-check` can catch the P1 framing failure
("assume `mu ~ mu_0` for `\Phi^4_3` before verifying preconditions).

## Blocking bug found and fixed

`proof_backend.clj` called futon3a `notions_search.py` with `--json`,
but the script does not support `--json` and emits line-based text.

Fix implemented:
- remove `--json` flag from process invocation
- parse futon3a text output lines (`"1. id (score) - title"`)
- keep JSON parsing as a forward-compatible path
- ignore non-neighbor log lines in stdout/stderr

Validation:
- `clojure -Sdeps '{:deps {futon3b/futon3b {:local/root "/home/joe/code/futon3b"}}}' -X:test`
- `clj-kondo --lint src/futon3c/peripheral/proof_backend.clj test/futon3c/peripheral/proof_backend_test.clj`
- `emacs -Q --batch -l /home/joe/code/futon4/dev/check-parens.el --eval "(arxana-check-parens-cli)" -- --no-defaults ...`

## 1) Golden path run against live futon3a index

Command:

```bash
PATH=/tmp/futon3a-minilm-venv/bin:$PATH \
clojure -Sdeps '{:deps {futon3b/futon3b {:local/root "/home/joe/code/futon3b"}}}' \
  scripts/proof-p1-stepper-golden-path.clj
```

Observed top-5 neighbors:

### Q1: "Phi^4_3 measure equivalent to Gaussian free field measure"
1. `aif/expected-free-energy-scorecard` (0.2044)
2. `f6/p6` (0.1743)
3. `devmap-coherence/ifr-f7-upa-upekkha` (0.1498)
4. `devmap-coherence/baseline-freeze` (0.1461)
5. `library-coherence/library-embedding-refresh` (0.1210)

### Q2: "renormalization constants diverge under smooth shift Phi^4 3D"
1. `f6/p6` (0.2298)
2. `library-coherence/library-staleness-scan` (0.1997)
3. `devmap-coherence/ifr-f5-samadhi` (0.1522)
4. `ants/hunger-precision-coupling` (0.1474)
5. `devmap-coherence/next-steps-to-done` (0.1391)

### Q3: "mutual singularity interacting quantum field measure under shift"
1. `devmap-coherence/ifr-f5-samadhi` (0.1856)
2. `software-design/adapter-pattern` (0.1843)
3. `f3/p8` (0.1776)
4. `f2/p4` (0.1659)
5. `software-design/observer-pattern` (0.1654)

Assessment: no neighbors are materially about checking `\Phi^4_3` preconditions,
Cameron-Martin applicability boundaries, or measure singularity under shift.

## 2) Signal quality assessment (current index)

Result: **RED**

Reason: the live index does not surface even weakly relevant theorem-checking
patterns for this query class; nearest neighbors are unrelated project/design
patterns.

## 3) Math.SE/MO sample probe (small local index)

Because futon3a default index does not include these threads, a small local
sample index was built from:
- `futon6/data/stackexchange-samples/math.stackexchange.com__mathematical-physics.jsonl`
- `futon6/data/stackexchange-samples/mathoverflow.net__mathematical-physics.jsonl`
(100 threads total)

Same 3 queries against this sample produced weakly relevant QFT/renormalization
hits, e.g.:
- `mathoverflow.net:376332` — renormalization group procedure
- `mathoverflow.net:283518` — renormalization/EFT cutoff question
- `mathoverflow.net:94014` — mathematical foundations of QFT

But still no direct `\Phi^4_3`-shift-equivalence or Cameron-Martin framing
signal sufficient to catch P1 Step-1 automatically.

Assessment with this sample: weak/indirect signal only.

## 4) Calibration bucket

**RED** for current production corpus-check behavior.

Interpretation: to catch this error class reliably, index must include
specialized `\Phi^4_3`/regularity-structures content (ArXiv-level).

## 5) Minimum ArXiv seed set (if RED/YELLOW)

Minimum set proposed:

1. `arXiv:1303.5113`
   - M. Hairer, *A theory of regularity structures* (2014)
   - Why: renormalization structure and 3D singular behavior of `\Phi^4_3`.

2. `arXiv:1805.10814`
   - N. Barashkov, M. Gubinelli, *A variational method for `\Phi^4_3`* (2020)
   - Why: modern measure construction context; directly tied to mistaken `mu ~ mu_0` framing.

3. `arXiv:1210.2684`
   - M. Gubinelli, P. Imkeller, N. Perkowski, *Paracontrolled distributions and singular PDEs* (2015)
   - Why: concrete renormalization/control framework bridging formal manipulations and valid estimates.

These three are a minimal high-signal nucleus for this failure mode.

