# E-apm-bundle-sorry-drift

**Opened 2026-08-18 by codex-6 for the Analyst. Discovery only: no problem
bundle or metadata was changed.**

## Result

`status.json` is not ground truth for open Lean work. Of the 443 problem
bundles having both `lean/Main.lean` and an integer
`lean.sorry_count_total`, a comment/string-aware lexical census found:

| result | bundles |
|---|---:|
| declared count agrees | 304 |
| declared count disagrees | 139 |
| declaration is higher | 74 |
| declaration is lower | 65 |
| executable `sorry` present | 228 |
| no executable `sorry` | 215 |
| metadata says open, lexical scan says closed | 72 |
| metadata says closed, lexical scan says open | 18 |

Thus drift is bidirectional, not just old positive counts left behind. A naive
standalone-word scan differs from the stripped scan in 36 files and counts 42
comment/string occurrences as work.

The census is reproducible from the repository root:

```sh
python3 -B scripts/apm_bundle_sorry_census.py
python3 -B scripts/apm_bundle_sorry_census.py --rows > /tmp/apm-sorry-rows.json
```

The population rule intentionally uses the actual `lean/Main.lean`, rather
than requiring the optional `lean.main` metadata field: some legacy statuses
omit that field despite containing the file. Configured Scratch files are also
counted; they contribute no executable sorries in this population.

## What “open” means

There are three different claims, with different authorities:

1. The supplied scanner removes Lean line comments, nested block comments,
   and strings, then counts standalone `sorry` tokens. This is ground truth for
   the narrower question “does source contain an executable placeholder?” It
   is cheap: the full 443-bundle pass takes about 0.4 seconds here.
2. Lean has no independent parse-only command in this toolchain. Its frontend
   parsing is exercised by elaboration. In the successful elaboration sample,
   compiler `declaration uses 'sorry'` diagnostics agreed with the stripped
   lexical classification: one warning for each sampled open file and none for
   each sampled closed file.
3. `lake env lean` under the pinned project is authoritative for **frame
   eligibility**. Only it distinguishes “zero sorries and compiles” from “zero
   sorries but is broken.” A candidate needs both a successful elaboration and
   genuine remaining proof work.

The measured elaboration command (output goes to `/tmp`, leaving no bundle
artifact) is:

```sh
cd /home/joe/code/apm-lean
/usr/bin/time -f 'ELAPSED=%e MAXRSS_KB=%M EXIT=%x' \
  lake env lean -o /tmp/apm-a02J03-audit.olean \
  problems/a02J03/lean/Main.lean
```

Across 42 measured invocations the mean was 2.72 seconds, median 2.55,
minimum 0.93, maximum 7.48, with peak RSS 6,727,420 KiB. A sequential full
pass therefore projects to about 20 minutes. It is feasible; wide parallelism
is inappropriate at the observed memory peak. This packet sampled rather than
running all 443 elaborations.

The sample exposed the important counterexample: `a94A09` has zero executable
`sorry` tokens but fails immediately because
`ConstructionTargets.SchwarzPick.olean` is absent. Lexically it looks closed;
authoritatively it is broken, not completed.

## `a02J03`

`a02J03` declares 3 sorries but contains zero executable sorries. It elaborates
successfully under the pinned project in 2.17 seconds (peak 3,347,844 KiB),
with only an unused-variable warning and no sorry diagnostic. It therefore has
**no open proof work** and is not a valid frame-9 solve target.

## Confirmed open-work shortlist

These sampled bundles both elaborate successfully and emit a real sorry
diagnostic:

`a01J05`, `a01J06`, `a03J05`, `a94A02`, `a97J05`, `a98A07`, `b01J03`,
`b93J03`, `t94J09`.

That establishes a nonempty target pool, but it does not by itself establish
the separate preregistration claim that any particular theorem is known
doable. Two other compiling/open bundles were deliberately omitted from the
shortlist because their classifications already say
`partial-invalid-statement` (`a96A06`) or `statement-defective` (`m93J05`).

The sample also confirmed five dangerous false-closed cases: `a94A07`,
`a96A08`, `a97J07`, `b00J02`, and `b97J01` declare zero while Lean reports a
sorry-containing declaration.

## Classification coupling (do not bulk-repair yet)

Classification is derived from the count in active import paths:

- `scripts/import-proof-frame.py` counts with raw `text.count("sorry")` and
  chooses `partial` versus `complete` from that count (lines 40, 348–362).
- `scripts/reconcile-legacy-lean-proofs.py` does the same and derives the same
  classification (lines 58, 258–271).
- `scripts/import-harvested-informal-proofs.py` writes zero with
  `informal-only`; the inventory script reads these records but does not repair
  this drift.

Consequently, replaying an importer while correcting counts can silently
reclassify problems and alter the experimental target pool. Counts and
classification need an explicit joint ruling; this excursion changes neither.

The `math-strategy/corpus-trust-protocol` warning applies literally here:
advertised corpus labels are hypotheses. Frame selection should require a
fresh pinned-build elaboration plus an observed executable hole, not accept
`status.json` as evidence of either.
