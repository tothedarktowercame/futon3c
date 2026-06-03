# CODEX-HANDOFF — mathlib Mission→Pattern tower reconstruction (git-only, read-only)

**From:** claude-6 (M-differentiable-code / E-the-dark-tower) · **To:** Codex · **Date:** 2026-06-02
**Venue:** `~/code/mathlib4` (blobless clone, already present) · **Discipline:** READ-ONLY git log. No checkout, no fetch, no write in the clone. Descriptive, baseline-aware.

## Why
`E-the-dark-tower` (`futon5a/holes/excursions/`) claims the FUTON evolution structure
(turns→iching = 1st-order, missions/patterns→iiching = 2nd-order) is an **exotype** that should
re-appear in a second substrate. We confirmed the 1st-order rung transfers (grows-at-edges:
FUTON −0.357 ≈ mathlib −0.340). This handoff reconstructs the **mission→pattern layer** that is
latent in mathlib's git log, so the 2nd-order claim can later be tested on real data.

## What's recoverable (verified on last 500 commits)
mathlib commits are squash-merged conventional commits: `type(Area/Path): desc (#PR)`.
- **PR `#NNNN` = mission** — 500/500 recent commits carry one (fully recoverable).
- **type prefix `feat|fix|refactor|chore|ci|doc|bump|style|perf|test` = pattern** (the move-type vocabulary).
- **`(Area/Path)` scope = location** in the manifold (Algebra, CategoryTheory, Analysis, …).

## Steps (all `git -C ~/code/mathlib4 log`, no working-tree ops)
1. **Parse the log.** `git log --format=%H%x09%ct%x09%s` over full history. For each subject, regex
   `^(\w+)(?:\(([^)]*)\))?:\s*.*?(?:\(#(\d+)\))?\s*$` → `(type, area, pr, ct)`. Drop merge commits
   (`Merge pull request`) into a separate count. Emit `/tmp/mathlib-commits.json`.
2. **Distributions (descriptive).** type histogram; area histogram (top-level segment of `area`);
   commits-per-PR (group by `#NNNN`) — report the multi-commit-PR fraction (these are the genuine
   multi-step "missions" vs single-commit ones).
3. **type × core/rim cross-tab — the descriptive form of the hypothesis.** Reuse, do NOT recompute:
   - `/tmp/mathlib-birth.json` (module birth_ct; built by `futon5/tools/embed/mathlib_morphology.py`).
   - import in-degree per module from the same script (the `indeg` Counter — lift it out or import it).
   For each commit, map its `area` to touched module(s) (use `--name-only` on a sample if needed,
   read-only). Label each module **core** (in-degree ≥ median) vs **rim** (< median). Cross-tabulate
   **commit-type × {core, rim}**. Report as a contingency table with a base-rate baseline: does `feat`
   skew **rim** and `refactor`/`chore` skew **core** *beyond* the base rate of rim-vs-core modules?
   Report a chi-square / lift-over-base-rate — NOT a predictive AP (per the descriptive-not-predictive
   convergence; see E-the-dark-tower §"consequence").
4. **Emit** a short markdown result next to this file: the three distributions + the type×position
   table + the one-line verdict (does feat→rim / refactor→core hold descriptively, with effect size).

## Guardrails
- Read-only: `git log` / `git show` only. The clone is shared; **no `git checkout`**.
- Full-history log walk is ~32k commits; it's fast (metadata only). Don't `--reverse` (no buffering).
- This is the **descriptive reconstruction only**. The *theoretical* test (feat=iching/edge-growth,
  refactor=iiching/curvature-on-core as the dark-tower's two rungs) is **gated** on pattern-library
  work on the FUTON side (iching Voronoi retract + iiching changes-of-changes articulation) — do not
  assert the tower interpretation; just deliver the table and let the effect size speak.

## Hand-back
Drop the result md here and ping claude-6 via the Agency bell (`localhost:7070`) or leave it for Joe.
