# T-lean-authority-alias — mathlib4 was aliased into APM's package authority, and the original is preserved

**Opened:** 2026-08-27 · claude-13, reviewing codex-22's diagnosis
(`invoke-1787830674542-2326-6e7822bb`). **Decision pending with Joe. Nothing has
been mutated.**

## The finding

`mathlib4/.lake/packages` is a symlink to `/home/joe/code/apm-lean/.lake/packages`,
created **2026-08-26 22:01** — mid-session, yesterday. Beside it sits the
directory it replaced:

    mathlib4/.lake/packages.pre-canonical-20260826/   (Aug 14 15:59)

That set-aside directory holds **exactly what mathlib4's manifest asks for**:

| | set-aside dir | shared APM authority | mathlib4 manifest wants |
|---|---|---|---|
| `batteries` toolchain | **v4.31.0-rc1** | v4.29.0-rc8 | v4.31.0-rc1 |
| `batteries` HEAD | **708b0578** | bce25af7 | **708b0578** |
| compiled `Logic.olean` | present, Aug 14 16:00 | Aug 26 22:04 (4.29) | — |

All eight packages are present (Cli, LeanSearchClient, Qq, aesop, batteries,
importGraph, plausible, proofwidgets).

## Why this changes the cost

codex-22's diagnosis is correct and its option B (a separate authority for
mathlib4) is the right direction. Two corrections to its costing:

1. **`AGENTS.md` does not cover `mathlib4`.** `grep mathlib4 AGENTS.md` returns
   nothing. The rule is scoped *"in an APM frame worktree"*, under the heading
   *"Lean frame worktrees and Mathlib substrate"*. `mathlib4` is the Mathlib
   source checkout (branch `darktower`, holding `DarkTower/`), not a frame
   worktree. So option B does **not** require a policy change — it requires
   recognising the rule's existing scope. The directory name
   *`pre-canonical`* records the intent: someone believed they were bringing
   `mathlib4` into line with the canonical authority. An understandable
   over-application.
2. **The repair is a revert, not a provisioning job.** codex-22 costed B at
   15–90 minutes plus fetch/build. The 4.31 dependency set with compiled
   artifacts has been on disk since Aug 14. The repair is a rename.

## Proposed repair — Joe's call, not executed

Restore the set-aside directory as `mathlib4/.lake/packages` and remove the
alias. Then re-probe:

    import Mathlib.Probability.Kernel.Category.Stoch

**Containment until then:** do not run Lake from `mathlib4` on any file that
imports anything. Mathlib-free files are unaffected.

## What is not at risk

**Every Lean gate run on 2026-08-26/27 was on a Mathlib-free file** —
`GainChain.lean` and `CommitmentTemperature.lean` import nothing, by the light
standard's rule. So the nine theorems verified yesterday are unaffected by the
flip-flop, and the Tier-0 handoffs can proceed while this is pending, provided
they keep that rule.

That is the constraint earning its keep: `NOTE-light-formalisation-standard.md`
requires standalone files because they elaborate in seconds. It also made
today's verification immune to a broken package authority.

## Also worth recording

- **The bug was demonstrated during its own diagnosis.** A `lake env lean` probe
  from `mathlib4` at ~11:30 on 2026-08-27 is the likely cause of the 11:32
  reconciliation codex-22 observed. Each invocation from either side flips the
  shared directory toward that side's manifest.
- **21 APM-side symlink consumers** point at the authority (codex-22's count,
  not independently verified): live frame pairs f36/f38/f39/f43/f45/f47, two
  generated solver frames, `apm-lean-library-loop-t00J02-solver`, and six
  `apm-lean-t00J02-proto-*` worktrees. Any repair that moves the *authority*
  touches all of them; the revert proposed here touches none, because it moves
  `mathlib4` off the authority rather than moving the authority.
