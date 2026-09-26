# No-op bridges in the APM Lean corpus

Date: 2026-09-06

## Question and sweep boundary

This note records a mechanical sweep of all 448 files matching
`/home/joe/code/apm-lean/problems/*/lean/Main.lean`. It asks only whether a
target declaration delegates its entire proof to a declaration in the same
file whose proposition is the same. It does not assess whether either
proposition faithfully states the source problem.

The scanner divided each file at top-level Lean declarations and accepted two
proof shapes:

1. a `by` body containing only `exact localName arguments` after comments and
   blank lines were removed; or
2. a direct term body `:= localName arguments`.

For each candidate, it compared the target and local declaration statements
after collapsing whitespace. A second comparison canonicalised explicit and
implicit binder identifiers in declaration order, then substituted those
canonical names through the statement. Thus the sweep distinguishes literal
agreement from agreement that differs only in binder names. It rejected calls
to declarations outside the same file, multi-tactic bodies, and local
declarations whose normalised statements differed. The three matches below were
then inspected directly in their source files. No binder-name-only match was
found.

## Fully proved local declarations

These are no-op wrappers over declarations with complete proof bodies. They
add a named forwarding layer but conceal no current proof obligation.

| Problem | Target | Local declaration | Statement comparison | Proof shape | Assessment |
|---|---|---|---|---|---|
| `a00J06` | `radius_gt_one_of_summable`, `problems/a00J06/lean/Main.lean:162` | private `radius_gt_one_of_summable_aux`, `problems/a00J06/lean/Main.lean:64` | Literal after whitespace normalisation | `exact radius_gt_one_of_summable_aux ha he h`, line 165 | The private lemma contains the complete radius argument; the public lemma republishes its identical proposition. The visibility boundary may be a deliberate API choice, but it isolates no mathematical sub-step, so structurally it is still a no-op wrapper over a complete proof. |
| `a96J08` | `apm_a96J08`, `problems/a96J08/lean/Main.lean:726` | `apm_a96J08_bridge_1`, `problems/a96J08/lean/Main.lean:693` | Literal after whitespace normalisation | `exact apm_a96J08_bridge_1 ω hre_pos hre_lt`, line 759 | The bridge was a useful historical closer boundary: its docstring at lines 689–692 identifies the formerly missing principal-value evaluation. Its proof is now complete at lines 699–711, however, and its proposition is the target proposition itself. The final theorem therefore isolates no smaller present-day step and is a style defect, not a hidden gap. |
| `a93J02` | `alternating_harmonic_eq_log_two`, `problems/a93J02/lean/Main.lean:111` | `tendsto_sum_alternating_harmonic`, `problems/a93J02/lean/Main.lean:58` | Literal after whitespace normalisation | direct term `tendsto_sum_alternating_harmonic`, line 113 | The local theorem contains the complete alternating-series and Abel-limit proof at lines 59–108. The target is only a second name for the identical proposition. It is not named as a bridge and carries no separate interface or proof boundary, so this is also a style defect. |

Count: **3 target declarations** are no-op wrappers over fully proved local
declarations.

## Local declarations that still contain `sorry`

None. None of the three matched local declarations contains `sorry` or
`admit`. Comment-aware
audits in `problems/a00J06/status.json`, `problems/a96J08/status.json`, and
`problems/a93J02/status.json` also report zero current sorry declarations.

Count: **0 target declarations** are no-op wrappers over a local declaration
that still contains a `sorry`.

## Interpretation

The `a96J08` bridges-rejected history is consistent with a bridge that ceased
to separate a subproblem once its proof was filled, but this sweep cannot
establish the unrecorded historical reason. The corpus-wide result does not
support seven current hidden gaps of this exact form: it finds three forwarding
aliases, all over completed proofs, and no instance where a target appears
complete while an identical local bridge carries a `sorry` one name away.
