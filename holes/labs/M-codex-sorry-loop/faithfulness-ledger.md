# Statement-faithfulness ledger (announcement gate 2)

Formal statements whose discharge does NOT constitute solving the
informal problem. Opened 2026-07-29 on the first wild instance. Every
entry: what the formal statement actually says, why it diverges, and
what genuine closure requires. The corpus-wide audit (every statement
vs original exam text) runs before any announcement; this ledger
collects instances found en route.

| # | Problem | Declaration | Divergence | Genuine closure needs | Found |
|---|---------|-------------|------------|----------------------|-------|
| 1 | a95J03 | `winding_number_bounded` | `windingNumber` is a stub `:= 0` (documented in-file); theorem discharges trivially via `omega` | winding-number/argument-principle infrastructure (same frontier as Rouché: `zeroCountInClosedBall-homotopy-invariant` family) | cron row sorry-0285, runner self-flagged, receipt in store 2026-07-29 |
| 2 | a95J04 | `coefficients_bounded_of_simple_poles` | conclusion is `∃ M, 0 ≤ M` — `M` never bound to `f`/coefficients; trivial witness `M = 0` discharges (divergence class: **under-constrained statement**, distinct from #1's stub definition) | restate with the real bound, then pole-subtraction + Cauchy estimates (in-file docstring: "not in Mathlib") | cron row sorry-0288, runner self-flagged, receipt 61d889c8 2026-07-29 |

| 3 | a00J05 | `cauchyTransform_not_extendable_to_entire`, `cauchyTransform_differentiableAt` | **FALSE AS STATED** — a new class, and the most serious yet. `variable (K) (hK_compact) (hK_measure)` at line 54 are NOT auto-included: only `K` is mentioned in the declarations, so neither hypothesis reaches the statements. Ground control re-derived the real signatures by its own `#check` and then MACHINE-CHECKED the refutation: taking the theorem's statement as a hypothesis and instantiating `K = ∅`, `g = 0` yields `False` (compiles, exit 0, `/tmp/a00J05_refute.lean`). Unlike #1 and #2, which proved *less* than they appeared to, this cannot be proved at all | restore the hypotheses to the declarations (explicit parameters or `include hK_compact hK_measure`) — a STATEMENT CHANGE, so it needs Joe's authorization; then the real analytic content | cron row hard-problems-a00j05, runner REFUSED to commit and named the counterexample unprompted, 2026-07-29 |

**Class note (#3):** #1/#2 are *weak* statements; #3 is an *unprovable*
one. The queue has no category for this: a row whose sorry can never be
discharged without a statement change is not "hard", it is blocked on an
owner decision, and leaving it `:untouched` would send runners at it
again. This is also the first Lean-4 `variable`-inclusion defect found in
the corpus — a mechanical, greppable failure mode, and a strong candidate
for the S9 static scan (any declaration in a section whose `variable`
hypotheses are not mentioned or `include`d).

Census note: the S2 census counted 0 `:statement-issue` rows — stub
DEFINITIONS under honest-looking theorems were not in its taxonomy.
A placeholder-definition scan (grep for `:= 0`/`default`-style stub
defs feeding sorried/discharged theorems) is a candidate S9 QA slice.
