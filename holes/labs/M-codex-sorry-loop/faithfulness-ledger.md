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

| 4 | a01A02 | `weakL2_implies_L1_bound` | **FALSE AS STATED**, by a mechanism distinct from #3. The weak-`L²` hypothesis is written `(volume {x | t < \|f x\|}).toReal ≤ C / t ^ 2` with no finiteness side-condition, and Mathlib defines `ENNReal.toReal ⊤ = 0` — so a function whose superlevel sets have INFINITE measure satisfies the hypothesis vacuously. Ground control MACHINE-CHECKED the refutation: taking the statement as a hypothesis at `α = ℝ`, `f ≡ 1`, `C = 1` and applying it to `Icc 0 (C₁²+1)` derives `False` (compiles, exit 0, `/tmp/a01A02_refute.lean`). Note the file asserted at line 35 that "the theorem statement is correct and captures the mathematical problem" | state the weak estimate in `ENNReal` (`volume {x \| t < \|f x\|} ≤ ENNReal.ofReal (C / t ^ 2)`), or keep the real-valued form and add finiteness of every positive-threshold superlevel measure. STATEMENT CHANGE → Joe's authorization | cron row hard-problems-a01a02, runner REFUSED to commit and named the counterexample unprompted, 2026-07-29 |

**Class note (#4) — the `toReal`-of-`⊤` trap, and why it is the most
dangerous idiom found so far.** `ENNReal.toReal ⊤ = 0` is silent and
total: it makes an unbounded quantity read as zero rather than
erroring. Compare with #2: there an unconstrained quantity sat in the
CONCLUSION, making the theorem trivially TRUE; here it sits in a
HYPOTHESIS, making the theorem FALSE. Same root idiom, opposite
effect, and only the second is detectable by trying to prove it. Both
are greppable: `.toReal` applied to a measure inside a hypothesis
without an accompanying `≠ ⊤` / `< ⊤` is a scan rule, and it should
join the S9 checklist alongside stub definitions, unbound
existentials, and #3's `variable` inclusion.

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
