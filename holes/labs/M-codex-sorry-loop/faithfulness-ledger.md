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

| 5 | a01A03 | `convolution_identity_implies_indicator` | **DEGENERATE — vacuous either way, and the most dangerous entry so far because it still looks provable.** `g` is a parameter and the conclusion is POINTWISE (`∀ x, g x = …`), while `hidentity` constrains `g` only through integrals, i.e. only a.e. Ground control MACHINE-CHECKED (`/tmp/a01A03_probe.lean`, compiles exit 0) that the theorem IMPLIES no integrable `g` satisfies `hidentity`: modify a witness at `0`, integrals are unchanged by a.e. congruence, so the modified function is another witness whose pointwise conclusion fails. The converse is immediate (nothing satisfies the hypotheses → vacuously true), so **the theorem is EQUIVALENT to `hidentity` being unsatisfiable**. Either horn is bad: if some witness exists the theorem is FALSE; if none does it is TRUE but says nothing about indicators, and a runner could discharge it axiom-clean by deriving `False` from `hidentity` | make the conclusion an a.e. equality AND restrict the test-function class to one whose derivatives are integrable (e.g. smooth compactly supported). STATEMENT CHANGE → Joe's authorization | cron row hard-problems-a01a03, runner blocked correctly but on an incomplete reason — see note | 2026-07-29 |

| 6 | a01A04 | `ball_volume_superexponential_decay` (part c), `ball_volume_recursion` (part a) | **FALSE AS STATED — wrong ambient instance.** `Fin n → ℝ` carries Mathlib's **Pi sup-norm**, not the Euclidean norm, so `Metric.ball (0 : Fin n → ℝ) 1` is the open CUBE `(-1,1)ⁿ` of volume `2ⁿ`, not the Euclidean unit ball. Mathlib certifies this itself: `Real.volume_pi_ball` computes `(2r)^(card ι)`. Ground control MACHINE-CHECKED part (c) at `A = 1` (`/tmp/a01A04_refute.lean`, exit 0): the claim becomes `2ⁿ → 0`, refuted since `1 ≤ 2ⁿ`. Part (a) fails from the same root cause — at `n = 1` it asserts `4 = 2·∫₋₁¹√(1-t²) = π` (arithmetic not separately machine-checked; the shared root cause is). Part (b), about `ballSlice` alone, is genuinely valid | replace `Fin n → ℝ` with `EuclideanSpace ℝ (Fin n)` consistently in (a) and (c). Secondary: the `∃ f` in (a) is vestigial — the displayed equality hardcodes `ballSlice` instead of using the bound `f`. STATEMENT CHANGE → Joe's authorization | cron row hard-problems-a01a04, runner blocked and named both contradictions unprompted, 2026-07-29 |

**Class note (#6) — the fourth mechanism: a TYPE-LEVEL modelling
error.** The others are local slips (a forgotten `include`, a missing
`≠ ⊤`, a pointwise-vs-a.e. confusion). This one is different in kind:
the statement is well-formed, compiles, reads correctly in English,
and means something else entirely, because `Fin n → ℝ` silently
supplies the sup-norm.

**Scoped by measurement, not assumed (claude-9 corrected its own first
estimate here).** A first pass grepped for `Fin n → ℝ` near any of
`Metric.ball` / `‖·‖` / `dist` and flagged 5 files — an OVERCOUNT. Two
of those (a94J01, a02J01) take norms of VALUES (`‖f x‖` with
`f x : ℝ`) and use `volume` on `Fin n → ℝ`, which is the PRODUCT
Lebesgue measure and therefore exactly right for ℝⁿ. The defect is
specifically **metric geometry on POINTS of the domain type**. The
precise scan — ball/closedBall/dist applied to a `Fin n → ℝ` point —
returns **a01A04 alone, 3 sites**. The S9 rule must encode that
distinction or it will flag correct measure-theoretic code across the
corpus.

**And the corpus already knows about this defect.** `problems/a95A03`
carries a written §Statement repair: *"The auto-generated statements
used `Fin 3 → ℝ` for ℝ³. We repair to `EuclideanSpace ℝ (Fin 3)`"*,
citing the inner-product structure and Mathlib's ball-volume lemmas.
So the repair is precedented and sanctioned, and the fix for a01A04 is
the same move.

**The deeper implication, which outranks this row.** a95A03 names the
source: *auto-generated* statements. If the problem statements were
machine-generated, then the defect rate in the 69 remaining hard rows
is a property of THAT GENERATOR, not bad luck — which is why a static
scan is the right instrument and row-by-row discovery is the wrong
one. Ledger #3 (`variable` not included) and #6 (wrong ambient type)
are both exactly the kind of error a generator makes systematically
and a human reader does not.

**Class note (#5) — where the runner was right, and where review
added something.** The runner reached the correct DISPOSITION (block,
needs statement repair) and its a.e.-modification construction is the
same one used in the machine-checked probe. But its two stated reasons
are in TENSION and it did not reconcile them: reason 1 ("only a.e.
equality is provable, so the statement is false") presupposes a witness
EXISTS, while reason 2 ("`hidentity` quantifies over every
differentiable `f` without requiring an integrable derivative") is
evidence that NO witness exists — in which case the theorem is
vacuously TRUE, not false, and "blocked as false" would be the wrong
verdict. The sharper characterisation replaces both. **Operational
consequence, and the reason this entry matters more than #3/#4:** a
false statement is self-defending — nobody can prove it. A DEGENERATE
one is not. If this row is ever re-dispatched, a competent runner may
well discharge it axiom-clean by deriving `False` from `hidentity`,
and it would enter the corpus as a solved row. `:do-not-redispatch` is
therefore load-bearing here in a way it is not for #3 and #4.

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
