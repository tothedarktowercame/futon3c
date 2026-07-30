# Statement-faithfulness ledger (announcement gate 2)

Formal statements whose discharge does NOT constitute solving the
informal problem. Opened 2026-07-29 on the first wild instance. Every
entry: what the formal statement actually says, why it diverges, and
what genuine closure requires. The corpus-wide audit (every statement
vs original exam text) runs before any announcement; this ledger
collects instances found en route.

## WHERE THE DEFECTS COME FROM (added 2026-07-30, on Joe's question)

Joe asked the right question about a01A06: the exam asks *"what can
you say about the measure"* — how does "the target theorem is false"
answer that? Tracing every blocked row back through
`problem.md` (original exam TeX) and `informal-solution.md` gives a
**three-way split**, and it matters because the three have completely
different remedies.

**A. TRANSLATION defects — the mathematics was already correct and
written down; the formaliser broke it. 4 of 6.**
- **a00J05** — `variable (hK_compact) (hK_measure)` never included in the declarations.
- **a01A02** — informal solution states the hypothesis as `m({|f|>λ}) ≤ Cλ⁻²`, a MEASURE bound, and requires `0 < m(E) < ∞`. The formaliser encoded it as `(volume …).toReal ≤ C/t^2`, and `toReal ⊤ = 0` is what makes it false.
- **a01A03** — informal solution says `g = χ_{[-h,h]}` **a.e.**, twice explicitly (lines 11, 21). The formaliser wrote `∀ x, g x = …`, strengthening a.e. to pointwise.
- **a01A04** — `Fin n → ℝ` where a Euclidean space was meant (and a95A03 already documents this exact repair).

For these, repair is MECHANICAL and needs no new mathematics: restate
the Lean faithfully to the informal solution.

**B. INFORMAL-SOLUTION defect — the mathematics itself is wrong. 1 of 6.**
- **a01A06.** The informal solution derives `m = O(1/λ)`, then `O(1/λ²)`, and states outright *"O(1/λ²) … is **not** enough for L²!"* — then overrides itself with an invalid step: it lower-bounds `(e^{λ/2}−1)δ > 1 − e^{-λ/2}` and uses that as an UPPER bound to conclude `log < log 2`. Assuming `δ` large makes both sides of the constraint large, so no contradiction follows. It then asserts exponential decay and "Yes, g ∈ L²". The formaliser was FAITHFUL to a wrong answer. Correct answers: (a) an entropy/`L log L` bound, `δ·log(1/δ) ≲ A'/λ`, i.e. roughly `m ≲ 1/(λ log λ)`; (b) **NO** — `g = x^{-3/4}` on `(0,1]` is in `L log L` but not `L²`. The exam's *"Why or why not?"* was signposting a negative answer.

**C. EXAM defect — the original problem asks for something false. 1 of 6.**
- **a01A05.** `problem.md` itself says *"Show that |f_m − e_m| ≤ 2⁻ᵐ"*, and that is narrowly false (margin ≈ 0.018 at the first index; it still fails under 1-indexing, ≈ 0.252 vs 0.25). No `informal-solution.md` exists for this row. The intended claim is presumably `O(2⁻ᵐ)`.

**CONSEQUENCE — a better S9 than a grep.** Every bundle contains both
a formal statement AND an informal solution stating the intended
theorem. **Reconciling those two catches all four class-A defects**,
including a01A03's a.e.→pointwise and a01A02's `toReal`, which NO
static grep would find. That is a stronger and cheaper instrument than
the syntactic scan I recommended yesterday, and it is work a model can
do by reading. The grep rules remain useful as a fast first pass.

**CONSEQUENCE FOR THE ANNOUNCEMENT GATE.** The gate's
statement-faithfulness audit must reconcile against the ORIGINAL EXAM
TEXT, not the informal solution — a01A06 shows the informal corpus can
be confidently wrong, and a01A05 shows the exam can be wrong too, in
which case the corrected claim must be stated openly rather than
quietly formalised.

| # | Problem | Declaration | Divergence | Genuine closure needs | Found |
|---|---------|-------------|------------|----------------------|-------|
| 1 | a95J03 | `winding_number_bounded` | `windingNumber` is a stub `:= 0` (documented in-file); theorem discharges trivially via `omega` | winding-number/argument-principle infrastructure (same frontier as Rouché: `zeroCountInClosedBall-homotopy-invariant` family) | cron row sorry-0285, runner self-flagged, receipt in store 2026-07-29 |
| 2 | a95J04 | `coefficients_bounded_of_simple_poles` | conclusion is `∃ M, 0 ≤ M` — `M` never bound to `f`/coefficients; trivial witness `M = 0` discharges (divergence class: **under-constrained statement**, distinct from #1's stub definition) | restate with the real bound, then pole-subtraction + Cauchy estimates (in-file docstring: "not in Mathlib") | cron row sorry-0288, runner self-flagged, receipt 61d889c8 2026-07-29 |

| 3 | a00J05 | `cauchyTransform_not_extendable_to_entire`, `cauchyTransform_differentiableAt` | **FALSE AS STATED** — a new class, and the most serious yet. `variable (K) (hK_compact) (hK_measure)` at line 54 are NOT auto-included: only `K` is mentioned in the declarations, so neither hypothesis reaches the statements. Ground control re-derived the real signatures by its own `#check` and then MACHINE-CHECKED the refutation: taking the theorem's statement as a hypothesis and instantiating `K = ∅`, `g = 0` yields `False` (compiles, exit 0, `/tmp/a00J05_refute.lean`). Unlike #1 and #2, which proved *less* than they appeared to, this cannot be proved at all | restore the hypotheses to the declarations (explicit parameters or `include hK_compact hK_measure`) — a STATEMENT CHANGE, so it needs Joe's authorization; then the real analytic content | cron row hard-problems-a00j05, runner REFUSED to commit and named the counterexample unprompted, 2026-07-29 |

| 4 | a01A02 | `weakL2_implies_L1_bound` | **FALSE AS STATED**, by a mechanism distinct from #3. The weak-`L²` hypothesis is written `(volume {x | t < \|f x\|}).toReal ≤ C / t ^ 2` with no finiteness side-condition, and Mathlib defines `ENNReal.toReal ⊤ = 0` — so a function whose superlevel sets have INFINITE measure satisfies the hypothesis vacuously. Ground control MACHINE-CHECKED the refutation: taking the statement as a hypothesis at `α = ℝ`, `f ≡ 1`, `C = 1` and applying it to `Icc 0 (C₁²+1)` derives `False` (compiles, exit 0, `/tmp/a01A02_refute.lean`). Note the file asserted at line 35 that "the theorem statement is correct and captures the mathematical problem" | state the weak estimate in `ENNReal` (`volume {x \| t < \|f x\|} ≤ ENNReal.ofReal (C / t ^ 2)`), or keep the real-valued form and add finiteness of every positive-threshold superlevel measure. STATEMENT CHANGE → Joe's authorization | cron row hard-problems-a01a02, runner REFUSED to commit and named the counterexample unprompted, 2026-07-29 |

| 5 | a01A03 | `convolution_identity_implies_indicator` | **DEGENERATE — vacuous either way, and the most dangerous entry so far because it still looks provable.** `g` is a parameter and the conclusion is POINTWISE (`∀ x, g x = …`), while `hidentity` constrains `g` only through integrals, i.e. only a.e. Ground control MACHINE-CHECKED (`/tmp/a01A03_probe.lean`, compiles exit 0) that the theorem IMPLIES no integrable `g` satisfies `hidentity`: modify a witness at `0`, integrals are unchanged by a.e. congruence, so the modified function is another witness whose pointwise conclusion fails. The converse is immediate (nothing satisfies the hypotheses → vacuously true), so **the theorem is EQUIVALENT to `hidentity` being unsatisfiable**. Either horn is bad: if some witness exists the theorem is FALSE; if none does it is TRUE but says nothing about indicators, and a runner could discharge it axiom-clean by deriving `False` from `hidentity` | make the conclusion an a.e. equality AND restrict the test-function class to one whose derivatives are integrable (e.g. smooth compactly supported). STATEMENT CHANGE → Joe's authorization | cron row hard-problems-a01a03, runner blocked correctly but on an incomplete reason — see note | 2026-07-29 |

| 6 | a01A04 | `ball_volume_superexponential_decay` (part c), `ball_volume_recursion` (part a) | **FALSE AS STATED — wrong ambient instance.** `Fin n → ℝ` carries Mathlib's **Pi sup-norm**, not the Euclidean norm, so `Metric.ball (0 : Fin n → ℝ) 1` is the open CUBE `(-1,1)ⁿ` of volume `2ⁿ`, not the Euclidean unit ball. Mathlib certifies this itself: `Real.volume_pi_ball` computes `(2r)^(card ι)`. Ground control MACHINE-CHECKED part (c) at `A = 1` (`/tmp/a01A04_refute.lean`, exit 0): the claim becomes `2ⁿ → 0`, refuted since `1 ≤ 2ⁿ`. Part (a) fails from the same root cause — at `n = 1` it asserts `4 = 2·∫₋₁¹√(1-t²) = π` (arithmetic not separately machine-checked; the shared root cause is). Part (b), about `ballSlice` alone, is genuinely valid | replace `Fin n → ℝ` with `EuclideanSpace ℝ (Fin n)` consistently in (a) and (c). Secondary: the `∃ f` in (a) is vestigial — the displayed equality hardcodes `ballSlice` instead of using the bound `f`. STATEMENT CHANGE → Joe's authorization | cron row hard-problems-a01a04, runner blocked and named both contradictions unprompted, 2026-07-29 |

| 7 | a01A05 | `gramSchmidt_approximation` | **FALSE AS STATED — and unlike #3/#4/#6 this is a genuine MATHEMATICAL error, not a mechanical one.** The statement asserts `‖fₘ − eₘ‖ ≤ 2⁻ᵐ` from a per-PAIR hypothesis `\|⟨fₙ,fₘ⟩\| ≤ 2⁻ᵐ (n<m)`, but Gram–Schmidt error ACCUMULATES over all preceding vectors, so a per-pair bound cannot yield the same bound on the accumulated error. Minimal witness, in `ℓ²(ℕ)`: `f₀ = e₀`, `f₁ = ½e₀ + (√3/2)e₁`, `fₙ = eₙ (n≥2)`. All norms are 1; the hypothesis is satisfied — `⟨f₀,f₁⟩ = ½ = 2⁻¹` EXACTLY, i.e. admissible at the knife edge. Gram–Schmidt at m=1 returns `e₁`, and `‖f₁ − e₁‖ = √(2−√3) ≈ 0.5176 > 0.5 = 2⁻¹`. Ground control verified the structure by hand and MACHINE-CHECKED the deciding inequality `√(2−√3) > ½` (`/tmp/a01A05_num.lean`, exit 0); the `ℓ²` construction itself was NOT formalised (it needs infinitely many dimensions — no finite-dimensional witness exists, since near-orthogonality of the tail requires infinite dimension) | strengthen the pairwise hypothesis, or weaken the conclusion to an accumulated bound (e.g. `∑ₖ<ₘ 2⁻ᵏ`-style). STATEMENT CHANGE → Joe's authorization | cron row hard-problems-a01a05, runner blocked and constructed the counterexample unprompted, 2026-07-29 |

| 8 | a01A06 | `orlicz_bound_implies_L2` AND `distribution_exponential_decay` | **BOTH FALSE AS STATED** — same family as #7, a genuine mathematical error. `OrliczBound` is `L log L` / ENTROPY control (Orlicz duality against the class `∫eᶠ ≤ 1`); both conclusions assert EXPONENTIAL-TAIL control, which entropy control does not imply. Counterexample `g(x) = x^(-3/4)` on `(0,1]`. Machine-checked: `∫₀¹ x^(-3/4) = 4` (so `g ∈ L¹` as required) and `x^(-3/2)` is NOT integrable on `Ioo 0 1` via `integrableOn_Ioo_rpow_iff` — the latter IS the refutation of the `L²` claim. Hand-verified (flagged as such): Young–Fenchel `gf ≤ eᶠ + g log g − g` gives `∫gf ≤ 1 + 12 − 4 = 9 < 10`, so `OrliczBound g 10` holds; and `μ{g>λ} = λ^(-4/3)`, which no `Ce^{-αλ}` bounds | strengthen to genuine exponential integrability of `g`, or weaken both conclusions to `L log L`/entropy and POLYNOMIAL-tail statements. STATEMENT CHANGE → Joe's authorization | cron row hard-problems-a01a06, runner found it by semantic inspection BEFORE implementing, 2026-07-29 |

| 9 | a01A07 | `tendstoUniformlyOn_of_L1_on_disks` | **FALSE AS STATED — third instance of the `variable`-not-included mechanism (with #3 a00J05 and the a01A11 defect), and the one that caught GROUND CONTROL out.** `variable (hΩ : IsOpen Ω)` is never referenced by this declaration, so Lean drops it. Counterexample, machine-checked (`/tmp/a01A07_refute.lean`, exit 0): `Ω = {0}`, `f n = 1`, `F = 0`, `K = {0}`. `L1ConvergesOnDisks` quantifies over closed balls of POSITIVE radius contained in `Ω`, and a singleton contains none, so it holds vacuously; the conclusion then asserts uniform convergence of the constant `1` to `0`. S9 returned `:ok` for this row, AND ground-control review confirmed that `:ok` as "defensible" — both were wrong; only a proof attempt found it | add `hΩ : IsOpen Ω` to the declaration. STATEMENT CHANGE → Joe | cron row hard-problems-a01a07, runner blocked and gave the counterexample unprompted, 2026-07-30 |

**Class note (#9) — the rule this row forces, and the review failure behind it.** Ground control's own mechanical sweep FLAGGED a01A07 for a section `variable` Prop hypothesis, and ground control then dismissed it by reasoning: "`differentiableOn_of_L1_limit` does include `IsOpen Ω`, and the uniform-convergence lemma plausibly does not need it." That was an unverified plausibility argument overriding a correct mechanical signal.

**RULE, now binding: presence of the variable-inclusion defect is MECHANICAL and decidable by inspection; whether it makes the statement FALSE is NOT, and needs a proof attempt. Flag every instance. Never dismiss one because the hypothesis looks unnecessary.**

**Refinement that makes the scan precise rather than noisy** (from sweeping all 61 untouched rows): the defect is dangerous exactly when the CONSTRAINED VARIABLE appears in the statement but its Prop constraint does not. If neither appears the omission is harmless — `a97A07`'s `apm_a97A07_Q_polynomial_in_z` omits `hR` but never mentions `R` either, so it is fine. That refined rule flags a00J05, a01A07 and a01A11 and clears a97A07: 3 for 3. **Remaining exposure in the untouched tail is exactly ONE row, a01A11.**

**Class note (#8) — the greppable/non-greppable split now stands at
4–2.** #7 and #8 are both HYPOTHESIS-STRENGTH MISMATCHES: the
hypothesis controls one functional quantity and the conclusion asserts
a strictly stronger one (pairwise vs accumulated; entropy vs
exponential tail). Neither is syntactically detectable — both files
are well-typed, well-scoped and internally coherent. This is now a
*named family*, not two one-offs, and it is the family S9 cannot
reach. Worth noting how #8 was found: the runner detected it by
SEMANTIC INSPECTION before writing any Lean, so error-recall never
fired. A cheaper triage than a full proof attempt may therefore exist
for this family — an inspection pass that asks only "what does the
hypothesis actually control, and is the conclusion of that same
strength?" — but it needs a model reading mathematics, not a grep.

**Class note (#7) — THE HONEST LIMIT ON THE S9 RECOMMENDATION.**
Mechanisms #3, #4 and #6 are mechanical and greppable: a `variable`
not included, a `.toReal` with no `≠ ⊤`, a `Fin n → ℝ` used as a
Euclidean space. A static scan finds all three for free. **#7 is not
like that.** Nothing about `gramSchmidt_approximation` is
syntactically suspicious — it is well-typed, well-scoped, uses the
right ambient structures, and is simply MATHEMATICALLY FALSE by a
margin of about 0.018. Finding it required attempting the proof and
constructing a knife-edge counterexample in an infinite-dimensional
space. So S9 reduces the cost of the tail; it does not eliminate the
need to attempt rows. The realistic split on today's evidence: of 5
statement defects, 4 were greppable and 1 was not — a scan would have
caught most of the waste, and a runner is still required for the rest.
Anyone quoting the S9 case should quote this limit with it.

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
