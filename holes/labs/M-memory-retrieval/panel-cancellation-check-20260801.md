# E2 panel cancellation pre-check — 2026-08-01

## Result

The named source does **not** contain LB/IN adjudications. Its own report says:
“No adjudication was performed.” I therefore audited the seven concrete
used-memory pairs supported by the frozen JSONL and labelled them
`unadjudicated-used-candidate`; I did not manufacture LB/IN labels.

No pair clears the bar for **strong** inclusion. Four are excluded because the
operative content is already in the pre-solution tree. Three a95J08 pairs remain
weak: two non-searchable scope guards lack a historical memory-free failure,
while the mathematical memory is route-relative and points toward the Jensen
route that failed three times rather than the Hölder route that closed.

| problem | memory | reachability | prior failure without? | recommendation | registration `:score-varies?` text |
|---|---|---|---:|---|---|
| a01A07 | `lift-the-circle-submean-bound-to-a-disk-area-bound` | `reachable-cheaply` | yes | **exclude** | No: the base file already contains the proved circle and polar bridge declarations and names the same API, so withholding the memory does not withhold its operative content. |
| a01A07 | `upgrade-diskwise-L1-convergence-to-local-uniform-convergence` | `reachable-cheaply` | yes | **exclude** | No: the base theorem docstring spells out the local smaller-disk route, the uniform-Cauchy step, and the exact packaging declaration. |
| a02J05 | `derive-a-sinc-tail-identity-by-differentiating-cosine-over-x` | `reachable-cheaply` | yes | **exclude** | No: both the named identity and the derived uniform tail theorem are already proved in the base file. |
| a02J05 | `remove-Abel-regularization-from-the-Dirichlet-sinc-integral` | `reachable-cheaply` | yes | **exclude** | No: the base file's remaining-obstruction comment gives the same Abel evaluation and uniform-removal plan. |
| a95J08 | `bound-automatic-frontier-descent-when-a-leaf-recurses` | `unreachable` | no | **weak** | Possibly: the scope guard is absent from the base tree and Mathlib, but historical data provide no memory-free failure and it governed stopping rather than the successful proof route. |
| a95J08 | `bound-the-interface-adapter-heuristic-with-genuine-construction-cases` | `unreachable` | no | **weak** | Possibly: its decision rule is not repository-searchable, but it only prevented a category error and did not supply the Hölder construction that eventually closed the theorem. |
| a95J08 | `prove-general-probability-kernel-Lp-contraction-by-integral-Young` | `reachable-with-route-knowledge` | yes | **weak** | Yes, but direction is not preregistered safely: the base informal solution already exposes Hölder, while the memory steers toward Jensen; the eventual first-try close used Hölder and Mathlib's route-specific lintegral_mul_norm_pow_le. |

`a93J07` is unassessable from the named artifacts: it has no used-memory row in
the candidate JSONL. Its five surfaced memories were recorded as ignored. This
is a panel-construction gap, not evidence that any invented pair is incidental.

## Method and boundary

- Each revision was exported with `git archive` into a temporary directory;
  the live worktree was not changed and the directory was deleted on exit.
- The script searched only the target problem tree plus the installed Mathlib
  source for imported API content. It did not search future git objects or
  futon3c prose for reachability.
- Memory bodies were verified by read-only GETs on port 7073. No dispatch or
  store-write endpoint was called.
- Frozen input hashes: `candidate` `1a4e0ee9b3349fbe15f0d24c17cd33f5551d12232f1deac1607c2c2473c788c8`, `candidate-report` `1eb5cf97c2f40121f5e56734294b31d00594d49bb641d3c80cd7504b08a2c573`, `receipts` `0cc527e23c3678a4cc7d8053d6636d0cde556dab15fcc3ce69bedf0b659820b3`, `queue` `d6b61d04a90db54fb7e6f48bb38c414318ff0de6abfdfb57d6b9081107a5a3a7`, `staging` `5392f6262fb36a3b0a68e111fbe4bc4e52fda33103e80ee2ecaaead907c9a914`, `causal-spec` `a6d21a855d8de9d81f2981fe947a825c20f7308e4d60f4229cba05418590909c`.
- `:reachable-cheaply` means the base problem names the operative theorem or
  route under an obvious query. `:reachable-with-route-knowledge` follows G6:
  the hit exists, but only vocabulary from the successful route exposes it.
  `:unreachable` means the operative content was absent under direct and close
  paraphrase queries in both allowed surfaces.
- V15 `repo_search` is the cancellation channel: a reachable pair is excluded
  when withholding the memory does not withhold the information.

## Evidence chains

### 1. a01A07 × `e-codexpilot-lift-the-circle-submean-bound-to-a-disk-area-bound`

Memory content (read-only `GET http://127.0.0.1:7073/api/alpha/evidence/e-codexpilot-lift-the-circle-submean-bound-to-a-disk-area-bound`): Use the proved circle sub-mean inequality and polar coordinates/Fubini to bridge circle averages to the translated disk-area sub-mean bound.

Base-tree search:

```text
$ git archive 81dccb3 | tar -x -C "$SNAPSHOT"
$ rg -n -i 'norm_le_circleAverage_norm|integral_closedBall_zero_eq_polar|integral_comp_polarCoord_symm|circleAverage' "$SNAPSHOT/problems/a01A07"
lean/Main.lean:77:lemma norm_le_circleAverage_norm
lean/Main.lean:80:    ‖g z₀‖ ≤ Real.circleAverage (fun w ↦ ‖g w‖) z₀ r := by
lean/Main.lean:85:  have hmean : Real.circleAverage g z₀ r = g z₀ := hdc.circleAverage
lean/Main.lean:87:  simp only [Real.circleAverage_def, norm_smul, Real.norm_eq_abs, abs_inv,
lean/Main.lean:93:lemma integral_closedBall_zero_eq_polar
lean/Main.lean:109:  rw [← Complex.integral_comp_polarCoord_symm ((closedBall (0 : ℂ) r).indicator q)]
lean/Main.lean:150:  rw [integral_closedBall_zero_eq_polar g hr]
lean/Main.lean:192:    have hcircle : Real.circleAverage g 0 s = g 0 := hdiff.circleAverage
lean/Main.lean:231:        simpa only [Real.circleAverage_def, hpolarCircle] using hcircle
lean/Main.lean:360:    Mathlib's `DiffContOnCl.circleAverage` supplies the circle identity.  The
lean/Main.lean:362:    `Complex.integral_comp_polarCoord_symm` and Fubini, translate the disk from
```

Mathlib search:

```text
$ rg -n -i 'integral_comp_polarCoord_symm|circleAverage' .lake/packages/mathlib/Mathlib
Analysis/Complex/Harmonic/MeanValue.lean:24:closed disc of radius `R` and center `c`, then the circle average `circleAverage f c R` equals
Analysis/Complex/Harmonic/MeanValue.lean:27:theorem HarmonicOnNhd.circleAverage_eq (hf : HarmonicOnNhd f (closedBall c |R|)) :
Analysis/Complex/Harmonic/MeanValue.lean:28:    circleAverage f c R = f c := by
Analysis/Complex/Harmonic/MeanValue.lean:39:  rw [← circleAverage_congr_sphere h₄F, Complex.reCLM.circleAverage_comp_comm,
Analysis/Complex/Harmonic/MeanValue.lean:40:    h₃F.diffContOnCl.circleAverage]
Analysis/Complex/Harmonic/MeanValue.lean:47:`|R|` and center `c` and continuous on its closure, then the circle average `circleAverage f c R`
Analysis/Complex/Harmonic/MeanValue.lean:50:theorem HarmonicContOnCl.circleAverage_eq {f : ℂ → ℝ} {c : ℂ} {R : ℝ}
Analysis/Complex/Harmonic/MeanValue.lean:52:    circleAverage f c R = f c := by
Analysis/Complex/Harmonic/MeanValue.lean:55:  have H : ContinuousOn (circleAverage f c) (Set.Ioc 0 |R|) := by
Analysis/Complex/Harmonic/MeanValue.lean:56:    refine (h₁f.2.mono ?_).circleAverage (fun z hz ↦ hz.1.le)
Analysis/Complex/Harmonic/MeanValue.lean:60:  rw [← circleAverage_abs_radius]
Analysis/Complex/Harmonic/MeanValue.lean:63:    apply HarmonicOnNhd.circleAverage_eq
... (136 additional hits omitted)
```

Verdict: **`:reachable-cheaply`**. No: the base file already contains the proved circle and polar bridge declarations and names the same API, so withholding the memory does not withhold its operative content.

Prior-failure evidence: Attempts before the memory left the disk-area bridge open; queue receipt 57ca09c6-cfd2-441e-a99b-c96f6c2fffaa records a blocked partial.

Recommendation: **exclude**.

### 2. a01A07 × `e-codexpilot-upgrade-diskwise-L1-convergence-to-local-uniform-convergence`

Memory content (read-only `GET http://127.0.0.1:7073/api/alpha/evidence/e-codexpilot-upgrade-diskwise-L1-convergence-to-local-uniform-convergence`): Work on a local half-radius disk, prove uniform Cauchy convergence from two diskwise L1 errors, then package it with the local-uniform/compact-uniform API.

Base-tree search:

```text
$ git archive 81dccb3 | tar -x -C "$SNAPSHOT"
$ rg -n -i 'smaller concentric disk|uniform Cauchy|tendstoLocallyUniformlyOn_iff_forall_isCompact|R / 2' "$SNAPSHOT/problems/a01A07"
lean/Main.lean:44:The remaining gap is the compact-local uniform Cauchy argument upgrading the
lean/Main.lean:400:    UniformCauchySeqOn f atTop (closedBall x (R / 2)) := by
lean/Main.lean:403:  let c : ℝ := 1 / (Real.pi * (R / 2) ^ 2)
lean/Main.lean:412:  have hsmall_subset : closedBall y (R / 2) ⊆ closedBall x R := by
lean/Main.lean:416:  have hsmallU : closedBall y (R / 2) ⊆ U := hsmall_subset.trans hRU
lean/Main.lean:417:  have hfm : DifferentiableOn ℂ (f m) (closedBall y (R / 2)) :=
lean/Main.lean:419:  have hfn : DifferentiableOn ℂ (f n) (closedBall y (R / 2)) :=
lean/Main.lean:423:        c * ∫ w in closedBall y (R / 2), ‖f m w - f n w‖ := by
lean/Main.lean:437:      (∫ w in closedBall y (R / 2), ‖f m w - f n w‖) ≤
lean/Main.lean:441:      (∫ w in closedBall y (R / 2), ‖f m w - f n w‖) ≤
lean/Main.lean:442:          ∫ w in closedBall y (R / 2),
lean/Main.lean:453:      _ = (∫ w in closedBall y (R / 2), ‖f m w - F w‖) +
```

Mathlib search:

```text
$ rg -n -i 'tendstoLocallyUniformlyOn_iff_forall_isCompact|UniformCauchySeqOn' .lake/packages/mathlib/Mathlib
Analysis/Calculus/SmoothSeries.lean:51:  have A : UniformCauchySeqOn (fun t : Finset α => fun x => ∑ i ∈ t, f' i x) atTop s :=
Analysis/Calculus/SmoothSeries.lean:52:    (tendstoUniformlyOn_tsum hu hf').uniformCauchySeqOn
Analysis/Calculus/SmoothSeries.lean:53:  refine cauchy_map_of_uniformCauchySeqOn_fderiv (f := fun t x ↦ ∑ i ∈ t, f i x)
Analysis/Calculus/UniformLimitsDeriv.lean:21:* `uniformCauchySeqOnFilter_of_fderiv`: If
Analysis/Calculus/UniformLimitsDeriv.lean:115:theorem uniformCauchySeqOnFilter_of_fderiv (hf' : UniformCauchySeqOnFilter f' l (𝓝 x))
Analysis/Calculus/UniformLimitsDeriv.lean:117:    (hfg : Cauchy (map (fun n => f n x) l)) : UniformCauchySeqOnFilter f l (𝓝 x) := by
Analysis/Calculus/UniformLimitsDeriv.lean:120:  rw [SeminormedAddGroup.uniformCauchySeqOnFilter_iff_tendstoUniformlyOnFilter_zero] at hf' ⊢
Analysis/Calculus/UniformLimitsDeriv.lean:177:convergence. See `cauchy_map_of_uniformCauchySeqOn_fderiv`.
Analysis/Calculus/UniformLimitsDeriv.lean:179:theorem uniformCauchySeqOn_ball_of_fderiv {r : ℝ} (hf' : UniformCauchySeqOn f' l (Metric.ball x r))
Analysis/Calculus/UniformLimitsDeriv.lean:181:    (hfg : Cauchy (map (fun n => f n x) l)) : UniformCauchySeqOn f l (Metric.ball x r) := by
Analysis/Calculus/UniformLimitsDeriv.lean:186:  · simp only [Metric.ball_eq_empty.2 hr, UniformCauchySeqOn, Set.mem_empty_iff_false,
Analysis/Calculus/UniformLimitsDeriv.lean:188:  rw [SeminormedAddGroup.uniformCauchySeqOn_iff_tendstoUniformlyOn_zero] at hf' ⊢
... (40 additional hits omitted)
```

Verdict: **`:reachable-cheaply`**. No: the base theorem docstring spells out the local smaller-disk route, the uniform-Cauchy step, and the exact packaging declaration.

Prior-failure evidence: The queue records attempt 1 as blocked-with-partial (receipt 57ca09c6-cfd2-441e-a99b-c96f6c2fffaa) before the later memory-carrying close.

Recommendation: **exclude**.

### 3. a02J05 × `e-codexpilot-derive-a-sinc-tail-identity-by-differentiating-cosine-over-x`

Memory content (read-only `GET http://127.0.0.1:7073/api/alpha/evidence/e-codexpilot-derive-a-sinc-tail-identity-by-differentiating-cosine-over-x`): Differentiate cos(x)/x to derive the finite-interval sinc identity and its 2/a Dirichlet tail bound.

Base-tree search:

```text
$ git archive fddc86c | tar -x -C "$SNAPSHOT"
$ rg -n -i 'integral_sinc_eq_cos_div_sub|abs_integral_sinc_le_two_div|cos x / x' "$SNAPSHOT/problems/a02J05"
informal-solution.md:171:      ∫ x in Icc (-R) (-ε), Real.cos x / x +
informal-solution.md:172:      ∫ x in Icc ε R, Real.cos x / x = 0 := by
lean/Main.lean:41:lemma integral_sinc_eq_cos_div_sub {a b : ℝ} (ha : 0 < a) (hab : a ≤ b) :
lean/Main.lean:43:      Real.cos a / a - Real.cos b / b - ∫ x in a..b, Real.cos x / x ^ 2 := by
lean/Main.lean:52:        (-Real.sin x / x - Real.cos x / x ^ 2) x := by
lean/Main.lean:58:      (fun x : ℝ => -Real.sin x / x - Real.cos x / x ^ 2) volume a b := by
lean/Main.lean:65:  have hcos : IntervalIntegrable (fun x : ℝ => Real.cos x / x ^ 2) volume a b := by
lean/Main.lean:68:  have hfun : (fun x : ℝ => -Real.sin x / x - Real.cos x / x ^ 2) =
lean/Main.lean:69:      fun x => -(Real.sin x / x) - Real.cos x / x ^ 2 := by
lean/Main.lean:106:lemma abs_integral_sinc_le_two_div {a b : ℝ} (ha : 0 < a) (hab : a ≤ b) :
lean/Main.lean:111:      IntervalIntegrable (fun x : ℝ => Real.cos x / x ^ 2) volume a b := by
lean/Main.lean:119:      IntervalIntegrable (fun x : ℝ => |Real.cos x / x ^ 2|) volume a b :=
... (2 additional hits omitted)
```

Mathlib search:

```text
$ rg -n -i 'sinc|integral_eq_sub_of_hasDerivAt' .lake/packages/mathlib/Mathlib
Algebra/AddConstMap/Basic.lean:260:    · -- Since `l ≤ x ≤ y`, the case `n < 0` is impossible
Algebra/AffineMonoid/Irreducible.lean:119:    -- Since `M` has a single unit, this means that
Algebra/AffineMonoid/Irreducible.lean:129:  -- Since `M` has a single unit, this means that `r = 1`. Contradiction.
Algebra/Algebra/Basic.lean:124:@[deprecated coe_algebraMap_ofSubsemiring (since := "2025-11-23")]
Algebra/Algebra/Basic.lean:129:@[deprecated algebraMap_ofSubsemiring_apply (since := "2025-11-23")]
Algebra/Algebra/Basic.lean:366:@[deprecated coe_eq_zero_iff (since := "2025-10-21")]
Algebra/Algebra/Basic.lean:476:@[deprecated (since := "2026-01-21")]
Algebra/Algebra/Basic.lean:479:@[deprecated (since := "2026-01-21")]
Algebra/Algebra/Bilinear.lean:159:@[deprecated (since := "2025-12-30")] alias toSpanSingleton_eq_algebra_linearMap :=
Algebra/Algebra/Defs.lean:176:instance, since this creates another `SMul R S` instance from the supplied `RingHom` and
Algebra/Algebra/Defs.lean:202:instance, since this creates another `SMul R S` instance from the supplied `RingHom` and
Algebra/Algebra/Defs.lean:407:@[deprecated _root_.smul_eq_mul (since := "2025-12-02")]
... (4163 additional hits omitted)
```

Verdict: **`:reachable-cheaply`**. No: both the named identity and the derived uniform tail theorem are already proved in the base file.

Prior-failure evidence: The first construction run proved the identity but remained blocked (receipt f8586558-7882-4ce9-b2cf-29402de797b1); later candidate rows 24 and 26 report the memory used.

Recommendation: **exclude**.

### 4. a02J05 × `e-codexpilot-remove-Abel-regularization-from-the-Dirichlet-sinc-integral`

Memory content (read-only `GET http://127.0.0.1:7073/api/alpha/evidence/e-codexpilot-remove-Abel-regularization-from-the-Dirichlet-sinc-integral`): Evaluate the damped sinc integral by Fubini, send the damping to zero, and remove regularization with a uniform Dirichlet tail estimate.

Base-tree search:

```text
$ git archive fddc86c | tar -x -C "$SNAPSHOT"
$ rg -n -i 'Abel regularization|arctan \(1/a\)|damped Fubini|uniform tail estimate' "$SNAPSHOT/problems/a02J05"
lean/Main.lean:40:uniform tail estimate needed to remove an Abel or Gaussian regularization. -/
lean/Main.lean:105:/-- Dirichlet's uniform tail estimate for the sinc integral. -/
lean/Main.lean:267:  -- The checked component route is Abel regularization:
lean/Main.lean:268:  --   ∫₀∞ exp (-a*x) * sinc x dx = arctan (1/a),
lean/Main.lean:269:  -- followed by a uniform tail estimate as `a ↓ 0`.
lean/Main.lean:271:  -- integral, but not the damped Fubini evaluation or the Abel-removal lemma.
```

Mathlib search:

```text
$ rg -n -i 'Abel|sinc.*integral|integral.*sinc' .lake/packages/mathlib/Mathlib
Algebra/AffineMonoid/Embedding.lean:9:public import Mathlib.GroupTheory.FreeAbelianGroup
Algebra/AffineMonoid/Embedding.lean:40:noncomputable def embedding : M →+ FreeAbelianGroup (Fin (dim M)) :=
Algebra/AffineMonoid/Embedding.lean:41:  .comp (FreeAbelianGroup.equivFinsupp _).symm.toAddMonoidHom <|
Algebra/AffineMonoid/UniqueSums.lean:12:import Mathlib.Algebra.FreeAbelianGroup.UniqueSums
Algebra/Algebra/Spectrum/Quasispectrum.lean:101:  mul_assoc x y z := equiv.symm.injective <| by simp [mul_add, add_mul, mul_assoc]; abel
Algebra/Algebra/Spectrum/Quasispectrum.lean:162:            abel
Algebra/Algebra/Spectrum/Quasispectrum.lean:168:            abel
Algebra/Algebra/Unitization.lean:14:public import Mathlib.Tactic.Abel
Algebra/Algebra/Unitization.lean:512:      abel
Algebra/Algebra/Unitization.lean:515:      abel }
Algebra/Algebra/Unitization.lean:524:      abel }
Algebra/BigOperators/Module.lean:10:public import Mathlib.Tactic.Abel
... (2329 additional hits omitted)
```

Verdict: **`:reachable-cheaply`**. No: the base file's remaining-obstruction comment gives the same Abel evaluation and uniform-removal plan.

Prior-failure evidence: Receipt f8586558-7882-4ce9-b2cf-29402de797b1 is the blocked run from which the frontier was drafted; a later attempt remained blocked before the third-attempt close.

Recommendation: **exclude**.

### 5. a95J08 × `e-codexpilot-bound-automatic-frontier-descent-when-a-leaf-recurses`

Memory content (read-only `GET http://127.0.0.1:7073/api/alpha/evidence/e-codexpilot-bound-automatic-frontier-descent-when-a-leaf-recurses`): Stop automatic construction-target descent after one newly exposed frontier level and return the dependency chain for operator prioritisation.

Base-tree search:

```text
$ git archive 61ddc05 | tar -x -C "$SNAPSHOT"
$ rg -n -i 'automatic.*frontier|frontier.*descent|leaf.*recurs|one newly exposed frontier' "$SNAPSHOT/problems/a95J08"
(no hits)
```

Mathlib search:

```text
$ rg -n -i 'automatic.*frontier|frontier.*descent|leaf.*recurs|one newly exposed frontier' .lake/packages/mathlib/Mathlib
(no hits)
```

Verdict: **`:unreachable`**. Possibly: the scope guard is absent from the base tree and Mathlib, but historical data provide no memory-free failure and it governed stopping rather than the successful proof route.

Prior-failure evidence: All three recorded a95J08 candidate rows (jobs 445, 460, 463) report this memory used; the frozen receipts contain no a95J08 attempt known to lack it.

Recommendation: **weak**.

### 6. a95J08 × `e-codexpilot-bound-the-interface-adapter-heuristic-with-genuine-construction-cases`

Memory content (read-only `GET http://127.0.0.1:7073/api/alpha/evidence/e-codexpilot-bound-the-interface-adapter-heuristic-with-genuine-construction-cases`): Require an end-to-end library theorem before calling a task adapter work; constituent lemmas without that theorem indicate genuine construction.

Base-tree search:

```text
$ git archive 61ddc05 | tar -x -C "$SNAPSHOT"
$ rg -n -i 'interface adapter|adapter work|end-to-end library theorem|constituent lemmas' "$SNAPSHOT/problems/a95J08"
(no hits)
```

Mathlib search:

```text
$ rg -n -i 'interface adapter|adapter work|end-to-end library theorem|constituent lemmas' .lake/packages/mathlib/Mathlib
(no hits)
```

Verdict: **`:unreachable`**. Possibly: its decision rule is not repository-searchable, but it only prevented a category error and did not supply the Hölder construction that eventually closed the theorem.

Prior-failure evidence: Jobs 445, 460, and 463 all report this memory used as a scope guard; no frozen a95J08 failure without it was identified.

Recommendation: **weak**.

### 7. a95J08 × `e-codexpilot-prove-general-probability-kernel-Lp-contraction-by-integral-Young`

Memory content (read-only `GET http://127.0.0.1:7073/api/alpha/evidence/e-codexpilot-prove-general-probability-kernel-Lp-contraction-by-integral-Young`): Reduce probability-kernel Lp contraction to a general integral Young/Jensen-Tonelli bridge after kernel normalization and translation continuity.

Base-tree search:

```text
$ git archive 61ddc05 | tar -x -C "$SNAPSHOT"
$ rg -n -i 'weighted Hölder|Jensen|Tonelli|Young.s convolution|lintegral_mul_norm_pow_le' "$SNAPSHOT/problems/a95J08"
informal-solution.md:3:# APM a95J08: Young's convolution inequality — f ∈ L¹, g ∈ Lp implies f*g ∈ Lp
informal-solution.md:17:By Tonelli's theorem (applied to the nonneg function |f(x-y)||g(y)|):
informal-solution.md:29:The second factor is finite for a.e. x by Tonelli:
informal-solution.md:43:= ||f||₁^{p/q} · ||f||₁ · ||g||_p^p (by Tonelli, as above)
informal-solution.md:51:**What connects.** This is Young's convolution inequality in the special case ||f*g||_p ≤ ||f||₁ · ||g||_p (the exponents satisfy 1 + 1/p = 1/1 + 1/p). The general form is ||f*g||_r ≤ ||f||_p · ||g||_q where 1/p + 1/q = 1 + 1/r. The proof technique — splitting the integrand via Hölder with a carefully chosen factorisation |f|^{1/q} · |f|^{1/p}|g|, then applying Tonelli to the "heavy" factor — is the standard Minkowski/Young argument. Part (a) is the "well-definedness" step that is often swept under the rug but is essential: the convolution is only defined a.e., and Fubini/Tonelli justifies this. The result is fundamental to harmonic analysis (approximate identities, mollifiers) and PDE (solution operators via convolution with fundamental solutions).
informal-solution.md:113:/-- Tonelli/Fubini step: ∫∫|f(x-y)||g(y)|^p dy dx = ‖f‖₁ · ‖g‖_p^p. -/
informal-solution.md:114:lemma tonelli_convolution_step
informal-solution.md:143:- `MeasureTheory.lintegral_lintegral_swap` / `MeasureTheory.Measure.integral_prod` — Fubini/Tonelli for swapping integrals
lean/Main.lean:2:! # APM a95J08: Young's convolution inequality — f ∈ L¹, g ∈ Lp implies f*g ∈ Lp
lean/Main.lean:9:- Young's convolution inequality is NOT in Mathlib (established in a95A03).
lean/Main.lean:12:- The Tonelli/Fubini step requires product measure setup.
lean/Main.lean:109:/-- Young's convolution inequality at the endpoint `p = 1`.
... (9 additional hits omitted)
```

Mathlib search (the G6 route-relative hit):

```text
$ rg -n -i 'lintegral_mul_norm_pow_le' .lake/packages/mathlib/Mathlib
MeasureTheory/Integral/MeanInequalities.lean:27:`ENNReal.lintegral_mul_norm_pow_le` is a variant where the exponents are not reciprocals:
MeasureTheory/Integral/MeanInequalities.lean:170:theorem lintegral_mul_norm_pow_le {α} [MeasurableSpace α] {μ : Measure α}
MeasureTheory/Integral/MeanInequalities.lean:226:            apply ENNReal.lintegral_mul_norm_pow_le
```

Verdict: **`:reachable-with-route-knowledge`**. Yes, but direction is not preregistered safely: the base informal solution already exposes Hölder, while the memory steers toward Jensen; the eventual first-try close used Hölder and Mathlib's route-specific lintegral_mul_norm_pow_le.

Prior-failure evidence: The first a95J08 candidate row (job 445) failed without this memory; later jobs 460 and 463 used it and still failed at the Jensen/ENNReal frontier.

Recommendation: **weak**.


## Panel decision

Do not spend E2's full budget on this panel as currently sourced. The frozen
candidate artifact is a use census, not the preregistered LB/IN panel, and four
of seven supported pairs have structural cancellation. A run-ready registration
needs an actual adjudication artifact plus replacement pairs for the four
exclusions and for a93J07. The three a95J08 pairs can remain pilot candidates,
explicitly marked weak and without a predicted beneficial direction.
