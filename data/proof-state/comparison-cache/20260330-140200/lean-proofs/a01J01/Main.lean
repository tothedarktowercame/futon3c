import Mathlib.MeasureTheory.Measure.Lebesgue.Basic
import Mathlib.MeasureTheory.Function.LpSeminorm.Indicator
import Mathlib.MeasureTheory.Function.LpSeminorm.CompareExp
import Mathlib.MeasureTheory.Integral.IntegrableOn
import Mathlib.Data.ENNReal.Holder

open MeasureTheory Set ENNReal Filter

noncomputable section

/-! # APM a01J01: Equi-integrability -/

-- CLOSED: HolderTriple 3 (3/2) 1 — the conjugate exponent pair for Hölder
instance : ENNReal.HolderTriple (3 : ℝ≥0∞) (3/2 : ℝ≥0∞) (1 : ℝ≥0∞) where
  inv_add_inv_eq_inv := by
    norm_num; rw [ENNReal.div_eq_inv_mul]
    rw [ENNReal.mul_inv (Or.inl (by norm_num : (2⁻¹ : ℝ≥0∞) ≠ 0))
                         (Or.inl (by norm_num : (2⁻¹ : ℝ≥0∞) ≠ ⊤))]
    rw [inv_inv]
    calc (3 : ℝ≥0∞)⁻¹ + 2 * (3 : ℝ≥0∞)⁻¹ = (1 + 2) * (3 : ℝ≥0∞)⁻¹ := by ring
      _ = 3 * (3 : ℝ≥0∞)⁻¹ := by norm_num
      _ = 1 := ENNReal.mul_inv_cancel (by norm_num) (by norm_num)

-- Part (a): Hölder → equi-integrability
theorem equi_integrable_of_Lp_bound
    (f : ℕ → ℝ → ℝ)
    (hf_meas : ∀ n, AEStronglyMeasurable (f n) (volume.restrict (Icc 0 1)))
    (hf_bound : ∀ n, eLpNorm (f n) 3 (volume.restrict (Icc 0 1)) ≤ 1)
    (ε : ℝ) (hε : 0 < ε) :
    ∃ δ : ℝ, 0 < δ ∧ ∀ (E : Set ℝ), MeasurableSet E → E ⊆ Icc 0 1 →
      volume E < ENNReal.ofReal δ →
      ∀ n, eLpNorm (E.indicator (f n)) 1 (volume.restrict (Icc 0 1)) ≤ ENNReal.ofReal ε := by
  classical
  set μ := volume.restrict (Icc 0 1)
  have h13 : (1 : ℝ≥0∞) ≤ 3 := by norm_num
  have hpow : (1 : ℝ) / 1 - 1 / 3 = (2 : ℝ) / 3 := by norm_num
  refine ⟨ε ^ (3 / 2 : ℝ), by positivity, ?_⟩
  intro E hE hEsub hEvol n
  have h_indicator :
      eLpNorm (E.indicator (f n)) 1 μ =
        eLpNorm (f n) 1 ((volume.restrict (Icc 0 1)).restrict E) := by
    simpa [μ] using
      (eLpNorm_indicator_eq_eLpNorm_restrict (μ := volume.restrict (Icc 0 1)) hE)
  rw [h_indicator]
  have hmeas' :
      AEStronglyMeasurable (f n)
        (((volume.restrict (Icc 0 1)).restrict E)) :=
    (hf_meas n).mono_measure Measure.restrict_le_self
  have hμE :
      ((volume.restrict (Icc 0 1)).restrict E) Set.univ = volume E := by
    have : (volume.restrict (Icc 0 1)) E = volume E := by
      simpa [Measure.restrict_apply, hE,
        Set.inter_eq_self_of_subset_right hEsub] using
          (Measure.restrict_apply (μ := volume) (s := Icc 0 1) hE)
    simpa [this] using
      (Measure.restrict_apply_univ :
        ((volume.restrict (Icc 0 1)).restrict E) Set.univ =
          (volume.restrict (Icc 0 1)) E)
  have hineq :=
    eLpNorm_le_eLpNorm_mul_rpow_measure_univ h13 hmeas'
  have hBound :
      eLpNorm (f n) 1 (((volume.restrict (Icc 0 1)).restrict E)) ≤
        eLpNorm (f n) 3 (((volume.restrict (Icc 0 1)).restrict E)) *
          (volume E) ^ ((2 : ℝ) / 3) := by
    simpa [hpow, hμE]
      using hineq
  have hmono :
      eLpNorm (f n) 3 (((volume.restrict (Icc 0 1)).restrict E)) ≤
        eLpNorm (f n) 3 (volume.restrict (Icc 0 1)) :=
    eLpNorm_mono_measure _ Measure.restrict_le_self
  have hBound' :
      eLpNorm (f n) 1 (((volume.restrict (Icc 0 1)).restrict E)) ≤
        eLpNorm (f n) 3 (volume.restrict (Icc 0 1)) *
          (volume E) ^ ((2 : ℝ) / 3) :=
    hBound.trans (mul_le_mul_right' hmono _)
  have hBound'' :
      eLpNorm (f n) 1 (((volume.restrict (Icc 0 1)).restrict E)) ≤
        (volume E) ^ ((2 : ℝ) / 3) :=
    hBound'.trans (mul_le_mul_right' (hf_bound n) _)
  have hpow_le :
      (volume E) ^ ((2 : ℝ) / 3) ≤
        ENNReal.ofReal (ε ^ (3 / 2 : ℝ)) ^ ((2 : ℝ) / 3) :=
    ENNReal.rpow_le_rpow (le_of_lt hEvol) (by norm_num : 0 ≤ (2 : ℝ) / 3)
  have hpow_eq :
      ENNReal.ofReal (ε ^ (3 / 2 : ℝ)) ^ ((2 : ℝ) / 3) =
        ENNReal.ofReal ε := by
    have hnonneg : 0 ≤ ε ^ (3 / 2 : ℝ) := by positivity
    have hreal :
        (ε ^ (3 / 2 : ℝ)) ^ ((2 : ℝ) / 3) = ε := by
      have := (Real.rpow_mul (le_of_lt hε) ((3 : ℝ) / 2) ((2 : ℝ) / 3)).symm
      simpa [by norm_num] using this
    simpa [ENNReal.ofReal_rpow_of_nonneg, hnonneg, hreal]
  exact hBound''.trans (hpow_le.trans (le_of_eq hpow_eq))

-- Part (b): counterexample
def counterex' (n : ℕ) : ℝ → ℝ :=
  (Icc (0 : ℝ) (1 / (↑n + 1))).indicator (fun _ => (↑n + 1 : ℝ))

-- CLOSED: eLpNorm = 1
lemma counterex'_eLpNorm_eq_one (n : ℕ) :
    eLpNorm (counterex' n) 1 volume = 1 := by
  have : eLpNorm (counterex' n) 1 volume =
      ‖(↑n + 1 : ℝ)‖₊ * volume (Icc (0 : ℝ) (1 / (↑n + 1))) ^ (1 / (1 : ℝ≥0∞).toReal) :=
    eLpNorm_indicator_const measurableSet_Icc one_ne_zero one_ne_top
  rw [this, Real.volume_Icc]; norm_num
  have hpos : (0 : ℝ) < ↑n + 1 := by positivity
  rw [ENNReal.ofReal_inv_of_pos hpos]
  rw [show (‖(↑n + 1 : ℝ)‖₊ : ℝ≥0∞) = ENNReal.ofReal (↑n + 1) from by
    simp only [ENNReal.ofReal, Real.toNNReal, NNNorm.nnnorm,
               Real.norm_of_nonneg (le_of_lt hpos)]
    congr 1; exact Subtype.ext (max_eq_left (le_of_lt hpos)).symm]
  exact ENNReal.mul_inv_cancel (by positivity) ofReal_ne_top

-- CLOSED: Archimedean
lemma exists_nat_inv_lt (δ : ℝ) (hδ : 0 < δ) : ∃ n : ℕ, 1 / (↑n + 1 : ℝ) < δ := by
  obtain ⟨n, hn⟩ := exists_nat_gt (1 / δ)
  refine ⟨n, ?_⟩
  have h1 : (0 : ℝ) < ↑n + 1 := by positivity
  exact one_div_lt h1 hδ |>.mpr (by linarith)

-- CLOSED: volume computation
lemma volume_Icc_inv (n : ℕ) :
    volume (Icc (0 : ℝ) (1 / (↑n + 1))) = ENNReal.ofReal (1 / (↑n + 1)) := by
  rw [Real.volume_Icc]; congr 1; ring

-- Not equi-integrable: wiring the closed lemmas
theorem counterex_not_equi_integrable (δ : ℝ) (hδ : 0 < δ) :
    ∃ n : ℕ, ∃ E : Set ℝ, MeasurableSet E ∧
      volume E < ENNReal.ofReal δ ∧
      1 ≤ eLpNorm (E.indicator (counterex' n)) 1 volume := by
  obtain ⟨n, hn⟩ := exists_nat_inv_lt δ hδ
  refine ⟨n, Icc 0 (1 / (↑n + 1)), measurableSet_Icc, ?_, ?_⟩
  · -- volume < δ
    rw [volume_Icc_inv]
    exact ENNReal.ofReal_lt_ofReal_iff hδ |>.mpr hn
  · -- indicator_E(counterex' n) = counterex' n since E is the support
    have hsub : Set.indicator (Icc (0 : ℝ) (1 / ((n : ℝ) + 1))) (counterex' n) = counterex' n := by
      unfold counterex'; ext x; simp only [Set.indicator_apply]
      split_ifs with h1 h2 <;> simp_all
    rw [hsub, counterex'_eLpNorm_eq_one]
