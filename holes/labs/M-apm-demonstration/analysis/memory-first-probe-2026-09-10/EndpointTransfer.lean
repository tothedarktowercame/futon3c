import Mathlib

/-! Reference feasibility calculation, Codex-17, 2026-09-10.
Memory e-apm-promotion-5fdb99169bd788313841375c797c302c from m00A05/f196.
Adapted WITH access to the retained f196 attempt-1 proof (18d4b281 Main.lean).
This is not a blinded student trial or evidence of memory-only causal gain.
The bounded interval hypotheses match the endpoint seam in m96J04;
operator construction/compactness and the whole problem remain separate.
-/
open Set Filter Topology
namespace MemoryFirstProbe

theorem endpoint_unique_on_unit {u v : ℝ → ℝ} {K : NNReal}
    {F : ℝ → ℝ → ℝ} (hF : ∀ t, LipschitzWith K (F t))
    (hu : ContinuousOn u (Icc 0 1)) (hv : ContinuousOn v (Icc 0 1))
    (hdu : ∀ s ∈ Ioo 0 1, HasDerivAt u (F s (u s)) s)
    (hdv : ∀ s ∈ Ioo 0 1, HasDerivAt v (F s (v s)) s)
    (huv : u 0 = v 0) : EqOn u v (Icc 0 1) := by
  intro t ht
  by_cases h0 : t = 0
  · subst h0
    exact huv
  · have htp : 0 < t := lt_of_le_of_ne (ht.1) (Ne.symm h0)
    have key : ∀ δ ∈ Ioo (0 : ℝ) t,
        dist (u t) (v t) ≤ dist (u δ) (v δ) * Real.exp ((K : ℝ) * (t - δ)) := by
      intro δ hδ
      have hstep := dist_le_of_trajectories_ODE (K := K) (v := F) hF
        (hu.mono (fun x hx => ⟨le_trans hδ.1.le hx.1, le_trans hx.2 ht.2⟩))
        (fun s hs => (hdu s ⟨lt_of_lt_of_le hδ.1 hs.1, lt_of_lt_of_le hs.2 ht.2⟩).hasDerivWithinAt)
        (hv.mono (fun x hx => ⟨le_trans hδ.1.le hx.1, le_trans hx.2 ht.2⟩))
        (fun s hs => (hdv s ⟨lt_of_lt_of_le hδ.1 hs.1, lt_of_lt_of_le hs.2 ht.2⟩).hasDerivWithinAt)
        (le_refl _) t (right_mem_Icc.2 hδ.2.le)
      exact hstep
    have hcont : ContinuousOn (fun s => dist (u s) (v s)) (Icc 0 1) :=
      ((hu.sub hv).abs).congr
        (fun s _ => (Real.dist_eq (u s) (v s)).symm)
    have hc0 := hcont.continuousWithinAt (by norm_num : (0:ℝ) ∈ Icc 0 1)
    have h1 : Tendsto (fun δ => dist (u δ) (v δ)) (𝓝[>] 0) (𝓝 0) := by
      have hc := ContinuousWithinAt.tendsto hc0
      rw [nhdsWithin_Icc_eq_nhdsGE (by norm_num : (0:ℝ) < 1)] at hc
      have h1' := hc.mono_left (nhdsWithin_mono _ Ioi_subset_Ici_self)
      rw [show dist (u 0) (v 0) = 0 by
        rw [huv]; exact dist_self _] at h1'
      simpa using h1'
    have h2 : Tendsto (fun δ : ℝ => Real.exp ((K : ℝ) * (t - δ))) (𝓝[>] 0)
        (𝓝 (Real.exp ((K : ℝ) * t))) := by
      have h2' := (Continuous.tendsto
        (by continuity : Continuous fun δ : ℝ => Real.exp ((K : ℝ) * (t - δ))) 0).mono_left
        (nhdsWithin_le_nhds (s := Ioi 0) (a := (0:ℝ)) : 𝓝[>] (0:ℝ) ≤ 𝓝 (0:ℝ))
      simpa using h2'
    have hg : Tendsto (fun δ => dist (u δ) (v δ) * Real.exp ((K : ℝ) * (t - δ)))
        (𝓝[>] 0) (𝓝 0) := by simpa using h1.mul h2
    have hmem : Ioo (0 : ℝ) t ∈ 𝓝[>] (0 : ℝ) := by
      have h := inter_mem (mem_nhdsWithin_of_mem_nhds (Iio_mem_nhds htp))
        (self_mem_nhdsWithin (s := Ioi (0:ℝ)))
      rwa [show Iio t ∩ Ioi (0:ℝ) = Ioo 0 t by
        ext x
        show x ∈ Iio t ∩ Ioi 0 ↔ x ∈ Ioo 0 t
        simp only [Set.mem_inter_iff, Set.mem_Iio, Set.mem_Ioi, Set.mem_Ioo]
        exact ⟨fun h => ⟨h.2, h.1⟩, fun h => ⟨h.2, h.1⟩⟩] at h
    have hle : dist (u t) (v t) ≤ 0 :=
      le_of_tendsto_of_tendsto
        (tendsto_const_nhds : Tendsto (fun _ : ℝ => dist (u t) (v t)) (𝓝[>] 0)
          (𝓝 (dist (u t) (v t)))) hg
        (eventually_of_mem hmem (fun δ hδ => key δ hδ))
    exact dist_eq_zero.mp (le_antisymm hle dist_nonneg)

/-- The scalar ODE seam required by the Volterra example, after FTC has
produced the interior derivative. No derivative at zero is assumed. -/
theorem scalar_ode_zero {u : ℝ → ℝ} (c : ℝ)
    (hu : ContinuousOn u (Icc 0 1))
    (hdu : ∀ s ∈ Ioo 0 1, HasDerivAt u (c * u s) s)
    (hu0 : u 0 = 0) : EqOn u (fun _ => 0) (Icc 0 1) := by
  have hF : ∀ _t : ℝ, LipschitzWith (⟨|c|, abs_nonneg c⟩ : NNReal)
      (fun x : ℝ => c * x) := by
    intro t
    apply LipschitzWith.of_dist_le_mul
    intro x y
    simp [Real.dist_eq, ← mul_sub, abs_mul]
  exact endpoint_unique_on_unit hF hu continuousOn_const hdu
    (fun s _ => by simpa using hasDerivAt_const s (0 : ℝ)) hu0

/-- Refuting control: equal ODE derivatives without equal initial values
must not establish equal trajectories. -/
example : ¬ EqOn (fun _ : ℝ => (1 : ℝ)) (fun _ => 0) (Icc 0 1) := by
  intro h
  have hz := h (by norm_num : (0 : ℝ) ∈ Icc 0 1)
  norm_num at hz

#print axioms endpoint_unique_on_unit
#print axioms scalar_ode_zero
end MemoryFirstProbe

/- The following prerequisite declarations are copied verbatim from the pinned
m96J04 Main.lean prefix; the original file is unchanged. -/
namespace VolterraTarget
noncomputable section

abbrev apm_m96j04_H := ℓ²(ℕ, ℝ) × ℓ²(ℕ, ℝ)
abbrev apm_m96j04_C := C(Set.Icc (0 : ℝ) 1, ℝ)

def apm_m96j04_extend (f : apm_m96j04_C) (x : ℝ) : ℝ :=
  if hx : x ∈ Set.Icc (0 : ℝ) 1 then f ⟨x, hx⟩ else 0

/-- Although the chosen zero extension need not be continuous across the
endpoints, its restriction to the integration interval is continuous. -/
lemma apm_m96j04_extend_continuousOn (f : apm_m96j04_C) :
    ContinuousOn (apm_m96j04_extend f) (Set.Icc (0 : ℝ) 1) := by
  rw [continuousOn_iff_continuous_restrict]
  convert f.continuous using 1
  funext x
  simp [apm_m96j04_extend, x.property]

lemma apm_m96j04_extend_intervalIntegrable (f : apm_m96j04_C)
    {t : ℝ} (ht : t ∈ Set.Icc (0 : ℝ) 1) :
    IntervalIntegrable (apm_m96j04_extend f) MeasureTheory.volume 0 t := by
  exact ((apm_m96j04_extend_continuousOn f).mono
    (Set.uIcc_subset_Icc (by norm_num) ht)).intervalIntegrable

lemma apm_m96j04_integral_hasDerivAt (f : apm_m96j04_C)
    {t : ℝ} (ht : t ∈ Set.Ioo (0 : ℝ) 1) :
    HasDerivAt (fun u ↦ ∫ s in (0 : ℝ)..u, apm_m96j04_extend f s)
      (apm_m96j04_extend f t) t := by
  have hcont : ContinuousAt (apm_m96j04_extend f) t :=
    (apm_m96j04_extend_continuousOn f t ⟨ht.1.le, ht.2.le⟩).continuousAt
      (Icc_mem_nhds ht.1 ht.2)
  have hmeas : StronglyMeasurableAtFilter (apm_m96j04_extend f)
      (nhds t) MeasureTheory.volume :=
    ContinuousOn.stronglyMeasurableAtFilter isOpen_Ioo
      ((apm_m96j04_extend_continuousOn f).mono
        (fun _ hx ↦ ⟨hx.1.le, hx.2.le⟩)) t ht
  exact intervalIntegral.integral_hasDerivAt_right
    (apm_m96j04_extend_intervalIntegrable f ⟨ht.1.le, ht.2.le⟩)
    hmeas hcont

/-- The block operator from the Hilbert-space example, together with its
pointwise formula and compactness of all powers from the second onward. -/
lemma apm_m96j04_block_exists :
    ∃ A : apm_m96j04_H →L[ℝ] apm_m96j04_H,
      (∀ x y : ℓ²(ℕ, ℝ), A (x, y) = (y, 0)) ∧
      ∀ n : ℕ, 2 ≤ n → IsCompactOperator (A ^ n) := by
  let A : apm_m96j04_H →L[ℝ] apm_m96j04_H :=
    (ContinuousLinearMap.inl ℝ ℓ²(ℕ, ℝ) ℓ²(ℕ, ℝ)).comp
      (ContinuousLinearMap.snd ℝ ℓ²(ℕ, ℝ) ℓ²(ℕ, ℝ))
  refine ⟨A, ?_, ?_⟩
  · intro x y
    simp [A]
  · intro n hn
    obtain ⟨k, rfl⟩ := Nat.exists_eq_add_of_le hn
    have hzero : A ^ (2 + k) = 0 := by
      rw [pow_add]
      simp [show A ^ 2 = 0 by ext z <;> simp [A, pow_two]]
    rw [hzero]
    exact isCompactOperator_zero

/-- Any nonzero-eigenvalue eigenvector of an operator with the Volterra
pointwise formula has the required zero initial value. -/
lemma apm_m96j04_eigenvector_zero_at_zero
    (V : apm_m96j04_C →L[ℝ] apm_m96j04_C)
    (hV : ∀ (f : apm_m96j04_C) (t : Set.Icc (0 : ℝ) 1),
      V f t = ∫ s in (0 : ℝ)..(t : ℝ), apm_m96j04_extend f s)
    (eigenvalue : ℝ) (heigen : eigenvalue ≠ 0) (f : apm_m96j04_C)
    (hf : V f = eigenvalue • f) :
    f ⟨0, by norm_num⟩ = 0 := by
  let t0 : Set.Icc (0 : ℝ) 1 := ⟨0, by norm_num⟩
  have h := congrArg (fun g : apm_m96j04_C ↦ g t0) hf
  change V f t0 = eigenvalue * f t0 at h
  rw [hV] at h
  have h' : 0 = eigenvalue * f t0 := by simpa [t0] using h
  have ht0 : f t0 = 0 := (mul_eq_zero.mp h'.symm).resolve_left heigen
  simpa [t0] using ht0

/-- On the interior, the Volterra eigenvector equation is the scalar ODE
`f' = f / eigenvalue`. -/
lemma apm_m96j04_eigenvector_hasDerivAt
    (V : apm_m96j04_C →L[ℝ] apm_m96j04_C)
    (hV : ∀ (f : apm_m96j04_C) (t : Set.Icc (0 : ℝ) 1),
      V f t = ∫ s in (0 : ℝ)..(t : ℝ), apm_m96j04_extend f s)
    (eigenvalue : ℝ) (heigen : eigenvalue ≠ 0) (f : apm_m96j04_C)
    (hf : V f = eigenvalue • f) {t : ℝ} (ht : t ∈ Set.Ioo (0 : ℝ) 1) :
    HasDerivAt (apm_m96j04_extend f)
      (apm_m96j04_extend f t / eigenvalue) t := by
  let F : ℝ → ℝ := fun u ↦ ∫ s in (0 : ℝ)..u, apm_m96j04_extend f s
  have heq : (fun u ↦ eigenvalue * apm_m96j04_extend f u) =ᶠ[nhds t] F := by
    filter_upwards [Icc_mem_nhds ht.1 ht.2] with u hu
    let tu : Set.Icc (0 : ℝ) 1 := ⟨u, hu⟩
    have h := congrArg (fun g : apm_m96j04_C ↦ g tu) hf
    change V f tu = eigenvalue * f tu at h
    rw [hV] at h
    simpa [F, apm_m96j04_extend, hu, tu] using h.symm
  have hscaled : HasDerivAt (fun u ↦ eigenvalue * apm_m96j04_extend f u)
      (apm_m96j04_extend f t) t :=
    (apm_m96j04_integral_hasDerivAt f ht).congr_of_eventuallyEq heq
  have hinv := hscaled.const_mul eigenvalue⁻¹
  convert hinv using 1 <;> field_simp

/-- New reference application of the memory's endpoint limit argument to the
actual target interface; conditional on V's formula, not its construction. -/
theorem eigenvector_zero
    (V : apm_m96j04_C →L[ℝ] apm_m96j04_C)
    (hV : ∀ (f : apm_m96j04_C) (t : Set.Icc (0 : ℝ) 1),
      V f t = ∫ s in (0 : ℝ)..(t : ℝ), apm_m96j04_extend f s)
    (eigenvalue : ℝ) (heigen : eigenvalue ≠ 0) (f : apm_m96j04_C)
    (hf : V f = eigenvalue • f) : f = 0 := by
  have hu0 : apm_m96j04_extend f 0 = 0 := by
    simpa [apm_m96j04_extend] using
      apm_m96j04_eigenvector_zero_at_zero V hV eigenvalue heigen f hf
  have huniq := MemoryFirstProbe.scalar_ode_zero (1 / eigenvalue)
    (apm_m96j04_extend_continuousOn f)
    (fun s hs => by
      simpa [div_eq_mul_inv, mul_comm] using
        apm_m96j04_eigenvector_hasDerivAt V hV eigenvalue heigen f hf hs) hu0
  ext t
  have hz := huniq t.property
  simpa [apm_m96j04_extend, t.property] using hz

#print axioms eigenvector_zero
end
end VolterraTarget
