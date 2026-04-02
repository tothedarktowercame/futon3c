import Mathlib.MeasureTheory.Measure.Lebesgue.Basic
import Mathlib.MeasureTheory.Measure.NullMeasurable
import Mathlib.MeasureTheory.OuterMeasure.AE
import Mathlib.MeasureTheory.Measure.AEMeasurable

open MeasureTheory Set

noncomputable section

namespace APM

/-- Part (a): any Lebesgue (null) measurable set is a.e. equal to a Borel set. -/
theorem exists_borel_ae_eq
    {A : Set ℝ} (hA : NullMeasurableSet A volume) :
    ∃ B : Set ℝ, MeasurableSet B ∧
      volume (A \ B) = 0 ∧ volume (B \ A) = 0 := by
  classical
  have hEq : volume.toMeasurable A =ᵐ[volume] A :=
    NullMeasurableSet.toMeasurable_ae_eq hA
  refine ⟨volume.toMeasurable A, measurableSet_toMeasurable _ _, ?_, ?_⟩
    <;> obtain ⟨h₁, h₂⟩ := ae_eq_set.mp hEq
  · exact h₂
  · exact h₁

/-- A restatement of part (a) using the symmetric difference explicitly. -/
theorem exists_borel_symmDiff_null
    {A : Set ℝ} (hA : NullMeasurableSet A volume) :
    ∃ B : Set ℝ, MeasurableSet B ∧
      volume ((A \ B) ∪ (B \ A)) = 0 := by
  classical
  obtain ⟨B, hB, hAB, hBA⟩ := exists_borel_ae_eq (A := A) hA
  refine ⟨B, hB, ?_⟩
  have h_le :
      volume ((A \ B) ∪ (B \ A))
        ≤ volume (A \ B) + volume (B \ A) :=
    measure_union_le _ _
  have h_rhs : volume (A \ B) + volume (B \ A) = 0 := by simpa [hAB, hBA]
  have h_nonneg : 0 ≤ volume ((A \ B) ∪ (B \ A)) := by exact zero_le _
  exact le_antisymm (le_trans h_le <| h_rhs.le) h_nonneg

/-- Part (b): a Lebesgue null measurable function on `[0,1]` agrees a.e. with a Borel one. -/
theorem exists_borel_function_on_Icc
    {f : ℝ → ℝ}
    (hf : NullMeasurable f (volume.restrict (Icc (0 : ℝ) 1))) :
    ∃ g : ℝ → ℝ, Measurable g ∧
      f =ᵐ[volume.restrict (Icc (0 : ℝ) 1)] g := by
  classical
  have hA : AEMeasurable f (volume.restrict (Icc (0 : ℝ) 1)) :=
    hf.aemeasurable
  exact ⟨hA.mk f, hA.measurable_mk, hA.ae_eq_mk⟩

end APM
