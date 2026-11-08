/-
F3c synthetic-negative witness.

This file does not alter the genuine a93J02 theorem.  It proves that the
same partial sums cannot tend to log 3, using the genuine theorem and the
strict monotonicity of Real.log on positive reals.
-/

import problems.a93J02.lean.Main

open Set Filter Topology Real

theorem alternating_harmonic_ne_log_three :
    ¬ Tendsto (fun n => ∑ i ∈ Finset.range n, (-1 : ℝ) ^ i / (↑i + 1))
        atTop (𝓝 (log 3)) := by
  intro hlog_three
  have hlog_eq : log 2 = log 3 :=
    tendsto_nhds_unique alternating_harmonic_eq_log_two hlog_three
  have hlog_lt : log 2 < log 3 :=
    Real.strictMonoOn_log (by norm_num) (by norm_num) (by norm_num)
  exact (ne_of_lt hlog_lt) hlog_eq
