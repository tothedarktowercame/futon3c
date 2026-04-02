**THE KEY INSIGHT**
- Treat the sequence as uniformly `L^3`-bounded and apply Hölder with exponents `(3, 3/2)` on any measurable `E`: `∫_E |f_n| ≤ (∫_E |f_n|^3)^{1/3} m(E)^{2/3} ≤ m(E)^{2/3}`. Thus picking `δ = ε^{3/2}` forces `∫_E |f_n| < ε` simultaneously for every `n`, i.e., the sequence is uniformly absolutely continuous.
- To invalidate the statement under only `L^1` control, concentrate mass on shrinking intervals: take `f_n = n·χ_[0,1/n]`. Each has `∫ |f_n| = 1`, yet on its own support the integral stays `1`, so no common `δ` can make all `∫_E |f_n|` small.

**THE NAIVE APPROACH THAT FAILS**
- Leaning solely on the `L^1` bound (via Markov/Chebyshev) yields at best `∫_E |f_n| ≤ 1`, which ignores the size of `E`. Without the stronger `p>1` information, nothing stops the functions from spiking on tiny sets, so the naive argument never produces a `δ` that works for all `n`—exactly the phenomenon exposed in part (b).
