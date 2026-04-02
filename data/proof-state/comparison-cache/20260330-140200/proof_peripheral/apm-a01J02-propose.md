**THE KEY INSIGHT**
- Lebesgue measurable sets are exactly the completion of the Borel σ-algebra. Thus any measurable `A ⊆ ℝ` agrees almost everywhere with some Borel set `B`, so the symmetric difference has measure zero.
- For measurable functions, approximate `f` by simple functions supported on measurable sets, replace each such set by its Borel representative, and pass to the limit. The resulting function is Borel-measurable and differs from `f` only on the null union of the replacement sets.

**THE NAIVE APPROACH THAT FAILS**
- Trying to “correct” `A` by taking its closure/interior or smoothing `f` misses the point: those operations can change measure drastically. Without invoking the completion property and almost-everywhere equality, there is no guarantee the modified set/function stays null-close to the original.
