# Zai Scribe pattern library — f196 (m00A05 student arc)

Patterns authored this frame because the reviewed corpus had no parent for
these filter/arith/calc stumbles.

## math-formalization/order-filter-lemma-sides-before-squeezing

- **Trigger:** A limit argument needs to bound a constant (or a fixed
  expression) by functions of a parameter tending to zero, or to weaken one
  side of a `Tendsto` through a subfilter such as a punctured neighbourhood.
- **Move:** Check which side of the order and which filter (source vs target)
  the candidate lemma manipulates before applying. For squeezing a constant
  `c ≤ f x` with `f x → 0` use the two-function order-limit lemma
  (`le_of_tendsto_of_tendsto`), not the one-function `le_of_tendsto` which
  has the wrong direction. For weakening a `Tendsto` to a smaller source
  filter use the left-directed `mono_left`; `mono_right` only grows the
  target filter. Annotate implicit set/point arguments on `nhdsWithin`-type
  lemmas explicitly rather than letting elaboration guess.
- **Why it works:** Order-limit lemmas come in directional pairs and the
  display names do not say which side is fixed; a wrong-direction apply
  produces a confusingly-shaped new goal instead of an error, burning
  iterations on a non-mathematical issue.

## math-formalization/bridge-nnreal-casts-before-linarith

- **Trigger:** A hypothesis mixes a coerced `ℝ≥0` constant (e.g. a Lipschitz
  constant or Gronwall `K`) with plain `ℝ` expressions, and `linarith` or
  `norm_num`-closing fails with an apparently-true arithmetic goal.
- **Move:** Rewrite the coerced atom to a plain expression once with an
  `rfl` bridge (`have h : ((a:ℝ≥0):ℝ) = <plain expr> := rfl; rw [h] at hyp`)
  before calling arithmetic tactics; run `norm_num at hyp` to normalise the
  cast when the lemma returns `K` as an `NNReal` coercion.
- **Why it works:** The coercion `((a:ℝ≥0):ℝ)` and the plain literal are
  syntactically different atoms to the arithmetic backend even when
  definitionally equal; one explicit bridge makes every later `linarith`
  succeed.

## math-formalization/terminate-single-step-calc-blocks-explicitly

- **Trigger:** A proof closes one step inside a `calc` block and then
  continues with further tactic lines at lower indentation; compilation
  produces cascading parse errors ("unknown identifier", "unexpected token")
  far from the calc block.
- **Move:** Do not leave a single-step `calc` open before further tactics;
  close it with an explicit `exact hstep` (or restructure into `have hstep :=
  calc …`) before the next tactic line.
- **Why it works:** A trailing calc step continues to absorb subsequent
  lines as phantom steps regardless of indentation, so the reported errors
  point elsewhere in the file; ending the block explicitly restores
  line-by-line attribution.
