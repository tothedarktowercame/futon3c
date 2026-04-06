# Handoff to Codex: APM Lean Cleanup Pass

**From:** Claude (Sonnet/Opus via futon3c)
**To:** Codex (GPT-5 via VSCode)
**Date:** 2026-04-05
**Context:** Claude ran a statement-only batch over APM prelim problems producing
`data/apm-informal-proofs/apm-<id>.md` files. Each file has three sections:
1. Complete informal proof
2. Lean 4 theorem statements (all `sorry`-bodied, real Mathlib types)
3. Mathlib cross-reference list

Your job: close the sorries. **Do not think about the mathematics — Claude did
that.** Just wire the Mathlib API. If you get stuck, leave the sorry with a
one-line note explaining the specific blocker, then move to the next theorem.

## Rules

1. **No handoff documents.** Do not write a "context summary" or "next steps
   for future agent" file. If you do that, Claude will laugh at you.
2. **Narrow focus.** Close the sorries in one file at a time. Do not refactor.
   Do not rename. Do not add new lemmas unless strictly necessary.
3. **Use `exact?` and `apply?` aggressively.** The Mathlib cross-reference
   section of each file names the lemmas you should reach for. Most sorries
   are one-liners or short tactic chains.
4. **Build locally.** Run `lake build` in `/home/joe/code/apm-lean/` after
   each file to verify. Target the specific module, not the whole library.
5. **Log your results.** Append one line per file to
   `data/apm-codex-cleanup-log.edn` with shape:
   `{:problem-id "a94A07" :closed 3 :remaining 3 :notes "..."}`

## Start here: Two concrete targets

### Target 1: a94A07 — Injective entire function is affine

**Files:**
- Read: `/home/joe/code/futon3c/data/apm-informal-proofs/apm-a94A07.md`
- Write: `/home/joe/code/apm-lean/ApmCanaries/Frames/A94A07/Main.lean`

**The theorem:** If `f : ℂ → ℂ` is entire and injective, then `f z = a₀ + a₁ z`
for some `a₁ ≠ 0`.

**Claude's Lean statements (copy these into Main.lean):**
- `entire_injective_is_affine` — the main theorem
- `entire_injective_is_polynomial` — step 1 via removable singularity at ∞
- `injective_polynomial_degree_one` — step 2 via degree ≥ 2 ⟹ not injective
- `polynomial_degree_ge_two_not_injective` — the counterexample lemma
- Plus 1-2 more helper lemmas

**Mathlib API to use (from Claude's cross-refs):**
- `Polynomial.aeval`, `Polynomial.natDegree`, `Polynomial.roots`
- `Complex.isMaxOn_of_isCompact_of_continuous` (maximum modulus)
- `Polynomial.exists_root` (fundamental theorem of algebra)
- `Differentiable.isLittleO_sub` (for Casorati-Weierstrass)

**Suggested starting point:** `polynomial_degree_ge_two_not_injective` is the
easiest — degree ≥ 2 polynomial has ≥ 2 roots counted with multiplicity, pick
two distinct preimages of any generic value. Try `exact?` after setting up
the witness.

### Target 2: a94J01 — Absolute continuity of the Lebesgue integral

**Files:**
- Read: `/home/joe/code/futon3c/data/apm-informal-proofs/apm-a94J01.md`
- Write: `/home/joe/code/apm-lean/ApmCanaries/Frames/A94J01/Main.lean`

**The theorem:** For `f ∈ L¹`, ∀ ε > 0, ∃ δ > 0 such that `m(E) < δ ⟹ |∫_E f| < ε`.

**Claude's Lean statements:**
- `absolute_continuity_integral` — main
- `absolute_continuity_integral_norm` — stronger version with `∫_E |f|`
- `tail_integral_tendsto_zero` — helper: tail integrals vanish

**Mathlib API to check first:**
- `MeasureTheory.Integrable.absolutelyContinuous` — **this may already be the
  whole theorem**. If it exists and has the right shape, one `exact` closes
  `absolute_continuity_integral`.
- `MeasureTheory.integrable_iff_integrableOn_and_forall_ae_lt` — for the tail
- `Tendsto.eventually_lt` — converting the tail limit to the δ-ε form

**Suggested starting point:** grep Mathlib for
`absolutelyContinuous` and `Integrable` — this is a classical result that
should already be formalized. If you find it, the whole file might close in
10 minutes.

## What to do after these two

If Target 1 and Target 2 go well, pick your own next target from
`data/apm-informal-proofs/`. Prefer files where:
- The informal proof is concrete (explicit constructions, named theorems)
- The Mathlib cross-references section names specific lemmas
- The theorem count is small (1-3 sorries)

Avoid:
- Files where the cross-references are vague ("measure theory results")
- Problems where Claude's informal proof itself contains sorries or admits
- Topology problems using `ConnectedComponents` (Mathlib coverage is thin)

## Contact

If you finish these and want more, or if you hit a blocker that needs
Claude's input, write to `data/apm-codex-cleanup-log.edn` with a
`:blocker` field and Claude will pick it up on the next pass.

Good hunting.
