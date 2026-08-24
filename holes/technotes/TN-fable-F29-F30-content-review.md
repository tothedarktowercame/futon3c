# TN: F29 / a01J05 and F30 / a01J06 — the two solver proofs are real; the two "student closures" are not what the ledger implies

Author: Claude (Fable 5, `claude-8`), 2026-08-24T19:50Z. Read-only content
review of frames `f29` and `f30` of `jit-all-open-nontopology-v1`, prompted by
Joe: "they are the only things we have gotten back from this system that seem
worth anything at all" — is that so, and what exactly is the thing of worth?

Apparatus: futon3c `master` at `b20fcd0d`; problem repo `apm-lean`, base
revision `f7de6887` for both frames; substrate at `f3c0638a`.
Companions: `TN-opus-f27-review.md` (method), `TN-sonnet-F29-finding.md`
(F29 memory channel, live), `TN-fable-F30-findings.md` (F30 retirement mess).

**Short answer.** The two *solver* proofs are complete, faithful, sorry-free
formalisations of the textbook arguments, independently recompiled here with
standard axioms only. That is genuinely worth something — F29 in particular is
~1,800 lines of Mathlib-adjacent content that Mathlib does not have. The
*student* results, which are what the campaign is nominally measuring, are
worth much less than the ledger suggests: F29's student closure is the
solver's proof pasted verbatim through the memory channel (its own receipt
says so), and F30's student closure is not in the ledger at all — it exists
only because codex-10 rescued a dirty worktree.

## Method — what was checked, not receipt-trusted

1. Recompiled all four final files (`f29` solver head `124c6765`, `f29` student
   attempt-3 head `4ef6bdc3`, `f30` solver head `6b6dd959`, `f30` student
   preserved ref `58658226`) with `lake env lean` against the canonical
   substrate, appending `#print axioms` for `apm_a01j05`,
   `apm_a01J05_bridge_1`, `apm_a01j06`. Not a receipt re-read.
2. Extracted every `def`/`theorem` header at base and at head and compared
   them byte-for-byte (statement-tampering audit).
3. Read the TeX problem statements, `informal-solution.md`, and the "statement
   repairs" the artifact author recorded, and checked the Lean encoding
   against them.
4. Read every lemma signature in both solver files and the final assembly of
   each main theorem; checked the mathematics against the classical proofs.
5. Diffed student files against solver files (line diff; per-declaration
   name and body comparison).
6. Fetched the memory entries the students' `:receipt/memory-use` reports as
   `:used-ids` from the futon1b substrate (`:7073`) and measured their
   Lean content.
7. Read the six student-attempt receipts (`dirty?`, `sorry-warnings`, heads)
   and the phase timeline from `problem-transitions.edn`.

Not done: per-frame token cost (nothing in the ledger or job records carries
usage); a line-by-line read of all 2,600 solver lines (I read signatures and
assemblies, and let the kernel do the rest).

## Results

| | F29 / a01J05 | F30 / a01J06 |
|---|---|---|
| Statement vs base | identical (8/8 headers) | identical |
| Solver head compiles | 0 errors, 0 sorries | 0 errors, 0 sorries |
| Axioms (`#print axioms`) | `propext, Classical.choice, Quot.sound` | same |
| Solver diff vs base | +1804 / −17, 8 commits | +585 / −1, 7 commits |
| Solve phase wall time | 22 min (11:15→11:37) | 50 min (14:43→15:33) |
| Whole frame wall time | 1 h 47 (11:13→13:00) | 3 h 35 (14:42→18:17) |
| Student attempt-3 | 0 sorries, **verbatim transfer of solver proof** | 0 sorries, **never submitted (dirty tree ×3)** |

### The mathematics

**a01J05** (analytic on a neighbourhood of the closed disk, unimodular on the
circle, N interior zeros ⇒ f′ has N−1 interior zeros). The artifact's
encoding is faithful: zeros counted by `analyticOrderAt` summed over the open
disk in `ℕ∞`; the added `0 < N` is *necessary* (for N = 0, f is a unimodular
constant, `deriv f ≡ 0` has order ⊤ everywhere, and the claim is false).
The solver's proof is the classical one, done properly:

1. Build the canonical-factor product `P` over the closed-disk divisor and
   show `f·P` (regularised at removable points via
   `meromorphicTrailingCoeffAt`) is analytic, nonvanishing, and unimodular on
   the circle; max-modulus in both directions gives `f·P ≡ c`, `|c| = 1`.
   That is "f is a finite Blaschke product" (`…_regularizedCanonicalProduct_eq_const`).
2. On the circle, `z·f′(z)/f(z) = Σ_u n_u (1−|u|²)/|z−u|² > 0`
   (`…_circle_logSlope_eq_neg_sum`, `…_circle_logDeriv_sum_neg`), which
   needs `N > 0` for the sum to be nonempty; hence `f′ ≠ 0` on the circle.
3. A function mapping the circle into the slit plane has zero winding
   (`…_circleIntegral_logDeriv_eq_zero_of_mapsTo_slitPlane`), so
   `∮ logDeriv f′ = ∮ logDeriv f − 2πi`, and the argument principle
   (from `ConstructionTargets.Rouche`) converts both sides to divisor sums:
   `N(f′) = N − 1`.
4. Transport back from closed-disk divisor sums to the artifact's open-disk
   `ℕ∞`-tsum, using the boundary nonvanishing of both `f` and `f′`.

Nothing is fudged; the awkward parts (regularising `f·P`, the `ℕ∞`/`ℕ`/`ℤ`
cast layer, the `finsum` bookkeeping) are where the 1,800 lines go. Three
lemmas are general enough to be worth lifting out of the artifact:
`…_circleIntegral_logDeriv_eq_zero_of_mapsTo_slitPlane`,
`…_logDeriv_finset_prod`, and the Blaschke-constancy step (1). Mathlib has
none of "unimodular boundary ⇒ Blaschke product" today.

**a01J06** (entire, `|f| ≤ B e^{A|z|}` ⇒ `Σ (1+|ω_n|)^{−α} < ∞` for `α > 1`).
Encoding faithful, with one remark: `EnumeratesZeros` forces `ω : ℕ → ℂ` to
hit *only* zeros, with finite fibres of the right cardinality — so when `f`
has finitely many zeros the hypothesis is unsatisfiable and the theorem is
vacuous. That matches the source's "ω₁, ω₂, … listed with multiplicity"
(the finite case is trivial anyway), but it means the formal theorem says
nothing about, e.g., `f = exp`. Not a defect; worth knowing. The solver's
proof is the textbook one through Mathlib's new Nevanlinna API:

1. `proximity f ⊤ R ≤ log⁺B + A·R` from the growth bound; Jensen / first
   main theorem gives `logCounting f 0 R ≤ log⁺B + A·R + const`.
2. `n(R)·log 2 ≤ N(2R)` (`…_zeroCount_mul_log_two_le_logCounting`), hence
   `n(R) ≤ C·R` for `R ≥ 1`.
3. Dyadic shells `2^k ≤ 1+|ω_n| < 2^{k+1}`: each has ≤ `C·2^{k+1}` members
   of weight ≤ `2^{−kα}`; the majorant `2C·Σ(2^{1−α})^k` converges for
   `α > 1`; summability of the nonnegative series follows.

Shorter and less novel than F29 (Mathlib already carries the hard analysis);
the contribution is the glue between `ValueDistribution.logCounting`, the
divisor, and an indexed enumeration. Still correct and still not in Mathlib.

### What the students actually did

**F29.** Attempts 1–2 left the bridge sorry open. After attempt 2, the Guide
deposited four memories (`e-83ece32c`, `e-021bf80a`, `e-c86a7d45`,
`e-f6b4c56b`; 20–27 KB each, 38–57 `:= by` apiece, ~176 tactic blocks in
total) named `lean-block-1of4-…-certified` … `4of4`, whose hook reads
*"reusable proof text, not method prose — verbatim … from the f29 solver's
proctor-certified head 124c6765 … RENAME your own … before pasting."*
Attempt 3 pasted them. The resulting file differs from the solver's by 103
diff lines (one retained base lemma, reordering). The student's own receipt:
*"transfer-from-artifact-replay; not independent derivation."* The
`:used-ids` are exactly those four blocks; the three prose memories from the
earlier interventions are reported as "subsumed by block text".

So the F29 "student closure with memory from attempt 3" is the solver's
proof, copied by the Guide into memory, pasted by the Student. The channel
worked (that is what `TN-sonnet-F29-finding` correctly observed live), but
what it carried was the answer, not a technique. As a measurement of
"does a student learn from distilled memory", F29 is a null.

**F30.** The four memories the student used were short prose (1.4–1.5 KB,
zero Lean). Its 715-line file is structurally its own: it proves the two
HOP-4 contracts the base file's comment spelled out
(`apm_a01J06_linear_zero_count`, `apm_a01J06_summable_of_linear_count`) and
assembles them. Of its 23 declarations, 19 share a name with the solver's 37;
of those, 11 are byte-identical — 5 are the base file's own, and **6 solver
bridge lemmas** (`zeroCount`, `divisor_eq_analyticOrderNatAt`,
`divisor_nonneg`, `logCounting_top_eq_zero`,
`logCounting_zero_eq_divisor_logCounting`, `proximity_top_le`) arrived
verbatim by a channel I did not trace (not the four `:used-ids`; possibly
the memory-search receipts or the Guide's intervention text). The remaining
proofs, including the Jensen bound, the `n(R) log 2 ≤ N(2R)` step and all of
the dyadic argument, are the student's own and compile here sorry-free.

That is a real partial re-derivation and the most interesting student
result in the campaign so far — and **the ledger does not record it**. All
three F30 student attempts closed with `:dirty? true` and no
`sorry-warnings`; the student iterated past its budget without submitting
a clean tree (the same failure mode `TN-sonnet-F29-finding` flagged on F29
attempt 1). The only durable copy is
`refs/apm/preserved-student-attempts/f30/a01J06/58658226`, which exists
because codex-10 committed the dirty tree before retirement.

### Time

Joe's "not worth 3 hours" is about F30's 3 h 35 wall clock. The solver
needed 50 min of it (22 min on F29). The other ~2 h 45 were the student /
guide / scribe cycle, which produced the unrecorded proof above. F29's whole
frame was 1 h 47, of which the student cycle (1 h 20) produced a copy.

## Assessment

1. **The solver output is worth keeping.** Two correct, faithful, sorry-free
   formalisations of classical theorems Mathlib lacks, on standard axioms,
   with statements unchanged from the artifact. F29 is the more valuable
   (finite Blaschke factorisation + critical-point count + two reusable
   general lemmas). Neither is upstreamable as-is — both are written against
   bespoke `apm_*` definitions — but each is a day's restatement away.
2. **The student outputs do not currently measure what the campaign wants
   to measure.** F29's closure is a copy; F30's re-derivation is real but
   unrecorded. If "does memory make the next solve cheaper" is the question,
   the F29 memory design (Guide deposits certified proof text as memory)
   answers it trivially and should be ruled out of scope — a memory that
   contains the answer cannot demonstrate transfer.
3. **Two apparatus fixes would make the next frames evidential:**
   (a) forbid or separately tag verbatim-code memories (`:kind :fact` with
   `lean-block-*` names) so that student receipts distinguish "used a
   technique" from "pasted the proof"; (b) make the student submit-early
   rule enforceable — a dirty tree at budget end should auto-commit and
   certify what compiles, not vanish.
4. On Joe's question — yes, these are the two things of worth, but the
   worth is in the **solver** column, and it was produced in 22 and 50
   minutes respectively. Everything after `:verify` in both frames added
   wall-clock and one unrecorded proof.

## Evidence pointers

- Compiles: `/tmp/f29-solver-check.out`, `/tmp/f29-student3-check.out`,
  `/tmp/f30-solver-check.out`, `/tmp/f30-student-check.out` (axiom lines at
  the tail).
- Files: `git -C apm-lean show 124c6765:problems/a01J05/lean/Main.lean`,
  `… 4ef6bdc3:…`, `… 6b6dd959:problems/a01J06/lean/Main.lean`, `… 58658226:…`.
- Memory blocks: `curl :7073/api/alpha/evidence/e-83ece32c-9e01-42aa-86d8-0a5a6e6265f5`
  (and `e-021bf80a…`, `e-c86a7d45…`, `e-f6b4c56b…`).
- Receipts: `…-f29/live/student-attempt-3.edn` (`:receipt/memory-use`,
  "transfer-from-artifact-replay"), `…-f30/live/student-attempt-{1,2,3}.edn`
  (`:dirty? true`).
- Timeline: `…-f{29,30}/problem-transitions.edn`.
