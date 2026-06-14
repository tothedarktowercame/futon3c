# Codex handoffs: M-typed-holes → Lean (DarkTower)

**Status:** SCOPED. Dispatch wave 1 (foundational, mathlib-backed) to the Codex
pool; wave 2 (greenfield) held for design. Owner-of-result: claude-2 (reviewer,
real gate). Source of truth: `M-typed-holes-lean-manifest.edn` +
`M-typed-holes-mathlib-audit.edn` (codex-3, reviewed `6243ed3`).

## House rules (every task)
- Add ONE file `mathlib4/DarkTower/<Name>.lean`, matching the existing DarkTower
  style: `import Mathlib`; module docstring with **literature grounding**
  (canonical ref + nLab); `namespace DarkTower`; `open CategoryTheory`; a
  `structure` (or def) + `id` + `comp` where it makes sense; doc-comment every
  field.
- Register it: add the file (the `globs := #[\`DarkTower.+]` lean_lib picks it up
  automatically — no lakefile edit needed).
- **Gate:** `lake build DarkTower` is clean and the new file has **0 `sorry`**.
  If a law genuinely needs a hard proof, mark it `sorry` AND leave a `-- TODO`
  with the obstacle — but prefer a complete, smaller statement over a sorried
  big one. clj-kondo/parens N/A (Lean).
- Build ON mathlib (the audit cites exact decls/paths) and, where noted,
  consult `sinhp/Poly` (Lean 4: composition, monoidal `C[X]`, differentiation —
  https://github.com/sinhp/Poly) for proof ideas. Do NOT vendor it; reference.
- Bell **claude-2** back with: file path, `lake build` result, sorry count, and
  the decls you built on. One concept per bell.

## Critical path (from the audit): typed-hole → fill → comb → scope-as-query.

---

### WAVE 1 — dispatch now (mathlib-backed; S/M, C is L-but-blueprinted)

**T1 · DarkTower/TypedHole.lean** (difficulty S; covers typed-hole + hole-type +
satiety-grading). Build on `PFunctor` (`Mathlib/Data/PFunctor/Univariate/Basic.lean`:
`A : Type`, `B : A → Type`) and `CategoryTheory.GradedObject` /
`Mathlib/Data/FunLike/Graded.lean`.
- `structure TypedHole` wrapping a `PFunctor` (positions = nodes, `B a` = the
  typed directions/holes at a node); a `holeType` accessor (`= B`); a
  `SatietyGrade` finite tag type (`:parse :payoff :canon :bundling :role`) and a
  `satiety : (a : P.A) → SatietyGrade` grading of positions, with an `eval`/
  projection consistent with `holeType`.
- Acceptance: compiles, 0 sorries; a `#check`/example showing a node with a
  typed hole and its satiety tag.

**T2 · DarkTower/Fill.lean** (difficulty M; covers fill + fill-laws). Build on
`PFunctor.comp` / `comp.mk` / `comp.get` (`Univariate/Basic.lean:148`).
- `def fill` selecting a position + direction and substituting (thin wrapper over
  `PFunctor.comp`); an identity polynomial `I`; prove **left unit, right unit,
  associativity** of composition/fill (the monoid-of-fills). Consult `sinhp/Poly`
  for the associativity encoding.
- Acceptance: compiles; unit + assoc lemmas proved (0 sorries) OR the one
  genuinely-hard lemma sorried with a TODO + the sinhp/Poly pointer.

**T3 · DarkTower/Comb.lean** (difficulty L; covers comb = dependent lens =
PFunctor morphism). mathlib has NO optics (audit: zero Lens/Optic/Tambara).
Blueprint: Riley *Categories of Optics* (arXiv:1809.00738); Agda
`ps-lenses-agda`; `sinhp/Poly` morphisms.
- `structure Comb` (a dependent lens / `PFunctor.Hom`): `onPos : P.A → Q.A` and
  `onDir : (a : P.A) → Q.B (onPos a) → P.B a`; `id` + `comp`; state the lens
  laws (prove what's clean, sorry+TODO the rest).
- Acceptance: structure + id + comp compile; laws stated; ≤2 sorries, each TODO'd.

**T4 · DarkTower/Discharge.lean** (difficulty M; covers discharge-duality). Build
on `CategoryTheory.Comonad` + counit `ε` (`Monad/Basic.lean:67/69`) and
`Comonad.Coalgebra` (`Monad/Algebra.lean:260`).
- Represent a hole + its discharge (sorry↔proof / query↔answer / ungrounded↔
  binder) as a counit/coalgebra instance over a position; show the discharge
  commutes with `fill` (cross-ref T2 if landed, else state the interface).
- Acceptance: compiles, 0 sorries for the instance; the commutation may be stated.

### WAVE 2 — held for design (greenfield; dispatch after wave 1 + a design note)

**T5 · DarkTower/ScopeQuery.lean** (L). scope-as-query: a partial hyperedge as an
element of a representable presheaf (`CategoryTheory.Functor.RepresentableBy`,
`Yoneda.lean`); "answer" = `fill` over a store. Needs a small design note on the
hyperedge signature first — hold.

**T6 · DarkTower/BV.lean** (L, greenfield). BV structures (seq/copar/par),
structural congruence, medial/switch rules. NO proof-assistant formalisation
exists anywhere (audit) — pure greenfield from Guglielmi (arXiv:cs/9910023).
Biggest task; dispatch last, alone, with a design pass first.

## Dispatch table (wave 1)
| task | file | difficulty | depends on |
|---|---|---|---|
| T1 | DarkTower/TypedHole.lean | S | PFunctor, GradedObject |
| T2 | DarkTower/Fill.lean | M | PFunctor.comp (+sinhp/Poly) |
| T3 | DarkTower/Comb.lean | L | PFunctor (+Riley/Agda/sinhp) |
| T4 | DarkTower/Discharge.lean | M | Comonad/Coalgebra |

T1 and T2 are independent of each other at the file level (T2 can reference T1's
`TypedHole` but needn't block on it); T3/T4 independent. Safe to run in parallel
across the pool.
