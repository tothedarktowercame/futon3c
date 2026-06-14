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

**WHICH "Comb" (Joe — formalise the CORRECT one):** the base object is a
**dependent lens = a morphism of polynomial functors** (a.k.a. a Poly morphism /
"Spivak lens"). The defining data and — the crux — the *directions*:
- `onPos : P.A → Q.A` — forward on positions;
- `onDir : (a : P.A) → Q.B (onPos a) → P.B a` — **BACKWARD** on directions
  (from `Q`'s directions at the image position back to `P`'s directions). Getting
  this direction right IS the correctness condition; a forward `onDir` is the
  wrong object.
This 1-hole comb (= dependent lens) is the base; the **`comb` shape proper**
(n-hole, sequential `⟨A;-;B;-;C⟩`) is the coend/open-diagram generalisation —
note it in the docstring as the intended next layer, do NOT build it yet.

Literature pointers (cite the right ones, exclude the wrong):
- **Niu–Spivak, *Polynomial Functors: A Mathematical Theory of Interaction*
  (arXiv:2312.00990)** — Poly morphisms = dependent lenses; the canonical target.
- **Spivak, *Generalized Lens Categories via functors Cᵒᵖ → Cat* (arXiv:1908.02202)**
  — dependent lenses as morphisms.
- **Riley, *Categories of Optics* (arXiv:1809.00738)** — the general optic; a lens
  is the cartesian case.
- nLab "dependent lens" / "polynomial functor" (the morphism section).
- multi-hole generalisation (FUTURE, note only): **Román, *Open Diagrams via
  Coend Calculus* (arXiv:2004.07353)** and **Román, *Comb Diagrams for
  Discrete-Time Feedback* (arXiv:2003.06214)** — the actual "comb" string-diagram.
- blueprints for the laws: Agda `ps-lenses-agda`; `sinhp/Poly` morphisms.
- **NOT** these: graph-theoretic combs; quantum combs in finite-dim Hilbert
  spaces (Chiribella–D'Ariano–Perinotti 2009 — the physics origin, wrong setting);
  cofree-comonad "combs". We want the categorical Poly/lens object.

- `structure Comb (P Q : PFunctor)` with `onPos`/`onDir` as above; `id` + `comp`
  (composition reverses on directions); state the lens/comb laws (prove what's
  clean, `sorry`+TODO the rest). Match DarkTower house style + literature grounding.
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

### WAVE 3 — by-example instantiation (Joe, 2026-06-14): concepts via exemplars

The "futon6 first rung BY EXAMPLE, not a rollout" — and it must span the TWO
reasoning domains Joe named: **missions** and **papers / research-programmes**
(the latter building on the math.ct representation). Instantiate the landed
DarkTower vocabulary (`TypedHole`/`Fill`/`Comb`/`Discharge`/`ScopeQuery`/`BV`) on
the actual exemplars, in Lean, so the unification is shown by example.

**T7 · DarkTower/Examples.lean** (difficulty M; depends on all wave-1/2 files).
Two namespaces, both compiling with `#eval`/`example … := rfl` witnesses, 0-sorry
target:

- `namespace MissionExample` (E-mission-head — the MISSION domain):
  - the lifecycle as a `BV.seq` chain of phase atoms
    `⟨HEAD ; IDENTIFY ; … ; DOCUMENT⟩` (use the `BV` from BV.lean; an `inductive
    Phase` for the atoms);
  - the two readings (scope ∥ organism) as `BV.copar` (held together);
  - a ghost / unwritten phase as a `TypedHole` position with `satiety` = a hungry
    grade; "writing a phase" as a `Cong`/`Step` move (or a `Fill`);
  - example lemmas: the seq chain reassociates (`BV.Cong`), the ghost is a hole.

- `namespace PaperExample` (math.ct grounding — the PAPER / research-programme
  domain; data from 0809.2517 "Let `H` be a Hopf algebra"):
  - the ungrounded symbol `H` as a `TypedHole` position hungry for its concept;
  - grounding (the use→binder edge) as `Discharge` (the `ungrounded↔binder`
    polarity of `discharge-duality`) and/or a `Fill`;
  - "which concept grounds `H`?" as a `ScopeQuery` over a tiny binder-store
    (`(symbol H, ?concept)` → `Hopf algebra`), answered by `rfl`;
  - example: ungrounded = open hole, grounded = filled, by `rfl`/`decide`.

The point (state it in the module docstring): the SAME `TypedHole`/`fill`
machinery types a mission and a paper — `(typed-hole, fill)` is domain-agnostic;
missions and papers are two instantiations, not two theories. M-first-flights
(the cascade→sorry→wiring fold) is a good THIRD example for a later pass.
- **Acceptance:** compiles; both namespaces present with `rfl`/`decide` witnesses;
  ≤2 sorries (TODO'd); literature/exemplar sources cited in the docstring
  (E-mission-head.md; the 0809.2517 golden graph). You may commit.

**T8 · DarkTower/FirstFlightsExample.lean** (difficulty M; the THIRD by-example,
the M-first-flights cascade→sorry→wiring fold — the *fill-as-graph-rewrite-fold*
grain not yet shown in Lean). Distinct file (don't edit the committed
Examples.lean). Source: `futon3c/holes/missions/M-typed-holes-example-first-flights.md`
+ `futon3c/holes/flights/first-flights-wiring.edn`.
- `namespace FirstFlightsExample`, compiling with `rfl`/`decide` witnesses, 0-sorry
  target. Model:
  - the checkpoint wiring as a small `BV.seq` chain of checkpoint atoms (a 3–4
    checkpoint slice is fine — represent the mined `:composes` chain);
  - the **sorry** = the unfilled want-port as a `TypedHole` position with a hungry
    `satiety` (the `:hungry-for :payoff` port);
  - the **cascade** = a tiny `ScopeQuery` store of candidate patterns; selecting
    the matching one = answering the query (the fill) — answer by `rfl`;
  - a `:jointly-with` pair as `BV.copar` — exercise the connective the mined
    linear `:composes` could NOT (the excursion's gap #3), shown here by example;
  - the **fold**: filling the want-port hole (a `Fill`/`Discharge`), with an
    example that the filled checkpoint is no longer hungry.
- Docstring point: this is the *fold* grain of `fill` (cascade-selected
  graph-rewrite), complementing the atomic `PFunctor.comp` grain — the two grains
  named in `M-typed-holes-example-first-flights.md` §4.
- **Acceptance:** `lake build DarkTower.FirstFlightsExample` clean; `rfl`/`decide`
  witnesses; ≤2 sorries (TODO'd); cites the exemplar sources. You may commit.

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
