# Codex handoff (DRAFT — scope only): M-typed-holes manifest × mathlib4 audit

**Status:** DISPATCHED to Codex (2026-06-14). **Owner of result:** claude-2
(reviewer); Codex (author).
**Goal:** For each concept in `holes/missions/M-typed-holes-lean-manifest.edn`,
decide its mathlib4 status (PRESENT / PARTIAL / ABSENT); for anything not fully
PRESENT in mathlib4, **web-search for formalisations in other systems** (Coq,
Agda, Idris, …); then scope the remaining build. This is an AUDIT — no
formalisation, no new Lean code.

## Inputs (READ-ONLY)
- `futon3c/holes/missions/M-typed-holes-lean-manifest.edn` — 10 concepts, each
  with a `:ct-target` (the categorical object/morphism to look for).
- `futon3c/holes/missions/M-typed-holes.md` — context (the Poly framing).
- **Full mathlib4** (source tree + Loogle / Moogle / `exact?`/`#find`). NOT the
  local `futon6/data/mathlib-defs.json` — that is a **3454-decl slice**, good
  only for confirming presence, never for concluding absence.

## Task — per manifest entry, produce
- `:status` ∈ `{:present :partial :absent}` for the `:ct-target`.
- `:decls` — exact mathlib declaration name(s) + file path, if present/partial.
- `:gap` — what's missing (for :partial/:absent).
- `:build-scope` — for :partial/:absent: what mathlib gives you to build on +
  a one-line approach.
- `:difficulty` ∈ `{:S :M :L}` (S = hours on existing API, M = days, L = research).
- `:confidence` + how verified (Loogle query / file cite).

## Pre-probe (from the 3454-decl slice — VERIFY against full mathlib, don't trust)
- **typed-hole / Poly** — `PFunctor`, `MvPFunctor` PRESENT (slice). Likely
  `Mathlib/Data/PFunctor/Univariate/Basic.lean` + `Multivariate/`. CONFIRM it is
  the Σ_{i}y^{p[i]} positions-and-directions form and check `PFunctor.W` (W-types)
  + `PFunctor.M` (M-types).
- **fill / substitution** — look for `PFunctor.comp` / polynomial composition;
  the slice showed no `*.comp` for PFunctor → likely :partial (composition may
  need building). VERIFY.
- **discharge-duality / fill (counit side)** — `Comonad`, `Coalgebra`,
  `CoalgebraStruct` PRESENT (slice). Assess whether the counit/discharge maps
  onto these.
- **comb / lens** — optics are (to our knowledge) NOT in mathlib4 → expected
  :absent. CONFIRM (search Lens/Optic/Iso-but-not-the-comb-sense).
- **bv-hole-algebra (deep inference: seq/par/copar, medial rule)** — expected
  :absent (BV is not in mathlib). CONFIRM.
- **fill-laws / scope-as-query / satiety-grading / illocutionary-hole** — these
  are OUR concepts, not standard math; expect :absent as named, but their
  substrate (operad/monoid for fill-laws; presheaf/representable for
  scope-as-query; grading for satiety; the IATC dialogue for illocution) may be
  partially present. Report the substrate, mark the concept :absent.

## Cross-system search (for any concept NOT fully :present in mathlib4)
Lenses/optics and Poly are heavily formalised by the CT community OUTSIDE
mathlib4 — so before concluding "must build from scratch," **web-search** for
existing formalisations in other systems and record them:
- **Coq** (e.g. profunctor-optics / lens libraries, coq-ext-lib, MathClasses),
- **Agda** (`agda-categories`, `1lab`, `agda-stdlib` — 1lab has optics/lenses,
  displayed cats; `agda-categories` has comonads/coalgebras),
- **Idris 2** (optics/lens libs),
- **Lean community / non-mathlib** (Poly / categorical-systems-theory ports),
- **the source literature** (Spivak–Niu *Polynomial Functors*; Riley's optics;
  Guglielmi's BV / deep inference) — note the canonical reference even when no
  proof-assistant formalisation exists.
Record per concept: `:other-formalisations [{:system :decl-or-lib :url :notes}]`,
with a one-line `:portability` judgement (how transferable to Lean/our use).

## Output (create)
`futon3c/holes/missions/M-typed-holes-mathlib-audit.edn` — keyed by `:concept`,
each `{:status :decls :gap :other-formalisations :portability :build-scope
:difficulty :confidence}`, plus a `:summary` `{:present N :partial N :absent N
:critical-path [...]}` naming which concepts unlock the most downstream (likely
typed-hole→fill→comb).

## Acceptance / gates
- Every `:present`/`:partial` cites a REAL declaration name + path (Loogle/Moogle
  verifiable) — no "probably exists."
- Every cross-system formalisation cites a REAL lib/decl + URL.
- Distinguish slice-present from full-mathlib-verified.
- No formalisation; audit + scope only. Bell **claude-2** back with the audit
  path + summary counts.
