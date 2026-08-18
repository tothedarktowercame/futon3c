# Excursion: t01A05 repair plan

Date: 2026-08-18

Scope: discovery only.  No problem bundle or live frame branch was modified.

## Finding

The earlier formalisation is recoverable from Git, but it is **not** a repair
that should be restored.  At Mathlib revision
`5ee50502f950564a1bc3f4a0bb7809b94e65a49a` (the pin is recorded at
`apm-lean/lake-manifest.json:4-11`), Mathlib has enough singular homology to
state an honest reduced surrogate, but not the geometric APIs needed to state
and prove the source problem's differential-form integral and intersection
number claims directly.  The honest repair is therefore a reduced statement
that takes both orientations as data and proves only a non-vacuous
fundamental-class relation, while explicitly marking source part (c) deferred.
That changes the frozen statement and must not land without Joe's sign-off.

## 1. What history contains

The four names in the current outline (`apm-lean/problems/t01A05/proof-outline.md:9-18`)
all occur in the historical `Main.lean`:

- commit `b9c6503` contains `t01A05_pullback_integral_eq` at historical lines
  67-72, `t01A05_preimage_intersectionNumber_eq` at 76-84, and
  `t01A05_preimageSubmanifold_compact_of_t2` at 88-95;
- commit `f84008b` additionally contains
  `t01A05_preimageSubmanifold_compact_of_isClosed` at historical lines
  100-107.

The first two lemmas are vacuous, not recovered mathematics.  In `b9c6503`,
`t01A05Integral` is definitionally evaluation at zero (historical lines
45-46), while `t01A05IntersectionNumber` is definitionally zero (lines
61-63).  The proofs are consequently `simp` (lines 67-84).  The file itself
says both identities reduce to zero (lines 122-130).  Restoring it would make
the theorem look more faithful while proving neither the source integral nor
the source intersection number.

The history records the correction explicitly.  Commit `1fa9d02` replaces the
vacuous encoding and states that Mathlib lacks the required geometric APIs,
that part (b) is represented by a fundamental-class relation, and that part
(c) is deferred rather than represented by a fabricated constant (historical
lines 17-31).  The present file retains that explanation at
`apm-lean/problems/t01A05/lean/Main.lean:17-31`.  Unfortunately its theorem
then changes a supplied upstairs orientation into an existential conclusion
(`Main.lean:77-96`).

Thus history supplies useful types and a warning, but no sound full solution
to recover.  The stale outline is explained by `1fa9d02`: that commit changed
only `problems/t01A05/lean/Main.lean`, leaving the four claims from `b9c6503` /
`f84008b` in `proof-outline.md`.

## 2. What is formalisable at this pin

The exact environment is Lean `v4.29.0-rc8`
(`apm-lean/lean-toolchain:1`) and Mathlib revision `5ee50502...`
(`apm-lean/lake-manifest.json:4-11`).

### Direct source parts (b) and (c): not supported as packaged mathematics

The pinned tree has differential forms on normed vector spaces
(`.lake/packages/mathlib/Mathlib/Analysis/Calculus/DifferentialForm/Basic.lean:14-24`)
and curve integrals of 1-forms
(`.lake/packages/mathlib/Mathlib/MeasureTheory/Integral/CurveIntegral/Basic.lean:16-38`).
It does not have integration of top differential forms over oriented
manifolds.  The manifold partition-of-unity file describes such integration
as a prospective use, not an implemented construction
(`.lake/packages/mathlib/Mathlib/Geometry/Manifold/PartitionOfUnity.lean:54`).
There is likewise no packaged oriented-submanifold/intersection-number or
Poincare-duality API in the pinned `Mathlib/Geometry/Manifold` tree.  This is
also the boundary recorded when the current encoding was introduced
(`apm-lean/problems/t01A05/lean/Main.lean:22-31`).

Building those foundations inside one problem bundle would be a new library
project, not a repair.  The old substitute does not help: its top form is a
homology morphism and its “integral” evaluates that morphism at zero
(`b9c6503:problems/t01A05/lean/Main.lean:36-46`); its intersection number is
constant zero (`b9c6503:.../Main.lean:61-63`).

### Honest reduced target: supported as a statement

The current file already uses actual integral singular homology and its induced
map (`Main.lean:43-50`), and represents an orientation by an explicit
top-homology isomorphism (`Main.lean:52-59`).  A faithful reduced theorem can
therefore take **both** `o : T01A05Orientation n X` and
`ot : T01A05Orientation n Xtilde` as given data and assert
`t01A05FundamentalClassRelation ... π ot o` (`Main.lean:61-66`).  This captures
the homological content behind part (b) without pretending to provide
differential-form integration.  It must say plainly that part (c) remains
outside this pin.

This is only statement-level feasibility.  The existing `sorry` at
`Main.lean:96-97` shows that the covering-degree theorem itself is not already
proved.  A subsequent repair packet must elaborate the proposed exact theorem
before claiming redispatch readiness.

## 3. Would `[ConnectedSpace Xtilde]` repair the current theorem?

It is a defensible **truth repair** for the known disconnected degree-zero
counterexample, but not a problem repair, so I do not recommend it as a
separate landing.

The current theorem assumes `0 < k` and compact manifold structures
(`Main.lean:84-92`) but asks, for every downstairs orientation, to construct an
upstairs orientation (`Main.lean:93-96`).  Adding connectedness excludes the
two-point `n = 0`, `k = 2` witness and moves the surrogate toward the standard
connected closed-manifold degree theorem.  It still does not encode the
source's premise that **both orientations are given**
(`apm-lean/problems/t01A05/problem.md:19-21`), and it still omits the actual
integral and intersection conclusions (`problem.md:28-37`).  The informal
proof makes the supplied orientation-preserving data load-bearing for signs
(`informal-solution.md:14-23`, `55-80`, and `106-125`).

Connectedness would therefore make a narrower homological surrogate plausible
without making it t01A05.  It also unnecessarily rejects legitimate
disconnected oriented covers: the source requires compact oriented manifolds,
not connected ones (`problem.md:19-21`).  The structural repair is to change
the quantifiers so `ot` and `o` are inputs, not to add a hypothesis solely to
block the discovered model.

## 4. Exact repair surface and approval

A repair should touch exactly these three bundle files:

1. `apm-lean/problems/t01A05/lean/Main.lean` — replace the false frozen theorem
   by the reviewed reduced theorem: supplied orientations, a non-vacuous
   fundamental-class relation, and explicit deferral of unsupported part (c).
2. `apm-lean/problems/t01A05/proof-outline.md` — remove the four stale “proved”
   claims at lines 9-18 and describe only what the repaired file actually
   establishes and defers.
3. `apm-lean/problems/t01A05/status.json` — update the Lean sorry counts at
   lines 12-20, classification at line 32, and closer-progress text at line 33
   from the elaborated repaired artifact.

`problem.md`, `problem.tex`, and `informal-solution.md` are source evidence and
must remain unchanged.  No pipeline-wide checker is warranted or proposed.
The live `exp/frame-11-t01A05-solver` branch and its commits remain immutable
frame evidence.

This repair changes `apm_t01a05` at `Main.lean:77-96`, i.e. the frozen formal
statement.  Joe's explicit sign-off is required before those three bundle
changes land.  The recommended implementation sequence is: approve the reduced
statement verbatim, edit `Main.lean`, elaborate it under the recorded pin, then
derive the outline and status metadata from that result.
