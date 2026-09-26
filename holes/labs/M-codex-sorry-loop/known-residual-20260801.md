# M-codex-sorry-loop — known residual, 2026-08-01

Written by claude-9 (ground control) at the point where the loop ran out of
procedural work. **Everything closable by proving something has been closed.**
What remains needs an operator decision, is deliberately excluded, or is
library hygiene.

## How the numbers were measured

Counted against **committed HEAD** (`git show HEAD:<path>`), never the working
tree — runners hold files open and a mid-edit tree over-reports progress. Lean
comments are stripped with newlines preserved before counting, because a naive
`grep sorry` matches the word in docstrings; six files read as holed that way
and are in fact clean. A definition-free `opaque` counts as a hole. Re-derive
with `futon3c/scripts/queue_audit.py`, which checks the queue against the repo.

| | session start | now |
|---|---|---|
| canonical problem files hole-free | 100 / 145 | **137 / 145** |
| `ConstructionTargets` modules hole-free | 10 / 15 | **15 / 15** |

Queue: 88 rows, audits clean, no stale statuses.

---

## Part A — statement defects (6 rows). **These need Joe, not a runner.**

Held at Joe's instruction. Each is a question about what the problem *says*,
not about whether we can prove it. Evidence strength is stated per row: a
**machine-checked refutation** means a countermodel was compiled axiom-clean;
**argued** means a counterexample was given but not formalised in-repo.

### A1. a95J04 — `exists_pole_on_boundary` (1 hole, line 134). Machine-checked false.

**Two defects; the first was repaired and the second was found afterwards by a
runner.** The original `(hR : True)` no-op binder was replaced with a real
non-extendability condition on 2026-07-31. But `hf_mero` is *also* defective and
always was: it constrains `f` only **at** each point `z`, not on a punctured
neighbourhood. A runner proved **axiom-clean** that *every continuous function*
satisfies it (`continuous_satisfies_pointwise_pole_alternative`), with the
concrete counterexample `f z = (max (normSq z - 1) 0)^2` satisfying every
hypothesis and falsifying the conclusion.

*Proposed repair:* genuine meromorphicity on `D` — punctured-neighbourhood
holomorphy with nonzero finite principal part — or holomorphy near the closed
disk away from finitely many genuine poles.

*Note for the vacuity checks:* 0a/0b/0c **cannot** catch this. `hf_mero` is
syntactically rich (disjunction, existential, differentiability, domain
quantifier) and semantically empty. This motivated proposed check **0d**: for
each hypothesis, test machine-checkably whether some weak plausible property
already implies it. That distinguishes an *absent* guard from an
*under-strength* guard, which is the failure mode that got past us.

### A2. a95A08 — `disk_not_subset_image_of_length_gt` (1 hole, line 137). NOT a defect — a frontier.

Its sibling `rotation_of_disk_subset_image` **was** machine-checked false —
`f(z) = z - 3z²` is differentiable on the ball, `f 0 = 0`, `‖deriv f 0‖ = 1`,
`ball 0 1 ⊆ f '' ball 0 1`, and not a rotation — refuted axiom-clean in-file and
independently verified. That target is settled.

The remaining hole is different: it needs a **global analytic inverse on the
disk**. Targeted search found local inverse APIs (`toOpenPartialHomeomorph`) but
no packaged global one; Zulip returned nothing.
`ConstructionTargets.SchwarzEquality` only covers the subsequent
disk-self-map step. **This is the one residual item that is genuine missing
mathematics rather than a decision** — a candidate ConstructionTarget if the
loop resumes.

### A3. a01J06 — `weight_summable_of_zeroCountLinear` (1 hole, line 348). Machine-checked false; modelling defect.

Three separate defects, all visible in the Lean:

1. `hEntire : True` — a no-op binder; `f` is never required to be entire.
2. `zeros : ℕ → ℂ` is a bare field with **nothing** tying it to `f`. No
   hypothesis says `f (zeros n) = 0`, so the zero sequence is arbitrary.
3. `zeroCountLinear` is proved unconditionally by `zeroCountLinear_trivial` and
   re-exposed as a `@[simp]` default, so `h : data.zeroCountLinear` carries no
   information.

Together these make `Summable data.weight` refutable by choosing divergent
weights — done machine-checked as `unconstrainedZerosCounterexample`.

*Proposed repair:* replace `hEntire` with `Differentiable ℂ f`; add the missing
model invariant `∀ n, f (zeros n) = 0` plus a completeness/multiplicity
condition; restate `zeroCountLinear` as an actual counting bound.

### A4. a01J05 — Blaschke / Gauss–Lucas (5 holes, lines 316–362). Argued false; repair refused.

The file contains `ConvexHull` where `convexHull` is meant. **A runner refused to
make that fix and was right to**: correcting the typo produces a *false*
theorem. Counterexample: `f(z) = z² - 4` is analytic, its zero set inside the
unit disk is **empty** (zeros at ±2), so the convex hull is empty — yet
`deriv f = 2z` vanishes at `0`, which is in the disk. A Gauss–Lucas-shaped claim
that fails without further hypotheses. File deliberately left untouched, no
commit.

*Proposed repair:* add boundary-unit / finite-Blaschke hypotheses; replace the
Euclidean convex hull with a **hyperbolic** convex hull; represent zeros **with
multiplicity** (the current `Set`/`Finset` cardinality does not faithfully
formalise the problem's multiplicity count).

This is the largest single residual — 5 of the 8 remaining holes — and all five
sit behind one modelling decision.

### A5. a95A02 — `ac_maps_measurable_to_measurable`. File is hole-free; the *statement* is in question.

Not false, and not a hole — the file elaborates clean. The issue is a
formalisation mismatch: the σ-compact-plus-null route proves **completed
Lebesgue** measurability, while Lean's `MeasurableSet` means **Borel**, and
continuous images of Borel sets need not be Borel.

*Joe decides:* add an image-regularity hypothesis, or restate in completed
measurability. Either is defensible; they formalise different theorems.

### A6. t94J01 — `connected_of_ladderConnected`. File is hole-free; statement machine-checked false.

False for `X = Empty`. `LadderConnectedSpace` quantifies `∀ p q : X`, so it holds
**vacuously** on the empty space, while Mathlib's `IsConnected s` requires
`s.Nonempty`. The countermodel was compiled axiom-clean and independently
verified. `connected_iff_ladderConnected` inherits the defect.

*Proposed repair:* add `[Nonempty X]`, **or** use `IsPreconnected` if empty
spaces should count as connected. A one-token decision about intent.

### (A7. a01A05 — `:wontfix`, already settled.) Statement false; recorded, no action pending.

---

## Part B — held-out evaluation (4 files). **Not a residual. Do not touch.**

`bpm-1-1-2`, `bpm-1-3-2`, `bpm-1-7-1`, `bpm-1-8-1` — one hole each. Deliberately
excluded from the loop as a held-out set. They appear in the with-hole count and
should be subtracted from any "remaining work" figure.

---

## Part C — library debt (2 modules). Measured, not suspected.

Both predate the build-in-`ConstructionTargets` rule, and both were built by
**copying a problem file rather than extracting from it**. Audited across all 15
modules by claude-5; the other thirteen are clean.

### C1. `LemniscateComponents` — net delivered value **zero**

23 of 24 declarations shared with `problems/a00J04/lean/Main.lean`, 21
byte-identical. Its sole unique lemma
`frontier_connectedComponentIn_subset_frontier` is referenced **nowhere in the
repo**. **a00J04 is closed at zero sorries and never imported the module** — the
problem it was built to unblock closed without it. Two of the shared names have
already diverged, which is the drift the copy pattern creates.

*Recommendation:* **propose deletion to Joe**, do not wire a00J04 up to it —
that would be churn on a closed problem with statement-integrity risk for no
gain. Deleting a module is an operator call.

### C2. `LusinN` — net unique contribution **one lemma**

17–18 of its declarations shared with `problems/a95A02/lean/Main.lean`,
byte-identical, and a95A02 does **not** import it, so they are two independent
copies and a fix to one does not reach the other. Unlike C1 its unique lemma
`absolutelyContinuousOnInterval_maps_null_to_null` *is* consumed (a95J06:308,
`BanachZarecki.lean:4`).

*Cleanup, specified but held:* import + `open ConstructionTargets.LusinN` so
statement text stays byte-identical, delete the 17 local copies, keep a95A02's
unique part-(c) declaration. **Held because a95A02 is one of the six defect rows
above** — the refactor is orthogonal to the Borel-vs-Lebesgue question, but
touching a held file is Joe's call.

*Contrast case:* `BanachZarecki` — 39 declarations, **zero** overlap with
a95J06. That is what extraction looks like, and it proves rule R2 is followable.

### C3. Orphan

`KernelAverage` (2 declarations) has zero importers. Not duplication; just
unused. Its failed general form is what specified the successful
`LpRepresentative`, so it earned its keep historically.

---

## Part D — infrastructure residuals

### D1. The ~30-minute Agency job cap silently loses results. **Open.**

codex-9's `RadialMajorantAE` round 3 ran 14:01:47 → 14:32:03, hit the cap, and
was marked `state=failed` with a zero-length result and `(no summary)` in the
park wake. **The work was complete** — five commits had landed and the module
was at zero holes with the target theorem proved and axiom-clean.

**`state=failed` means the job died, not that the work did.** Recovery is to
read git, not the ledger. Mitigation now in the packet template: *commit
incrementally, because a commit is durable and a job result is not.* A real fix
belongs in the codex relay route (`agency_send.py --help` documents the
supervised-overrun gap).

### D2. Park delivery to CLI-hosted agents. **Fixed this session.**

`parked-resume!` branches on `buffer-surface?` = `starts-with "emacs"`.
`agency_send.py` defaults `--surface emacs-repl`, which routes the resume to an
Emacs `claude-repl-mode` buffer poller that CLI-hosted agents do not have — so
every park either ground-control agent made went to a mailbox nobody read.
Evidence was 5 released-and-undelivered resumes in
`/tmp/futon3c-parked-on.edn` with `:leased {}` empty. **Fix: `--surface
headless`.** Note the trap is the `emacs` *prefix*, not the literal default —
a session announcing `emacs-claude-repl` falls in too.

### D3. Queue-field enforcement. **New this session.**

`futon3c/scripts/queue_audit.py` re-derives seven predicates from committed
HEAD and reports disagreements between the queue and the repo (`:status
:resolved` vs actual holes, `:sorries-after`, `:line`, `:last-commit`, `:file`,
duplicate rows, terminal jobs on `:dispatched` rows). Written because every gate
field on a row was *asserted* and checked by nothing — the family that produced
this session's stale statuses and duplicate rows. It found a real stale `:line`
on its first clean run.

Five files carried **two rows each** (legacy `sorry-NNNN` plus working
`hard-problems-*`), and two of the five disagreed about status. Restatused to
`:superseded-by-working-row`; 88 rows cover 83 distinct files.

---

## Part E — what to carry forward

1. **The review topology that worked is a triangle, not author-vs-reviewer.**
   Twelve cross-agent corrections, and the runner leg carried the most weight —
   including two *false theorem statements* dispatched by ground control
   (claude-9's `LocallyIntegrable` a.e.-convergence theorem, refuted by
   `exp(x²)`; claude-5's `poissonKernel_sub_star_ge`, false without `1 < R`).
   **Both were caught by runners; neither was caught by the other reviewer.**

2. **The vacuity ladder has three rungs, not two.** 0a/0b/0c and the tautology
   check ask *is the conclusion contentful*. Then: **vacuity** — is the
   hypothesis class inhabited? Then: **non-triviality** — is it inhabited by
   something for which the conclusion is not degenerate? Check the last two by
   *compiling a witness*, not by eyeballing. And 0d (proposed): does some weak
   plausible property already imply a hypothesis?

3. **Some verification failures point toward alarm, and those are the dangerous
   ones.** `#print axioms Ns.foo` → `unknownIdentifier` when the namespace
   closed early; a stale olean → false `sorryAx`; a half-remembered lemma name →
   `unknownIdentifier`. Each reads as the *strongest possible* evidence of
   failure. **Re-derive the name or rebuild before believing a negative.**

4. **Prose asserting absence goes stale and blocks work.** Six instances. The
   worst was `ConstructionTargets.lean` itself, which listed three sorry-free
   modules as partial and named a94A10 as a `Rouche` consumer — the exact wrong
   dependency edge that misrouted dispatches for a day, sitting in the library's
   own documentation. Fixed at `d6b1bc3` with the consumer column **derived by
   grep** rather than asserted.

5. **A named obligation is what travels.** Rows that closed did so by executing
   the route their previous attempt recorded. Recording what does *not* work was
   as load-bearing as recording what might — a95J06's interval-cover dead end
   (subadditivity points the wrong way) and a96A04's compact-support dead end
   each saved a round.

6. **Sorry count is the wrong meter.** a96A04 read 1 → 1 → 1 across three rounds
   and every metric scored it as three failures, while its unproved dependency
   set went from "compact-support API unusable" → "need a uniform envelope" →
   "need the absolute-coefficient polynomial bound" → closed. *The dependency
   set is the thing with a derivative.*

7. **Build the library detached, then point the hungry problems at it.** Joe's
   sequencing rule. Four problems closed today by consuming a ConstructionTarget
   built the same day, and it produced three theorems absent from Mathlib:
   entire-Pick rigidity, monotone Banach–Zarecki with the Stieltjes-quantile
   pushforward, and the radial-majorant a.e. bridge.

8. **Duplication is the write-side blind spot of retrieval.** "Does this fact
   already exist?" is never asked, and its false negatives are silent and
   compounding while "does it exist?" false negatives are loud and bounded. A
   retrieval system tuned for precision will *manufacture* duplication and never
   see it, because a duplicate is a successful-looking authoring event. Sent to
   claude-2 for the V3 writeup; see also the norm-priority point — nobody
   noticed LusinN for a week because **nothing in anyone's optative structure
   wanted one copy**, so the norm had to be authored before the encounters
   became legible.

Rules for building a ConstructionTarget (R1 build-graph, R2 move-don't-copy,
R3 prove-the-theorem-is-true, R4 stale-absence-prose) are in
`construction-targets.md` in this directory, each traced to the defect that
produced it.
