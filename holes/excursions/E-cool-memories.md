# E-cool-memories: from temporal memory laws to proofs as diagrams

**Status:** COMPLETE — P0–P7 and the general normalization path landed
2026-10-13, with the scope-evaluation limitations recorded below.

**Repos:** `minus40` (the system and first laws), `futon3c`
(`M-diagramprover`, later scope comparison), and a Lean workspace to be chosen
before implementation.

## Aim

Build a small, falsifiable instance of **a proof is a diagram**. Start with
behavioural laws of the working 40ºBelowDB (`minus40`, abbreviated M40°)
temporal-memory system. Prove one law in Lean as a typed composite of named
diagram rewrites, rather than drawing a picture after an ordinary proof has
finished.

If that works, add two bridges:

1. a second theorem about composition of the existing finite open causal
   theories in `futon3c`; and
2. an empirical comparison between a Lean proof's structure and the scopes
   mined from the human-agent conversation in which the proof was developed.

The prospective artifact for Aleks Kissinger is deliberately small:

> one operationally meaningful M40° law, proved in Lean, represented as a
> compositional proof diagram, with the development conversation compared
> against the proof's formal structure.

This excursion records the route and its receipts. The artifact exists at the
bounded level stated here: M40° delta composition has a general checked proof
diagram, with a concrete transient cancellation as its smallest witness.

## The three objects that must not be conflated

For every worked example keep these separate:

1. **System diagram:** the processes and state through which M40° computes.
2. **Proposition:** a claim that two typed composites have the same
   interpretation.
3. **Proof diagram:** a typed composite of named rewrites taking one side to
   the other, each step checkable by Lean.

A tactic trace rendered as boxes is not sufficient. The composition and the
locality of each rewrite must be part of what Lean checks. Conversely, the
diagram engine and renderer need not be trusted if they emit a certificate
that Lean checks.

## First case: compositional snapshot deltas

Let a snapshot be the finite set of facts live at a time. The canonical net
delta between snapshots `A` and `B` is

```text
Δ(A,B) = (A \ B, B \ A)
           remove   add
```

The first semantic law is:

```text
apply Δ(A,B) A = B
```

The first diagrammatic law is:

```text
Δ(A,B) ; Δ(B,C) = Δ(A,C)
```

This should not be made true merely by defining composition to return the
right-hand side. Represent a raw composite as two edit stages, then normalise
it by cancelling edits that start a fact in the first interval and stop it in
the second (and the dual case where applicable). Prove that normalising the
raw composite gives the canonical delta. The proof diagram should expose the
cancellation and reassociation as named local rewrites.

The second diagrammatic law is interchange for independent memories. With a
declared disjointness condition, edits to memory `P` and memory `Q` may be
performed in either order:

```text
(ΔP ⊗ idQ) ; (idP ⊗ ΔQ)
  =
(idP ⊗ ΔQ) ; (ΔP ⊗ idQ)
```

Whether the finished structure is best presented as a monoidal category, a
category of certified edit scripts, or a smaller typed rewrite system is an
implementation question. Do not announce a PROP or groupoid before the Lean
definitions and laws justify it.

### Supporting laws, not headline diagrams

These remain useful specifications and Lean lemmas:

- replay is deterministic over stored versions;
- events later than `t` do not change `asOf t`;
- applying a well-formed delta has the declared source and target;
- delta normalisation preserves its interpretation.

The first two are likely structural-induction proofs over the stored history.
They support the model but do not by themselves exercise the diagram thesis.

## Finding already produced by the proposed law

`minus40/scripts/m40.py`, `cmd_changed`, currently combines two readings of
"changed between `d1` and `d2`":

```python
started = [f for f in rows
           if d1 < f["said"] <= d2 and true_at(f, d2)]
stopped = [f for f in rows
           if f["unsaid"] and d1 < f["unsaid"] <= d2]
```

`started` is a **net snapshot** reading: a fact must still hold at `d2`.
`stopped` is an **event interval** reading: every withdrawal in the interval
is reported. Thus a fact said and unsaid wholly inside the interval appears
only under "stopped being said".

This is a semantic ambiguity, not yet classified as a code defect. Specify two
operations before changing the implementation:

- `netChanged(A,B)`: the compositional difference between endpoint snapshots;
- `eventsBetween(t1,t2)`: every start and stop event in the interval, including
  transient facts.

The Lean theorem concerns `netChanged`. A separate theorem should relate
`eventsBetween` to replay and the two endpoint snapshots. Tests against the
current command decide whether `changed` is renamed, split, or given one of
the two semantics.

## Minimal Lean artifact

The first implementation should contain no database, HTTP, LLM, or M40° file
parser. It formalises the small pure core against which extracted snapshots
can later be checked.

Candidate definitions:

- `Snapshot Fact := Finset Fact` (with the needed decidable equality);
- a typed or source/target-indexed `Delta`;
- `Delta.apply`;
- raw sequential composition;
- delta normalisation and its named rewrite steps;
- interpretation of a delta as a state transformation.

Candidate theorems, in order:

1. `apply_canonical_delta`;
2. `normalise_preserves_apply`;
3. `canonical_delta_comp`;
4. `independent_delta_interchange`.

Mathlib's existing `Mathlib/Tactic/Widget/StringDiagram.lean` was inspected
before implementation. It renders equalities of global morphisms in a
monoidal category, whereas endpoint deltas are state-relative: their equality
holds at a typed source snapshot, not as equality of arbitrary state
transformers. P2 therefore uses a custom Mermaid renderer whose input contains
the Lean-checked rewrite `Path`. Forcing a `MonoidalCategory` instance merely
to satisfy the widget would put the renderer ahead of the semantics. Revisit
the widget only after parallel composition is itself represented in the typed
diagram syntax.

### Acceptance for the first case

- A clean Lean build checks the four definitions/theorems above, or records a
  narrower justified theorem set.
- At least one proof is a composite of named local diagram rewrites; changing
  an intermediate diagram or its boundary causes Lean to reject it.
- The same derivation is rendered with typed boundaries and intermediate
  diagrams.
- Four planted failures are rejected: wrong source snapshot, wrong target,
  invalid cancellation, and interference falsely claimed independent.
- A small exporter turns two real M40° endpoint snapshots into the pure input
  representation and the checked delta reproduces their net difference.
- The current transient-fact case is a regression fixture for the distinction
  between `netChanged` and `eventsBetween`.

## Second case: composition of open theories

After the M40° case, reuse the proof-diagram representation for the existing
`futon3c.diagramprover.causal.open-theory/glue` implementation. Its finite
Boolean fragment already:

- glues theories on declared shared names;
- refuses undeclared sharing, double ownership of a mechanism, and cycles;
- feeds the composite to the existing causal receipts;
- has tests and mutation checks recorded under M-diagramprover's "Skeleton
  use case 2".

The initial candidate was equality with sequential composition. Inspection and
the P5 fixture rejected that statement: `glue` has no independently defined
sequential-composition operation or interpretation. The implemented claim is
smaller. The producer and consumer arrows form a two-box `TypedDiagram` whose
middle boundary is the declared shared variable, and interpreting that diagram
gives the arrow chain exported by the real glued union DAG.

Acceptance: use the same typed syntax and match the real fixture, while
recording the stronger equality as undefined. This tests representation
transfer without manufacturing semantics the Clojure implementation lacks.

## Later bridge: scopes and real proofs

Claude-16's 2026-10-12/13 scope campaign supplies labelled conversational
structure, not a ready-made proof syntax. Its findings constrain the bridge:

- scope openers are principally constraints, plans, mission statements and
  other governing framings;
- cross-turn scopes preferentially target standings;
- `keeps` means that an answer addresses an opener but leaves it open, not
  that it is an intermediate proof result;
- many answer closes are short approvals of proposals, not discharged proof
  goals;
- explicit scope closes are sparse;
- labellers saw kernel ports and standings as candidate targets, so the labels
  are not independent of the running system.

Therefore the first comparison makes only the following candidate mapping:

```text
standing framing  <->  assumption or rewrite available across a proof region
within relation   <->  a step developed while that framing is in force
```

Everything else is a question to measure. For the conversation that develops
one M40° theorem:

1. freeze the turns and their scope labels;
2. freeze the elaborated Lean proof, named lemmas and subgoals;
3. ask whether cited standings correspond to assumptions or lemmas actually
   used;
4. report precision, recall and counterexamples;
5. do not train or change the extractor on the evaluation conversation.

Only after a positive result should we try to compile conversational scopes
into proof diagrams. The intended destination is ordinary mathematical proofs,
but this excursion does not claim that conversational scopes and proof scopes
are isomorphic.

## Elephant 2000: precedent and boundary

McCarthy's *Elephant 2000* is useful external precedent for typed speech-act
I/O, commitments, reference to the past, and the distinction between
input-output and accomplishment specifications. It is not an accurate
description of the current implementation:

- an Elephant program performs formally typed speech acts;
- 象 infers acts from text produced without that type discipline;
- M40° provides temporal history and memory operations adjacent to the
  bookkeeping such a runtime would need;
- the current 象 kernel does not check accomplishment merely because it
  closes an answer port.

The warranted claim, if needed, is:

> 象 and M40° implement parts of the history and commitment bookkeeping
> that an Elephant-like runtime would require, over existing human-agent
> conversations, with additional provenance and scope machinery.

Do not call this an implementation of a substantial fragment of the Elephant
2000 language until typed act production and accomplishment semantics exist.

## War Machine and Markov categories

War Machine deliberately set Markov categories aside until the deterministic
system worked. Preserve that decision. It is a later held-out case for the
diagram language, not a reason to introduce probabilistic semantics now.

If the deterministic proof/certificate layer transfers across M40° and
`open_theory/glue`, a later excursion may ask whether a Markov-category
reformalisation preserves War Machine's existing decisions and refusals while
adding principled observation, intervention and uncertainty. No such result is
claimed here.

## Work plan and progress ledger

| Stage | Exit condition | State |
|---|---|---|
| P0 — freeze semantics | Written distinction and fixtures for `netChanged` versus `eventsBetween`; no production behaviour changed silently | **Complete — minus40 `3b246c4`** |
| P1 — pure Lean core | Snapshots, deltas, apply, raw composition and normalisation compile | **Complete — mathlib4 `de5a2a58ec`** |
| P2 — first diagram proof | Canonical delta composition checked as named rewrites and rendered | **Complete — checked `Path` + Mermaid renderer in `de5a2a58ec`** |
| P3 — adversarial checks | Four planted invalid certificates/diagrams rejected | **Complete — four executable corruptions in `de5a2a58ec`** |
| P4 — M40° adapter | One pair of real snapshots exported; Lean result agrees with endpoint difference | **Complete — public fixture plus live streamed check, 1892→1953** |
| P5 — transfer check | Real `open_theory/glue` fixture checked through the same typed syntax; stronger undefined claim refused | **Complete — futon3c `606aa86bf`, mathlib4 `1333ca5a8d`** |
| P6 — scope comparison | Frozen conversation compared with the Lean proof; metrics and counterexamples reported | **Complete as a documented dry run — futon3c `3610a2225`; no precision/recall claim** |
| P7 — external note | Reproducible artifact and a short, claim-ledgered note suitable for Aleks | **Complete — `NOTE-cool-memories-prototype-2026-10-13.md`** |

Each stage records commands, commits, test receipts and known limitations in
this file or in a linked artifact. A narrative update does not advance a stage.

### P0–P4 receipts (2026-10-13)

Codex-76 implemented P0 and the export half of P4 in minus40 commit
`3b246c430fbabc476c3d1db5f7ad11ecb0e72165`:

- pure `snapshot_at`, `net_changed`, `apply_net_delta`, `events_between` and
  `export_delta` operations;
- separate `net-changed`, `events-between` and `export-delta` commands while
  retaining legacy `changed` output;
- a text-free deterministic JSON fixture;
- six focused tests, including a fact born and withdrawn wholly within the
  interval.

Focused receipt:

```text
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest scripts/test_m40.py -v
# 6 tests, PASS
```

Codex-75 implemented P1–P4's Lean half in mathlib4 commit
`de5a2a58ec`:

- `Delta.comp` and global `apply_comp`;
- source/target-indexed `CertifiedDelta` and domain-independent
  `TypedDiagram` syntax;
- checked local `Step` constructors for fusion, source-licensed removal and
  addition cancellation, identification, and identities;
- `Path` and proofs that every step/path preserves interpretation;
- a concrete transient-fact path: fuse, cancel the redundant removal using
  `1 ∉ source`, then identify the canonical identity;
- disjoint-namespace interchange;
- a custom renderer that consumes the checked path;
- a text-free JSON checker with four executable corruptions.

Focused receipts:

```text
lake build DarkTower.CoolMemories \
  DarkTower.CoolMemoriesTraceChecker DarkTower.CoolMemoriesDiagram
# 680 jobs, PASS

lake env lean --run DarkTower/CoolMemoriesTraceChecker.lean \
  /home/joe/code/minus40/test/fixtures/snapshot-delta.json
# M40-DELTA-ACCEPTED 2→2

PYTHONDONTWRITEBYTECODE=1 python3 /home/joe/code/minus40/scripts/m40.py \
  export-delta 2026-10-10 2026-10-12 |
  lake env lean --run DarkTower/CoolMemoriesTraceChecker.lean -
# M40-DELTA-ACCEPTED 1892→1953
```

The last command streamed fact identifiers from the live scratch store to
Lean; it did not retain or commit the private export. The endpoint semantics
still inherit M40°'s known treatment of `said-again-at`; revived facts should
not be selected as a special validation case until that policy is specified.

### P5 receipt and correction (2026-10-13)

Codex-76 derived a public fixture by calling the real Clojure `ot/theory` and
`ot/glue` functions in futon3c commit `606aa86bff669d2194ce5b3be1f3aa4ea226e198`.
It pins `source → shared → result`, ownership and mechanisms, and the real
`:undeclared-sharing` refusal when the consumer's interface declaration is
removed. Its focused test ran 9 assertions with no failures.

The fixture also corrects this excursion's proposed theorem: the current
implementation defines n-ary union-DAG construction and validation, but no
independent sequential composition or interpretation of open theories.
Therefore equality of `glue` with sequential composition is **not defined**
and is not claimed.

Mathlib4 commit `1333ca5a8d` proves the smaller warranted transfer. The two
exported arrows inhabit the same domain-independent `TypedDiagram` syntax as
the M40 proof; their shared variable is the type-level middle boundary; and
interpreting the two-box diagram gives exactly the arrow list exported by the
real glued DAG.

```text
clojure -M:test -n futon3c.diagramprover.causal.open-theory-export-test
# 1 test, 9 assertions, PASS

lake build DarkTower.CoolMemoriesOpenTheoryChecker
lake env lean --run DarkTower/CoolMemoriesOpenTheoryChecker.lean \
  /home/joe/code/futon3c/test/futon3c/diagramprover/causal/fixtures/open-theory-glue.json
# OPEN-THEORY-TYPED-DIAGRAM-ACCEPTED source→shared→result
```

P5 therefore passes as a representation transfer and records a semantic
refusal. It is not the stronger open-theory composition theorem originally
imagined.

### P6–P7 receipts and verdict (2026-10-13)

Claude-16 compared the frozen conversation available to it with mathlib4
`de5a2a58ec` in futon3c commit `3610a2225`. The report is
`holes/labs/M-diagramprover/REPORT-P6-cool-memories-scope-comparison-2026-10-13.md`.
It found that the principal governing constraints are realised in the formal
artifact, but also isolated a limitation in the frozen commit: the general
endpoint composition theorem was an extensional proof, while only the concrete
transient-fact instance was a checked `Path` of named rewrites. Interchange is
semantic rather than a diagram rewrite because the syntax has no parallel
constructor. A misleading renderer label mentioning reassociation was removed
in mathlib4 commit `74e3e35a51`.

This is a dry run, not a valid measurement of scope/proof alignment. The
available conversational population included a retrospective consolidation
rather than Joe's frozen source turns; the sole labeller had contributed to
the design; and no independent scope labelling was performed. Consequently no
precision or recall is reported. The standing-to-within comparison method and
its named counterexamples are the result worth retaining.

P7 is the deliberately short external artifact
`holes/labs/M-diagramprover/NOTE-cool-memories-prototype-2026-10-13.md`. It leads
with the bounded positive result, supplies reproduction commands, and states
the negative claims explicitly: no Elephant implementation, accomplishment
judge, Markov-category model, War Machine reformalisation, parallel proof
constructor, or open-theory composition theorem.

The next technically meaningful extension is a parallel constructor making
interchange diagrammatic.

Mathlib4 commit `d18f441b66` establishes the first bounded parallel layer:
`snapshotTensor` forms disjoint `Sum`-tagged boundaries, `Delta.tensor` and
`CertifiedDelta.tensor` compose independent edits, `Delta.apply_tensor` proves
componentwise interpretation, and `Derivation.parAtom` constructs a typed
parallel diagram for two atomic boxes. A concrete `{1}→{2}` alongside
`{10}→{20}` example is checked by `run_source`. All five Cool Memories targets
rebuilt successfully (682 jobs). This validates the tagged-boundary design;
recursive `par` syntax and an interchange `Step` remain the next unit rather
than being claimed by the atomic constructor.

Mathlib4 commit `a9a7a03608` completes that next unit. `liftLeft` and
`liftRight` recursively preserve sequential diagram structure in either tagged
lane; `Derivation.par` and `parRightFirst` retain the two possible schedules;
and `ParallelStep.interchange` is a checked rewrite between them. Its soundness
uses the component interpretation lemmas `run_liftLeft` and `run_liftRight`,
not endpoint typing alone. A concrete two-lane certificate and soundness check
compile with the core. All five Cool Memories targets rebuilt successfully
(682 jobs). The remaining presentation task is a two-lane renderer and a
decision about whether the resulting structure now warrants adapting
Mathlib's monoidal string-diagram widget.

Follow-up mathlib4 commit `48abaaa949` makes the first of those tasks precise:
`mem_comp_between_remove_iff` and `mem_comp_between_add_iff` prove for arbitrary
`A`, `B`, `C` that a fused delta differs from the canonical endpoint delta only
by facts absent at the source which appear then disappear, and facts present at
the source which disappear then reappear. These are exactly the two
source-licensed edit classes the general rewrite path must cancel. The
four Cool Memories targets rebuilt successfully (681 jobs).

Mathlib4 commit `9343186e70` closes that path gap. It adds checked bulk
cancellation steps for all removals absent at the typed source and all
additions already present there; proves that applying those steps to any
certified delta yields its canonical endpoint delta; and constructs
`betweenSeqNormalizationPath A B C` as `fuse → dropAbsentRemoves →
dropPresentAdds → identify`. Its soundness is inherited row-by-row from
`Path.sound`, not from the older extensional theorem. All five Cool Memories
targets, including the negative boundary target, rebuilt successfully (682
jobs).

The P6 audit also correctly observed that the wrong-source rejection was only
described, unlike the three executable semantic corruptions. Mathlib4 commit
`f02ecf84e7` closes that evidence gap with
`CoolMemoriesBoundaryNegative.lean`: a `#guard_msgs` test checks that Lean
rejects composition of a `{1} → {2}` box with a `{3} → {4}` box at
elaboration. `lake build DarkTower.CoolMemoriesBoundaryNegative` passed (593
jobs), meaning the expected rejection occurred. The audit's X2 was separately
closed by `74e3e35a51`; X4 described its frozen pre-adapter commit and is closed
by the P0/P4 receipts above. X1 is closed by `9343186e70`; the absence of a
parallel proof constructor remains open.

## Stop conditions

Stop and reassess rather than enlarging the theory if:

- the proof diagram is only a rendering of an opaque Lean proof;
- composition is defined so the headline theorem is true by construction and
  no cancellation proof remains;
- the Mathlib widget requires replacing rather than reusing the semantic core;
- the M40° and open-theory cases cannot share a certificate representation;
- scope alignment is reported without a frozen comparison set;
- Markov-category terminology appears before a deterministic transfer works.

## Grounding

- M40° implementation and milestones: `/home/joe/code/minus40/README.md`.
- M40° rough specification: `/home/joe/code/minus40/docs/spec-2026-10-09.txt`.
- Current temporal CLI and the `changed` asymmetry:
  `/home/joe/code/minus40/scripts/m40.py`, `cmd_changed`.
- Open causal theories: `src/futon3c/diagramprover/causal/open_theory.clj`.
- Existing tests and limitations: `holes/missions/M-diagramprover.md`,
  "Skeleton use case 2".
- Proofs-as-diagrams workstream and its unstarted status: the same mission,
  "WS-D" and the R5 row.
- Scope campaign evidence:
  `holes/labs/M-象-2000/LAB-2026-10-12-move-grammar.md`.
- Contact-note context:
  `holes/labs/M-diagramprover/NOTE-cover-note-kissinger-2026-10-13.md`.
- McCarthy, *Elephant 2000: A Programming Language Based on Speech Acts*:
  <https://www-formal.stanford.edu/jmc/elephant.pdf>.

## Origin record

This plan consolidates Joe's 2026-10-13 discussion with codex-75 and two
reviews by claude-16 (Agency jobs
`invoke-1791870193459-8614-0bdeca22` and
`invoke-1791870329444-8624-59b6775f`). The second review corrected an initial
misreading of M40° as M-象-2000 and proposed delta composition as the first
genuinely diagrammatic theorem.
