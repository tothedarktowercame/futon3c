# Cool Memories: a small checked instance of “a proof is a diagram”

The prototype now has one defensible result.  For a concrete M40° transient
fact, Lean checks a typed path from two successive endpoint deltas to the
canonical identity delta.  The path is not a picture of an opaque proof: its
three rows are named, locally checked rewrites—fuse the edits, cancel a removal
under the carried proof that the fact is absent at the source, then identify
the resulting edit with the canonical endpoint delta.  A renderer consumes
that checked path.

The surrounding general law is also proved:

```text
apply (Δ(A,B) ; Δ(B,C)) A = apply Δ(A,C) A
```

The general theorem now also has a checked rewrite path for arbitrary `A`,
`B`, and `C`: fuse the edits; cancel every removal licensed by absence at the
typed source; cancel every addition licensed by presence there; then identify
the result with the canonical endpoint delta.  Lean separately proves that
these cancellations always reach `Δ(A,C)`, so the normal form is not selected
by definition.

## What is operational rather than illustrative

The M40° side now distinguishes endpoint difference (`net-changed`) from all
events in an interval (`events-between`).  The legacy `changed` command was
left intact.  A JSON adapter exports snapshots and their delta; Lean checks a
public fixture and also accepted a live streamed pair of snapshots containing
1892 and 1953 facts.  No private export was retained.

The proof syntax also transferred once, narrowly, to futon3c's real Clojure
`open_theory/glue`: the exported producer and consumer inhabit the same typed
two-box syntax, with the declared shared variable as their middle boundary.
The Clojure operation constructs and validates a union DAG; it does not define
an independent sequential-composition semantics.  We therefore do not claim
that glue equals sequential composition.

## Scope comparison

A dry-run comparison of the development conversation with the frozen Lean
artifact found that the governing constraints were mostly visible in the
definitions and soundness arguments: separation of syntax, interpretation and
proof; structural rather than endpoint-by-definition composition; source-
licensed cancellation; and a pure trusted core.  It also found the important
counterexample that was true of the frozen commit: only the transient instance,
not the general law, was then a rewrite path. That gap was subsequently closed
by mathlib4 `9343186e70`. The comparison cannot support precision or recall claims. It
used a retrospective consolidation rather than the original turn records,
and its sole labeller had also supplied part of the design.  Its method may be
reused; its numbers should not be.

This is evidence for a possible bridge between standing conversational
constraints and formal proof regions, not evidence that conversational scopes
are proofs.

## Reproduction

Pinned commits:

- minus40 `3b246c430fbabc476c3d1db5f7ad11ecb0e72165`
- mathlib4 `de5a2a58ec` (core and diagram), `1333ca5a8d`
  (open-theory checker), `74e3e35a51` (claim-label correction), and
  `9343186e70` (general checked normalization path)
- futon3c `606aa86bf` (open-theory fixture) and `3610a2225` (scope report)

Focused commands:

```sh
cd /home/joe/code/minus40
PYTHONDONTWRITEBYTECODE=1 python3 -m unittest scripts/test_m40.py -v

cd /home/joe/code/mathlib4
lake build DarkTower.CoolMemories DarkTower.CoolMemoriesTraceChecker \
  DarkTower.CoolMemoriesDiagram DarkTower.CoolMemoriesOpenTheoryChecker
lake env lean --run DarkTower/CoolMemoriesTraceChecker.lean \
  /home/joe/code/minus40/test/fixtures/snapshot-delta.json
lake env lean --run DarkTower/CoolMemoriesDiagram.lean
lake env lean --run DarkTower/CoolMemoriesOpenTheoryChecker.lean \
  /home/joe/code/futon3c/test/futon3c/diagramprover/causal/fixtures/open-theory-glue.json

cd /home/joe/code/futon3c
clojure -M:test -n futon3c.diagramprover.causal.open-theory-export-test
```

The detailed scope audit is
`REPORT-P6-cool-memories-scope-comparison-2026-10-13.md`; the full decisions,
receipts, limitations and next theorem are in
`holes/excursions/E-cool-memories.md`.

## Claim boundary

This prototype does not implement Elephant 2000, an accomplishment judge,
Markov categories, or a War Machine reformalisation. It now has recursive
parallel composition over disjoint tagged namespaces and a checked interchange
rewrite, but no two-lane renderer yet. Its existing renderer remains a small
certificate consumer rather than Mathlib's monoidal string-diagram widget.
