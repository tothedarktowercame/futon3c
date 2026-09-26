# Pattern-warranted proof construction and topology collection induction

Joe asked for two concrete follow-ups to the pattern-first APM investigation:
construct a Lamport-style proof while consulting patterns from the top down,
and induce topology patterns from the completed collection rather than one
route. This packet does both as offline development work. It makes no service,
campaign, Student-dispatch, canonical-pattern, or RUN4 changes.

Evidence directory:
`holes/labs/M-apm-demonstration/analysis/pattern-construction-2026-09-10/`.
The preceding retrieval census and consumer trace remain in
[TN-APM-pattern-first-development-2026-09-10.md](TN-APM-pattern-first-development-2026-09-10.md),
commit `380a323da085678fabea5f53a50351e06786d02e`.

## Actual construction, not retrospective labels

`prelim-development.md` records the development of a93A01: uniform continuity
on (0,1) is equivalent to small value differences whenever the difference
quotient exceeds a suitable threshold. The initial obligation tree and actual
consultations were committed in `d15b2e84`; the full paper proof was committed
in `5b53ebd2` before reading the remainder of the retained solution. Definitions
and its first bridge had already been read, and the problem was familiar from
the previous packet. This is not an unexposed trial or a causal reuse result.

The forward branch requires a globally bounded numerator. A library pattern
suggested that intermediate obligation, but its stated bounded-domain premise
is false for general metric spaces. The proof establishes a finite net and
uses uniform oscillation plus a finite maximum to prove boundedness. The
reverse branch uses the existing quotient threshold to split high and low
ratios, choosing δ=ε/N. Each numbered unit records its warrant or the remaining
construction. Two plausible search results were explicitly rejected. No memory
body supplied an implementation, and no memory-use claim is made.

The result demonstrates a useful cascade: proof architecture → intermediate
mathematical construction → premise witnesses → elementary algebra. Library
methods guide construction but do not discharge hypotheses. The finite-net
oscillation bound and threshold dichotomy are two candidate middle-level
patterns exposed by this development, pending other examples. The retained
Lean proof agrees on the threshold construction and uses the library's
`TotallyBounded.image` for boundedness. This packet contains a paper proof and
source comparison, not a new Lean implementation or compiler run.

## Topology population and dependencies

The census uses apm-lean commit `f053ab5936725f2e41c937cfff82277f3a23c868`.
It examines every canonical t-prefix problem bundle, every ConstructionTargets
Lean file, and both topology worklists. It traverses local imports from every
source-complete candidate and separately handles documentary branch closures.

| Observation at the pin | Count |
|---|---:|
| Topology problem bundles | 138 |
| Source-complete candidates (theorem present; no local sorry/admit/declared axiom) | 61 |
| ConstructionTargets modules scanned | 794 |
| Modules without local placeholders/declared axioms | 793 |
| ConstructionTargets reached by candidate consumers | 457 |
| ConstructionTargets reached by at least two candidate consumers | 340 |
| Done ledger rows, all with retained review pass | 729 |
| Done rows with committed implementation evidence | 549 |
| Done rows recording reviewed STOP / replan | 82 / 98 |

The last three categories must not be conflated. An author outcome of
`done-unreviewed` can occur on a subsequently reviewed, done row; the retained
review supplies the pass. A reviewed STOP or replan is a completed disposition,
not a new theorem. Source completion is also distinct from fidelity to the
original statement. Nineteen of the 61 source candidates carry other status
labels (nine partial, nine informal-only, one statement-defective). For
example, t00A04 proves a conditional reduction with non-nullhomotopy supplied
as a hypothesis. A source scan cannot turn that into a proof of the original
unconditional geometric claim. No live status or ledger was repaired here.

The scanner strips nested comments and strings before counting placeholders.
It finds no locally declared axioms or placeholders in the traversed local
closures of the 61 candidates. External Mathlib dependencies are outside this
source audit. Import reachability is availability, not evidence of invocation;
there was no fresh compiler or transitive axiom check.

## Six candidate methods

`topology-patterns.md` supplies conditions, construction steps, positive
witnesses, failure cases, and distinctions from broad existing patterns.

| Candidate | Inspected witnesses |
|---|---|
| Compare generating relations, then construct quotient homeomorphisms | t96A03, t01J03, t02A03 |
| Choose outside a null bad set using a positive-measure target | t93A04, t92J07, t94J05, t95J04 |
| Retractions force injectivity of invariant maps | t01A03, t91J02 |
| Reconstruct sources from target-natural mapping equivalences | GenericTargetNaturalSourceReconstruction; TwiceSpiralSourceEquivalence |
| Compact-to-Hausdorff continuous bijections give homeomorphisms | t92J02, t94J02 |
| Identify the actual comparison morphism before using its isomorphism | LocalHomologyNeighborhoodExcision; CoefficientRelativeSmall; CoefficientExcision |

These occupy the middle level between “use an invariant” and a named theorem:
they describe how to build the map or witness that makes a theorem applicable.
The sixth is especially relevant to library-building topology work: abstract
isomorphism existence can leave the consumer's actual inclusion or generator
map unidentified. Its completed library stages are useful evidence even when
no completed problem yet imports one of them.

The population scan is exhaustive within its stated repository scope; the
semantic induction is a bounded selection from that population. The coverage
matrix records all 61 roots, with 48 unassigned to these six selected witness
families. This does not claim six patterns explain all completed work. Shared
infrastructure and stages of one program are identified, so repeated imports
are not counted as independent discoveries. No canonical pattern or use edge
has been authored by this packet.

## Smallest proposed Student slice

Retain the earlier proposal for structured pattern descriptions and reviewed
example links. Add a bounded proof-construction artifact at the existing
Student planning step, initially one development problem and two candidate
methods per open obligation. A node should contain:

- `id`, `parent`, `assumptions`, `goal`, and exact definition references;
- selected pattern ID/revision and actual read receipt, or a logical rule;
- required conditions, each linked to an assumption or proved child obligation;
- a consulted memory/example reference only when actually read;
- child obligations, artifact/proof references, and state: open, constructed,
  checked, rejected, or blocked.

Selecting a pattern may create children; it may not close them. A missing
condition becomes a child goal or a reason for rejection. An uncovered node
permits ordinary mathematical construction and a draft pattern proposal. It
must not self-certify by authoring a new pattern that repeats its own goal.
The proof/reviewer establishes the mathematics; a schema checker establishes
only provenance and structural completeness. Keep claims of pattern use,
example use, and proof-checked discharge separate. Captions remain a useful
entry point for discovery and are complementary to these obligations.

This can be introduced without changing the theorem checker: first serialize
and inspect the plan alongside the existing proof artifact. Then connect
pattern reads and premise checks to existing retrieval observations. Evaluate
on development tasks with both applicable and deliberately missing-premise
cases, including an uncovered node. Success means a justified proof action or
correct rejection, not a nonempty citation list. Broad retrospective extraction
should use the census population and preserve statement variants and conditional
results rather than treating every done row as a solved original problem.

## Reproduction and validation

See the evidence directory's README for exact offline commands. Scripts read
only pinned Git objects and serialize selected ledger fields; no sealed
holdout, live services, Claude calls, Student dispatch, or Lake invocation is
involved. Exact witness excerpts are hash-checked against the census. Replay,
syntax/lint, parentheses and diff checks are recorded in `checks.txt`.
