# APM V4 and WM pattern learning: bounded alignment proposal

2026-09-11, Codex-17. Response to Agency job
`invoke-1789134489964-20148-3ee0a51d`. Design only; no RUN4 gate,
publication authority, live change, or new experiment is activated here.

The V4 revision lifecycle is a useful reusable contract. Adopt its separation
of proposal, review, publication, exposure and observed use; do not adopt the
demo library as a second WM library. Publication establishes an available
revision, not learning or AIF compliance. The retained pilot and simulated
next attempt cannot establish a causal benefit from this revision mechanism.

## Actual consumers and representation boundaries

| Existing object or consumer | Recommended mapping |
| --- | --- |
| `futon2/src/futon2/aif/pattern_registry.clj`, `aggregate-pattern-candidates` | Existing retrieval certificates provide episode/turn, query, rank and evidence IDs. They establish candidate exposure, not reading or successful use. |
| Same file, `pattern-artifact-receipt` and `actuation-construction` | Reuse canonical `.flexiarg` path, pattern ID, byte SHA and evidence IDs. `:fire-pattern` already constructs an author/reviewer/grounded-implementation obligation. It is not a library-publication operation. |
| `futon2/src/futon2/aif/full_loop_runner.clj`, `construct-selected-action :fire-pattern` and final actuation validation | Join the selected pattern revision to the resulting implementation and independent review. A grounded implementation still needs a judgment about whether this method was used correctly. |
| `futon3/checks/find_organise.clj`, `organise`, `fire`, `apply-edit` | Reuse organisation and execution semantics where the adapter has the required authored relations and policy. `organise` itself fires nothing. Policy admission must retain its separate provenance. |
| `mathlib4/DarkTower/WarMachine/Holes.lean`, `Cascade` and `CascadeDiff` | Reuse nodes, organisation additions, authored/organised edges, precedence and observed acting-order distinctions. `CascadeDiff` is not a library text patch; it records selected/added/admitted node origins and before/after organisation/execution evidence. |
| `futon2/scripts/futon2/report/cascade_lane.clj` | This runtime lane invokes the Python constructor and exposes `:shown` and rollout data. It is not automatically the authored-edge `organise` implementation. Its output must not inherit the latter's guarantees merely because both are called cascades. |

A TA goal node may cite several patterns, repeat one pattern, or be an ordinary
deduction. Preserve the obligation graph with its source digest and map its
nodes to pattern *occurrences* and revisions. Do not identify it with a set of
pattern IDs. In particular, mathematical dependency, example association and
authored `@why` reachability are different relations. The latter authorizes
organisation edges in `find_organise`/`construct_cascade`; neither retrieval
similarity nor a retrospective TA interpretation can supply it.

Reuse existing structures through a checked projection plus a provenance
record. Leave unavailable acting order and scores unobserved; do not invent
values merely to populate `CascadeDiff`. Its recorded Lean instances do not
prove correspondence for this proposed adapter.

## Smallest episode-to-revision seam and authority

Start after an independently reviewed WM episode: combine its exact pattern
receipt, retrieval certificate and implementation/review artifacts with a
reviewed diagnosis of one unmet applicability condition. This produces a V4
proposal; it neither closes a repair obligation nor changes selection. If the
episode lacks evidence of actual pattern use, diagnose only the supported
retrieval/exposure problem. A timeout alone is not evidence of a pattern defect.

For the inspected WM registry, production-addressable authority defaults to
`/home/joe/code/futon3/library`, under configured root containment. The adapter
must retain that configured authority, not assume the APM demo JSON owns it.
Pattern source, retrieval index and example/memory bodies need separate pins.

Proposed ownership: an episode worker authors the candidate; a separately
authenticated reviewer checks the candidate bytes and diagnosis; the existing
library owner or an explicitly delegated publisher admits it. This note grants
neither role. A publication receipt must bind repository/root, pattern ID,
old and candidate byte hashes, proposal and trigger digests, executed review
job/actor/verdict, publication commit, and resulting index revision. Recheck
the old hash under the publication lock. The prototype's string identities and
evidence assertions are insufficient at that boundary.

The next episode must receive a receipt for the exact revision it actually
read. `cascade-policy-for` currently caches by `[psi-text budget epsilon]`,
without library/index revision. A future adapter must make cache/index identity
part of that receipt and freshness contract; clearing a cache by convention
does not prove which revision a worker consumed.

## Next use, conflicts, and one bounded experiment

Use a separate problem and independent worker with recorded exposure, before
reference-solution access. Retain retrieval, delivered/read bytes, premise
judgment, attempted construction, produced artifact, independent verification
and usefulness judgment as separate events. Unknown observations need typed
unknown states: V4's mandatory booleans cannot faithfully represent missing WM
read/use evidence. A new problem ID alone does not establish independent work
or an unexposed task. Likewise `:shown`, selection and citation cannot stand in
for execution.

The reusable contract is revision-bound diagnosis/review/publication/use with
immutable history and stale-source refusal. Mathematical entailment, Lean proof
checking, finite-net prerequisites and counterexample adequacy remain domain
validators. WM substitutes task-specific applicability, build/review/grounding
and original-problem acceptance checks; passing those does not itself establish
active-inference parameter learning or causal efficacy.

Recommend one future **applicability-contrast adapter experiment**: select one
retained WM episode with a reviewed condition gap and an existing canonical
pattern; freeze a positive and an inapplicable contrast task before revising
that condition. If no episode meets this evidence requirement, report that
absence rather than manufacture a diagnosis. In disposable authority, run the
existing candidate/organisation consumers against old and proposed revisions;
retain node origins and authored-edge checks. Then have independent workers
attempt the separately frozen tasks with exact exposure receipts and independent
artifact review. Record correct use, correct refusal and failures separately.
This is a finite development experiment, not a population learning claim.

Controls: citation-only “use”; stale publication base; mismatched review hash;
unknown read event; teaching edge passed as authored edge; repeated occurrences
collapsed into one; old index/cache served after publication. No new canonical
pattern admission or autonomous publisher is needed for this slice.

This preserves all seven requirements in
`futon2/holes/labs/wm-contract/runs/WM-pattern-learning-follow-on-2026-09-10.md`:
artifact plus decomposition (1); separate graphs/library/examples (2); independent
exposure/use (3); localized diagnosis (4); reviewed promotion without self-warrant
(5); one version history retaining failures (6); observed subsequent use (7).
It also provides an empirical row for the proposed P4NG sequel: original problem,
mechanism prediction, discriminating contrast, exact revisions, observed use and
remaining uncertainty. It does not add conditions to the current RUN4 chain.

## Verification performed

Read the V4 README and `library_loop.py`, the seven-principle WM note, and the
consumer/Lean definitions named above. Executed the prototype's actual command:

```sh
python3 -m unittest discover -s holes/labs/M-apm-demonstration/analysis/apm-v4-offline-2026-09-11 -p 'test_*.py'
```

Result: 15 tests passed. This validates the offline prototype controls only;
the adapter and experiment above remain proposed. No Lean build, worker trial,
live publication, or runtime modification was performed.
