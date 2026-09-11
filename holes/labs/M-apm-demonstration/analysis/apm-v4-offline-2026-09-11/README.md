# APM V4: first offline review component

Completed 2026-09-11 while V3 runs. This packet adds executable review bookkeeping
for the existing pattern cascade. It does not install a new loop, dispatch an
agent, access holdouts, change canonical patterns, or alter V3.

`plan_review.py` accepts the existing TA-cascade JSON shape and a separate review
map keyed by node ID. It checks graph references, reciprocal parent/child links,
duplicates, cycles (including disconnected components), root reachability,
nonempty conditions, and presence of method pins. Each obligation needs an
explicit applicability argument and a proof argument before the whole packet
is marked `ready_for_ta_review`. Open/refuted conditions stay visible. Pattern
citations, historical `paper-proof-available` labels and draft patterns never
close an obligation automatically.

This is **review completeness, not mathematical validity**. Even nonsense prose
could fill the argument fields; a TA must adjudicate it. The result always says
`mathematics_verified: false`. It assumes the retained cascade's basic JSON
shape, not arbitrary malformed input. Method-pin presence is checked here;
actual pattern/source hashes are checked by the existing provenance verifier.
Neither validator proves that a cited pattern entails a step.

## Executed bounded unit

Reused the eleven-node a93A01 reconstruction, with its exact source SHA256 in
`source-pin.json`. Ran its existing provenance verifier successfully (source,
pattern revisions and graph checks). Ran this component with `reviews-open.json`:
**zero structural errors, 22 open review obligations**, retained in `result.json`.
This intentionally does not convert existing retrospective paper prose into
newly adjudicated proof evidence. No new mathematics or Student turn is claimed.

Seven unit tests passed: citation-only closure refusal; the bounded discrete
finite-net failure contrast; missing applicability argument; complete bookkeeping
without mathematical certification; disconnected cycle; unknown dependency,
duplicate ID and missing pin; nonreciprocal parent/child linkage. The discrete
contrast is an authored unit-test control, not a new observed Student failure.

Run from the futon3c checkout:

```sh
python3 holes/labs/M-apm-demonstration/analysis/ta-cascade-2026-09-10/verify.py
python3 -m unittest discover -s holes/labs/M-apm-demonstration/analysis/apm-v4-offline-2026-09-11 -p 'test_*.py' -v
python3 holes/labs/M-apm-demonstration/analysis/apm-v4-offline-2026-09-11/plan_review.py holes/labs/M-apm-demonstration/analysis/ta-cascade-2026-09-10/a93A01-ta-cascade.json holes/labs/M-apm-demonstration/analysis/apm-v4-offline-2026-09-11/reviews-open.json
```

## Proposed next slice, not implemented here

Use the same node IDs through Student planning, TA response and revision. The
Student supplies goals, definitions, proposed methods and applicability arguments
before attempting the detailed proof. The TA returns concrete missing premises,
invalid estimates or retrieval gaps, with cited examples and failure contrasts.
A candidate pattern remains a draft and cannot warrant itself merely by being
authored. Ordinary deductions remain legitimate; do not invent a new pattern
for every arithmetic step just to reach apparent pattern coverage.

The existing transfer pilot completed three cases in five real Student turns.
Its positive finite-net target was already solved before teaching; the observed
repair concerned the counterexample. Treat those cases as exposed development
examples, not an unseen evaluation set. The next comparison needs separately
frozen development tasks, explicit exposure records and reviewed applicability
and proof judgments. Compare retrieval, applicability, proof execution and repair
on the next use separately. This component supplies review obligations for that
experiment; it does not yet supply retrieval, dispatch, independent review or a
comparison runner.

Historical reconstruction supplied three draft gaps, including a discoverability/
example-link gap rather than a genuinely new method. Review those against existing
patterns before minting entries. Batching library reads and tuning HTTP capacity
are separate implementation questions; neither is changed by this packet.
