# Historical verifier freshness review — 2026-09-11

Reviewed 94a71111/a2a8bbdf after the earlier actor correction. Independently
ran verifier test: 1/9 passing, using actual qualification producer output.
Actor is now joined to the trusted review job, execution required, and the
single digest marker must match. Canonical output rejects a symlink target.

Found and fixed post-check source drift: the retained reproduction changes a
disposable source at the HEAD check after the initial source validation. Before
the fix it admitted awaiting-validation. The verifier now rereads manifest and
all qualification source pins plus HEAD before publication. The original
reproduction now refuses; permanent regression asserts reason :source-drift.
Race regression plus its reused real-producer fixture: 2 tests/10 assertions,
zero failures/errors. Producer regression: 5 tests/20 assertions passed.
Lint 0/0, parentheses OK, diff check clean.

Also exported the producer's strict plan validator and reused it in the reader;
exact qualification/row/output key sets now refuse a shape the producer could
not emit. This prevents schema drift between the two sides.

This is a checked final reread, not a filesystem transaction locking source
repositories against arbitrary concurrent writers. No hermetic execution or
cross-process source snapshot claim is made. Final production packet must pin
the actual qualification implementation and finding, not the harmless fixture
commands used to test the interfaces. Successful fixture commands establish
port composition, not that repair 057 is repaired.

Following independent review of this small correction, proceed to explicit
selection/admissibility and a historical-verification repair-store transition.
A verification receipt must never claim freshly authored code or resolve the
repair; the distinct production-shaped successor remains mandatory. Existing
failed attempt and all live state remain unchanged. No activation performed.
