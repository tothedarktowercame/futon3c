# RUN4 pinned config review: snapshot and freshness

Codex-17, 2026-09-10. Reviewed eac0f12e. Independent series-service tests
pass: 4 tests / 33 assertions, including actual asynchronous click, wrapper,
run-record/projection/binding writes, strict reader and controller transition.
Task-producing core remains stubbed; no external task execution is established.

Recording-root integration is evidenced by the disposable roundtrip. However,
acceptance of the complete change is withheld: validate-pin caches read-text
and passes that same cached port into the runner's fresh selection validation.
Reproduction /tmp/run4-cache-review.clj prepares via actual trusted entry,
changes source.md, then calls the returned runner port. Result:
{:prepared true :runner-reader-still-returns-old true :disk-changed true}.
Thus post-preparation source changes are hidden from the deciding boundary.

Requested separate captured-snapshot config materialization and fresh runner
validation against current authorized sources. Drift must refuse dispatch;
it cannot silently select a successor configuration or pass by replaying cached
bytes. Preserve same-snapshot parse/hash consistency and explicit false values.
No live state or service changed during review. Next packet also prepares the
read-only visibility projection, independently of operator acceptance.
