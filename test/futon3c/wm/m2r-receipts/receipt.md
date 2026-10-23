# M2r execution receipt

Full 40-character patch identity is now an input to initial/requeue ID minting.
No production caller needs the three-argument form; it is removed. Deploy the
futon2 and futon3c changes together. Existing IDs remain opaque and readable;
this does not migrate existing queues, ledgers or acceptance files.

The authoritative patch is the author receipt: coordinator reads it at
meta_repair_coordinator.clj:396 and verifies review/enact/retry payloads against
it at :418/:443/:470. Requeue now passes that same transaction value.
A missing patch is refused by the existing author-payload check before requeue;
the mint boundary additionally refuses malformed/absent patch values.

Tests were run in their own worktree JVMs. Temporary files were redirected
with JAVA_TOOL_OPTIONS=-Djava.io.tmpdir=/home/joe/notes/M2r/tmp (the initial
futon2 red run, before this override, emitted a Clojure error report under the
JVM default /tmp; no worktree or task files were placed there).

Commands:
- futon2: clojure -X:test :nses '[futon2.aif.retry-patch-identity-test futon2.aif.meta-repair-coordinator-test]'
- futon3c focused: clojure -M:test -n futon3c.wm.retry-identity-test
- futon3c existing + focused: clojure -M:test -n futon3c.wm.retry-identity-test -n futon3c.wm.meta-self-heal-adapters-test
- baseline adapter: same namespace alone at detached 6a7f411c3.

Red: futon3c 1 test/11 assertions, 6 failures (both ordinals collide, no second
execution). The red test used a test-only old-arity fallback to reach real
admission on base; final test calls the four-argument producer directly.
Futon2 red demonstrates missing fourth callback argument. Missing-patch fixture
was resealed, so validation exercises payload absence rather than seal tampering.

Green: futon2 20 tests/125 assertions, zero failures/errors; futon3c focused
2 tests/16 assertions, zero failures/errors. Existing adapter suite is NOT fully
green: combined run 37 tests/319 assertions, 3 failures/0 errors, identical to
unchanged-base adapter run (35 tests/303 assertions, 3 failures/0 errors):
acceptance-run-is-bounded-pinned-and-shell-free line573 and obsolete
executed-command-inserts-the-test-runner-main assertions lines664/666.
These are reported, not repaired by this retry-identity patch.

clj-kondo: 0 errors/0 warnings on changed Clojure. check-parens: OK.
git diff --check: clean. No shared JVM reload, click, live-store/queue write,
or merge was performed.

Reader audit (all hits in notes/M2r/readers.txt):
- futon3c meta_self_heal_adapters: changed retry-click-id and enact! to
  supply/full-bind patch; ports forwards the four-argument function, retry
  queue and receipt carry IDs unchanged (exempt, opaque values).
- futon3c runner_service: exempt throughout configured ports, status lookup,
  retry-child-opts and admission; no ID parsing, exact identity/lookup.
- futon2 meta_repair_coordinator: changed requeue mint call; other hits
  compare, persist or forward opaque IDs, including historical doc example.
- futon2 meta_injury_ledger: exempt; nonblank/equality checks and persistence,
  no parsing. Existing recorded IDs remain valid.
- futon2 meta_terminal_injury: exempt; matches exact child ID to ledger.
- futon2 meta_self_heal_production: exempt; forwards callback unchanged
  (comment contains no arity); latest-retry-click reads phase names, not ID
  grammar; readiness matches exact IDs; disposition copies the identity.
- futon2 meta_self_heal_harness: exempt; offline literal IDs, not production
  minting or ID parsing.
