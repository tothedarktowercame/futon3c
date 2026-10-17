minus40 memories injected before round 1 for this task:

--- memory slow-tagged-tests-hide (cc:-home-joe) ---
`deps.edn`'s `:test` alias runs `-e :slow` — it EXCLUDES slow-tagged tests. To
run them: `clojure -M:test:test-all -i :slow -n <ns>`.

On 2026-09-25, `futon3c.agency.clock-decision-test` reported 13 tests / 64
assertions / 0 failures, and I was about to record a War Machine criterion as
satisfied on that basis. The criterion's locator explicitly named the `^:slow`
`dispatch-inheritance-through-real-job-path`. Run with `-i :slow`: 5 tests, 55
assertions, **2 failures** — in exactly that test. It had been failing
invisibly since a rule change three days earlier.

**Why:** a test tagged out of the default run is a test nobody reads. The
failure is not hidden by anything subtle — it is hidden by the alias everyone
types.

**How to apply:** before claiming a named test passes, check whether it is
tagged `^:slow` and run the slow selector too. When a criterion's locator
names a specific test, run THAT test by name rather than its namespace's
default. A "13 tests, 0 failures" line is not evidence about a test the runner
never loaded. Related: [[absence-needs-a-harder-search]],
[[wm-lean-registry-closure]].
--- end of injected memories ---

An acceptance criterion says: "futon3c.test-registry-test/clojure-closure-covers-dynamic-loads-and-resources passes". Write the exact command you would run, from /home/joe/code/futon3c, to check that criterion, and say how you would know from the output that this test was actually executed. Do not run it. Reply with the command in one code block and two sentences.
