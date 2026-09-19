# Test-registry read endpoints — 2026-09-19

Implementation commits: `31d2400a8de629253d810b4eee68f28e40ebbad2`,
`f0d2620efaa4eec8c19451ee2a243c2a2e343469`.

The handlers call `futon3c.test-registry/check-record!` and
`futon3c.test-registry.validation/report!` against the running server's
configured durable evidence store. Report results are cached for 30 seconds
only while the evidence-store identity and the subjects/queue ledger lengths
and mtimes remain unchanged.

## Registered test run

The final registered suite warrant is
`test-registry-6096411058158f06b4289a73b080fd670e440affcdd6041026bfe9a7d540e12e`:
3 tests, 13 assertions, zero failures/errors. `register-final.*` and
`final-execution/` are its receipts. The earlier `register.*` warrant is
retained as superseded evidence: its acceptance probe exposed the uncached
15.28-second report read, leading to the cache correction and final run.

## Live deployment observations

`reload.txt` records canonical-checkout namespace reload and the existing
master-only pouch route probe. `pouch-final.http` is HTTP 200 after reload.

Warm live reads on the running JVM:

- POST check: 0.74 seconds (`check-final.time`).
- GET report: 0.00 seconds (`report-final.time`).
- Fabricated check: HTTP 200 with typed `missing-entry`, `warrant? false`.
- Report: 17 rows for 17 current subject bindings (`counts.txt`).

The real final warrant check reaches the authority but honestly returns
`environment-mismatch`: the registered runner recorded `TZ`/`LC_ALL`, while
the long-running server process has those variables unset. The endpoint does
not waive or reinterpret that refusal.

Static receipts: `clj-kondo.txt` (zero errors/warnings, one informational
finding) and `check-parens.txt` (`OK`).
