# Test-registry read endpoints — 2026-09-19

Implementation commits: `31d2400a8de629253d810b4eee68f28e40ebbad2`,
`f0d2620efaa4eec8c19451ee2a243c2a2e343469`, and response-shape amendment
`b30c0dd17c0ce64770b2d7f6b5a6a9ee14171874`.

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

## Validity-now response-shape amendment

The check endpoint now returns the authority's result unchanged under
`:check`, alongside `:meaning "validity-now, not the recorded mint verdict"`.
It never exposes an ambiguous top-level `:warrant?`; the evidence lookup's
recorded mint verdict and the check endpoint's current validity answer are
therefore structurally distinct.

The amendment's registered warrant is
`test-registry-e714c236b698c02c2cc90d813e37a77a52e1b902df58baf770d18738ed0c81e1`:
4 tests, 19 assertions, zero failures/errors. `register-shape.*` and
`shape-execution/` are its receipts. The stale-fixture test pins a recorded
true mint verdict against a current `:stale-sha` refusal.

After canonical namespace reload, `check-shape.json` contains only top-level
`check` and `meaning`; `fabricated-shape.json` is a nested typed
`missing-entry`. Warm live reads were 0.868 seconds for check and 0.001 seconds
for report. `pouch-shape.http` records HTTP 200 after reload. Static amendment
receipts are `clj-kondo-shape.txt` and `check-parens-shape.txt`.

## Drift provenance and report completeness

Implementation commit `641db0252ac675a25242d34492879ee7a630eeb0`
keeps `:stale-sha` unchanged while adding `:details/:scope-drift`, whose
per-path `:classification` is `:uncommitted` for worktree-dirty bytes and
`:committed` for reproducible superseding commits. The check response meaning
now tells polling clients to wait rather than re-dispatch on uncommitted drift.
Report summaries include `:subjects`, equal to `(count :rows)`.

The registered warrant is
`test-registry-53d40f7b96a8346b3b43af3fe455e76e5cd456a39412c90212663ea026a18346`:
4 tests, 23 assertions, zero failures/errors. `register-drift.*` and
`drift-execution/` are the run receipts. After canonical hot reload, the new
warrant checked current in 0.907 seconds; the superseded prior warrant named
both changed files as `:committed` in 1.037 seconds; the warm report returned
18 rows and `:summary/:subjects` 18 in 0.001 seconds. Static receipts are
`clj-kondo-drift.txt` and `check-parens-drift.txt`.
