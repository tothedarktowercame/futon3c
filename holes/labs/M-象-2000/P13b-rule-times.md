# P13b: sourced rule application intervals

Adoption, commit and runtime application are separate observations. The original
P13a description is unchanged. Two new minted `rule/record` versions reference it.
Their storage valid time is the time of this retrospective recording; historical
application queries use the embedded sourced timeline, not storage or commit time.

The initial adoption is reconstructed from Joe's three operator acts at 16:03,
16:20 and 16:28. There is no P3 grant record. Both commits (80428193 precursor,
5146606d definition) remain queryable. First observed application is the harness
notice at 2026-09-24T19:04:24.252395403Z, identified through origin/backfill.
There is no claimed September 24 code-load receipt. This is a dispatcher-run
rule, for which the first execution is the application witness.

The removal version retains 2ef7a010 as an intermediate deduplication change;
it did not withdraw followups. Its actual load receipt is 19:37:18.013Z.
626df9fa removed followups. The saved Claude tool result at
2026-09-25T20:05:51.002Z reports successful namespace reload AND execution of the
new no-op `enqueue-caller-followup!`. This is the removal's application time.
The last notice at 19:59:53.408044206Z is retained only as delivery evidence,
not used to infer withdrawal. Exact commands/results, transcript file hashes,
line numbers, tool-use and result IDs are embedded in the fixture/records.

`rule-timeline/intervals` produces half-open intervals ordered by positive runtime
observations. Unobserved versions do not close an interval. `as-of` returns
"committed, not yet live" before application; `promulgated-as-of` independently
returns adoption/commit events. Code-kind versions require load evidence.
T10 `:stale-runner-source` / `:loaded-file-code-mismatch` are negative observations,
not proof of application. Present-day T10 cannot establish a historical load.

Run `clojure -M scripts/xiang2000_p13b.clj` to validate without writing;
`--write` appends both minted, idempotent versions and verifies readback.
No server route or JVM reload is needed. P0 snapshots include raw `rules.json`
under both capture-time XTDB pins and its integrity manifest, permitting offline
replay. P0 rows 9 and 10 now query application time; only clearance remains a row
stub. The separate retrieval system-time subclaim remains explicitly unresolved.

## Live acceptance

Implementation: `fc7e768a`. Both appended records verified through GET hyperedges:

- Initial application: `act:4b526112-dd5d-4765-a8a6-ed8701d0089c`.
- Followup withdrawal: `act:9bb67b13-caf7-4f96-9232-52bacf99fd8e`.

Kondo: 0 errors/warnings. Parens: OK. Namespaced rule-timeline tests: 5 tests,
15 assertions; rule-record: 5 tests, 36 assertions; all passed. Python P0 tests:
5 passed; py_compile passed.

Live `--snapshot /tmp/p13b-p0-final --check`: exit 0. Two `--from-snapshot`
replays matched the live bytes exactly. Output:

```
QUERY | as of 09-24 17:00 | committed, not yet live
QUERY | as of 09-25 21:00 | followup half withdrawn
stubs: 1 of 12
check: PASS
```

Bad case: replacing initial live time with commit time in a COPY of the snapshot,
then updating its integrity hash, yielded exit 1 naming **row 9**, expected
`committed, not yet live`, got `requisition rule applied`. An additional byte
alteration without updating the manifest yielded exit 1, `snapshot hash mismatch:
rules.json`, with no answer printed. No server reload was needed.
