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
