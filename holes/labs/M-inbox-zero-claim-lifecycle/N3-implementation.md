# N3 — C8 local implementation: uncertain-pressure producer → consumer chain

**Mission:** [M-inbox-zero-claim-lifecycle](../../missions/M-inbox-zero-claim-lifecycle.md) C8; contract per owner requisition (N2 `cf87f30d`, option A).
**Status:** LOCAL IMPLEMENTATION ONLY. Not deployed. No runtime reload/start,
no sweeper tick, no notification sends, no scheduler start. C8 not claimed
complete. Full DERIVE and S2 remain pending separately.

## Commits and changed paths

| Repo | SHA | Paths |
|---|---|---|
| futon3c | `4a20507d` | `src/futon3c/inbox_zero/sweeper.clj`, `test/futon3c/inbox_zero/sweeper_test.clj` |
| futon0 | `8399bee` | `scripts/mana-snapshot.bb`, `scripts/mana_snapshot_uncertainty_test.bb` (new) |
| futon2 | `8427db315` | `scripts/futon2/report/war_machine.clj`, `test/futon2/report/war_machine_test.clj` |

No other paths staged in any repo; pre-existing dirt untouched.

## What changed (structural, not a flag)

- **futon3c sweeper:** the recipient/delivery machinery (`attribute`,
  `recipients`, `notice-prompt`, `followup-payload`, `default-deliver`,
  notices ledger, renotify/dedupe) is **deleted**, not disabled — there is
  no flag that can re-enable temporal-overlap cleanup assignment. The pass
  now emits uncertain-ownership rows for **every** over-threshold repo into
  (a) the operator backlog (closing the mixed-repo `empty?-targets`
  omission) and (b) an atomic `uncertain-pressure.edn` feed keyed by
  **canonical worktree root**, with bounded `:newest` (5) + explicit
  `:remainder`, `:diagnostic-overlaps` labeled non-authorship, and
  `:interval-ms`/`:drilldown` for consumer staleness and navigation. Push
  and merged-worktree lanes are unchanged. Historical claim/confirmation
  records are never read by this lane.
- **futon0 mana snapshot:** merges the feed per-repo by canonical root
  (`fs/real-path` vs sweeper `getCanonicalPath`); missing ⇒ `:status
  :missing`, malformed ⇒ `:status :malformed`, stale (>2× interval) ⇒
  merged but `:stale?`/`:uncertain-stale`; no repo ever gets an
  authoritative zero. Path overridable via `FUTON0_UNCERTAIN_PRESSURE_PATH`
  (tests use it).
- **futon2 projection+render:** `scan-metabolic-balance` carries
  `:uncertain`/`:uncertain-stale`/`:uncertainty` through its map
  reconstruction; `summarize-working-tree-hygiene` (not
  build-commit-hygiene — owner's correction applied) includes
  pressure-zero uncertain repos, keeps the 8-queue bound with explicit
  `:queue-remainder` + drilldown, surfaces `:missing` as unavailable;
  render shows `count (K ownership-unknown)` and an
  `… + N more repo(s) — full detail: <backlog path>` line. Pressure
  semantics untouched (P formula, tiers, channels unmodified).

## Validation (all run, one namespace/script at a time)

- futon3c: `clj-kondo` clean (src+test); check-parens OK;
  `clojure -M:test -n futon3c.inbox-zero.sweeper-test` → **22 tests, 63
  assertions, 0 failures** (includes Joe's 10-file/3-overlap example ⇒ zero
  deliveries, all 10 represented; sole overlap; historical-records;
  rollover; mixed repos; idempotent bounded passes; canonicalization).
- futon0: `bb scripts/mana_snapshot_uncertainty_test.bb` → **4 tests, 13
  assertions, 0 failures** (missing/malformed/fresh-symlink-alias/stale;
  label-collision rejection). The test strips the `-main` guard before
  `load-string` so it cannot regenerate the real snapshot.
- futon2: `clj-kondo` clean; check-parens OK;
  `clojure -X:test :nses '[futon2.report.war-machine-test]` → **94 tests,
  532 assertions, 0 failures**, including a real snapshot-file →
  scan-metabolic-balance → summarize → render integration test (not stubs).
- p6o3: `python3 -m unittest test_xiang2000_p6o3` → 6 tests OK; no new
  notice template exists, historical INBOX classification untouched;
  `py_compile` OK.

## Deployment state and exact activation sequence (NOT executed)

Current live state (owner-verified, re-confirmed read-only during N3):
`GET /api/alpha/war-machine` → 503 `war-machine-snapshot-unavailable`,
scheduler `running? false`. The running futon3c JVM still executes the OLD
sweeper code, which still emits the unsound personal notices. **Corrected
sequence (N4, finding 5): the consumer must be proven live BEFORE the old
lane is cut over** — removing the old notices while the pressure surface is
still 503 trades a noisy lane for an unread one. Deployment is a separate,
reviewed authorization; this packet executes none of it.

1. **Prove the consumer first:** regenerate one WM snapshot (one
   `futon2.report.war-machine` run, or start the wm scheduler — Joe's call
   per the restart-safety rule) and verify `GET /api/alpha/war-machine`
   serves commit-hygiene queues with the `ownership-unknown` annotation,
   remainder names, and the rendered `### Uncertain detail` section. If
   this fails, STOP — the old lane stays until the surface works.
2. **Reload sweeper in the futon3c JVM** (Drawbridge `load-file
   src/futon3c/inbox_zero/sweeper.clj`, futon3/AGENTS.md procedure) — stops
   the unsound notices structurally.
3. **Paired publisher/consumer verification:** confirm one sweeper pass
   writes `storage/inbox-zero/uncertain-pressure.edn` and the backlog
   (read-only checks), run one mana snapshot
   (`bb futon0/scripts/mana-snapshot.bb`), verify `:uncertainty {:status
   "available"}` and per-repo `:uncertain`, then re-verify the
   war-machine route shows the SAME repo's uncertain count — publisher
   and consumer checked as one pair, not independently assumed.
4. Only then is C8's "documented triage route" demonstrable; C8 stays
   unchecked until a reviewer sees step-3 outputs.

Prerequisite risks: step 4 depends on the WM scheduler/futon2 report
pipeline being runnable in this environment (currently down — read-only
diagnosis shows `last-error null`, never started this JVM boot; starting
it is a runtime decision outside this packet).

## Limitations and disclosures

- The WM UI markdown synthesis path is futon2-side and now renders the
  annotation; any other consumers of `commit-hygiene :queues`
  (street_sweeper_backend) see new keys `:uncertain-count`/
  `:queue-remainder` — additive, shape-compatible.
- **Disclosure:** during futon0 test development, an early test draft
  evaluated the producer's `-main` guard once and regenerated
  `storage/futon0/mana-snapshot.json` (a regenerable HUD feed file). The
  test was fixed to strip the entry guard; no other state was written.
- Emacs/HUD surfaces read the same war-machine JSON; no Emacs change was
  needed or made.
- The personal-notice ledger file (`commit-notices.edn`) is now unread
  legacy state; left in place (deleting state is not this packet's call).
- S2 transaction blocker and capture spike remain separately pending;
  nothing here advances DERIVE.
