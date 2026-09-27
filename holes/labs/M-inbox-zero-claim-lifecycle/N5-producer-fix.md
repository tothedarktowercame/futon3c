# N5 — producer integration failure closed

**Corrects N4 (`6bc34c88`, `2165e90`, `57a9460e4`).** Local implementation
only; no deployment/reload/start/live state/notifications. N4 improvements
preserved. C8/full DERIVE not claimed; S2/capture deferred; prompt
coordination (a36f3dc5/c1191954) untouched — no contact, no implementation.

## Owner reproduction → fix

**Producer NPE (run verbatim):** `bb scripts/mana-snapshot.bb --out <tmp>`
with a valid fresh feed containing one union-only root failed at
`mana-snapshot.bb:212` — `(apply max 0.0 (map :P per-repo))` NPE'd on the
union row's absent `:P`, exit 1, no output.
**Fix:** max aggregates **measured** pressures only (`keep :P`), and the
snapshot now carries `:pressure-coverage {:measured n :unmeasured m}` so
the aggregate can never imply complete measurement; per-row nil `:P` stays
absent (unknown, not zero).
**Verified (real producer run, disposable feed+out, sandboxed):** exit 0,
union row present with no `:P`, `coverage {measured 19, unmeasured 1}`,
`:collection-complete? true`, `:backlog-written? true`.
**Downstream seam:** `street_sweeper_backend/list-repos-with-pressure`'s
`(sort-by (comp - :pressure) …)` NPE'd on the same nil — fixed (unmeasured
sort last) and tested through the real backend fn with the HTTP boundary
stubbed.

## Contract failures → fixes

- **≤8 bound + complete detail:** summarize retains `:all-queues`; the
  renderer's `### Uncertain detail` sections now iterate ALL active queues,
  so the ninth uncertain repo's paths appear in the markdown (tested for
  all nine markers). Pressure-queue identities/measurements remain
  accessible via the named `:remainder-repos` line.
- **Unknown formatting:** nil pressure/age render as `?` (table) and
  `pressure unavailable`/`age unavailable` (needs-fixing); semantic nil
  retained in data; tests assert no fabricated `0.00`/`0.0d` appears.
- **Interval wiring:** the loop previously ran `(dissoc options
  :interval-ms)`, and the feed hard-coded `default-interval-ms`. The
  configured interval now flows loop → pass → feed (`:interval-ms` in the
  EDN), tested with a nondefault 42000.
- **Row validation:** absolute roots only (relative/traversal rejected);
  `:paths` entries must be unique non-blank maps with length ==
  `:dirty-count`. No scope expansion beyond shape correctness; render
  emits plain markdown like existing helpers.
- **Completeness propagation:** git-scan failures are per-repo fault
  tolerant (recorded, not pass-killing); the feed carries
  `:collection {:complete? :row-failures}` and
  `:publication {:backlog-written?}`; futon0 propagates them;
  futon2 renders a `Pressure completeness: …` line whenever collection is
  incomplete or the backlog write failed. Tests: row failure (one repo's
  git-fn throws ⇒ row absent, `complete? false` in counts AND feed),
  backlog failure (marker inside the successfully written feed), feed
  write failure (N4's INCOMPLETE path, retained).

## Validation (all executed)

- futon3c: clj-kondo clean (4 files); check-parens OK;
  `-n futon3c.inbox-zero.sweeper-test` → **27 tests, 85 assertions, 0
  failures**; `-n futon3c.peripheral.street-sweeper-test` → **42 tests,
  130 assertions, 0 failures** (incl. nil-pressure backend consumer test).
- futon0: `bb scripts/mana_snapshot_uncertainty_test.bb` → **4 tests, 22
  assertions, 0 failures**; real producer repro run (above) exit 0;
  bb files form-read verified.
- futon2: clj-kondo clean; check-parens OK;
  `clojure -X:test :nses '[futon2.report.war-machine-test]` → **99 tests,
  562 assertions, 0 failures** (ninth-repo detail, `?` rendering,
  completeness line, plus all N3/N4 tests).

## Chain coverage honesty

No single process spans bb→futon2→futon3c-backend, so the chain is three
real legs, not one: (1) real bb producer run against a real feed file with
a union-only root; (2) real snapshot-file → scan → summarize → render in
futon2; (3) real backend consumer fn against a stubbed HTTP boundary
serving a nil-pressure payload. Each leg uses real code and real data at
its own boundary; none is an isolated-merge test.

## Remaining blockers

- Deployment still blocked per the corrected N3 sequence: consumer must be
  proven live (WM scheduler/report regeneration) before cutover; separate
  reviewed authorization; unexecuted here.
- Unmeasured queues sort last in the backend — a conservative display
  choice; owner may prefer explicit unknown-band grouping.
- Legacy `commit-notices.edn` remains unread legacy state.
