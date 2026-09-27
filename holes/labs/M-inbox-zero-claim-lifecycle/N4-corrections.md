# N4 — visibility corrections to the C8 uncertain-pressure chain

**Supersedes/corrects:** N3 (`4a20507d`, `8399bee`, `8427db315`; report
[N3-implementation.md](N3-implementation.md), whose deployment sequence is
corrected in place). Local implementation only: no reload/start/live state/
notifications; C8 not claimed; S2/capture and full DERIVE still deferred.

## Owner findings → changes

**F1 — full per-file drilldown.** `uncertain-row` now carries `:paths`
(complete, newest-first) in both the backlog and the feed; bounded displays
take a prefix and `:remainder` reports the rest. The War Machine markdown
itself renders a `### Uncertain detail — <repo>` section per uncertain
queue row listing **every** dirty path — navigation lives inside the
existing rendered consumer, not as a naked filesystem path (the backlog
path remains as the file-level reference). Test: Joe's 10-file example
asserts 10 paths in feed AND backlog and rendered detail lines
(`- runs/out-0.edn` … `- runs/out-9.edn`).

**F2 — union join and identity.** `merge-uncertainty` (now pure, in
futon0 `scripts/mana_uncertainty.clj`) unions: feed roots absent from the
mana manifest are appended as `:uncertain-only` rows with identity only —
**no invented pressure/count measurements** (no `:P`, no `:count`). The
owner's direct counterexample (`merge-uncertainty [] feed-with-dirty10`)
now returns one row. futon2 `scan-metabolic-balance` carries
`:abs-path`/`:uncertain-only`/nil pressure through (previously dropped);
summarize computes `:display-name` so same-label worktrees render
distinctly (`futon3c [/home/joe/code/worktrees/futon3c-d]`); the >8 bound
now emits `:remainder-repos` (names of ALL remaining queues, pressure and
uncertain alike) in the rendered remainder line.

**F3 — validation and unknown-vs-zero.** `validate-feed` rejects:
non-map, non-vector repos, missing/future timestamps, non-positive
`:interval-ms` (staleness uses the feed's configured interval, never a
constant), invalid rows (blank root, negative or untracked>dirty counts,
`:paths` length ≠ `:dirty-count`), and duplicate canonical roots. The
owner's `{:repos [{:root "/tmp" :dirty-count -7}]}` ⇒ `:malformed
:invalid-row`. futon2 `:uncertain-count` is **nil** when the feed has no
row (unknown), never 0; the renderer prints an explicit
`Uncertain-ownership feed: <status>[(stale)]` line whenever the feed is
missing/malformed/stale — tested against actual rendered markdown.

**F4 — aux failures and typed completeness.** windows/roster failure
degrades diagnostics only: rows, backlog, and feed are still written, with
`:diagnostics-available? false` in counts AND feed. Both writers return
booleans; `finish-pass` emits `:backlog-written?`/`:feed-written?`/
`:complete?` and prints `INCOMPLETE` on any write failure — no false
fresh/complete pair. Tests cover both (throwing windows-fn; blocked feed
path).

**F5 — deployment sequence + sandboxing.** N3's reload-first order is
corrected (see N3-implementation.md): consumer proven live first, then
cutover, then paired publisher/consumer verification; deployment remains a
separate reviewed authorization, unexecuted. The futon0 test no longer
strips entry guards: pure logic moved to `scripts/mana_uncertainty.clj`
(side-effect-free at load), which both producer and test load directly;
temp dirs are deleted after the run; a sandboxed producer smoke run used
`--out` into a disposable directory only. `.bb` paren checking: the
check-parens tool skips `.bb`, so both bb files were verified by a full
PushbackReader form-read (26/15 forms OK); `mana_uncertainty.clj` passes
the real check-parens as a `.clj`.

## Validation (all executed this packet)

- futon3c: clj-kondo clean; check-parens OK;
  `clojure -M:test -n futon3c.inbox-zero.sweeper-test` → **24 tests, 75
  assertions, 0 failures**.
- futon0: `bb scripts/mana_snapshot_uncertainty_test.bb` → **4 tests, 22
  assertions, 0 failures** (incl. owner counterexample, union, alias join,
  stale-via-feed-interval); producer smoke `bb scripts/mana-snapshot.bb
  --out <tmp>` works, `:uncertainty {:status "missing"}` honestly (no live
  feed yet).
- futon2: clj-kondo clean; check-parens OK;
  `clojure -X:test :nses '[futon2.report.war-machine-test]` → **96 tests,
  543 assertions, 0 failures**, incl. real-file
  producer→projection→render integration with rendered drilldown lines.

## Joe's steering note (recorded, not implemented)

With claude-7 building starship-like agent/design-pattern prompts
(🐘/诺必践 family): this reporting feed is a **potential consumer-side
input only**. A prompt mark like `?` may denote unattributed checkout
dirt; `*` must not appear until verified current-work attribution exists
(which the claim lifecycle has not yet established); a baseline prompt
never asserts clean. **This feed grants no ownership.** No prompt
implementation or contact was made.

## Limitations

- `### Uncertain detail` lists paths for the ≤8 displayed queues; repos in
  the remainder keep full detail in the backlog/feed files (named in the
  remainder line). Rendering full detail for all remainder repos is a
  display-budget decision for the owner.
- The legacy `commit-notices.edn` ledger remains unread legacy state.
- Live cutover state unchanged: the running JVM still executes the old
  sweeper until the corrected deployment sequence is authorized and run.
