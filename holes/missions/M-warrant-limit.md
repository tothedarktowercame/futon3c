# M-warrant-limit

**Status:** MAP in progress (2026-09-28)
**Owner:** claude-8. Authors: Codex and Kimi seats, one packet each.
**Repos:** futon3c (test registry, wire tests), futon2 (product code the tests reach); operational state in `/home/joe/code/storage/test-registry/`.
**Lifecycle:** [Futonic Mission Lifecycle](../../../futon4/holes/mission-lifecycle.md)

## 1. IDENTIFY

### Motivation and operator anchor

Joe, 2026-09-28: "the system is supposed to make life more efficient, not less"; "refactor the code so that tests run based on localised changes. e.g. 12 might be fine but 194 is not"; "if we invalidate more than 10 tests, we trigger a refactor ... we cannot just put up with rerunning tests any more than we can put up with slow futon1b behaviour ... we want the war machine to run automatically and not have to redo loads of work"; "just like with the Inbox0 work, we need to get this to actually run properly".

On 2026-09-28 each edit to product code made 122 to 194 of the 194 wire warrants stale. A full renewal takes about 30 minutes with ten JVMs. Three renewals ran that day; one was made stale within minutes.

### Principle and prior work

A warrant replaces a rerun with a record (`futon2/holes/labs/wm-contract/NOTE-test-registry.md`). A record that goes stale on unrelated edits does not replace the rerun. A consumer reads the producer's record and does not recompute it.

Taken from M-inbox-zero-claim-lifecycle, which has the same parts (detect a condition, queue a request, act, confirm):
- unknown is shown as unknown, never as zero or clean;
- publication is not closure: a result is confirmed by reading it back through the path the consumer uses;
- the consumer is shown working before the old path is switched off;
- correctness does not depend on other processes' discipline;
- a finished job is not a review.

### Scope

In scope: what a warrant depends on; the count of warrants one change makes stale; the request raised when the count exceeds the limit; the refactor of tests and support files that answers a request; the dispatch of that refactor; confirmation that the count fell.

Deferred: recording the definitions a run called (it replaces function objects in the test JVM, `storage/test-registry/reach-d6/REPORT.md`); tests outside the wire suite, until the wire suite meets the limit; narrowing a file's `ns` form from its other top-level text.

### Completion criteria

- [x] **C1 — Dependencies are definitions.** A warrant depends on the definitions its test can reach, not on every file loaded. Rule: `futon2/holes/labs/wm-contract/NOTE-warrant-definition-rule.md`. Built at futon3c a6898528 … 252860bf; read back through the live lookup for all 194 on 2026-09-28 17:42Z.
- [x] **C2 — One test sees a file that stops loading.** `wm-wire-everything-loads-test` keeps the whole-file rule (futon3c a17ab9a9, 252860bf).
- [x] **C3 — The count is reported by cause.** For the current tree: each changed definition, remainder or file, and the warrants it makes stale. `warrant_index.py impact` (futon3c e747cc03); run on the real database 2026-09-28 17:59Z: 0 stale, 0 causes, since no product file has changed since the renewal. Not yet seen on a real product edit.
- [ ] **C4 — Over the limit, a request exists.** A cause with more than 10 stale warrants has exactly one open refactor request, written without anyone asking for it. Built: `impact --record` writes it (e747cc03) and the rerun worker calls that at the start of each pass (a30d70a1). Open: the worker itself is started by hand, so a request is written only when someone starts a pass.
- [ ] **C5 — A request is dispatched without me.** An open request reaches a Codex or Kimi seat as a packet, with a job id on the request.
- [ ] **C6 — A request closes on evidence.** It closes when the same edit, replayed against the refactored tests, makes 10 or fewer warrants stale; the replay's count is on the request.
- [ ] **C7 — The wire suite meets the limit.** No product definition is reached by more than 10 wire tests, or each exception is listed with its reason. At 2026-09-28 17:53Z: 2,511 of 7,683 reached definitions are over.
- [ ] **C8 — Seven days of operation.** For every commit to product code in the period: warrants made stale, requests raised, reruns performed. Read from the database, not from reports.

## 2. MAP

Open. Inputs so far, all under `/home/joe/code/storage/test-registry/`: `exercised-d`, `reach-d`, `exercised-d2`, `reach-i1`, `reach-i2`, `reach-i3`, `reach-i5`, `reach-d6`, `loads-i`. In flight: `limit-i` (C3, C4), `producers-d` (which wire tests run product operations to obtain the value they check, and the producers that would replace that).

## 3. DERIVE — not started
## 4. ARGUE — not started
## 5. VERIFY — not started
## 6. INSTANTIATE — not started
## 7. DOCUMENT — not started

## Log

- 2026-09-28 17:58Z claude-8: mission opened. C1 and C2 recorded as met from the day's work; C3–C8 open.
- 2026-09-28 18:00Z claude-8: LIMIT-I reviewed (e747cc03, codex-1); worker step added (a30d70a1). C3 met; C4 built, not yet automatic.
- 2026-09-28 18:01Z claude-8: PRODUCERS-D reviewed (codex-7, read only): 194 wire tests are 126 runs-operation, 44 calls-function, 24 reads-record; 60 (operation, inputs) groups; estimate 2,511 -> 151 definitions over the limit if every runs-operation test reads a producer's record and calls no product code. 13 tests unbounded (I had said 14; one of the 14 is not a wire test). PRODUCER-1 dispatched to codex-7: the temporal courier flight, ten readers; it sets the pattern.
- 2026-09-28 18:21Z claude-8: PRODUCER-1 reviewed (5485129f, b672ac09, codex-7): record name matches its bytes (checked by me); ten readers reach no product definition (codex-7's measurement; before: 260 for eight, 860 for two); ledger counts unchanged (codex-7's run). Note: a reader's intervention tests now read outcomes the producer recorded; the producer asserts them. 59 packets generated in `storage/test-registry/producer-packets/`. Batch 1 dispatched: PRODUCER-3 codex-7, PRODUCER-4 codex-1, PRODUCER-2 kimi-1 (E-kimi-task-112), PRODUCER-5 kimi-2 (E-kimi-task-113).
- 2026-09-28 18:28Z claude-8: PRODUCER-3 reviewed (79322b06, codex-7): ten files, all under test/; record `target-observe@1e180c170615.edn`, name matches bytes (checked by me); eight readers from 1,884 reached product definitions to none (codex-7's measurement). 18 of 126 converted. PRODUCER-6 dispatched to codex-7.
- 2026-09-28 18:33Z claude-8: PRODUCER-6 reviewed (fc4532f8, codex-7): seven files, all under test/; record `token-input-observe@d377fec38f90.edn`; five readers from 729 and 763 reached product definitions to none (codex-7's measurement). All six record files present have names matching their bytes (checked by me). 23 of 126 converted. PRODUCER-7 dispatched to codex-7.
- 2026-09-28 18:34Z claude-8: PRODUCER-4 reviewed (6f1d42a6, codex-1): ten files, all under test/; record `r9-run-tick@78701532b765.edn`, name matches bytes; eight readers from 1,336 reached product definitions to none (codex-1's measurement). I compared one reader before and after: same deftests, same literal expectations; its check of a live record on disk is now a recorded outcome that the producer asserts. 31 of 126 converted. PRODUCER-8 dispatched to codex-1.
- 2026-09-28 18:38Z claude-8: PRODUCER-2 reviewed (c6312cbd, kimi-1, E-kimi-task-112, 16 min): twelve files, all under test/; record `plan-observe@475e9f6d7541.edn`, name matches bytes; nothing left out; ten readers from 664 reached product definitions to none (kimi-1's measurement); one reader compared before and after by me. 41 of 126 converted. PRODUCER-9 dispatched to kimi-1 (E-kimi-task-114). Finding: producers are tests too. The 60 groups span 18 operations, 16 groups run `flight/run!` alone, and 41 groups have one reader; one producer per group would leave the operations' own definitions with 16 or more dependent producers. After the groups of four or more (PRODUCER-1 to 10), the remaining groups are to be added to a producer namespace per operation family, not given producers of their own.
- 2026-09-28 18:39Z claude-8: PRODUCER-7 reviewed (9449c667, codex-7): seven files, all under test/; record `outer-inputs-observe@5635cbf16d3a.edn`; five readers from 649 reached product definitions to none (codex-7's measurement). All eight record files present have names matching their bytes. 46 of 126 converted. PRODUCER-10 dispatched to codex-7, the last packet with a producer of its own.
- 2026-09-28 18:41Z claude-8: PRODUCER-8 reviewed (fdac4194, codex-1): six files, all under test/; record `ask-out-live-census@2c5da3de3a3c.edn`, name matches bytes; four readers from 6,918 reached product definitions to none (codex-1's measurement); these four were among the tests that kept the whole-file rule. 50 of 126 converted. Order changed: the small groups go out as the per-group packets already generated (they do not share files, so four can run at once), and the producers are merged by operation family afterwards, one packet per family. PRODUCER-11 dispatched to codex-1.
- 2026-09-28 18:42Z claude-8: PRODUCER-5 reviewed (b0bbf058, kimi-2, E-kimi-task-113, 21 min): eight files, all under test/; record `small-observe@396b79fda55e.edn`, name matches bytes; six readers from 351 to 457 reached product definitions to none (kimi-2's measurement); temporary directories replaced by named relations. 56 of 126 converted. PRODUCER-12 dispatched to kimi-2 (E-kimi-task-115).
- 2026-09-28 18:45Z claude-8: PRODUCER-10 reviewed (a05b3e47, codex-7): six files, all under test/; record `fold-in-observe@5fef6275816a.edn`; all eleven record files present have names matching their bytes (checked by me); four readers from 1,237 reached product definitions to none (codex-7's measurement). 60 of 126 converted. PRODUCER-13 dispatched to codex-7. Of the first ten groups only PRODUCER-9 is out.
- 2026-09-28 18:46Z claude-8: PRODUCER-11 reviewed (72c2697e, codex-1): five files, all under test/; record `selection-out-observe@68b72698e9ee.edn`; three readers from 794, 172 and 172 reached product definitions to none (codex-1's measurement). One reader's diff read in full: same deftests and expectations; relation checks are now one assertion in a loop. Noted: this reader's wire declaration had `:live-records-read support/live-records-read` and now has `[]`; the ledger test does not read that key (checked by grep); the census of live records is in the producer record. 63 of 126 converted. PRODUCER-14 dispatched to codex-1.
- 2026-09-28 18:54Z claude-8: PRODUCER-9 reviewed (b8a52aa0, kimi-1, E-kimi-task-114): six files, all under test/; four readers from 260 reached product definitions to none (kimi-1's measurement). The ten largest groups are converted: 67 of 126. PRODUCER-15 dispatched to kimi-1 (E-kimi-task-116). First measurement of `impact` on the real database, 18:50Z: 76 stale reported as 3,485 causes, 2,097 over the limit; a fault in the report, since an edited test lost the definitions on its old record and each was counted as a cause. Fixed at 7f8b48f8 (kind own-test-file). Measured again: 79 stale, 81 causes, none over; 75 are tests edited by the conversion; 4 are whole-file tests made stale by other agents' commits to six files under futon3c/src (pattern cards, rule record, http.clj). Those commits made no narrowed warrant stale. Worker pass 5 started 18:54Z.
- 2026-09-28 18:54Z claude-8: PRODUCER-14 reviewed (d4376428, codex-1): record check by script clean; two readers from 795 reached product definitions to none (codex-1's measurement). 69 of 126 converted. PRODUCER-16 dispatched to codex-1.
