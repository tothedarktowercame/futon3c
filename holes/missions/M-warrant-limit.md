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
- [ ] **C3 — The count is reported by cause.** For the current tree: each changed definition, remainder or file, and the warrants it makes stale.
- [ ] **C4 — Over the limit, a request exists.** A cause with more than 10 stale warrants has exactly one open refactor request, written without anyone asking for it.
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
