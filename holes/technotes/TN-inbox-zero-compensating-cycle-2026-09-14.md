# Compensating inbox-zero: first live commit-and-push cycle

**Dirty-age flags: 4 -> 3.** The reviewed futon0 README package committed as
`efcc7407e5d784524d77b278d0ddd1c4fbf13695` and was pushed to `origin/main`.
A separate `git ls-remote origin refs/heads/main` confirmed that exact SHA;
futon0's working tree is clean. The other **35 packages** were typed-refused
as `:in-flight`: futon2 (9), futon3 (3), futon3c (23).

The earlier four idle packages were a stale snapshot: futon3's newly committed
workshop pattern changed its activity status. No bytecode was blindly committed,
no ownership claim was invented, and no in-flight repo was swept. This measures
the board's dirty-age clause, not all five inbox-zero clauses.

## Execution and compensation

`batch-dispatch/execute-batch!` requires explicit `:safety :compensating`, a
recorded operator dispatch, exact package review/blob IDs, message, and passing
gates. Strict/default callers still get `:atomic-feel-commit-unavailable`.
The existing consumer lock serializes cooperating consumers; it is not claimed
to lock editors. Forward commits use `futon3.inbox-zero.promote-exec/execute-plan!`;
pushes use `futon3.inbox-zero.promote-push/push-promoted!` only after a clean
postcheck. Neither gates nor hooks are disabled.

The new commit-scoped Python detector arms one recursive inotify queue before
the final idle check. It watches the local ext4 working tree, excluding Git
metadata. New directories themselves count as activity. It detects ordinary
edit-and-restore writes even when the final bytes equal the reviewed bytes.
After commit, the consumer drains the queue, compares committed blobs and
working-tree blobs with the reviewed IDs, checks HEAD/parent, and drains again
to cover activity during verification. Incomplete/overflowed queues, detector
errors and response timeouts are inconclusive, never clean.

A detected edit yields `:finding/type :raced-with-edit`; an inconclusive detector
yields `:detector-inconclusive`. Both trigger compensation. The inverse commit
has the pre-attempt tree and the optimistic commit as its parent. Updating the
branch uses Git's expected-old-SHA compare-and-swap. It does **not** check out,
reset, stash or delete working-tree content, and does not modify the index.
The optimistic changes therefore remain as staged work after their committed
effect is neutralized. That is recorded explicitly and the batch stops.

A moved HEAD, unexpected parent or failed inverse/CAS yields
`:compensation-failed`; other writers' commits are not rewritten, nothing is
pushed, and no restored-safety claim is made. A Git wrapper failure after the
commit is handled by identifying this attempt's unique trailer before attempting
compensation. Successful inverse creation alone is not enough: the CAS must
succeed. Failure preserves reporting, not the survivor guarantee.

## Detection window, cost, and the conditional theorem

The ledger/commit trailers declare a **5000 ms detector response budget** and
`:compensate-on-timeout-or-inconclusive`. Actual deadline attainment is recorded.
This is an operational budget, **not a proved hard-real-time bound**. The old
background watcher's 5000 ms polling interval is not treated as a latency proof.

In the live successful attempt, the promotion executor call took **12.42 ms**;
post-command verification took **6.19 ms**, with zero events and no mismatched
paths. Those sum to an observed **18.61 ms** call-through-verification envelope;
the precise ref-update instant inside Git is not instrumented. The post-command
measurement alone must not be labeled a ref-update-to-detection bound.

The three recorded real-Git race controls measured:

| Control | Detection after executor return | Compensation | Combined after-return interval |
|---|---:|---:|---:|
| edit before staging | 3.86 ms | 5.31 ms | 9.17 ms |
| edit then restore bytes | 3.68 ms | 4.55 ms | 8.23 ms |
| edit after commit | 4.75 ms | 4.80 ms | 9.55 ms |

Every control restored the branch-tip tree to the pre-attempt tree and preserved
the concurrent working-tree bytes. These are measurements of fixtures, not
universal numerical bounds. An optimistic local commit can be briefly visible;
compensation cannot erase earlier observations, arbitrary hook side effects or
historical Git objects. Push waits until verification.

Coordination with codex-17 confirmed the fit to **conditional T2'** in mathlib4
`cf88292da5`, `DarkTower/WarMachine/InboxZeroCompensationWitness.lean`.
The model bounds survival by **D + U after commit**, or **C + D + U after the
edit**, assuming complete detection and successful bounded undo. Finite model
delays are not evidence of bounded production I/O. The ledger records
`:survival/proved-production-bound-ms nil` and `:hard-real-time-guarantee? false`.

Runtime assumptions remain explicit: local admitted filesystem API events,
process survival, OS scheduling, terminating Git commands, no concurrent Git
index writers, and successful compensation CAS. This does not prove handling of
mmap-only transient changes, remote filesystem writes, mount replacement, or
writes through external hardlinks. Persistent blob discrepancies may still be
caught, but detector completeness for excluded events is not claimed. See the
[Linux inotify documentation](https://man7.org/linux/man-pages/man7/inotify.7.html)
for notification limitations. Callers requiring unconditional exclusion retain
the honest strict refusal.

## Evidence and review

The operator-reviewed bytes and executable options are in
`inbox-zero-compensating-review-2026-09-14.edn`. The doc review preserves the
existing author's incident evidence; it does not claim an independent Emacs
crash reproduction or mint an attribution claim.

- `inbox-zero-compensating-cycle-2026-09-14.edn`: before/after certificates,
  per-package review, successful commit witness, push receipt and residuals.
- `inbox-zero-compensation-controls-2026-09-14.edn`: three race-control receipts,
  inverse SHAs, before/after trees, preserved-byte checks, detector events and
  measured detection/undo intervals. Disposable fixture repositories were removed.
- Full live journal: `/home/joe/code/storage/inbox-zero/compensating-batch-2026-09-14.edn`,
  SHA-256 `6433196164ff7d6ac1b29423341385216b2a105b12fe9fb7abe1e902243487f8`.

The actual commit message cites the per-repo proposal:

- board `sha256:6070d733e67e2a35e32788a53e341cb6fb8ff04699e6d3bbd05555ddd6c56a20`
- inputs `sha256:e152858f745f13841fe7f3b80fb6f565b6e9d45c55a9ace9fdc41668570d0f31`
- verbs `sha256:c54bdbe94b1c192333e08fd63902fc6e607285fda7c6865b060e4948c58f59f0`
- attempt `4653a689-90cd-46c0-8be4-2d6da3dddd60`

As before, verb digests identify functions within the generating process;
archived digests do not establish cross-process runtime correspondence.

## Validation

Run each namespace separately from canonical futon3c:

```sh
clojure -M:test -n futon3c.inbox-zero.batch-dispatch-test
clojure -M:test -n futon3c.inbox-zero.compensating-commit-test
clojure -M:test:test-all -i :slow -n futon3c.inbox-zero.compensating-commit-test
clojure -M:test:test-all -i :slow -n futon3c.inbox-zero.compensating-batch-test
python3 scripts/test_inbox_zero_detector.py
```

**13 Clojure tests / 84 assertions passed; 2 Python detector tests passed.**
Real-Git tests are tagged `^:slow`. They cover quiet success, edit-before-stage,
edit-after-commit, edit-and-restore, detector timeout/overflow, moved HEAD,
post-commit wrapper failure, and a full 1->0 commit-and-push cycle against a
local bare remote. All changed Clojure passed clj-kondo **0/0** and
`futon4/dev/check-parens.el`; no shared JVM was restarted or branch-loaded.

## Follow-up: consult standing watcher history after commit

The audit prompted by `invoke-1789415249330-20832-4252f995` found that
06ff3181 consulted standing watcher history for the final idle check, but its
postcheck used only inotify and Git. The original live 4->3 receipt above
**predates this correction**; it must not be cited as exercising the added read.

The idle check now returns its watcher snapshot. Postcheck rereads the same
standing snapshot and overlays a fresh scan through the existing watcher, then
examines every newly recorded file observation for this repository. Intermediate
edit-and-restore observations remain evidence even when the latest content
matches. Unchanged content/status and the reviewed paths' clean transition at
our commit are the expected effects; unexpected observations trigger
compensation. Missing or altered immutable history is inconclusive. Missing
source callbacks refuse before committing; failed postcommit reads compensate.
Inotify remains armed across these reads and the final Git checks. The journal
names all four sources in the assumptions. The 5000 ms operational response
budget includes the history read; it is still not a proven wall-clock bound.

Compensation still creates an inverse tree commit and uses compare-and-swap to
advance the ref. It performs no checkout, reset, or index write. The new
watcher-only race control checks actual index bytes, working file bytes and the
restored tree, while inotify reports zero events. Existing real-edit controls
exercise edits before staging, after committing, and edits restored in-window.
The moved-HEAD control still returns a typed compensation failure and preserves
the survivor. These are runtime controls for conditional T2', not a proof of
unconditional detector completeness or eventual successful undo.

The corrected live rerun was **3->3**, with **0 commits, 0 pushes and 33 typed
`:in-flight` package refusals** across futon2, futon3 and futon3c. No admissible
package remained from the earlier reviewed batch. The corrected real-Git batch
integration control committed and pushed to its disposable bare remote, 1->0.

New evidence: `inbox-zero-compensating-watcher-recheck-2026-09-14.edn`, containing
both board certificates and every outcome. Full runtime journal:
`/home/joe/code/storage/inbox-zero/compensating-watcher-recheck-2026-09-14.edn`.
Before and after board digest:
`sha256:f584796e759670112b0179359bc96152db00371a42e498296a1a662fffecfd5c`;
input digest:
`sha256:103c02a0d282820175cbdb4b920913cc9bbac8e5dbc883f7c57d3b9c2b26b9d0`;
verbs digest:
`sha256:fed5a2ab9f2805239cdd4646d39cc874f79f61fe84e17309146a122a3da7fb7c`.
Both certificates replay-verified in their generating process.

Follow-up validation: **17 Clojure tests / 102 assertions**, one namespace per
invocation using the commands above; **2 Python detector tests**. Changed
Clojure passes clj-kondo **0 errors / 0 warnings** and the workspace paren gate.
