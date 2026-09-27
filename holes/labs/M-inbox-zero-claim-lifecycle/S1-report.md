# S1 — ref/index gap closure and pinned recovery identities

**Mission:** [M-inbox-zero-claim-lifecycle](../../missions/M-inbox-zero-claim-lifecycle.md). C1–C7 unchanged.
**Packet:** S1 transaction correction, author kimi-9 for owner codex-5.
Evidence spike feeding DERIVE; not acceptance of any design, no phase movement.
S0 artifacts ([S0-report.md](S0-report.md), s0 script/transcript) untouched.
**Artifacts:** [s1-git-transaction.py](s1-git-transaction.py),
[s1-transcript.txt](s1-transcript.txt), this report.
**Environment:** git 2.43.0, python 3.12.3. Disposable repos, cleanup in
`finally`, `timeout 300`. No production/runtime/settings/network writes.

**Run:** `python3 s1-git-transaction.py` → exit 0.
**Score:** 30 assertions — historical S0 defects re-confirmed (5, re-run
unmodified under their original `[repro-*]` labels), both new owner
reproductions confirmed (2/2), S1 candidate demonstrations 23/23 passed.

## 1. Owner reproductions confirmed as defects (against the S0 candidate)

- **[owner-gap]** `candidate_commit(crash_at='after-cas')` then native
  `git commit` in the gap: the porcelain commit lands stale index X over
  promoted HEAD H. The post-CAS refresh is **not** cosmetic — the unlocked
  window permits a silent revert. DEFECT REPRODUCED.
- **[owner-receipt]** After a descendant commit lands in that gap, S0
  `recover()` pinned the receipt to *current HEAD* (the descendant), not the
  claim commit. DEFECT REPRODUCED.

## 2. S1 transaction candidate (what changed and why)

**Lock scope.** The dangerous interval is exactly ref-CAS → index-refresh;
everything earlier is covered by the CAS failing on moved HEAD. S1 therefore
holds `.git/index.lock` only from immediately before the CAS until after the
refresh — milliseconds, not the gate/hook duration. This works because git
plumbing with `GIT_INDEX_FILE` locks `<tmp>.lock`, never `.git/index.lock`,
so the holder can still operate (asserted in `s1-gap`; S0's run-1 failure
came from trying to use the *shared* index under the lock, which S1 never
does). Native `git add` **and** `git commit` are refused with the standard
`index.lock` error during the interval and are usable immediately after
release (`s1-gap`, `s1-ordinary`). No global disabling; readers (`git
status`) are unaffected.

**Lock ownership and orphan recovery.** Our lock file carries JSON
`{magic, tx, pid, host, ts}`; native locks (empty) are distinguishable.
Recovery (`s1-crash` C2/C3): an ours-formatted lock with a **dead pid**
(same host) is removed only after byte-for-byte content re-verification
immediately before unlink; a **foreign** lock is reported
`:foreign-lock-present` and never touched — journal resolution and receipt
still complete, refresh is deferred, not forced. Stated limitations, not
worked around: pid liveness is checkable same-host only
(`:orphan-lock-cross-host-left` recorded otherwise); pid reuse within the
crash window is theoretically possible — the content re-check plus journal
resolution bound it but do not eliminate it; the check-then-delete race
against a *third party* recreating a lock is mitigated by the content
match, not closed against an adversary who copies our content.

**Pinned recovery identities.** `find_claim_commit` locates the exact
commit by trailer **and** parent == prepare.h0 **and** tree == frozen
staged tree — never current HEAD. `s1-crash` C4 (descendant changed the
same path after an admin-removed lock): the receipt names the exact claim
commit (≠ HEAD), refresh is `:refresh-skipped-head-moved`, and the
descendant's content is verified untouched. An outcome record naming an
unverified commit is `:journal-conflict`, fail closed.

**Journal acquisition and durability.** `jacquire` uses `O_EXCL`
create-new (rename-overwrite cannot be single-winner), file fsync +
directory fsync, write-once records, and a sha256 identity over canonical
JSON. `s1-contention`: 8 racing threads → exactly 1 winner; losers re-read
the winner's prepare under acquisition and detect staleness
(`:stale-plan`); a malformed prepare fails closed
(`:journal-malformed`, claim never promotable).
**Crash-test honesty:** all cutpoints are *simulated early returns* inside
the test process; dead-pid status in C2 is emulated by rewriting our own
lock content. No SIGKILL and no power-failure durability was exercised;
the fsync boundary is implemented but only application-ordering guarantees
are asserted.

**Authorization identity under filters.** S1 stages the object returned by
the actual `hash-object -w` **and** requires it to equal the frozen
mint-time authorized OID; mismatch ⇒ `:authorization-mismatch` — typed
refusal, no commit, no restaging of whatever a stateful filter returned,
claim stays excluded (`s1-filter`, with S0's deterministic
invocation-count filter). No raw-bytes "suspicion" weakening.

## 3. Evidence grading corrections (owner's report findings, applied)

- **I1 and I8 are UNTESTED**, not go: no planner-predicate or
  link-derivation tests exist in S0 or S1. S0's CE2 assertion tested the
  trailer scan only, not `attributable?`.
- `git diff`-based baselines normalize through filters; S0's report
  language suggesting raw-byte trust from them is withdrawn. Raw-byte
  baseline trust remains an open capture-spike requirement.
- No byte-identical alias exception is asserted anywhere in S1; the
  C2/C4 paths require exact commit identity regardless of content
  equality.
- S0's "post-CAS refresh is cosmetic" claim is withdrawn per [owner-gap].

## 4. Retained positives and preservation properties

`s1-ordinary`: clean status after success, no phantom reversal, later
porcelain commit does not revert, native writers usable after.
`s1-foreign`: other-path foreign staged content survives the whole locked
transaction; same-path foreign staged at plan time ⇒ `:staged-elsewhere`
hold with no commit, foreign blob untouched, no lock residue.
`s1-crash` C1: after-prepare crash ⇒ ambiguous, fail closed, never
reauthorized. Historical `[repro-*]` cases (A, B, D1-clobber) re-run
unmodified and still reproduce.

## 5. Go/no-go by invariant (corrected grading)

| Invariant | Status | Basis |
|---|---|---|
| I1 plan⇒promotable | **untested** | no planner tests in S0/S1 |
| I2 staged==authorized | go | s1-ordinary, s1-filter (returned==authorized or typed refusal) |
| I3 commit==authorized delta, CAS, no uncontrolled index race | go as revised: real index.lock held over CAS→refresh; foreign staged preserved | owner-gap defect, s1-gap, s1-foreign |
| I4 committed⇒consumption durable | go as revised: journal prepare/outcome + exact-identity recovery; trailer-loss ⇒ fail closed | repro-B (historical), s1-crash C1–C4 |
| I5 consumed⇒never promotable | go under journal | s1-crash, s1-contention |
| I6 mint⇒verified capture | **untested** (out of scope for S1) | capture spike still owed |
| I7 trusted baseline | partial: raw-vs-filtered rule owed | §3 |
| I8 links via committed objects | **untested** | no link-derivation test exists |

## 6. Remaining structural blockers / unanswered requirements

1. Journal records must become witness-store records (schema: prepare,
   staged, outcome, receipt as immutable claim-targeted records); S1 models
   them as files. If the store cannot carry them, that is the blocker —
   S1 confirms again that no purely-Git mechanism is monotone under ref
   rewrites.
2. Orphan-lock recovery's cross-host and pid-reuse limits (§2) need an
   owner decision: accept as documented residual, or require an operator
   token in the journal before clearing any lock.
3. Capture spike (I6) prerequisites from S0 §5 are all still open, plus
   the mode-authorization question.
4. Power-failure durability of the fsync boundary is implemented but
   untestable at this level; if the mission requires it, a kill -9 /
   filesystem-level fault-injection spike must be named separately.
5. Whether the planner holds on `:foreign-lock-present` recovery outcomes
   or merely defers — availability policy, not safety; undecided.

## 7. Validation of this packet

`python3 -m py_compile s1-git-transaction.py` OK; full run exit 0
(30 assertions, transcript committed unedited). S0 artifacts byte-untouched
(git status confirms only s1-* added). Markdown links verified against
sibling artifacts. No Clojure/Lisp/EDN authored; wiring EDN unchanged
(still proposed). No production, runtime, live-settings, network, or
shared-state writes.
