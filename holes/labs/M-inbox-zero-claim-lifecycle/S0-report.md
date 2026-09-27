# S0 — real-Git transaction evidence (DERIVE input, not approval)

**Mission:** [M-inbox-zero-claim-lifecycle](../../missions/M-inbox-zero-claim-lifecycle.md). C1–C7 unchanged.
**Packet:** S0 evidence spike, author kimi-9 for owner codex-5. Not a DERIVE
verdict, not ARGUE advancement, not production code.
**Artifacts:** [s0-git-transaction.py](s0-git-transaction.py) (harness),
[s0-transcript.txt](s0-transcript.txt) (full captured run), this report.
**Environment:** git 2.43.0, python 3.12.3. All repos disposable
(`tempfile.mkdtemp`, cleanup in `finally`), run under `timeout 300`, no
network, no production state, no live CLI settings touched.

**Run:** `python3 s0-git-transaction.py` → exit 0.
**Score:** 6/6 defect reproductions confirmed; 26/26 desired-behavior
demonstrations passed (32 assertions total). Assertion naming distinguishes
`DEFECT REPRODUCED` (evidence against a design — no safety criterion is
claimed to pass there) from `BEHAVIOR OK` (candidate protocol property).

## 1. Owner findings reproduced (defects, against D2 8fedc4cc)

- **repro-A (owner A, 3 assertions).** D2 ordinary success, *no* foreign
  writer: private-index commit lands H, but the shared index still holds X,
  so `git status` = `MM f` and the staged diff is an H→X **reversal** of the
  promotion. The harness then runs an ordinary porcelain `git commit` and
  shows HEAD reverting to X — the phantom staged entry silently undoes
  promoted work. D2 §9's "clean" positive trace is false, and D2's "every
  untouched staged entry is foreign dirt" mislabels the ordinary case.
- **repro-B (owner B).** Trailer commit, crash before receipt, then
  `git update-ref HEAD <mint-head>`: the `mint..HEAD` trailer scan returns
  nothing. Reachable-history consumption is **not monotone under ref
  rewrites**. D2 R1 as written is insufficient.
- **repro-CE2 (original).** B commits X on the new HEAD and re-dirties H:
  the trailer scan *does* hold (trailer survives ordinary later commits), so
  the trailer half of D2 R1 covers the original CE2 — but only while refs
  are not rewritten.
- **repro-D1.** `git reset -q HEAD -- f` (D1 step-5) destroys a foreign
  same-path staged entry (staged OID changed away from B's blob). Confirms
  the owner's earlier repro; D1's resync stays withdrawn.
- **repro-D2 (3 assertions).** D2's private index *does* preserve a foreign
  same-path staged entry and commits exactly the authorized blob; status
  honestly shows `MM f` as foreign dirt. Preservation works; the ordinary
  path (repro-A) is what was broken.

## 2. Candidate transaction protocol (task 2)

**Shape:** durable prepare → gates/hook → private stage → frozen-tree
verify → commit-tree (trailer) → ref CAS → durable outcome → conditional
shared-index refresh → receipt. Journals are atomic-rename files standing
in for the existing validated witness intake.

- **Index CAS (the new mechanism).** Build the refreshed index as a *copy*
  via `GIT_INDEX_FILE` (no lock needed), then acquire the real
  `.git/index.lock`, compare the live index file's **raw bytes** to the
  pre-copy snapshot, and `rename(2)` the copy into place only on exact
  match. `cand-lock` demonstrates native `git add` refuses while
  `index.lock` is held and proceeds after release — the lock interoperates
  with native writers by Git's own mechanism; it is not our advisory
  convention. Any interleaving native write changes the bytes and aborts
  the refresh (`cand-I4`, 4 assertions including retry-after-interleaving).
- **Refresh guard.** An entry is advanced to the committed blob only when
  the live entry equals the prepare-time entry **and** that entry equals the
  clean mint-HEAD blob. The second clause matters: `cand-I2` shows an entry
  already foreign *at prepare time* must be preserved, not clobbered (and
  the plan-time `:staged-elsewhere` hold is the primary defense there).
  Foreign other-path staged state is untouched throughout (`cand-I3`).
- **Crash cutpoints (cand-crash).** C1 after-prepare: no commit, HEAD
  unchanged, resolution `ambiguous-fail-closed`, claim never reauthorized.
  C2 after-CAS: recovery completes outcome+refresh from the trailer with no
  recommit, status clean. C3 after-CAS + ref rewritten to mint: trailer
  gone, no durable outcome ⇒ **ambiguous, fail closed, operator resolution —
  never reauthorized**. C3b: same rewrite but outcome already durable ⇒
  `consumed-by-durable-outcome-despite-rewrite`. C4 after-outcome:
  refresh-only recovery.
- **HEAD/ref vs index atomicity, stated precisely.** The ref moves by CAS
  (atomic). The index refresh is a *separate, later, cosmetic* transaction:
  it never affects the commit's content (repro-D2 + cand-I4 show the commit
  stands whatever happens to the index), only whether `git status` is clean
  afterwards. A durable prepare+outcome journal — not reachable history —
  is the consumption witness; the trailer is the recovery accelerator, and
  its loss (rewrites) degrades to fail-closed, never to reauthorization.
- **A prepared claim can never reauthorize** (`promotable()` in the harness
  excludes any claim with a prepare record; there is no abort path in S0 —
  operator release is the modeled resolution).

## 3. Hook safety and object identity (task 3)

- **hook/mutating-hook-detected:** a hook that injects foreign content into
  the *private* index is caught by re-running `write-tree` after the hook
  and comparing to the frozen tree ⇒ held. Frozen-tree revalidation must
  happen **after** hook execution, not before.
- **hook/refusing-hook-holds:** explicit pre-commit refusal propagates as a
  held promotion.
- **filter/stateful-filter-diverges (defect):** a deterministic stateful
  clean filter (invocation-count-dependent output) makes two
  `hash-object --stdin --path` calls on identical bytes return **different
  OIDs**. Consequence: any precomputed expected OID (including a mint-time
  claim OID) can be invalidated by filter state; the only stageable identity
  is the OID returned by the actual `hash-object -w` invocation
  (`filter/stage-returned-oid` stages exactly that). Designs must treat
  OID-mismatch as *suspicion requiring re-verification against raw bytes
  and filter config*, not as automatic proof of foreign editing.

## 4. Failed hypotheses inside S0 (recorded, not smoothed over)

1. Holding `index.lock` manually and running `git update-index` inside it
   fails — the lock excludes *all* writers including the holder (run 1
   traceback). Hence the copy-then-CAS-rename shape.
2. Refresh guard `cur == E0` alone is unsafe: E0 captured after foreign
   staging makes the clobber look legitimate (cand-I2 first draft). The
   mint-HEAD-blob clause is required.
3. (Harness-only) JSON round-trip turned entry tuples into lists, breaking
   equality — fixed by normalization; mentioned because the same
   serialization trap awaits the EDN journal records.

## 5. Capture prerequisites still unresolved (task 4; no CLI probes run)

- CLI hooks synchronize with *that CLI's* tool execution only; they say
  nothing about non-CLI writers between pre and post. Delta verification
  detects inconsistency but cannot attribute it; false-negative holds are
  the cost. **No blanket atomicity or any-interleaving-detection claim is
  made.**
- Observed pre/post **mode delta is not tool-authorized by capture alone**:
  B's `chmod` between pre and post would be recorded as part of "the"
  transaction. Minimum additional evidence: capture must record *which*
  tool surfaces can change mode (Write/Edit cannot) and hold on any
  observed mode delta.
- A normalized (filtered-OID) clean baseline can hide raw dirt: CRLF
  worktree vs LF blob compare equal under `--path`. Baseline trust needs a
  raw-byte rule (e.g. `git diff --no-textconv HEAD -- path` empty, or
  recorded raw SHA equality to a witnessed clean state), specified per
  filter configuration.
- Symlink components can change after realpath checking (TOCTOU on the
  path itself); the conservative rule (hold on any symlink component) makes
  the residual race benign only because a swapped-in symlink changes what
  bytes the post-read sees, tripping delta verification — unless the swap
  restores identical bytes, which is the same accepted benign case as
  before, now stated explicitly.
- Remaining questions for a later capture spike: exact Pre/PostToolUse
  payload fields per CLI version; whether hook stdout/stderr can carry the
  capture or files are required; MultiEdit partial-failure bytes; codex-CLI
  hook support. None investigated here (no billable probes).

## 6. Wiring (task 5)

[wiring-d2.edn](wiring-d2.edn) retained as **proposed** declarations; S0
changes no box and claims no production conformance. Post-S0 the map needs
two new boxes (`prepare-journal`, `index-cas-refresh`) and one renamed wire
(`:git/trailer-consumption` → durable-outcome consumption) — deferred to the
DERIVE revision that consumes this evidence, not edited mid-review.

## 7. Go/no-go by invariant (evidence basis for the owner, not a verdict)

| Invariant | Status after S0 | Evidence |
|---|---|---|
| I1 plan⇒promotable | go (predicate shape unchanged) | cand promotable() exclusion |
| I2 staged==authorized | go | repro-D2, filter/stage-returned-oid |
| I3 commit==authorized delta, CAS, no foreign writes | **revised:** executor may write the shared index only via the post-commit index CAS (never porcelain reset/add) | repro-A/D1 (defects), cand-I1..I4, cand-lock |
| I4 committed⇒consumption durable | **revised:** trailer insufficient alone; durable prepare+outcome journal required; trailer-loss ⇒ fail-closed | repro-B (defect), cand-crash C2/C3/C3b/C4 |
| I5 consumed⇒never promotable | go, under journal (not history) | C3, C3b |
| I6 mint⇒verified capture | no-go pending capture spike (§5 prerequisites) | not tested here by design |
| I7 trusted baseline | partial: raw-vs-filtered baseline rule needed (§5) | Exp C + §5 |
| I8 links via committed objects | go (attributable? on receipt+trailer unchanged) | repro-CE2 |

**Smallest next structural decision for DERIVE:** adopt the durable
prepare/outcome journal as the consumption witness (making the witness
store — not Git history — the authority for claim lifetime) together with
the post-commit index CAS as the *only* permitted shared-index write. If
the witness store cannot carry prepare/outcome records without a schema
change the owner judges too large, that is the structural blocker to name —
S0 shows no purely-Git mechanism is monotone under ref rewrites.

## 8. Validation of this packet

`python3 -m py_compile s0-git-transaction.py` OK; full run exit 0
(6/6 + 26/26); transcript committed unedited. No Clojure/Lisp/EDN authored
or modified; wiring EDN untouched. Markdown links checked against sibling
artifacts. No production code, runtime config, live CLI settings, or shared
state touched; all temporary repos removed.
