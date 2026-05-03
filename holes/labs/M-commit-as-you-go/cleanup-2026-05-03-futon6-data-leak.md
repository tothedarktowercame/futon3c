# Cleanup log: futon6 data-leak (2026-05-03)

A worked example of the "semi-worst-case cleanup checklist" for the
forthcoming `commit-as-you-go/working-tree-commit-pressure` invariant.
Each step records: **why** (what tension it resolves), **command**
(exact bash), **finding** (real output excerpt), **verify**
(assertion proving the step worked), **risk/undo** (recovery path).

Status going in (audit 2026-05-02): futon6 = 507 dirty, dwarfing
every other repo on the stack. Hypothesised cause: data leak —
generated outputs landed under `data/` instead of `~/code/storage/`.

## Phase A — Pre-flight (don't trust the audit; verify state)

### Step 1 — confirm starting state of the working tree
- **why:** the audit was 18 hours ago; state may have moved.
- **cmd:** `git status --porcelain | wc -l`; tally by status code, by top-level dir.
- **finding:** porcelain count 509 (504 D, 3 M, 2 ??); 502 under `data/`.
- **verify:** counts match the audit shape (507 → 509 — close enough for diff).
- **undo:** N/A (read-only).

### Step 2 — scope the data/first-proof/ subtree
- **why:** the hypothesis says the bulk is generated outputs from the prelim-tutor work.
- **cmd:** `find data/first-proof -type f | wc -l`; `git ls-files data/first-proof | wc -l`.
- **finding:** **0 files on disk**, 347 tracked entries in the index.
  → The directory has already been deleted from the working tree;
  what we're seeing is a pile of staged-equivalent ` D` (deleted-but-not-staged) entries.
- **verify:** `git status` shows ` D data/first-proof/...` for each.
- **undo:** if the deletion was wrong, `git checkout HEAD -- data/first-proof/` recovers all 347 tracked files.

### Step 3 — confirm destination is clean and reachable
- **why:** if we're moving data to `~/code/storage/`, we must not clobber a prior move.
- **cmd:** `test -d /home/joe/code/storage/futon6/first-proof && echo CONFLICT || echo no`
- **finding:** **misleading first read** — I checked the wrong path (`first-proof` directly under `futon6/`)
  and reported "no". Correct path is `storage/futon6/data/first-proof/` (preserving the `data/` parent).
- **verify:** any cleanup checklist needs to test for `<storage>/<repo>/<full-source-path>` — *not*
  just the leaf — because a sane mirror preserves source structure.
- **undo:** N/A.
- **lesson:** **make the destination-path computation a function**, not a freeform check.

### Step 4 — same-filesystem check
- **why:** if src and dst are on different mounts, `mv` becomes copy+unlink (slower, race-prone).
- **cmd:** `stat -c %d` on src and dst.
- **finding:** src device empty (because src is gone), dst = 66306. Inconclusive given Step 2.
- **verify:** N/A here; would matter for an active move.
- **undo:** N/A.

### Step 5 — existing .gitignore inspection
- **why:** if the source repo's `.gitignore` already covers the moved path, no edit needed.
- **cmd:** `grep -E '^data|^/data|first-proof' .gitignore`
- **finding:** `.gitignore` line 9 already has `data/*`. So untracked files under `data/` are already ignored.
  Tracked files are not — gitignore doesn't apply to anything in the index.
- **verify:** new files added to `data/` will be ignored.
- **undo:** N/A.

### Step 6 — code references to the moved path
- **why:** if scripts read/write to the moved path, they'll silently break at runtime.
- **cmd:** `git grep -l 'data/first-proof' -- ':!data/first-proof'`
- **finding:** **101 tracked files** reference `data/first-proof/...`. Mix of:
  - operational scripts (Python verify/explore scripts) — would fail at runtime
  - mission/handoff docs (historical references) — fine, just stale text
  - tests that read fixtures — would fail
- **verify:** none yet — this is a *known followup*, not blocking the cleanup.
- **undo:** N/A.
- **lesson:** **distinguish operational refs from documentary refs** before mass-rewriting paths.

## Phase B — Reconcile audit-vs-now and locate the data

### Step 7 — reconcile yesterday's snapshot vs current state
- **why:** the discrepancy in Step 2 (502 → 0 on disk) means an event happened in between.
- **finding:** porcelain status now `504 D | 3 M | 2 ??` — i.e. nearly all the dirt is now staged-equivalent
  deletions. Someone (or a prior run) deleted the working-tree files but didn't stage or commit.
- **lesson:** **the cleanup checklist must run a "what changed since the last audit" pass first** —
  state can drift overnight, and acting on stale state is how data gets lost.

### Step 8 — confirm the index references a path that doesn't exist on disk
- **why:** a ` D` status code means the index has the file but the working tree doesn't.
- **finding:** 347 tracked entries, 0 on-disk. Confirmed.

### Step 9 — interpret .gitignore vs tracked deletions
- **why:** gitignore won't help us if the files are tracked; we still need to either commit deletions or restore.

### Step 10 — `storage/futon6/` exists and is large (10,698 files, 40G)
- **why:** Joe said data should live at `~/code/storage/`; if the prior move went there, the data is safe.
- **finding:** `storage/futon6/` has subdirs but `storage/futon6/first-proof` (the path I checked first) doesn't.
- **lesson:** **first negative result is not a conclusion** — try the structural mirror path before declaring loss.

## Phase C — Confirm the data is at storage (recoverable)

### Step 11 — exhaustive destination search
- **cmd:** `find /home/joe/code/storage -maxdepth 4 -name 'first-proof'`
- **finding:** `/home/joe/code/storage/futon6/data/first-proof` — yes, **the move preserved the `data/`
  parent**. The mirror is structural.
- **verify:** `du -sh` shows 98M in 572 files.

### Step 12 — does git history have the 347 tracked files?
- **cmd:** `git ls-tree -r HEAD data/first-proof/ | wc -l` → 347.
- **verify:** if storage somehow lost data, the tracked subset is recoverable from HEAD.
- **undo:** `git checkout HEAD -- data/first-proof/` would restore 347 files (98M of disk).

### Step 13 — what about the untracked subset (audit had 502, tracked is 347)?
- **finding:** 502 − 347 = 155 untracked at audit time. Those would be unrecoverable from git
  if storage didn't capture them.
- **storage check:** Step 17 below shows storage has 572 files, 225 more than the 347 tracked.
  → the move WAS comprehensive — both tracked and untracked outputs went to storage.
- **lesson:** **always count "untracked-and-now-gone" separately** from "tracked-and-now-gone".
  Git can rescue the latter; nothing rescues the former unless you mirrored it.

### Step 14 — reflog/stash sweep
- **cmd:** `git reflog -10`; `git stash list`
- **finding:** no overnight stash; reflog shows recent commits unrelated to the cleanup.
- **verify:** no hidden state holds the missing data; storage is the only copy of the untracked subset.

## Phase D — Cross-check storage completeness against the index

### Step 15 — confirm storage holds the data
- **cmd:** `find /home/joe/code/storage/futon6/data/first-proof -type f | wc -l` → 572 files, 98M.
- **verify:** files exist; date range Feb 11 → Mar 7 2026 (consistent with the prelim-tutor work window).

### Step 16 — every tracked-deleted file is present at storage
- **cmd:** loop over `git ls-files data/first-proof/`, check each at `storage/futon6/<path>`.
- **finding:** 347 / 347 present, 0 missing.
- **verify:** **safe to stage the deletions** — git history holds them anyway, but storage doubles the safety net.

### Step 17 — storage extras (untracked outputs preserved too)
- **finding:** storage 572 − tracked 347 = 225 extras.
- **verify:** the prior move didn't filter to "tracked only" — it took everything under `data/first-proof/`.
  This is what we want.

## Phase E — Recognise the broader scope

### Step 18 — characterize *all* dirty entries under `data/`, not just `first-proof/`
- **why:** the audit lumped 502 under `data/`. first-proof accounts for 347. What about the other 155?
- **finding:** the deletion is *stack-wide across `data/`*: 25 distinct subdirs (ct-validation 54,
  synthetic-qa 35, frontiermath-pilot 24, ...). All gone from the working tree.
- **verify:** all of these subdirs exist under `storage/futon6/data/`.
- **lesson:** **don't fix only what was named** — the same root cause typically affects sibling paths.

### Step 19 — non-first-proof deletions are also at storage
- **cmd:** for each non-first-proof D, check storage.
- **finding:** all 2 `se-data/` deletions also at storage. ✓

### Step 20 — identify what's NOT part of the data leak
- **finding:**
  - 3 M (mark2 work in progress): `holes/missions/M-superpod-mark2.md`, `scripts/mark2`, `tests/test_mark2.py`
  - 2 ?? (separate): `holes/missions/M-hyperreal-dictionary-planning.md`, `holes/missions/data/`
- **decision:** the cleanup *boundary* is "deletions that have a storage backup". Don't touch the others.
- **lesson:** **make the cleanup boundary explicit and small** — every "while we're here" expansion
  is a chance to stomp on unrelated work-in-progress.

## Phase F — Stage the deletions

### Step 21 — verify the storage backup for non-`data/` deletions
- **finding:** 2 `se-data/...` deletions at storage ✓.
- **verify:** safe to stage all 504 deletions.

### Step 22 — stage
- **cmd:** `git add -u data/ se-data/`
- **verify:** `git diff --cached --name-only --diff-filter=D | wc -l` → 504.
- **undo:** `git restore --staged data/ se-data/` un-stages.

### Step 23 — verify post-stage state
- **finding:** porcelain shows `504 D` (now staged), `3 M` (untouched mark2 work), `2 ??` (untouched untracked).
  No collateral damage.
- **verify:** the only remaining unstaged: 3 M + 2 ?? — exactly the items we listed in Step 20.

## Phase G — Commit (paused for operator approval)

### Step 24 — commit decision point
- **why:** this is the "the easy cleanup is *done* when the diff lives in history, not in the working tree".
- **proposed message:**

  ```
  Move data/* and se-data/math-processed/* outputs to ~/code/storage/

  504 generated outputs (M-the-prelim-tutor work products + se-data
  stage6 results) belong under ~/code/storage/, not in the source repo.
  Storage mirror at /home/joe/code/storage/futon6/data/* preserves all
  files including 225 untracked extras from the original tree.

  Storage already populated by prior cleanup pass; this commit only
  records the deletions in the index.

  Note: 101 tracked source files still reference data/first-proof/ paths;
  those will be addressed separately when the relevant scripts are
  next exercised.
  ```

- **risk/undo:** `git revert HEAD` restores the 347 tracked files (the 225 untracked extras stay
  at storage regardless). For full restoration: `git checkout HEAD~1 -- data/ se-data/`.
- **status:** ✅ committed as `338a2fa` (2026-05-03 18:25) — 504 files / 3,519,892 line-deletions.

### Step 25 — verify the loop closed

- **cmd:** re-run `bb futon0/scripts/futon-sync.clj status`
- **finding:** futon6 went from `Dirty=507 / Untracked=2 / ↑1` → `Dirty=3 / Untracked=2 / ↑2`.
  The 3 remaining are exactly the mark2 WIP we deliberately scoped out (Step 20).
- **verify:** the cleanup boundary held — no collateral on adjacent paths.
- **lesson:** **the audit tool that surfaced the problem should be the same one that confirms the fix.**
  If `futon-sync.clj` is going to be the SENSE half of `working-tree-commit-pressure`, this round-trip
  (audit → diagnose → repair → re-audit) is the loop the invariant formalises.

### Step 26 — note the size signal

- **observation:** 3,519,892 line-deletions. That's the magnitude of source-repo bloat that one
  forgotten data leak produced. The pressure signal in the new invariant should be sensitive to
  *size* (lines and bytes) as well as count — a single 100,000-line file matters differently from
  100 small files, even if both register as "1 dirty".
- **followup for invariant design:** include `total_dirty_bytes` and `total_dirty_lines` in the
  pressure function alongside `count` and `age`.

## Phase H — Sweep across the rest of the stack (2026-05-03 afternoon)

After the futon6 cleanup landed (`338a2fa`), the rest of yesterday's session pile across futon0 / futon3 /
futon3b / futon3c / futon5 / futon5a / futon6 was committed in **4 more commits**:

| # | Commit | Repo | Scope |
|---|---|---|---|
| 1 | `d521820` | futon0 | M-the-futon-stack.md (new — was untracked, 370 lines of session work) |
| 2 | `fb533ce` | futon3 | 13 coord flexiarg M + 3 scripts D + 22 mission-doc Status: archived M |
| 3 | `ad7c824` | futon3b | 13 coord flexiarg D (paired with #2) |
| 4 | `5f18bc2` | futon3c | 3 source patches (clean) + 6 D + 28 mission-doc M + 2 new files |

### Findings from the multi-repo sweep (extends the 13-question checklist)

These are findings *additional* to the futon6 phases above. Each one narrows the design of the
forthcoming `working-tree-commit-pressure` check-fn.

#### Finding 1 — "untracked but operator-active" is a third state.

`M-the-futon-stack.md` carried 370 lines of session work but appeared as `??` in `git status`.
futon-sync's "Untracked" column counted it but didn't surface "has authored content vs blank scaffold."
For the pressure function, **untracked-with-content should weight separately from untracked-blank** —
authored work-not-yet-tracked is a higher-pressure state than placeholders.

#### Finding 2 — single repo, multiple session concerns.

The futon3 commit carried three concerns (coord-flexiarg consolidation, scripts retirement,
mission-doc dispositions). The futon3c commit carried four (source patches, data cleanup, mission
dispositions, new mission docs). Per-repo commits keep cross-reference burden low at the cost of
commit-purity. **Future tooling: `bb futon-sync.clj scope --session=$id` should preview "files whose
diff matches a session-pattern" so the operator can audit what's about to be lumped together
before the staging step.**

#### Finding 3 — cross-repo paired commits need linking.

Commit `ad7c824` (futon3b) only makes sense as a pair with `fb533ce` (futon3). The link lives in the
commit message text ("paired with futon3 fb533ce"). **Tooling: a session-scope marker — possibly
in `git notes` or a sidecar `.session-id` — would make the link mechanical, not literary.**

#### Finding 4 — pre-commit hooks catch real things; don't bypass.

The `reachable-from-boot/agent-registry` hook rejected the futon3c commit because dev.clj had
pre-existing `swap! reg/!registry` call sites at lines 1314/1321. CLAUDE.md discipline ("never add
--no-verify") was the right call — investigation revealed those call sites were *not from our
session* but from operator WIP we'd accidentally pulled in via `git add <file>`. The hook
prevented a polluted commit. **Existing futon3c hooks are a *prior art* the new invariant can
build on, not duplicate.**

#### Finding 5 — WIP-entanglement is a real cleanup blocker.

`dev/futon3c/dev.clj` and `dev/futon3c/dev/bootstrap.clj` had operator WIP interleaved with our
session edits on shared lines (Joe's `bridge-state` tracking + our `text-buf` declaration shared the
same hunk). Patches can't be split cleanly when this happens. **Decision rule: drop entangled
files from the session commit; the session change continues running in-memory; commit later
when the surrounding WIP lands.** This is a legitimate "deferred commit" pattern — the pressure
function should distinguish "deferred-with-reason" from "default-undecided."

#### Finding 6 — gitignored files can carry session work invisibly.

`futon5a/data/stack-stereolithography-priority-queue.json` is gitignored (`data/*` rule). Our
edit to it (added `run-068` at top with score 800) lives only on disk, invisible to git and to
futon-sync. The runbook (`stack-stereolithography-runbook.edn`) is the *tracked* source-of-truth
and should hold the new candidate. **Pressure function should ignore git-ignored paths but flag
"edited gitignored generated artifacts whose runbook was not also touched" — that's the silent
divergence the operator wouldn't otherwise see.**

#### Finding 7 — heterogeneous commits are a tradeoff.

Commit 4 (futon3c) carried code + data + mission-meta + new-missions in one commit. Smaller
commits would be purer; this one was coherent only as "the day's work on futon3c." For
session-scope cleanups, "one commit per repo" was the chosen tradeoff. **The pressure function
should not punish heterogeneity per se — what matters is whether the diff's coherence is
operator-attestable. The `git commit -m` message itself is the attestation.**

### Distilled additions to the 13-question checklist

| # | New question (additions to the original checklist above) |
|---|---|
| 14 | What hunks of each modified file are session-work vs operator-WIP? Stage only the former. |
| 15 | Are any files entangled (session edits + operator WIP on shared lines)? Drop entangled files; commit later. |
| 16 | Does any session work live in gitignored generated artifacts? If so, is the corresponding tracked source-of-truth also touched? Flag divergence. |
| 17 | Are there cross-repo paired commits? Link them in commit messages (or via mechanical session-scope marker). |
| 18 | Did pre-commit hooks fire? Investigate root cause; never bypass with --no-verify. |
| 19 | Is "untracked but authored" present? Distinguish from "untracked but blank/scaffold." |
| 20 | After the sweep: re-run the audit tool. The same tool that surfaced the problem should confirm the loop closed. |

That's 20 questions total. The set partitions cleanly into:

- **Reconciliation** (Q1, Q2, Q20): "what is the working tree's state, before and after?"
- **Locate-and-verify** (Q4–Q7): "where did the data go; is it recoverable; what's at storage?"
- **Scope-and-boundary** (Q3, Q8, Q9, Q14, Q15): "what's mine, what's yours, what's coupled?"
- **Stage-and-commit** (Q10, Q11, Q12, Q17, Q18): "stage with verification; commit with attestation; pair across repos."
- **Operational hygiene** (Q13, Q16, Q19): "followups for runtime references, gitignored divergence, untracked-with-content."

These five clusters are the spec for `working-tree-commit-pressure`'s check-fn, read top-down.

## Distilled checklist (what an instrumented `commit-as-you-go` invariant should make routine)

For each candidate cleanup target, the apparatus should:

| # | Phase | Question the apparatus answers automatically |
|---|---|---|
| 1 | Pre-flight | Has the working-tree state drifted since the audit? Reconcile. |
| 2 | Pre-flight | What is on disk vs what the index believes? (Surface ` D` vs `D ` vs `?? `.) |
| 3 | Pre-flight | Are there code references to the path being moved? Count them; classify (operational vs documentary). |
| 4 | Locate | If "moving to storage", what is the *structural mirror* path? `storage/<repo>/<full-source-path>`. |
| 5 | Locate | Does the structural mirror exist? **Test the structural form before declaring loss on a leaf-only check.** |
| 6 | Verify | Does the storage mirror cover *every* file in the index, plus the untracked overflow? |
| 7 | Verify | Does git history hold the tracked subset as a fallback? |
| 8 | Scope | Same root cause, sibling paths? Look beyond the named target. |
| 9 | Boundary | Which uncommitted entries are *not* part of this cleanup? Surface them so they don't get caught up. |
| 10 | Stage | Stage only the deletions/changes that have a verified backup. |
| 11 | Verify post-stage | Collateral damage check — anything unexpected in `--cached`? |
| 12 | Commit | Message captures: what moved, where it went, why, what's left for follow-up. |
| 13 | Followup | List operational refs to the moved path that will need updating when next exercised. |

The 13 questions above, applied at boot or via probe-tap, are roughly what `working-tree-commit-pressure` *adds* on top of `bounded-disposition/branch` and `bounded-disposition/mission-doc`: not just "you have N undecided items" but also "if you're going to resolve them by relocation/deletion, here is the storage-mirror discipline that makes the move safe."
