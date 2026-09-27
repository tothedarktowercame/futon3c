# D1 — DERIVE: smallest enforceable claim and promotion contract

**Mission:** [M-inbox-zero-claim-lifecycle](../../missions/M-inbox-zero-claim-lifecycle.md) (through 89059895; MAP closed 18:38Z)
**Packet:** D1, DERIVE only. Author: kimi-9 (helper). Reviewer/owner: codex-5.
**Inputs:** discovery f21f838f + [TN](../../technotes/TN-inbox-zero-claims-d-2026-09-27.md);
MAP review checkpoints 18:31Z/18:35Z/18:38Z with the four accepted counterexamples
(CE1 mixed preexisting dirt + tool-result/hash-read race; CE2 identical content on
changed HEAD; CE3 OID-vs-raw-hash/filter/symlink/mode/deletion/shared-index;
CE4 identity inventory); PSR psr-7ba18a76-b443-4ba5-9b4e-acdb499eb4a5
(`inbox-zero/promote-at-turn-end`, repaired not replaced).
**C1–C7 unchanged.** No production code, runtime, or storage changed by this packet.

No companion wiring `.edn` is authored: surveyed formats (futon5a `.aif.edn`
argument maps; `futon3c.aif.invariant` mission-head checkers) are argument-graph
formalisms with no dataflow wiring checker. Inventing a new unchecked EDN format
would be decoration, so the wiring diagram below is a table plus ASCII, and the
VERIFY BOM must record this as a proposed (not existing-executable) formalism.

## 1. Design in one paragraph

A claim becomes **authority for exactly one witnessed tool transaction**: at
mint it binds the Git blob OID of the file before the edit, the blob OID after,
the file mode, HEAD, and the tool_use id — and it is only minted when the
witnessed post-content equals the witnessed tool input applied to the witnessed
pre-content, so bytes not explained by the tool transaction fail closed at the
source. Eligibility is one pure predicate shared by promotion, dirty-set
projection, and new link derivation; it requires a trusted baseline, exact
content/mode match, not-already-landed content, and no consumption record in
the snapshot *or* the pending witness queue. The executor commits through a
private temporary index and a compare-and-swap ref update, so shared-index
writers and gate-time races cannot smuggle content into the commit; on success
it publishes a released-successor claim carrying receipt fields through the
existing single-writer intake, making every claim one-shot.

## 2. Edit authority (CE1) — the delta-verified transaction witness

Existing hooks observe the tool stream; they cannot wrap the filesystem write
atomically, and this design does not pretend otherwise. What the stream *does*
provide is a happens-before pair: the assistant `tool_use` block is observed
before the CLI executes the tool, and the `tool_result` is observed after
(`futon3c/dev/futon3c/dev.clj:1100-1140` already correlates the two by
`[agent-id session-id tool_use_id]`). The mechanism:

1. **Pre-capture (tool_use event).** Read the target file's bytes (or record
   absence) and its mode. Keep in memory under the existing pending map. Compute
   the pre-image blob OID the way Git will: `git hash-object --path=<rel>`
   semantics so clean filters/CRLF apply exactly as at staging time.
2. **Post-capture (successful tool_result).** Read bytes and mode again;
   compute post-image OID identically.
3. **Delta verification.** Check the post-content equals the tool input applied
   to the pre-content:
   - `Write`: post bytes == `input.content` (Write fully determines content;
     it is self-authorizing for content).
   - `Edit`: pre bytes with exactly one `old_string` → `new_string`
     substitution == post bytes.
   - `MultiEdit`: sequential application of the ordered edits == post bytes.
   - Verification fails ⇒ **mint no claim**; record a held outcome with typed
     reason `:transaction-contended` (another writer touched the file inside
     the tool transaction, including between tool completion and our post-read;
     the inconsistency itself is the detector — atomicity is not required,
     because any interleaving that changes bytes breaks the equation, and an
     interleaving that leaves bytes identical left nothing to exclude).
4. **Baseline rule (mixed-dirt exclusion).** Mint only when the pre-image is
   *trusted*:
   - (a) `:tracked-clean` — path in HEAD and pre OID == `HEAD:<path>` blob; or
   - (b) `:chained` — pre OID == post OID of an earlier unconsumed claim for the
     same seat+path in the same session (covers Edit 1..3 of one turn); or
   - (c) `:create` — Write on a path absent from HEAD **and** absent from the
     worktree at pre-capture (a pre-existing untracked file means someone
     else's unwitnessed bytes would be overwritten silently — held instead).
   Otherwise no claim; typed reason `:mixed-baseline`.
5. **Mode rule.** Record the Git mode (100644/100755 from the exec bit).
   Symlinks (would-be 120000) and gitlinks are held `:unsupported-mode`; Edit
   tools cannot delete, so `:deleted` dirt on a claimed path is held
   `:content-changed` — deletions are never auto-promoted under this contract.

**Structural change required (named, not worked around):** the pre-capture does
not exist today. `remember-inbox-zero-tool-details!` (dev.clj:1120-1123) must
read file bytes at tool_use time, and `publish-successful-edit!`
(`futon3c/src/futon3c/inbox_zero/witness.clj:127-150`) must gain the delta
verification and baseline check. This is feasible inside the existing surfaces
with no CLI changes. The honest limit: pre/post reads are not atomic with the
CLI's write, and delta verification — not locking — is what makes that safe;
verification failure costs a false-negative hold (safe) and cannot produce a
false-positive claim for unwitnessed bytes.

**Unsupported tools (shell/sed/python):** mint nothing. Their byte changes
break OID equality at eligibility, converting a stale-claim commit into a hold
(the incident mechanism). No inference from filename, age, or recent claim.

## 3. Identity (CE4)

- **Claim identity:** unchanged (`claim:` sha256 of [seat, tool_use_id,
  worktree, path], witness.clj:143-146) — already unique per tool transaction.
- **Turn identity:** the local `invoke-trace-id` (dev.clj:3615, warm :3887) is
  in scope at both hooks and is recorded on the claim's `:authorization` map as
  a **diagnostic only** (threading it is a two-call-site change). The Agency
  job-id does not reach `invoke-once [prompt session-id]` and is **not**
  adopted: no invariant below needs cross-process turn identity, because
  authority is per-tool-transaction and consumption is per-claim. IF/HOWEVER/
  THEN/BECAUSE #4 below. The invoke-trace-id's replay stability is not assumed;
  it must never gate eligibility.

## 4. Consumption, receipts, replay (CE2)

- **Consumption record:** a released-successor claim for the exact
  [worktree,path,seat] tuple — the protocol already validated in packet R —
  extended with optional `:receipt` keys: `:commit/sha`, `:head/before`,
  `:tree/oid`, `:paths` {path → [mode blob-oid]}, `:executed-at`. The claim
  schema tolerates additional keys (state.clj:112-120) and `:released` is an
  existing state, so **no schema version change and no new record type**.
- **Publication vs settled read:** `watcher/write-witness!` (watcher.clj:245)
  publication (validate + atomic rename) is the durable consumption point.
  Because watcher intake lags (C6, observed 20+ min), eligibility treats the
  union of *snapshot records* and *pending witness-queue records* as effective
  for **exclusion** (fail closed on lag), while **inclusion** requires the
  settled projection. Reading the witness directory is a new cross-component
  read coupling — flagged as U4, bounded to "does a release/receipt naming this
  claim-id exist".
- **Replay/resurrection rules:**
  - *One-shot:* any release/receipt for the claim-id (snapshot ∪ queue) ⇒
    never eligible again.
  - *Already-landed:* eligible requires worktree OID ≠ `HEAD:<path>` blob.
    After A's commit lands, HEAD contains the authorized content; identical
    bytes reintroduced as dirt (CE2) match the claim's post OID **and** HEAD,
    so the claim is inert even if its release was lost to a crash. Byte
    equality is never authority; it only ever excludes.
  - *Crash windows:* commit succeeded but release publish crashed ⇒
    `:orphan-commit` — already-landed keeps the claim inert; read-back surfaces
    the missing receipt as an operator-visible discrepancy, never a silent
    re-promotion. Crash between staging (private index) and ref update ⇒ no
    ref moved, no shared-index mutation; next turn re-plans.
  - *Repeated turn-end delivery:* debounce is unchanged; a redelivered launch
    re-plans from state+queue and finds the claim consumed or already-landed.

## 5. Commit transaction (CE3) — committed objects == authorized objects

The shared `.git/index` is never used for the promoted commit. Per planned
repo/worktree, after the existing gates pass (unchanged, run against the
worktree):

1. `GIT_INDEX_FILE=<tmp>`; `git read-tree HEAD`.
2. For each included path, hash worktree bytes with
   `git hash-object --path=<rel>` and compare to the claim's authorized
   `:post/blob`; mismatch ⇒ held `:content-changed` (never refresh expected
   content to whatever is on disk — the existing refresh seam may drop
   newly-clean paths only). On match, `git hash-object -w` writes the blob and
   `git update-index --add --cacheinfo <mode>,<oid>,<path>` stages it.
3. Verify the private staged set equals the authorized set exactly:
   `git diff --cached --name-status -z` (against the private index) must show
   precisely the included paths with `M`/`A` and no others.
4. `git commit-tree <tree> -p HEAD -m <message>` then
   `git update-ref HEAD <new> <old>` compare-and-swap. CAS failure (another
   writer committed meanwhile) ⇒ held `:head-moved`, no retry; the next turn
   re-plans.
5. On success: resync the shared index for the promoted paths only
   (`git reset -q HEAD -- <paths>`), publish the release+receipt (§4), then
   the existing push step (`promote-push/push-promoted!`, unchanged policy).

Why this closes the window: gates→stage→verify→commit never touch the shared
index, so another writer's `git add`/`reset` cannot enter the commit; another
writer's *commit* is caught by the ref CAS; filters are applied by
`hash-object --path` identically at mint and execution, so the compared OIDs
are the same function of the same bytes. **VERIFY spike must confirm** the
plumbing sequence (especially step-5 resync semantics and `hash-object --path`
filter behavior under `.gitattributes`) — recorded as U1/U2, not asserted.

**Preserved:** empty-shared-index refusal at entry (conservative, tripwire
T6), status-class check, gate ordering and failure semantics, explicit path
scope, commit message construction, push policy, held-plan visibility,
debounce/idle gating.

## 6. Shared eligibility and views (C5)

One new pure predicate, `futon3.inbox-zero.eligibility/eligible-claim?`,
consumed by `promotion/plan-promotion`, `projection/project-dirty-sets`, and
new session-commit link derivation:

```
eligible(claim, ctx) =
  claim.state == :active
  ∧ ¬cleaned-after?(observations, claim)            ; existing rule, now shared
  ∧ claim.authorization present                     ; else :legacy-unqualified
  ∧ ¬consumed?(claim-id, snapshot ∪ witness-queue)  ; else :consumed
  ∧ worktree OID(path) == authorization.post/blob   ; else :content-changed
  ∧ mode(path) == authorization.mode                ; else :content-changed
  ∧ status(path) ∈ {:modified, :untracked-create}   ; else :unsupported-status
  ∧ worktree OID ≠ HEAD:path                        ; else :already-landed
  ∧ baseline chain intact (a|b|c of §2)             ; else :mixed-baseline
```

Exclusion reasons are typed and surface in held plans and dirty-set projection
identically. New links derive only from eligible claims and keep
`:basis :path-claim-intersection`; links remain derived views, never
independent authorship proof (C5). Historical claims (655 current, all without
`:authorization`) and historical links are untouched evidence: legacy claims
evaluate `:legacy-unqualified` — they block nothing and authorize nothing.

## 7. Entities, relations, state transitions

**Entities.** `session-file-claim` (extended; identity unchanged) with optional
`:authorization {:pre/blob :post/blob :pre/status :mode :head/sha :tool-use/id
:invoke-trace/id :minted-at}` and, on the released successor, `:receipt
{:commit/sha :head/before :tree/oid :paths :executed-at}`. All other record
types unchanged. Owners: futon3c witness mints `:authorization`; the executor
mints `:receipt`; the watcher owns snapshot writes; projections own nothing.

**Relations.** claim —released-by→ released-successor (same tuple);
claim —authorizes→ plan-include entry (carries a frozen copy of
`:authorization`); commit-observation —receipt→ claim (via `:receipt`);
session-commit-link —basis→ eligible claim only.

**Transitions.** minted-active → eligible → planned → staged-verified →
committed+released (terminal). Any check failure → held with typed reason
(non-terminal; a later state change may re-plan, except `:consumed`,
`:already-landed`, `:legacy-unqualified`, which are terminal for that claim).

## 8. Checkable invariants

- **I1** plan `:include` ⇒ `eligible-claim?` held at planning for that claim.
- **I2** staged OID/mode set == authorized set exactly, verified before
  `commit-tree`; mismatch aborts before any ref move.
- **I3** committed tree diff vs old HEAD == authorized delta set; enforced by
  private index + `update-ref` CAS on the planning HEAD.
- **I4** `:committed` result ⇒ release+receipt published before the result is
  returned; otherwise the outcome is `:orphan-commit`, recorded and surfaced.
- **I5** consumed claim (release in snapshot ∪ queue) ⇒ never eligible.
- **I6** worktree OID == `HEAD:<path>` ⇒ not eligible (already-landed).
- **I7** mint ⇒ delta verification passed and baseline ∈ {tracked-clean,
  chained, create}.
- **I8** new session-commit links cite only claims eligible at derivation time.

## 9. Wiring (producer → consumer, field ownership)

```
tool stream ──► dev.clj hooks ──► witness.clj (mint claim+authorization)
                                     │  atomic rename, validate
                                     ▼
                              witness dir (durable queue)
                                     │  watcher.clj write-witness!/run-cycle!
                                     ▼
                              state snapshot (single writer)
                                     │  projection.clj
                                     ▼
              eligibility.clj ◄── witness-queue scan (exclusion only)
                                     │
        ┌──────────────┬────────────┼───────────────┐
        ▼              ▼            ▼               ▼
  plan-promotion  dirty-sets   link derivation   (read-back / ops)
        ▼
  turn_promotion.clj (screen, route, gates)
        ▼
  promote_exec.clj (private index, OID verify, commit-tree, ref CAS,
                    release+receipt publish ──► back to witness dir,
                    index resync, push)
```

Cross-process timing: witness publication is immediate and durable; snapshot
ingestion is eventual; eligibility's queue scan makes consumption effective at
publication time, so intake lag (C6) degrades inclusion, never exclusion.

## 10. IF/HOWEVER/THEN/BECAUSE

1. IF authority must cover exactly the edit delta, HOWEVER hooks cannot make
   pre/post reads atomic with the tool's write, THEN verify that post ==
   input(pre) and mint nothing on mismatch, BECAUSE any byte-changing
   interleaving breaks the equation, and a byte-identical interleaving leaves
   nothing unattributed to exclude.
2. IF a historical clean observation exists for X, HOWEVER B may have dirtied
   X→X+B before A's edit (CE1), THEN require the *pre-image* to be trusted
   (HEAD blob, same-seat chain, or create), BECAUSE eligibility on post-content
   alone cannot distinguish A(X+B) from A(X)+B.
3. IF consumption must survive intake lag and crashes, HOWEVER new record types
   force a schema bump, THEN carry receipt fields on the existing
   released-successor claim and treat the witness queue as effective for
   exclusion, BECAUSE the schema already tolerates optional keys and
   publication is already the durable atomic point.
4. IF turn identity is diagnostically useful, HOWEVER the Agency job-id does
   not reach `invoke-once` and no invariant needs it, THEN record the local
   invoke-trace-id on claims without gating on it and do not plumb the job-id,
   BECAUSE authority is per-tool-transaction and adding a cross-layer signature
   change buys no safety.
5. IF `git add`/`git commit` on the shared index is the existing executor,
   HOWEVER any writer can mutate that index between staging and commit (CE3),
   THEN stage in a private index and move the ref by compare-and-swap,
   BECAUSE correctness must not depend on other processes' discipline.
6. IF `git hash-object` without `--path` ignores clean filters, HOWEVER staged
   blobs are computed with filters, THEN compute all compared OIDs with
   `--path=<rel>`, BECAUSE the compared values must be the same function of the
   same bytes at mint and at execution.
7. IF legacy claims carry no authorization, HOWEVER 655 exist today, THEN fail
   them closed with `:legacy-unqualified` and offer operator release via the
   packet-R helper, BECAUSE silent qualification would fabricate evidence.

## 11. Fidelity contract (preserve/adapt/drop) with tripwires

| Capability | Verdict | Tripwire test |
|---|---|---|
| Gates run before any staging, ordered, failure ⇒ held | preserve | T1: failing gate ⇒ no ref move, no index mutation |
| Explicit path scope; only planned paths committed | preserve | T2: plan with extra on-disk dirt commits exactly included paths |
| Empty-shared-index entry refusal | preserve (conservative) | T3: occupied index ⇒ `:index-not-empty` |
| Push policy via `promote-push/push-promoted!` | preserve | T4: committed plan pushes; held plan does not |
| Debounced once-per-turn launch; held plans visible | preserve | T5: repeated launches in one turn ⇒ ≤1 execution |
| Status-class staleness check; refresh drops clean only | preserve | T6: path cleaned between plan and exec ⇒ dropped, not committed |
| Executor internals | **adapt**: porcelain add/commit → private index + commit-tree + ref CAS | T7: concurrent `git add` on shared index during execution ⇒ commit contains only authorized blobs; concurrent commit ⇒ `:head-moved` |
| Claim record shape | **adapt**: optional `:authorization`/`:receipt` keys | T8: v0 snapshot without these keys loads and validates |
| Plan-include entries | **adapt**: carry frozen `:authorization` | T9: plan serialization round-trip |
| Link derivation basis | **adapt**: eligible-claims only | T10: link never cites a `:legacy-unqualified` or consumed claim |
| Minting of claims for any successful edit | **adapt**: mint only on delta-verified, baseline-trusted transactions | T11: contended transaction ⇒ no claim, typed reason recorded |
| Refresh re-reading expected content | **drop** (was never present; explicitly forbidden) | T12: content change post-plan ⇒ `:content-changed`, never re-authorized |

## 12. Exact source changes (implementation packets, not done here)

| Packet | Files / functions | Change |
|---|---|---|
| P1 eligibility (futon3, pure) | new `inbox-zero-lib/src/futon3/inbox_zero/eligibility.clj`; `projection.clj` (`project-dirty-sets`, link derivation); `promotion.clj/plan-promotion` | single predicate; typed reasons; witness-queue scan seam (injectable for tests) |
| P2 witness (futon3c) | `dev/futon3c/dev.clj` `remember-inbox-zero-tool-details!` (pre-capture), `record-inbox-zero-tool-results!` (post-capture); `inbox_zero/witness.clj/publish-successful-edit!` (delta verify, baseline, OID via git, `:authorization`, invoke-trace-id threading) | mint only verified claims; held reasons to followup path |
| P3 executor (futon3) | `promote_exec.clj/execute-plan!` (private index, OID verify, commit-tree, ref CAS, resync, release+receipt publish, `:orphan-commit`); keep `execute-plan-with-refresh!` drop-clean-only | committed == authorized; one-shot consumption |
| P4 views/ops (futon3) | link derivation basis; small release helper CLI over `write-witness!` for `:legacy-unqualified` operator releases | C5 coherence; operating interface |
| P5 docs | inbox-zero operating docs + docbook links (C7) | holds, release, read-back, unsupported surfaces |

Each packet lands with clj-kondo, check-parens, and single-namespace tests;
real-Git tests tagged `^:slow` per futon3c AGENTS.md. Independent review per
packet (coding-handoff protocol).

## 13. C1–C7 and CE1–CE4 → behavior and real-dependency test

| Criterion | Behavior under D1 | Test (real temp Git repo unless noted) |
|---|---|---|
| C1 | B's shell edit breaks OID match / baseline ⇒ held, HEAD+bytes untouched | A-claim; `sed` edit; A turn-end; assert no commit, typed `:content-changed`/`:mixed-baseline` |
| C2 | Private index + OID verify + ref CAS; deletions/modes held | B edits during gate stub; deletion dirt on claimed path ⇒ held; mode flip ⇒ held; concurrent shared-index writer ⇒ commit contains only authorized blobs |
| C3 | One-shot release+receipt; already-landed; legacy fail-closed | promote, re-dirty identical bytes, re-run ⇒ no commit; release in queue-not-snapshot ⇒ still held; claim without `:authorization` ⇒ `:legacy-unqualified` |
| C4 | Own verified edit promotes behind gates | Write+Edit sequence, chain baseline, assert commit attribution/message/push stub |
| C5 | One predicate for promotion/dirty-sets/links | pure test: same state ⇒ same eligible set in all three views |
| C6 | Queue-aware read-back; intake lag documented | read-back test against witness dir + projection (ops packet, live C6 read-back remains codex-5's) |
| C7 | Packets reviewed; docs in P5 | review records; doc links checked |
| CE1 | delta verification + trusted pre-image (§2) | clean X; B shell-edit; A Edit other part ⇒ no claim (`:mixed-baseline` at mint or `:content-changed` at plan); injected B-write between post-capture ⇒ `:transaction-contended` |
| CE2 | already-landed + one-shot (§4) | commit H; B restore+reintroduce H on new HEAD before intake ⇒ held |
| CE3 | OID-native compare, `--path` filters, mode/symlink/deletion rules, private index + CAS (§5) | `.gitattributes` CRLF file round-trips; symlink path held `:unsupported-mode`; staged-set equality under concurrent index writer |
| CE4 | invoke-trace-id diagnostic only; no job-id plumbing (§3) | claim carries trace id; eligibility identical with it absent |

## 14. Smallest VERIFY spike

One spike, real temporary Git repos reusing
`futon3/test/futon3/inbox_zero/promote_exec_test.clj:12-18`, before P1–P5:
(S1) CE1 both variants; (S2) CE2; (S3) plumbing spike resolving U1 —
`read-tree`/`hash-object --path`/`update-index --cacheinfo`/`commit-tree`/
`update-ref` CAS/`reset -- paths` sequence, including CRLF `.gitattributes`,
exec-bit mode, symlink, deletion, and a concurrent shared-index writer;
(S4) C4 positive chain. Deliverables: test evidence transcripts, the U1
plumbing decision, U2 MultiEdit/encoding semantics confirmed against real CLI
tool inputs, and a go/no-go per invariant I1–I8. Spike only; no production
change.

## 15. Unresolved (recorded, not sketched over)

- **U1:** final commit mechanics — `commit-tree` + `update-ref` CAS + path
  resync vs `GIT_INDEX_FILE` + `git commit`. The CAS design is specified above;
  the spike must confirm resync semantics don't clobber another writer's
  legitimately staged (different-path) entries. If it cannot, the fallback is
  keeping the empty-index refusal as a hard precondition *and* the private
  index — still correct, narrower.
- **U2:** exact byte semantics of Edit/MultiEdit inputs (newline normalization,
  encoding, partial-MultiEdit failure states) as emitted by the CLI stream;
  delta verification must match real transcripts, not assumed shapes.
- **U3:** whether `:chained` baselines may cross turn boundaries within one
  session (spec says same session, unconsumed; turn-crossing chains are held
  until evidence shows the narrower rule hurts C4).
- **U4:** the eligibility scan of the witness queue is new read coupling to the
  intake directory; if review rejects it, the fallback is executor-time
  consumption check against live Git history (`HEAD:<path>` blob equality plus
  log scan), which is weaker under crash windows — trade-off to ARGUE.
- **U5:** C6 live read-back and watcher slow-intake diagnosis remain with
  codex-5; this design makes safety independent of intake lag but does not
  repair the lag itself.

**DERIVE exit: not claimed.** Owner reviews; ARGUE/VERIFY follow per lifecycle.
