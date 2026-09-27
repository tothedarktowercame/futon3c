# D2 — DERIVE (revised): enforceable claim and promotion contract

**Supersedes:** [D1-derive.md](D1-derive.md) (commit b9e6fb65), which owner
review did **not** accept. D1 remains in place for history; this document is
the current DERIVE candidate. Every D1 section is either confirmed, revised
(marked **R#** per the owner's six blocking findings), or withdrawn.
**Mission:** [M-inbox-zero-claim-lifecycle](../../missions/M-inbox-zero-claim-lifecycle.md). C1–C7 unchanged. No-workaround rule in force.
**Packet:** D2, DERIVE only. Author kimi-9; owner/reviewer codex-5.
**New evidence this round** (bounded `timeout`, `mktemp`+`trap rm -rf`,
no production state touched): Exp A — private-index commit with foreign
same-path staged bytes mid-window; Exp B — commit-trailer consumption scan;
Exp C — `hash-object --path` CRLF/filter behavior, hooks/signing detection,
symlink mode. Transcripts summarized inline. Live-repo scan: futon3c has an
active `.git/hooks/pre-commit` (five invariant scripts), `commit.gpgsign`
unset, `core.hooksPath` unset; futon3 and inbox-zero-lib have no active hooks.

## Revision log

- **R1 (finding 1, CE2 resurrection):** already-landed withdrawn as a
  consumption witness. Consumption is now claim-targeted and commit-atomic:
  the promoted commit carries an `Inbox-Zero-Claim: <claim-id>` trailer, and a
  claim is consumed iff (a) a release record naming *its own claim-id* exists
  in snapshot ∪ witness queue, or (b) a commit reachable from HEAD carries its
  trailer. Exp B verified the trailer scan (`git log
  --format='%(trailers:key=Inbox-Zero-Claim,valueonly)' <mint-head>..HEAD`
  returned the id). The trailer is written in the same `commit-tree` operation
  as the content, so consumption is durable **before** any receipt-publish
  crash window. Release records gain a required `:supersedes {:claim/id …}`
  field; end-of-authority is per-claim-id (monotone: records immutable, ids
  unique), never latest-per-tuple timestamp — a stale release for an old
  claim-id cannot suppress a newer same-tuple claim. Receipt records keep the
  rich fields for read-back and links; `:orphan-commit` (trailer present,
  receipt absent) is consumed-and-surfaced, never re-authorized.
  Owner's repro under D2: A commits H (trailer t, crash before receipt); B
  commits X on new HEAD; B leaves H dirty ⇒ trailer scan finds t ⇒ claim
  consumed ⇒ held `:consumed`. **R1 spike check S2 re-runs exactly this.**
- **R2 (finding 2, index transaction):** the step-5 resync and the U1
  fallback are **withdrawn**. Structural ownership change: the promotion
  executor **never writes the shared index** — no `add`, no `reset`, no
  resync, at any point, including failure paths. Exp A: with B's same-path
  staged bytes present from mid-window, the private-index commit landed
  exactly the authorized blob (`HEAD:f.txt` == authorized OID), B's staged
  entry survived untouched in `git ls-files -s`, worktree intact, and
  `git status` honestly reports `MM` — B's staged work remains visible as
  B's dirt, never clobbered, never committed. The entry empty-index check is
  retained as a conservative availability tripwire only; correctness does not
  depend on it, so no later race reopens what it checks. Immediately before
  the ref CAS the executor records the shared staged set for included paths
  into the receipt (diagnostic `:foreign-staged-observed`); it never refuses
  or touches it. A promoted path under a foreign staged entry therefore shows
  as that foreign owner's dirt afterwards — correct attribution, documented.
- **R3 (finding 3, I8 contradiction):** eligibility is split into two
  explicitly separate predicates. `promotable?` — prospective permission
  (§5); consumed/already-landed correctly make a claim non-promotable after
  its commit. `attributable?` — historical receipt attribution for link
  derivation, operating on the *receipt/trailer and the committed objects*,
  not on current dirt: a new link cites `(commit/sha, claim-id via trailer,
  receipt :paths {path → [mode oid]})` with `:basis :commit-receipt`. This
  requires adding `:commit-receipt` to the schema's link-basis vocabulary
  (`state.clj:167-169` currently forces `:path-claim-intersection`) — a named
  schema change, versioned; the old basis stays for historical links. I8 is
  rewritten (§7). Positive C4+C5 trace in §9.
- **R4 (finding 4, capture ordering):** the D1 claim that stream observation
  orders our pre-read before the CLI write is **withdrawn** — the stream is
  buffered and no such guarantee exists. Structural change, named not worked
  around: authority-bearing capture moves to CLI-owned **PreToolUse /
  PostToolUse hooks** (matcher `Edit|Write|MultiEdit`), which the CLI runs
  synchronously before/after tool execution — that is the only synchronization
  point that owns the write. The hook process captures: raw-byte SHA-256,
  filtered OID (`git hash-object --path`), mode (lstat exec bit), HEAD, the
  tool input transcript, and realpath status, for pre and post respectively.
  The dev.clj stream correlation still joins seat/session/tool_use_id; mint
  happens only when hook capture + successful result + delta verification all
  agree. **If hooks are not configured for a CLI (codex CLI support unknown —
  U2), that CLI's edits mint nothing** — a documented availability limitation,
  not a silent weakening; C4's positive case is specified against the hook
  surface. Hook ordering is a spike-verifiable assumption (S5 probe: hook
  writes a marker, tool modifies file, order asserted from the hook side).
  Exact tool semantics specified (§4): Edit unique-`old_string` substitution,
  `replace_all`, MultiEdit ordered application and its all-or-nothing failure
  surface, Write create vs full-replace. Symlink/path-alias: realpath each
  path component; any symlink in the repo-relative path or the target ⇒
  `:unsupported-path-alias`. Filter caveat: baseline identity compares the
  **filtered OID** to `HEAD:<path>`; delta verification compares **raw
  bytes** (tool input lives in worktree-raw space); both hashes are recorded
  so neither is asked to do the other's job. Mode is captured pre and post;
  the claim authorizes the mode delta; eligibility requires current mode ==
  authorized post mode.
- **R5 (finding 5, execution capture + porcelain parity):** execution reads
  the worktree bytes **once** into memory; OID is computed
  (`hash-object --stdin --path=<rel>`) and compared; only on match is the
  same in-memory byte string written (`-w --stdin --path=<rel>`); no second
  disk read exists to race. HEAD is read **once** at execution start and that
  single value feeds `read-tree`, `commit-tree -p`, and the CAS old-value.
  Attribute/config drift between mint and execution surfaces as OID mismatch
  ⇒ `:content-changed` (fail closed; Exp C confirmed `--path` applies the
  clean filter, CRLF worktree → LF blob). **Porcelain-parity inventory
  (fidelity matrix F9–F11):** `commit-tree` bypasses hooks, signing, and
  `git commit` config behavior. Live scan: futon3c's pre-commit hook is five
  invariant scripts — bypassing them would violate the no-workaround rule.
  D2 therefore runs the resolved pre-commit hook **explicitly** with
  `GIT_INDEX_FILE` exported (same refusal semantics, sees the private staged
  state), before `commit-tree`. Executor startup refuses
  `:unsupported-repo-config` when `commit.gpgsign` is true, `core.hooksPath`
  is set, or any active hook other than pre-commit exists (commit-msg,
  post-commit, pre-push…) — priced adaptation, never silent bypass.
- **R6 (finding 6, wiring checker):** the D1 "no grounded wiring format"
  claim was false — surveyed argument maps instead of
  `futon3c.diagramprover.wiring`. Corrected: companion
  [wiring-d2.edn](wiring-d2.edn) authored in the checker's declared
  box/field format and **run** (read-only): ingest OK, `multiply-written` [],
  boundary fields identified, conformance reports 21
  `:declaration-without-occurrence` findings, all naming unimplemented
  packets — reported in the file trailer, not hidden. The checker's actual
  limitation, stated precisely: conformance is a textual occurrence check
  (comments counted, `:unclassified` possible), so it keeps declarations
  honest but does not prove runtime behavior; the VERIFY BOM must list it as
  an existing executable structural check with that level.

## 1. Design in one paragraph (revised)

A claim is authority for exactly one CLI-synchronized tool transaction:
PreToolUse/PostToolUse hooks capture raw and filtered content identity, mode,
HEAD, and the tool transcript around the write; a claim is minted only when
the post-image equals the witnessed tool input applied to the pre-image and
the pre-image has a trusted baseline. `promotable?` (prospective) and
`attributable?` (historical) are separate predicates. The executor captures
bytes once, stages them in a private temporary index it alone owns, runs the
repo's pre-commit invariants explicitly, commits via `commit-tree`, and moves
HEAD by compare-and-swap; it never writes the shared index. The commit
message carries the claim-id trailer, making consumption durable at commit
time; a release record with `:supersedes` provides the settled form, and the
witness queue is consulted for exclusion so intake lag cannot resurrect
authority.

## 2. Consumption, receipts, replay (R1 detail)

- **Consumed(claim)** ⇔ release r with `r.:supersedes.:claim/id == claim.id`
  in (snapshot ∪ witness queue) ∨ ∃ commit reachable from HEAD with trailer
  `Inbox-Zero-Claim == claim.id` (scanned over `(:head/sha at mint)..HEAD`).
  The two halves are independent: trailer covers crash-before-publish;
  release/queue covers operator releases and the settled record.
- **Monotone, claim-targeted:** authority ends only by a record or commit
  naming *that* claim-id. A same-tuple newer claim is unaffected by an older
  claim's late release; tuple-latest projections remain display views only.
- **Receipts:** released-successor carries `:receipt {:commit/sha :head/before
  :tree/oid :paths {p → [mode oid]} :foreign-staged-observed :executed-at}`.
  Read-back reconciles trailer-present/receipt-absent as `:orphan-commit`
  (consumed, surfaced). Repeated turn-end delivery re-plans and finds
  `:consumed`; identical content on a changed HEAD (owner's CE2 repro) is
  held by the trailer half regardless of already-landed. Already-landed
  (worktree OID == `HEAD:<path>`) is retained as an exclusion *signal* but is
  no longer load-bearing for consumption.
- **Publication vs settled read:** unchanged from D1 §4 — queue effective for
  exclusion, projection required for inclusion. U4 from D1 is resolved by the
  trailer check, which needs no queue scan to survive crashes; the queue scan
  remains for operator releases.

## 3. Capture surface (R4 detail)

- **Hooks (new structural surface):** PreToolUse writes
  `capture/<session>/<tool_use_id>-pre.edn` {:raw/sha256 :git/oid :mode
  :head/sha :realpath/status :present?}; PostToolUse writes `-post.edn` plus
  the tool input transcript. Hook configuration is a CLI settings change —
  operator-owned, named here as a required structural change, not assumed.
- **Mint conditions (all required):** hook pre+post present; successful
  tool_result correlated; delta verification (raw space); baseline trusted;
  realpath clean; mode delta recorded. Any failure ⇒ no claim; typed reason
  (`:transaction-contended`, `:mixed-baseline`, `:unsupported-path-alias`,
  `:unsupported-mode`, `:capture-unavailable`).
- **Stream-only fallback:** none for authority. If hooks are absent the edit
  is unsupported for promotion (documented; operator release path unchanged).

## 4. Exact tool semantics (R4 detail)

- **Write:** post raw bytes must equal `input.content` bytes. Baseline
  `:create` requires path absent at pre-capture (absent from worktree) and
  untracked in HEAD; overwrite of an existing file requires `:tracked-clean`
  or `:chained` baseline. Write on a path whose pre-existence is unwitnessed
  ⇒ `:mixed-baseline`.
- **Edit:** `replace_all=false` ⇒ exactly one occurrence of `old_string` in
  pre, post == single substitution; `replace_all=true` ⇒ post == substitution
  of all occurrences; zero occurrences or ambiguous input ⇒ the tool itself
  errored, no successful result, no mint path reached.
- **MultiEdit:** ordered application of the edits array to the pre-image; the
  CLI applies them as one tool call — PostToolUse sees the final file;
  intermediate states are not witnessed and are not claimed. Partial-failure
  semantics of the CLI (whether any edit applies when a later one fails) is
  U2 spike evidence; until confirmed, any non-success result mints nothing.
- **Symlinks/aliases:** realpath every component of the repo-relative path;
  symlink anywhere ⇒ `:unsupported-path-alias` (mode 120000 observed in Exp C
  confirms Git records such paths as links, never blobs).

## 5. Eligibility (R3 detail) — two predicates

`promotable?(claim, ctx)` = active ∧ ¬cleaned-after? ∧ `:authorization`
present ∧ ¬Consumed (§2) ∧ current filtered OID == `:post/blob` ∧ current
mode == `:post/mode` ∧ status ∈ {:modified, :untracked-create} ∧ trusted
baseline chain intact ∧ ¬already-landed(signal) ∧ realpath clean.
Consumed by `plan-promotion` and `project-dirty-sets` only.
`attributable?(commit, claim, ctx)` = commit trailer names claim.id ∧
receipt `:paths` match the commit's actual tree diff for those paths (mode +
OID). Consumed by link derivation only. Typed reasons as D1 plus
`:consumed`, `:capture-unavailable`, `:unsupported-path-alias`,
`:unsupported-repo-config`.

## 6. Commit transaction (R2/R5 detail)

1. Capture HEAD once (`H0`). Entry: shared index empty (tripwire T3 only).
2. Gates run against the worktree (unchanged).
3. Per included path: read bytes once; OID check vs `:post/blob`; `-w` write;
   private `GIT_INDEX_FILE`: `read-tree H0`, `update-index --cacheinfo
   <post/mode>,<oid>,<path>`.
4. Verify private staged set == authorized set exactly.
5. Run resolved pre-commit hook with `GIT_INDEX_FILE` exported; refusal ⇒
   held `:gate-failed` (hook is a gate with invariant semantics).
6. `commit-tree <tree> -p H0 -m <message + trailer>`; `update-ref HEAD <new>
   H0`; CAS failure ⇒ held `:head-moved`, no retry.
7. Publish release+receipt (`:supersedes` claim-id). Crash before ⇒
   trailer still consumes; `:orphan-commit` surfaced at read-back.
8. Push (existing policy, reads the new commit — wiring `:push` box).
The shared index and the worktree are never written by the executor.

## 7. Invariants (revised)

- **I1** plan `:include` ⇒ `promotable?` held at planning.
- **I2** private staged OID/mode set == authorized set, verified pre-commit.
- **I3** committed tree diff vs `H0` == authorized delta set; ref moves only
  by CAS on `H0`; the executor never writes the shared index or worktree
  (enforceable: executor's Git commands enumerated; tripwire T7/T13).
- **I4** `:committed` ⇒ commit carries the claim-id trailer (atomic) ∧
  release+receipt published or `:orphan-commit` recorded.
- **I5** Consumed claim ⇒ never `promotable?` (trailer half survives receipt
  loss, intake lag, and HEAD movement).
- **I6** mint ⇒ hook-captured pre/post, delta verified, baseline trusted,
  realpath clean, mode delta recorded.
- **I7** baseline chain: `:tracked-clean` | `:chained` (same seat, same
  session, unconsumed predecessor) | `:create`.
- **I8 (rewritten)** new session-commit links derive only via
  `attributable?`: trailer + receipt + actual committed tree diff; a link is
  never derived from current dirt and never presented as independent
  authorship proof.

## 8. Fidelity matrix (revised; tripwires T1–T14)

| Capability | Verdict | Tripwire |
|---|---|---|
| Gates before staging, ordered, refusal held | preserve | T1 failing gate ⇒ no ref move, no index/worktree write |
| Explicit path scope | preserve | T2 extra dirt not in plan stays uncommitted |
| Empty-index entry check | preserve (availability tripwire only) | T3 occupied ⇒ `:index-not-empty` |
| Push policy | preserve | T4 committed pushes; held does not |
| Debounced once-per-turn launch; held visible | preserve | T5 ≤1 execution per turn |
| Status staleness; refresh drops clean only | preserve | T6 cleaned path dropped, never content-refreshed |
| Executor internals | adapt: private index + commit-tree + CAS; **no shared-index writes** | T7 owner repro: foreign same-path staged bytes mid-window ⇒ staged entry intact, commit contains only authorized blobs, status MM |
| Commit semantics vs porcelain | adapt: pre-commit hook run explicitly; signing/other hooks ⇒ `:unsupported-repo-config` | T8 pre-commit invariant runs and its refusal holds (real futon3c hook scripts in a scratch clone) |
| Claim record shape | adapt: optional `:authorization`, release `:supersedes` + `:receipt` | T9 v0 snapshot loads; stale release ≠ suppress newer claim |
| Plan-include entries | adapt: frozen `:authorization` | T10 round-trip |
| Link basis | adapt: add `:commit-receipt` (schema version bump) | T11 links cite trailer+receipt; never consumed-claim dirt |
| Minting | adapt: hook-captured, delta-verified only | T12 owner CE1 repro + injected mid-transaction write ⇒ no claim |
| Content re-read at execution | drop (single capture) | T13 byte-flip between check and stage impossible by construction (code review + fault-injection test) |
| Already-landed as consumption witness | drop (demoted to signal) | T14 owner CE2 repro ⇒ `:consumed` via trailer |

## 9. Positive C4+C5 trace (R3)

A's turn: Edit₁ on f (pre==HEAD blob ✓) ⇒ claim c₁(post=b₁). Edit₂ on f
(pre==b₁ == c₁.post, chained ✓) ⇒ claim c₂(post=b₂). Turn end ⇒ plan
includes f with c₂'s frozen authorization (c₁'s post ≠ current ⇒ superseded
display, but note: c₁ remains *promotable-false* via chain rule: baseline of
c₂ consumes c₁'s content; eligibility marks c₁ `:superseded-by-chain` — typed,
not silent). Gates pass; executor commits tree with f=b₂, trailer
`Inbox-Zero-Claim: c₂`. Receipt+release for c₂ published. Link derivation:
commit trailer names c₂; receipt `:paths` f→[100644 b₂] == actual tree diff ⇒
link (commit,c₂,:commit-receipt). Dirty-set: f clean, no dirt. All three
views agree; c₁ was never separately promotable, no double-attribution.

## 10. Sources to change (packets, unchanged from D1 except as noted)

P1 eligibility (`promotable?`/`attributable?`, trailer scan seam, targeted
release lookup) — futon3, new `eligibility.clj`, `projection.clj`,
`promotion.clj`, `state.clj` (link-basis vocabulary, `:supersedes` docs).
P2 capture+mint — CLI hook scripts + settings (operator-owned), dev.clj
correlation, `witness.clj` mint conditions. P3 executor — `promote_exec.clj`
per §6, hook invocation, trailer message support in `turn_promotion.clj`
commit-message fn. P4 links/read-back/release helper. P5 docs (C7).

## 11. VERIFY spike (smallest evidence set)

S1 CE1 (both variants, hook capture); S2 owner CE2 repro exactly as ran
against D1, asserting `:consumed` via trailer with receipt deliberately
unpublished; S3 plumbing incl. explicit pre-commit hook run against a
scratch clone carrying futon3c's real hook scripts; S4 C4 chain trace (§9);
S5 hook-ordering probe (PreToolUse marker vs tool write); S6 MultiEdit
partial-failure semantics (U2); S7 symlink/CRLF/mode/deletion matrix.
Deliverables: transcripts + go/no-go per I1–I8.

## 12. Accepted facts / decisions / unresolved

**Accepted facts:** owner repros 1–2 stand (D1 mechanisms failed them);
futon3c pre-commit hook is live and invariant-bearing; `hash-object --path`
applies clean filters (Exp C); private-index commit leaves foreign staged
entries intact (Exp A); trailer scans work (Exp B); wiring checker exists
and runs on this design (R6).
**Decisions:** R1–R6 as logged; D1's already-landed consumption, resync
step, U1 fallback, and stream-ordering claim withdrawn.
**Unresolved:** U1(retired). U2 — MultiEdit partial-failure byte semantics;
codex-CLI hook support (if absent: that CLI is promotion-unsupported,
documented). U3 — turn-crossing chains (unchanged). U4(retired by trailer).
U5 — watcher intake lag repair remains out of scope (safety no longer
depends on it). U6 — PreToolUse/PostToolUse synchronous-execution guarantee
is CLI-vendor behavior; S5 must confirm, and if it fails the capture surface
has no fallback — DERIVE would return here.

**DERIVE exit: not claimed.** Owner reviews.
