# INBOX-ZERO-CLAIMS-D: stale claims and turn-end promotion

Discovery and one release request, codex-5 for claude-8, 2026-09-27.
Request: invoke-1790532726042-25531-c7ad0fb3. No promotion implementation changes.

## Finding

Confirmed: promotion treats an old active path claim as authority for the
current dirty file, without identifying the session responsible for its bytes.
Three of the six commits examined captured claude-8's shell edits for claude-10.
Two match claude-10's own editing commands; one remains undetermined.
The Git author/committer on all six is Joseph Corneli; the agent attribution
is in the commit message and inbox-zero links, not Git's author identity.

There is already a clean-after-claim exclusion in the dirty-set projection,
including a comment describing this exact old-claim/other-agent-shell-edit case
from August 26. Promotion and commit-link derivation do not use that exclusion.

## Mechanism and source sites

All paths below are relative to /home/joe/code; line numbers describe the
canonical source checked during discovery, not a claim of live code reloading.

- `futon3c/src/futon3c/inbox_zero/witness.clj:20,110-148` recognizes only
  Edit/Write/MultiEdit tool details naming a file. A successful edit publishes
  an immutable seat and `:active` claim, keyed by seat/tool/worktree/path.
  The claim has first/last observation timestamps but no content hash or turn ID.
  `futon3c/dev/futon3c/dev.clj:1120-1146` hooks successful tool results to this
  publisher. Bash/Python/sed edits do not create these witnesses.
- `futon3/inbox-zero-lib/src/futon3/inbox_zero/confirm.clj:65-103` is the other
  minting path: an exact-seat attribution confirmation creates an active claim.
- `futon3/inbox-zero-lib/src/futon3/inbox_zero/watcher.clj:245-270` exposes
  `write-witness!`, the validated atomic immutable seat/claim intake interface.
  `run-cycle!` at 295-344 ingests witnesses before deriving links and observations.
- `futon3/inbox-zero-lib/src/futon3/inbox_zero/state.clj:16-18,54,175-200,342-361`
  defines one snapshot writer, active/superseded/released states, immutable IDs,
  referential validation, and serialized append. Rewriting an existing claim ID
  is an error; releases are new records, not mutations of historical records.
- `futon3/inbox-zero-lib/src/futon3/inbox_zero/projection.clj:21-43` selects the
  latest claim for [worktree,path,seat] by last-observed-at, breaking ties by ID.
  A later released/superseded claim for the tuple ends earlier active authority.
  No TTL, session-close, turn-close, or successful-commit release producer was
  found in the inbox-zero source. All 1,483 stored claim records in the initial
  snapshot have state active, though only 655 are current.
- `projection.clj:126-156` discounts claims after a non-dirty observation in
  `project-dirty-sets`. This does not actually release the stored claim.
- `futon3/inbox-zero-lib/src/futon3/inbox_zero/promotion.clj:18-51` separately
  joins latest dirty observations to current active claims by worktree/path.
  Exactly one claim belonging to the ending seat includes the path; zero,
  multiple, or other-seat claims exclude it. No age, clean-cycle, hash, or
  current-edit provenance check is made.
- `futon3c/src/futon3c/inbox_zero/turn_promotion.clj:300-331,389-428` constructs
  the exact ending seat, plans, screens, executes, and pushes committed plans.
  The launcher debounces and waits for that agent to stop invoking, not other
  editors. `futon3c/dev/futon3c/dev.clj:3801,4030` invokes the turn-end launcher.
- `futon3/inbox-zero-lib/src/futon3/inbox_zero/promote_exec.clj:84-118,131-160`
  checks the empty index, Git status classes, gates, and staged path set, then
  commits. Refresh drops clean paths and refreshes status classes. These checks
  do not verify whose content is staged. No claim is closed on success.
- `projection.clj:67-108` derives session-commit links using the same active
  path-claim intersection. These are not independent authorship evidence.

## Complete stale-claim inventories

The 192,114,187-byte snapshot was streamed through a PushbackReader one record
at a time, never opened in an editor or slurped. The scan retained only seats,
claims, commit observations and links for analysis. Snapshot mtime was
2026-09-27T18:02:26.074732640Z. Scan reference time:
2026-09-27T18:13:56.387Z; strict older-than-12-hours cutoff:
2026-09-27T06:13:56.387Z. `created_at` means `first-observed-at`; no separate
created-at field exists.

- [Current active claims older than 12h](inbox-zero-claims-d-current-active-2026-09-27.tsv):
  **652 rows**, every claim ID, exact seat, repo, worktree, path, creation and
  last-observed time. This is the relevant set for promotion.
- [All historically active records older than 12h](inbox-zero-claims-d-history-active-2026-09-27.tsv):
  **1,480 rows**, including older records displaced by later claims for the same
  tuple; the `current` column distinguishes those. Included to make “every
  ACTIVE claim” unambiguous. There are 655 current claims total, all active.

Record counts: 270,179 file observations; 2,851 session-commit links;
10,725 commit observations; 1,483 claims; 3,927 cursors; 38 seats.

## Six commits and attribution

| Commit | Actual changed file/content | Finding |
|---|---|---|
| 58c3dd7c, Sep 27 17:35:57Z | Ledger test counts 59/21/35/79 → 62/21/38/73 | **Other seat: claude-8.** Exact Python replacement in session 9824b707-fdaf-40c6-886e-9ce6eed1cee9 at 17:35:31.085Z. |
| 685d211d, Sep 27 17:50:00Z | Ledger test counts 62/21/38/73 → 63/23/41/67 | **Other seat: claude-8.** Exact Python replacement in the same session at 17:47:54.174Z. |
| e347e724, Sep 27 15:31:36Z | Ledger test counts 43/20/10/107 → 48/20/13/99 | **Other seat: claude-8.** Exact Python replacement in the same session at 15:31:20.219Z. Message lists two paths; actual diff changes only the test. |
| 1d2a9f94, Sep 26 15:29:55Z | wm-flight-wiring.edn R13 horizon declarations and standing findings, +173/-3 | **Claiming seat's work: claude-10**, supported by that session's R13 edit commands at 15:18:42, 15:23, 15:27:42 and 15:29:15. Message lists two paths; actual diff changes only the map. |
| 4427b740, Sep 26 15:10:52Z | wm-flight-wiring.edn futon2 pin 5d8105f2 → 6165ec8b and F1a observation declaration, +5/-2 | **Undetermined.** Link says claude-10, but no independent matching edit was located. Subject matter or the link alone cannot establish the editor. |
| 08c4bb43, Sep 25 19:53:09Z | wm-flight-wiring.edn W_c call/test/trace wiring and futon2 pin, +13/-7 | **Claiming seat's work: claude-10.** Matching commands in its session at 19:50:41.571Z, 19:51:51.575Z and 19:53:04.000Z. |

Claiming seat throughout: `seat:claude-10:158bb5ed-3985-4627-878d-c8babeaeb9d8`.
Five commits have complete links to that seat in the initial snapshot;
685d211d has no commit observation/link in that initial snapshot. A second
streamed scan at 18:19:43.150Z finds its complete claude-10 link too, using
the same stale claim. Test-file links use
claim:8c7eb3879d22ea52d75ef793b7316dc1d08cc70eb9ae5cee2f5ac8bb0307dc84.
1d2a9f94 and 4427b740 use map claim
claim:9f2da2d253a1af644d61dc2971f0b4cfb0630a84f1ce3e313f21d46eb56692d1;
08c4bb43 uses claim:d31856c720b5fc123cce766ac44bdefb65887aa21c0fac230b24610854edfd61.

Local independently inspected edit evidence:

- `/home/joe/.claude/projects/-home-joe-code-voxterm/9824b707-fdaf-40c6-886e-9ce6eed1cee9.jsonl:2157,2268`
  (the two reported replacements).
- That session's pre-compaction history contains the 15:31:20 replacement;
  extracted source filename/line and command are in
  `/tmp/inbox-zero-claims-d/historical-edits.json`.
- `/home/joe/.claude/projects/-home-joe-code-voxterm/158bb5ed-3985-4627-878d-c8babeaeb9d8.jsonl.pre-compact-1790368651:928,931,935`
  contains the W_c edits.
- That session's `.jsonl.pre-compact-1790437858` contains the R13 edits
  (including lines 1764,1773,1780).
- Exact snapshot commit/link records: `/tmp/inbox-zero-claims-d/commit-links.edn`.
  Temporary extracts are supporting local evidence, not required to interpret
  the committed inventories and conclusions.

## Narrow repair: published, awaiting intake

Before: claim
`claim:8c7eb3879d22ea52d75ef793b7316dc1d08cc70eb9ae5cee2f5ac8bb0307dc84`,
seat above, path `test/futon3c/diagramprover/wm_wire_ledger_test.clj`,
worktree `worktree:83d88d109136f3c8`, repo `futon3c-d`, state active,
first/last observed **2026-09-26T11:42:30.777Z**,
tool witness `toolu_019pLGZzcS4MmojnJFTCj8jN`.

At **2026-09-27T18:16:17.037Z**, called existing
`futon3.inbox-zero.watcher/write-witness!` from a bounded standalone bb process
with one new released successor record for the exact tuple:
`claim:release-inbox-zero-claims-d-8c7eb3879d22-20260927`.
It preserves first-observed-at and records the request ID, original claim ID,
requester claude-8, operator codex-5 and release reason. The existing schema
accepts `:released`; current-claims documents this successor protocol. There
is no dedicated release CLI; the validated claim-record intake is the existing
interface used here. No direct snapshot append or manual edit was performed.

Published record:
`/home/joe/code/storage/inbox-zero/witnesses/bab978a621b01692e75b93f8d4b46f5e72051206f94bbf0117cd4397af390a68.edn`.

Validation before publication used real state/replay, apply-record and
projection/current-claims: the successor projects as released and the original
record remains byte-for-byte equal as a Clojure value. write-witness! validates
and atomically publishes the record. **This does not establish live closure.**

The live watcher's read-only status reports running=true, interval=5000ms,
last completed cycle 18:02:28.797646013Z, next cycle started 18:02:33.801857396Z,
last progress 18:06:22.288458211Z, phase inbox-zero/cycle 556, last-error nil.
A second streamed scan at **18:19:43.150Z** finds that the snapshot has advanced
(mtime 18:17:59.580694841Z), adding commit observations/links, but still has
1,483 claims and the original active claim. Thus the watcher is making slow
progress, not proven stopped; the in-progress cycle began intake before the
release was published. The release remains pending intake and the claim
remains active in the persisted snapshot at this verification.
Do not report it closed until the watcher ingests this successor. No restart,
reload, competing writer, or forced extra watcher cycle was used. Diagnosing
slow intake and verifying its next cycle is a separate follow-up; creating a second writer
would violate the store's single-writer contract.

## Proposed implementation packet (not implemented)

**Require evidence for the exact content being promoted, and end its authority
when consumed.** Just expiring at turn-end is insufficient: B can edit while A's
claim is still active, before A's first ending turn. Merely sharing the existing
clean-observation filter also misses edits between watcher polls.

Smallest defensible scope: add a witnessed, turn-scoped edit chain with a known
clean Git baseline (or a verified same-session predecessor), and bind the
promotion to its exact resulting Git blobs/modes/deletions. Paths with legacy
claims, absent edit-chain evidence, mixed/unproven baselines, different content,
or expired/consumed turns must be held with typed reasons. After gates and
staging, verify the actual staged objects against the frozen witnessed result;
never silently refresh expected content to whatever is now on disk. Close the
consumed claim via the same single-writer intake. Keep dirty-set, promotion and
commit-link eligibility consistent. Shell edits without supported witnesses
remain ineligible for automatic attribution; supporting them needs real tool
instrumentation, not inference from a recent claim or matching filenames.

Files: futon3c `inbox_zero/witness.clj`, successful-edit/turn hooks in
`dev/futon3c/dev.clj`, `inbox_zero/turn_promotion.clj`; futon3 inbox-zero-lib
`state.clj`, `projection.clj`, `promotion.clj`, `promote_exec.clj`, plus corresponding
witness, projection, planner, executor and turn-promotion tests. Add a small
explicit release helper over the existing intake for operational use.
Estimate: **2–3 engineering days including real-Git race tests and review**;
no live deployment/reload belongs to this discovery packet.

Required bad-case test uses a real temporary Git repo: A creates an eligible
claim; B changes the file and leaves it dirty; A ends its turn. Assert HEAD and
B's working-tree bytes are unchanged, no commit is made for A, and a typed
content/provenance refusal is recorded. Repeat with a legacy claim, after a
previous commit, within one watcher interval, and with B editing during a gate.
Include own-session positive promotion, mixed baseline refusal, delayed release
intake and consumed-turn replay. A hash captured only after an edit is not enough
if it can include another seat's preexisting dirt.

## Validation and limits

Read-only source, Git diff and session-history inspection; streamed inventories;
real existing schema and projection validation of the release record. No
production code changed and no application suite was run. Documentation and
inventory commits use explicit paths only. Both TSVs were checked for expected
row counts, unique claim IDs, column shape and the exact age cutoff. Other agents' existing dirty files
were left untouched. Authorship of 4427b740 and live ingestion of the release
remain unresolved; the stale-claim mechanism itself is confirmed.
