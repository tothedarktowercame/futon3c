# P6o-3: reconstructed origin backfill

The tool is `scripts/xiang2000_p6o3.py`; tests are
`scripts/test_xiang2000_p6o3.py`. No historical record is modified. The new
`:origin/backfill` evidence type is registered in the shared shape definition.

## Owner ruling and production execution

Claude-17 approved the reviewed plan on 2026-09-27: the reconstructed rules
are the authority; 436/27 are undocumented hand-count approximations, not
replayable membership lists. This resolves the write hold described below.
The exact pinned plan was executed: **671 written, 0 skipped, originals modified
0**, comprising 597 park wakes, 32 inbox notices and 42 Kimi notices. Each new
record was read back. The live rerun wrote **0** and skipped **671** matching
interpretations. One overlapping read initially received HTTP 503
`expensive-read-busy`; rerunning after the P0 capture finished succeeded.
No JVM reload was needed.

P0 now queries Joe-attributed user chat turns from 09-24 19:04 inclusive through
09-25 19:59 inclusive, matching the Kimi notice rule and harness origin. A known
write-time stamp takes precedence; an absent/unknown stamp may use an inferred
backfill. It counts source IDs, not interpretation rows. Both the notice turns
and backfill records are captured in the integrity-checked snapshot; evidence
reads share one P6 system-as-of pin.

Validation after the write:

- Live P0: `QUERY ... 42 notices ...`, `stubs: 3 of 12`, `check: PASS`.
- Two offline snapshot replays are byte-identical to each other and the live run.
- A copied snapshot with one Kimi backfill removed (and its manifest rebuilt to
  represent that altered dataset) reports 41; `--check` exits 1 naming row 6.
  Removed id: `origin-backfill:12bddfaac65ec3a36077dfe0ebc65b9c85ff93180ee9d0550223a1dcfbce63fd`.
- Changing the expected commit SHA exits 1 naming row 5. Byte tampering without
  updating the manifest exits 1 naming `origin-backfills.jsonl`, with no answer.
- Ten Python tests pass, compilation passes, EDN check-parens passes. No Clojure
  source changed in this follow-up.

Live output: `/tmp/p0-origins-live.log`; snapshot:
`/tmp/xiang2000-p0-origins-snapshot`; write and replay receipts:
`/tmp/p6o3-write.log` and `/tmp/p6o3-rerun.log`.

## Initial dry run and reconciliation hold

Read-only run on 2026-09-27, pinned at system time
`2026-09-27T18:58:28.219689Z`, event-time window
`[2026-09-22T00:00:00Z, 2026-09-27T00:00:00Z)`:

| Rule | Mission count | Live candidates | Saved export candidates | Live minus mission |
|---|---:|---:|---:|---:|
| park-wake | 436 | 597 | 406 | +161 |
| inbox-zero | 27 | 32 | 22 | +5 |
| kimi-notice | 42 | 42 | 42 | 0 |

There are 1,259 live user chat turns attributed to Joe, versus 961 window rows
in the raw local export. All 961 local IDs occur in the live result; 298 live
IDs are absent locally. The local window ends at
`2026-09-26T03:14:08.645224216Z`. Its kept-Joe file has 485 rows.
The live-only rows add **191 wakes and 10 inbox notices**, accounting exactly
for the live-versus-export differences. No reconstructed match is in the kept
Joe file. The 42 Kimi IDs agree in both sources.

This explains the corpus difference, **not the undocumented 436/27 baseline**:
that baseline exceeds the saved export by 30/5. A single later event-time
cutoff does not recover it: at 09-26 12:16:24.283845877Z the running counts are
436/25/42; when inbox reaches 27 at 13:25:46.776232408Z, wakes are already 459.
No original baseline membership list was found, so examples of its alleged
missing 30/5 cannot honestly be listed. At initial review, production writes were held under the
packet's “numbers match or differences are explained” condition. **Written at initial review: 0.**
The owner can resolve the remaining baseline discrepancy or revise the accepted
scope; this report does not silently treat unknown membership as reconciled.

Examples on the observable sides:

- Live-only wake `emacs-45eab4c3cdcf34faf13a935dc7471128`,
  09-26 23:54:38Z: exact `--- resumed: parked dependencies complete (1) ---`
  followed by a dependency bullet. Not present in the local raw export.
- Live-only inbox `emacs-c3838a8c20281b7baa394abaaa40680a`,
  09-26 21:57:02Z: `inbox-zero: futon2-d is carrying 14 dirty file(s)` with
  the full generated closing `Full list: git -C ... status --porcelain`.
- Matched Kimi `emacs-c193ae65f79b01da4fec7d90d5262dde`,
  09-25 19:59:53Z: requisition for M-futon-seams while clocked on
  M-the-perfect-crime. Present in both sources.
- Excluded operator discussion `emacs-46e69c9bcb4fad45e86fd6a63e649a84`,
  09-24 16:20:01Z: “we should create an enforcement rule similar to the
  inbox-zero followup” and a quoted Kimi phrase. Preserved, not classified.
- Excluded operator quotation `emacs-0eef8d4472ea9e80240e578e4cb38863`,
  09-24 21:11:21Z: “these 2 messages for claude-8” followed by quoted
  `followup: You requisitioned ...` messages. Preserved, not classified.
- No local-only evidence IDs exist in this window. No real operator park-phrase
  quotation was found; its regression test is explicitly constructed, while
  the real 16:20 Joe turn above is also tested verbatim.

The raw-file SHA256 is
`5b0f42c3833bc8a935ac01b4e333973963be1a3ea2ad562e448f4ceccc86d99c`;
kept-file SHA256 is
`8e2dfc230b973ad12f2aa9bb16aec7363cd7ae816d34f094f7080e31c61a3666`.
The full local review plan (including source rows and candidate hashes) is
`/tmp/p6o3-plan.json`; printable comparison is `/tmp/p6o3-dry.json`.

## Rules and search

Repeated searches covered the two files in `storage/operator-turns/window-0922`,
futon3c scripts, and `git log --all -S 'operator-turns-joe'` over scripts and
mission/lab documentation. Only P0 and documentation referenced that filename;
no original extraction/filter implementation or membership manifest was found.
These rules are **reconstructed**, version `p6o3-reconstructed-v1`:

- Park: exact line-anchored completion/deadline marker, followed by a dependency
  bullet when present, or an empty zero-completion tail. Reject fenced examples.
  Matched span is retained. This classifies the delivery act; an original payload
  prefix and dependency reports retain their own authorship.
- Inbox: whole-text match of the generated dirty-file notice including its
  opening, instruction, and git-status closing.
- Kimi: whole-text match of the three requisition notice variants or the
  requisition-refusal template, including agreement of repeated work targets.

These are retrospective interpretations, not proof of authorship or authority.
A verbatim unfenced pasted producer message may remain indistinguishable from
an actual historical delivery. Ordinary prose mentions, quoted lines and fenced
park examples are excluded; no unclassified turn is inferred to be operator.
Records with a known write-time origin are skipped.

## Read/write contract

```
python3 scripts/xiang2000_p6o3.py --dry-run --plan /tmp/p6o3-plan.json
# Only after the reconciliation condition has been met:
python3 scripts/xiang2000_p6o3.py --write --plan /tmp/p6o3-plan.json
```

Pagination uses `(at,id)` and a system-time pin, with limit 1000. The write phase
re-reads the pinned source window and checks every candidate hash, rule and
window before any append. IDs derive from `(rule version, original evidence id)`;
an identical existing interpretation is skipped and a conflicting one refused.
Each successful append is read back. A partial run can resume with the same plan.

The new record body carries original id/time/hash, inferred origin/actor,
rule/version, `basis: backfill-inferred`, and the window. The envelope's own
origin is harness/write-time because this script writes the interpretation now.
It does not change the original author, origin, timestamps or text, and grants
no authority. Readers must opt into these interpretation records separately
from original write-time stamps. No shared-JVM reload was performed; the production write followed the owner
ruling above.

## Gates

- Six Python tests pass, including the quote bad case, real Joe discussion,
  notice variants, source/plan tampering, conflicting interpretations, and
  append-only idempotence (first run 1, second run 0, original unchanged).
- `py_compile` passes for both Python files.
- clj-kondo: zero errors/warnings; check-parens: OK for the shape and its test.
- `futon3c.social.shapes-test`: 50 tests, 132 assertions, zero failures/errors.

The isolated replay test uses an in-memory store; production replay results are
recorded in the execution section above.
