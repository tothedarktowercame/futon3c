# SPIKE-f2-rebase — VERIFY spike S2: three-way text rebase for F2

Author: kimi-2, 2026-11-08. Original branch `spike/f2-rebase`. Promoted to
`src/futon3c/arxana/text_rebase.clj` and
`test/futon3c/arxana/text_rebase_test.clj` by F2a.

Contract under test: DERIVE-F2.md "Text rebase: what F2 must define
(codex-21, round 9)".

## What was built

- `canonicalise`: UTF-8, LF newlines, BOM stripped; revision digest is sha256
  of canonical bytes.
- Patch identity: base digest, author, operation id, and ordered,
  non-overlapping codepoint hunks `{:start :end :replacement}`.
- Five overlap classes: `:disjoint`, `:identical`, `:same-position`,
  `:nested`, `:partial-overlap`.
- `rebase`: disjoint/identical edits merge deterministically; the other three
  yield two attributable alternatives and never a silent winner.
- `transform` and `compose`: helpers for offline replay across landed edits.
- `locate`: coordinate-based addressability verification. Deletions cannot be
  relocated reliably from result bytes alone, so rebased hunk metadata must
  travel with the result.

## Evidence

- Five hand-written overlap cases.
- Offline replay of three sequential stale patches across two landed patches.
- 500 seeded random patch pairs over ASCII, Greek, CJK and emoji: 265
  mergeable, 235 conflicting; mergeable results commuted bytewise.
- Multi-byte replacements and emoji insertions use codepoint, not UTF-16,
  offsets.
- Original focused run: 5 tests, 2910 assertions, 0 failures, 0 errors.
- Promoted F2a run: see `test/resources/arxana/formal_arxana/F2a-warrant.edn`.

## Counterexamples and boundary

Content search was unsound for deletions and whitespace-only replacements;
coordinate-based relocation fixed both. `compose` deliberately refuses a
later same-side edit nested inside text inserted by an earlier edit, because
that chain cannot be represented in original-base coordinates. A production
sequencer must preserve that refusal or replay the intermediate revisions.

## Verdict

The single CAS sequencer plus deterministic rebase retains both edits for the
tested workload. Same-position, nested and partial overlaps remain explicit
alternatives. The CRDT re-arm condition remains availability: a client that
must converge while no accepted sequencer is reachable.
