# SPIKE-f2-rebase — VERIFY spike S2: three-way text rebase for F2

Author: kimi-2, 2026-11-08. Branch `spike/f2-rebase` (worktree
`~/worktrees/futon3c/spike-f2-rebase`). Spike only — not for merge; nothing
was loaded into the running futon3c JVM.

Contract under test: DERIVE-F2.md "Text rebase: what F2 must define
(codex-21, round 9)".

## What was built

`src/futon3c/arxana/spike/rebase.clj`:

- `canonicalise` — UTF-8, LF newlines, BOM stripped; `revision-digest` =
  sha256 of canonical bytes. CRLF input normalises before digest and patch
  application (tested).
- `->patch` / `patch-digest` — patch = base digest + author + op-id +
  ordered, non-overlapping codepoint hunks `{:start :end :replacement}`;
  `patch-digest` over the canonical pr-str of that identity.
- `classify-hunks` — the five overlap classes: `:disjoint`, `:identical`,
  `:same-position`, `:nested`, `:partial-overlap`. Boundary convention: a
  zero-length insertion at a hunk boundary counts as disjoint and orders
  deterministically (insertions at point p sort before a hunk starting at
  p; two insertions at p order by replacement bytes).
- `rebase base pa pb` — verifies both base digests (stale writer → typed
  `ex-info`, the CAS-conflict analog). Mergeable (all pairs disjoint or
  identical) → `{:result bytes :merged-hunks [...] :dropped-identical n}`;
  identical hunks are applied once. Any conflicting pair →
  `{:alternatives [a-side b-side] :conflicts [...]}` where each conflict
  names both hunks, both authors, both op-ids, and each alternative
  applies one side's conflicting hunks plus all non-conflicting hunks
  from both sides. No silent winner, ever.
- `transform` / `compose` — same-side sequencing helpers for offline
  replay. `transform` shifts a patch across a disjoint landed patch and
  refuses (throws) on any non-disjoint pair. `compose` folds a client's
  sequential chain into one patch against the original base; it throws
  when a later edit nests inside the client's own insertion (limitation,
  see below).
- `locate` — addressability check. **Coordinate-based, not content
  search**: given the merged hunk list, each hunk's result position is
  computed by offset arithmetic; a replacement must equal the result text
  at exactly that position, and a deletion checks each surviving base
  neighbour at the computed gap boundary (vacuous where another merged
  hunk occupies the boundary).

`test/futon3c/arxana/spike/rebase_test.clj` — the four required test
groups.

## What passed

- Five hand-written overlap-class cases: disjoint merges with both hunks
  locatable and `rebase(a,b)` bytewise equal to `rebase(b,a)`; identical
  applied once; same-position, nested, and partial-overlap each yield
  exactly two attributable alternatives and no `:result`.
- Offline replay: B's three sequential patches against h0 rebase onto h2
  (A's two landed patches) **in order** via per-step `transform`, and the
  result is bytewise identical to the one-shot combined rebase and to the
  hand-written expected text. B's hunks remain locatable in the result.
- Property test: 500 seeded random patch pairs (1–3 hunks each) over
  random texts (20–120 codepoints, alphabet includes α/β, CJK, emoji).
  Tally: **265 mergeable, 235 conflict**. Mergeable: every pair
  commutative bytewise, both patches locatable (2910 assertions total).
  Conflict: every pair produced alternatives, never a silent winner, with
  author/op-id attribution on each conflict.
- Multi-byte: CJK replacement + emoji insertion at codepoint offsets
  compose correctly; emoji same-position conflict yields alternatives.
- Gates: `clj-kondo` clean (0 errors, 0 warnings);
  `~/code/futon4/dev/check-parens.el` OK on both files.
- Test command (own process, seed 20261108):
  `clojure -Sdeps '{:aliases {:spike {:extra-paths ["test"]}}}' -M:spike -e "(require 'futon3c.arxana.spike.rebase-test)(clojure.test/run-tests 'futon3c.arxana.spike.rebase-test)"`
  → `{:test 5, :pass 2910, :fail 0, :error 0}`

## What failed / counterexamples found

1. **Content-based `locate` is unsound for deletions.** First cut searched
   the result for the replacement string, or for deletions an 8-codepoint
   base context. The property test killed it in ~12% of mergeable trials:
   a disjoint edit adjacent to a deletion disturbs the context window, so
   the deletion could not be found by content even though it was correctly
   applied. Fix: addressability must be coordinate-based (positions
   computed from the merged hunk list), with content used only to verify
   the coordinate. This is a real finding for F2: "both edits remain
   addressable" requires carrying the rebased hunk metadata (or an
   equivalent coordinate map) alongside the result bytes; the bytes alone
   do not reliably support relocation of deletions.
2. **Whitespace-only replacements** were invisible to a `str/blank?`-guarded
   search (a single-space replacement reported unlocatable). Realised via
   a property-test counterexample; fixed by the coordinate-based locate.
3. **`identical` never arose randomly.** 500 random pairs produced no
   identical hunk pair; that class is covered only by the hand-written
   case. Fine for a spike, but an F2 property suite should generator-force
   identical pairs.
4. **`compose` limitation (recorded, not fixed):** a client's sequential
   edit that nests inside its own earlier insertion cannot be expressed in
   original-base coordinates and throws. Offline replay of such chains
   would need per-step rebasing against intermediate docs rather than
   chain composition. This does not affect the contract question (the
   chain is same-side, so a sequencer sees it as one patch), but an
   implementation must not assume `compose` always succeeds.

## Verdict

For the cases F2's workload actually contains — concurrent disjoint edits,
identical edits from retries/races, offline chains replayed against a
moved head — the CAS-sequencer-plus-rebase contract **retains both edits'
intent deterministically**: merge results are commutative bytewise,
stale-base writes get a typed conflict, and every hunk remains
addressable, PROVIDED the rebased hunk metadata travels with the result
(finding 1). Same-position, nested, and partial overlaps never silently
pick a winner; they surface as exactly two attributable alternatives,
which is the mission's required behaviour, not a failure. No case in 500
random pairs or the hand-written suite triggered the CRDT re-arm
condition: no legitimate history was lost and the single sequencer never
had to be refused. The re-arm condition to keep watching is not textual
at all — it is the *availability* leg of DERIVE-F2's falsifier (a client
that must converge with no accepted sequencer reachable). On text
semantics alone, F2 does not need a CRDT.
