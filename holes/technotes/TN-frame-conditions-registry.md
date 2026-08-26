# TN — a conditions registry for running APM frames

*claude-19, 2026-08-26. Joe: "to make this work well, we need a way to
register 'conditions' into the frames. A git hash is registered already. If
we could register an operational note somewhere, like a changelog, that would
be good."*

## The gap, with today's instances

A frame's manifest pins `:apparatus :revision` = `git rev-parse` of the
checkout at mint. That records the code on disk. Under "run continuously
(Codex), improve continuously (Claude)" most of what changes the running
instrument is not that:

| what changed today | in the sha? | where it was recorded before this note |
|---|---|---|
| namespaces reloaded into the shared JVM (H1, H2 before f43; H3 at 18:05Z; registry at 18:41Z) | no — HEAD moves on commit, the JVM on reload | session transcripts |
| `memory-cascade-arm.edn` (per-tick operator file) | yes, but only as a data file; its effect starts at the next mint | amendment 7 |
| futon3 library `@why` edits (H5a) and substrate reassignments (H5b) | no — other repo / no repo | TN addendum 7, PLAN-H5 |
| commits on master **not** loaded: `6a7735e1` (queue exclusions), `0e4b71ba` (attempt-1 holdout) | yes, and that is the problem: the sha claims them, the process does not run them | nowhere |

The last row is the sharp one. At 18:36Z HEAD was `0bc2b81f`; the JVM's
`open-problem-queue/excluded-classifications` lacked `"construction-blocked"`
and `live-learning-phases/receipt` was loaded from line 332 of a file whose
HEAD version has it at 355. Had f45 minted then, its manifest would have
pinned a revision containing two behaviours the frame did not have. And in
the other direction: f44 (minted 17:28Z, revision `7c0a9338`) is *not* an arm
frame, though amendment 7 expected it to be — the sha does say so, but only
to a reader who knows which later commit to compare against.

## The mechanism

- **`<campaign-root>/conditions.edn`** — an append-only vector of maps, one
  per operational change. Fields: `:id` (`C-n`), `:at`, `:by`, `:kind`
  (`reload` / `code` / `arm` / `library` / `substrate` / `queue` / `prereg` /
  `contract`), `:note`, `:head` (futon3c HEAD when registered), and
  optionally `:sha`, `:since-frame`, `:amendment`, `:namespaces`, `:loaded?`,
  `:expect` (the prediction), `:field` (the receipt path that would show it),
  `:until` (withdrawn).
- **`countdown-control/campaign-conditions`** reads the file on every tick
  (same pattern as `memory-cascade-arm`) and passes the entries without
  `:until` to the adapter; `queued-frame-adapter/mint` puts them on the frame
  and `one-off-manifest` copies them to the manifest as top-level
  `:conditions`, covered by `:manifest/id`. Empty registry ⇒ no key,
  byte-identical to before. An unreadable registry becomes a
  `:kind :registry-error` entry on the frame rather than a silent drop.
- **`scripts/apm-condition.bb`** appends (stamps `:at`, `:head`, next id),
  withdraws (`--withdraw C-n`), lists.
- **`scripts/apm-reload.sh --by <id> ns…`** reloads from
  `/home/joe/code/futon3c` on master with `(require ns :reload)` — never
  `:reload-all`, so other agents' unloaded commits stay unloaded — and then
  registers a `:kind :reload` entry. **While a campaign is running, a reload
  without an entry is the violation**, the way a bell without a park is.

The registry records what was in force. The prereg amendments record intent
and predictions. The receipt fields named in `:field` are where the outcome
appears. Those three, read together per frame, are what "data on what works"
means under the continuous plan; the frame is still the unit of evidence.

## State at 18:45Z

Entries C-1…C-8 backfilled from today's work (H1, H2, reader fixes, H5a,
H5b, H3 arm — with the f45 correction — and the two not-loaded commits with
their probe evidence); C-9 is the registry's own reload. f45, the next mint,
is the first frame whose manifest will carry them. Correction record
appended to the prereg after the second "amendment 5".

## Not done

- Nothing verifies `:loaded?` automatically. Clojure keeps no hash of loaded
  source; the probe used here (a var's `:line` metadata against the file)
  is a tell, not a check. If this matters enough, `apm-reload.sh` could
  record per-namespace file digests at reload time and the mint could
  compare them against HEAD's blobs — then a frame would say *which*
  namespaces differ from its pinned revision.
- Substrate state at mint (futon1b as-of, relation counts) is not captured;
  H5a/H5b are registered by hand.
- The registry is per campaign root. Cross-campaign conditions (the JVM is
  shared) would need one file per JVM, or the same file symlinked.
