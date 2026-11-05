# Moving derived records out of evidence

Joe, 2026-11-05: the copies already in evidence "need to be moved out of the evidence store ...
they are derivatives of evidence and not evidence, and therefore they need to be handled properly.
I question if they should be in XTDB or in a sidecar. The whole design remains open. But querying
for 'hearo' and 'Lionheart' etc. should return real turns not derivative turns."

## What the two searches hit (FTS sidecar, all time, 2026-11-05 22:20Z)

"hearo" 88 rows, "Lionheart" 85. Real acts among them: Joe's turns (4 / 4), claude-8 replies (5 / 4),
turn-commits (2 / 0), one parked-resume dispatch. The rest are derived:

| Family | hearo | Lionheart |
|---|---:|---:|
| artifact-activation (weak activation) | 25 | 25 |
| zai/kimi `:turn-start` prompt copies | 23 | 22 |
| 象-1 `:turn-round` (Circus seat output) | 9 | 10 |
| promise-history | 7 | 0 |
| 象 settled-turn (`:xiang-turn`) | 4 | 5 |
| context-retrieval | 3 | 2 |
| 馬 observation | 3 | 3 |
| dev `:invoke` prompt preview | 2 | 3 |
| minus40 `:memory-injected` | 0 | 7 |

## Size, all time (824,411 rows indexed)

| Family | Rows | Body bytes |
|---|---:|---:|
| promise-history | 50,767 | 375 MB |
| artifact-activation | 22,290 | 270 MB |
| zai/kimi turn-round | 127,193 | 210 MB |
| zai/kimi turn-start | 9,141 | 156 MB |
| dev invoke (start/complete/preview) | 245,232 | 137 MB |
| **chat turns + replies (the acts)** | **44,855** | **102 MB** |
| context-retrieval | 77,790 | 60 MB |
| 象 settled turn | 3,328 | 33 MB |
| 馬 observation/card | 1,312 | 7 MB |
| minus40 memory-injected | 657 | 3 MB |

The participant acts are about a tenth of the indexed bytes.

## Classification is by role, not only by tag

`:turn-round` is the Zai harness's per-round output. For a working seat (zai-2 doing a bell) the
final round is that agent's reply, an act. For a Circus seat (象-N, 猴-1, 馬, 驴, 狗) it is machinery
output, derived. So the classifier needs the seat's role (M-repl-circus animal table), not just tags.
Dispatch records (`:invoke-start`, bell/whistle `:mesh-edge` between agents) are acts; mesh edges
written by Circus readers (ma-reading, lu-rerank, ma-occurrence, turn-capture) are not.

## Plan

0. **Search returns acts (reversible, now).** The text-search endpoint gets an act allow-list as its
   default scope, with an explicit `scope=all`. An allow-list, so a new derived writer cannot leak
   back into search results.
1. **Derived store (design, Joe's assessment).** Recommendation below.
2. **Migration, per family.** Writers already stopped copying (writers 1-4, promise lifecycle step 5
   pending). For each family: copy to the derived store, verify counts and a sample, then remove from
   evidence (XTDB `ERASE`, which drops history, so a store snapshot first) and from the FTS sidecar.
   Erase is irreversible; each family's erase is confirmed with its counts before it runs.

## Recommendation for step 1: XTDB, in its own table, not a sidecar

- Derived records are joined to their source acts (`turn-evidence-id`, `turn-record-id`,
  `in-reply-to`). In futon1b they can be joined by XTQL with the acts and the hyperedges, which is
  what the GCC/XTQL direction asks for; a sidecar puts that join in application code.
- Readings are recomputed (象 `interpretation_version`, 馬 prompt versions). XTDB keeps the versions
  bitemporally; a sidecar would need its own versioning.
- Promise state already moved this way (hyperedges in futon1b), so there is one pattern, not two.
- The case for a sidecar: write load on futon1b (reads there are already slow: 0.3 s per point read),
  and volume (turn-round and invoke rows are 350 MB). Answer: many of those rows need not be stored at
  all, or only as counts; decide that per family in step 2, rather than choosing a store to absorb them.

A derived table is not full-text indexed with evidence, so search over acts stays clean by
construction; a separate index can be added for derived text if a reader needs it.
