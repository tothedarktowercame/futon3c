# 狗 per-family memory use — measurement, 2026-11-11

Packet: M-repl-circus 狗 family measurement (claude-13's requisition,
invoke-1794436114541-17637-6931f4b6). Read-only discovery; no running
code changed. Window: 2026-10-29 → 2026-11-11 (14 days) unless a cell
says otherwise. Use rule: futon3c dog-provider/use-check, called from a
stand-alone Clojure process (not reimplemented): "named" (reply names
slug or project/slug) or "echoed" (≥ echo-min-shared = 2 shared
distinctive content bigrams vs the memory body on disk).

## 1. Lookups per turn, by route

| family | turns | lookups | lookups/turn | routes | source |
|---|---|---|---|---|---|
| claude | 1103 | 31 | 0.03 | m40_find 14, ToolSearch(memory) 13, Read of memory/*.md 2, m40_near 2 | `~/.claude/projects/*/*.jsonl` (1266 transcript files touched in window); turns = string user messages |
| zai | 287 | 18 | 0.06 | m40_find 12, memory_search 4, m40_near 1, read-file 1 | futon1b :7073 transcript evidence (`tags=transcript`, per-seat author query) |
| kimi | 144 | 6 | 0.04 | m40_find 3, read-file 2, memory_search 1 | same |
| codex | — | — | — | — | typed absence: zero `:transcript`-tagged entries for codex-7/12/26 (codex seats author coordination entries but no tool rounds); its memory is provider-side, nothing is observable from here |

Counting rules and their limits:
- Zai/kimi windows are bounded by the store's author+tags query, which
  returns a 1000-entry window per seat and whose `next-cursor` repeats
  page 1 (measured: 1.07M fetched lines dedupe to 7,460 distinct ids).
  Per-seat windows: zai-1 11-04→11-11, zai-2 11-06→11-09, zai-3
  10-29→11-06 (460 entries, complete), zai-4/zai-5 10-31→11-06, kimi-1
  11-10→11-11, kimi-2 11-07→11-11, kimi-3 11-01→11-07. A full 14-day
  census is a typed absence: not reachable within the packet budget at
  this store volume; per-seat windows as listed.
- "Turn" for zai/kimi = transcript `turn-start` events; for claude =
  string user messages (Agency dispatches arrive as user messages).
- Claude's routine memory use is the auto-loaded MEMORY.md index at
  session start, which is not a lookup event and leaves no tool call:
  the 0.03/turn figure measures explicit lookups only. Typed absence:
  index-driven use is not observable as events.

## 2. Hits (share of lookups reaching a named memory)

| family | lookups | hits | share | rule |
|---|---|---|---|---|
| claude | 18 content lookups | 8 | 44% | m40_find/near result names a memory (slug + cc:project, or [date] slug), or Read of a memory file. The other 13 lookups were ToolSearch for the tools themselves — reaches a tool, not a memory: excluded from the base, noted |
| zai | 18 | 1 | 6% | same name rule over the round's `result-text` |
| kimi | 6 | 2 | 33% | same |
| codex | — | — | — | typed absence (as above) |

Zai's 12 m40_find calls mostly answered "0 memories match".

## 3. Use (share of reached memories the reply then used)

| family | memories measured | used | share | notes |
|---|---|---|---|---|
| claude | 20 | 5 | 25% | all 5 echoed, 0 named |
| zai | 1 | 0 | 0% | the one pair: 1 shared bigram, below echo-min-shared |
| kimi | 2 | 1 | 50% | the "used" one is a MEMORY.md index read (named); content-memory use 0/1 |
| codex | — | — | — | typed absence |

Base: lookups that named a memory AND have a reply text in the same
turn/session. use-check ran against the bodies on disk.

## 4. 狗 itself (operator turns of 2026-11-11, by family of the answering seat)

| family | turns with dog_recall | with items | items | items used (dog_use) |
|---|---|---|---|---|
| claude | 13 | 12 | 24 | 3 |
| codex | 4 | 4 | 10 | typed absence: dog_use never written (use check not yet live for those turns) |
| zai, kimi | 0 | 0 | 0 | typed absence: no operator turns went to zai/kimi seats in the window |

## Examples (3 per family where available)

- claude: session 75b00893, `-home-joe/apm-one-problem-a-day-tutorial` —
  **used** (echoed: "holes labs", "every problem"); session 80c1021b,
  `-home-joe-code/wm-lead-claude-1` — **used** (echoed); session
  80c1021b, `-home-joe-code-voxterm/outer-selection-landscape-research`
  — not used (m40_near's top hit, reply never touched it).
- zai: zai-turn-b16d3c75, `zai-4/wm-unticked-work-unseen` — not used
  (one bigram, "unticked-work-unseen closed", below threshold). Only one
  usable pair in the window; two more lookups were m40_find misses.
- kimi: kimi-turn-6359bb97, `-home-joe-code-voxterm/MEMORY` (the index
  read) — used/named; kimi-turn-7b9765d0,
  `-home-joe-code-voxterm/agency-dispatch-mode-work` — not used.
- codex: typed absence.

## The finding to act on first

Lookup volume is NOT the differentiator Joe's aphorism implies once
Claude's index-loaded model is counted: explicit-lookup rates are
0.03–0.06/turn across all three observable families. What differs is
the hit rate: claude 44%, kimi 33%, zai 6% — zai's obligatory lookups
mostly MISS (12 m40_find calls, 1 named memory). If 狗's recall is
tuned per family, the zai problem is query construction (its finds
return empty), not reluctance.

Correction (claude-13's review): an earlier version of this note
said a 狗-buried memory (狗/joe-2026-11-11-thinking-become-excursion-…)
"was Read by a claude seat the same evening and the reply echoed it",
as live evidence that burying works. That Read was claude-13 opening the
file to correct its truncated quote (a8d3e3fdc), the only Read of a 狗
file in the window. It is not use, and there is no live evidence yet.
