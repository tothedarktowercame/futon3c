# Excursion: E-monster-to-joey — compact oversized agent sessions so they fit the warm pouch (limit WIP memory + token cost)

**Date:** 2026-07-01
**Status:** **CLOSED 2026-07-02.** v0 recency-shell compactor built, resume-verified,
pouch-integrated (compact-then-warm at the monster-cold gate), ON by default, proven end-to-end on
real agents (claude-1/2/5/10 monster→joey, warm-resumed coherently). Commits `d72253e` (compactor),
`11406b2` (pouch integration), `4c2334d` (default-on), `180d76a` (doc). A real memory win (kills
per-turn cold-spawn RAM spikes). **NB:** the Emacs finalisation *lag* that partly motivated this had a
**separate** root — `arxana-ui-refresh`'s Reazon validation on every window change, not memory (see
[[project_loop_lag_emacs_stalls]]) — so this stands on its own memory merits. **Deferred follow-ons:**
the semantic selector (M-points-de-fuite, anchored by the clock — the "keep what matters not what's
last" ideal, swaps into the same shell); prune the per-compaction `.pre-compact-<ts>` backups; and
gate joey-max on *context* bytes (MAP finding #1). Owner: claude-3.
**Repo:** futon3c — `src/futon3c/agency/agent_pouch.clj` (the kangaroo pouch + joey
gate), agent session JSONL under `~/.claude/projects/<enc>/<session>.jsonl`.
Sibling of **M-kangaroo** (warm-pouch-per-agent). Driver:
[[project_loop_lag_emacs_stalls]] (system memory pressure — swap 100% full, a
`claude -p` spawn ~370MB is the spike that thrashes the box).

## HEAD (one line)
A **monster** session (transcript over `joey-max`, default 2 MiB) is kept COLD, so
every turn spawns a fresh `claude -p` with the full transcript — the ~370MB RAM
spike that swaps a memory-pressured box, *and* an uncached full-transcript re-send
(5-min prompt-cache TTL). **Compact the transcript so the session drops under
`joey-max` → it becomes a joey → stays warm (no per-turn spawn) AND costs fewer
tokens per turn.** Turn monsters into joeys instead of paying the monster tax.

## Why (evidence)
- **The joey gate is token-cost-first, RAM-second** (`agent_pouch.clj:51-58`):
  "A pouch keeps the PROCESS warm but cannot keep Anthropic's server-side prompt
  cache warm (5-min TTL). So warming a giant transcript is a token-cost trap:
  every turn re-sends the whole transcript uncached." Monsters stay COLD unless
  `allow-monster!`/env override. `default-joey-max-bytes = 2 MiB`
  (`FUTON3C_KANGAROO_JOEY_MAX_BYTES`); `default-max-warm = 8`.
- **The cold path re-sends the full transcript too** (cold resume = spawn `claude
  -p` with the whole session). So a big transcript is expensive *regardless* of
  warm/cold — compaction helps both paths.
- **Memory-pressure driver** (2026-07-01, via loop-lag): 30GB box, Firefox ~15GB
  (baseline, kept open), JVM ~5.3GB, Emacs ~2.1GB, swap 100% full. Each `claude
  -p` spawn (~370MB) tips into reclaim → multi-second stalls "when Claude is
  thinking." Fewer/smaller spawns = less thrash. Warming (via compaction) removes
  the per-turn spawn for that agent entirely.
- **The double win:** compaction reduces (a) RAM (smaller warm process / smaller
  spawn), (b) per-turn token cost (less uncached re-send), (c) latency (warm
  beats cold-resume — M-kangaroo's whole point). One mechanism, three payoffs.

## The idea (DERIVE options — not chosen)
Add a **compaction pass** that shrinks a session's transcript while preserving the
agent's working coherence, then re-evaluates `joey-eligible?`:
- **A. Deterministic prune** — drop/truncate large `tool_result` blocks and
  redundant tool I/O, keep the last N turns verbatim, collapse older turns to
  headers. Cheap, no LLM call, bounded loss. Likely the bulk of the win (tool
  outputs dominate transcript bytes).
- **B. Summarization head** — an LLM pass rewrites older turns into a compact
  summary prefix (the harness's own `/compact` analogue). Higher fidelity, costs
  one call per compaction.
- **C. Memory-leaning** — lean on the agent's durable auto-memory (`MEMORY.md`)
  for durable facts and prune the transcript harder; the memory *is* the
  compaction of durable context. Natural synergy with the existing memory system.
- Likely **hybrid**: deterministic prune of tool bytes (A) → optional summary of
  the remainder (B), anchored by memory (C). Trigger when a session crosses
  `joey-max`, before the next warm decision; never mid-turn.

## MAP (2026-07-01) — monster composition + the Evidence-Landscape reframe

**Measured a 42.1 MB monster** (`f813a7b8…jsonl`, 12067 lines):
- **Model context (`message.content`, re-sent to the API): 22.6 MB (54%)** — dominated by
  `tool_result`, and within that **`Read` outputs = 6.7 MB** (52 large file-reads, 185–195 KB each):
  recoverable (file is on disk) and low coherence-value once the turn is past.
- **Local envelope: 19.5 MB (46%)** — biggest single field is **`toolUseResult` = 10.7 MB, a
  redundant local copy of tool results** not sent to the API; plus `attachment` (1.2 MB) and per-record
  ids/timestamps.
- Fleet: **53 monsters (>2 MiB) of 858 sessions**; biggest ~42 MB (20× joey-max).

Findings:
1. **The joey-max gate over-counts** — it measures raw on-disk bytes, but 46% is local envelope
   (incl. the redundant 10.7 MB `toolUseResult`) that costs **zero tokens**. The gate should measure
   *context* bytes, not raw JSONL — some "monsters" are joeys in token terms.
2. Within context, recoverable tool outputs (`Read`/`Bash`) are the dominant prunable chunk.

**Reframe (Joe, 2026-07-01) — the decisive one: history lives in the Evidence Landscape.**
The durable record of what happened is the **Evidence Landscape** (EvidenceEntry / PSR·PUR·PAR /
substrate-2 / the mining pipeline), NOT the session JSONL. So the session transcript is **ephemeral
working context**, not an archive — and **aggressive compaction of live sessions is sanctioned**. The
coherence bar collapses from "preserve everything" to "**enough to continue**"; `MEMORY.md` carries
durable facts, the Evidence Landscape carries history. This retires the lossless-first caution.

**Consequent DERIVE shape (aggressive):** keep a **recent working window** (last-N turns / last-K
context bytes) verbatim + **strip local envelope** (`toolUseResult` dup, attachments); drop older
turns (history is safe in the Evidence Landscape). Optional thin continuity-summary head. Target:
well under joey-max in **context** bytes → the session stays a warm joey.

**Coupled dependency (safety, not a blocker):** aggressive dropping is only safe if the Evidence
Landscape actually captured the dropped history. The XTDB evidence store was observed **degrading**
(`evidence count timed out after 15000ms` — see [[project_loop_lag_emacs_stalls]]). So compaction's
safety **couples to evidence-capture health** — the same XTDB thread. Verify evidence coverage of a
session before enabling aggressive drop for it.

## DERIVE decision (Joe, 2026-07-01): ship the recency shell first
Ship **v0 = recency/byte-window shell** to prove we beat the memory issue (monster cold-spawns →
swap thrash); the **semantic selector** (M-points-de-fuite comb/threads/concentration anchored by the
clock — the *ideal*, "keep what matters not what's last") is a **later** swap into the same pluggable
shell, gated on M-points-de-fuite being well-functioning (currently basic-pass, weak on the very
code-heavy sessions that are the monsters — §6.0 of that mission). The shell is identical either way;
only the selector differs.

**v0 design (recency shell):**
- **Never mutate the on-disk transcript** — produce a compacted *copy* the pouch resumes from.
- **Strip local envelope** (`toolUseResult` dup ~10.7 MB, `attachment`) — not sent to the API.
- **Recency window** — keep newest records until a context-byte budget (< joey-max, headroom target
  e.g. 1.5 MiB); drop older (history is safe in the Evidence Landscape).
- **Target:** the compacted copy is joey-eligible → the agent stays warm → no per-turn cold-spawn.

**Open RISK to resolve before building (resume-correctness):** the session JSONL is a `parentUuid`
chain that `claude --resume` walks to reconstruct the conversation. Blind record-dropping breaks the
chain, and it is unverified whether `toolUseResult`/`attachment` are load-bearing for resume. The
transform must yield a session claude resumes *correctly* — so DERIVE step 1 is: determine the safe
transform (shrink-in-place vs truncate-and-re-root) and **prove a compacted session resumes**, on a
throwaway session, before wiring anything into the pouch.

## v0 BUILT + resume-VERIFIED (2026-07-01, claude-3) — `scripts/compact_session.py` (d72253e)
The recency shell is built and the resume-correctness risk is **resolved live**:
- **De-bloat (lossless):** a de-bloated fork resumed and recalled prior turns (codeword + which file
  it read) — full conversation preserved through dropping non-conversation records + `toolUseResult`.
- **Truncate (lossy) + re-root:** a hard-truncated session resumed coherently — dropped the old turn
  (`APPLE-ONE` gone), kept the recent (`ZEBRA-NINE`), and continued normally. `claude --resume`
  accepts our rewritten transcript.
- **Monster:** 42.15 MB → **1.70 MB** (under the 2 MiB joey-max); de-bloat alone ~70% (29 MB).
- **Bug caught by testing:** an over-tight budget could emit a user-less/empty transcript
  ("No conversation found"); fixed — truncate never stops before the window includes a `user` turn.
- **In place, same session-id** (agent clock/evidence/pouch key untouched), original backed up; runs
  on cold sessions.

**Pouch integration DONE + proven on a real agent (2026-07-01, 11406b2):** `agent_pouch.clj`
`compact-session!` (shells the compactor, in place, backup) + `joey-eligible-or-compact?`;
`dev.clj` monster branch swapped to it (flag `FUTON3C_KANGAROO_COMPACT_MONSTERS`, default OFF;
registration-frozen closure → auto-path activates on next restart). **Live capstone on claude-2**
(a real, idle/quiescent monster): `compact-session!` took it 2.61 MiB → 1.69 MiB (dropped 124 old
turns), `joey-eligible?` flipped false→true, and its next turn **warmed** (`[invoke] claude-2 warm
pouch feed`, not a cold-spawn) and **resumed coherently** — it recalled its recent work (the
C-cascade-real capability census) from the surviving tail. The ~370 MB cold-spawn is eliminated for a
compacted agent *without a restart* (the plain `joey-eligible?` gate passes once the file shrinks).

**Follow-ons (recorded):** (a) prune the per-compaction `.pre-compact-<ts>` backups; (b) fix the gate
to measure *context* bytes per MAP finding #1 (free correctness win); (c) tune the default target —
for sessions where de-bloat alone lands just over joey-max, a target near 2.0 MiB truncates far fewer
turns than 1.7; (d) the semantic selector (M-points-de-fuite) — the *ideal* extraction, later.

## VERIFY (acceptance — to define at DERIVE)
1. **Fits:** a real monster session compacts to < `joey-max` and `joey-eligible?`
   flips true.
2. **Coherent:** the compacted agent's next turns continue its in-flight work
   correctly — no critical context loss (judged on a real session, side-by-side).
3. **Measured:** the agent runs WARM (no per-turn `claude -p` spawn), lower
   per-turn token count, and loop-lag shows fewer/smaller spawn-correlated stalls.
4. **Safe:** compaction is idempotent and reversible (keep the original JSONL);
   busts the prompt cache exactly once, then stabilises.

## Scope out
- NOT changing `max-warm` or serialising invocation concurrency (separate WIP
  levers — worth their own note, but not this excursion).
- NOT the swappiness / JVM `-Xmx` / XTDB fixes (see [[project_loop_lag_emacs_stalls]]).

## Care points
- Compaction is **lossy** — the hard part is the coherence bar, not the byte
  reduction. Bias to pruning tool *outputs* (recoverable, high-byte) over
  reasoning/decisions (load-bearing).
- The prompt cache resets once per compaction (changed prefix) — acceptable; the
  monster was already uncached every turn.
- Coordinate with the agent's own summarisation/continuity: don't fight the
  harness's context management; compaction is about the *persisted session* that
  feeds cold-resume / warm re-send, not the live context window.

## Cross-links
- **M-kangaroo** — the warm-pouch system this extends (the joey/monster gate,
  `allow-monster!`, `joey-eligible?`).
- [[project_loop_lag_emacs_stalls]] — the memory-pressure driver + loop-lag as the
  measurement instrument for the "fewer spawn spikes" claim.
- [[project_blackboard_backpressure]] — synchronous backpressure is the adjacent
  stall family.
