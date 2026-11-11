# Review: 狗 (per-turn memory hook) and minus40 — 2026-11-11

Discovery packet for M-repl-circus (requisition invoke-1794428500422-17538-a170d4bf).
All measurement read-only; nothing reloaded, no memory writes. Times UTC.

## 1. The record-id "fault" — no write was skipped; the recall lives on the OPERATOR record

Joe's turn "🈳: OK please send pattern-index packet 3" (claude-13 session ed69fb38,
20:16:31Z) is the 象 **operator** record `turn-vsC2tD.json`. It **does** carry
`dog_recall`, written 20:16:32Z with the three recalled memories (top:
`-home-joe-code-voxterm/kimi-unique-re…`). The record claude-13 inspected,
`turn-x024sM.json` (created 20:17:34Z), is a **different** 象 record: origin
`agent`, author **kimi-2**, source_text "Requisition: M-repl-circus — BUILD packet
3…", turn_id `invoke-1794428254743` — kimi-2's answer turn, one job later. The
chain, file:line:

- Emacs sends the operator record id in the invoke payload; `payload-turn-record-id`
  reads it (transport/http.clj:5397-5401); the stream path threads it into the
  render (http.clj:7089-7092, `wrap-agent-facing-surface` … `prompt-facts-line`
  http.clj:5403-5420 → `prompt-line/render!` prompt_line.clj:228-234).
- The 狗 provider reads the recall cache keyed by **that** id (dog_provider.clj,
  `provider`, ~line 300) — `cached-recall "turn-vsC2tD"` would have hit;
  `cached-recall "turn-x024sM"` is correctly nil: the listener only computes
  recalls for operator-origin records (`handle-event`, dog_provider.clj:252-268).
- `write-dog-recall!` (prompt_line.clj:198-221) writes to the same operator id.

So the Prompt line's three items and the write agree; the confusion is that "the
象 turn record" is ambiguous — an exchange has an operator record AND an agent
record, and `dog_recall` rides only the operator one. Two real untyped-absence
gaps do exist: (a) a render whose ctx lacks `:turn-record-id` writes nothing and
says nothing (prompt_line.clj:232 `when (contains? ctx :turn-record-id)`); (b)
`write-dog-recall!` silently skips when the store lacks the record
(prompt_line.clj:214-216). A post-hook keyed on the agent record finds nothing.

## 2. Who edited 狗's files at 20:14–20:17Z — no record names the seat

What the records show:
- fs-watch: `dog_provider.clj` ingested 20:14:56 and 20:17:22;
  `dog_provider_test.clj` 20:15:14 and 20:17:30 (dev-zone-env journal).
- A language client queried futon1b hyperedges with
  `source-file=/home/joe/code/futon3c/test/futon3c/agency/dog_debug_test.clj`
  at 20:15:30, 20:17:32, 20:17:45 — two of the three ended `client-disconnected`.
  So an LSP-backed editor had a scratch `dog_debug_test.clj` open over the
  futon3c checkout in that window.
- Agency jobs 20:10–20:17: only 馬-reading/馬-occurrence whistles, 驴-rerank,
  象 turn-capture — no dispatch to any coding seat.
- Claude transcripts: `dog_debug_test` appears ONLY in claude-13's ed69fb38
  transcript, from 20:17:46 onward, reading journalctl/git status (its own
  investigation of this same mystery). No transcript writes the file. No
  kimi/zai/codex session log names it; the file no longer exists anywhere.

Plainly: an interactive editor session (LSP client over the main futon3c
checkout) did scratch debugging — touched `dog_provider.clj` without changing
content (checkout still matches `e74d5d3d5`), created and deleted
`dog_debug_test.clj` — and no Agency-side record identifies the seat. The two
`client-disconnected` traces are the only fingerprints.

## 3. Relevance — 15 real operator turns (typed, 2026-11-11 18:08–20:32) vs the three sources

Keyword recall (the live 狗 query: 6 longest non-stopword tokens, OR'd, FTS5,
memories only). One line each, honest:

| turn | text (start) | 狗 keyword recall | verdict |
|---|---|---|---|
| SbJ0iS | "plenty of other aspects of the REPL Circus…" | zai-5/draw-pile-probe-defects, zai-10/wm-click-loop… | generic (matched junk tokens) |
| FUJVC9 | "狗 has been under-developed but is straightforward" | zai-5/injuries-ledger-invalid | wrong |
| 5AdxZx | "pre-hook to retrieve relevant memories" | futon3c/apm-pattern-attachment…, emacs-graph-process-leak | one borderline (memory vocabulary), rest wrong |
| wu8Uwr | "'post-' … bury new memories" | futon3c/apm-promoted-memories-not-elaborated | borderline (promotion ladder ≈ bury ladder) |
| a06MZq | "run the WM over a pre-constructed field" | (none) | none |
| jLoseG | "empirical aspect … Zai obliged to look" | voxterm/voxterm-visibility-state-block ×3 | wrong ("visibility" drag) |
| yoPK5P | "update the tickets directly" | zai-6/q1-ticket-admission-slice | borderline ("tickets") |
| wWZNam | "how to run it? coordinator…" | voxterm/apparatus-design-principles ×2 | generic ("principles") |
| Yv4KkP | "checkpoint the environment" | zai-6/q1-runner-preparation-witness… | borderline (prep/checkpoint) |
| ni2vBM | "stage-2 cost, policy network?" | voxterm/selection-reasons-over-policies | borderline (wrong sense of "policy") |
| 6MMa3B | "pager-like mechanism, check top 10" | zai-1/lu-read-half | **relevant** |
| vsC2tD | "OK please send pattern-index packet 3" | voxterm/stop-dispatches-2026-10-23 ×2 | wrong — and actively misleading |
| qLnngG | "another seat was debugging 狗" | voxterm/minus40-encryption-parked | borderline (right topic, wrong fact) |
| DpRG1G | "sandbox issues, empty mission registry" | zai-4/standing-authority…, zai-6/wm-preflight-jvm | generic |
| UkLbCu | "continue with chained or parallel dispatches" | voxterm/stop-dispatches-2026-09-26 | wrong (same trap: "dispatches") |

Tally: relevant 1, borderline 5, generic 4, wrong 5. Worst failure mode:
standing *orders* ("stop dispatches on <date>") recalled as if they were current
facts on two dispatch-related turns — the FTS5 query has no time filter and the
memory's hook is a dated instruction.

驴's patterns for the same turns (from `turn-*.json.patterns.json`) are markedly
better aimed: `operator/bare-continue-signal` + `operator/grant-the-go-ahead`
for "send packet 3"; `operator/recognise-repeated-advice-by-recalling-where-it-was-given-before`
for the 狗-debugging turn; `operator/span-the-turn-boundary-by-chaining` for the
pager turn. Mostly relevant, occasionally noisy on long technical turns
(a06MZq matched 40+ patterns).

m40_near (embeddings; sampled on 3 of the 15, k=3): for the 狗 pre-hook turn —
minus40-rollout-vision, capability-narrative-on-return, proposals-crystallise-over-a-chain
(bordering generic); for the two dispatch turns — proposals-crystallise-over-a-chain
and dispatch-chain memories: better than the keyword recall's stop-dispatches,
still generic.

Which source to use first: **驴's patterns** (they already encode what the turn
is *for*); memory recall second, ideally seeded by the matched patterns' terms
rather than raw keywords; keyword-only recall last, with a recency/standing-order
guard. (This matches Joe's 11-05 shape: 驴 patterns as the pre-hook's relevance
signal when available.)

## 4. Timing — ready at render 4/4; but the background query is 8× slower than measured

Typed operator turns since the hook went live (~19:10Z): `turn-ni2vBM` (20:10),
`turn-6MMa3B` (20:13), `turn-vsC2tD` (20:16), `turn-qLnngG` (20:21). All four
have `dog_recall` with items (2/2/3/1) — **no `not-ready`, no `failed`** in the
recorded fields. The listener fires at :recorded, comfortably before the agent's
invoke arrives. However the recorded `query-ms` is 613–824 ms, an order above
the provider docstring's measured p50 75 ms / max 101 ms — the production
OR-of-6-keywords FTS query is heavier than the measurement's queries. Harmless
today (the provider is cache-only, budget 25 ms), but it caps how many turns a
burst could cover on the single dog-recall thread.

## 5. minus40 as it is

Stores: Claude Code memory files (294 at milestone 2, grown since) with full
version history rebuilt from transcripts (`scripts/history.py`; 370 versions
then), `[[link]]` edges, extracted facts (`extract_facts.py`, `backfill_facts.py`,
`crystallise.py`), lu-situations (驴 writes its verdicts, e.g. tags `:lu
:lu/occurrence :xiang`), and zai-seat `memory_assert` appends under per-seat
projects. Reads: MCP tools (`scripts/m40_mcp.py`) for Claude seats; HTTP on
:7093 (`/api/alpha/evidence/text-search`, evidence queries — EDN responses);
`m40.py` CLI + embeddings sidecar (`embed_facts.py`) behind m40_near. Writes:
Claude Write/Edit PostToolUse hooks (`call_hook.py`, `ingest_hook.py`) with the
near-duplicate check; SessionStart inject (`inject.py`, `brief-enabled.txt`);
`memory_assert` from zai/kimi harness tools. Zai/Kimi reach it only through
their harness tools (m40_find/state/asof/history/near, memory_assert) — no MCP.
Stale/odd: project slugs are path-mangles (`-home-joe-code-voxterm`) — stable
but ugly and they dominated the bad recalls above; `brief-enabled.txt.bak-2026-10-14`
still beside the live list; encryption-at-rest parked
(voxterm/minus40-encryption-parked).

## 6. Per-family memory use (Joe's empirical question)

Rule used: a "lookup" is a recorded m40_* or memory_* tool call (harness
`turn-round` rows for zai/kimi/codex; transcript `tool_use` blocks for Claude);
"reached a named memory" = the call returned/named memory slugs; "used" = the
reply names the slug/hook, opens the memory file, or applies its claim.

- **Zai/Kimi/Codex (harness records):** paged the evidence store's `turn-round`
  rows from 2026-11-11 back past 2026-11-04 (~1833 rows; 馬/象/驴 automation
  included). **m40_* calls: 0. memory_record: 1** (kimi-1, 2026-11-11T18:52 —
  a write, not a lookup). So on records, these seats essentially never LOOK at
  minus40 even though the harness offers the tools — consistent with Joe's "Zai
  is obliged to look and doesn't find the store useful". Examples: (1) zai-1's
  11-10 daily-loop session: memories written (verdict-semantics), none read;
  (2) kimi-1's 18:52 memory_record — the only memory-ish call in the window;
  (3) zai-1's current review session: m40_find/near used only because the
  harness injects them per round (this packet's own calls).
- **Claude (transcripts):** 0 `m40_*` tool_use in `~/.claude/projects/**.jsonl`
  since 10-28. Claude's routine use is its NATIVE memory (auto-loaded memory
  dirs, SessionStart injection — 24 of claude-13's 188 frames carry `injected`
  lists) plus writing memory files that minus40 then ingests via hooks. So
  Claude *feeds* minus40 constantly and reads it via injection, not via calls.
  Examples: (1) claude-13's dog-01/dog-02 packets cite its own memory files'
  contents without any m40 call; (2) the 狗 recall items themselves are Claude
  memories (kimi-unique-re… under a voxterm path-slug); (3) claude-13's fix
  6621c4e message documents a memory it wrote earlier that session.
- **Codex:** turn-starts exist (codex-6 106, codex-26 38, codex-4 29 in the
  window) but no harness turn-round call records; nothing of Codex memory use
  is visible to Agency records — provider-side, as Joe guessed.

Caveat, stated plainly: the turn-round paging hit 503s and one short page; the
window is solidly 11-04→11-11, the deeper fortnight is interpolated from the
same store's density, not separately paged.

## 7. Inputs for the post-hook (use check + bury)

Observable today, per turn, with no new instrumentation:
- the recall: `:dog_recall` on the operator 象 record (items, why, query, ms) —
  item 1 says the post-hook must read it there (or the write must also stamp
  the agent record / link the operator id);
- the reply: the agent 象 record's `source_text`/`reply_text`, and for zai-family
  seats the `turn-round` rows' tool calls (a `read_file`/`run_shell` touching
  `~/.claude/projects/<proj>/memory/<slug>.md`, or an m40_* call naming the slug);
- for Claude seats, the transcript's Read/tool_use blocks name memory paths;
- string evidence: `scope_view_data.echoes()` (≥5-gram overlap with the memory's
  statements, name-match excluded for [[links]]) already implements "applied its
  claim" as a checkable rule.
Bury path: minus40's write side as it stands — `memory_assert` (creates the file
under the seat's project when given a hook; PostToolUse then runs the
near-duplicate check), or the HTTP store for JVM-side writing in 驴's
lu_memory style. A buried bone should carry status buried-by-狗/not-yet-used so
the use check can promote it (E-gcc-schema §16 item 14 ladder), per Joe's 11-11
decision that burial writes at once.

## Ranked fixes for 狗

1. Fix the post-hook's join: read `dog_recall` from the OPERATOR record (or
   write it onto both records); make a render without `:turn-record-id`, and a
   write to a missing record, typed absences (prompt_line.clj:214-216, 232).
2. Stop recalling standing orders as current facts: time-guard or flag dated
   directive memories (stop-dispatches-*) — the single worst relevance failure.
3. Seed the query from 驴's matched patterns (terms/titles) before falling back
   to raw keywords; keep keywords last.
4. Re-measure with the production OR-of-6 query (613–824 ms live vs 75–101 ms
   measured); consider 4 keywords or a tighter FTS query.
5. Record the use outcome per recalled memory (post-hook) so retrieval can
   learn — the prediction/observation pair the mission already specifies.
6. Cosmetics that matter for trust: readable project names for path-slug
   projects; drop the `×3` duplicate-hit rendering seen on jLoseG.

— zai-1, 2026-11-11. Sources: futon3c e74d5d3d5 + working tree (prompt_line.clj,
http.clj, dog_provider.clj); 象 records under ~/.emacs-graph/session-turn-analysis;
dev-zone-env journal; ~/.claude/projects transcripts; futon1b evidence store via
GET :7073; minus40 store via GET :7093.
