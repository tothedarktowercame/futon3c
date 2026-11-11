# MAP: the Circus animals against the code — 2026-11-11

Packet: claude-13's MAP requisition (invoke-1794439280050-17659-879efd6f).
Mission table: holes/missions/M-repl-circus.md §"The Circus" (l.116–124).
Every citation read this session; "unconfirmed" marks what time ran out on.
Line numbers are futon3c master 1f600fb36.

## 驴 (donkey) — pattern retrieval for the operator turn

1. **Trigger:** turn-service `:recorded` listener `::lu-rerank`
   (`xiang/lu_rerank.clj:334` `start!`; `handle-event` :300–324). Runs on
   every operator turn with non-blank `:source_text` and no existing
   `:lu_ranked` (guard at :313–315), unless the off-switch file exists.
   Additionally the Prompt-line provider
   (`agency/pattern_card_provider.clj`, registered via prompt-line) runs at
   every render of a non-analysis seat. The `.patterns.json` candidates are
   written at dispatch time by `turn_service/find-pattern-candidates!`
   (l.1331–1343, writes via `turn_store.clj:167`).
2. **Input:** the record's `:source_text`; the candidate pool = embedding
   top 20 + giant-graph neighbours of already-occurrent patterns
   (lu_rerank.clj docstring, l.1–12); `:pattern_verdicts` of earlier records
   of the session (lu_rerank.clj:71–80).
3. **Output:** `:lu_ranked` {:version :at :ms :source :reason :ranked} on
   the operator record (`~/.emacs-graph/session-turn-analysis/turn-*.json`);
   `.patterns.json` beside it (turn_store.clj:53); the Prompt-line
   `:pattern` segment; `:proposed_patterns` appended by prompt-line's
   render! (prompt_line.clj `write-proposed-patterns!`).
4. **Readers:** the agent (Prompt line, transport/http.clj:5404
   `pattern-prompt-fact`); 馬-occurrence reads `:proposed_patterns`
   (ma_occurrence.clj docstring l.4–6); turn_service's pattern-outcome
   witness reads `.patterns.json` (l.728); 狗 reads `.patterns.json` for its
   recall seed (dog_provider.clj `lu-terms`); the cascade lab reads
   `.patterns.json`/occurrences.
5. **GCC:** `[:pattern/proposal 1]`, one attestation per producer —
   **[decided], no writer** (E-gcc-schema.md l.389).
6. **Table match:** table says "post-hook" — the code runs 驴 at
   `:recorded`, i.e. *before* the reply, not after the turn (the row's
   "post-hook" is wrong or at least misleading; the rerank is record-time).
   "Metadata on the operator turn" ✓ (:lu_ranked, :proposed_patterns).

## 象 (elephant) — the turn reading

1. **Trigger:** `turn_service/record-turn!` → `dispatch!` (l.1292) to the
   analysis seat (default `象-sonnet`, l.65, `analysis-seat` l.235), on
   every operator turn unless dispatch is `:none`/declared or the record
   routine-draft settles it (l.1318–1326). So: every operator turn, modulo
   the routine-draft fast path and the declared/not-requested statuses.
2. **Input:** the record (`:source_text`, sentences), the 小象 draft
   (turn_service.clj `draft!` l.1167).
3. **Output:** the reading as `turn-*.json.analysis.json`
   (`turn_store/publish-analysis!` l.141; landed from the analysis seat's
   reply at transport/http.clj:12493 → turn_service/publish-analysis!
   l.1630); `:acts`/`:sentences` on the record (turn_record.clj); the
   settled evidence packet appended to :7073 by `settle-evidence!`
   (turn_service.clj:549–583); relationship-review evidence entries (same
   path); `:ports` from `compute-ports!` (l.~1000).
4. **Readers:** compute-ports! → `:ported` → 馬-reading; scope edges
   (`post-scope-edges!`); the stepper (scripts/turn_frames.py reads the
   record); daxiang.clj reads `:reply_text` (the input 大象 reads —
   turn_service.clj:1200). The analysis JSON's readers: the stepper;
   `retrieved-patterns` (turn_service.clj:726).
5. **GCC:** `[:interpretation 1]`; reply marks as `:mark-occurrence`
   entities — **[built] on fixtures; no production caller** (E-gcc-schema
   l.390).
6. **Table match:** ✓ "operator turn → proforma reading, intent marks";
   "derived store keyed by turn evidence-id" — actually the reading lives
   beside the record (analysis.json) AND as :7073 evidence; the table
   understates the record-side store.

## 狗 (dog) — the agent's memory hook (my build, cf9580a30 → 4fc17816f)

1. **Trigger:** `:recorded` (pre-hook recall, `agency/dog_provider.clj`
   `handle-event` :recorded branch; background compute kicked at arrival,
   provider reads cache only) and `:settled` (post-hook: use-check →
   `:dog_use`, then bury → `:dog_buried`; `prepare-use-check!`). Every
   operator turn unless dog-off; bury unless dog-bury-off.
2. **Input:** `:source_text`, `.patterns.json` (驴 seed), minus40 :7093
   text-search (GET); post-hook: `:dog_recall`, `:reply_text`, `:acts`,
   memory bodies from `~/.claude/projects/*/memory/*.md`.
3. **Output:** `:dog_recall` (prompt_line.clj `write-dog-recall!`),
   `:dog_use`, `:dog_buried` on the operator record; buried memory files
   under `~/.claude/projects/狗/memory/` + two store docs POSTed to :7093
   (`/api/alpha/evidence`, `/api/alpha/hyperedge`, ingest_hook's shape).
4. **Readers:** the agent (Prompt line `memory ~…`, http.clj
   `memory-prompt-fact`); scripts/dog_helps_report.py reads `:dog_use` and
   `:dog_buried` (l.186–194, 279); the minus40 store makes buried memories
   recallable by the next turn's pre-hook (verified live 11-11).
   Written-never-read: `:dog_buried`'s per-item statuses have no consumer
   beyond the report script; no stepper/Emacs reader of any 狗 field
   (grep: turn_frames.py, turn-stepper.el — none).
5. **GCC:** **[open]** — no record type maps 狗's output (E-gcc-schema
   l.393). Candidates: `:memory/assert` for burials (already the shape
   written); recall/use have none.
6. **Table match:** table says "metadata on the agent reply" — the code
   writes all three fields on the **operator** record (the reply rides it
   as `:reply_text`). Mismatch of store wording, not of dataflow.

## 馬 (horse) — reply-side reading (three surfaces)

1. **Trigger:** `ma-reading` on `:ported` (after compute-ports! stores
   ports; ma_reading.clj docstring l.30–32); `ma-occurrence` on `:aligned`
   (ma_occurrence.clj:405–410, after the alignment reading); `ma-observer`
   on turn-events `:post` finalisation, per agent in ma-enabled.txt
   (ma_observer.clj:1–8). The first two run per operator turn with a reply;
   the observer only for opted-in agents.
2. **Input:** the record's acts/reply (ma-reading), `:proposed_patterns` +
   pattern files from the futon3 library (ma-occurrence), the whole exchange
   incl. narration and commits (ma-observer, scripts/ma_observe.py:1–14).
3. **Output:** `:alignment` on the record (+ `:aligned` event);
   `:pattern_verdicts` on the record (ma-occurrence, one verdict per
   proposed pattern, quote-verified); Mastra observation rows as :7073
   evidence (event "observation", tag ma-observation, keyed to the turn's
   evidence id).
4. **Readers:** scripts/turn_frames.py puts `:alignment` (l.1053),
   `:pattern_verdicts` (l.1057) and observations (l.807–816) on the
   stepper's frames; `xiang/lu_memory.clj` reads `:pattern_verdicts`
   (l.146,184) into memory edges; 驴 rerank reads earlier verdicts
   (lu_rerank.clj:71–80).
5. **GCC:** `[:pattern/occurrence 1]` — **[decided] not written** (l.391).
6. **Table match:** the table's "無門関: tension resolved/re-raised/
   dropped" is the ma-observer surface; the table omits alignment and
   occurrence (the per-turn 馬-sonnet whistle = ma-occurrence, known to
   fail "exceeds per-call budget" ~1 in 7). Table's "agent reply" ✓.

## Giraffe — batch rereads and priority, not per-turn

1. **Trigger:** NOT turn-driven. `giraffe/reread_worker.clj` is a
   one-at-a-time queue worker ("Not scheduled or started here", l.2);
   `giraffe/run.clj` `-main` composes adapter + pass + write for a given
   edges file. Relationship-review *candidates* are produced per-turn at
   settle (`turn_service` `relationship-review-evidence-entries`, using
   `xiang/relationship_review.clj`). Unconfirmed: what launches the reread
   worker on a schedule (no systemd unit or listener found in budget).
2. **Input:** reread queue + corpus (reread_worker.clj); operator text
   work-id references vs the clock (relationship_review.clj:14–30).
3. **Output:** Giraffe run directories (run.clj: one dir per run, input/
   rejects/pass output + manifest + report); relationship-review evidence
   entries at :7073; priority projection maps (priority.clj).
4. **Readers:** unconfirmed in budget — grep shows the stepper does not
   read Giraffe run dirs; the relationship-review entries' reader is the
   review flow (futon7 holes rereads). Likely written-never-read: the
   priority projection.
5. **GCC:** `[:needs 1]` + reviewer verdict as assessment —
   **[decided] not written; the needs are in Giraffe's run files**
   (E-gcc-schema l.392).
6. **Table match:** table's "馬's tensions + pattern cascade" input is the
   intent; the code's per-turn Giraffe surface is relationship-review, and
   the run pipeline is batch — the row conflates the two.

## 猴 (monkey) — operator-focus reader: mostly absent from the code

1. **Trigger:** table says "timer (seat 猴-1)". In code: 猴-1 does not
   appear in futon3c src/, scripts/, or dev/ at all (grep, 11-11). Its last
   evidence is a zai-profile turn on 2026-11-07 that died on HTTP 429
   (:7073 author=猴-1, 3 entries). The timer/wiring is **unconfirmed** —
   not found in futon3c; the mission's own note says pattern occurrence
   "moves from 馬 to 猴" as a decision, i.e. not yet built.
2–4. **Input/output/readers:** none found in code. Typed absence.
5. **GCC:** **[open]** (E-gcc-schema l.394).
6. **Table match:** the row describes an intent, not a running animal.

## 7. The dispatch boundary

- **Dispatches** (an agent asked to do work): invoke jobs —
  `POST /api/alpha/invoke` (transport/http.clj; `agency_send.py`), each
  carrying From/To/Origin; the job lands in the invoke ledger and the
  seat's transcript evidence (`tags=[:transcript …]`, author = the seat).
- **Circus machinery:** (a) turn-service listeners — 驴 `:recorded`,
  狗 `:recorded`/`:settled`, 馬 `:ported`/`:aligned`, 小象 draft at record
  time — no invoke job exists for these; they are fn calls on daemon
  threads inside the serving JVM; (b) the analysis seats 象-N and the
  驴-haiku/馬-sonnet *whistles* — these DO ride the invoke machinery
  (bounded `:ask` bells from inside listeners: ma_reading.clj l.17–21,
  lu_rerank.clj), so they appear as jobs; 狗's own queries are plain GETs,
  never jobs.
- **How a reader tells them apart in evidence:** the note
  ~/notes/wm-next/write-attribution-2026-11-07.md l.45 states the rule:
  "For a Circus seat (象-N, 猴-1, 馬, 驴, 狗) [the final round] is
  machinery". Structurally: turn records carry `:origin` ("operator" vs
  "agent", turn_record.clj:466–470); evidence carries
  `:evidence/origin {:kind :agent … :writer "agent-chat/turn"}`; a
  whistle job's caller is the listener (not a seat's own dispatch), and
  analysis-seat jobs are recorded on the turn record's
  `:analysis_dispatch` (turn_service.clj `note-dispatch!`).

## Closing list

**Mismatches with the mission table:**
1. 驴 row says "post-hook"; the rerank runs at `:recorded`, before the
   reply (lu_rerank.clj:301 "The :recorded event is raised … before the
   小象 draft, so 驴 works while 狗 does").
2. 狗 row says output is "metadata on the agent reply"; all three fields
   live on the operator record.
3. 馬 row names only the 無門関 tensions; the code has three surfaces
   (alignment, occurrence verdicts, Mastra observations).
4. Giraffe's row conflates the per-turn relationship-review candidates
   with the batch reread/priority pipeline.
5. 猴's row describes a timer-driven reader that does not exist in code;
   its last sign of life is 11-07.

**Written-never-read outputs:**
- 狗's `:dog_recall`/`:dog_use`/`:dog_buried` have no stepper or Emacs
  reader (only the agent's Prompt line and scripts/dog_helps_report.py).
- Giraffe's priority projection: no reader found (unconfirmed-negative,
  one grep pass).
- (For contrast: `:alignment`, `:pattern_verdicts`, observations all reach
  turn_frames.py; `:lu_ranked` reaches 狗 and the record.)

**Animals not running on every operator turn:**
- Giraffe (batch/not scheduled per turn).
- 猴 (no live wiring found; dormant since 11-07).
- 馬-observer (opt-in agents only, ma-enabled.txt).
- 象 skips turns that routine-draft settles or are declared/not-requested.
- 驴 and 狗 skip when their off-switch files exist, and 狗 skips analysis
  seats; 馬-reading needs a reply + computed ports first.
