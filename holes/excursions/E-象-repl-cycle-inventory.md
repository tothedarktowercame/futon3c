# E-象-repl-cycle: inventory of the live turn loop (discovery, packet 1)
**Status:** DONE — confirmed 2026-10-29 (Giraffe closing check): Discovery inventory (packet 1) delivered in full: eight-stage table with file:line triggers, port table, join-key mis-join analysis, and live-store counts, committed read-only. Evidence: 31b6b2a7f.

Written 2026-10-24 ~20:05Z by zai-1, from the futon3c source tree at
`/home/joe/code/futon3c` (shared checkout, unmodified) and the live turn store
`~/.emacs-graph/session-turn-analysis/`. Discovery only: no code was changed.
The Lean-fidelity standard is `mathlib4/DarkTower/AIF-COMPLIANCE.md:135-144`
("the Lean `Stage`s mirror the controller's *actual* stages").

## 1. Stage table (one operator turn, in running order)

| # | Stage | Function (file:line) | Trigger | Reads | Writes | Live? Evidence |
|---|---|---|---|---|---|---|
| 1 | Record the operator turn (classical sentence split) | `session-mode--analyze-start-turn` emacs/session-turn-analysis.el:1881 (advice on `agent-chat--start-turn`); JVM side `record-turn!` src/futon3c/xiang/turn_service.clj:820; segmentation `futon3c.xiang.turn-record` `make-record` src/futon3c/xiang/turn_record.clj:428-445 | Emacs, at operator send (every ordinary operator turn) | operator text | turn record `turn-<id>.json` with `:sentences` (lexical cues, offsets), `:analysis_status "requested"`; `:quotes` sidecar; 小象 draft inline bounded 2 s (`draft!` turn_service.clj:908) | Yes. 22,737 `turn-*.json` files; e.g. `turn-vTCk0j.json` (2026-10-24T20:00:44Z, `analysis_status "analyzed"`) |
| 2 | Prompt-line render, proposed pattern set | `render!` src/futon3c/agency/prompt_line.clj:180; `write-proposed-patterns!` :148 | Every agent invoke whose body carries `:turn-record-id` (Emacs REPL builders: claude-repl.el:1067-1070, codex-repl.el:3941, zai-repl.el:414) — i.e. once per tool round of the agent's turn, not once per turn | provider registry (BM25/pattern providers) | appends to record `:proposed_patterns` [{:patterns […] :at :job_id}] — multiple renders per turn are all kept, the job id says which the agent acted on | Yes. 257 records since 2026-10-19 carry the key, 218 with a non-empty set (see §4); earliest `turn-pAGTpo.json` (2026-10-19T13:43:59Z) |
| 3 | Evaluate: agent works, replies, commits | reply capture `session-mode--note-reply-segment` session-turn-analysis.el:1666; turn end `session-mode--on-turn-finished` :1672; JVM `attach-happened!` turn_service.clj:935 (called from transport/http.clj at turn finalisation), commit filter `filter-turn-commits` :318 | reply segments stream; turn-final evidence row (`futon3c.agency.turn-events/final-turn`, agency/turn_events.clj:44) | streamed reply segments; commits seen this turn | record `:reply_text`, `:reply_at`, `:happened_summary` (classical), trailer-credited `:commits`, foreign ones `:commits_excluded` | Yes. 4,220+ records since 10-19 have `happened_summary`; key census shows `reply_text`/`reply_at` on settled records |
| 4 | Read (象 proper): analysis of the turn | `dispatch!` turn_service.clj:1034 (from Emacs `session-mode--dispatch-pending-turn` session-turn-analysis.el:1652 at reply end, or attach-happened for undispached); `publish-analysis!` turn_service.clj:1373 → `turn-store/publish-analysis!` turn_store.clj:141 | reply end (callback or end-of-turn, whichever first) | record + 小象 draft + BM25 pattern candidates (`find-pattern-candidates!` turn_service.clj:1074, stored `.patterns.json`) | `.analysis.json` beside the record (tagged `:sentences` in the proforma vocabulary, validated `tr/validate-analysis` turn_record.clj:804+); `analysis_status "analyzed"`; settled evidence row (§1 row 7); scope hyperedges (`settle-evidence!` turn_service.clj:467 → `xiang-post-hyperedge!` transport/http.clj:12009) | Yes. `analysis_status "analyzed"` on live records (e.g. `turn-vTCk0j.json`); `.analysis.json` files beside records |
| 5 | Ports (HAPPENED/DIDN'T-HAPPEN) | `compute-ports!` turn_service.clj:710 | after settle (`settle-evidence!` schedules it; also `:ported` event) | session's settled turns, reply marks, commits | record `:acts`, `:ports`, `:standings`, `:ports_status`; raises `{:event :ported :id}` | Yes. `:ports_status` state "computed" on live records; the ma-reading listener keys off it |
| 6 | Print (馬, part 1): alignment reading | `futon3c.xiang.ma-reading` listener on `:ported` (ma_reading.clj:279 `add-turn-listener!`); wired live by src/futon3c/xiang/ma_reading_live.clj (`(ma/start-live!)`), required from transport/http.clj:86 | `:ported` turn event | record's operator acts, reply marked paragraphs | record `:alignment` v1 (ma_reading.clj:260), per operator **act** verdicts `answered/noted/deferred/declined/not-answered` with verbatim-quote check; raises `:aligned` | Yes. 347 records since 10-19 have `:alignment` (§4); ma_reading_live is required by the running transport ns |
| 7 | Print (馬, part 2): observations + agent card | `futon3c.xiang.ma-observer` listener on turn-events `:post` (ma_observer.clj:57-59) → `scripts/ma_observe.py once <session>` (scripts/ma_observe.py:1-30, 53-60) | turn-final `:post` JVM event, only when the agent is in `~/code/data/minus40/ma-enabled.txt` | the whole exchange window + previous observations/card | evidence rows (author 馬, tag `ma-observation`), one per finished turn, keyed to the operator turn's evidence id; the AGENT CARD is carried in the row bodies and re-derived by consumers | Yes. ≥100 `ma-observation` evidence rows 2026-10-21→10-24 (memory_search tags ma-observation, limit 100, still full; latest e-c59e85aa 2026-10-24T19:27Z) |
| 8 | Loop: the next operator turn | stage 1 again; `:reply_to` resolution (`rt/resolve-targets`, turn_service.clj:847-858) | next operator send | `<mark>:` pointers in the new text vs the agent's recent turns | record `:reply_to {:replies […] :declared_intents […]}` — the ONLY explicit link from turn n+1 back to turn n's agent output | Yes. `reply_to` key present on records since 10-19 (key census §4); 象 tags the new turn via stage 4 as usual |

Consumers (not stages): `scripts/futon_view.py` (stepper preview: mission→pattern aggregation, latest 馬 card per session), `emacs/turn-stepper.el` (`turn-stepper--render-alignment` :380, `--render-patterns` :253, OBSERVED rows), `scripts/turn_frames.py` (frames that place 馬 observations on the turn).

Memory/minus40: the loop's durable writes are (a) `e-xiang-turn-<id>` evidence rows into the futon3c evidence store (`turn-evidence-entry` turn_service.clj:378, appended via `:evidence!`), and (b) `xiang/scope` hyperedges POSTed to the **minus40 scratch store** at `127.0.0.1:7093` (transport/http.clj:11994-12012 — deliberately, because every scope edge so far lives there). Nothing in the turn loop writes the m40 fact store automatically; m40 is reachable only as an agent tool (`m40_*` in agents/zai_api.clj:358-411, shelling to `/home/joe/code/minus40/scripts/m40_mcp.py`).

## 2. Port table

| Input a stage needs | Port | Fed today? |
|---|---|---|
| Operator turn text | stage 1 record `:source_text` (redacted copy separately held) | yes (every ordinary operator turn) |
| 象 tags of the turn | `.analysis.json` `:sentences` (+ record's classical `:sentences` pre-reading; routine turns keep only the 小象 draft, `analysis_status "drafted"`) | yes, except routine-drafted and `declared` (bell) turns, which get no 象 reading by design |
| Proposed pattern set | record `:proposed_patterns` | yes since packet 1b (257 records since 10-19); NOT available before 2026-10-19 |
| Agent reply | `:reply_text` (all streamed segments joined) | yes |
| Commits | `:happened_summary` + trailer-credited commits | yes (foreign/unsigned commits kept in `:commits_excluded`) |
| 馬 card | evidence rows (tag `ma-observation`), consumed by futon_view.py/turn_frames | yes, per enabled agent only (`ma-enabled.txt`) |
| 馬 alignment reading | record `:alignment` | yes (347 records since 10-19) |
| Per-pattern verdict (`used` / `didn't-apply` / `missed`) | **no port exists** | **not built — confirmed**: no occurrence of a per-pattern verdict vocabulary in src/, scripts/ or emacs/; 馬's verdicts are per operator act, not per proposed pattern |
| Next-turn 象 tags as outcome | implicit only: same-session ordering of records (`list-records :session-id` sorted by `created_at`, as `compute-ports!` does) + `:reply_to` | partially: no field says "this turn's tags are the outcome of turn n"; the join is positional |
| Memory write | `e-xiang-turn-*` evidence row (+ scope edges to minus40 scratch :7093) | yes for settled turns; no automatic m40 fact-store write |

## 3. Join keys and where they can attach to the wrong turn

- **Turn record id** (filename base, `turn-<id>.json`) is the spine: `:proposed_patterns` land on it via `:turn-record-id` in the invoke body; `e-xiang-turn-<id>` cites it (`:in-reply-to` the operator turn's own evidence_id); 馬 observation rows are keyed to the operator turn's evidence id; alignment/ports/acts live on the record itself.
- **job_id** inside each `proposed_patterns` entry distinguishes which render the agent acted on when a turn was shown patterns twice.
- **Session id**: first turns are recorded under a placeholder and re-pointed by `attach-happened!` (turn_service.clj:940-955) — before that lands, the turn sits in a session of its own.
- **The 10-19 open risk, confirmed in code**: `session-mode--reply-pending-path` (session-turn-analysis.el:1632) is set at turn start (:1899) and cleared ONLY in `session-mode--dispatch-pending-turn` (:1656-1658), which runs from the reply callback or end-of-turn. If neither fires (an interrupted turn, or a dispatch error before the clear — note the clear happens *before* `dispatch-analysis-after-reply`, so an error there leaves the record `requested` but the path already nil), the stale path survives into the next turn. `session-mode--pending-turn-record-id` (:1641) then hands the REPL invoke builders (claude-repl.el:1067, codex-repl.el:3941, zai-repl.el:414) the PREVIOUS turn's record id, so the new turn's `proposed_patterns` append to the old record and the new record never receives any. (The next operator send does overwrite the path at :1899, so at most one turn's patterns are mis-joined per stale path.) A second, softer mis-join: the same buffer can legitimately hold a pending path while a bell-initiated agent turn renders prompts; those renders also carry the pending id.
- **Same-second `created_at` ordering**: the Loop join (§2) sorts by `created_at`; two turns recorded in the same second in one session can swap order in the positional outcome join.

## 4. Counts from the live store (counted 2026-10-24T20:05Z; claude-11's 227/214 at ~19:45Z have grown as expected)

Command (run in `~/.emacs-graph/session-turn-analysis`; each count is one filter over it):

```sh
python3 - <<'EOF'
import json,glob,collections
recs=[d for f in glob.glob('turn-*.json')
      if isinstance(d:=json.load(open(f)),dict) and str(d.get('created_at',''))>='2026-10-19']
pp=[r for r in recs if 'proposed_patterns' in r]
ne=[r for r in pp if any(e.get('patterns') for e in r['proposed_patterns'])]
print(len(recs),len(pp),len(ne),sum(1 for r in recs if r.get('alignment')))
EOF
```

- records since 2026-10-19: **4,222**
- with `proposed_patterns`: **257**; with a non-empty set: **218**
- with a 馬 alignment reading (`:alignment`): **347**
- 馬 cards/observations (evidence store, not the record): **≥100** `ma-observation` rows 10-21→10-24 — `memory_search` tags `["ma-observation"]` limit 100, still full (exact total needs a store-side count; rows exist for every enabled-agent turn observed)
- non-empty-`proposed_patterns` records with a next record in the same session: **212**; of those whose next record is `analyzed` (i.e. 象-tagged): **188** (same script, zip over session-sorted records)

## 5. Differences from E-象-repl-cycle's four phases

1. **Read is two-stage and late**: the classical record (sentences) happens at send, but 象's reading is dispatched only when the reply ENDS (so 象 reads the turn together with what the agent did) — Read overlaps Evaluate rather than preceding it. A routine turn may settle by the 小象 draft alone (`analysis_status "drafted"`, no 象 model call), and bell turns (origin "agent") are `declared` with no reading ever.
2. **The proposed pattern set is not 象's output**: it is written by the prompt-line render during the agent's turn (packet 1b), per render, with a job id. 象's own retrieval is a separate artifact (`.patterns.json` pattern candidates). The excursion's "Read … retrieves a pattern set" is implemented as the render-time BM25 providers, not the 象 seat.
3. **Print is two distinct 馬 outputs**: (a) the alignment reading — per operator act verdicts with a verbatim-quote check — and (b) ma_observe observations + agent card, written as evidence rows only for agents in `ma-enabled.txt`. Neither produces the excursion's per-pattern `used / didn't-apply / missed` verdict; that port does not exist (confirmed by search).
4. **Loop has no explicit outcome join**: the next turn's 象 tags are not attached to the previous row; the join is positional (same session, created_at order), plus `:reply_to` which links a new turn to the agent replies it addresses, which is close to but not the "outcome of row n" relation.
5. **Memory writes exist but are partial**: settled turns become `e-xiang-turn-*` evidence rows and scope hyperedges (to the minus40 *scratch* store, not the m40 fact store); nothing in the loop writes m40 memories automatically.
6. **Proposed-pattern coverage starts 2026-10-19**: earlier turns have no `proposed_patterns`, so any diagram page over history is bounded by that date.

## 6. Corrections found while writing the Lean (claude-14, 2026-10-24; folded in by claude-11)

The tables above are left as zai-1 wrote them. `mathlib4/DarkTower/XiangMaLoopExample.lean` (darktower `ac63df59fb`) cites the corrected lines.

- Line drift: `write-proposed-patterns!` prompt_line.clj:150 (not :148), `render!` :181 (not :180); `session-mode--note-reply-segment` session-turn-analysis.el:1662 (not :1666), `session-mode--on-turn-finished` :1681 (not :1672); `final-turn` turn_events.clj:47 (not :44); `settle-evidence!` turn_service.clj:472 (not :467); the ma_observer listener ma_observer.clj:54-59 (not :57-59); the `:reply_to` block turn_service.clj:845-858 (not :847-858).
- Row 2, "once per tool round": wrong for claude-repl. `--call-claude-streaming-with-retry` makes one `invoke-stream` per operator send, plus retries, and the record id goes into that one body. Not checked for codex-repl or zai-repl.
- Row 5 trigger: `compute-ports!` runs promptly only when `:happened_summary` is already present (turn_service.clj:500-504). Otherwise the evidence write waits `:settle-evidence-delay` (30 min). Not traced: whether ports run on that delayed path.
- 馬 card port: graded fed above, but it is fed only for agents in `~/code/data/minus40/ma-enabled.txt`, so it is partial in the same way as the 象 tags. claude-11 was added to that file on 2026-10-24.
- §3, the stale `reply-pending-path` risk (claude-11's reading of session-turn-analysis.el:1880-1899): an ordinary operator send resets the path before the agent renders, so a stale path cannot reach the next operator turn. The mis-join needs either `record-turn` throwing inside the `condition-case` (the old path is never replaced) or a turn that is not an ordinary operator turn (park-resume, bell, work-order nudge) rendering while the old path is still set.
