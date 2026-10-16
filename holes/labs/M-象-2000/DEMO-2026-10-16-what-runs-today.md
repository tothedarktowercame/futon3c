# DEMO 2026-10-16 — what runs today, with the commands and this morning's output

claude-16, 2026-10-16 05:40–06:10Z. Joe: "I'd like to see some actual demos of
the M-象-2000 and Minus40°DB work working. Supposedly we were mining scopes
'like futon6' and I have yet to see that. I have yet to see any evidence of
anything useful done at all with Minus40°DB in fact, and minimal evidence with
M-象-2000."

Every command below was run this morning against the live stores and the
output is pasted as it came back (trimmed to a few lines). Each one is a thing
you can run yourself, from a shell or from Emacs. The last section is what did
not work and what I did about it.

Stores: minus40 at :7093 (`minus40-store.service`, up since 10-14 17:40Z);
象 turn records under `~/.emacs-graph/session-turn-analysis/` (15,870 files);
Agency routes at :7070.

## A. Minus40°DB (40ºBelowDB)

All from `~/code/minus40`. Each took under 2 s except `near` (5 s, model load).

### A1. What did memory say on a date?

    python3 -I scripts/m40.py asof 2026-09-24 zai exhausted

    As of 2026-09-24, 2 facts in 2 memories matching zai exhausted:
    wm-closure-strategy-zai-owners  said 2026-09-16: "... Codex nearly exhausted ..."
    zai-seat-allocation             said 2026-09-15: Quota addendum ... the zai seats share one upstream API quota ...

### A2. How did a lesson evolve? (said / withdrawn / said again)

    python3 -I scripts/m40.py history zai window

    18 events for zai window (+ said, - stopped being said, ↺ said again):
    2026-09-06 + subscription-usage-endpoints  **Zai** — GET .../usage/quota/limit ...
    2026-09-27 + proof2-dispatch-pool          Quota facts observed 2026-09-24 ...
    2026-10-12 + zai-five-hour-window          Zai's binding quota is the five-hour token window ...
    2026-10-12 - zai-five-hour-window          (same three lines withdrawn the same day)
    2026-10-12 + zai-five-hour-window          "Zai has a five-hour token window AND a weekly/monthly limit ..." (merged)
    2026-10-15 + minus40-rollout-vision        Seat quota, 2026-10-15 evening: Kimi ... hit its MONTHLY limit (HTTP 403, 19:11Z) ...

The last line is the fact that explains section D below.

### A3. What was SO on a date (not what was said)?

    python3 -I scripts/m40.py state 2026-10-16 encryption

    [2026-10-15] On 2026-10-15 Joe said "let's wait on encryption until we have proved more capabilities" ...
    [2026-10-15] **Why:** proving what the memory can do ... ranks above hardening it.
    [2026-09-14] When counting recipients per entry, match the **encryption subkey** keyid ...

### A4. Full-text find, grouped by memory, with version counts

    python3 -I scripts/m40.py find "scope view"

    4 memories match scope view (index as of 2026-10-16T04:46:24Z; 38 hits checked, 33 not memories):
    lean-layer-ownership  minus40-memory-store (2 versions)  marimo-zone-test-instance  scope-view-reached-not-near

### A5. Two memories that contradict each other (18 open)

    python3 -I scripts/m40.py ... or the MCP tool `sorry_list` from any seat

    18 open sorries; 36 memories challenged
    hx-m40-sorry-25d0e53ff84852d2afc1
      A voxterm/no-slow-test-reruns-background-sweep-warrants   "checks consume warrants and never rerun"
      B voxterm/warrant-checks-instantaneous-small-files        "a lookup that finds no local warrant is 'a signal to rerun the test'"
      why: Note A says warrant checks never rerun a test, but Note B, a later ruling, makes a lookup miss trigger a rerun ...

Both quotes are verbatim from the files (the checker verifies that before it
writes the edge). Several of these are real disagreements in what I have been
told to do, which no flat memory directory would ever surface.

### A6. Rank facts by meaning

    ~/code/futon3a/.venv/bin/python -I scripts/m40.py near "mining scopes from operator turns like futon6"

    0.4719  [2026-09-23]  operator-turns-as-pattern-language  Joe speaks; the agent translates each turn into a cascade of futon3 library pattern ids ...
    0.4457  [2026-10-05]  wm-spec-trail-read-before-touching-cascade-code ...
    0.4404  [2026-10-05]  pattern-learning-lives-in-the-connected-pattern-graph ...

(The system `python3` lacks sentence-transformers; the futon3a venv has it.
`m40.py near` under plain python3 fails with a ModuleNotFoundError.)

### A7. It is in the loop, not beside it

- Every Claude seat on this host has the `m40_*` MCP tools (asof, state,
  history, find, near, sorry_list, sorry_discharge, memory_assert). I called
  `m40_state` and `sorry_list` from this seat during this turn.
- `~/.claude/settings.json` runs `minus40/scripts/ingest_hook.py` on memory
  writes and `brief.py` at session start/resume/compact. The hook log shows
  three memory versions ingested between 03:46Z and 04:05Z today; the brief
  for this session is `~/code/data/minus40/briefs/claude-16-00e653e3-…-resume.md`.
- In Emacs: `(load-file "~/code/minus40/emacs/m40-walkthrough.el")` then
  `M-x m40-walkthrough` runs the nine-step scripted demo with a pause between
  steps. Ten records under `~/code/data/minus40/walkthroughs/`; the 10-15 17:05Z
  run (your Emacs) shows steps 1–7 ok.

## B. M-象-2000: the conversation side

### B1. Scope edges on your own turns, live (the "futon6-style" scopes)

    curl 'localhost:7070/api/alpha/xiang/scopes?session=00e653e3-28f7-4add-8304-47ca0475b5c2&turn=latest'

    {"ok": true, "turn": "claude-16-turn-168", "rules-version": "2026-10-15.2"}
    ACT claude-16-turn-168-f-s1-0  approve     "That's fine"
    ACT claude-16-turn-168-f-s1-1  ask-action  "please look into the Zai changes"
    ACT claude-16-turn-168-f-s2-0  propose     "You could in fact bell things out to Zai helpers so that we gather evi…"
    ACT claude-16-turn-168-r-1     report      "("look into the Zai changes") The Zai harness persists every tool roun…"
    EDGE claude-16-turn-168-f-s1-1 within claude-16-turn-168-f-s1-0   rule R1  (the ask sits inside the approval)
    EDGE claude-16-turn-168-f-s2-0 within claude-16-turn-168-f-s1-1   rule R3  (the proposal sits inside the ask)
    EDGE claude-16-turn-168-r-0..4 within claude-16-turn-168-f-s2-0   rule R3  (every reply paragraph sits inside the proposal)

These are the classical rules R1/R3/R4/R5 distilled from the Zai labelling
(section C), applied by `scope_rules.clj` when a turn settles. The same edges
are what `xiang-scope-overlay.el` paints in your REPL buffer (loaded in your
Emacs now: `(featurep 'xiang-scope-overlay)` → t) and what the ledger page
lists, one line per operator turn:
https://zone.hyperreal.enterprises/wip/turns/index.html (rebuilt every
minute; your turn this morning is the second line).

### B2. Where you stand in a mission, in one second

    python3 -I holes/labs/M-象-2000/brief/resume_brief.py --mission M-象-2000 --since 2026-10-13

    Read 24 records in 8 sessions, newest 2026-10-15.
    ## Seed (3)                       — your three anchor turns, verbatim
    ## Awaiting you (0)
    ## Awaiting other agents (1)      — claude-11's mfuton preamble, 7 d
    ## Asked by the operator, no answer linked (1)
    - ask-action: @proof-scribe please produce the final frozen summary of t94A01 … — 2026-10-14
      within: explain "[discharge | m0029]"
    ## Recently closed (100)          — each requisition with the job that closed it, e.g.
    - Requisition: M-象-2000 — scope_ledger.py … 2026-10-15 (inferred: job done 2026-10-15T19:53Z)

    real 0m0.997s

### B3. The McCarthy checks on this session

    python3 -I scripts/xiang_acceptance.py 00e653e3-28f7-4add-8304-47ca0475b5c2

    FAIL     R17 recording is a side effect of acting  (1 failing: 2026-10-15T23:14:02Z)
    PASS     R18 the past is well ordered
    PASS     R16/R29 outputs are read as acts
    PASS     R3/R4 records are true (commits exist)
    PASS     R6/R19 every promise is accounted for
    PENDING  R19 each turn can be rewound to its start  (109 failing; re-arms when turn-commits rows carry start-heads)
    open promises at the end of the session: 0

### B4. The mission as a dependency graph with a review table

    python3 -I holes/labs/M-象-2000/graph/build_mission_graph.py --mission M-象-2000 --since 2026-10-10 --out <file>.html

    records 358  nodes 1246  edges 676
    answers/closes 178  answers/keeps 173  scope/within 307  port/close 18
    labelled 635  kernel 18  labelled+kernel (agreed) 23  cycles 0   (1.1 s)

Published: https://zone.hyperreal.enterprises/wip/scopes/mission-graph-M-xiang-2000.html
(pan/drag SVG, legend, and the edge review table: id, type, source, target,
provenance, record, quote, verdict). The "agreed 23" is the only measurement
so far of the kernel against the labellers: of the kernel's 18 port closes,
23 labelled edges coincide with one.

## C. The scope mining: 1,124 turns labelled, now in a store

What exists on disk from the Zai campaign of 10-11 to 10-14
(`holes/labs/M-象-2000/data/moves-scale*/`), counted this morning:

| records | acts | answer incidences | closes / keeps | `within` scopes | scope openers | distinct moves |
|---|---|---|---|---|---|---|
| 1,124 | 5,421 | 2,162 | 1,114 / 1,048 | 2,126 | 494 | 581 |

Until this morning those were JSON sidecars and nothing else. The carrier
(`moves/carrier.py`, accepted 10-14 with a two-edge test) had never been run
over the set: :7073 had 0 `xiang/scope` edges and :7093 had 2. Run now:

    python3 -I holes/labs/M-象-2000/moves/carrier.py --apply      # ~3 min
    posted: 4170  no-op: 0  failed: 0

So :7093 now holds the labelled incidences as `xiang/answers` edges and the
labelled scopes as `xiang/scope` edges, each with the labeller's quote, move,
effect, confidence and shard, e.g.

    GET :7093/api/alpha/hyperedge/hx-xiang-answers-6280bc5b5cfa2407776e
    endpoints [claude-17-turn-459-r-1  claude-17-turn-459-f-s1-1]
    effect closes  move explain/decline-alternative  confidence high  labeller zai-1
    quote "I considered Peirce's categories, separating brute encounter from habit or law"

This is the first time the mined scopes are queryable rather than files, and
it is what the resume brief and the mission graph should be reading instead
of globbing sidecars (they still glob; that change is not made).

## D. The memory layer over a session (Zai seats)

    python3 -I scripts/scope_view_over_time.py --agent zai-1 --session zai-c02b13ca-… --md

    Session zai-c02b13ca (zai-1): 25 turns, 2 with minus40 calls, 13 memories ever named.
    | turn (created)       | calls | c1 | c2 | … | c8 | c9 | c10 | c11 | c12 | c13 |
    | 437a4a50 10-16T03:40 | 3     | ·  | ·  | … | ●  | ·  | ●   | ●   | ●   | ·   |
    | 6f64d143 10-16T03:53 | 3     | ◉  | ◉  | … | ·  | ●  | ·   | ·   | ·   | ○   |
    ○ retrieved-only  ● reached (statements entered the context)  ◉ echoed (the reply repeats them)

Twenty-three of the seat's twenty-five turns never consulted memory; the two
that did were the two checks I belled out after your turn 168. The page form
is now published with its fixtures:
https://zone.hyperreal.enterprises/wip/scopes/index.html?session=zai-1 (and
`?session=zai-2`, `?turn=mark7-0708.2185`, `?turn=analysis-turn-OfO380`;
until this morning only one fixture was on the server, so the grid and the
mark7 views 404'd there).

## E. What is not working, found while doing this

1. **Your turns since 2026-10-15 19:11Z are not being interpreted.** The
   ledger shows your last three turns as "not yet interpreted"; the records
   carry `analysis_status: failed` with Kimi's HTTP 403 "monthly usage
   limit". The analysis pool in your Emacs is the four 象-N seats, all
   Kimi-backed. So B1 works on turn 168 (analysed 10-15) and nothing newer.
   Zai's five-hour window is at 1% and its month at 44%. The fix is to point
   `session-mode-analysis-pool` at Zai seats, or to let the pool fall through
   to one when Kimi answers 403; see the status report for what was done.
2. **Nothing reads the mined edges yet.** The brief and the graph still glob
   sidecars (section C).
3. **`m40.py near` needs the futon3a venv** (A6); the walkthrough step that
   calls it uses plain python3 and will fail until the step is changed.
4. **The hyperedge list route's cursor does not advance** on :7093 (every
   page returns the same 1,000 ids), so counting edges by paging is not
   possible; the carrier's own count is the record.
