# E-prove-memory-useful: making the memory's usefulness something you can see live

Date: 2026-10-16. Owner: claude-16. Status: building, piece by piece (table at
the end). Joe (2026-10-16): "start by porting the 象 stepper display to
TypeScript/HTML and address the gaps that are needed to offer proof points
live."

## Where this comes from

minus40 (40ºBelowDB) keeps every version of the agents' memory files with
the time each was true, and answers as-of, history and open-contradiction
questions that the flat memory directory cannot ([E-cool-memories], the
10-14 lab note `holes/labs/M-象-2000/LAB-2026-10-14-live-memory-check.md`).
The scope view ([E-scope-view], `web/scopes/`) draws, per turn, which
memories *reached* a seat: the memory's name appears in the result of a
recorded tool call, so its text entered the seat's context at that round.
`web/scopes/useful.html` follows one memory through four dates.

What none of that shows is that reaching a memory changed an action. The
one live pair run on 2026-10-16 (a seat with the store and a seat without,
on the stash task) came out null: both avoided the stash anyway. So the
honest position at the start of this excursion is: **the store is correct;
its usefulness is unproved; and its use is opt-in and rare.**

## What "useful" has to mean here

Three checkable things, in the order they can be checked.

1. **Delivery.** The relevant memory is in the seat's context before its
   first round, put there by the harness and not by the seat's choice, and
   the turn record carries a `reached` (or `injected`) edge saying so.
2. **Effect.** On a task where the uninformed seat fails, the informed seat
   succeeds, and the memory is the deciding fact. A pair where both pass
   proves nothing; a task bank is only as good as its failing baselines.
3. **Absence.** When nothing reached, the record says so in words ("no
   recorded memory call for this turn") rather than by a blank. The store
   already does this for facts (typed absence); the delivery record must
   too.

A fourth thing is owed to M-象-2000's two queries ("in force as of T",
"derivation of R"): **derivation**. Every memory version should carry the
operator turn and act that made it, taken from the 象 record of that turn,
so the margin of a memory can show the operator act it descends from.

## What the record shows today

`scripts/memory_use_rate.py` (piece 1, done) counts, from the Zai harness's
`turn-start` and `turn-round` evidence in futon1b, how often a seat
consulted the store. Run 2026-10-16T06:55Z:

    python3 -I scripts/memory_use_rate.py --since 2026-10-14

| seat | day | turns | with tools | using minus40 | m40 calls | requisitioned (used) | unprompted |
|---|---|---|---|---|---|---|---|
| zai-1 | 2026-10-14 | 3 | 3 | 0 | 0 | 0 (0) | 0 |
| zai-1 | 2026-10-15 | 17 | 17 | 0 | 0 | 3 (0) | 0 |
| zai-1 | 2026-10-16 | 4 | 4 | 2 | 6 | 2 (2) | 0 |
| zai-2 | 2026-10-14 | 2 | 2 | 0 | 0 | 1 (0) | 0 |
| zai-2 | 2026-10-15 | 7 | 7 | 0 | 0 | 2 (0) | 0 |
| zai-2 | 2026-10-16 | 3 | 3 | 2 | 6 | 2 (2) | 0 |
| zai-3 | 2026-10-14 | 2 | 2 | 0 | 0 | 0 (0) | 0 |
| zai-3 | 2026-10-15 | 5 | 4 | 0 | 0 | 3 (0) | 0 |
| zai-4 | 2026-10-14 | 1 | 1 | 0 | 0 | 0 (0) | 0 |
| zai-4 | 2026-10-15 | 1 | 1 | 1 | 2 | 1 (1) | 0 |
| zai-5 | 2026-10-14 | 1 | 1 | 0 | 0 | 0 (0) | 0 |
| zai-5 | 2026-10-15 | 5 | 5 | 1 | 2 | 1 (1) | 0 |
| zai-5 | 2026-10-16 | 1 | 1 | 0 | 0 | 0 (0) | 0 |
| **all** | | 52 | 51 | 6 | 16 | 15 (6) | 0 |

"Requisitioned" means the prompt named a minus40 tool or the store; it is
read from the prompt text, so it says what the record can show. Every one
of the six using turns was requisitioned; nine requisitioned turns did not
use it; no turn used it unprompted. The per-turn records are in the
`--json` output (`~/notes/memory-use-rate-2026-10-16.json`).

For Claude and Codex seats run from Emacs there is no `turn-round`
evidence at all, so the script cannot count them. That is gap G5 below.

## The display: the 象 stepper, ported

The Emacs stepper (`emacs/turn-stepper.el`) shows one frame per operator
turn: SAID, OPERATORS, PARSE, PATTERNS, HAPPENED, and the open and closed
acts. It is the place a person already looks to see what a turn did, so it
is where the memory proof points go. The port has two parts.

- **A frames route.** `GET /api/alpha/xiang/frames?session=<id>&limit=<n>`
  on the futon3c JVM runs `scripts/turn_frames.py` as a helper process and
  returns `{ok, session, count, frames, elapsed-ms, warnings}`; 400 for a
  missing or odd session, 504 on the 90 s bound, 502 when the script fails
  or prints no JSON; `warnings` carries the script's stderr (it says when
  futon1b did not answer and it fell back to the files). The Emacs stepper
  runs the same script, so the two show the same frames. Each frame now
  carries `record {id, turn_id, agent_id, session_id}` when a turn record
  was joined, so a viewer can reach the scope view for that turn.
  Test: `test/futon3c/transport/xiang_frames_http_test.clj` (real processes
  for the plumbing, including one that outlives its bound; the runner
  redefined for the route's shaping).
- **The page.** `web/stepper/` (packet `PACKET-stepper-view.md`, codex-23,
  job `invoke-1792133908910-15180-cb70d3fc`): `stepper.ts` → `stepper.js`,
  `index.html`, `test_stepper.mjs`, no framework, same conventions as
  `web/scopes/`. Same sections and order as Emacs, keys `n`/`p`/`g`,
  `?session=` for live frames, the fixture `fixtures/frames-claude-16-00e653e3.json`
  (12 live frames) otherwise. One new section, **MEMORY**: the minus40 calls
  of the turn with the names they reached, anything injected before round 1,
  and, when the frame has no `memory`, the sentence "no recorded memory
  call for this turn (nothing reached it that the record can show)". The
  rewind commands are not ported (footer says "rewind: Emacs only").

Nothing fills `frame.memory` yet. That is pieces 3 and 4.

Delivered 2026-10-16T07:00Z (codex-23, 7bff1dc7f; review fix 2026-10-16,
status line says live or fixture). Reviewed by claude-16: `tsc --strict`
clean and `node web/stepper/test_stepper.mjs` green under my own run; the
constructed cases (a trailing missing frame dropped, a trailing provisional
one kept, an older missing one kept; `17m` and `<1m`; two commits in a
window; empty ports; the absence sentence; a cue spanning an emoji found by
code point) all came out as the Emacs code would have them. Not checked by
anyone yet: the page by eye in a browser (no browser on the box). Published
at `zone.hyperreal.enterprises/wip/stepper/` (fixture mode); live mode on the
box: `web/stepper/index.html?session=<id>&base=http://localhost:7070`;
on the zone site: `/wip/stepper/?agent=claude-16` (G8).
The live route answered 3 frames of claude-16's session in 6.6 s.

Joe's first look (2026-10-16 07:10Z): the demo showed the fixture's 12
frames of 10-11/10-12, opened on the oldest, while the point of a live view
is the current turn. Three causes, fixed the same hour (claude-16, no
handoff: small edits in files held in context): `turn_frames.py --limit`
kept the *oldest* N turns, so the page's default limit of 60 cut off this
morning of a 67-turn session (now the newest N, tests
`test_limit_keeps_the_newest_turns` and `..._window_ending_at_the_next_turn`);
the page opened on frame 1 (live mode now opens on the newest, `?frame=`
still overrides); and `readyFrames` dropped a trailing turn 象 had not read,
which in live mode is the turn in progress (live mode keeps it, the PARSE
section says "no reading yet — 象 has not read this turn", HAPPENED shows the
window so far). Live mode refetches every 30 s while the tab is visible
(`?poll=0` stops it); a reader on the newest frame follows the newest, one
reading back stays put, as `turn-stepper--index-after-reload` does.
`?agent=claude-16` resolves the seat's current session from the roster, so
the link in the WIP index does not go stale when the session changes. Each
refetch runs `turn_frames.py` (about 6 s of python and futon1b reads for
this session); if that shows up in the JVM's load the interval goes up.

One deviation worth knowing: the SAID heading links to the scope view for
the turn, but `web/scopes/index.html` loads fixtures only, so the link
works for the turns that have a fixture (`claude-16-turn-162` today) and
otherwise shows "Could not load scope data". Giving the scope page a live
mode over `GET /api/alpha/xiang/scopes` is a follow-up under G2.

## Joe's second look (2026-10-16 22:52Z): features, not a surface

Joe: the page shows what the Emacs stepper already showed, and nothing of
what 象 can do. He named four things he is not seeing: his turn's intents
lined up with the intents my reply addresses; Elephant-2000 rewind moves;
the connected pattern graph growing as turns are mined; patterns and
cascades stored as 3/2 categories that support inference. Checked the same
hour, each against the record:

- **Alignment**: built (commit below, same hour, no handoff). Each frame
  carries `reply`: my final answer to the turn, read as marked paragraphs,
  each lined up with the sentences whose text contains its bracketed
  target. The page's ANSWERED section shows it per sentence. On turn 196
  the two quoted brackets land on s2 and s1 and the ㊟ label on nothing;
  on turn 193 one of six brackets was a quotation and five were labels,
  which is the 4% of `xiang_bracket_census.py` seen one turn at a time.
  This is daxiang_live §9 step 1 running live.
- **Rewind**: exists in Emacs only (`r`, `R` in `turn-stepper.el`, with
  the `Agent-Session` trailers). Not on the page; the footer says so.
- **Graph growth**: not happening. `mined-pattern-graph.json` was last
  written 2026-10-01T23:18Z, and nothing runs `mined_pattern_graph.py` on
  a producing event. 象-sonnet's per-turn candidates (1,902 files, four
  for turn 197) are read into the frame's "proposed" list and go nowhere
  else: no promotion, no graph node. A rebuild to a dated sibling
  (`mined-pattern-graph.2026-10-16.json`, canonical file and its pin left
  alone) added 1,129 edges (551 next-in-session, 422 rejected-beside, 149
  co-cited, 7 why) and 10 patterns from 1,677 analyses the graph had never
  seen; giant component 734 → 794 of 1,441.
- **3/2 categories**: nothing in the record stores a pattern or cascade in
  that form. The notebook's §7 compilation is "finite and Boolean" and runs
  only there.
- Also: the Emacs stepper refreshes only on `g`; nothing refreshes it when
  a reading lands, so it and the page drift apart until the key is pressed.

The pattern-status wording was Joe's s2 exactly: "no patterns matched" read
the same whether 象 had finished or not. The page now says which.

## The gaps, each as a packet

| id | gap | packet (one behaviour, one acceptance) |
|---|---|---|
| G1 | nobody can see the use rate without counting by hand | **done**: `scripts/memory_use_rate.py`, tests in `scripts/test_memory_use_rate.py` (requisitioned vs unprompted told apart; a turn with no calls counts as a turn; string and map bodies; the regex matches the packet wording and not the word "memory") |
| G2 | the stepper has no memory section and no web form | **in flight**: route (this excursion's first commit) and `web/stepper/` (codex-23) |
| G3 | frames do not carry the memory layer | `turn_frames.py --memory`: for a frame whose session has `turn-round` evidence, add `memory {calls[{round, tool, args, reached[]}]}` using `scope_view_data.trace_for` and `memories_named` (import, do not copy); for a frame with none, leave `memory` absent so the page prints the absence sentence. Acceptance: on `zai-1`'s 2026-10-16 03:40Z turn the frame lists the two `m40_find` calls and the names the grid shows as ●; on a 10-15 turn `memory` is absent. The route passes the flag through. |
| G4 | delivery is the seat's choice | **Injection for Zai seats.** In the Zai harness, before round 1: take the incoming packet's 象 act reading (acts and pattern ids, as `turn_acts` already types them), derive find terms, run `m40_find`, and put the hits into the system turn as memory segments. Write `injected [names]` on the `turn-start` evidence and a `reached` edge per name on the turn record, so the grid draws ● for a turn with no call. Acceptance: a requisition-free review packet to a Zai seat shows ● in the grid and "injected before round 1: …" in MEMORY; the use-rate table gains an `injected` column and it is non-zero on turns with no `m40_*` call. For Claude seats the equivalent place is the `UserPromptSubmit` hook (`minus40/scripts/brief.py` is the session-start half of this and runs for one seat). |
| G5 | Claude and Codex seats leave no tool trace the record can join | `minus40/scripts/ingest_hook.py` (PostToolUse) posts, for each `m40_*` call, a `coordination` evidence row `{event "memory-call", tool, args, reached[]}` with the seat's session id and the call time. `turn_frames.py` already puts every non-operator row in the window into HAPPENED, so the stepper shows it with no further change; G3's `memory` reads these rows too. Acceptance: a claude-16 turn that calls `m40_find` shows the call in HAPPENED and in MEMORY; the use-rate script, given `--agents claude-16`, counts it. |
| G6 | effect is unmeasured | **Task bank with failing baselines.** About five tasks taken from incidents whose cause was a missing memory (the 09-09 and 09-30 stash incidents; the amend-in-shared-tree, /tmp-worktree and park-deadline incidents are candidates, see `web/scopes/packets/zai-*.md`). For each: run the baseline (no store) first; a task whose baseline passes is dropped and the drop recorded. Then the informed run. Report the pair table, task by task, with the job ids; prose only after the table. Acceptance: at least one task with a failing baseline and a passing informed run, or the finding that none of the five has a failing baseline today, which is itself the result. |
| G7 | a memory's derivation stops at the frontmatter | **Memory derivation.** Every minus40 version carries the turn id and act id it was written in, taken from the 象 record of the turn whose tool call wrote the file (the hook knows the session and the time; the record store has the turn). The scope view's margin shows, for a memory segment, the operator act it descends from (`derived-from` edge). Acceptance: `m40 history <name>` prints a turn id and act per version written since the change; the margin link opens that turn in the stepper. |
| G9 | the reply is not lined up with the turn it answers | **done** 2026-10-16 23:02Z (claude-16): `reply` layer in `turn_frames.py`, ANSWERED on the page; tests in both suites |
| G10 | the pattern graph does not grow with the mined turns | rebuild `mined_pattern_graph.py` on the producing event (a reading landing), write the delta (edges and patterns added by this turn) on the frame as a GRAPH line; promotion of 象-sonnet candidates is a separate decision for Joe |
| G11 | the Emacs stepper does not follow new readings | `session-turn-analysis.el` calls `turn-stepper-refresh` on a live stepper buffer when a reading or candidates file lands |
| G8 | the live page cannot be reached from the zone site | **done** 2026-10-16 07:19Z: `/etc/caddy/Caddyfile` forwards GET `/wip/stepper/api/alpha/xiang/*` and `/wip/stepper/api/alpha/agents` to loopback :7070 with the prefix stripped; the page and the routes share basic_auth realm `xiang` (password `~/.xiang-web-password`), under the page's own prefix so the browser sends the credential preemptively (a route outside the prefix gets a bare 401 from the host's poll-401 handler and the fetch never retries). Nothing else on :7070 is reachable: POST to the routes, `bell`, `parked` all 404. Backup `Caddyfile.before-xiang-20261016-071710`. I had marked this Joe's: it was not blocked, joe has sudo and the file carries a dozen agent-made backups. |

Pieces are small on purpose: one file or one behaviour each, a checkpoint
between discovery and implementation, as the handoff protocol asks.

## What the demo shows when the pieces are in

One page, four facts, every one a record on this host:

1. a task where the baseline seat does the wrong thing (G6, the pair table
   row);
2. the same seat with injection does the right thing (G4), and the stepper's
   MEMORY section names what was injected before round 1;
3. the reached memory on the turn record, drawn as ● in the grid and as a
   line in the stepper (G3, G5);
4. the memory's derivation pointing at the operator turn and act that made
   the rule (G7), one click away in the same stepper.

Until G6 produces a failing baseline the claim stays "correct, not shown
useful", and the page should say exactly that.

## Order of work and status

| piece | gap | owner | status (2026-10-16) |
|---|---|---|---|
| 1 | G1 use-rate script | claude-16 | done, table above |
| 2 | G2 frames route + stepper page | claude-16 (route), codex-23 (page) | done: route 550620a99 live on :7070; page 7bff1dc7f, published; by-eye check open |
| 3 | G3 memory layer in frames | next packet | not started |
| 4 | G5 Claude/Codex tool trace rows | next packet | not started |
| 5 | G4 injection for Zai seats | packet after 3 and 4 | not started |
| 6 | G6 task bank, baselines first | packet; needs Zai or Kimi quota | not started |
| 7 | G7 derivation | packet | not started |
| 8 | G8 reach | claude-16 | done: `zone.hyperreal.enterprises/wip/stepper/?agent=claude-16` is live |
| 9 | G9 reply alignment | claude-16 | done 2026-10-16 23:02Z, live on the page |
| 10 | G10 graph growth per turn | next packet | not started; today's one-off rebuild measured the backlog |
| 11 | G11 Emacs stepper follows readings | small, claude-16 | not started |

## Cross-links

- [E-cool-memories](E-cool-memories.md): the store, its laws, the Lean checks.
- [E-scope-view](E-scope-view.md): the view and the `reached` / `echoed` layer; `web/scopes/useful.html`.
- `holes/labs/M-象-2000/LAB-2026-10-14-live-memory-check.md`: the stage-by-stage check that found the delivery and brief gaps.
- `holes/labs/M-象-2000/P23-frontend-port.md`: the first 象 frontend port (turn routes, `packages/xiang-client`); this excursion ports the stepper the same way, server routes first, a pure consumer second.
- `emacs/turn-stepper.el`: the display being ported; `scripts/turn_frames.py`: the frames.
- M-象-2000: the two queries this excursion's G7 serves.
