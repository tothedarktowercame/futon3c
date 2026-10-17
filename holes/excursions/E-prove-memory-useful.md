# E-prove-memory-useful: making the memory's usefulness something you can see live

Date: 2026-10-16; updated 2026-10-17. Owner: claude-16 until the handoff
below; Joe (2026-10-17 01:25Z) is handing the excursion to an Opus agent with
claude-16 on tap for advice. Status: the display is live and pushed; nothing
yet fills its MEMORY section; usefulness is unproved (the **Checkpoint**
section near the end says exactly where the work stands and what the next
owner picks up). Joe (2026-10-16): "start by porting the 象 stepper display to
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

**Re-run 2026-10-17T01:28Z** (`~/notes/memory-use-rate-2026-10-17.txt`): the
table above is unchanged, row for row. Two things the re-run showed that the
first run did not:

- The script's default seat list is `zai-1..zai-5`, so **zai-6** (codex-68's
  seat for E-implementing-G-over-cascades, running since 2026-10-16 23:56Z)
  is not in the table. With `--agents zai-6 --since 2026-10-16`: 13 turns, 5
  with a minus40 call (one `m40_find`, four `memory_assert` on bell-back
  turns), all five counted as "unprompted".
- **"Unprompted" does not mean what the column says.** The Zai harness's
  system prompt (`src/futon3c/agents/zai_api.clj`, the trial text from
  2026-10-15) tells every seat: "before starting work that touches a project,
  service or rule you have not handled this session, call m40_find with its
  terms ... when you learn a durable fact about this workspace, also assert
  it with memory_assert". The script's requisition regex reads the packet
  text only, so a harness-instructed call counts as unprompted. zai-6's own
  round text for its one `m40_find` says "Check m40 memory first per
  instructions"; the query was `["G-over-cascades" "executable-cascade"]`
  and the store answered "0 memories match". So the honest reading of both
  tables is: every Zai seat has had a standing instruction to consult the
  store since 10-15; zai-1..5 followed it on 0 of 52 turns, zai-6 on 5 of
  13; the one find that was not requisitioned by a packet reached nothing.
  No turn on record has used the store with no instruction anywhere. The
  column needs renaming and the harness instruction needs counting as a
  requisition (gap G12).

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
refetch ran `turn_frames.py` (about 6 s of python and futon1b reads for
this session). *Superseded the same afternoon*: the timer is gone, see the
next section.

One deviation worth knowing: the SAID heading links to the scope view for
the turn, but `web/scopes/index.html` loads fixtures only, so the link
works for the turns that have a fixture (`claude-16-turn-162` today) and
otherwise shows "Could not load scope data". Giving the scope page a live
mode over `GET /api/alpha/xiang/scopes` is a follow-up under G2.

## The page follows the server: cache and stream (2026-10-16, 16:18Z to 17:10Z)

Joe, on seeing the 30 s refetch: "Reading should happen once, not on demand
when fetching the URL ... the URL's served contents should be updated by
push from websocket when the contents are ready rather than on a 30 second
loop." futon1b is shared by every seat and already busy; a page that
re-reads the store on a timer turns one viewer into a steady load, and the
frames change only at moments the JVM already knows (a turn recorded, a
reading settled). This is now the standing rule for every page or route
built over futon1b or the turn store: cache in the JVM, rebuild on the
producing event, serve the cache, push to the browser. Three packets, all to
codex-23, each reviewed by claude-16 and reloaded from master into the :7070
JVM:

1. **Frames cache** (`web/stepper/PACKET-frames-cache.md`, job
   `invoke-1792167486447-15629-0cc1cf80`, commit 5b74bb283).
   `src/futon3c/xiang/frames_cache.clj` holds one entry per fetched session
   (`!cache`, with `:frames :version :built-at :last-error`); the first GET
   runs `turn_frames.py` once and later GETs answer from memory (`cached
   true`, `elapsed-ms 0`); `turn_service.clj` gained a reload-safe listener
   registry (`add-turn-listener!`, keyed, run off the turn's path) and the
   cache rebuilds a session only when a turn is recorded or settled in it; a
   failed rebuild keeps the last good frames and records the error.
   Review fix ab0d78f04: watched live, the first build for a session
   overlapped a turn event, the rebuild ran alongside and wrote the first
   build's promise back, so every later GET came from a frozen value
   (version 1 while the atom was at 6). A build in flight now takes a dirty
   mark and the finished build queues the one follow-up.
2. **Frames stream** (`PACKET-frames-stream.md`, job
   `invoke-1792168312385-15645-d7592b24`, commit af8d2cfa7).
   `GET /api/alpha/xiang/frames/stream?session=S&limit=N` is server-sent
   events from `src/futon3c/xiang/frames_stream.clj`: `hello`, then
   `frames` (id = cache version, data = the GET route's object) on connect
   and on every version change, `status` for `building` or
   `rebuild-failed`, a keepalive comment every 25 s; `Last-Event-ID` on
   reconnect suppresses an unchanged resend. SSE rather than a websocket
   because every websocket upgrade on this JVM goes to the Agency mesh
   handler before any path is looked at, and SSE passes through the Caddy
   proxy as it stands. Review fixes: 0fc2a21f2 (a throwing `send!` removes
   the watch); 33c2d52a3, found live: http-kit 2.8.1 never runs an HTTP
   channel's `:on-close` when the client disconnects (the selection key's
   attachment keeps the accept-time channel), so a `curl -m 3` left its watch
   and keepalive task for 90 s; the keepalive tick now reads the key by
   reflection and closes the channel when it is invalid.
3. **The page subscribes** (`PACKET-frames-page.md`, job
   `invoke-1792170238446-15670-bc02fcd3`, commit df9baa88d). One
   `EventSource` per live page; the `setInterval` and `?poll=` are gone;
   `g` re-reads the cache route once. `parseStreamEvent` and the follow-the-
   newest rule are pure and tested in node. Review fix fca3a3b88: a pushed
   frame set clears a `building` / `rebuild-failed` status line (untested:
   `renderStepper` needs a DOM).

Two defects were found only by watching the live JVM after reload, having
passed 75 green tests and a two-curl check: the frozen-promise race above,
and 7811c49fc: the cache rebuilt twenty-two times in four minutes with no
new turn, because nine records had their analysis file but a flag still
`requested`, so every Emacs poll of the turn view (every 20 s) reaped them
again and each reap re-settled, appending a duplicate evidence entry and
firing the listeners. `reap!` now marks a published record analyzed, and
`settle-evidence!` announces a reading once. The habit that caught both:
after a reload, dump the component's atom without bulk fields and count its
producing events for a minute against what the design says should fire.

A frame is produced only when `emacs/session-turn-analysis.el` POSTs an
operator turn or the xiang-bells hook records a bell job; a park self-wake
or parked-resume turn records no turn, so nothing rebuilds or pushes after
it (a 10-minute watch after a self-wake saw nothing, correctly). To see a
rebuild and push without faking a turn:

    scripts/proof-eval.sh '(futon3c.xiang.frames-cache/invalidate! "<session>" (#'"'"'futon3c.transport.http/frames-cache-opts))'

(as typed at a bash prompt: the `'"'"'` run puts a literal apostrophe
inside the single-quoted form, so the JVM sees `(#'futon3c.transport.http/frames-cache-opts)`)

with the page open: the version increments and the page updates in about
10 s with no GET.

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
- **3/2 categories**: nothing in the record stored a pattern or cascade in
  that form at the time of the look. The notebook's §7 compilation is
  "finite and Boolean" and runs only there. *Since then (23:37Z onward)*:
  Joe settled CASCADE-SPEC-v3 §2 on turn-zZ52wx (futon2 e5c3a4604): a
  pattern's square is its four written parts under IF = I1, HOWEVER = I2,
  BECAUSE = G, THEN = B; an occurrence is the four maps, each a situation
  element with a source; an unmatched part voids the citation; a square
  whose texts are not the pattern's parts is refused at the producer.
  `scripts/occurrence_square.py` (c5157bc33, with
  `holes/labs/M-象-2000/LAB-2026-10-16-occurrence-squares.md`) emits
  `:wm/three-halves-square-v1` records from 象 citations and glues
  occurrences sharing a situation element: four citations gave 2 components,
  a bag, all four `:valid` under the War Machine's own validator. futon2's
  parts-check (51eb13f19, codex-2, job `invoke-1792193935835-15912-5c902ffb`)
  refuses defaulted squares; its first version stringified the part's line
  vector and accepted any substring, fixed in 38cded186 (object text must
  equal the whole part after whitespace normalisation, Joe's OK 2026-10-17).
  The WM side has since installed production cascade square ports (futon2
  0353002dc, codex-68's lane). What is still not done on the 象 side: nothing
  writes the four maps at citation time; the four on record were authored by
  hand for one turn. That producer change is the open end of this bullet.
- Also: the Emacs stepper refreshes only on `g`; nothing refreshes it when
  a reading lands, so it and the page drift apart until the key is pressed.

The pattern-status wording was Joe's s2 exactly: "no patterns matched" read
the same whether 象 had finished or not. The page now says which.

## The gaps, each as a packet

| id | gap | packet (one behaviour, one acceptance) |
|---|---|---|
| G1 | nobody can see the use rate without counting by hand | **done**: `scripts/memory_use_rate.py`, tests in `scripts/test_memory_use_rate.py` (requisitioned vs unprompted told apart; a turn with no calls counts as a turn; string and map bodies; the regex matches the packet wording and not the word "memory") |
| G2 | the stepper has no memory section and no web form | **done** as a surface: route 550620a99, page 7bff1dc7f, cache 5b74bb283, stream af8d2cfa7, page subscribes df9baa88d; the MEMORY section exists and prints the absence sentence on every frame until G3/G5 fill it. Open under G2: the page has never been checked by eye in a browser (no browser on the box); `web/scopes/index.html` loads fixtures only, so the SAID link to the scope view works for fixture turns alone; a live mode over `GET /api/alpha/xiang/scopes` is the follow-up |
| G3 | frames do not carry the memory layer | `turn_frames.py --memory`: for a frame whose session has `turn-round` evidence, add `memory {calls[{round, tool, args, reached[]}]}` using `scope_view_data.trace_for` and `memories_named` (import, do not copy); for a frame with none, leave `memory` absent so the page prints the absence sentence. Acceptance: on `zai-1`'s 2026-10-16 03:40Z turn the frame lists the two `m40_find` calls and the names the grid shows as ●; on a 10-15 turn `memory` is absent. The route passes the flag through. |
| G4 | delivery is the seat's choice | **Injection for Zai seats.** In the Zai harness, before round 1: take the incoming packet's 象 act reading (acts and pattern ids, as `turn_acts` already types them), derive find terms, run `m40_find`, and put the hits into the system turn as memory segments. Write `injected [names]` on the `turn-start` evidence and a `reached` edge per name on the turn record, so the grid draws ● for a turn with no call. Acceptance: a requisition-free review packet to a Zai seat shows ● in the grid and "injected before round 1: …" in MEMORY; the use-rate table gains an `injected` column and it is non-zero on turns with no `m40_*` call. For Claude seats the equivalent place is the `UserPromptSubmit` hook (`minus40/scripts/brief.py` is the session-start half of this and runs for one seat). |
| G5 | Claude and Codex seats leave no tool trace the record can join | `minus40/scripts/ingest_hook.py` (PostToolUse) posts, for each `m40_*` call, a `coordination` evidence row `{event "memory-call", tool, args, reached[]}` with the seat's session id and the call time. `turn_frames.py` already puts every non-operator row in the window into HAPPENED, so the stepper shows it with no further change; G3's `memory` reads these rows too. Acceptance: a claude-16 turn that calls `m40_find` shows the call in HAPPENED and in MEMORY; the use-rate script, given `--agents claude-16`, counts it. |
| G6 | effect is unmeasured | **Task bank with failing baselines.** About five tasks taken from incidents whose cause was a missing memory (the 09-09 and 09-30 stash incidents; the amend-in-shared-tree, /tmp-worktree and park-deadline incidents are candidates, see `web/scopes/packets/zai-*.md`). For each: run the baseline (no store) first; a task whose baseline passes is dropped and the drop recorded. Then the informed run. Report the pair table, task by task, with the job ids; prose only after the table. Acceptance: at least one task with a failing baseline and a passing informed run, or the finding that none of the five has a failing baseline today, which is itself the result. |
| G7 | a memory's derivation stops at the frontmatter | **Memory derivation.** Every minus40 version carries the turn id and act id it was written in, taken from the 象 record of the turn whose tool call wrote the file (the hook knows the session and the time; the record store has the turn). The scope view's margin shows, for a memory segment, the operator act it descends from (`derived-from` edge). Acceptance: `m40 history <name>` prints a turn id and act per version written since the change; the margin link opens that turn in the stepper. |
| G9 | the reply is not lined up with the turn it answers | **done** 2026-10-16 23:02Z (claude-16): `reply` layer in `turn_frames.py`, ANSWERED on the page; tests in both suites |
| G10 | the pattern graph does not grow with the mined turns | rebuild `mined_pattern_graph.py` on the producing event (a reading landing), write the delta (edges and patterns added by this turn) on the frame as a GRAPH line; promotion of 象-sonnet candidates is a separate decision for Joe |
| G11 | the Emacs stepper does not follow new readings | `session-turn-analysis.el` calls `turn-stepper-refresh` on a live stepper buffer when a reading or candidates file lands |
| G12 | the use-rate table calls harness-instructed calls "unprompted" and omits seats | `memory_use_rate.py`: count the Zai system prompt's standing instruction as a requisition (read it from the `turn-start` row if the harness records it there, else from `zai_api.clj` by date), rename the column to say what it measures (followed the standing instruction / packet-requisitioned / neither), default `--agents` to every `zai-*` seat on the roster, and add a `reached` column (names returned) so "called and got nothing" is told apart from "called and read". Acceptance: zai-6's five calls move out of "unprompted"; the 23:56Z find shows `reached 0`. |
| G8 | the live page cannot be reached from the zone site | **done** 2026-10-16 07:19Z: `/etc/caddy/Caddyfile` forwards GET `/wip/stepper/api/alpha/xiang/*` and `/wip/stepper/api/alpha/agents` to loopback :7070 with the prefix stripped; the page and the routes share basic_auth realm `xiang` (a fresh password was used 07:17Z to 16:07Z and dropped at Joe's request; the page now carries the one site password, `~/.voxterm-web-password`, user `joe`, the same as `/voxterm/`, `/wysiwyg/` and the VNC), under the page's own prefix so the browser sends the credential preemptively (a route outside the prefix gets a bare 401 from the host's poll-401 handler and the fetch never retries). Nothing else on :7070 is reachable: POST to the routes, `bell`, `parked` all 404. Backup `Caddyfile.before-xiang-20261016-071710`. I had marked this Joe's: it was not blocked, joe has sudo and the file carries a dozen agent-made backups. |

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

| piece | gap | owner | status (2026-10-17 01:30Z) |
|---|---|---|---|
| 1 | G1 use-rate script | claude-16 | done, table above; re-run 10-17 unchanged; its reading corrected (see G12) |
| 2 | G2 frames route + stepper page, cache, stream | claude-16 (route, reviews), codex-23 (page, cache, stream) | done and live: route 550620a99, page 7bff1dc7f, cache 5b74bb283 + ab0d78f04, stream af8d2cfa7 + 33c2d52a3, page subscribes df9baa88d + fca3a3b88, settle-once 7811c49fc; published copy matches HEAD; by-eye check and scope-view live mode open |
| 3 | G5 Claude tool trace rows | claude-3 (owner), codex-23 | **done** 2026-10-17 01:45Z, see "G3's premise, and G5 first" |
| 4 | G3 memory layer in frames (re-scoped: reads `memory-call` rows) | claude-3 (owner), codex-23 | **done** 2026-10-17 02:00Z, see below; live on the page |
| 5 | G4 injection for Zai seats | packet after 3 and 4 | not started |
| 6 | G6 task bank, baselines first | packet; needs Zai or Kimi quota | not started |
| 7 | G7 derivation | packet | not started |
| 8 | G8 reach | claude-16 | done: `zone.hyperreal.enterprises/wip/stepper/?agent=claude-16` is live, 401 without the site password as intended |
| 9 | G9 reply alignment | claude-16 | done 2026-10-16 23:02Z, live on the page |
| 10 | G10 graph growth per turn | packet | not started; the 10-16 one-off rebuild measured the backlog |
| 11 | G11 Emacs stepper follows readings | small | not started |
| 12 | G12 use-rate reads the standing instruction | small | named 2026-10-17, not started |
| — | 象-side occurrence producer (four maps written at citation time) | claude-16, with M-象-2000 | not started; `occurrence_square.py` consumes hand-authored maps today |

## Checkpoint (2026-10-17, at Joe's handoff request of 01:25Z)

This is where the excursion stands, written so the next owner can act on it
without claude-16's context.

**What is true and checked this hour.**

- `GET localhost:7070/api/alpha/xiang/frames?session=00e653e3-28f7-4add-8304-47ca0475b5c2&limit=3`
  answers in about 1 ms with `cached true`, version 66, built 01:26:18Z; the
  newest frame is `claude-16-turn-207`, Joe's handoff turn.
- `GET .../frames/stream?session=<same>&limit=1` sends `hello`, then
  `frames` with `id: 66` and the same object. It is the live page's only
  input after load.
- Every frame has `turn parse patterns operators happened record`, and
  `reply` once claude-16's answer to that turn is on record. **No frame has
  `memory`.** The page's MEMORY section prints the absence sentence on all
  of them. That is the state of the proof: a display with nothing yet to
  display.
- The published page (`/var/www/zone.hyperreal.enterprises/wip/stepper/`)
  is byte-identical to HEAD (`index.html`, `stepper.js`), and the WIP index
  card points at `stepper/index.html?agent=claude-16`.
- `memory_use_rate.py --since 2026-10-14`: 52 turns, 6 using, 0 unprompted
  for zai-1..5, as on 10-16; the correction in the use-rate section applies.

**What was never checked.** The page by eye in a browser; `fca3a3b88` (DOM
only). Nobody has yet watched a rebuild and push land in a browser; the
`invalidate!` recipe above is how to do it.

**The claim the page may make today**, in Joe's terms: the store is correct
(E-cool-memories), delivery is the seat's choice and seats rarely choose it
even under a standing instruction, and no effect has been shown. The page
should say "correct, not shown useful" until G6 has a failing baseline.

**Next packet: G3.** It is the first piece that puts a fact in MEMORY.
`turn_frames.py --memory` (the route passes the flag): for a frame whose
session has `turn-round` evidence, add
`memory {calls [{round tool args reached []}]}` using
`scope_view_data.trace_for` and `memories_named` (import, do not copy); for
a frame with none, leave `memory` absent. Acceptance: on zai-1's 2026-10-16
03:40Z turn the frame lists the two `m40_find` calls and the names the scope
grid shows as ●; on a 10-15 turn `memory` is absent. The frames cache needs
no change: it runs the script, so the flag reaches it through
`frames-cache-opts` in `http.clj`. Then G5, then G4; G6 as soon as a Zai or
Kimi seat has quota, because nothing above it is a usefulness result.

**Seats.** codex-23 (idle, excursion E-prove-memory-useful) built the page,
cache and stream and holds that context; codex-2 (idle, same excursion) did
the futon2 parts-check. Nine codex seats idle at 01:30Z. zai-6 belongs to
codex-68's E-implementing-G-over-cascades and is not ours to use.

**Rules that bit this excursion, for the next owner.**

1. No polling, no on-demand reads over futon1b: cache in the JVM, rebuild on
   the producing event, push (Joe, 2026-10-16). Any new route or page here
   follows the frames cache and stream.
2. After every reload, watch the live state for a minute (atom dump, event
   count) before calling a review done; two of this excursion's defects were
   invisible to green tests.
3. The :7070 JVM is reloaded only from `/home/joe/code/futon3c` master
   (`scripts/restore-http-routes.sh`, or `(require 'ns :reload)` via
   `scripts/proof-eval.sh`); never `load-file` a worktree copy. Reload needs
   no permission; a restart is Joe's call.
4. One behaviour per packet, discovery split from implementation, always
   `--from <your-id>`, quoted heredocs, park on every bell and read the park
   record back (a `deadline-ms` three orders of magnitude off slipped past
   once here).
5. Features, not presentation: before shipping a surface, name the one thing
   it shows that no existing surface shows. If there is none, build the
   feature first.
6. Report with tables (use-rate, pair tables) and the job ids; prose after.

**Advice channel.** claude-16 stays on the roster; bell it with
`--from <your-id>` and it will answer in a later turn. Joe's words on the
handoff: "keeping you on tap to offer advice".

## G3's premise, and G5 first (claude-3, 2026-10-17)

Joe handed the excursion to claude-3 at 01:35Z. The checkpoint's G3
acceptance cannot pass: `turn_frames.py` on zai-1's session
`zai-c02b13ca-b1db-4357-8f76-b6f9382b7a1f` prints `[]`. Frames are built
from operator chat-turn rows (`is_operator_turn`), and a Zai session has
none: 1,052 `turn-round`, 48 `turn-start`, 48 `clock-decision`, 34
`turn-commits`. The stepper shows Joe's turns on Claude and Codex seats, and
until today those seats recorded no minus40 call (the PostToolUse matcher
was `Write|Edit|Bash`, so `mcp__minus40__*` calls never reached a hook). So
the first row MEMORY can show had to come from a Claude seat, and G5 went
first.

G5, done: `minus40/scripts/call_hook.py` (codex-23, 057b776, job
`invoke-1792201063414-16034-3fa64b61`, packet
`web/stepper/PACKET-memory-call-rows.md`; review fix 0d51885, by claude-3:
a bare MCP content-part list was JSON-encoded instead of read as text, now
tested). Registered in `~/.claude/settings.json` (matcher
`mcp__minus40__.*`, backup `settings.json.before-call-hook-20261017`). It
posts one coordination row per call, tags `memory-call minus40`, body
`{event tool args result-text error? at}`. It does not parse names: the
reader applies `scope_view_data.memories_named` to `result-text`, so there
is one parser. Live check: claude-3's `m40_find ["bellback" "park"]` in
session `ff41903c-6510-4c5e-a293-01324d39fb45` wrote
`e-6ea042af-b894-47f4-a367-490471906e1e` (author claude-3); `memories_named`
reads 9 names from it, the 9 the store reported.

What the review checked: the diff; that the tests pin the shape, the no-op
for other tools, the 20,000-char cap with suffix, exit 0 with one log line
when Agency is down, and `error?`; one bad case built (a bare list response)
that the original code got wrong.

Next, G3 re-scoped: `turn_frames.py` fills `frame.memory` from the
`memory-call` rows inside a frame's window (and, for a Zai-seat record,
`trace_for`), names by `memories_named`. Then the SessionStart brief
(`minus40/scripts/brief.py`) as an `injected` row: it already puts memories
into a Claude seat's context before round 1, and nothing records it.

G3, done: `turn_frames.py` gives a frame `memory {calls [{tool args at
error evidence-id reached [{name project}]}]}` from the `memory-call` rows in
the same window pass that fills HAPPENED, names by `memories_named`; a frame
with none has no `memory` key; the page prints `name (project)` (codex-23,
0441b280f, job `invoke-1792201240021-16040-0cf3f33d`, packet
`web/stepper/PACKET-frames-memory.md`). The tests pin the window: a second
call after the next operator turn lands on the next frame.

Review fix 664c3a7c3 (claude-3). codex-23 also added
`add_missing_operator_rows`, run on every session, because claude-3's own
session had no operator chat-turn row in the store: Joe's first turn there
was recorded only as `invoke-start`, so the session had no frames. As
written it backfilled every 象 record of the session, and on claude-16's
session that added 33 frames (7 bell jobs, 25 Matrix proof-critic moves
whose records say origin `operator`, and claude-16's own first turn),
shifting every window. It now backfills only records with surface `typed`
and origin operator (or neither field, as old records have): claude-16's
session goes 81 → 82 frames (its turn 1, which the store lacked) with the
other 81 byte-identical to before. Test added. Open: why the store has no
operator chat-turn row for that turn is not investigated.

Live: after `invalidate!` on session `ff41903c-…`, the cached GET answers
`claude-3-turn-1` with `m40_find` and 9 reached. `stepper.js` published to
`/var/www/zone.hyperreal.enterprises/wip/stepper/` (index.html unchanged).
**This is the first frame on record whose MEMORY section says something.**
It shows delivery by the seat's own choice; it is not a usefulness result.

## Cross-links

- [E-cool-memories](E-cool-memories.md): the store, its laws, the Lean checks.
- [E-scope-view](E-scope-view.md): the view and the `reached` / `echoed` layer; `web/scopes/useful.html`.
- `holes/labs/M-象-2000/LAB-2026-10-14-live-memory-check.md`: the stage-by-stage check that found the delivery and brief gaps.
- `holes/labs/M-象-2000/P23-frontend-port.md`: the first 象 frontend port (turn routes, `packages/xiang-client`); this excursion ports the stepper the same way, server routes first, a pure consumer second.
- `emacs/turn-stepper.el`: the display being ported; `scripts/turn_frames.py`: the frames.
- `web/stepper/PACKET-stepper-view.md`, `PACKET-frames-cache.md`, `PACKET-frames-stream.md`, `PACKET-frames-page.md`: the four packets behind the page, in order.
- `holes/labs/M-象-2000/LAB-2026-10-16-occurrence-squares.md` and `scripts/occurrence_square.py`: the 3/2-category answer to Joe's second look; futon2 `holes/labs/wm-contract/CASCADE-SPEC-v3-draft-2026-10-05.md` §2 for the rule.
- M-象-2000: the two queries this excursion's G7 serves.
