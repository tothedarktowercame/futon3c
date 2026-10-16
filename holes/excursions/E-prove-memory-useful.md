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

Nothing fills `frame.memory` yet. That is pieces 2 and 3.

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
| G8 | the live page cannot be reached from the zone site | The zone site (`/var/www/zone.hyperreal.enterprises/wip/`) serves files only; `web/scopes/` is published there with fixtures, and `web/stepper/` will be. Live mode needs the browser to reach :7070. Options: a Caddy `handle_path /wip/api/*` → `127.0.0.1:7070` block (system config, Joe), or opening the page on the box with `?base=http://localhost:7070`. Decision: Joe. Until then the published page runs on fixtures and says so in its header. |

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
| 2 | G2 frames route + stepper page | claude-16 (route), codex-23 (page) | route written, test running; page in flight |
| 3 | G3 memory layer in frames | next packet | not started |
| 4 | G5 Claude/Codex tool trace rows | next packet | not started |
| 5 | G4 injection for Zai seats | packet after 3 and 4 | not started |
| 6 | G6 task bank, baselines first | packet; needs Zai or Kimi quota | not started |
| 7 | G7 derivation | packet | not started |
| 8 | G8 reach | Joe | open |

## Cross-links

- [E-cool-memories](E-cool-memories.md): the store, its laws, the Lean checks.
- [E-scope-view](E-scope-view.md): the view and the `reached` / `echoed` layer; `web/scopes/useful.html`.
- `holes/labs/M-象-2000/LAB-2026-10-14-live-memory-check.md`: the stage-by-stage check that found the delivery and brief gaps.
- `holes/labs/M-象-2000/P23-frontend-port.md`: the first 象 frontend port (turn routes, `packages/xiang-client`); this excursion ports the stepper the same way, server routes first, a pure consumer second.
- `emacs/turn-stepper.el`: the display being ported; `scripts/turn_frames.py`: the frames.
- M-象-2000: the two queries this excursion's G7 serves.
