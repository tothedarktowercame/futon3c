# M-turns-first — operator and agent turns as the primary artifact, on every surface

**Status: IDENTIFY — Joe's decisions recorded; MAP next** (2026-09-14, claude-19).
Nothing is built under this mission yet.

**Prompt (Joe, 2026-09-14):** *"marimo will be another UI, like Emacs, IRC, or
Matrix. Agent turns and Operator turns are the primary artifacts. Both can
inject, reason about, and evaluate Python blocks."* Earlier the same evening:
*"I want an agent-first notebook"*, *"I don't want a sidebar"*, *"a model that
'just works' in which agent turns are a native part of the UI"*, and enabling it
*"should be part of the site-wide config"*.

## Why

On 2026-09-14 a marimo test instance on zone (`~/code/marimo-zone`) went through
five designs in one evening: a send form, `turn("…")` cells, a coloured prompt
box, a sidebar proposal, and the notebook itself as the chat. Each put the
conversation *inside* a code-first document. Operator turns had to be dressed as
Python, and the notebook file became the only record of the conversation, so
when a stale browser copy was saved over it, a reply was lost. The individual
failures were plumbing (a stale page, watch mode, two logins competing for one
header), but all five designs made the same choice: the conversation lived in
the notebook, not the notebook in the conversation.

## Claim

The **thread** lives in FUTON: an ordered record of operator turns and agent
turns, with Python blocks and their evaluations attached. Every UI (Emacs, IRC,
Matrix, marimo) renders the same thread and can add to it. No UI's file is the
source of truth; a UI can be closed, reloaded or swapped mid-thread and nothing
is lost.

## What already exists

- **Turns are evidence entries** with `author`, `session-id`, `in-reply-to`
  (reply-chain threading), tags (`chat`, `turn`, `user`), `claim-type` and
  `body`. Joe's Emacs turns are recorded this way today.
- **`/invoke` and `/invoke-stream`** carry a surface header and turn ids (the
  Emacs REPL); park resumes are delivered back as turns.
- **M-walkie-talkie** (DONE 2026-03-08) made evidence a write surface any agent
  can use, with `in-reply-to` threading. Its Gate B, *cross-surface parity*, was
  deferred; this mission takes up that gate for turns.
- **marimo code mode** (`marimo._code_mode`, reached through
  `POST …/api/kernel/execute`) lets an agent create, edit and run cells in a live
  kernel and read their outputs. It works on zone today (`marimo-zone/tools`).

## What is new: two layers

- **Turn layer (the REPL):** operator and agent turns in order, stored in FUTON
  as evidence entries. This is the conversation.
- **Code layer (like Pluto):** Python blocks that can be addressed, updated in
  place and re-run reactively, living in a **marimo kernel**. Turns refer to
  blocks by id: a turn can inject a block, update one, reason about one, or
  evaluate one.

1. **Block references in turns:** a turn that adds or changes a block records
   the block id, the code, and the author. The block's current code lives in the
   marimo notebook; the turn records who changed it, how, and why.
2. **Evaluation entries:** a run of a block, recording who ran it, from which
   surface, the output or error, and when. Because the code layer is reactive,
   one evaluation can re-run the blocks that depend on it; the entry records
   which blocks re-ran.
3. **One marimo kernel per thread**, reachable from every UI and from agents
   through the execute endpoint and code mode. So Emacs, IRC and Matrix
   evaluate through marimo too, and marimo's own UI is one view of the code
   layer among several.
4. **Renderers:**
   - Emacs (exists; gains block and evaluation display plus an evaluate command).
   - marimo: turns render natively and in colour, blocks are real cells, and the
     operator types turns directly, not inside Python.
   - IRC and Matrix: turns as messages, blocks as code, and a command such as
     `!run <id>` to evaluate one.

## Scope

- **In:** the turn/block/evaluation data model; one kernel type; marimo as the
  second renderer after Emacs.
- **Out, for now:** threads with several operators or several agents, and
  hosting for Charlie.

## Decisions (Joe, 2026-09-14)

1. **Execution:** the code layer is separate from the turn layer. Blocks are
   addressable and can be updated, so it behaves like Pluto (reactive), while
   turns stay in order like a REPL.
2. **Kernel:** marimo's.
3. **Agent-injected blocks:** the agent runs code *"as and when it sees fit"*.
   It doesn't wait for the operator to accept a block.
4. **Threads:** start simple, one thread per agent session as now, and get the
   UI and the interaction right before revisiting.

## Still open (for MAP)

- **A kernel with no browser attached.** marimo creates a session when a
  browser connects, and code mode needs one open. A thread's kernel must stay
  alive while the operator is using Emacs or Matrix, which means FUTON holding
  the session open itself. On 2026-09-14 a bare WebSocket probe to `/marimo/ws`
  started a kernel, so this looks possible, but it hasn't been tested as a
  working setup.
- **Native turns in marimo's UI.** marimo's cell types are Python, markdown and
  SQL, all Python underneath. Candidates, not yet compared: cells rendered by
  the thread and never typed by the operator (for example a widget for the
  operator's input); a marimo frontend extension or fork; or a separate web UI
  for turns with marimo embedded as the code view.
- **Who else runs a notebook's code.** When an agent edits a block that is also
  open in the operator's browser, both see the change. Tonight's lost reply
  showed what happens when a browser holds a stale copy.

## First-slice completion criterion

Begin one thread in Emacs and continue it in marimo:

- an operator turn in marimo injects a Python block;
- the agent reasons about the block and evaluates it;
- the evaluation appears in both UIs;
- reloading either UI loses nothing.

## Related

- The service for Charlie Danoff (Chicago Linode) is the eventual first outside
  user. p2r's rule that each participant reports only on their own work maps
  onto per-entry authors.
- The marimo experiments and their failure modes are recorded in claude-19's
  memory note `marimo-zone-test-instance`. The test instance stays up, unchanged.
