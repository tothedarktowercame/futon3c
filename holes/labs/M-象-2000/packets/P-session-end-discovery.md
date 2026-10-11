# Packet: where does a session's end become observable? (discovery for HOLE 18)

From claude-16 (owner, M-象-2000), 2026-10-11. Mode: work. Discovery only:
read, probe with GETs, write a note. No code changes, no store writes.

## Question

The inbox (minus40 SPEC-open-items §4.1, §5) needs three states for a port
whose session has ended: the agent's own ports **lapse**; the operator's ports
become **orphaned** (the session had marked replies, none answered the port)
or **unread** (the session's replies carried no marks). Today minus40's fault
intake decides these from the Agency registry. The kernel
(`src/futon3c/logic/xiang.clj`) already has `:lapse :closes` as a kind and
nothing that emits it. Find out what a **session-end act** would be built from.

1. **Where is a session's end observable in futon3c?** Read
   `src/futon3c/agency/registry*.clj` and the http routes that change an
   agent's `:agent/session-id` (`reset-session!`, compact, deregister, the
   `/agents/:id/...` POSTs). For each: what is written, where (registry store?
   evidence?), with what timestamp, and whether the previous session id is
   kept anywhere. Probe the live roster (`GET /api/alpha/agents`) for the
   fields that exist today.
2. **Is there an event already?** Check whether a session change is written as
   evidence or to any log that a consumer could read without polling.
3. **The act shape.** Propose, as EDN, the act the turn service would add to
   the session's act vector when its session ends: kind, author, `:at`, what it
   targets (the session? every open port?), and how `turn-ports`/`openo` would
   read it so that agent-authored ports close with effect `:lapse` and
   operator-authored ports stay open but carry an `:orphaned`/`:unread` mark.
   Say which of these the kernel can decide from facts it has (acts) and which
   need a new fact (a per-session count of marked reply paragraphs).
4. **Who emits it.** Options: the registry on session change calls the turn
   service; the turn service polls the registry; minus40's fault intake keeps
   emitting and 象 reads its edge. Give one recommendation with the reason.

## Output

`holes/labs/M-象-2000/LAB-2026-10-11-session-end-discovery.md`: findings for
1–2 with file:line references and probe outputs; the EDN proposal for 3; the
recommendation for 4; open questions. Commit only that file; never `-a`. Bell
claude-16 back with the sha and the recommendation in two sentences.
