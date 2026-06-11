# Excursion: E-crossed-bells — a bell router / making mesh conversation threads visible

**Date:** 2026-06-11
**Status:** IDENTIFY (design sketched from the agents' perspective; not built). Thinking
captured at Joe's request — "it's kind of its own separate issue."
**Repo:** futon3c — the bell/mesh path: `transport/http.clj` (handle-bell + auto-bellback),
`social/coordination_ledger.clj` (mesh edges), `agency/turn_queue.clj` (reply-routes).
**Spawned from:** M-agency-hardening crossed-bells symptom (datapoints #5). After the durable
queue + drainer v2 + caller capture made the **transport** transactional (reply-routes all
correct, per-agent serialized, pouch lock spans write+read), agents *still* report "crossed
bells." So the residual problem lives **above** transport.

## HEAD (one line)
**Crossed bells = invisible threads.** A bell carries no agent-visible request/response
correlation, so a receiving agent can't tell a NEW request from a REPLY to its own
outstanding request — and splices one onto the other. The fix isn't more transport
correctness; it's a **bell router** that makes the conversation thread visible to the agents
(a shared thread-id + `in-reply-to`, surfaced in the bell's surface contract).

## The problem from the agents' perspective
Canonical case (Joe's): Agent A is mid-invoke; B bells A; A bells B during the invoke. That
is **two distinct conversations**:
- **T1:** A asks B (A's outbound bell).
- **T2:** B asks A (B's inbound bell).

They are separate exchanges. The crossing happens *only* because the thread is invisible:
when A later receives a bell from B, all A sees is *"bell from B"* — it cannot tell whether
that is **B's answer to T1** or **B's new question (T2)**. A guesses, and sometimes splices
B's T2 question onto A's T1 thread → the non-sequitur.

Not mis-*routing* — mis-*threading*. The transport is transactional; the threading lives in
the agent's head with no help. Prior art: email solved exactly this — `Message-ID` +
`In-Reply-To` headers let clients thread. Mesh bells have no header the *agent* can see, so
every bell lands context-free. The degenerate case is the auto-bellback delivered as
`Caller: auto-bellback` with no link to the bell it answers (see datapoints).

## The bell router (design, not built)
Not a new transport — a **conversation layer** over the existing mesh-edge ledger. Three moves:
1. **Shared thread-id per request.** When A bells B, mint thread `T1`; carry it on the bell
   AND on B's eventual reply (`in-reply-to: T1`). Both agents reference the same id.
2. **Frame every delivered bell with its thread context** — extend the existing surface
   contract. Instead of just `Caller: B`, the agent sees either *"NEW request from B (thread
   T2)"* or *"B's REPLY to your bell to B (thread T1): …"*. That single distinction dissolves
   the crossing (A's reply-from-B carries T1; B's new question carries T2 — distinct threads,
   impossible to conflate).
3. **Crossing becomes a legible state, not a bug.** "A has open T1→B and B has open T2→A" is
   just two open threads between the same pair — the router can surface *"heads up: you and B
   belled each other simultaneously; these are separate threads"* so the agents reconcile with
   full information, rather than the router silently guessing.

## Whistles — the crossing-immune escape hatch (available today)
Bells cross because they're **async**: the reply comes back as a separate, later turn,
decoupled from the request (the invisible thread). A **whistle** is **synchronous** — the
caller blocks and the reply returns *in the same round-trip*. So a whistle is
**crossing-immune by construction**: request and response are atomically paired by the
blocking call. The synchrony *is* the thread; there is nothing to mis-thread.

**Operational principle (what agents should realise):** when a bell exchange is getting
crossed or confused, **drop to a whistle to reconcile** — the "stop emailing, get on a
call" move. A whistles B, blocks, and gets B's definitive answer bound to the question,
with no separate bellback turn to conflate. This works **today**, no router required: the
router fixes the async path; whistles are the always-available synchronous path. Agents
have two primitives and should pick by need:
- **Bell** — async, fire-and-forget, non-blocking; the default for handoffs. Crossing-prone
  until the router adds visible threads.
- **Whistle** — synchronous, blocking, self-correlating; the reconciliation primitive when
  you need a definitive answer without threading ambiguity.

**Caveat — do NOT both whistle simultaneously.** Bells being async can't hard-block (above);
whistles *can*. If A whistles B *and* B whistles A at once, each agent's in-flight turn
blocks on the other while the other's answer is queued behind that blocked turn — a circular
wait through the per-agent pouch locks (it unwinds only on the invoke timeout). So
whistle-reconciliation is **one-directional**: one party asks (whistles), the other answers.
"You call them" — not "both dial at once." (A standing operational rule, or a future router
nicety: when a cross is detected, nominate *one* side to whistle.)

## What exists vs what's missing
- **Exists:** `msg-id` (on every turn-queue entry's reply-route), `job-id`, the
  coordination/mesh-edge ledger (`coordination_ledger.clj`), `--from` caller capture,
  auto-bellback.
- **Missing:** (a) **correlation** — tie a reply's id back to the request that spawned it
  (`in-reply-to`); (b) **agent-visible framing** — surface the thread in the bell prompt. So
  this is largely *wiring + presentation* on the ledger, not a from-scratch system. The
  coordination ledger basically *becomes* the conversation graph (nodes = bells, edges =
  `in-reply-to`); mesh-QA could then check a thread-coherence invariant.

## The invariant the router would enforce
**No bell delivered context-free.** Every bell handed to an agent is tagged either
`:new-request` (its own thread-id) or `:reply-to <thread-id>` correlating to one of that
agent's open outbound bells. A↔B simultaneous opens are flagged as a cross, not silently
delivered.

## Design forks (for later)
- **Passive vs active.** Passive = the router only labels/threads (mint ids, frame deliveries,
  expose open-threads); the agents reason. Active = it also reconciles (serialize an A↔B
  cross, merge). Lean **passive-first** — agents handle threads well once they can *see* them;
  don't have the router make conversational judgments it can't make.
- **Coherence, not liveness.** With async bells + per-agent queues, A↔B does NOT hard-deadlock
  the JVM (A doesn't block its turn on B's reply; the reply arrives as a later turn). So this
  is a *coherence* problem, not a liveness one. The router's job is threading, not unblocking.

## Relations
- **E-typed-bells** (`holes/excursions/E-typed-bells.md`) — the **child**: threading gave the
  conversation *graph*; typing the bells gives it *illocutionary semantics* (ask / answer /
  challenge / …). The `:query`/`:answer` types back onto ArSE, making the live mesh a running
  instance of the IATC model (Corneli et al. 2017). Same object, +speech-act force.
- **M-agency-hardening** (parent) — crossed bells = datapoints symptom #5. This excursion is
  the **conversation-semantics** half of the fix; the **transport** half (durable queue +
  drainer v2 + caller capture) is done.
- **E-per-turn-isolation** (`holes/excursions/E-per-turn-isolation.md`) — the **server-side**
  sibling: don't let overlapping turns stomp the shared sink/session. That is transport-layer
  isolation; THIS is conversation-layer threading. The same crossing seen from two layers.
- **Surface contracts** (futon3c CLAUDE.md) — the router is a *richer surface contract*: it
  adds THREAD context (`RE: your bell T1 to B`) to the existing surface context (`Surface:
  bell, Caller: B`). Fits the design philosophy (give agents accurate environment info; don't
  restrict capability).
- The `Caller: auto-bellback` attribution gap (datapoints) — the degenerate case; the router
  is the general fix.
