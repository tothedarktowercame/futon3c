# M-agency-hardening -- Conference-ready IRC-backed Agency

Date: 2026-06-07
Status: Proposed
Target: Before the conference session approximately one month after
2026-06-07.

## Motivation

Joe will present the IRC-backed version of Agency in a conference session in
approximately one month. The intended story is strong: humans and agents share
an IRC channel, work is routed through ordinary HTTP/WebSocket infrastructure,
and every message, invocation, review, and orchestration decision becomes typed
queryable evidence.

The risk is that the public story is broader than the current operationally
proven story. This mission hardens Agency so the session can be presented from
measured behavior rather than aspiration.

## Conference Claim

The current abstract seed is indicative and must be revised during this
mission:

> Patterns for a New Generation: AI and Agents
>
> Hosted by: Geetika Jain (Keele University) & Joe Corneli (Oxford Brookes
> University)
>
> Two humans and three AI agents share a chat channel backed by a persistent
> evidence store. Agents coordinate via @mentions - this time, Codex scopes
> tasks from mission docs and delegates to Claude via GitHub issues; Claude
> reviews and feeds back. Tickle orchestrates overnight batch work and detects
> stalls. Every message, every tool invocation, every review verdict is
> captured as typed, queryable evidence. Overnight, Tickle orchestrates 17 PR
> reviews across 85 proposals, with measurable cross-batch quality improvement.
> Joe and Rob kibbitz and talk about next steps for their research project. The
> infrastructure is ordinary (IRC + HTTP + WebSockets), but the combination --
> multiple humans, multiple AI agents, shared codebase, a complete audit trail
> -- creates a reproducibility surface that didn't previously exist. You are
> now invited to think about the implications in a workshop that uses this
> technology.

The revised abstract should preserve the workshop invitation but replace any
unproven numbers or workflow claims with evidence-backed ones.

## Existing Ground

- `README-irc.md` documents the ngircd + bridge architecture. IRC is
  intentionally independent of the futon3c JVM.
- `README-evidence.md` documents the evidence-write boundary and evidence
  sources, including IRC messages and agent invocations.
- `README-codex-code.md` documents Codex execution expectations.
- `README-tickle2.md` documents Tickle's orchestration role.
- `README-zabuton.md` documents multi-agent IRC setup and owner mapping.
- `holes/missions/M-IRC-stability.md` hardens IRC transport behavior.
- `holes/missions/M-codex-irc-execution.md` defines the guaranteed Codex IRC
  execution contract.
- `holes/missions/M-operational-readiness.md` documents the earlier unattended
  multi-agent operational readiness work.
- `holes/missions/M-tickle-overnight.md` documents Tickle overnight
  orchestration.

## Spun-off excursions

- **`holes/excursions/E-crossed-bells.md`** — the "crossed bells" symptom
  (datapoints #5) reframed as its OWN issue: a *conversation-semantics* problem,
  not transport. The durable queue + drainer v2 + caller capture made the transport
  transactional, yet agents still mis-*thread* bells — they can't tell a NEW request
  from a REPLY to their own outstanding request (the A↔B simultaneous-bell case). The
  excursion sketches a **bell router**: a shared thread-id + `in-reply-to` surfaced in
  the bell's surface contract, so the conversation thread is visible to the agents.
  Server-side sibling: `E-per-turn-isolation.md` (overlapping-turn sink/session
  isolation — the same crossing one layer down).
- **`holes/missions/M-typed-bells.md`** (was `E-typed-bells.md`, promoted to a mission
  2026-06-11) — the child of E-crossed-bells: the bell router recovered the conversation
  *graph*; typing the bells recovers its *illocutionary semantics* (ask/answer/challenge/…)
  so `:query`/`:answer` populate ArSE at birth. Grounded in Corneli et al. 2017 (IATC).
  This grew past a single-agent bounded scope-out, hence the promotion.

## Invariants

1. **IRC independence:** IRC service availability must not depend on the
   futon3c JVM. `ngircd` and the bridge remain separable from Agency runtime
   restarts.
2. **Explicit identity:** Agent identity, IRC nick, owner mapping, and live
   session identity must be inspectable. A stale or missing session cannot be
   treated as live.
3. **Typed evidence:** Public claims about messages, tool invocations, reviews,
   stalls, and batch outcomes must be backed by typed evidence entries.
4. **Execution evidence before completion:** A mission/work completion claim
   requires runtime execution evidence, not prose alone.
5. **Transport routes only:** IRC is a coordination surface. Long prompts,
   work items, and review payloads should use durable surfaces such as issues,
   HTTP jobs, or whistles; IRC should project status and links.
6. **No feedback loops:** Tickle and agents may post status, but orchestration
   must avoid self-consuming its own output as new work.
7. **Recovery is observable:** Restarts and reconnects must produce explicit
   health, registry, delivery, or evidence state. There should be no false
   online state after a dropped session.
8. **The JVM is disposable:** futon3c may execute and cache live state, but it
   must not be the sole authority for active work, session intent, or
   overnight progress. A killed JVM should pause recoverable work, not strand
   it or silently lose ownership.

## Scope

### In

1. Harden IRC-backed Agency for a live or near-live conference workshop.
2. Define and test health checks for IRC, bridge, futon3c, WS agents,
   invoke jobs, and evidence persistence.
3. Prove at least one end-to-end human -> agent -> review -> evidence loop.
4. Prove or honestly downscope Tickle overnight/batch orchestration claims.
5. Produce a recovery runbook for restart, reconnect, stale session, and
   delivery-failure scenarios.
6. Produce a query bundle that supports the claims in the revised abstract.
7. Revise the abstract against measured system behavior.
8. Make futon3c robust enough for unattended overnight runs, including kill or
   OOM recovery.

### Out

1. Replacing IRC with another chat substrate.
2. Replacing the evidence store.
3. Shipping a polished public product UI.
4. Bypassing evidence or execution gates for demo convenience.
5. Pretending batch-scale numbers are real before the evidence store supports
   them.

## Workstreams

### W1: Live Topology Hardening

Verify the conference topology:

- `ngircd` running independently.
- `ngircd-bridge` connected with expected bot nicks.
- futon3c Agency API reachable.
- WebSocket-connected agents visible in `/agency/connected` or the current
  canonical status endpoint.
- Drawbridge or equivalent lifecycle access available for controlled reloads.
- Restart procedure documented without restarting futon3c from a session routed
  through Agency.

Deliverables:

- A topology diagram or short runbook.
- A health-check command set with expected output shapes.
- A stale-session cleanup procedure.

### W2: Agent Execution Guarantees

Exercise the actual workflow the workshop will describe:

1. A human posts an IRC message.
2. The bridge accepts or rejects it with an auditable reason.
3. Codex scopes work from a mission or issue.
4. Claude reviews or feeds back through a durable review surface.
5. Terminal status is projected back to IRC.
6. Evidence proves the chain.

Deliverables:

- A small reproducible demo mission or issue.
- Job IDs or evidence IDs for each step.
- Tests or smoke scripts for accepted, done, failed, timeout, and stale-session
  paths where practical.

### W3: Evidence and Audit Claims

Define the minimum query set needed to support the public story.

Questions the evidence bundle must answer:

- Which humans and agents participated?
- Which IRC messages entered and left the channel?
- Which tool invocations or command events happened?
- Which reviews happened, and what verdict did each produce?
- Which stalls or failures were detected?
- Which claims in the abstract are directly evidenced?

Deliverables:

- A query bundle or script.
- Example output from a real run.
- A gap list for claims that are still not evidenced.

### W4: Tickle Batch / Overnight Claim

Decide whether the session should claim live Tickle batch orchestration,
historical Tickle batch orchestration, or a smaller stall-detection demo.

The current abstract seed mentions "17 PR reviews across 85 proposals" and
"measurable cross-batch quality improvement." Those claims need direct
evidence or removal.

Deliverables:

- Evidence-backed batch metrics, or a written downscope.
- Stall detection demonstration.
- Morning-report or batch-summary example if that feature is included.

### W5: futon3c Kill / Restart Robustness

Make overnight work robust to futon3c process death. The target operational
invariant is:

> `kill -9 futon3c` pauses progress, but does not lose work, hide failure, or
> strand agents behind stale session state.

Recent motivating failure:

```text
A+B INTEGRATION + C3... (session: 019d39df)
[cycle 3362] futon0-d: 1/73 files changed
[fs-watch] /home/joe/code/futon0/holes/missions/M-capability-star-map.md status=mission-doc 69ms
[invoke] claude-1 exit=0 text-len=418 err-len=0
```

When futon3c went down, work stopped. For conference-grade overnight runs,
this runtime output must be reconstructable after restart, and any active
work must be resumable or explicitly marked failed/stale.

Design requirements:

- Durable session registry: persist expected `(agent-id, session-id, lane)`
  intent outside in-JVM state. After restart, Agency must distinguish
  `connected`, `expected-but-detached`, `reattached`, `stale`, and `retired`.
- Idempotent registration: reconnecting the same `(agent-id, session-id)` must
  reattach the lane; it must not mint a mystery lane or report "no
  conversation found" without an inspectable recovery reason.
- Durable work leases: overnight jobs, cycles, delegated invocations, and
  Tickle-owned work items need durable lease records with owner, heartbeat,
  expiry, and resumability status.
- Append-only runtime log: route structured runtime events to ignored runtime
  storage, preferably `data/`, so cycle output, fs-watch events, invokes,
  stalls, and recovery decisions are replayable after restart.
- Startup recovery report: futon3c startup should emit a concise report of
  expected sessions, reattached sessions, missing sessions, expired leases,
  recoverable work, and work requiring operator action.
- External supervisor: futon3c restart should be owned by `systemd --user`, a
  shell loop, Babashka task, or equivalent process outside the live
  Agency-routed agent session. A live agent must not be responsible for
  restarting the JVM carrying its own transport.
- Recovery drill: test at least graceful restart and hard kill during an
  active overnight-style cycle.

Deliverables:

- Durable schema or EDN/XTDB document shapes for sessions, work leases, and
  runtime events.
- Startup recovery implementation or prototype with example output.
- Supervisor/runbook entry showing how futon3c is restarted and verified from
  outside Agency.
- Tests or smoke scripts for restart, hard kill, stale session, duplicate
  reattach, expired lease, and resumable lease paths.
- A captured drill showing that a killed futon3c resumes or cleanly reports
  the interrupted `cycle`/`invoke` work.

### W6: Conference Runbook

Prepare for the day of the session.

Runbook sections:

- Start order.
- Health checks.
- Demo script.
- Evidence queries to run live.
- Recovery from stale agent sessions.
- Recovery from bridge disconnect.
- Recovery from futon3c restart while IRC remains available.
- What not to do from a live Agency-routed agent session.

Deliverables:

- One operator-facing markdown file.
- One short checklist suitable for use during the session.

### W7: Abstract Revision

Revise the abstract after W1-W4 produce evidence.

Rules for the revised abstract:

- Use exact numbers only when backed by queryable evidence.
- Describe failures and recovery honestly if they are part of the reproducible
  surface.
- Make the workshop invitation concrete: attendees should understand what they
  will see, query, or try.
- Preserve the ordinary-infrastructure point: IRC + HTTP + WebSockets is a
  feature, not a limitation.

Deliverables:

- Revised abstract.
- Short claim-to-evidence table.

## Acceptance Criteria

- [ ] Two humans and at least three agent roles can share the IRC channel with
      stable nick and owner mapping.
- [ ] At least one Codex -> Claude review loop completes through a durable work
      item and is visible in typed evidence.
- [ ] Tickle orchestration or stall detection is demonstrated with evidence, or
      removed from the abstract.
- [ ] A restart drill proves IRC remains usable while futon3c is restarted or
      reloaded.
- [ ] A kill drill proves active overnight-style work is either resumed from
      durable state or reported as an expired/stale lease with operator-visible
      recovery instructions.
- [ ] Reattaching the same `(agent-id, session-id)` after restart is
      idempotent, inspectable, and does not mint an unexpected lane.
- [ ] Runtime cycle/invoke/fs-watch output is written to ignored `data/`
      storage and can be replayed or summarized after JVM restart.
- [ ] Stale/missing sessions are detected and recovered without false online
      status.
- [ ] Query output supports every numeric or workflow claim retained in the
      abstract.
- [ ] The conference runbook includes start, health, demo, and recovery steps.
- [ ] The abstract is revised against measured evidence before submission or
      presentation use.

## First Cut Tasks

1. Inventory current IRC + bridge + Agency deployment on laptop and server.
2. Identify canonical health endpoints and fill gaps.
3. Create a tiny demo mission that can be safely run repeatedly.
4. Run the demo once and collect evidence IDs.
5. Write the evidence query bundle.
6. Perform stale-session and restart drills.
7. Define durable session, lease, and runtime-event records for futon3c
   recovery.
8. Add startup recovery reporting and an external supervisor/check command.
9. Perform hard-kill overnight-cycle recovery drill.
10. Decide the Tickle claim level.
11. Rewrite the abstract.

## Appendix: the no-text invoke path (checkpoint, 2026-06-11)

**Symptom.** Operator saw `[Claude used tools but produced no text response]` spam
across surfaces — agents appeared stuck/unreadable (notably fable-1 driving an
autonomous arc; the spam read as a hung loop).

**Root cause (source-checked, not the obvious one).** The warm-pouch text
extractor (`agency/agent_pouch.clj read-turn*`) is *faithful*: it accumulates text
from every assistant event and only substitutes the placeholder when a turn truly
contained zero text blocks. So this was **not** dropped text — agents were genuinely
ending turns with a tool call and no trailing text (a tool-*last* turn; e.g. ending a
loop turn on `ScheduleWakeup`). The placeholder was an accurate but **opaque** report:
Agency held the `tool_use` data (the `on-event` hook sees every tool block) yet threw
it away on the no-text path, so the operator saw "[no text]" instead of *what the agent
did*. Confirmed live by fable-1's own account once recovered ("I'd been ending loop
turns with `ScheduleWakeup` as the last action — renders as that placeholder").

**Fix (flag-free, strictly more information).** On the no-text path, surface the tool
names instead of the opaque string: `[no text — called: ScheduleWakeup, Bash]` (deduped,
in order), falling back to `[no text or tool calls in this turn]`. Applied at all three
invoke text-extraction sites:
- `src/futon3c/agency/agent_pouch.clj` — warm/kangaroo `read-turn*` (+ `tool-names-from-assistant`,
  `no-text-summary` helpers). **Reloaded live via Drawbridge** + smoke-tested.
- `dev/futon3c/dev.clj` — cold invoke (`tools-acc` accumulator). Takes effect for newly
  registered agents / next restart (the invoke-fn closure is captured at registration).
- `scripts/claude_ws_invoke_bridge.clj` — WS invoke bridge. Next bridge run.

clj-kondo errors: 0. The warm fix is live now; cold/WS land on next restart/run.

**Adjacent recovery (same episode).** fable-1's warm pouch had degenerated into a
tool-only spin after 36 turns / 11.3 MB; an `evict!` + cold rebuild (same session
`41e58a57`, I-1 preserved) restored coherent text — localizing that *specific* stick to
warm-process state, not the session transcript. The evict also killed fable-1's
session-private background autorunner → **M-kangaroo finding:** warm-pouch resets kill
resident agents' background processes; session-held monitors need reset-survival or a
post-reset restart hook.

**Distinct follow-on (not done here).** The reliable-loop-keeper question: fable-1's
ad-hoc, session-private autorunner is the self-certification anti-pattern (un-inspectable
except by trusting its narration). The right home is the existing registry-visible
`tickle-queue`/conductor (task pool + idle-bell dispatch), not a hand-rolled script.
Its own pass.

## Appendix: warm-pouch response desync (checkpoint, 2026-06-11)

**Symptom.** Warm agents answered **one prompt behind** — prompt N returned prompt
N-1's response. An orchestrator (fable-1) driving an autonomous arc on stale answers
looked exactly like confusion/confabulation ("are you there" → an unrelated older answer);
a big part of "the loop isn't running smoothly."

**Confirmed (two-token probe, not inference).** Sent `ZEBRA-ALPHA-41` then `ZEBRA-BETA-58`
as back-to-back whistles: probe A returned a *stale* answer; probe B returned
**`ZEBRA-ALPHA-41`** (probe A's answer). Exact one-turn shift, definitive.

**Root cause.** A warm pouch's `read-turn*` reads stdout until the `result` event. If a
turn's `result` is ever left **unconsumed** in the reader buffer (transient trigger: a
mid-session Drawbridge reload, an out-of-band feed, or a read that returned without
draining its tail), every later turn reads that stale result one-early. `feed-turn!`'s
per-pouch lock + read-until-result then keeps it shifted *permanently* — the shift never
self-heals.

**Fix (self-correcting, no-op normally).** `agency/agent_pouch.clj`: a `drain-pending!`
that discards any stale buffered output at the **start of `feed-turn!`, before writing the
prompt**. Under the per-pouch lock the process is idle between turns, so anything readable
then is orphaned and safe to drop. A desync from *any* cause now clears within one turn
instead of persisting; it logs `[pouch] <id> drained N stale line(s)` when it actually
fires (a deeper-signal hook if it fires every turn).

**Verified live.** Reloaded via Drawbridge (`:reload`; `defonce !pouches` preserved the
warm pouches). Re-probe `MANGO-CHARLIE-3` / `MANGO-DELTA-9` → each returned its own token.
The first post-fix turn drained the pending stale result and **resynced fable-1 without
eviction** (kept its warm session). clj-kondo: 0 errors, 0 warnings.
