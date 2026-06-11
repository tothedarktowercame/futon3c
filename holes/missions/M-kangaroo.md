# M-kangaroo — Warm-pouch persistent agent processes

Date: 2026-06-10
Status: INSTANTIATE v1 LANDED on master + LIVE-VALIDATED 2026-06-10 (agent_pouch.clj, gate in make-claude-invoke-fn, flag FUTON3C_KANGAROO default OFF). VERIFY ✅ — live real-claude `feed-turn!` ×2: turn1 spawn 7.6s → "ONE", turn2 warm 3.4s → "TWO" (≈2.2× faster even fresh; gap scales with session size = the 5.8MB win). ACTIVATION: next restart (closure-capture, like Car-3) + cr-new agents + flag ON. Reviewed: OFF byte-for-byte cold; ON→cold-fallback on any Throwable; feed-turn! evicts pouch on failure (no desync).
Cross-reference: `M-agency-hardening.md` (durable turn-queue, invoke path,
false-online-state, the flag-gated-rollout discipline). Kangaroo extends the
same invoke path; it is the natural successor once the queue is live.

## HEAD (assembled from Joe's turns, 2026-06-10)

Bringing fable-1 in via the legacy CLI exposed the substrate: **every
agency-routed turn invokes `claude -p --resume <session>` — a fresh cold process
that reconstructs the session from disk before it can answer.** The cost scales
with session size and is unbounded as sessions grow. It was invisible until
fable-1's 5.8 MB session made it loud (~67 s/turn, measured). Joe's interactive
CLI never feels this because it is *one persistent warm process* that the harness
keeps compacted across turns.

The improvisation theme: **make the warm steady-state first-class.** Hold each
agent in a persistent "pouch" (Joe's Kangaroo) so the agent *is* a continuously
inhabited process — the I-1 ideal — rather than reconstructed every turn. A
fleet-wide latency win, not a fable-1 patch; a managed revival of the old
"route into the live process" pattern.

- *Jazz-head reading:* warm-pouch-persistence vs cold-re-spawn is the generative
  kernel the rest of the mission improvises over.
- *AIF-head reading:* satisfaction condition = an agency turn reaches the agent
  **without re-paying session-reconstruction cost**, with identity and surface
  routing preserved.

## Motivation

- Cold-resume-per-turn is structural: `make-claude-invoke-fn` /
  `make-codex-invoke-fn` spawn a one-shot CLI per invoke. Latency ∝ session size,
  unbounded over a long collaboration.
- The interactive CLI proves the warm model works (persistent process,
  harness-compacted). Kangaroo brings that warmth under Agency management for
  ALL agents, fed by the durable queue that already serializes per-agent turns.

## Existing Ground (IDENTIFY)

- `dev.clj` `make-claude-invoke-fn` / `make-codex-invoke-fn` — the current COLD
  path (`claude -p --resume … --output-format stream-json`, one-shot). This stays
  as the safety-net fallback.
- `futon3/fuclaude` + `futon3/fucodex` — mature one-shot `claude --print
  --output-format stream-json --verbose` wrappers. **Reuse their stream-json I/O
  parsing**; they are NOT warm (re-spawn per turn), so they are plumbing, not the
  persistence mechanism.
- `claude --print --input-format stream-json --output-format stream-json` — the
  candidate native warm mechanism (feed multiple turns to one stdin). **KEYSTONE
  SETTLED 2026-06-10 (W1 smoke test, `/tmp/kangaroo_w1.py`):** one
  `claude --print --input-format stream-json --output-format stream-json
  --verbose` process answered TWO consecutive turns and stayed alive — PERSISTENT.
  Schema learned: input per line `{"type":"user","message":{"role":"user",
  "content":[{"type":"text","text":"…"}]}}`; output events `system→assistant*→
  result`; **turn boundary = the `result` event**. No pty/SDK needed.
- `agency.turn-queue` (M-agency-hardening) — per-agent FIFO accept→enqueue→drain
  with per-surface reply routing. Kangaroo's **feeder**: the drain target becomes
  the warm process instead of a cold spawn.
- `claude-repl.el` + the interactive CLI — the REPL surface. Design goal: REPL
  and agency **share one warm process per agent** (one inhabited process, two
  surfaces) rather than competing for the session (cf. the fable-1 inhabitance
  collision).

## DERIVE — Experiment log

### E1 — stream-json multi-turn persistence (W1 keystone), 2026-06-10 ✅

- **Question (gate):** does one `claude --print --input-format stream-json`
  process answer *multiple* turns warm, or exit after one? Determines whether the
  pouch is a native stream-json process or needs pty/interactive/the Agent SDK.
- **Method:** `/tmp/kangaroo_w1.py` — spawn `claude --print --input-format
  stream-json --output-format stream-json --verbose --permission-mode
  bypassPermissions` with piped stdin/stdout (FRESH session, no `--resume`). Send
  one stream-json user message, read stdout events until a `result`, assert the
  process is still alive, then send a SECOND user message on the SAME process and
  repeat.
- **Result: PERSISTENT.** turn1 → event types `[system, rate_limit_event,
  assistant, assistant, result]`, process alive; turn2 (same process) →
  `[system, assistant, result]`, process alive. Both turns answered off one warm
  process; the process blocks on stdin between turns and resumes on the next line.
- **Derived facts:**
  - Input (one JSON object per line):
    `{"type":"user","message":{"role":"user","content":[{"type":"text","text":"…"}]}}`.
  - Output per turn: `system → (rate_limit_event?) → assistant×N → result`.
    **Turn boundary = the `result` event.** No pty/SDK required.
- **Design implication:** the pouch = one long-lived process per agent; an invoke
  = write one user line to its stdin + read events until `result` (collect the
  `assistant` text). This is a drop-in replacement for `make-claude-invoke-fn`'s
  per-turn cold spawn, and the durable queue's per-agent serialization is exactly
  the right front-end (warm process = one turn at a time; no extra lock).

### E2 — `--resume` context-carry + warm persistence, 2026-06-10 ✅

- **Question:** does the REAL pouch invocation — `claude --resume <sid>
  --input-format stream-json` — both resume the agent's existing conversation AND
  stay warm across turns? (E1 used a fresh session.)
- **Method:** `/tmp/kangaroo_e2.py`. Phase 1: a fresh stream-json process stores a
  fact ("magic word = BANANA-42") and yields its `session_id` (`f13d5d59…`).
  Phase 2: a NEW `--resume <sid>` process is asked to recall the word
  (context-carry test), then given a second turn (persistence test).
- **Result: RESUME+PERSISTENT.** turnA recalled "BANANA-42" (context carried),
  process alive; turnB answered "E2-OK" on the same process, still alive.
- **Derived fact / KEY design implication:** the pouch pays the session-resume
  cost **exactly once — at spawn** (`--resume` loads the existing transcript);
  every subsequent turn is warm. So fable-1's ~67s is a one-time pouch-spawn cost
  under Kangaroo, not per-turn. `session_id` is available in the stream events —
  use it to bind the pouch to the agent's session-file. **DERIVE core complete
  (E1+E2); the mechanism is proven.**

### E5 — Resume-latency, caching & compaction economics (Max + claude-3 case study), 2026-06-11 ✅

- **Question:** is warming a giant session a *token*-cost trap (re-prefilling the
  whole history past the 5-min prompt-cache TTL), and can we "compact before resume"
  to make a days-long session load fast? Prompted by the joey gate (added 2026-06-10,
  commit `042fad2`) and Joe's 30-min turn cadence.
- **Method:** `/claude-api` skill for API-level caching/compaction facts; a
  `claude-code-guide` agent + `claude --help` for the CLI knobs
  (`T-kangaroo-caching-and-compaction.md`); auth probe of `dev-laptop-env` + the live
  JVM env + `~/.claude`; read-only inspection of claude-3's transcript.
- **Findings:**
  1. **The knobs are env vars, not CLI flags.** TTL: `ENABLE_PROMPT_CACHING_1H=1`
     (1-hour cache TTL; default 5-min for API-key users; `FORCE_PROMPT_CACHING_5M=1`
     forces 5-min). Compaction: auto-compaction *does* run in the pouch's `--print
     --input-format stream-json` headless mode (default ~95%; tune via
     `CLAUDE_AUTOCOMPACT_PCT_OVERRIDE` / `CLAUDE_CODE_AUTO_COMPACT_WINDOW`; disable via
     `DISABLE_AUTO_COMPACT`). **No on-demand compaction trigger in headless** —
     `/compact` is interactive-only; no stream-json stdin control message. `--resume`
     inherits prior compaction (resumed context = the summary), first turn a cache miss.
  2. **The pouches run on Joe's Max SUBSCRIPTION, not an API key** — no
     `ANTHROPIC_API_KEY` / `_AUTH_TOKEN` / `CLAUDE_CODE_OAUTH_TOKEN` anywhere
     (dev-laptop-env or live JVM env); `~/.claude/.credentials.json` login is inherited
     by the spawned `claude` processes. Subscription users get the **1-hour TTL
     automatically within included usage** + transparent auto-compaction. ⇒ The
     token-trap the joey gate guarded against **does not apply within included usage**,
     and `ENABLE_PROMPT_CACHING_1H` is a no-op for this setup. (It revives only if
     Agency's load exceeds the included allotment, where CC drops to 5-min TTL.)
  3. **claude-3 case study:** transcript `8146485e…` = **29 MB, 10,356 lines, ~4 days
     (2026-06-06 → 06-10), already auto-compacted 5×** (`compactMetadata` /
     `isCompactSummary` ×5). So its *context* is already small (post-last-compaction
     summary); the **resume slowness is the 29 MB FILE** — disk read + parse to
     reconstruct — NOT the context size. Compaction shrinks what the model sees, NOT
     the append-only `.jsonl`. "Compact before resume" is therefore *already happening*
     and won't speed the resume; the latency lever is FILE size (prune/fork the
     `.jsonl` — CC exposes no such op; risky surgery) or **warm-to-amortize** (pay the
     29 MB read once at pouch spawn instead of every cold turn).
- **Design implication — joey gate REFRAMED.** Two of the gate's three rationales are
  now handled elsewhere: the *token-trap* by Max's auto-1h-TTL (within usage), and
  *slow-spawn-holding-a-shared-lane* by **drainer v2** (the spawn runs on the agent's
  own drainer thread, not a shared invoke lane). What survives is **resume-latency
  hygiene + RAM** — and for an *actively-used* monster (claude-3) warming actually
  WINS (amortize the 29 MB read once vs re-reading cold every turn), so the gate is
  counterproductive there. ⇒ Keep the joey gate as a conservative DEFAULT, but
  `allow-monster!` the monsters Joe uses heavily (claude-3 is the canonical override).
  The `.jsonl`-bytes metric remains the right proxy for the surviving (latency) axis.
  **Open:** a real `--resume` latency number for 29 MB (measure on a throwaway copy —
  no live-agent collision); and a session-file prune/fork tool if file-latency must be
  attacked directly.

### E6 — Live cold-resume latency: bytes is the WRONG proxy, 2026-06-11 ✅

- **Question (E5's open item):** real `--resume` latency for the monsters, measured
  on the LIVE production path (evict pouch → "just say hello" bell → agency drain →
  pouch respawn), not a synthetic copy.
- **Method:** `agent-pouch/evict!` both warm pouches via Drawbridge, bell each agent
  (`agency_send.py`, hello ping), read spawn→first-result off `pouch/snapshot`
  (`:spawned-at` vs `:last-used-ms`) + job-event timeline (accepted→done).
- **Result — INVERSION:**
  - **claude-3** (29 MB `.jsonl`, auto-compacted ×5): accepted→done **20.6 s**
    (pouch spawn→result 20.3 s).
  - **fable-1** (5.8 MB `.jsonl`, one long uncompacted day, Fable model):
    accepted→done **36.6 s** (spawn→result 36.3 s).
  - The 5× smaller FILE took 1.8× LONGER to resume. Both well under the old 67 s
    anchor, and both are now once-per-spawn costs (subsequent turns warm, ~3 s class).
- **Derived fact:** `.jsonl` bytes does NOT predict resume latency — the dominant
  cost is the LIVE (post-compaction) context prefill + model, not file parse. This
  partially corrects E5's "the resume slowness is the 29 MB FILE" inference: 29 MB
  parsed + answered in 20 s, while a small-but-uncompacted session took 36 s.
- **Design implication:** the joey gate's byte metric survives only as a coarse
  RAM/disk-hygiene proxy; for the latency axis it actively misranks. With the durable
  monster allowlist (`FUTON3C_KANGAROO_MONSTER_ALLOWLIST`, default `claude-3,fable-1`)
  the practical exposure is low; if calibration (T-kangaroo ticket item 2) ever
  matters, the right metric is estimated live-context size, not transcript bytes —
  and the threshold can comfortably go UP.

### Observability parity (review fix, 2026-06-11)

The v1 warm path skipped invoke-once's whole observability contract: evidence
events (invoke-start/complete), the `*invoke:*` blackboard updates, the 5s
ticker (the `*agents*` refresh), live tool-activity surfacing
(`update-invoke-activity!`), the interrupt control (a warm turn could NOT be
interrupted — it ran to timeout), and `context-retrieval!` (pattern retrieval —
Joe: must-have). Fixed for parity: `feed-turn!` gained an `:on-event` hook
(exceptions swallowed — observability can't kill a turn); the warm branch in
`make-claude-invoke-fn` now emits evidence, drives the blackboard + ticker,
surfaces tool activity, registers an interrupt (= evict the pouch; cold-fallback
discipline holds), fires pattern retrieval, and returns `:invoke-trace-id`.
Principle: **warmth must not darken the turn** — pruning observability (e.g.
whether blackboard stays) is a decision over BOTH paths, not a silent warm-path
default. Known residual asymmetry: cold keeps only the LAST assistant message's
text (resets on post-tool text), warm concatenates all assistant text —
unchanged for now, flagged.

### Open experiments (refinements — not gates)

- **E3:** crash/error signalling in the event stream → the trigger for the
  cold-fallback (invariant 3); and `rate_limit_event` handling (seen in E1).
- **E4:** codex equivalent — does the codex CLI offer a persistent stream mode,
  or do codex pouches need a separate mechanism? (claude agents can ship first.)

## Invariants (Kangaroo-specific, atop the futon3c architectural invariants)

1. **One warm process per agent identity (I-1):** the pouch holds exactly one
   live process per agent; never two competing for the same session.
2. **Explicit turn boundaries:** a warm process handles one turn at a time;
   boundaries come from the stream-json `result` event, never guessed.
3. **Crash → cold fallback, never hang:** if the warm process dies mid-turn, the
   invoke falls back to the current cold `claude -p --resume` and the pouch
   re-spawns. A turn is never stranded by warmth.
4. **Bounded warmth:** idle pouches evict on policy (idle-TTL / max-warm count);
   resource cost is capped and observable.
5. **Session continuity:** the warm process's in-memory session stays
   reconcilable with the durable session-file/`--resume`, so cold fallback or
   restart resumes the same conversation.
6. **Flag-gated, load-dark:** behind a flag, default OFF; the cold path is
   byte-for-byte preserved when off (same discipline as `FUTON3C_DURABLE_QUEUE`).

## Scope

### In
1. Per-agent warm-process manager ("the pouch") with full lifecycle.
2. Feed-turn + parse-response over the persistent stream (reusing fuclaude/
   fucodex parsing); turn-boundary = `result` event.
3. Wire the turn-queue drain → warm process, flag-gated, with cold fallback.
4. Observability: pouch state (warm/cold/evicted), warm-hit vs cold-miss,
   per-turn latency (mesh-QA-adjacent).

### Out
1. Replacing the claude/codex CLI with a custom model client.
2. Rewriting the durable queue (Kangaroo is a new drain target, not a queue
   change).
3. Persisting warm processes across JVM restart (warmth is in-process; cold
   fallback covers restart).

## Workstreams

- **W1 — Keystone verify (GATE).** Smoke-test whether one `claude --print
  --input-format stream-json` process answers ≥2 consecutive turns (and the
  codex equivalent). Choose the mechanism (stream-json `--print` vs
  interactive+pty vs SDK) with evidence. **Everything else depends on W1.**
- **W2 — The pouch.** Per-agent warm-process manager + lifecycle
  (spawn/keepalive/crash-restart/idle-evict/cap).
- **W3 — Turn I/O.** Feed-turn + per-turn response parsing; boundary = `result`.
- **W4 — Queue integration + rollout.** Drain → pouch behind a flag; cold
  fallback; OFF = current behavior.
- **W5 — REPL coexistence.** Design one-warm-process-shared-by-REPL-and-agency;
  impl may be a follow-on slice.
- **W6 — Observability + acceptance metrics** (warm latency vs cold).

## Acceptance Criteria

- [ ] W1 keystone settled with evidence: a warm process answers ≥2 consecutive
      turns, or the mechanism is re-chosen.
- [ ] A warm-routed turn is measurably faster than cold on a large session
      (fable-1 5.8 MB: warm ≪ 67 s).
- [ ] Warm-process crash falls back to cold resume without stranding the turn.
- [ ] Flag OFF = byte-for-byte cold behavior; ON = warm routing.
- [ ] Exactly one warm process per agent identity; idle eviction works + is
      observable.
- [ ] (stretch) REPL + agency share one warm process per agent.

## ARGUE (2026-06-10) — design decided

- **A1 — exactly one warm process per agent (forced, not chosen).** IF we ran two
  processes on one session (a warm Agency pouch + a separate REPL process),
  HOWEVER `claude --resume` collides on an inhabited session (the fable-1 lesson:
  "No conversation found"), THEN there must be ONE warm process per agent
  identity, BECAUSE a session can only be inhabited once.
- **A2 — the invoke-fn is the single chokepoint, so warm it once.** `cr` opens a
  `*claude-repl:<aid>*` buffer whose turns post bells to Agency; Agency bells do
  the same — BOTH route through `invoke-agent!` → the agent's invoke-fn. THEREFORE
  warming `make-claude-invoke-fn` (feed a persistent pouch instead of cold
  `claude -p --resume`) warms REPL **and** Agency from one pouch, no shared-stdin
  gymnastics. The durable turn-queue already serializes per-agent invokes, so the
  one pouch is fed one turn at a time — safe by construction.
- **A3 — mint-time warmth via `cr new` = zero resume cost.** Agents are minted via
  `cr new` with a FRESH session, so the pouch spawns with nothing to resume; the
  once-at-spawn cost (E2) only bites on rare legacy imports (fable-1). The
  invoke-fn closure is built at registration, so warmth takes effect for agents
  minted AFTER the change — rollout is naturally scoped to new agents, existing
  ones undisturbed.
- **A4 — cold fallback, flag-gated load-dark.** Pouch dead or flag off → invoke
  falls back to the proven cold `claude -p --resume`; a turn never hangs
  (invariant 3 + the `FUTON3C_DURABLE_QUEUE` discipline).
- **A5 — `cr` (claude) first, `cx` (codex) later.** Ship the claude pouch
  (`make-claude-invoke-fn`); defer codex (E4 open) since only claude's stream-json
  persistence is proven (E1/E2).

**VERIFY** is largely discharged by the design: E1/E2 prove the mechanism; the
flag-gated/cold-fallback/once-at-spawn structure bounds the risk. Acceptance =
measure warm-vs-cold turn latency on a `cr new` agent (and on fable-1's 5.8 MB
session: cold ≈67 s/turn → warm spawn-once then fast).

## INSTANTIATE (build scope, v1 = cr/claude, via Codex handoff)

Replace the cold spawn inside `make-claude-invoke-fn` with a warm pouch:
- New `agency/agent-pouch` ns: per-agent registry of one persistent
  `claude --resume <sid> --input-format stream-json --output-format stream-json
  --verbose --permission-mode …` process; spawn/keepalive/crash-evict/idle-evict.
- Invoke = write one `{"type":"user",…}` line to the pouch stdin, read events
  until `result`, return assistant text + session_id (schema from E1/E2).
- `make-claude-invoke-fn`: when the flag is on, route through the pouch; else the
  current cold body (byte-for-byte). Crash/timeout → cold fallback.
- Flag `FUTON3C_KANGAROO` (default OFF), load-dark; warmth applies to agents
  minted after reload (cr new), per A3.
- Out of v1: codex/cx pouch (E4); cross-restart warmth.

## First Cut Tasks

1. **W1 smoke test** — `claude --print --input-format stream-json` multi-turn on
   one process. [BLOCKS the mission.]
2. Read `futon3/fuclaude` + `futon3/fucodex` fully — harvest stream-json I/O
   parsing.
3. Sketch the pouch data model + lifecycle states.
4. Design the turn-queue-drain → pouch interface.
5. Decide v1 scope: agency-only warm, or shared-with-REPL from the start.
