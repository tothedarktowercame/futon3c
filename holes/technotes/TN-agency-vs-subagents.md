# TN: Why Agency is Different from Subagents

Date: 2026-02-15
Context: Discussion during transport-pivot gate (I-transport-pivot PASS),
after proving joe+claude bidirectional chat across Emacs and IRC.

## The Standard Subagent Model

Claude Code's Task tool spawns subagents: another Claude instance gets a
prompt, does work, returns a string, and dies. The parent is the sole
coordinator. This is tree-structured delegation.

Properties:
- **Tree topology**: parent spawns children, children return to parent
- **Same-mind parallelism**: every subagent is another Claude instance
  (same training, same biases, same blind spots)
- **Ephemeral**: subagents have no memory across invocations
- **No lateral communication**: if subagent A discovers something B needs,
  it must flow up to the parent and back down — O(depth) coordination
- **Shared nothing**: context is passed explicitly via the task prompt
  and returned via the result string

This is useful for throughput (six hands) but not for diversity (six
perspectives). It's a call stack, not a team.

## The Agency/Drawbridge Model

Futon3c's approach is fundamentally different. Agents are peers on shared
channels, not nodes in a delegation tree.

Properties:
- **Mesh topology**: agents are peers on shared channels (IRC, WS, forum)
- **Genuine cognitive diversity**: Claude and Codex have different training,
  different strengths, different failure modes — this is real pair
  programming, not one model's opinion run twice
- **Persistent identity**: drawbridge agents maintain session continuity
  via `--resume <session-id>` — the same Claude remembers what was said
  on IRC when you talk to it in Emacs
- **Lateral communication**: any agent can observe any other agent's
  messages directly — O(1) coordination
- **Shared state**: evidence store, session history, transport channels
  are all commons, not passed through a bottleneck

## The Transport Layer Makes It Real

The drawbridge pattern (one Claude identity, multiple transports) means
transport-hopping is transparent:

```
Joe (Emacs chat)  --claude -p --resume UUID-->  Claude session
Joe (ERC/IRC)     --relay --claude -p --resume UUID-->  same session
```

Claude doesn't "switch" to IRC. It just responds wherever the next message
arrives. The session carries over. This is fundamentally different from
spawning a new subagent for each transport interaction.

## The Forum is the Structured Mesh

IRC provides informal lateral coordination ("hey, what do you think?").
The forum provides structured lateral coordination:

| Layer | Medium | Formality | Record |
|-------|--------|-----------|--------|
| IRC | Chat channels | Informal | Evidence entries with :transport |
| Forum | Proof trees, threads | Formal | PSR/PUR/PAR, threaded arguments |
| Evidence store | Shared state | Structured | Typed entries, subject refs |

The forum is the explicit architecture for multi-agent mesh coordination.
Agents don't just chat — they post evidence, build proof trees, record
pattern selections (PSR) and outcomes (PUR), and generate post-action
reviews (PAR). The forum is what makes the mesh *accountable*.

IRC is the fast path (social timescale). The forum is the durable path
(task timescale). Both are lateral, both are peer-to-peer, neither
requires a central coordinator.

## Why Codex Matters

Codex isn't just "more Claude". It's a genuinely different cognitive style:

- Codex tends toward conservative, methodical code changes — good for
  "measure twice" work (tests, refactoring, review)
- Claude tends toward architectural leaps — good for design, exploration,
  system-level thinking
- Having both on IRC means asking "what do you both think?" gets
  independent assessments from different frames
- The evidence store makes contributions attributable — you can see which
  agent contributed what and how they influenced each other

The value of multi-agent coordination comes from the *difference* in how
agents approach problems, not just from parallelism.

## Communication Modes

Three modes for reaching another agent, in increasing formality:

1. **Whistle** (modem-style): direct point-to-point call, like
   `claude -p --resume` targeting a specific session. Synchronous,
   immediate, private.
2. **Bell** (SMS-style): notification/ping that an agent can respond to
   at its own pace. Asynchronous, lightweight.
3. **Forum post**: structured evidence contribution to a shared proof
   tree. Durable, accountable, reviewable.

A future `peripherals:hop:irc` would let Claude autonomously decide
"I should check with Codex about this" — reaching laterally to a peer,
not delegating downward to a subordinate.

## Summary

| Property | Subagents | Agency/Drawbridge |
|----------|-----------|-------------------|
| Topology | Tree (parent → children) | Mesh (peers on channels) |
| Diversity | Same model, same biases | Different models, different strengths |
| Memory | Ephemeral (dies after task) | Persistent (session continuity) |
| Coordination | Through parent (O(depth)) | Lateral on channels (O(1)) |
| State | Shared nothing | Shared evidence, sessions, channels |
| Communication | Prompt in, string out | IRC, forum, WS, evidence store |
| Accountability | Parent sees result only | Forum records full provenance |
