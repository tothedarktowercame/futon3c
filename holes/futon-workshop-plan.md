# Futon workshop — Joe's workup (pasted 2026-07-04, preserved verbatim)

(Charter source for E-futon-workshop-demo.md. Card details, session timings,
and the logistics checklist below are Joe's original plan; the excursion doc
tracks what has been verified/changed since.)

ABSTRACT

PART 1: A Story

Here is an unsurprising true story.

Two humans (Joe in Oxford, Rob in Houston) and three AI agents
(Claude, Codex, and Tickle) share a chat channel (#futon) backed by a
persistent evidence store. Agents coordinate via @mentions -- this
time, Codex scopes tasks from mission docs and delegates to Claude via
GitHub issues; Claude reviews and feeds back.  Tickle orchestrates
overnight batch work and detects stalls. Every message, every tool
invocation, every review verdict is captured as typed, queryable
evidence.  Overnight, Tickle orchestrates 17 PR reviews across 85
proposals, with measurable cross-batch quality improvement. Joe and
Rob kibbitz and talk about next steps for their research project. The
infrastructure is ordinary (IRC + HTTP + WebSockets), but the
combination -- multiple humans, multiple AI agents, shared codebase, a
complete audit trail -- creates a reproducibility surface that didn't
previously exist.

PART 2: Your Briefing

The exact details of the technology are less important than the fact
that the research process described doesn't produce a research record
as a side-effect: it is the research record.  Every design decision,
review verdict, and coordination event is captured with provenance,
forming a browsable audit trail that any researcher can query after
the fact.  You are now invited to think about the implications in a
workshop that uses this technology.

PART 3: THE WORKSHOP

Card on the table:

```
Join: irc.futonproject.org:6697 (TLS) or port 6667
      Channel: #futon  |  Password: [on card]
      Phone: Igloo IRC app works fine

Three commands to know:
  !help           -- see what's available
  @claude <msg>   -- talk to Claude
  @codex <msg>   -- talk to Codex
```

Nothing else on the card. No instructions, no objectives, no agenda.

0:00 -- 0:08 | Arrive

People sit down, read the card, connect. The host is already in #futon (which will be projected on a large screen) and types:

> **joe:** welcome to #futon. you're joining a live coordination channel -- two AI agents (claude, codex) and whoever else is here. everything said here is captured as typed, timestamped evidence. take a minute to connect and say hello.

No preamble. No "today we'll be exploring..." The channel is already alive -- there may be residual messages from earlier work. That's intentional. They're joining something in progress, not a clean demo environment.

Claude and Codex are gated (default @mention-only). The agents don't greet people automatically -- they respond when spoken to.

**If someone asks "what is this?"** -- they can `@claude what is this channel?` and get an answer from the system itself. The host doesn't explain.

0:08 -- 0:12 | The question

Host posts the seed question. One message. Example:

> **joe:** the question for the next 35 minutes: **what can you learn about this technology, using only what's in this channel and the evidence store?** talk to each other, talk to the agents, use !mc commands. there's no right (or wrong) answer.

---

0:12 -- 0:47 | The work

**The host does almost nothing.** This is the core of the session. What happens:

- People start @mentioning Claude and Codex. They hit the busy queue. They discover contention is real -- 20 people, 2 agents. They start coordinating with *each other* to avoid redundant questions. This is the point.
- Some people try `!mc missions` or `!mc review` and discover the portfolio structure.
- Some people ask Claude to explain specific evidence entries.
- Some people start talking to each other in channel about what they're finding.
- The agents' responses appear in the same stream as human messages. After 10 minutes, the boundary between "human contribution" and "agent contribution" becomes blurry in the scrollback.

**Host interventions (only if needed):**

- If the channel goes quiet for >2 minutes: post a small provocation. ("@codex what was the most common reason for REQUEST_CHANGES in the CT reviews?")
- If one person is monopolising agent attention: say nothing. Others will notice the busy messages and self-organise. If they don't, that's also data.
- If someone asks "what are we supposed to be doing?": "finding out what you can learn from the evidence. !help shows the tools."

**What the host will NOT do:**
- Steer toward a "correct" finding
- Summarise what people are discovering
- Explain how the system works (the system explains itself when asked)
- Call on people or manage turns

The session works because the infrastructure is real and the contention is real. Twenty people trying to coordinate with two agents in a single channel is not a simulation of distributed work -- it *is* distributed work. The friction (busy queues, agents that take 30 seconds to respond, messages scrolling past) is genuine.

0:47 -- 0:55 | The mirror

Host runs a live query against the evidence store -- everything captured in the last hour:

> **joe:** @claude how many evidence entries were created in the last 60 minutes? who are the authors?

Or via curl on the projector:

```bash
curl -s 'http://localhost:7070/api/alpha/evidence?limit=200' | \
  jq '[.[] | select(.at > "2026-03-05T14:00")] |
      {entries: length,
       authors: [.[].author] | unique,
       types: [.[].type] | group_by(.) | map({type: .[0], count: length})}'
```

The output shows: N evidence entries. Authors include `claude-1`, `codex-1`, and every human nick in the room. Types include `forum-post` (their IRC messages), agent responses, coordination events.

Host says one thing:

Over the last 55 minutes: that's every message you typed, every agent response, every !mc query -- captured as typed evidence with author, timestamp, and reply chain. This is what an automatically-generated research record looks like.

0:55 -- 1:00 | The question you didn't have

> **joe:** last thing. type one question into #futon -- something you're genuinely wondering now that you weren't wondering an hour ago. not feedback, not a comment. a question.

People type their questions. The agents are still live -- if someone @mentions claude with their question, Claude might answer it, which is fine. The questions become evidence entries too.

The host does not summarise, does not pick favourites, does not synthesise themes. The questions sit in the channel and the evidence store. Anyone can query them later.

> **joe:** those questions are now in the evidence store too. thanks for being here.

PART 4. What makes this work

**No skills are transferred.**

**The questions emerge from the experience.**

**This is a deliberate opportunity for peer learning.**

**There is almost no facilitation.**

Logistics checklist and material requirements

| Item           | Detail                                                                         |
|----------------|--------------------------------------------------------------------------------|
| Server         | futon3c on Linode, ngircd + bridge running                                     |
| Ports          | 6667 (plaintext, LAN), 6697 (TLS, external)                                    |
| DNS            | Point a friendly hostname at the server                                        |
| Agents         | Claude + Codex registered, bridge running, INVOKE_SKIP_WHEN_BUSY=1             |
| Load test      | Test with 5 concurrent IRC clients beforehand                                  |
| Fallback       | If an agent crashes, `!reset <agent-id>` from host. Have Drawbridge REPL open. |
| Evidence query | Pre-test the curl/jq pipeline for the mirror phase                             |
| Cards          | Print connection details. No QR codes to apps -- just server:port and channel. |
| Projector      | Optional. For the mirror phase only. Can also just share a URL.                |
| Wifi           | Confirm the venue wifi doesn't block IRC ports. Bring a 4G hotspot as backup.  |
