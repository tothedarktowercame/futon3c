# voxterm — Voice Surface

voxterm is a **surface**, not a peripheral: voice in, voice out, over a session
that is otherwise unchanged. You dictate into a phone; whisper transcribes on
the box; the text lands in the Emacs claude-repl exactly as if typed. The
agent's reply streams to the buffer as normal, and its first paragraph is spoken
back.

Implementation lives outside this repo, in `~/code/voxterm/` (Python stdlib +
one HTML page + `voxterm.el`). **Nothing in futon3c is modified** — the Emacs
integration is `advice-add` from `voxterm.el`, removable at runtime.

## Why a surface and not a peripheral

Every entry in `resources/peripherals.edn` is
`{:peripheral/id :tools :scope :entry :exit :context}` — the schema has no slot
for transport, rendering, or output medium, because a peripheral constrains
*what an agent can do*. voxterm constrains nothing; it changes how words get in
and out. It belongs with `"irc"`, `"emacs-socket"` and `"ws"`, not with
`:explore` and `:edit`.

The genuinely peripheral half of "talk about code without editing it" would be a
separate `:converse` envelope (read-only tools, hops to `:edit` on request).
That is **not implemented** and is not required for the voice surface to work.

## Architecture

```
  Phone (DeX / handset)                    The box
  ┌─────────────────────┐                  ┌────────────────────────────────┐
  │ Chrome │ voxterm    │                  │  server.py  (stdlib, :8081)    │
  │        │ page       │                  │                                │
  │  mic ──┼──WAV 16k──▶│──POST /transcribe├─▶ whisper.cpp small.en         │
  │        │            │                  │        │                       │
  │        │            │◀─────────────────┤    POST /route                 │
  │        │            │                  │        │                       │
  │  🔊 ◀──┼── WAV ─────│◀─POST /speak ────┤    piper TTS                   │
  │        │            │                  │        ▲                       │
  │        │  poll ─────│──GET /say/next ──┤────────┘                       │
  └─────────────────────┘                  └──────────┬─────────────────────┘
                                                      │ emacsclient -s server
                                                      ▼
                                           ┌────────────────────────────────┐
                                           │  Emacs daemon                  │
                                           │   voxterm.el                   │
                                           │    ├─ voxterm-insert  (in)     │
                                           │    └─ advice on                │
                                           │       agent-chat-stream-text   │
                                           │       (out, POST /say)         │
                                           │  *claude-repl:claude-N*        │
                                           └────────────────────────────────┘
```

The page is served over an ssh tunnel (`ssh -N -L 8081:localhost:8081`), which
also satisfies the browser's secure-context requirement for microphone access —
`localhost` counts as secure, so no TLS is needed. The server binds to loopback
only. **mosh cannot forward ports**; this needs a plain ssh alongside it.

## The two seams

**In — `voxterm-insert`.** Emacs needs no listener: `emacsclient` pushes into the
running daemon over its unix socket. `voxterm.el` is loaded on demand by
`server.py`, so there is nothing to add to `init.el`. Text goes to point in the
selected window of a visible frame, falling back to `*voxterm*` if that buffer is
read-only. With submit enabled it then runs whatever `RET` is bound to — in
`claude-repl-mode` that is `claude-repl-send-input`, so dictation actually sends.
It calls the command rather than synthesising a keypress, which is more reliable
from a daemon eval.

**Out — advice on `agent-chat-stream-text`.** Every streamed text delta passes
through `agent-chat-stream-text` (`emacs/agent-chat.el:1494`). voxterm accumulates
deltas and, at the first blank line, POSTs that paragraph to `/say`;
`agent-chat-end-streaming-message` flushes single-paragraph replies that never
contain one. Because it rides the stream, the paragraph arrives **before the tool
calls** — a turn-start signal, not an end-of-turn summary.

The box has no speaker, so Emacs cannot synthesise directly. It enqueues; the
page (on the phone) drains `GET /say/next` and plays.

Enable inside Emacs with `M-x voxterm-toggle-speaking`. Off by default, so a
session you are not listening to stays silent.

## The design invariant

**The audio is a pure duplicate of the buffer.** The REPL streams exactly as it
does today; speech is never in the path and nothing waits on it. Therefore every
failure degrades to silence: a paragraph opening with a code fence is skipped, a
sanitiser miss is at worst ugly, a dead server is a quiet turn. The queue is
capped and drops oldest-first, because stale speech is worse than dropped speech.

This is what licenses shipping a rough version and tuning by ear.

## No agent cooperation required

Speaking the first paragraph needs **no surface contract and no instruction to
the agent** — paragraph breaks are a boundary that already exists, and
`claude-repl` already splits on them (`emacs/claude-repl.el:1286`).

Measured over 121 user turns in recent transcripts: **88% already open with
spoken prose**, median 145 characters, 91% plain (9% open with a fence, table or
heading and are skipped). A `CLAUDE.md` instruction to "summarise your plan at
the start" would buy at most 12 points, against the current model guidance to
*remove* forced-narration scaffolding because models already do it. Not adopted.

If a surface contract is added later it should state facts, not commands —
*"your reply will be spoken aloud"*, not *"keep it short"*. Per
`futon3/docs/peripheral-spec.md`, constraints are structural, not behavioural.
Brevity follows from the fact; it should not be instructed. The stream already
carries `:surface "emacs-repl"` (`emacs/claude-repl.el:923`), which is where a
`"voxterm"` value would go.

## Vocabulary

small.en mangles domain words. whisper-cli's `--prompt` biases the decoder, and
**the shape of the prompt matters more than its contents**:

| prompt | "Ask Claude…" | "Tell Claude…" | jargon |
|---|---|---|---|
| none | "Ask **call**…" | ✓ | FUTUM-3C, Clodjoe, M-RAPL |
| word list | "Ask **call**…" | "Tell **Clojure**…" | ✓ |
| **words in position** | ✓ | ✓ | ✓ |

A bare list is *actively harmful* for "Claude": listing it beside "Clojure" makes
them compete and breaks a case that worked with no prompt at all. Showing each
word where it occurs — `"Ask Claude. Tell Claude. Claude Code."` — fixes every
case at no latency cost. Override with `VOXTERM_PROMPT`.

`FIXUPS` in `server.py` is the backstop for residue (`quad` → `Claude`). Keep it
short: `call` and `Clojure` are also common mishearings of "Claude" but are real
words here, so they must never be substituted.

## Measured on this box (EPYC 4545P, 16 cores, CPU only)

| stage | figure |
|---|---|
| whisper small.en, `-ac 512`, greedy | **~550 ms** for a 6 s utterance |
| whisper large-v3-turbo | 6.7 s — too slow to converse with |
| piper `en_GB-semaine-medium` | **~0.5 s** to synthesise, ~11× realtime |

Two counterintuitive results worth keeping: `-ac` gives small.en a 3× speedup but
*hurts* large-v3-turbo (clipped context sends it into a repetition loop that
costs more than full context), and `-ac 128` is past a cliff — it gets slower
*and* truncates.

## Rejected

**Echoing the transcript back at dispatch.** Built and tried; reads as a
hyperactive parrot rather than a thinking partner. The justification was that it
confirms the transcription before minutes of wrong work — but the transcript is
already on screen in the pending buffer, which is the better channel for
checking it. Same "don't duplicate what the monitor already shows" argument that
rules out mid-turn narration. Survives as a debug toggle, off by default.

**A condensation model (Haiku / local GLM) summarising the buffer for speech.**
Unnecessary once the agent's own first paragraph is used: a summariser produces
another model's account of what happened, where the first paragraph is what the
agent meant to say. It also adds a round trip and a dependency for no latency
gain, since both run after the turn.

## Open

- **Barge-in** — interrupting mid-speech needs the listener live during playback
  with echo cancellation. This is what separates "works" from "feels like the
  claude.ai app", and nothing here addresses it.
- **`:converse` peripheral** — spec drafted in `~/code/voxterm/DESIGN.md`, not
  validated against `futon3c.social.shapes/PeripheralSpec`.
- **Length** — the p90 lead paragraph is ~19 s of speech. Uncapped for now;
  decide by ear.
