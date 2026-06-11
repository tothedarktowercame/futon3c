# Mission: M-smart-emacs-cursor — the spoken mission loop

**Date:** 2026-06-11
**Status:** :greenfield — HEAD + IDENTIFY + MAP, authored live (Joe + Fable).
**Spawned:** task #3 of the 2026-06-11 shortlist; the prior smart-cursor work
stalled because it was dispatched for autonomous building when it is
inherently interactive. This revival is built WITH the operator at the
keyboard, and connects the day's other work as its organs.

## HEAD (Joe, 2026-06-11, verbatim sense)

> I have a Voice Typing mode I should be able to initiate — it invokes
> Whisper. So I could say "Let's start a mission about keeping my Futon City
> map up to date" — and you would get that message sent through, then you'd
> drive the smart cursor around to type everything needed into Emacs, change
> all the modes, bring up the mission (with all the scopes), and the cursor
> itself might then emit a speech bubble (with posframe): "Done, what next?"

The loop, typed: **voice → Whisper → text → agent → cursor choreography →
mission-on-screen with scopes → spoken-back completion.** The operator
speaks an intent; the system performs the clerical realization and shows its
work spatially; the completion bell arrives WHERE THE WORK IS (a posframe at
the cursor), not in a side channel. This is [[project_tickle_as_operator_model]]
inverted — operator drives system — and the missing interaction layer of
[[project_joe_hud_model]]: the cyborg loop's motor half.

## 1. IDENTIFY — what exists, live-verified 2026-06-11

1. **Voice input EXISTS and is loaded** — `futon0/contrib/voice-typing.el`:
   `my-chatgpt-shell-voice-toggle` starts `~/opt/voice-typing-linux/voice`
   (Whisper; dvorak; enter-keyword "rocket") + `ydotoold`. Voice lands as
   synthetic KEYSTROKES into whatever holds focus — so routing speech to an
   agent = focusing that agent's input surface (a `*claude-repl:*` prompt or
   the CLI). `M-x stack-hud` is alive (`fboundp` → t) as the control surface.
2. **The actuator EXISTS** — `futon3c/emacs/smart-cursor.el` (1,215 lines):
   WS-attached companion cursor (◆ + caption), `run-script` executor, e2e
   harness proven at 14ms/5-buffer cycle
   (`docs/technote-smart-cursor-external-e2e-handoff.md`). Currently
   `smart-cursor-mode` is OFF in the live Emacs, and the harness holds a
   **non-editing invariant** (cursor proves control without buffer edits).
3. **The transport EXISTS as of today** — `futon3c/emacs/futon-agency-ws.el`
   (built this session): one shared Agency WS, observer identity, subscribe/
   dispatch; the `*agents-ws*` HUD is its first subscriber. The cursor and
   the speech bubble become subscribers two and three.
4. **The content pipeline EXISTS as of this week** — mission scope detection
   (`futon6/scripts/mission_scope_detect.py`) + ingest + `mission-mode.el`
   overlays: "bring up the mission with all the scopes" is a working render,
   and a newborn mission file scope-trees immediately (E-mealy-style-transducer
   and E-anatomy-of-a-proof were both born detector-readable today).
5. **The speech bubble is cheap** — posframe is loaded and cached (the
   claude-repl popup latency fix); a cursor-anchored "Done, what next?" is a
   small function, not a build.
6. **The completion-bell contract EXISTS** — agents already bell on
   completion (`ring-bell-file!` → joe/visible-bell). The posframe bubble is
   that same bell rendered AT the cursor instead of in the modeline.

## 2. MAP — bound parts (each used by name in DERIVE)

### Inventory: input
`futon0/contrib/voice-typing.el` (Whisper + ydotoold lifecycle),
`futon0/contrib/stack-hud.el` + `stack-entry.el` (the control surface).

### Inventory: actuation
`futon3c/emacs/smart-cursor.el` (companion cursor, run-script),
`futon3c/src/futon3c/peripheral/emacs_cursor.clj` (surface projection
contract), `futon3c/scripts/smart-cursor-e2e-cycle.sh` (external drive).

### Inventory: transport
`futon3c/emacs/futon-agency-ws.el` (shared WS connector, this session),
`futon3c/src/futon3c/transport/ws/invoke.clj` (`broadcast-frame!`).

### Inventory: content
`futon6/scripts/mission_scope_detect.py` + `futon3c/src/futon3c/scripts/
mission_scope_ingest.clj` + `mission-mode.el` (the scope render),
`futon6/holes/E-scope-audit.md` (the audit method missions are born under).

## 3. DERIVE — the loop, decomposed (sketch; ARGUE items marked)

1. **Voice→agent routing:** voice types into a focused input surface that
   reaches the agent (v1: the `*claude-repl:*` buffer of the driving agent;
   `voice-typing.el` unchanged). Later: a dedicated "spoken bell" surface.
2. **Agent→cursor choreography:** the agent performs edits through its
   normal tools (file writes, `emacsclient`); `smart-cursor.el` SHOWS the
   work — jumps, region sweeps, captions at each site. **ARGUE-1: this
   preserves the non-editing cursor invariant** (the cursor narrates, the
   agent's ordinary effectors act). The alternative — the cursor itself
   typing — needs a consent-gate design and is deferred.
3. **Mission bring-up:** agent writes the mission file (detector-readable
   from birth), runs detect+ingest (`mission_scope_detect.py` →
   `mission_scope_ingest.clj`), opens it in `mission-mode` with the scope
   overlays — "with all the scopes" is literal.
4. **The bubble:** a `futon-agency-ws.el` subscriber renders completion
   frames as a posframe anchored at the smart cursor: "Done, what next?" —
   the completion bell, spatialized. Dismiss on any operator input;
   never steal focus (REPL UX discipline).
5. **ARGUE-2 (identity):** who drives? The interactive CLI agent cannot be
   WS-invoked (inhabited-session constraint, M-kangaroo A1); v1 drives the
   cursor via the external harness path (`emacsclient` / HTTP) which needs
   no agent WS identity. The full agent-inhabited drive waits on W5
   (REPL/agency shared process).

## Scope

### Scope in
1. The v1 demo loop: spoken sentence → agent receives → cursor choreography
   over an existing mission → posframe bubble. All parts existing; wiring only.
2. The bubble subscriber in `futon-agency-ws.el`.
3. `smart-cursor-mode` re-enabled + attached via the shared connector
   (migration of smart-cursor onto `futon-agency-ws.el` may be v2).

### Scope out
1. The cursor typing buffer text itself (ARGUE-1 alternative; consent-gate
   design first).
2. Whisper/voice-typing changes (it works; we route focus, not audio).
3. New peripheral types server-side.

## First cut tasks
1. Live demo of the back half, operator watching: drive the cursor to
   `E-anatomy-of-a-proof.md`, enable `mission-mode`, walk three scopes,
   emit the posframe bubble. (No voice yet — proves choreography + bubble.)
2. Voice front half: `M-x stack-hud` → voice-toggle → speak into the REPL
   prompt → agent receives. (Proves routing; no new code expected.)
3. The full loop end-to-end on a real sentence: "Let's start a mission
   about keeping my Futon City map up to date."
