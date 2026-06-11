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

## 4. ARGUE (opened by voice, 2026-06-11 — "Let's start the argue phase.")

### A1 — the cursor narrates; it does not type

IF the spoken loop needs the operator to see work happening in the editor,
HOWEVER a cursor that can type is a hand that can act without being watched,
THEN the companion cursor only ever points, sweeps, and captions, while the
agent's ordinary tools (file writes, the editor server) do the changing,
BECAUSE the proof that the work happened should come from the same channel
the work actually used — and a pointer that cannot edit needs no consent
machinery to be trusted.

### A2 — one voice channel, owned at the seam where text is born

IF speech should reach the agent no matter where the operator's cursor sits,
HOWEVER keyboard focus is the operating system's router and it follows the
operator's attention, not their intent,
THEN we intercept at the single point where the voice system turns sound
into text, and deliver that text to an inbox the agent watches,
BECAUSE the place where something is born is the only place it can be
redirected without chasing it. (Decided AND built today; the toggle file
means the old behavior is one `rm` away.)

### A3 — the driver stays outside until the house has two doors

IF the full vision has the agent inhabiting the editor connection directly,
HOWEVER a live working session can only be lived in once (the fable-1
lesson), and the warm-process work has not yet given one session two doors,
THEN version one drives the editor from outside, through the same door any
shell command uses,
BECAUSE a demo that works today through the plain door teaches us more than
an architecture that waits for the elegant one.

### A4 — the agent needs its own cursor, because authorship must be visible (Joe, spoken challenge, 2026-06-11)

> "What's missing is the argument for an actual cursor, because I can't have
> you impersonate me and move my cursor. You need your own cursor, so that
> when we open the argue phase, the frame will refresh and we'll see that
> *you* have just typed text into that argue phase, not me."

IF two minds work in one editor, the operator must always be able to tell
whose hands did what,
HOWEVER the way the helper's changes arrive today — a file rewritten and the
buffer reloaded — makes new text appear with no author at all, which is a
quieter kind of impersonation than grabbing the operator's cursor,
THEN the helper gets its own visible cursor (the ◆ companion that already
exists), and its changes enter the buffer AT that cursor, under its glyph
and caption, while the operator's own cursor is never moved or borrowed,
BECAUSE attribution has to be something you can SEE at the moment of change,
not something you reconstruct from a log afterwards.

**A1 amended accordingly (A1′):** the invariant is non-impersonation, not
non-editing. The helper's cursor may type — as itself, visibly. What remains
forbidden is acting through the operator's cursor, and acting with no
visible body at all. (The consent question shifts from "may it edit?" to
"is its identity unmistakable while it edits?" — which is the editor-grain
form of the agent-identity invariant the Agency already enforces.)

### A5 — concurrent editing deferred; the demo's concurrency is modal (Joe, spoken, 2026-06-11)

> "The question I would have is whether we need to also use CRDT — but I
> don't think we need that for a first demo, because I'm actually not typing
> into the same buffer you are. I'm just talking with my voice."

Resolved as spoken: in v1 the operator speaks and the agent types — two
bodies, two channels, no contention for one buffer. CRDT-grade machinery is
the answer to a problem this modality does not have. Recorded so it is
reached for at the right moment and not before: it becomes live only when
both cursors type into one buffer in the same span of time — and even then,
the editor's command-grain seriality means the real problem will be
intention conflict, not byte conflict. Revisit when a second typing body
exists and the modal discipline is deliberately broken.

### A6 — witnessed: this paragraph was typed by the agent's own cursor (fable-2, 2026-06-11)

IF authorship must be visible at the moment of change,
HOWEVER every earlier edit in this mission arrived by file-write and reload — authorless,
THEN this argument is its own witness: it entered the buffer character by character at the ◆ cursor you just watched, while your cursor stayed yours,
BECAUSE the best argument for a typing body is the sight of one.

### A7 — the form of the first demo (argued by voice, typed by the body)

IF the demo must prove the whole loop and not a montage of parts,
THEN it is one unbroken spoken scene, six beats, no hands on the keyboard:
(1) the operator speaks a mission request; (2) the words reach the agent
through the inbox, off-focus; (3) the agent authors the mission file,
detector-readable from birth; (4) the ◆ cursor brings it up in
mission-mode, scopes live; (5) the cursor types the HEAD into the buffer,
visibly, as itself; (6) the bubble asks — done, what next — and the next
spoken sentence begins the ARGUE round.
BECAUSE each beat is the witness of the one before it: a montage can be
faked, a scene cannot.

### 4.1 Plain-language argument (the version anyone can read)

You speak. The words go to the helper instead of wherever your cursor
happens to be. The helper has its own cursor — you can always see it, and
you can always tell it apart from yours. When text appears, it appears
under whichever cursor wrote it, so you never have to wonder who did what.
Your cursor is yours alone: never moved, never borrowed. When the helper
finishes, it tells you so, right there on the screen. And if you want your
old voice-typing back, you delete one file.

## 5. VERIFY — does the build carry the demo (opened by voice, 2026-06-11)

Feature checklist, evidence-first; the demo is GO when every demo-blocking
row is BUILT.

1. voice reaches the agent off-focus — BUILT (shim 60ae5b6; this very
   section was commanded by voice).
2. agent authors and ingests missions — BUILT (this file; the scope count
   in the header line is the receipt).
3. visible agent cursor with identity badge — BUILT (a865865).
4. typing body — BUILT (A6 is its own witness).
5. navigation, including take-operator — BUILT (18d7dfe; the operator was
   carried to ARGUE by it).
6. completion bubble — BUILT, hand-fired; auto-fire from completion events
   via futon-agency-ws is a follow-on, not demo-blocking.
7. one-command demo launch — MISSING and DEMO-BLOCKING: a cold Emacs must
   reach demo-ready state from a single entry point (load the three el
   files, register the observer, connect, enable).
8. revision primitive — the body can type but not yet delete. MISSING, not
   demo-blocking: the six beats only add text.
9. smart-cursor.el migration onto the shared connector — deferred to v2.

Row 7 — BUILT: scripts/demo-ready.sh, first run all green — observer
registered, ws open and ready, posframe + mission-mode + agent-cursor
loaded, voice route ON, six agents on the roster. The body cannot yet
strike the MISSING above (see row 8); let this line stand as the
correction.

Method ruling (Joe, spoken — and FIRST MISHEARD by the transcription
channel as plain reason; the body struck its own error and retyped this,
discharging row 8 in the act): verify with a REAZON logic model —
miniKanren in Emacs Lisp, logic-model-before-code in the substrate of the
build itself. core.logic stays for JVM designs; Emacs verifies Emacs.
Reazon is confirmed loaded in the operator Emacs. VERIFY therefore holds
at the gate until the scene invariants run as Reazon relations: one
conforming trace satisfying all of them, and one adversarial trace per
invariant, each caught.

Exit: build row 7, rehearse the scene once end to end, then the demo is
real.

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
