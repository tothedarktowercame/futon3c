# P6 — Conductor peripheral (spec for operator ruling)

*claude-7, 2026-08-16. Nothing here is built. The three decisions at the end
are the ruling surface; everything else is context. Implementation follows as
small packets to non-codex-4 lanes after the ruling.*

## Why (one paragraph)

Round 1's audit (W.18): every peripheral-ized role held protocol flawlessly;
every recorded violation traced to the one agent operating outside any
peripheral — the conductor, who used the system as a library. For the carded
seats, violations were inexpressible; for the conductor they were merely
prohibited, and at conversational timescale prohibited decays to done. The
fix is the same move the whole stack is built on, applied one level up: the
conductor inhabits a room whose walls are the protocol.

## What already exists (I-4 inventory — this is wiring, not writing)

- **The cycle engine is the room.** `futon3c.peripheral.problem` already
  phase-gates every tool: a tool absent from `base-phase-tools` for the
  current phase cannot be stepped. The walls exist; the conductor just never
  stood inside them.
- **The runner protocol is the inhabitation interface** (`runner/start`,
  `runner/step`, `runner/stop`) — same as every other peripheral (I-3).
- **The conductor namespace** (`futon3c.apm.conductor`) already wraps the
  engine: `open-frame!`, `dispatch-solver!` (atomic dispatch+park),
  `guide-solver!`, `dispatch-student!`, `adjudicate!`, `close!`, `resume`.
  Today these are library calls from a REPL; under P6 they become the ONLY
  effectful surface.
- **Gates already merged this round:** seat keys + guide≠proctor at
  `:register` (P5); promotion = attach-then-review with reviewer≠depositor
  (P3-impl); scribe-lane reports seat-locked to `:reg/scribe-seat` (P4);
  operator pane rendering seats and phase (P7).
- **Precedent for an inhabited driving role:** the War Machine pilot
  (`README-pilot.md`) — a REPL inhabitant with hard disciplines. P6 is that
  pattern with the disciplines moved from documentation into the tool
  surface.

## The shape

- **Entry** at frame launch: `conduct!` (name TBD) binds agent-id + session
  to an open problem cycle. Entry REFUSES if the registration has not passed
  `:register` gates (unstaffed seats, guide==proctor — P5 findings) or if
  the entering agent is not the registered `:reg/guide-seat`.
- **Inside**, the conductor's effectful surface is exactly the conductor
  namespace fns, each phase-gated by the live cycle (the engine already
  refuses out-of-phase tools). Read-only observation is unrestricted:
  job status, roster, blackboard, worktrees (surface contract, not
  capability restriction — CLAUDE.md).
- **Absent from the surface** (the walls, all with round-1 incident
  precedent): raw `record-memory!` (W.15 off-machinery scribe pass); raw
  bells to arbitrary recipients (duplicate-guidance incident — only
  seat-addressed ground-control dispatches exist); close without a fired
  stop-rule (W.12 premature close); mid-cycle namespace reloads.
- **Exit** at the sentinel, or via a fired stop-rule. No other exit
  expresses.

## The three decisions

### D1 — Enforcement strength (the central one)

How hard are the walls?

1. **Convention**: the conductor namespace is documented as the only
   entry; nothing stops a Drawbridge eval. (This is round 1. It failed.)
2. **Audited single entry-point**: all conductor fns require the bound
   inhabitation handle; a **proctor-side audit** diffs Agency rows and
   store writes against the cycle's recorded steps — any effect with no
   matching step is a `:off-surface-action` finding in the envelope.
   Violations are possible but never silent, and they fail the cycle.
3. **Transport-enforced**: conductor turns route THROUGH the peripheral
   (like IRC posts through the chat peripheral) — off-surface action is
   physically inexpressible. Strongest; most build; touches the warm-pouch
   surface layer.

**Recommendation: 2 now, 3 later.** Option 2 is buildable this round from
existing parts (the proctor seat + Agency log + store audit — the same
mechanism as the retro-proctor audit, run per-cycle), and it converts
"witnessed-not-enforced" into "witnessed-and-cycle-failing," which is the
property that actually changed behaviour everywhere else in round 1.
Option 3 is the right end state and a natural M-diagramprover-era slice.

### D2 — Guidance typing in the tool

`guide-solver!` today accepts any text. The plan-first ruling (W.11) and
the content-hint incident (frame-4 refuted hint) were held by discipline
only. Should the tool take a declared type — `:process` | `:content` — and
**refuse `:content` guidance** in rounds where the registration says so
(a `:reg/guidance-regime` pin)?

**Recommendation: yes.** It makes the plan-first regime a registration
pin instead of a memory, and the P1 guidance count gains a type column
for free (the typed-guidance specimen from W.7 wanted exactly this).

### D3 — Other seats' tools on the conductor surface

Does the conductor get relay access to other seats' tools (e.g. invoking
`:record-scribe-lanes` on the scribe's behalf), or none at all?

**Recommendation: none.** Seats act through their own dispatches; the
conductor dispatches TO seats and reads results. Round 1's scribe pass
went off-machinery precisely because the conductor could impersonate the
lane. P4's authorship lock already points this way; P6 makes it uniform:
if a seat's output is missing, the cycle records it missing — the
conductor cannot paper over an unstaffed or silent seat. (This is I-3 at
the role level: inhabited, not delegated.)

## Implementation plan after ruling (small packets, no codex-4)

1. Inhabitation handle + entry/exit gates (entry refusal tests).
2. Conductor fns require the handle; off-handle calls refuse.
3. Per-cycle proctor audit producing `:off-surface-action` findings into
   the envelope (D1-2).
4. Guidance typing + `:reg/guidance-regime` pin (D2).
5. Blackboard: render the inhabitation (who is inside, since when).

Each is one behaviour, one reviewed packet, dispatched to codex-3 /
ams-codex-1 / ams-codex-2 (codex-4 reserved for the operator's interview
and the solver seat).
