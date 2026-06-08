# Excursion: E-turn-dsl — a turn-prefix DSL for clocking, attribution, and phatic speech

**Date:** 2026-06-05
**Status:** IDENTIFY (design seed; captured by claude-6 + Joe, emacs-repl). No code yet.
**Repo:** futon3c (the DSL lives in the agent-chat / claude-repl surface).
**Spawned from:** the routing-bug thread (a REPL reply got dropped when bells interleaved) → the
"push to talk / phatic speech" idea → this DSL, which unifies *clocking*, *per-turn attribution*, and the
*phatic escape hatch* on one turn-prefix grammar.

## HEAD (Joe, 2026-06-05, verbatim sense)

> *"What if my turns in the Claude-REPL (or agent-chat.el more widely) get a small DSL attached, e.g., if I
> say `E: per-turn-isolation bla bla bla` we clock that one turn to E-per-turn-isolation (creating it if
> needed). You would see a mildly-transformed version of the text, e.g. `In the new E-per-turn-isolation
> excursion bla bla bla`. This would extend M-autoclock-in and the DSL could be good for both one-off turns
> and transitions, e.g., `E→per-turn-isolation bla bla bla` could translate to `I'm clocking in on
> E-per-turn-isolation. bla bla bla`. Relative to the phatic speech point, any turn like `OT: bla bla bla`
> would not be clocked."*

## The core idea

A small prefix-DSL on the operator's REPL turns. The prefix has a **clocking side-effect** and the agent
sees a **mildly-transformed** natural-language version (prefix stripped, framing prepended), so the operator
gets terse ergonomics and the agent gets clean context.

This is the **explicit operator-intent layer** that `M-autoclock-in`'s fuzzy auto-clock
(`agent-chat.el:672 agent-chat--maybe-auto-clock-from-turn`) deliberately refused to be. That auto-clock
only **fills the floor** (won't switch an active clock) and only clocks to **existing** targets — because a
*fuzzy mention* must not re-clock or invent. A **typed prefix is unambiguous operator intent**, so it earns
the two moves the fuzzy path forbids: it can **switch**, and it can **create**. It stays fully inside
M-autoclock-in's discipline: explicit-not-fuzzy (principle #8), single-active, confirmable-never-silent,
audited witness.

## The grammar → existing-infra mapping

| DSL | Semantics | Transform the agent sees | Maps to existing |
|---|---|---|---|
| `E→slug text` | **transition** — switch the persistent clock to E-slug (create if needed) | "I'm clocking in on E-slug. text" | `agent-chat-set-clock!` + creation-clock watcher (`agent-chat.el:660-670`, "fire after the file resolves") |
| `E: slug text` | **one-off** — attribute *this turn only*; persistent clock unchanged | "In the [new] E-slug excursion: text" | the turn-level capture the code already names but doesn't expose — `agent-chat.el:677`: *"a mention while already clocked is left for turn-level mention capture (NNexus-style), not promotion"* |
| `OT: text` | **phatic / off-trail** — not clocked | bare "text" (or "[off-trail] text") | skip the clock path; the marker the push-to-talk lane keys off |

Generalises across the `C › M › E` hierarchy that already exists (`agent-chat-mission-label`):
`C→`/`C:`, `M→`/`M:`, `E→`/`E:`. ASCII fallback for the arrow (`E>`) likely wanted alongside the unicode `→`.

**Hook site (already fingered by M-autoclock-in's MAP):** `agent-chat-send-input` (`agent-chat.el:1434`) —
"the user turn is available as a trimmed string… before evidence capture." Parse the prefix there →
side-effect the clock/creation → rewrite the text → emit the turn evidence carrying the attribution.
`claude-repl-send-input` (`claude-repl.el:1145`) is the REPL-specific caller.

## Why all three nested ideas converge here

- **Routing bug** = per-turn *isolation* (one claude-6 session multiplexes REPL turns + bells; superseded
  replies discarded at `claude-repl.el:997-1005`). See the diagnosis thread.
- **This DSL** = per-turn *attribution* made explicit — the same `{surface, turn-index, target}` evidence
  that would have made the bell/REPL interleave legible.
- **Phatic / push-to-talk** = `OT:` is exactly where the reliable side-channel plugs in.

One turn-prefix grammar carries clocking, attribution, and the phatic escape hatch.

## Forks to resolve (MAP/DERIVE)

1. **One-off needs its own field.** `E:` must attribute the turn *without* touching the single-active
   session clock (`agent-chat--excursion-id`) — so a turn-level `:clocked-target` on the evidence, distinct
   from the session clock. The code already draws this distinction conceptually (`:677`); the DSL makes it
   operator-driven. (One-off ≠ a momentary clock-switch-and-restore.)
2. **Create-if-needed = what, exactly.** `E: per-turn-isolation` with no existing file → write a stub
   `holes/excursions/E-per-turn-isolation.md` (minimal HEAD/frontmatter), then the creation-clock watcher
   clocks it. Needs a **stub template**. (This is the ergonomic version of how excursions have been born by
   hand all session — cf. E-interest-mining, and this very file.)
3. **Transform visibility.** "Confirmable, never silent" → surface the rewrite + clocking as a witness (like
   the existing `[auto-clock: … -> …]` system line at `:696-701`), ideally with a one-key undo, so a
   misfire is visible and reversible.
4. **`OT:` now vs the phatic *lane* later.** Cheap version: `OT:` = "don't clock" (a few lines now). Full
   version: `OT:` also routes via the reliable side-channel that bypasses the multiplex (the bigger build it
   triggers once the lane exists). Separable.
5. **Coexistence with the fuzzy auto-clock.** A prefix is authoritative (explicit); the fuzzy auto-clock
   keeps filling only the floor when there is no prefix and no clock. Clean precedence, but state it.
6. **Delimiter / slug parsing.** `E: per-turn-isolation bla` — first token = slug, rest = text. Settle the
   delimiter rule (first whitespace) and slug normalisation (kebab-case; strip a leading `E-`).

## Proposed first slice

**`E→`/`M→`/`C→` transition + `OT:` unclocked marker** — both map onto existing `agent-chat-set-clock!` +
skip, the smallest real DSL. Defers the `E:` one-off (needs the new turn-level field, fork 1) and
create-if-needed (needs the stub template, fork 2) to a second pass. Gives the explicit-switch and the
phatic marker immediately, on top of the auto-clock already there.

## Relations

- `M-autoclock-in` (`futon3c/holes/missions/M-autoclock-in.md`) — this extends it: the DSL is the explicit
  operator-intent layer over the fuzzy auto-clock. Shares `agent-chat-set-clock!`, the creation-clock
  watcher, the witness discipline, and the `agent-chat-send-input` hook site.
- The routing-bug diagnosis (REPL/bell multiplex; `claude-repl.el:997-1005` discard) — the sibling problem
  `OT:`/phatic and per-turn attribution speak to. A possible `E-per-turn-isolation` excursion (Joe's own
  worked example slug) would hold the *fix* to that bug.
- `E-repl-evidence-turns` (futon0) — the turn-evidence schema (`{session-id, agent-id, surface, turn-index,
  timestamp}`) the attribution rides on.
- `agent-chat.el` clock infra: `agent-chat-set-clock!`, `agent-chat--maybe-auto-clock-from-turn` (:672),
  `agent-chat-mission-label`, the clock hydra (`C-c C-o` / 🍒).
