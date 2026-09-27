# SEAM: the prompt line — a line other subsystems write into

claude-17, 2026-09-27, at Joe's request ("can we expose that as a seam that other agents
can build on?"). Extends decision P7a (the top pattern shown like a shell prompt). Joe
relayed codex-5's proposal for inbox-zero markers; it is adopted below.

## What the seam is

A small registry of **segments**. A subsystem contributes a segment; the REPL and the
per-turn header render whatever segments are present. No subsystem edits the renderer.

    {:segment/id     :pattern                 ; or :inbox-zero, :surface, …
     :segment/value  "象/诺必践"
     :segment/marker "*"                      ; optional one-character suffix
     :segment/provider "futon3c.agency.pattern-card"
     :segment/as-of  "2026-09-27T19:40:00Z"
     :segment/basis  {:evidence/id "…"}}      ; what the claim rests on, auditable

Rules:
1. **Absence is no claim.** A missing segment or marker never means "fine" or "clean".
2. **Providers are reads, never gates.** A provider that errs or exceeds its time budget is
   omitted from that render (rule 1 applies); the prompt is never delayed or blocked.
3. **Every segment carries provenance** (`provider`, `as-of`, `basis`), so the line
   itself can be audited later (P6o and P21 spirit: who says so, on what).
4. **One writer per segment id.** Two providers for one id is a registration error.

## Two renderings

- **REPL prompt** (for Joe): compact, in front of the prompt, e.g. `$象/诺必践*> `.
- **Per-turn header** (for the agent), spelled out beside the surface facts:
  `Prompt: pattern 象/诺必践; inbox-zero: own changes outstanding (verified)`.

## First two segments

**`:pattern` (P7a).** The pattern in force for this agent's turn: the active pattern card
if one is set (P10: an agent may swap its card at once), else the top retrieved pattern
for its last turn. Provider reads the existing retrieval (session-mode sigil /
context-retrieval evidence) and backpack/PSR facilities; it does not re-run retrieval.

**`:inbox-zero` (codex-5's markers, adopted).**

| prompt | meaning |
|---|---|
| `$象/诺必践>` | agent and current pattern; **no claim** about cleanliness |
| `$象/诺必践*>` | outstanding changes **verified as belonging to this agent's current work** |
| `$象/诺必践?>` | dirty files exist in the shared checkout; **attribution unresolved** |

A verified-clean state is deliberately not shown: by rule 1, the absence of a marker is
not a cleanliness claim. If a clean claim is ever wanted it needs its own marker and a
basis, not the absence of `*` and `?`.

## Constraints for the implementation packet

- The REPL prompt is found by the literal `"> "` (`emacs/agent-chat.el:847-862`,
  `agent-chat--ensure-prompt-markers!`) and is read-only for a recorded reason
  (`agent-chat--insert-prompt`, 2026-09-24 incident). A prefix must keep the literal
  `"> "` suffix, keep the whole prompt read-only, and update prompt detection and repair
  to allow a prefix. Test: the 2026-09-24 kill-backward case still cannot delete it.
- `"^> "` elsewhere is a markdown blockquote; the prefix must not make those ambiguous.
- The header line is added where the surface contract is built
  (`src/futon3c/transport/http.clj` ~4535-4549), as facts, not instructions.
- Seats without a provider render exactly as today.

## Who builds what

The renderer and the `:pattern` segment are M-象-2000's (P7a). The `:inbox-zero` segment's
provider belongs to codex-5's inbox-zero claim-lifecycle work, written against this seam.
