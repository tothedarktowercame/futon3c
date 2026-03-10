# Technote: Response to `review-claude-repl.md` and `review-codex-repl.md`

Date: 2026-03-10

This note records the outcome of the cleanup pass driven by:

- `holes/technotes/review-claude-repl.md`
- `holes/technotes/review-codex-repl.md`

It is not a claim that the REPL work is complete. It is a response to the
reviews: what was fixed, what changed shape, and what remains open on purpose.

## Goal

The cleanup target was not "make Codex perfect." It was narrower:

1. reduce avoidable duplication between `claude-repl.el`, `codex-repl.el`,
   and `agent-chat.el`
2. move Codex closer to the Claude structure where Emacs is a thinner UI
   client over the server-managed invoke path
3. preserve or improve behavioral coherence across:
   - `*claude-repl*`
   - `*codex-repl*`
   - `*invoke: codex-repl*`
   - `*agents*`
4. answer the review findings honestly rather than by adding more prompt
   machinery

## Files changed in this pass

- `emacs/agent-chat.el`
- `emacs/agent-chat-invariants.el`
- `emacs/claude-repl.el`
- `emacs/codex-repl.el`

## Claude review response

### Addressed

- `B1` fixed.
  `claude-repl-api-url` now defaults to `agent-chat-agency-base-url` rather
  than hardcoding `http://localhost:7070`.

- `B2` fixed.
  The dead non-streaming `claude-repl--call-claude-async` path was removed.

- `B3` fixed.
  Drawbridge `defcustom`s were moved into the top configuration block.

- `B4` fixed.
  `claude-repl--init` now binds `ws` once and reuses it.

- `B6` fixed.
  Streaming retry is now bounded to one re-registration retry rather than
  recursing forever.

- `B7` fixed.
  Evidence helpers were moved into shared `agent-chat.el` helpers and both
  REPLs now call those.

### Partially addressed

- `B8` partially addressed.
  The Drawbridge path still exists as a fallback and still evaluates a raw
  Clojure form string. It was not extracted in this pass. The API reset path
  remains the primary route.

### Not addressed yet

- `B5`
  The client-side `full-prompt` wrapper remains. This needs a protocol-level
  decision, not blind deletion.

- `B9`
  Session-file race concerns remain documented but were not structurally
  changed here.

- `B10`
  The `json-parse-buffer` / `json-parse-string` inconsistency in
  auto-registration code remains.

## Codex review response

### Addressed

- `D1` mostly fixed.
  Dead CLI-only config and helpers were removed:
  - `codex-repl-codex-command`
  - `codex-repl-sandbox`
  - `codex-repl-approval-policy`
  - `codex-repl-model`
  - `codex-repl-reasoning-effort`
  - dead CLI JSONL parser helpers

  One caveat: a small `agent_message` extractor had to be reintroduced after
  live testing showed that some real server-managed Codex completions still
  deliver the final answer through `item.completed` payloads.

- `D3` fixed.
  Evidence helpers were extracted to `agent-chat.el`.

- `D10` effectively fixed.
  The old Codex-local Emacs-26 compat JSON branch disappeared with the removal
  of the old helper; shared parsing now lives in `agent-chat.el`.

- `D11` effectively fixed, but not in the way proposed.
  The bespoke Codex evidence query helper was removed entirely. Shared evidence
  fetching now uses the `agent-chat.el` implementation.

- `D12` fixed.
  Base streaming begin/append/end behavior moved into `agent-chat.el`.

- `D17` fixed.
  The legacy `codex-command` variable is gone.

### Partially addressed

- `D2` partially addressed.
  We did not fully unify progress parsing and trace summaries into one function.
  Instead, we split the problem more honestly:
  - `*invoke: codex-repl*` keeps a detailed trace summary
  - `*codex-repl*` now uses a separate compact inline renderer

  This reduced user-facing noise even though some duplicate event matching
  still exists.

- `D6` partially addressed.
  The modeline now reflects the actual server-managed invoke path and uses the
  resolved API base, but the broader "diagnostics should not live in redisplay"
  criticism still stands.

- `D9` partially addressed.
  Drawbridge parsing still falls back from a JSON parse attempt into EDN-like
  extraction. The wasted-parse critique is still valid.

- `D14` partially addressed.
  The stale-session handling remains because the failure mode is still real,
  but the code was not simplified or newly documented in this pass.

### Not addressed yet

- `D4`
  The invoke dashboard still exists and is still on by default.

- `D5`
  Routing diagnostics remain in `codex-repl.el`.

- `D7`
  Registry heartbeat remains.

- `D8`
  The thinking heartbeat still exists and still refreshes live UI state.

- `D13`
  The `surface-contract` helper still needs a clearer disposition.

- `D15`
  The stale-buffer repair path remains.

- `D16`
  IRC-send machinery remains embedded in `codex-repl.el`.

- `D18`
  Session-clearing helpers were not fully rationalized.

## Additional work not explicitly requested by the reviews

The review-driven cleanup exposed real live bugs. These were fixed during the
same pass because leaving them behind would have invalidated the behavioral
comparison.

- Codex REPL is now server-managed rather than locally shelling out to
  `codex exec` from Emacs.

- Codex final answers now render correctly even when the server stream emits
  them as `item.completed` / `agent_message` instead of plain `text` chunks.

- Codex inline narration was made less noisy by separating:
  - main-buffer narration
  - invoke-trace detail

- `agent-chat-invariants.el` was integrated into both REPLs:
  - `agent-chat-invariants-setup` is called during REPL init
  - streaming-completion paths call `agent-chat-invariants-turn-ended`

- The invariants hook storage was corrected to use the named function symbol
  rather than a compiled function object, because some code paths expected the
  hook value to be symbol-shaped.

## Behavioral outcome from live retests

The important result is not just code reduction.

Side-by-side live tests against `futon5@ada2533` showed:

- Claude still has the better live user experience.
  It narrates its own work in a way that is easy to follow.

- Codex REPL is now materially more coherent than before.
  State agreement between the REPL, invoke trace, and agent view improved.

- Codex still has a closure-quality and latency problem independent of the old
  surface incoherence.
  In a bounded edit/test task, Codex eventually completed the local patch, but
  it was much slower and its narration remained structurally poorer than
  Claude's.

That distinction matters. A large class of "surface lies about what Codex is
doing" bugs has been reduced. What remains is closer to a real model/workflow
problem rather than pure harness confusion.

## Current verdict

The reviews were substantially correct.

This pass did produce real convergence:

- less duplicated evidence code
- less duplicated streaming code
- no dead Claude non-streaming path
- no dead Codex CLI-era config path
- more honest server-managed Codex semantics

But it did not finish the job. The remaining large Codex-specific diagnostic
and routing machinery still needs either extraction or deletion, and Claude
still has a few unresolved protocol/cleanup questions of its own.

So the right summary is:

- review findings used as intended
- multiple high-value issues actually fixed
- parity improved
- architectural cleanup still incomplete
