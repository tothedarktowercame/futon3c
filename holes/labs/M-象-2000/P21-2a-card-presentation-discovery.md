# P21-2a — what a pattern-card selection was shown

Read-only live sample: 2026-09-28T22:35Z, `GET :7073/api/alpha/hyperedges?type=pattern-card%2Fselection&limit=20&include-total=false`. The store returned two records total, with no cursor.

## What is shown, and what is stored

An ordinary exact-seat turn renders the prompt-line `:pattern` segment as a
`Prompt: pattern ~id (...)` header (`transport/http.clj:4590-4615,4617-4645`).
The provider prefers an active card, whose value is its one selected id and
whose basis is the selection act; otherwise it shows the top context-retrieval
id plus two alternatives (`agency/pattern_card_provider.clj:65-83,271-290`).
Emacs session mode separately renders the top context-retrieval id as a sigil
after the turn (`emacs/session-mode.el:125-129,179-210,379-404`).

The retrieval list is durable: its evidence records the exact agent, session,
query and ranked results, and the exact-seat cache retains its evidence id
(`dev/futon3c/dev.clj:937-960`; `agency/pattern_card_provider.clj:126-149`).
But a selection contains only author, agent, session, pattern id and time; its
route accepts no presentation reference or shown-id list
(`agency/pattern_card_record.clj:20-47,79-101`;
`transport/http.clj:9370-9398`). Thus no durable record says **which display**
the selection answered. An active card's basis points back to that same
selection, so it cannot establish the pre-selection presentation.

## Live join result

Both records select `musn/intent-restatement` for claude-17 session
`564c8e50-c240-46fc-81ad-55afafbc63ee`:

- `act:0c93c8e3-b180-4ce9-a078-9f9ea8c6eec3`, at
  `2026-09-28T12:44:40.545Z`. The latest prior exact-session retrieval is
  `e-293ff559-0c78-4ba4-bdae-768450924a00` at 12:44:26.948Z; it lists
  `memory/no-tightening-while-held`, `memory/verify-in-the-serving-process`,
  and `snatch/re-enter-after-observed-repair`, not the selected id.
- `act:cb9bff2a-b65d-4bf0-b32e-a94b383cf594` has no `:at`, so even a temporal
  correlation is unavailable.

Exact presentation-id joins: **0/2**. Records that cannot be joined: **2/2**.
Echo: **0 known**; independent: **0 known**; presentation-unknown: **2**.
Session plus a time window is only correlation and must not be promoted to a
presentation witness. This is the gap DERIVE-2 identifies
(`holes/missions/M-象-2000.md:513-518`).

## Smallest P21-2 packet

Add a closed `:pattern/attestation` act containing pattern id, attester, time,
use ref, basis turn, proposal author, a presentation ref plus its verified
shown ids, and disposition `:counts | :proposer-warrant |
:shown-list-echo | :presentation-unknown` (the earlier shape is at
`P21-0-structural-match-discovery.md:95-110`). Extend the selection request so
the writer accepts a presentation evidence id, resolves that exact record,
copies its ranked ids, and writes the attestation beside the verified selection.
Absent or unresolvable presentation yields `:presentation-unknown`; it never
guesses from session/time.

Tests should pin closed validation, presentation/seat matching, proposer
self-use, echo detection, unknown presentation, and the BUILD-PLAN case: two
uses of one pattern, one whose selected id was shown and one independently
presented, produce two records but only the latter has `:counts`.
