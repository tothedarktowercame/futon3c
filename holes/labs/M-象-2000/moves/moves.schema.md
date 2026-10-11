# turn-<id>.json.moves.json

```json
{
  "record": "turn-XXXXXX",
  "labeller": "zai-1",
  "schema": "moves/1",
  "at": "2026-10-11T18:00:00Z",
  "acts": [
    {
      "act": "<act id from the record's :acts, or <turn_id>-f-<s>-<i> / -r-<i> when :acts is absent>",
      "kind": "<the kernel kind as given>",
      "move": "<kind>/<move>",
      "quote": "<verbatim span from the act's text, 8+ chars>",
      "answers": [
        {"act": "<operator act id>", "quote": "<verbatim span in THIS paragraph showing it answers that act>", "effect": "closes|keeps|unclear"}
      ],
      "scope": {"opens": "<short name or null>", "closes": "<act id of the scope opener or null>", "within": "<act id of the governing scope opener or null>"},
      "confidence": "high|medium|low",
      "note": "<one sentence, optional>"
    }
  ],
  "unlabelled": ["<act ids left unlabelled, with reason in notes>"]
}
```

Moves: an open vocabulary written `<kind>/<move>`, lower-case, hyphenated.
Seeds (use when they fit, invent when they do not, never force):
- operator: `ask-action/status-check`, `ask-action/commission`,
  `propose/redirect-scope`, `propose/alternative`, `approve/plain`,
  `approve/with-amendment`, `constrain/standing-rule`, `constrain/for-now`,
  `clarify/ask-decision`, `clarify/check-understanding`, `delegate/to-agent`,
  `report-problem/observed`, `report-problem/suspected`, `defer/until`.
- agent: `report/done`, `report/partial`, `report/with-options`,
  `explain/decline`, `explain/mechanism`, `verify/checked-live`,
  `ask-action/one-decision`, `propose/next-step`, `qualify/caveat`.

Answers: list every operator act of the same turn (or a visible earlier
offer) that the paragraph bears on; many-to-many is expected. `effect`:
closes if the paragraph settles what was asked, keeps if it bears on it but
leaves it owed, unclear otherwise.

Scope: `opens` names a region the act starts ("the hook first", "M-象-2000",
"no second agent reads turns"); `closes` points at an earlier opener this act
ends ("back to the plan"); `within` the opener that governs how this act
should be read. Null when none.
