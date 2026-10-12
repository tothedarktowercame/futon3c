# SHARD-NOTE — moves-scale3 shard 5 (zai-5, 2026-10-12, batch 3 wave 2, M-diagramprover)

57 sidecars, all `validate.py` exit 0 with void 0 (build-time substring
verification against each act's own authority text; the validator
re-confirms, resolving open_ports ids as targets).

## Counts

- 218 acts labelled (131 operator fragments, 87 reply acts); 0 unlabelled.
- 93 answers: 21 closes / 72 keeps / 0 unclear.
- Scopes: 29 opens, 99 within, 0 closes (no explicit "back to X" closer in
  this shard; the one redirect-type turn, turn-YljeQ1, redirects by naming
  what to return to, which I labelled as within, not closes).
- `within` targets drawn from `open_ports`: 15; `answers` targets from
  `open_ports`: 0 (every answered act was in the record's own fragments —
  the kernel's stale ports stayed unlinked, which is the expected signal
  for the sole-port rule test).

## Invented labels (with reasons)

- `propose/tentative` x8: the batch-2 spelling for "I wonder / we could /
  maybe" proposals; vocabulary.json carries `propose/other` and
  `propose/alternative` but neither names the tentativeness, and the
  batch-2 rule prefers the dialogue-shape name. Candidate for
  consolidation.
- `report/acknowledge` x1 (turn-pYwIRA r-0, restating an assigned
  responsibility): vocabulary has no acknowledgement label; candidate.

Everything else used vocabulary.json labels verbatim (report/done,
report/dispatch, report/status, report/in-flight, report/partial,
explain/mechanism, qualify/caveat, verify/check-live, ...).

## Five most-cited openers (all open_ports ids)

1. claude-4-turn-6-f-s1-0 "Arxana to align Lean's source with its published
   documentation" (5 citations as within)
2. claude-4-turn-3-f-s1-0 "build a DiagramProver checker adapted to Lean
   itself" (4 — the mission's own founding opener)
3. claude-4-turn-42-f-s1-0 "make our own fork of mfuton-share" (2)
4. claude-4-turn-17-f-s1-0 "bell claude-18 to take over on the LSP stuff" (1)
5. claude-4-turn-12-f-s2-0 "documentation hypergraph annotations may provide
   a layer" (1)

## Input anomalies

- Same `acts`-text truncation as batches 1-2 (literal "…", validator
  prefers `acts`); quotes drawn from visible prefixes (e.g. turn-X6Mqqz
  f-s4-0, turn-dZmpQh f-s2-0).
- Two records carry pasted-id fragment ids (`claude-4-turn-21@turn-jedqCo-*`,
  `claude-4-turn-341@turn-mflxD1-*`) — labelled as given.
- Sub-8-char fragments exist and were quoted whole with a note
  ("Hm...", "yes", "Sounds great", "*compile that*." is truncated in the
  source itself).
- open_ports lists are highly stable across records (same early-session
  ports repeating in nearly every record) — no wrong-looking entries
  spotted, but none of them was ever answered by a reply act in this
  shard.

## Time

About 2.5 h of seat time: reading 57 records and labelling ~2 h, fixture
fixes and validation 0.5 h.
