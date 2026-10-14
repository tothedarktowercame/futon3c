# SHARD-NOTE — move-grammar batch 3 wave 5, shard 13 (zai-3, 2026-10-14)

80 sidecars, `validate.py` exit 0, **void 0** on all 80 (374 acts).

## Counts

- 374 acts: 301 operator fragments, 57 reply acts (13 records with marked
  replies), 0 unlabelled.
- Answers: 25 closes / 6 keeps.  Scopes: opens 0 (this wave opens nothing —
  the sessions are continuations), within 22, closes 0.
- Targets: 11 answers target `in_force` ids (claude-16's standings: the
  reply paragraphs answer standing-rule asks recorded by the kernel);
  0 within/answers target `open_ports` — the stale-port situation of waves
  3–4 persists here, so I refused them.
- Sessions: 22 matrix proof-room turns (t94A01 Hilbert-cube dialogue),
  claude-14 ×25 (topo loops / blueprints), claude-16 ×10 (M-象-2000 owner),
  claude-17 ×10 (小象/gap tooling), claude-1/3/11 ×7, codex-75 ×1, +2 misc.

## Vocabulary

43 distinct dialogue-act moves.  Dominated by explain/mechanism (155) — the
matrix proof-room turns are proof/checkpoint prose; explain/role-marker (27)
is INVENTED for their `[role | mNNNN]` headers (nothing in the vocabulary
names a framing marker), and the matrix records are otherwise labeled by the
consolidated set (report/done 22, delegate/to-agent 28 for their `@role`
mentions).  Also invented: report-problem/blocked (matrix turns stating
"no further proof work is authorized"), report/relay (claude-17 relaying
claude-3's comments), report/with-options (claude-16's numbered example
lists), challenge/premise (carried from wave 4).

## Most-cited openers

Only in_force ids were cited (11 answer links); top: none exceeded 2.  The
kernel's open_ports lists remain months-stale (claude-17's turn-2..37 ports
in every record; claude-14's turn-18..20 in every record) — refused.

## Input oddities

- Matrix proof-room records have fragment ids prefixed `matrix:proof-<role>-
  <ts>-<n>`; several contain a JSON block as a fragment (the scribe
  checkpoint) — quoted as-is.
- turn-3vqBlD has 23 fragments including three bare `**1.`/`**2.`/`**3.`
  stubs (kernel split artifacts, <8 chars) — left unlabelled-by-omission
  (3 acts); their content lives in the long numbered fragments.
- claude-14 turn numbers restart per session (turn-70/72/69/89/96 appear in
  multiple records with different texts) — ids are session-unique but
  builder-naive matching needed care; verified via input files.
- Typos kept verbatim ("orgin", "simklar", "ghe remaininv", "revions",
  "origianal", "vairious", "shoild", "seemd", "effdctive", "tjme",
  "pussyfoit", "replucate", "loose track", "tp write").

## Time & budget

About 2 h.  No 429 or 500 encountered.
