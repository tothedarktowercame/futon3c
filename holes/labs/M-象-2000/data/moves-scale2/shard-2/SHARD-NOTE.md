# SHARD-NOTE — move-grammar batch 2, shard 2 (zai-2, 2026-10-11)

`python3 -I ../../../moves/validate.py *.moves.json`: exit 0, **59 files
valid, void=0** (every quote substring-verified at build; the builder
replicated the validator's authority order).

## Counts
- 328 acts labelled, 1 unlabelled (turn-4f0Exc f-s1-1 "(yes)": whole text is
  5 chars, under the 8-char quote minimum).
- 230 answers: **149 closes / 81 keeps / 0 unclear**; 98 scoped acts
  (opens/within chains; no explicit `closes`).
- 54 distinct move labels; seeds and pilot inventions reused (report/diagnosis,
  report/evidence, report/gap, report/in-flight, report/partial,
  report/fact-check, qualify/risk-assessment, propose/ordered-plan,
  propose/explanation-outline, ask-action/explain-failure, retract/correction,
  report-problem/grievance, challenge/necessity, report/ack, report/planned,
  prioritize/urgent-fix, redirect/topic, report/post-mortem).

## Invented this batch (dialogue-act labels, not topics)
report/inventory x9, delegate/relay-question x3, promise/commitment x2,
explore/hypothesis x2, report/re-baseline, report/redirect-issued,
report/evidence-chain, challenge/analogy, challenge/proposal,
agree/with-reasoning, delegate/retire-seats, prioritize/efficiency,
explain/apology; batch-1 labels reused (report/job-spec,
delegate/handoff-spec x5 each, constrain/teach-helpers).

## Single-opener records: answer-rate findings (sole-port test set)
- Every record has exactly one opening fragment, yet 81 answers keep the
  opener owed: turn-2pTVKc (both packets keep the 2-incorrect-rows opener),
  turn-DGJKIz r-2..r-4, turn-LakryF (branch begun, compile not shown).
- Same-paragraph closes of opener AND a previous-turn act: turn-4f0Exc-r-0
  (f-s1-0 and prev 317-r-4), turn-OVLgZb-r-1 closes the status-check while
  r-2 re-opens a decision; turn-DblTYM-r-0 closes both operator fragments.
- No-answer reply acts are rare: only pure-relay/housekeeping headers
  (turn-Q0dTrM-r-1, turn-dg3QZG-r-2 tail, turn-1UTHJA-r-2).

## Oddities in the inputs
- `:acts` entries are 120-char truncated and win authority whenever the id
  appears there, so quotes come from the prefix; noted per act where the key
  sentence fell past the cut (turn-wxNpi9 f-s1-0's pin ask, turn-C42bzz
  f-s1-0, turn-drztA5 f-s2-0's causal header).
- Empty/header-only reply bodies upstream: turn-Q0dTrM-r-1, turn-5EMInw-r-4,
  turn-gVrTPx-r-3 (noted per act).
- `@turn-…`-suffixed ids on both acts and fragments/reply_acts for seven
  records (ioGJZn, LvQ6Vg, NmxOoQ, oPHLtM, tpHYvQ, zwTFEs, nVbzFX); those
  ids used as-is, which the validator resolves.
- turn-LakryF-r-0 is one reply_act holding three glued marks (starting/setup/
  waiting/status) — labelled as one act, noted.

Time: about 5 h across two sittings (the 429'd attempt's 5 sidecars reused).
