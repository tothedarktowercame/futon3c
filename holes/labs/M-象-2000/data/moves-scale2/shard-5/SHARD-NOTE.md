# SHARD-NOTE — moves-scale2 shard 5 (zai-5, 2026-10-11, batch 2 re-dispatch)

24 sidecars, all `validate.py` exit 0 with void 0 (quotes verified
substring against each act's own authority text at build time).

## Counts

- 135 acts labelled (54 operator fragments, 81 reply acts); 0 unlabelled.
- 98 answers: 29 closes / 69 keeps / 0 unclear.
- 123 acts carry scope; one explicit `closes`: turn-TTgUYF r-2 closes the
  "decide-later repos" opener f-s2-0 (outage-recovery set discharged).
- Sole-port observation for the kernel test: the single opener is NOT
  always the answered port — e.g. turn-Q2aKgI f-s1-0 (approve) is answered
  keeps-only while f-s2-0 carries both closes; turn-v2AHXA r-0 closes three
  ports at once.

## Invented move labels (counts; beyond the schema seeds)

report/evidence 15, report/diagnosis 4, report/observation 4,
explain/context 4, report/inventory 3, retract/correction 3,
propose/tentative 3, qualify/risk-assessment 2, qualify/impact 2,
clarify/context 2, and 1 each: report/gap, propose/directive,
qualify/scope, report/dispatched, report/acknowledge, clarify/assertion,
approve/provisional.  Batch-2 rule applied: no topic-shaped labels
(one late propose/design on an operator directive was relabelled
propose/directive before commit).

## Multi-port incidences worth a second look

- turn-v2AHXA r-0 answers f-s1-0 + f-s2-0 + f-s2-1, all closes.
- turn-wDJkVr r-3 answers f-s1-0 and f-s2-0 (both keeps).
- turn-TTgUYF r-2 answers both f-s1-0 and f-s2-0 and closes the f-s2-0
  scope.
- turn-ZHPLjj r-0/r-1/r-2 each answer both f-s2-0 and f-s3-0.

## Input anomalies

- Same as batch 1: where `acts` texts exist they are upstream-truncated
  with a literal "…" and the validator prefers them; quotes were drawn
  from the visible prefix (turn-OdFzQ8 r-0 note records one mid-sentence
  truncation).
- turn-vK4wYJ r-1 is a header-only paragraph ("(closure programs, in
  priority order)" — the table below is not in reply_acts' text? it is,
  but the label mark is the whole visible prefix in `acts`).
- turn-RHQbI4/turn-v2AHXA are the 象 demo pair: two-word fragments
  ("see *Occur*" is turn-ZHPLjj) and one-word approvals are quoted whole.

## Time

About 2.5 h seat time (interrupted once by the 429; 21 sidecars from the
first attempt reused after re-validation, 3 written fresh in the
re-dispatch).
