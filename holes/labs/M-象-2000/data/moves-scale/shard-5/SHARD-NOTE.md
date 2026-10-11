# SHARD-NOTE — shard 5 (zai-5, 2026-10-11)

20 sidecars, all `validate.py` exit 0 with void 0 (every act's quote
substring-verified against its own authority text at build time; the
validator re-confirms).

## Counts

- 209 acts labelled (110 operator fragments, 99 reply acts); 0 unlabelled.
- 124 answers: 47 closes / 77 keeps / 0 unclear.
- 198 acts carry scope (opens or within); no explicit `closes` ("back to...")
  appeared in this shard.

## Invented move labels (with counts; beyond the schema seeds)

report/observation 15, propose/design 13, clarify/context 12,
explain/context 11, report/evidence 8, clarify/assertion 5, qualify/scope 4,
explain/background 3, report/diagnosis 3, report/dispatched 3,
report/inventory 3, constrain/accept 2, report-problem/grievance 2,
ask-action/read-record 2, and 1 each: ask-action/counterfactual-question,
report/estimate, report/acknowledge, report/fact-check, clarify/ambiguity,
qualify/risk-assessment, challenge/decision-criterion, disagree/correction,
report/gap.  (The pilot's invented labels were reused where they fit.)

## Multi-opener incidences worth a second look

- One paragraph, several openers: turn-6paVBT r-2 answers f-s1-0 AND f-s3-0
  (diagnosis that also bears on the closer-loop proposal); r-3 likewise
  (closes f-s3-0, keeps f-s4-0); r-5 answers f-s3-0 + f-s4-0.  turn-zi0gEE
  r-1 answers f-s2-0 + f-s3-0.
- Several paragraphs, one opener: turn-0r52n8 f-s4-0 answered by r-0..r-5
  (closes at r-0, four keeps); turn-fIuIof f-s7-0 by r-0..r-4; turn-oxdell
  f-s1-1 by r-0..r-4; turn-4DmSW5 f-s1-0/f-s3-0 each answered twice.
- Cross-turn: turn-fIuIof r-5 answers previous turn-10-r-2 (re-offers the
  earlier promised triples pass).

## Input anomalies

- Where a record carries `acts`, those texts are upstream-truncated with a
  literal "…" and the validator prefers them over the fuller
  fragments/reply_acts texts (pilot ambiguity 4, unchanged).  Quotes for
  those ids were drawn from the visible prefix.  turn-Wdb4Sq r-3's `acts`
  text truncates mid-question.
- turn-EO0A4w f-s1-0/-1/-2 are the same sentence three times under three
  intents (kernel over-split); labelled per-intent with distinct moves.
- turn-6paVBT and turn-zi0gEE also carry `c-` commit acts with text null;
  these are not in fragments/reply_acts, so per the packet they are not
  labelled and not listed unlabelled (validator authority excludes them too).
- turn-5HxdtH f-s2-0 is a two-word locator ("on voxterm"); quote is the whole
  fragment (shortest legal quote in the shard).

## Time

About 3 hours of seat time: reading 20 records and labelling ~2.5 h
(build-side quote verification caught ~15 near-miss quotes, mostly from the
truncated-acts prefixes above), validation and note 0.5 h.
