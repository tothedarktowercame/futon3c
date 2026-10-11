# PILOT-NOTE — move-grammar pilot, 20 records (zai-1, 2026-10-11)

Sidecars: 20 `*.moves.json`, 145 acts (87 operator-fragment, 58 marked reply
paragraphs, plus 1 turn whose reply acts came from the record's own `:acts`),
102 answers (50 closes / 52 keeps), 43 scoped acts, 2 unlabelled acts.
Every quote was substring-verified at build time; the validator reports
**valid=67 void=0** across all 20 files (67 = acts it can resolve; see the
validator gap below).

## Move vocabulary (with counts)

Seeds used: `ask-action/one-decision` ×7, `report/done` ×7,
`report-problem/observed` ×4, `explain/mechanism` ×3, `propose/alternative` ×3,
`propose/next-step` ×3, `approve/plain` ×3, `delegate/to-agent` ×3,
`defer/until` ×2, `verify/checked-live` ×2, `report/partial`,
`clarify/check-understanding`, `report/with-options` ×1 each.
Invented (each fits the kind; the frequent families): `report/*` diagnostics
(diagnosis, evidence, evidence-chain, fact-check, correction, gap, mapping,
inventory, dispatched, in-flight, status — replies are mostly reports whose
distinction is *what is reported*, not whether); `qualify/*` (caveat, scope,
impact, risk-assessment, unvalidated, out-of-scope); `propose/*` (design,
pipeline, acceptance-plan, defaults, re-baseline, case-study, ordered-plan);
`ask-action/*` (two-decisions, explain-failure, read-record, status-check,
inspect-live-case); operator-side `challenge/*`, `disagree/correction`,
`retract/correction`, `approve/provisional`, `constrain/accept`,
`promise/explanation-outline`, `report-problem/grievance`.

## Ambiguities and resolutions

1. **Reply-paragraph authority vs the validator.** `validate.py` resolves an
   act's quote only against `:acts`/`:fragments`; derived `-r-<i>` acts are
   "unknown act" errors and their quotes count void. Confirmed on a trial
   file. I followed the packet (one act per marked paragraph, 0-based among
   marked) and verified every `-r-` quote myself; the 58 `-r-` acts are the
   void/error population. Fix (owner's): let validate.py build authority from
   `reply_text` as `sample.py:reply_marks` does, adopting the packet's
   marked-only 0-based numbering (sample.py is 1-based over all paragraphs —
   a second, separate mismatch to settle).
2. **Marked paragraphs joined mid-sentence.** Recorders sometimes join
   "…text.㊥ (label)" with no newline; blank-line splitting merges them. I
   split additionally at a mark following sentence punctuation, else the ㊥
   paragraph of turn-34pFgV would have been lost.
3. **Answers target fragments or previous acts**; continuation paragraphs
   that elaborate a marked one are not separate acts, so some evidence
   (e.g. turn-34pFgV's cost tables) sits in unmarked text and answers cite
   only what the marked paragraph itself contains.
4. **turn-rcPsFk** carries `:acts` with upstream-truncated texts (literal …);
   quotes had to come from those truncated spans. turn-RQwzVb has a mark-like
   char (㭭) outside the packet's mark set — treated as unmarked.

## Multi-opener / multi-paragraph incidences

One paragraph answering several openers: 34pFgV-r-0 (delegate + clarify),
7QZ8sL-r-0 (propose + previous 🈯), PhLBYe-r-0 (report-problem + inspect ask),
a3V7wF-r-3 (grievance + previous test offer), oQdwzD-r-2 (three openers at
once). Several paragraphs on one opener: 34pFgV f-s5-0 answered by r-0, r-2,
r-3; Cu0AN8 f-s1-0 answered by r-0–r-6 (seven paragraphs, closes only at the
ordering step); q0fjvy f-s1-0 answered by all five reply paragraphs.

## Scopes found

`opens` with governing `within` chains: element-fast-mode plan, preamble
best-of draft, card preregistration design, lean-elaborator route question,
B1d chord sheet, interactive infographic page, minus40 hook-up plan, SVO
triples §3.1, method post-mortem, inbox-zero path order. One explicit
`closes`-shaped return ("back to…") did not occur in this sample.

## Validator and scorer output

`validate.py *.moves.json`: valid=67 void=0, plus "unknown act" errors for
every derived `-r-` act (the gap above); the four records with no `-r-` acts
(or with `:acts`) validate clean. `score.py --moves . --labels
../../moves/checkpoint-labels.json`: `{"labelled": 24, "matchable": 22,
"incidence": 4, "closed": 1, "predicted_open": 2,
"precision_answered_vs_open": 0.333, "unmatchable": [14, 23]}`.

## Time

About 4.5 h of seat time: harness reading 0.5 h, labelling 20 records 3.5 h
(build-side quote verification caught 14 near-miss quotes), note and commit
0.5 h.
