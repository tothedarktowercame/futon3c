# SHARD-NOTE — shard-1, 52 records (zai-1, 2026-10-11)

## Counts

52 sidecars, 470 acts (280 operator-fragment, 190 reply-paragraph), 268
answers (133 closes / 135 keeps), 147 scoped acts (all `opens`/`within`
chains; no explicit `closes` case in this sample), 0 unlabelled.
`validate.py *.moves.json`: **exit 0, total valid=470, void=0** across all 52.

## Move vocabulary

149 distinct labels; 97 are one-offs.  Seeds dominate:
ask-action/one-decision x28, qualify/caveat x26, report/done x25,
approve/plain x21, report-problem/observed x21, verify/checked-live x11.
Pilot inventions reused heavily: report/design x25, report/diagnosis x13,
report/evidence x13, explain/mechanism x13, report/status x10,
ask-action/two-decisions x4, retract/correction x2.  New recurring families:
report/dispatched x6, report/in-flight x5, report/case-study x3,
propose/icon-choice x3, delegate/to-agent-with-tool, delegate/dispatch,
propose/acceptance-plan.  One-off inventions stay `<kind>/slug` shaped
(e.g. propose/zip-two-sides, explain/role-tension,
report-problem/grievance, clarify/named-gap).

## Multi-opener incidences worth a second look

Paragraphs answering 3+ openers of one turn:
turn-TAe3yN r-1 (three icon proposals verified at once); turn-g6q1nC
r-2 (work-order gap answers propose+report+report-problem); turn-j1Pdqp
r-0 (approve answers propose+redirect+propose); turn-tSMlKZ r-3 (approve
answers explain x3); turn-uy7gV9 r-0 (report answers propose+qualify+clarify);
turn-xm9Otx r-0 (approve answers approve+defer+propose).

## Input anomalies

- Every input carries BOTH `acts` (texts truncated to ~120 chars with a
  literal `…`) and `reply_acts` (full text).  `validate.py`'s `setdefault`
  gives `acts` precedence, so a quote is only safe inside the SHORTEST text
  per id; all my quotes were chosen to satisfy that.  The truncation looks
  like an upstream sampler bug worth fixing before scale-out (it silently
  caps quotable evidence).
- `sample.py` numbering vs packet numbering: inputs' `-r-<i>` are 0-based
  over marked paragraphs, matching the packet, not sample.py's 1-based
  over all paragraphs (already noted in the pilot; the new sampler
  commit 97162ddd says kernel-numbered — this resolves it for sharding).
- turn-mlUamp r-1 ("(what to type)") and turn-xm9Otx r-4 ("(draft paragraph
  for Brendan Fong)") are heading-only paragraphs whose content sits in
  unmarked continuation text; labelled with low confidence and a note.

## Time

~7.5 h of seat time: harness/quote-verification setup 1 h (the truncated-
text collision cost several refusal-fix rounds), labelling 52 records 6 h,
validation/note/commit 0.5 h.
