# SHARD-NOTE — move-grammar shard 2 (zai-2, 2026-10-11)

`python3 -I ../../../moves/validate.py *.moves.json`: exit 0, **35 files
valid, void=0** (every quote substring-verified at build time; the builder
replicated the validator's authority order).

## Counts
- 282 acts labelled (222 operator fragments, 60 reply acts), 1 unlabelled
  (turn-h7zTox f-s1-0 "Hm...", under the 8-char quote minimum).
- 145 answers: **104 closes / 41 keeps / 0 unclear**; 84 scoped acts
  (opens/within chains; no explicit `closes`, as in the pilot).
- 58 distinct move labels; seeds cover the bulk, pilot inventions reused
  (report/diagnosis, report/evidence, report/gap, report/in-flight,
  report/with-options, report/fact-check, qualify/risk-assessment,
  propose/ordered-plan, retract/correction and four others).

## Invented this shard
report/buffer-excerpt x14 (operator-pasted progress lines, turn-MwN5mA),
report/job-spec x3, delegate/handoff-spec x3, and x2-or-once: constrain/
retry-better, report/ack, report/resource-state, challenge/necessity,
propose/speedup-item, explain/decision-reversal, report/planned,
prioritize/urgent-fix, propose/speedup-list, redirect/topic, report/
post-mortem, propose/idea, report/redirect-issued, constrain/hold-dispatch,
constrain/teach-helpers, report/partial, constrain/measurement-scope,
constrain/method-first.

## Multi-opener incidences worth a second look
- turn-DYfs4d-r-0 answers f-s1-0 and previous 237-r-3 (while-True).
- turn-ELTBL4-r-0 answers two fragments AND previous 85-r-4: three ports
  closed by one delivery (the new rowmap view).
- turn-PGeqxq-r-0 answers the redirect-scope opener and the accelerate-Lisp
  ask (both keeps).
- turn-FDuXUh-r-0 answers three openers (census closes; speed worry and
  Rob's rule keep); r-4 answers three speedup openers.
- turn-W34IgZ-r-0 answers the "only test that matters" proposal and the
  comparator reminder together — the rule change closes both.

## Oddities in the inputs
- `:acts` entries are 120-char truncated and win the validator's authority
  whenever an id appears there, so many quotes come from the prefix; noted
  per act where the key sentence fell past the cut (tjxDid f-s1-0,
  mtauh1 f-s1-1).
- Empty reply bodies (header only): ELTBL4-r-1, PGeqxq-r-1, llSmbF-r-1;
  turn-3RuGAU r-0 only in reply_acts. OxiFT2 f-s2-0/f-s2-1: same text,
  two kinds; both labelled.
- turn-7dRWAW/ELTBL4/L3v2a8 `:acts` ids carry `@turn-…` suffixes (fragment
  ids labelled per the packet); W34IgZ `:acts` kinds disagree with
  `fragments` on two acts (fragment kinds used).

Time: about 4 h seat time (reading 0.5 h, labelling ~3 h, note 0.5 h).
