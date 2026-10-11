# PILOT-NOTE — move-grammar pilot, 20 records (zai-1, 2026-10-11)

Sidecars: 20 *.moves.json, 145 acts (69 the validator can resolve: operator
fragments plus turn-rcPsFk's own :acts; 76 derived reply-paragraph acts),
102 answers (50 closes / 52 keeps), 43 scoped, 2 unlabelled.  Every quote,
including all 76 -r- quotes, was substring-verified against its own text at
build time; validate.py reports valid=69 void=76 -- the void 76 are exactly
the -r- acts, void only because the validator resolves no authority for
them (gap 1 below), not because any quote failed.

## Move vocabulary (with counts)

Seeds used: ask-action/one-decision x7, report/done x7, report-problem/observed x4,
explain/mechanism, propose/alternative, propose/next-step, approve/plain, delegate/to-agent
x3 each, defer/until and verify/checked-live x2, report/partial,
clarify/check-understanding, report/with-options x1.  Invented: report/* diagnostics
(diagnosis, evidence, evidence-chain, fact-check, correction, gap, mapping, inventory,
dispatched, in-flight, status), qualify/* (caveat, scope, impact, risk-assessment,
out-of-scope), propose/* (design, pipeline, acceptance-plan, defaults, re-baseline,
case-study, ordered-plan), ask-action/* (two-decisions, explain-failure, read-record),
and operator-side challenge/*, disagree/correction, retract/correction,
approve/provisional, constrain/accept, promise/explanation-outline,
report-problem/grievance.

## Ambiguities and resolutions
1. validate.py resolves an act's quote only against :acts/:fragments, so
   derived -r-<i> acts are "unknown act" errors (confirmed on a trial file).
   I followed the packet (marked paragraphs only, 0-based) and verified every
   -r- quote myself; those acts are the whole void/error population.  Owner
   fix: build authority from reply_text as sample.py:reply_marks does, with
   the packet's numbering (sample.py is 1-based over all paragraphs).
2. Recorders join "...text.㊥ (label)" with no newline; I split additionally
   at a mark after sentence punctuation, else 34pFgV's ㊥ paragraph is lost.
3. Continuation paragraphs elaborating a marked one are not acts, so answers
   cite only what the marked paragraph itself contains.
4. rcPsFk's :acts texts are upstream-truncated (literal ...); quotes come
   from those spans.  RQwzVb's 㭭 is outside the packet's mark set: unmarked.

## Multi-opener / multi-paragraph incidences

One paragraph, several openers: 34pFgV-r-0 (delegate+clarify), 7QZ8sL-r-0
(propose + previous ask), PhLBYe-r-0, a3V7wF-r-3, oQdwzD-r-2 (three at once).
Several paragraphs, one opener: Cu0AN8 f-s1-0 answered by r-0..r-6 (closes
only at the ordering step); q0fjvy f-s1-0 by all five reply paragraphs.

## Scopes found

opens/within chains: element-fast-mode plan, preamble best-of draft, card
preregistration design, lean-elaborator route question, B1d chord sheet,
interactive infographic page, minus40 hook-up plan, SVO triples 3.1, method
post-mortem, inbox-zero path order.  No explicit closes ("back to...").

## Validator and scorer output

validate.py: valid=69 void=76, "unknown act" errors for every derived -r-
act (the gap above); records with no -r- acts validate clean (exit 0).
score.py
--moves . --labels ../../moves/checkpoint-labels.json: labelled 24, matchable
22, incidence 4, closed 1, predicted_open 2, precision_answered_vs_open
0.333, unmatchable [14, 23].

## Time

About 4.5 h of seat time: harness reading 0.5 h, labelling 20 records 3.5 h
(build-side quote verification caught 14 near-miss quotes), note and commit
0.5 h.
