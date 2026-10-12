# SHARD-NOTE — move-grammar batch 2, shard 4 (zai-4, 2026-10-11)

24 sidecars, 149 labelled acts (67 operator fragments + 82 reply acts),
92 answers (56 closes / 36 keeps / 0 unclear), 117 scoped acts, 1 unlabelled.
validate.py: exit 0, void 0 over all 24 files
(`python3 -I ../../../moves/validate.py *.moves.json`).

## Unlabelled (1)
- codex-18-turn-38-f-s3-0 — fragment is the bare marker "QUOTE" (5 chars), below
  the 8-char quote floor; the quoted claude-4 text is not itself in the fragment.

## Moves (dialogue-act vocabulary, per the batch-2 rule; no topic labels)
approve/plain x5, approve/provisional, ask-action/commission x8,
ask-action/one-decision x11, ask-action/operator-step, clarify/ask-decision x4,
clarify/ask-recap, clarify/ask-role, clarify/check-understanding x3,
clarify/answer, clarify/context, collect/ask-options x7, collect/offer-example,
constrain/for-now x5, constrain/standing-rule x2, defer/until x6,
delegate/to-agent x2, disagree/assessment, disagree/correction x2,
challenge/claim, propose/directive x6, propose/tentative x2, propose/alternative x2,
propose/next-step x3, propose/ordered-plan, propose/design x2, propose/analogy,
propose/redirect-scope x2, propose/positioning, retract/correction x3,
report/done x9, report/answer x3, report/evidence x4, report/inventory x4,
report/status x2, report/in-flight, report/dispatched x2, report/correction x6,
report/observation x5, report/assessment, report/gap x2, report/negative,
report/quotation (dropped — the QUOTE fragment is unlabelled instead),
report/evidence-chain, report/answer; verify/checked-live x5;
qualify/caveat x6, qualify/uncertainty, qualify/gap x2, qualify/implication x2,
qualify/expectation, qualify/agree-with-caveat, qualify/impact, qualify/scope x3,
explain/*: mechanism x3, distinction x3, comparison, alternative, analogy,
reading x2, positioning, design, diagnosis, difference, evidence (reused as
report), reference, example (used as report/observation instead where possible).

## Sole-port test observations (single-opener incidences)
- Openers answered only partially are common: ND51hI f-s1-0 (README-hardening)
  is answered by a health-status report (keeps) — the sole-port rule would
  leave it open, correctly.
- wNt6UQ: opener "build it directly" closes at r-0; the deferred routing
  fragment f-s1-1 is answered by a correction (r-1, keeps).
- q0fjvy / a3V7wF: the single opener is answered across multiple paragraphs,
  only the correction paragraph closes it (closes vs keeps separation).
- E5KqDv f-s2-0/s2-1/s2-2 are three intent tags over one identical sentence —
  see wrongness below.

## Things in the inputs that looked wrong
1. Same as batch 1: `acts` texts truncate (~200 chars) while fragments and
   reply_acts are full; quotes must come from the truncated span. Noted per-act.
2. Header-only reply_acts again (Cu0AN8 r-2, kNLxcE r-1/r-2, Prtgyo r-1/r-2):
   labelled low-confidence from the header.
3. E5KqDv has three fragments (f-s2-0/1/2) with identical text but different
   intents (extend/propose/defer) — the fragmenter split one sentence into
   three overlapping acts; each labelled against the same span.
4. IkroS8 has no `acts` at all (authority = fragments + reply_acts only); fine.
5. ngC1Zc f-s2-0/f-s2-1 appear in fragments but not in the printed acts set;
   they validated, so they are present in authority somewhere.

## Time
About 4 h of seat time (reuse of the batch-1 harness; ~10 min per record
including truncation-probe retries; note and commit 0.5 h).
