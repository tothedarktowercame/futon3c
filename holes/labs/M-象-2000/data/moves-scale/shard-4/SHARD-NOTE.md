# SHARD-NOTE — move-grammar shard 4 (zai-4, 2026-10-11)

21 sidecars, 277 labelled acts (172 operator fragments + 105 reply acts),
162 answers (85 closes / 77 keeps / 0 unclear), 208 scoped acts,
4 unlabelled. validate.py: exit 0, void 0, over all 21 files
(`python3 -I ../../../moves/validate.py *.moves.json`).

## Unlabelled (4, with reasons)
- claude-2-turn-29-f-s4-0 — text is "I.e." (4 chars), below the 8-char quote floor; continuative of s3-0.
- claude-2-turn-123-f-s1-0 — text is "Hm..." (5 chars), below the quote floor.
- claude-2-turn-108-f-s10-0 — text is "recipe." (7 chars), below the quote floor.

## Invented move labels (counts; seeds reused elsewhere)
Reused from pilot/seeds heavily: report/done x17, qualify/caveat x15,
approve/plain x12, delegate/to-agent x7, report-problem/observed x7,
verify/checked-live x9, ask-action/commission x6, defer/until x5,
ask-action/one-decision x4, clarify/distinction x4, constrain/for-now x4,
report/evidence x7, report/in-flight x5, report/status x5, report/correction x3,
retract/correction x2, approve/provisional, retract/idea.
Notable invented: report/* (analysis, answer, assessment, commits, dispatched,
history, inventory, mapping, tooling), qualify/* (defer-reason,
deferred-to-operator, theory-reference, agree-with-placement, reassessment,
scope-effort, gap, precedent, contextual, estimate, open-question, analogy),
ask-action/two-decisions (pilot), ask-action/operator-step, collect/ask-options,
collect/ingredients, propose/* (redraft, ordered-plan, wording, infer-from-
behaviour, first-cut, pipeline, acceptance-plan, design, discovery, driver,
row-loop, health-scan, cheat-sheet, process, project-idea, research-question,
redirect-scope, complexify, combine, one-cascade, shape-claim, living-object,
inner-structure, restatement, track-efficiency, cost-reductions,
speed-same-technology, elaborate-cascade, extend-analysis, extend-scope,
figure-idea, evidence, infrastructure-need, reorganization, resize, revise-page,
merge-staging, practical-improvements, priority-order, checklist, simplify as
propose/simplification), explain/* (parse-vs-formation, g-semantics,
conceptual-blend, pushout, emergent-structure, zeroth-engagement,
lean-grounding, lean-is-model, formation-first, subject-to-constraints,
improvable-loop, no-finish-needed, enabler-example, parallel-work-example,
edge-kinds, edge-semantics, structure-change, integration, recap, intent,
purpose, mechanism, metaphor, reference, reading, consequence, distinction,
difference, tokens, step, transition, note, observation, example, evidence,
hypothesis, iteration, position, deferred-payoff), retract/decision-requests.

## Multi-opener incidences worth a second look
- claude-2-turn-121-r-0 (0IgOYv) answers four openers at once (s1-0, s6-0, s5-0, s7-0) — a gist paragraph that both enacts and records.
- claude-2-turn-29-r-1/r-2 (0OmtsL) both bear on s5-0 with keeps; only r-0 closes it.
- claude-2-turn-12-r-4 (Dmp0fi) answers both the current delegate act and the previous turn's standing question (claude-2-turn-11-r-7) — cross-turn incidence.
- claude-2-turn-4-r-0 (5VfZ6V) answers four openers (s1-0, s3-0, s4-0, s10-0) all keeps — a progress paragraph touching everything, closing nothing.
- claude-2-turn-113-r-2 (X3R1HJ) answers s2-0 (closes) and s5-0 (keeps) in one paragraph.

## Things in the inputs that looked wrong
1. `acts` texts are truncated (~200 chars, "…"), while `fragments`/`reply_acts`
   texts are full; the validator takes the acts text as authority, so quotes for
   reply acts must come from the truncated span — several answers therefore cite
   a shorter span than the evidence the full paragraph carries (noted per-act).
2. Some reply_acts are header-only (5VfZ6V r-2/r-3, LZNJ49 r-1/r-5/r-7,
   Vk4oka r-4): the mark glued to a heading whose body paragraphs are unmarked.
   Labelled at low confidence from the header; bodies not labelled per packet.
3. Sub-8-char fragments exist and cannot carry an 8+ char quote (3 unlabelled
   above) — the floor silently drops these.
4. sMH1Sm f-s10-0 is a bare "#2" continuation marker, labelled low-confidence
   as a transition.

## Time
About 5.5 h of seat time: harness + truncation discovery 1 h, 21 records 4 h,
validator iterations and this note 0.5 h. Quote verification was build-time
(asserter), which caught ~15 truncation mismatches before they became voids.
