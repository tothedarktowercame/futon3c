# Frame: t00A05 (frame-5 — PLAN-FIRST, full budget)

## Your first deliverable is a PLAN, not code
Before proving anything: produce your own complete proof plan for the one
remaining goal —
  `∫ t in Icc 0 (2π), apm_t00a05_angularDensity γ t = 2π`
— as a numbered list of lemma statements (Lean signatures where you can),
each with its intended proof method and what existing branch machinery it
uses. Your branch carries 36 verified commits including your own formal
refutation (`ce6f1ac`) of the naive angle composition; the plan must route
around it (a path-lifted angle accumulating seam corrections, sign from the
positive signed-area hypothesis — or any better route YOU see). Commit the
plan as a comment block or .md in the problem directory FIRST. Then execute
it, lemma by lemma, committing each landing.

## Budget
You have the full attempt budget and a fresh window. Siege discipline per
your card; the conductor will send only process continuations. If the plan
survives contact, say so; if it needs revision mid-execution, revise it in
the file and continue — the plan's evolution is data.

## Acceptance
1. `lake env lean problems/t00A05/lean/Main.lean` → exit 0, ZERO sorry warnings.
2. `#print axioms apm_t00a05` → at most [propext, Classical.choice, Quot.sound].
3. Commits; report with shas, verbatim axiom output, memory verdicts.
