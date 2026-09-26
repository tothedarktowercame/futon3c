# Frame: t00A05 (frame-3, round 1 measured unit — MEMORY-ONLY CHALLENGE)

## Target
`problems/t00A05/lean/Main.lean` carries exactly one executable `sorry`: the
final conjunct of `apm_t00a05` — the line integral of the winding form over
any C2 closed curve equals -2π. Two supporting lemmas are already proved in
the file. The bundle's `proof-outline.md` and `informal-solution.md` are
available.

## The store is your map
The memory store contains a COMPLETE route for this exact problem — search
subject t00A05 (memory_search), open bodies by id (memory_read). Twelve
memories including a 10-step route map with declaration shapes, formulas,
and Mathlib API names. Follow it, or find better. Per-memory USED/IGNORED
verdicts with reasons in your report.

## The one rule that matters
GO AS FAR AS POSSIBLE, END-TO-END. Never stop to ask a question; never wait
for help — a stopped run is a failed run. If a sub-step resists: try another
route, commit what compiles, and keep advancing toward the closed theorem.
Commit partial progress continuously.

## Contract
Work ONLY in your checkout (ENVIRONMENT block below); commit ONLY to the
frame branch. Verify the hole state against YOUR BRANCH first.

## Acceptance
1. `lake env lean problems/t00A05/lean/Main.lean` → exit 0, ZERO
   "declaration uses `sorry`" warnings.
2. `#print axioms apm_t00a05` → at most [propext, Classical.choice, Quot.sound].
3. Commit; report summary, shas, verbatim axiom output, memory verdicts.
