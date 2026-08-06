# PASS-1 PROVING packet — frozen statements, honest outcomes

You are proving Lean 4 statements from the APM statement bank.
Repo: `/home/joe/code/apm-lean` (run `export PATH="$HOME/.elan/bin:$PATH"`).

Your set: {problem_list}

THE STATEMENTS ARE FROZEN. Each `problems/<id>/lean/Main.lean` contains
an approved statement whose hash is recorded in the statement bank.
You may NOT change any statement, hypothesis, definition, or theorem
name — a changed statement voids the run mechanically at the gate. If
a statement seems wrong or unprovable as stated, SAY SO in your report
(that is valuable review signal) and move on; do not repair it yourself.

For EACH problem in your set, in order:

1. Read the statement and `problems/<id>/informal-solution.md`.
2. Consult `memory_search` and `psr_search` for routes (tags in your
   own vocabulary; the store holds routes from prior closed problems).
3. Attempt the proof, replacing ONLY the `sorry`. Cite every memory
   whose content you actually use with `-- (Memory: e-<id>)` adjacent
   to the informed code, and every PATTERN whose content you use with
   `-- (Pattern: <ns>/<name>)`. The forms are machine-checked and
   distinct: `Memory:` is ONLY for store memory ids (they begin
   `e-`); `Pattern:` is ONLY for pattern-library ids. Single `--`,
   exact spacing.
4. HONEST OUTCOMES, pick one per problem:
   - PROVED: zero sorries, `lake env lean` exit 0. Run
     `#print axioms` for the theorem and include the output.
   - PARTIAL: some steps proved, remaining `sorry`s each documented
     with a boundary comment (APIs searched with backticked names,
     routes tried, the specific blocker).
   - SKIPPED: statement attempted < 10 minutes, blocker identified
     and stated (e.g. "needs Mathlib API for X which I could not
     find"). Skipping honestly is BETTER than a forced fake.
5. Commit per problem, path-limited:
   `git add problems/<id>/lean/Main.lean && git commit -m "<id>: pass-1 <proved|partial|skipped>"`.
6. Budget ~15–25 minutes per problem; move on when the budget is
   spent. Total set budget: do not exceed the job window.

Report: one line per problem — outcome, sorry count, axioms (if
proved), memories cited (ids), blocker (if not proved) — then a
closing note listing any statement you believe is WRONG as stated,
with your reason (this feeds statement review; it does not authorize
you to change it).
