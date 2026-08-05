# STATEMENT-ONLY packet — formalize, do NOT prove

You are formalizing Lean 4 statements for the APM statement bank.
Repo: `/home/joe/code/apm-lean` (Mathlib via `.lake`; run
`export PATH="$HOME/.elan/bin:$PATH"` first).

Problems in this batch: {problem_list}

For EACH problem `<id>` above, one at a time:

1. Read `problems/<id>/problem.md` (the TeX source is the contract) and
   `problems/<id>/informal-solution.md` (for intent; the SOLUTION is not
   your job).
2. Write `problems/<id>/lean/Main.lean` containing exactly ONE theorem,
   named `apm_<id>` (lower-case id, e.g. `apm_a98a01`), stating the
   problem faithfully, with the proof body exactly `:= by\n  sorry`.
   Helper definitions (`def`, not `theorem`) are allowed and encouraged
   when the problem defines objects — DEFINE the problem's objects, do
   not inline surrogates for them.
3. THE HONEST-ENCODING RULE (from the a98A01 rejection): if an object
   is hard to encode (ℝⁿ, a.e. convergence, an exotic space), encode it
   anyway or leave a `sorry`-typed placeholder and say so — NEVER
   simplify the statement to scalar shadows of the real objects. A
   statement that drops the problem's central object will be rejected
   in review; an honestly-hard statement with a documented encoding
   question will not.
4. If the source statement needs REPAIR (typo, circular conclusion,
   missing hypothesis), repair it minimally and declare it in the file
   header under `## Statement repairs` with the reason. Undeclared
   repairs are rejected.
5. Verify: `lake env lean problems/<id>/lean/Main.lean` exits 0 (the
   statement elaborates; the sorry warning is expected). A statement
   that does not elaborate is not done.
6. Commit path-limited: `git add problems/<id>/lean/Main.lean &&
   git commit -m "<id>: formal statement (statement bank)"`.

Constraints:
- STATEMENTS ONLY. Do not prove anything. Do not add proof content
  beyond the single `sorry`. Budget your effort accordingly — this is
  a formalization task, ~5–15 minutes per problem.
- Consult `memory_search`/`psr_search` if available for encoding
  patterns previously used (e.g. how a92J06/a96J02 encoded measure
  bounds); cite consulted memories as `-- (Memory: <id>)` in the file
  header if their content shaped an encoding choice.

Report per problem: statement written (paste the theorem verbatim),
elaboration exit code, repairs declared (or "none"), encoding
questions flagged (or "none"), commit SHA.
