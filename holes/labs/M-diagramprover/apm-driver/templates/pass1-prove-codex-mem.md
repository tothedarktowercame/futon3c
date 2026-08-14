# PASS-1 PROVING packet (Codex lane) — frozen statements, honest outcomes

You are proving Lean 4 statements from the APM statement bank.
Repo: `/home/joe/code/apm-lean`. FIRST, in every shell you use:
`cd /home/joe/code/apm-lean && export PATH="$HOME/.elan/bin:$PATH"`.
The `cd` is REQUIRED, not cosmetic: elan resolves the toolchain from
this repo's `lean-toolchain`, and on hosts with no global default
`lake` fails with "no default toolchain configured" from anywhere
else. Verify with `lake env lean --version` BEFORE deciding anything;
if that command fails, STOP and report a toolchain blocker rather
than skipping the problems — a skip means "I tried and could not",
and an unusable toolchain is not that.

Your set: {problem_list}

THE STATEMENTS ARE FROZEN. Each `problems/<id>/lean/Main.lean` contains
an approved statement whose hash is recorded in the statement bank.
You may NOT change any statement, hypothesis, definition, or theorem
name — a changed statement voids the run mechanically at the gate. If
a statement seems wrong or unprovable as stated, SAY SO in your report
(that is valuable review signal) and move on; do not repair it yourself.

For EACH problem in your set, in order:

1. Read the statement and `problems/<id>/informal-solution.md`.
2. DESK RESEARCH IS PART OF THE JOB — not an optional preliminary.
   Consult, and say what each returned:
   - **`LEMMA-INDEX.md`** at the repo root — every helper lemma ALREADY
     PROVED here, with signatures. GREP IT FIRST; do not re-derive
     something the corpus has proved. `LIB:` rows import directly; the
     rest name the problem whose file holds the proof, so read it there
     and reuse the argument.
   - **Mathlib source** for the API you need. Prior searches by other
     agents were bounded, not exhaustive; the lemma may well be there.
   - **The ~144 fully-closed artifacts** in `problems/*/lean/Main.lean`
     (find them with: `grep -L sorry problems/*/lean/Main.lean`). Several
     are likely in this problem's mathematical neighbourhood.
   - **`ConstructionTargets/`** and **`YoungL2.lean`** — libraries of
     REUSABLE PROVED LEMMAS built to unblock exactly these problems
     (BanachZarecki, CircleParam, HerglotzRigidity, KernelAverage,
     L2Translation, …). `import ConstructionTargets.X` works; they are on
     the module path. Check them before concluding a lemma is missing.
   - **Boundary comments** in partial artifacts, and `git log`.
   (You HAVE `memory_search` and `memory_record` over MCP. SEARCH FIRST —
   before concluding an API is missing, query the store by tags or subject;
   the corpus is young, so an empty result is normal and is itself worth
   recording. Cite any memory whose content you actually used as
   `-- (Memory: e-<id>)` adjacent to the informed code — and ONLY when you
   used it; an unearned citation corrupts grading data. RECORD what a
   future prover would want: a Mathlib API that did or did not exist, a
   route that worked, a dead end worth not repeating — self-contained body,
   at least one typed subject. Identity and domain are server-stamped; do
   not pass them.)
3. Prove it. THREE ROUTES ARE ALL LEGITIMATE: find the missing
   dependency; **prove it locally as helper lemmas** (you MAY add
   auxiliary lemmas to the file — only the STATEMENT is frozen, not the
   file); or route around the obstacle via machinery you have not yet
   tried. Validate continuously with
   `lake env lean problems/<id>/lean/Main.lean`. Cite reuse as
   `-- (Sibling: <problem-id>)` or `-- (Lib: ConstructionTargets.X)`.
4. HONEST OUTCOMES, pick one per problem:
   - PROVED: zero sorries, `lake env lean` exit 0. Run
     `#print axioms` for the theorem and include the output.
   - PARTIAL: some steps proved, remaining `sorry`s each documented
     with a boundary comment (APIs searched with backticked names,
     routes tried, the specific blocker).
   - SKIPPED: only after GENUINE SUSTAINED ATTEMPT including the
     research in step 2 — not a first-look bail. State the blocker and
     what you consulted. Skipping honestly is better than a forced fake,
     but skipping early is just a lost problem.
5. Commit per problem, path-limited:
   `git add problems/<id>/lean/Main.lean && git commit -m "<id>: pass-1 <proved|partial|skipped> (codex)"`.
   The `(codex)` suffix is load-bearing — it separates this lane from
   the zai lane in the outcome tally. Do NOT `git add -u`.
6. Budget ~15–25 minutes per problem; move on when the budget is
   spent. Total set budget: do not exceed the job window.

Report: one line per problem — outcome, sorry count, axioms (if
proved), siblings/libs cited, blocker (if not proved), plus a NUMBERED
LIST OF EVERY RESOURCE CONSULTATION (what, what it returned, used or
discarded and why) — then a closing note
listing any statement you believe is WRONG as stated, with your reason
(this feeds statement review; it does not authorize you to change it).

NEVER INTRODUCE `opaque` OR `axiom`. Not for the statement, not for a
helper, not to make something elaborate. An `opaque` constant has no
defining equations, so nothing about it can ever be proved, and an
`axiom` pollutes `#print axioms` at the gate — both are now detected
and downgrade the outcome to `defective`. Eight artifacts were lost
this way (one fabricated wedge, oriented integration AND the exterior
derivative, leaving a statement that referred to no actual forms). If
a bridge cannot be built honestly, leave the `sorry` with a boundary
comment and say so — an honest partial is a valid outcome; a
placeholder that elaborates is not.
