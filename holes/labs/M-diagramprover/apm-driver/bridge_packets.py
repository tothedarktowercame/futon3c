"""Hop-4 pilot: 5 Tier A + 5 Tier B, dispatched once, job ids recorded.

The shift (Joe, 2026-08-08): a closer that has already modelled the proof and
then names a missing Mathlib item has done its job. Re-running the same closer
loop re-derives what it already knows. So hop 4 is not "close the problem" --
it is "close the hole the closer identified".

Two packet shapes, because the two tiers have made different amounts of
progress:

  Tier A -- named a specific gap. Ask for the lemma, but make FINDING existing
    Mathlib material an equally valid success: an earlier problem was blocked on
    a hole that adjacent material already filled, and the only waste was that
    nobody was looking. There is no cheap pre-filter for this -- you cannot grep
    for a lemma that does not exist, and the identifiers these comments name are
    the subject matter (`RatFunc`, `Finset`) rather than the missing theorem.

  Tier B -- reported open work without naming a gap. Ask ONLY for a precise Lean
    STATEMENT of what is missing. Turning "the classification bridge remains
    open" into a statable lemma is itself the modelling work, and it converts
    the problem into Tier A (or reveals it was never reduced).
"""
import json
import pathlib
import re
import sys

sys.path.insert(0, "/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
import agency
import statement_campaign as sc
import gates

DRIVER = pathlib.Path("/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
REPO = pathlib.Path("/home/joe/code/apm-lean")
ROWS = DRIVER / "mathlib-holes.jsonl"
OUT = DRIVER / "bridge-pilot-jobs.jsonl"
SEATS = ["ams-codex-1", "ams-codex-2"]

STRICT = re.compile(r"(no packaged|does not exist|Mathlib (?:has no|lacks|installs no)|"
                    r"no general|no lemma|no such|not found)", re.I)

COMMON = """
HARD RULES (the campaign's, unchanged):
- The problem statement is FROZEN. Do not weaken, restate or restructure
  `{main}`. Adding helper lemmas is legal and expected; changing the
  statement voids the work.
- NEVER introduce `opaque` or `axiom`, and never `native_decide`. All three
  are detected and downgrade the outcome to defective. An `opaque` constant
  has no defining equations, so nothing about it can be proved.
- Validate with: cd /home/joe/code/apm-lean && lake env lean {path}
  Final state must be exit 0.
- Consult LEMMA-INDEX.md at the repo root (every already-proved helper in this
  repo, with signatures) and ConstructionTargets/ before deriving anything.

NO-DUPLICATION POLICY (standing, from 2026-08-09 - this is a CHANGE):
  Re-deriving a lemma this repo has already proved is now reportable waste.
  We measured it: 125 helper lemmas sit in duplicate groups, and 94% of those
  duplicates are between problems in the SAME prelim class (a / m / t / b) --
  i.e. your nearest neighbours are the likeliest source of a lemma you are
  about to reprove.
  Before you prove ANY helper:
    - grep LEMMA-INDEX.md for the STATEMENT SHAPE, not just a name you guessed.
      Search the operative terms (`IsCompactOperator`, `heatKernel`,
      `gramSchmidt`, whatever your goal mentions);
    - look hardest at solved problems in YOUR OWN prelim class;
    - if it already exists, REUSE IT: `import` it when it is importable
      (`LIB:` rows are), otherwise cite it by (problem-id, lemma-name) and
      reuse the argument WITH ATTRIBUTION rather than reinventing it.
  Two honest caveats, so you calibrate rather than over-trust:
    - THE INDEX CAN BE STALE. It is regenerated only occasionally, so work
      finished in the last hours may be absent. Not finding something is weak
      evidence of absence; say which index build you searched.
    - Finding a duplicate is NOT an error and NOT a reason to stop. Report it
      as a fact and carry on.
  And if you prove something you believe is GENERAL (its statement mentions no
  `apm_` definition), say so explicitly in your report so it can be promoted
  into ConstructionTargets. That is how the next agent gets to import it
  instead of reproving it.
- Commit path-limited to problems/{pid}/ with a specific message.
"""

TIER_A = """HOP 4 - apm-{pid}: CLOSE THE IDENTIFIED MATHLIB HOLE.

Three closer hops have already modelled this proof. Their conclusion, verbatim:

    {hole}

We are TAKING THAT AT ITS WORD. Your job is that gap, not a fresh attempt at
the problem.

TWO OUTCOMES, BOTH SUCCESS - say plainly which one you reached:
  (A) PROVE the missing lemma as a helper in the problem file, then use it.
  (B) FIND existing Mathlib material that does the same job, and use it. This
      is NOT a failure and NOT a wasted dispatch: it closes the problem and
      tells us the gap was apparent rather than real. Prior searches were
      bounded, not exhaustive, so look properly before concluding the gap is
      real.

If after genuine effort BOTH fail, that is a legitimate third outcome: leave
the sorry, and report the lemma you would need AS A PRECISE LEAN STATEMENT.

That statement is REVIEWED, and half of the ones we receive are sent back.
Two failure modes account for all of them. Self-check against BOTH before you
report:

  1. IT IS THE GOAL WEARING A HAT. If your "missing lemma" is a conjunct of
     {main}, or {main} with a definition unfolded, or {main} restated in this
     problem's own `apm_{pid}_*` vocabulary, then it names no gap - it hands
     the problem back. A LIBRARY lemma is stated in LIBRARY vocabulary:
     Mathlib names and standard classes, with NO `apm_` definitions occurring
     in it. If you cannot phrase it without them, then the missing content is
     LOCAL ASSEMBLY, not a Mathlib absence. Say that instead, and name the
     Mathlib declarations the assembly would use - that is a more useful
     answer, not a lesser one.
  2. IT ALREADY EXISTS. Do not report an absence you have not searched for.
     Search BOTH places and say so:
       - Mathlib, at .lake/packages/mathlib/Mathlib;
       - THIS REPO - LEMMA-INDEX.md (2098 proved lemmas, 182 of them importable
         today across 17 ConstructionTargets modules) and ConstructionTargets/.
     A result already proved here is the commonest false alarm, and it is
     cheaper to find than to rebuild. Report the SEARCH, not just its
     conclusion: the identifiers you grepped, WHERE you grepped them (both
     sources), and the NEAREST declaration you did find together with the
     specific reason it does not fit. "Prior searches were bounded, not
     exhaustive" applies to yours too.

Then give the ROUTE, not only the statement. If the remaining work is several
steps, list them as several steps, in order. One lemma the size of the whole
goal gives the next prover nothing to start from - whereas "prove S invertible,
bundle S-inverse-T as an equivalence, show it unipotent, transport uniqueness"
is immediately actionable. If your prose already describes such a route, the
route is the deliverable: make the Lean statements match it.

FOURTH OUTCOME - THE FROZEN STATEMENT IS DEFECTIVE. This is a legitimate,
wanted result, not an excuse: three problems today were genuinely wrong as
frozen (a sup-norm where the Euclidean norm was meant, a missing separation
axiom, an unrestricted uniqueness claim that is false). Report it rather
than working around it, and NEVER repair it yourself.
BRIEF EVIDENCE IS REQUIRED - one or two sentences plus ONE of:
  - a concrete counterexample (the object that satisfies the hypotheses and
    violates the conclusion);
  - the specific type/universe/instance obstruction, shown concretely (e.g.
    "the natural index inhabits Type (max u v), so it cannot instantiate an
    existential over Type");
  - the line of the SOURCE the formalisation diverges from.
A bare assertion that the statement looks wrong is not evidence and will be
sent back.

TARGET: {path} (currently {sorries} sorry(ies))
MAIN THEOREM: {main}
{common}
REPORT: which outcome (A / B / failed-with-statement / statement-defective); the lemma proved or the Mathlib
declaration reused, by full name; final sorry count; verbatim `#print axioms
{main}`; confirmation the statement is untouched; commit sha.
For failed-with-statement, ALSO report, as separate labelled items: the ROUTE
in prose as ordered steps; the SEARCH RECEIPT (identifiers grepped, where, and
the nearest declaration found with why it does not fit); and one line per named
lemma confirming it contains no `apm_` definition, or, if it does, saying
plainly that the work is local assembly rather than a Mathlib gap.
"""

TIER_B = """HOP 4 - apm-{pid}: STATE THE MISSING LEMMA. DO NOT PROVE IT.

Three closer hops have modelled this proof and reported what remains:

    {hole}

That says what is LEFT, but not what is MISSING as a statable lemma - and until
it is statable nobody can prove it. Producing that statement IS this job.

DELIVERABLE: add to the problem file, above the main theorem, one or a few
declarations of the form

    /-- Why this unblocks apm_{pid}: <one or two sentences>. -/
    theorem apm_{pid}_bridge_1 : <the precise missing statement> := by
      sorry

Each must be the mathematical content that is actually missing, stated so that
it could be handed to someone with no knowledge of this problem. Do NOT attempt
the proofs, and do not touch the main theorem or its existing helpers.

A bridge that is FALSE is worse than none, so sanity-check each statement: it
must be true, and it must genuinely unblock the remaining sorry. If you cannot
reduce the remaining work to statable lemmas, say so and explain what is
blocking the reduction - that is a real answer.

TRUTH IS NOT ENOUGH, AND THIS IS WHERE MOST BRIDGES FAIL. Every bridge we have
rejected so far was TRUE. They were rejected for saying nothing. Check each of
yours against all seven shapes below and do not submit one that matches:

  - verbatim      a conjunct of {main} handed back unchanged
  - unfolded      a definition expanded in place
  - split         {main}'s own conjuncts, or the two halves of its iff
  - reformulated  the same content in different clothes
  - reordered     assumes conjunct 1, concludes conjunct 2
  - restricted    {main} with a strengthened hypothesis
  - trivial       genuinely different, but supplying no mathematics

Note that "strictly weaker" is NOT the test - a bridge may legitimately be
STRONGER than what is needed, such as a closed form where only a bound was
required. The test a bridge must PASS is that it supplies mathematics not
already present in this file and not already contained in {main}.

So before writing a bridge, GREP THIS FILE. The single most common rejection
is a bridge that the file ITSELF already proves, forty lines above, under
another name - at which point the bridge is just that lemma's conclusion
restated. For each bridge, state what NEW content it adds, and name the closest
existing declaration in this file together with why it is not already
equivalent.

FOURTH OUTCOME - THE FROZEN STATEMENT IS DEFECTIVE. This is a legitimate,
wanted result, not an excuse: three problems today were genuinely wrong as
frozen (a sup-norm where the Euclidean norm was meant, a missing separation
axiom, an unrestricted uniqueness claim that is false). Report it rather
than working around it, and NEVER repair it yourself.
BRIEF EVIDENCE IS REQUIRED - one or two sentences plus ONE of:
  - a concrete counterexample (the object that satisfies the hypotheses and
    violates the conclusion);
  - the specific type/universe/instance obstruction, shown concretely (e.g.
    "the natural index inhabits Type (max u v), so it cannot instantiate an
    existential over Type");
  - the line of the SOURCE the formalisation diverges from.
A bare assertion that the statement looks wrong is not evidence and will be
sent back.

TARGET: {path} (currently {sorries} sorry(ies))
MAIN THEOREM: {main}
{common}
REPORT: each bridge statement, verbatim; one line each on why it unblocks the
proof; for each, the NEW mathematics it supplies and the nearest existing
declaration in this file with why it is not equivalent; which of the seven
shapes you checked it against; confirmation the main statement is untouched;
commit sha.
"""


def main() -> int:
    rows = [json.loads(l) for l in ROWS.read_text().splitlines() if l.strip()]
    tier_a = [r for r in rows if STRICT.search(r["hole"])]
    tier_b = [r for r in rows if not STRICT.search(r["hole"])]
    # Prefer the most specific reports: a longer hole sentence that cites
    # identifiers has actually said something.
    key = lambda r: (-len(r["identifiers"]), -len(r["hole"]))
    picks = ([("A", r) for r in sorted(tier_a, key=key)[:5]] +
             [("B", r) for r in sorted(tier_b, key=key)[:5]])

    dispatched = []
    for i, (tier, row) in enumerate(picks):
        pid = row["problem-id"]
        lean_rel = f"problems/{pid}/lean/Main.lean"
        source = (REPO / lean_rel).read_text(encoding="utf-8")
        try:
            main_name = gates.statement_hash(source, pid)[0]
        except Exception:
            main_name = f"apm_{pid.lower()}"
        fields = {"pid": pid, "hole": row["hole"], "path": lean_rel,
                  "sorries": gates.count_sorries(source), "main": main_name}
        fields["common"] = COMMON.format(**fields)
        packet = (TIER_A if tier == "A" else TIER_B).format(**fields)
        seat = SEATS[i % len(SEATS)]
        try:
            job = agency.dispatch_fn(seat, packet)["job-id"]
        except Exception as exc:
            print(f"  {pid}: DISPATCH FAILED ({exc})")
            continue
        dispatched.append({"at": sc.now_iso(), "problem-id": pid, "tier": tier,
                           "seat": seat, "job-id": job})
        print(f"  {tier}  {pid:8s} -> {seat}  job {job}")

    with OUT.open("a", encoding="utf-8") as fh:
        for rec in dispatched:
            fh.write(json.dumps(rec) + "\n")
    print()
    print(f"dispatched {len(dispatched)} of {len(picks)}; recorded in {OUT}")
    print("JOB IDS:", " ".join(r["job-id"] for r in dispatched))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
