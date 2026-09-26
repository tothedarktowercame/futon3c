# Memory-first transfer: a concrete pair and a checked missing step

Codex-17, 2026-09-10. Joe proposed selecting a problem from the memory store's
available knowledge, separately from the campaign, to investigate missing
cross-problem reuse. This note records a **retrospective feasibility probe**,
not a new campaign result or a blinded student experiment. Campaign state and
canonical problem files were not changed. No pause was requested from the
controller and no agent was dispatched.

## Result

There is at least one concrete opportunity for cross-problem transfer. A
reviewed memory from **m00A05 / f196** describes moving an ODE comparison from
an interior interval to its endpoint by a one-sided limit. **m96J04** has a
still-open Volterra eigenvector argument with exactly that endpoint obstacle.
A reference adaptation now proves the missing uniqueness implication against
the actual target's hypotheses in Lean. It does not construct the Volterra
operator or prove its compactness, so it does not close the whole problem.

The same memory is also sensitive to query vocabulary: the production reviewed
retrieval function misses it for “ODE uniqueness endpoint,” but ranks it first
for “Gronwall.” That is direct evidence of a retrieval opportunity being missed
by one reasonable query, not proof that students never try the right query.

## Memory-first selection, with exclusions recorded

The source is the stored memory
`e-apm-promotion-5fdb99169bd788313841375c797c302c`,
**interior-gronwall-to-endpoint-by-one-sided-limit**. Its statement says to apply
an interior bound on [δ,t], then take δ toward zero from above; continuity at
the endpoint supplies the limiting initial value. It explicitly warns against
a destructive neighborhood rewrite and gives the filter-membership construction.
Its source problem asks about existence, uniqueness and long-term behaviour of
u′ + u = cos(u). The memory's reviewed origin and full content are frozen in
`memory.json` and `review.json` beside the probe artifacts.

Selection began with the 57 new memories identified in the preceding audit.
I inspected their actual stored names and then the candidate bodies; this was
a purposive search, not a random relevance sample. I considered the parametric
integral memory from f211/m02J04 for m01J06, but rejected that as the first probe:
the target already names the central API and still needs domination/assembly.
Simply supplying the API name would repeat what the target already knows.

The chosen target's part (b) asks for a compact operator without nonzero
eigenvalues. Its existing development uses the Volterra operator and already
proves:

- a putative eigenvector for a nonzero eigenvalue vanishes at zero;
- its extension is continuous on [0,1];
- in the interior it satisfies f′ = f/λ.

The remaining commentary specifically identifies the mismatch between its
interior derivative and an ODE uniqueness API requiring an endpoint derivative:
`apm-lean/problems/m96J04/lean/Main.lean:184`. The memory supplies a way to avoid
requiring that unjustified endpoint derivative. This is a mathematical method
transfer from nonlinear ODE uniqueness to an operator-eigenvector problem,
not another attempt at the same problem and not mere shared topic vocabulary.

## What actually ran

### Reviewed retrieval, in an independent process

`retrieval.clj` invokes `futon3c.peripheral.memory-recall/propose-patterns-by-query`
with domain mathematics and limit 10. It uses the same lexical-plus-reviewed-
projection function called by the students' authenticated endpoint. It does
not impersonate a student, use an old job token, append a student-use receipt,
or exercise the role-specific holdout wrapper. The selected source is a
different problem from the proposed target.

| Query | Target memory's direct-content rank | Direct memory matches |
|---|---:|---:|
| ODE uniqueness endpoint | absent | 2 |
| Gronwall | 1 | 2 |
| interior-gronwall-to-endpoint-by-one-sided-limit | 1 | 1 |

All three ran the full-query strategy. Responses took about 2.2, 2.2 and 1.4
seconds respectively. The exact-name query is a reachability control, not a
realistic blind retrieval success. “Gronwall” was also chosen with knowledge
of the memory; the probe establishes reachability and query sensitivity only.

The implementation only runs its bounded token-disjunction fallback when the
primary query has no admitted content or pattern results
(`futon3c/src/futon3c/peripheral/memory_recall.clj:539`). A primary hit is therefore
not a guarantee the needed memory was found, and does not trigger broader
fallback. That is a concrete boundary to test in retrieval-quality work.

Historical student searches are genuinely active. For example, f211 recorded
“differentiate under integral sign parameter derivative intervalIntegral
HasDerivAt integral of F x (y x + t eta x)” with zero result IDs. Other queries
returned material. Such observations motivate examining query/answer pairs;
they do not by themselves establish whether the desired memory existed and
was admissible at that earlier time. No current search result is substituted
for a historical result.

### Lean reference calculation

`EndpointTransfer.lean` contains:

1. `MemoryFirstProbe.endpoint_unique_on_unit`: Lipschitz ODE trajectories
   continuous on [0,1], differentiable only on (0,1), and equal initially are
   equal on [0,1]. It uses the interior Gronwall bound and one-sided limiting
   argument described in the memory.
2. `MemoryFirstProbe.scalar_ode_zero`: the corresponding result for u′ = c u
   with zero initial value, still without an endpoint derivative premise.
3. `VolterraTarget.eigenvector_zero`: applies that result with c = 1/λ to the
   actual Volterra hypotheses and previously established target helpers.

The target prerequisite prefix is copied verbatim from the pinned Main.lean
into a separate namespace; it contains no incomplete proof. The original file
is untouched. The new implication remains conditional on the operator formula,
which the larger target must still construct and prove compact.

A counterexample control rejects equality of constant trajectories 1 and 0
without equal initial values. Compilation exits 0; all three axiom prints list
only `propext`, `Classical.choice`, `Quot.sound`, with no `sorryAx`.

**Important limit:** I consulted the retained f196 attempt-1 proof while making
this reference adaptation, as well as the memory and Mathlib. The result proves
the method is applicable and the target step can be completed. It does not
prove that the memory text alone was sufficient for me or a Zai student, or
that either learns faster with it. The source proof is explicitly in the basis.

## The next student diagnostic

Use the checked target implication as the bounded task. A fresh Zai session
gets the frozen target prerequisites and the actual stored memory. It may use
the ordinary Mathlib source and Lean checker. It should report what it used,
where it helped, and any remaining obstruction; no reward for citing a memory
it did not use. The reference adaptation and f196 proof must be excluded from
the allowed inputs and prompt. Retain consulted-path evidence; access to either
reference invalidates a memory-only interpretation of that attempt.

A successful build would establish one deliberately chosen transfer example.
A failure would tell us whether the missing work is memory content, adaptation
skill, or tooling, rather than being obscured by an unrelated problem choice.
A separate search-only session could then test discovery of the same memory
without its name or the hint “Gronwall.” Do not teach the answer and later call
that same session an independent retrieval test.

A no-memory control would be necessary for a causal benefit claim, but not for
this first feasibility demonstration. Likewise, choosing a task because a
memory is relevant is legitimate for this diagnostic; it does not estimate
transfer prevalence in the original queue. Keep both denominators.

A short hold before starting another campaign batch seems justified while this
probe is designed and run. It should use the campaign's ordinary safe boundary;
the present work neither cancels an active job nor mutates its inputs. Joe's
question about whether a pause is worthwhile has not been treated as an
instruction to stop the running campaign.

## Consequence for Cascade Live and War Machine

This supplies a concrete interpretation of R-D: the outer loop can propose a
task because a reviewed memory appears relevant to an unresolved obligation,
then record that selection reason. The relation is a proposed applicability
judgment, not a fabricated authored pattern edge. R-A requires the origin,
review and outcome witness; R-B requires joining these records; R-C requires
the failure or correction to reach the next learner. A pattern describes the
endpoint method; institutional rules govern permissible evidence, review and
experimental attribution. None of these requires inventing a preference weight.

## Artifacts and checks

Directory:
`futon3c/holes/labs/M-apm-demonstration/analysis/memory-first-probe-2026-09-10/`.
`basis.json` pins source files and revisions; `retrieval.json` preserves the
three responses; `memory.json` and `review.json` preserve the selected evidence;
`lean-check.txt` records the successful axiom prints. The script writes its
result to /tmp, never under data/.

Run `clojure -M <path-to-retrieval.clj>` from futon3c in its own process.
Run `lake env lean <absolute-path-to-EndpointTransfer.lean>` from canonical
apm-lean, using its existing package installation. An initial invocation from
futon3c lacked Mathlib on the search path; it was rerun from the correct checkout.
The first Lean draft also needed `NNReal` rather than unscoped notation. Neither
failed attempt is counted as a successful witness. Final Lean exit 0,
retrieval script clj-kondo 0 errors/0 warnings, check-parens OK, diff check clean.
