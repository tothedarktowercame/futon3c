# Descriptive captions for memory retrieval

Codex-17, 2026-09-10. Joe proposes historical enrichment from problem/solution
context and descriptive captions on future memories. This note makes that
proposal concrete with one grounded draft and an isolated lexical check.
No historical evidence or live index was changed; no campaign was paused.

## Correction and opportunity

A free-text index already exists. `futon1b/futon1b_text.clj:155` indexes the
whole evidence body (a string directly, a map through pr-str), so a memory's
name, hook and narrative already participate. It uses FTS5 unicode61 without
stemming; `match-string` at line 371 quotes query tokens and uses AND by default.
It does not infer that “compare two ODE solutions” answers “ODE uniqueness.”
The earlier production search probe found the selected memory for “Gronwall”
but missed it for “ODE uniqueness endpoint.”

Thus the proposed improvement is a grounded applicability description in the
language a problem-solving agent is likely to use, including synonyms and the
kind of obstacle. It is not the introduction of text search for the first time.

## One concrete caption

Memory: `e-apm-promotion-5fdb99169bd788313841375c797c302c` from f196/m00A05.
Draft, not published:

> ODE uniqueness from equal initial values when derivatives are known only in
> the interior of an interval. Compare two continuous solutions away from the
> initial endpoint, apply a Gronwall distance bound, and pass to a one-sided
> limit to recover the endpoint estimate. Useful when the available uniqueness
> theorem requires an endpoint derivative that has not been established.
> Requires an appropriate Lipschitz bound and continuity at the endpoint.

The retained source proof establishes the endpoint uniqueness step. It does
not establish the source problem's entire existence/convergence claim; the
caption does not promise either. The later m96J04 reference adaptation supports
a separate applicability record, not a rewritten claim of historical student
transfer. Problem IDs and source pointers remain provenance fields rather than
being inserted into reusable prose to game retrieval.

The source memory body was frozen as EDN. An isolated SQLite FTS5 index using
the same tokenizer and query quoting was run on that body and on body plus
caption:

| Query | Original | With caption |
|---|---|---|
| ODE uniqueness endpoint | no match | match |
| Gronwall | match | match |
| global existence contraction | no match | no match |
| compact operator construction | no match | no match |

`analysis/memory-caption-probe-2026-09-10/probe.py` asserts these outcomes.
This is a vocabulary mechanism check on an intentionally chosen query and two
negative controls. It is not an evaluation of live ranking, precision on the
full corpus, or generalization to independent queries. A held-out evaluation
must fix its queries before captions are authored.

## Historical enrichment

For each memory, bind the source problem, source attempt, review and original
content digest. Read the actual use site where available. Produce:

- a task description and obstacle in ordinary mathematical language;
- the reusable method and likely query variants;
- prerequisites and circumstances where it does not apply;
- what was observed in the source artifact versus a proposed future use;
- source pointers and a caption revision identity.

Unknown or missing use evidence remains unknown. A source problem association
alone cannot justify claiming that the memory solved that problem. Regulative
memories need process evidence; a Lean-token test is not a substitute.

Append the caption as a versioned, reviewable description associated with the
immutable memory. Do not silently rewrite signed memory bodies or old snapshots.
A caption search hit must resolve back to the original currently reviewed memory
and pass the existing visibility, domain and holdout checks. The caption is
retrieval metadata, not a new mathematical result or a new authored pattern edge.
Conflicting or retracted captions need an explicit lifecycle and retained history.

## Forward loop: the same contract

The scribe proposes the caption from the attempt it actually observed. The
independent reviewer checks entailment, applicability and overclaim; persistence
records both revisions; read-back and search verify that the admitted caption
reaches the next consumer. Later retrieval misses or misleading hits can produce
a reviewed correction to that description. This is an actual candidate learned
object: a versioned applicability description, updated from observed retrieval
and use evidence, consumed by the next retrieval. It does not on its own define
an AIF parameter-learning equation.

A prompt-only change is insufficient. The current candidate persistence seam
selects only name/hook/kind/body/why/how-to-apply/admission-schema
(`futon3c/src/futon3c/apm/promotion_candidate_store.clj:38`). A newly added caption
key would be dropped there. A separate caption evidence type would also need an
explicit projection from its search hit to the reviewed memory; indexing a new
document alone does not make the current recall validator accept it. These are
implementation obligations, not reasons to bypass review or expand permissions.

The first implementation/evaluation packet should use a small pinned cohort,
keep original and captioned retrieval arms on the same memory population, and
measure relevant recall and false positives on independently authored queries.
Include caption-hit-to-memory resolution, stale/retracted caption rejection,
holdout preservation, and absence-of-caption compatibility. A larger historical
backfill should follow that check; the candidate here is not a license to
fabricate applicability across the whole store.

For Cascade Live, this keeps the forms distinct: patterns describe reusable
methods; institutional rules govern truthful captions and review; retrieval
consumes those descriptions when proposing a next task. No numerical preference
weights are inferred. The immediate next-consumer check is whether an agent can
find an appropriate memory without already knowing its specialized API name.

## Artifacts

`futon3c/holes/labs/M-apm-demonstration/analysis/memory-caption-probe-2026-09-10/`:
caption.json (grounded draft), original-body.edn (frozen prior content), probe.py
(isolated executable check), result.json (observed output). Source evidence and
reference proof remain in the preceding memory-first-probe directory.

Validation: the four FTS assertions pass and Python compilation passes.
flake8 is unavailable in the system interpreter (No module named flake8);
no flake8 success is claimed. No production source changed.
