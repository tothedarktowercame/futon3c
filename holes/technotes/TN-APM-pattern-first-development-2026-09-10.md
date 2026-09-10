# Pattern-first APM retrieval: existing methods, fragmented descriptions, unchecked fit

Codex-16, 2026-09-10. First bounded packet requested by Joe through codex-17,
under handoff `0949acf2`. Development investigation only; no Student execution,
production edits, campaign changes, publication, service calls or reloads.

Three findings answer Joe's question:

1. **A useful middle level already exists.** There are 136 committed canonical
   mathematical/proof-search patterns across 16 families, including concrete
   Grönwall, weak-convergence, bounded-image and representation-change methods.
   It is not just a handful of strategies plus isolated theorem names.
2. **Descriptions, recorded attachments and Student exposure do not coincide.**
   Four retained f211 snapshots contain 364 memories, with 400 distinct
   memory–pattern pairs marked reviewed and 102 distinct pattern IDs. Only 37
   of those IDs have a canonical file in this census. All 136 canonical files
   have catalog rows, but catalog search is separate from the Student's
   authenticated reviewed-memory search. Full cascade pattern offers exclude
   subject-suffixed families, including CA and FA. None of these counts proves
   that a particular Student read a pattern body.
3. **Ranking cannot decide applicability.** The six-query offline probe misses
   the Grönwall pattern from the operator task statement in all three arms.
   It retrieves the weak-convergence pattern for both its positive setting and
   a counterexample where the norm-limit premise fails. The smallest useful
   Student slice is an authenticated pattern-read/example-resolution receipt
   followed by a contextual condition check and an observed proof attempt.

Artifacts: `holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/`.
`frozen.json` contains source pins, exact pattern texts/fields/spans, sanitized
retained memory rows, queries and historical evidence. `result.json` contains
all top-ten rankings and exact recorded attachment pairs. `census.md` gives a
real representative from every family, IF/HOWEVER/THEN excerpts, metadata and
exact-ID attached examples where present. A recorded example is not thereby a
validated implementation of the pattern's whole method.

## What was counted, and what was not

The canonical library is futon3 `a376e404`, declarations under
`library/math*/` and `library/proof-search/`. The census parses declared IDs,
not filenames. The wider scan examines committed `.flexiarg` and `.multiarg`
files across all 14 repositories listed in `scripts/pattern_store_census.py`'s
watcher surface. It finds 141 relevant declarations / 138 distinct IDs:
136 in futon3 and five staging declarations in futon3c. Three staging IDs
duplicate canonical IDs; two do not. This is an exact committed-source census
for the declared prefixes, not every mathematical concept in every file or a
fresh live-store census. Other-family patterns about mathematics are outside
this operational scope.

| Family | Canonical patterns |
|---|---:|
| math-strategy | 20 |
| math-informal | 24 |
| math-informal-CA / CO / CT / LO / NA / RA | 6 / 2 / 7 / 1 / 2 / 2 |
| math-formalization | 31 |
| math-formalization-CA / CV / FA / GN / GR / MG | 23 / 7 / 3 / 2 / 2 / 1 |
| proof-search | 3 |

126 files have an explicit `+ IF:`, 133 a `+ HOWEVER:`, and 135 a `+ THEN:`.
The missing sections are not proof of missing conditions: several use other
labels. These are prose methods, not an executable prerequisite schema.
Metadata includes title, keywords, why/see-also and sometimes how/provenance;
`@why` is an authored relation to another pattern, not a memory-use witness.
For example, `ode-gronwall-api` points to `math-informal/reduce-to-known-result`.
The former specifies the analytic API and time-reversal obligations; the
latter supplies a general strategy. This is a real difference of granularity.

Other stores and history were kept separate:

- Eighteen committed `pattern-library-*.md` coined deposits contain 53 ID
  occurrences / 51 distinct IDs. Thirty-nine additional untracked deposit
  paths were inventoried without reading their bodies or committing them.
  `coined_pattern.clj:10,64` publishes rationale and witness references with
  `attachment-status=proposed`; publication is not independent review.
- The four retained f211 snapshot files contain 359, 364, 360 and 361 rows.
  Their union is 364 distinct memories; selected fields agree across repeated
  rows. They are retained data outside Git, pinned by exact byte hashes and
  sanitized through `sanitize_snapshot.bb`. The 400 pairs and their reviewed
  status come from explicit `pattern-ids`/`attachment-status` fields, not
  inferred similarity. They are historical snapshot assertions, not a fresh
  revalidation of 400 live hyperedges. The earlier audit's 57 new memories
  remain a subset, not the whole population.
- `retro-promotion-receipts.edn` retains concrete approvals **and rejections**.
  For example `e-54ea43ad-bdcf-4c0f-8bf6-2654e14a6ba3` was approved for
  `math-formalization/weld-range-lemmas-at-representation-seams`; another
  candidate was rejected because eliminating an exact differential did not
  establish independent pieces. These are stronger evidence than a draft ID.
- The historical split manifest records 83 patterns at its August 17 ruling,
  and explicitly moves analysis methods into subject families. Earlier
  `wire_math_memory_patterns.clj` uses `math/*` IDs and 13 lexical descriptions.
  Family logs in `frozen.json` retain the migration/backfill history, including
  `53ef3cd` and `04ad315`. Old IDs are not silently aliased to new IDs here.
- Definitions live in problem statements and Lean declarations, e.g.
  `WeakTendsto`, `apm_m02A01_weaklyConverges`, and the continuous-map domain
  of the Volterra operator. They are not counted as patterns. Proof bodies,
  memory implementations, draft descriptions and reviewed attachments are
  different objects, even when their prose overlaps.

The 65 snapshot pattern IDs absent from the canonical set are listed in
`result.json`; this does **not** say they are absent from the substrate.
One is `math-formalization/pass-interior-estimates-to-a-closed-endpoint-by-one-sided-limits`.
Its retained memory/review and earlier retrieval projection establish a
specific attachment even though there is no canonical flexiarg in this census.
Conversely, no exact-ID attachment to the canonical `ode-gronwall-api` occurs
in this 364-memory population. Do not create that edge from a shared word.
All 364 memories have at least one recorded pattern ID and 36 have two.
Thus there is already some explicit pattern indexing; it is not a single-parent
tree, nor does it supply a complete, checked hierarchy of definitions and methods.

The FA example is a different problem: seven retained memories attach to
`math-formalization-FA/weak-convergence-hilbert`, but the bodies include Fourier
probes and Lp elaboration advice. In particular
`e-apm-promotion-3cd1a2080b57e8df4ba88f9099560274` fixes a stuck `HolderTriple`
result exponent by type ascription. That is useful formalization advice, but
its body does not implement weak-plus-norm convergence. The broad attachment
is preserved, not revoked; selecting it still needs a task-specific fit check.

## Student consumption: source trace and observation limits

Source locations below refer to futon3c `0949acf2`, not inferred live JVM state.
Their Git blobs and hashes are pinned in `frozen.json.code`.

| Step | Actual producer → surface → next consumer | Limit or missing observation |
|---|---|---|
| Discover a catalog pattern | `transport/http.clj:2789–2855` loads `futon3/resources/sigils/patterns-index.tsv`; GET `/api/alpha/patterns/search?q=...` returns substring-hit counts over ID/rationale/hotwords | Catalog has all 136 rows. It returns neither full flexiarg nor an APM job-bound search receipt; availability is not Student use. Default top five; ties preserve TSV order. |
| Discover reviewed memory/pattern proposals | `typed_role_submission.clj:445` puts `apm-search-memory.py` in the role command → POST `/api/alpha/invoke/jobs/JOB/memory-search` → `role_memory_search.clj:80` | Authenticated Student role is permitted; max ten. Current prompt explicitly invites this path. |
| Rank/project results | `memory_recall.clj:319–349,542–693`: evidence FTS → typed memory/description/caption filter → reviewed mathematics projection | A raw catalog entity/file is not a typed evidence description. A description must project at least one currently reviewed memory. Pattern candidates ultimately sort by ID, not FTS score. |
| Resolve examples | `memory_recall.clj:111,219,429`: endpoint recall/batch recall follows existing reviewed attachments and returns compact memories; single-endpoint recall supports full bodies | Core resolver exists. No pattern-first authenticated read/example bundle is supplied by the shown catalog route. `memory-support` is a reference, not a read receipt or proof of fit. |
| Deliver cascade | `conductor.clj:563–833` expands reviewed seed attachments by sibling, why-hop and co-incidence; cycle request carries route-labelled offers; `live_learning_phases.clj:1097` renders them | Cap defaults to 20; route/ID ordering precedes the cap. `domain-general-pattern-id?` at 534 excludes uppercase subject suffixes from full pattern surfaces/offers: 78/136 canonical IDs pass; 58 do not. This does not exclude every memory attached to them. |
| Record search/exposure | `role_memory_search.clj:101–130` hashes/persists receipt with query, candidates and direct content; `receipt-surfaced-ids` derives identifiers; terminal controller joins current/repair receipts | Receipt proves returned content, not that the Student studied it. Pattern-description support uses plural `:memory-ids`; that form is not the singular `:memory-id` extraction in `receipt-surfaced-ids`. Resolve originals explicitly before claiming their exposure. |
| Check conditions/report use | Current Student command/card invite optional `apm-memory-caption.py observe`; `memory_caption_store.clj:249` checks job receipt, original memory, admissibility and revision; `live_learning_phases.clj:368–425` validates used IDs against controller-known exposure | Existing observation is memory-indexed. For nonhistorical observations, original memory must occur in receipt `:result-ids`; a pattern-only hit does not establish this. Student terminal carries used IDs, not mandatory pattern conditions or a pattern-use proof. |
| Learn a description | Caption propose → independent review → current-caption resolution (`memory_caption_store.clj:308,358,456,482`; `memory_recall.clj:466`) | Complementary implemented path. Author and observation author cannot review the caption. Source presence does not establish activation, publication or useful next consumption. |

There is also an instruction conflict to resolve before an experiment:
the current `zai-student-v2.md` says the snapshot is the complete authority,
forbids outside IDs and asks for every memory before starting and at a wall;
the current generator explicitly authorizes controller-recorded open search.
This report does not decide which blob an earlier Student actually obeyed.
A new frozen role card must state one coherent authority rule and be checked
against the generator. No instruction was bypassed during this offline probe.

Costs have distinct sources. The card specifies 30 minutes per attempt and
three attempts. Recall overfetches at most three times the requested limit,
bounded by 100, and batches at most 20 endpoints. The ten-result Student path
first searches generic evidence and captions separately; a four-token OR
fallback runs only when there are no admitted primary results. Unrelated
admitted hits can therefore suppress broader fallback.

The earlier retained f190–f213 audit observed 218 role-search receipts (95
Student), 34 nonempty cascade deliveries drawing on only 20 distinct IDs,
median expansion latency 326.568 seconds, and no accepted use of a cascade
offer. Its 22 accepted memory-use events contained no fingerprinted
cross-problem transfer. These are inherited, bounded historical observations;
they neither establish present latency nor prove transfer impossible. No new
network timing, body-read receipt or Student-use result is claimed here.

## Two cross-problem candidates, and one additional condition check

### 1. m00A05 → m96J04: interior comparison followed by an endpoint limit

The source statement asks for unique solutions of `u' + u = cos u`, initially
specified at zero and differentiated for positive time. The target statement
asks for a compact Banach-space operator without nonzero eigenvalues. The
target's chosen Volterra construction turns its eigenvector equation into
`f' = f/λ` in `(0,1)`, with `λ ≠ 0`, and proves `f(0)=0` and continuity on
`[0,1]`. Those helpers are present in pinned `apm-lean/problems/m96J04/lean/Main.lean`;
its comments explicitly identify the initial-endpoint derivative gap.

The canonical `math-formalization-CA/ode-gronwall-api` says to choose the
distance or norm-difference Grönwall API, with Lipschitz trajectories, and
handle negative time by reversal. It supplies a useful analytic search layer,
but does not itself state the endpoint-limit repair. The narrower retained
memory `e-apm-promotion-5fdb99169bd788313841375c797c302c` supplies that repair.
Its independent review `e-apm-promotion-review-02bee0ac9d6dbd565d46b51fb03696e7`
**reassigned** it away from a method reconstructing endpoint derivatives to
`pass-interior-estimates-to-a-closed-endpoint-by-one-sided-limits`.
This is explicit evidence that mathematical conditions already matter in review.

| Condition | Status in the bounded Volterra implication |
|---|---|
| Continuous trajectories on the closed interval | Established by retained target helper; endpoint continuity must not be replaced by mere interior continuity |
| Interior ODE and equal initial values | Established by retained eigenvector helpers and comparison with zero |
| Lipschitz field | Derivable for `y ↦ y/λ`, with constant `|1/λ|`; assembled in the prior reference proof |
| Derivative at zero | Unchecked/unprovided; the method avoids requiring it, not silently assumes it |
| Bundled compact Volterra operator exists | Outside this implication; target construction/compactness remain open |

The earlier `EndpointTransfer.lean` and `lean-check.txt` are frozen here as
**inherited** evidence: the reference implication compiled without `sorryAx`
in that investigation. I read the target, source development, memory, review
and reference proof; I did not rerun Lean or prove the whole problem. That
reference used source proof evidence as well as the memory. This is neither
a memory-only success nor autonomous discovery. Removing endpoint continuity
invalidates the limiting step: the function equal to one on `(0,1]` and zero
at zero has derivative zero inside yet is not the zero solution.

### 2. a93A03 → m02A01: weak-to-strong upgrade must be rejected in the counterexample

The source explicitly assumes weak convergence in a Hilbert space **and**
convergence of norms to the norm of the weak limit. Its retained Lean theorem
`norm_sub_tendsto_zero_of_weak_and_norm` expands `norm_sub_sq_real`, takes the
inner-product limit and norm-square limit, then applies continuous square root.
That is precisely the main route in the committed
`math-formalization-FA/weak-convergence-hilbert` pattern. Part (b)'s real-valued
liminf argument separately requires filter side conditions; the retained source
has a completeness/boundedness repair. Do not confuse those requirements with
part (a)'s squared-norm calculation.

The target asks when weak convergence implies strong convergence and demands
a counterexample. Its actual Lean development defines weak convergence using
`WeakSpace`, and proves the standard `lp.single 2 n 1` sequence weakly tends
to zero while its norms remain one. For this subtask, Hilbert structure and
weak convergence are established; convergence of norms to the norm of the
weak limit is **absent**, not merely unchecked. Applying the source upgrade
would contradict the target's verified algebraic norm computation.

For the target's general normed-space clauses, Hilbert structure is also not
given. Its finite-dimensional clause is proved through continuous coordinates,
not by inventing an inner product instance. To use the source pattern in a
future positive variant, establish the norm-limit premise and a bridge between
the target's continuous-dual weak topology and the source's inner-product test
definition. No such bridge is supplied by the word “weak” alone. This pair is
a plausible retrieval candidate with an explicit rejected application, not a
second successful transfer. The target proof is inspected source, not a fresh
compilation result.

Additional continuity contrast: `a93A01` works on the totally bounded interval
`(0,1)` and motivates `uniform-continuity-boundedness`. `m99A04` assumes a dense
subset of complete metric spaces and uniform continuity, but no total
boundedness. A uniformly continuous identity on the complete space ℝ with
dense subset ℝ already has unbounded image. Thus the bounded-image route is
not justified for that general target. Moreover the pattern's phrase “bounded
— or totally bounded” overstates the general metric-space premise: an infinite
discrete metric space is bounded and every map from it is uniformly continuous,
including an unbounded map to ℝ. Keep the valid interval setting distinct from
that generalization. This is a content-review finding, not a silent repair.

## What actually ran offline

Freeze: the same 136 canonical patterns and six analyst-authored queries for
every arm; top ten; no model, token budget, live evidence index or service.
Five queries paraphrase the inspected task statements. `operator-residual`
is explicitly the previously tuned “ODE uniqueness endpoint” query, requiring
knowledge of the target's Volterra residual. It is not counted as statement-only
discovery. No sealed material was opened, searched or copied.

Three arms distinguish availability from an experiment with the live Student:

1. **Catalog keyword count:** a Python port of the current catalog function's
   substring counting and stable TSV ties, restricted to the frozen 136 IDs.
   The original catalog fields and row order are frozen. This is the available
   catalog algorithm offline, not an HTTP invocation or full-catalog ranking.
2. **FTS component:** SQLite FTS5 `unicode61`, default quoted AND, falling back
   to OR over the first four distinct non-stopword tokens of length ≥4, with
   body BM25. This reproduces lexical mechanisms, **not** the complete reviewed
   evidence projection, caption lane, overfetch crowding or authenticated gate.
3. **Exploratory pattern fields:** OR over all query tokens, BM25 weights
   title/keywords 2, context/IF/HOWEVER/THEN 3, full body 1. It is a local
   development index, with no learned weights, embeddings or inferred edges.

| Query / designated candidate | Catalog | FTS component | Pattern fields |
|---|---:|---:|---:|
| differential equation initial value unique solution / Grönwall | absent | absent | absent |
| compact linear operator Banach space no eigenvalues / Grönwall | absent | absent | absent |
| ODE uniqueness endpoint / Grönwall | 2 | 10 | 1 |
| Hilbert weak convergence norm convergence / weak-convergence-hilbert | 1 | 1 | 1 |
| weak convergence does not imply strong convergence counterexample / same pattern | 2 | 1 | 1 |
| complete metric spaces dense uniformly continuous extension / bounded-image pattern | absent | absent | 3 |

“Absent” means outside the returned top ten. Exact returned IDs, scores and
source pointers are in `result.json`. Lower FTS/BM25 scores rank first; catalog
hit counts rank descending. Conditions in query records are analyst judgments
from the inspected statements/proofs, not generated by the scorer. The negative
cases therefore remain negative even when their patterns rank well. The arms
vary query composition and fields together; this does not isolate a single
scoring change or show causal learning. In particular, increasing description
coverage can increase misleading exposure too.

## Smallest proposed Student slice

Implement **one authenticated pattern-read/resolve action and its receipt**
alongside the existing role-memory search. Do not change the live cascade or
its eligibility rule as a shortcut. This is a structural addition for any
mathematical pattern, not a CA/FA special case.

Input: current job authority, a controller-recorded discovery receipt, selected
pattern ID/revision, and a target obligation pointer. Output: full versioned
pattern content or explicit missing-description status; at most two exact
reviewed attached memories with original IDs, revisions and review provenance;
and a hashed receipt of precisely what was returned. Resolve through existing
`recall-by-endpoint(s)` and existing visibility/domain/lifecycle/holdout rules.
Unreviewed attachments stay excluded; a readable authored pattern without a
reviewed example remains an authored method with zero admitted examples.

Add a distinct typed authored-pattern discovery result to the same role
authority boundary for catalog hits; do not make those entities impersonate
reviewed memories or weaken recall's admission invariant. A supplied result
must distinguish authored pattern status from independently reviewed example
status. If the selected narrow pattern has no full description, report the gap
and retain its reviewed memory as the available evidence; separately authored
descriptions require review before any new publication.

The receipt must explicitly include resolved original memory IDs in the
controller's exposure set, and the existing applicability path must recognize
that typed receipt. Tests must fail when a candidate has only a pattern hit,
an unreviewed/retracted example, a mismatched original revision or a withheld
depositor. Do not manufacture an old search receipt to satisfy observation
validation. Exact-ID aliases or migrations require their own authority.

Then ask the Student to record a bounded condition matrix for the actual goal:
condition, definition/source pointer, established/absent/unchecked, and witness
or obstruction. Reuse the memory-applicability condition vocabulary; add the
selected pattern revision, pattern-read receipt and proof-obligation pointer.
Treat pattern-level and memory-level observations distinctly so that rejecting
a general method does not become an invented observation about an unread leaf.
The checker verifies provenance/shape, while the proof/reviewer checks the
mathematics; presence of an “established” label is not a theorem certificate.

Definitions should be explicit typed source references, not new patterns:
closed-interval continuity and its one-sided limit; Lipschitz bound; continuous
dual versus Hilbert inner-product weak convergence; norm versus weak topology;
bounded versus totally bounded. Supply a bounded source excerpt and exact
declaration revision when needed, keep any equivalence bridge as a separate
proof obligation, and record which definition was actually consulted.

Finally bind an attempted proof step or a reasoned rejection to the selected
pattern, consulted originals and artifact diff/checker result. Existing
`used-ids` remains an honest memory-use account. A new pattern-use observation
must record what proof action changed or why no application was attempted.
Do not reward citation, a search hit or token overlap as successful transfer.

**Prompt-only part:** align the frozen role card with existing authenticated
search authority; ask for at most two candidate methods and the above premise
check, with permission to reject both. This can improve investigation habits,
but cannot provide a missing full description, resolve examples, repair a
receipt gap or certify use. Keep that intervention separate from endpoint work.

**First later diagnostic, proposed only:** use the already analyst-validated
m96J04 implication with frozen target prerequisites and a separate negative
premise case. For fresh sessions, freeze task, corpus revisions, model version,
tools, token/time budget and reference exclusions before dispatch. Compare
existing retrieval, existing retrieval plus condition-check prompt, and the
same prompt plus structured pattern/example access. A matched-exposure arm
giving the same selected content without discovery separates retrieval from
adaptation. Count retrieved, exposed, read/considered, premise-checked, claimed
used and proof-supported used separately; a disconnected resolver must fail.
The reference adaptation and source completed proof must not be available in
a memory-only arm. This first deliberately selected case cannot estimate
transfer prevalence. No experiment is activated by this proposal.

## Reproduction and checks

From futon3c:

```sh
python3 holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/probe.py replay --output /tmp/pattern-first-result.json
cmp holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/result.json /tmp/pattern-first-result.json
bb holes/labs/M-apm-demonstration/analysis/pattern-first-development-2026-09-10/check_catalog.bb
```

`replay` uses only the committed frozen JSON and in-memory SQLite. `freeze`
rebuilds inputs from Git objects (futon3c fixed at `0949acf2`) plus the four
explicit retained snapshot paths, checks each snapshot unchanged after reading,
and passes only whitelisted fields through the sanitizer. It intentionally does
not scrape role packets. Other repo revisions and snapshot hashes must match
the frozen pins for a historical regeneration claim. The handoff file hash was
also checked equal to its `0949acf2` blob. A first attempt to discover snapshots
in Git returned zero because these retained files are untracked; explicit
hashed paths corrected the census before any reported population result.

Validation: offline replay agrees byte-for-byte; the pinned production catalog
scorer, extracted and executed without loading its namespace, agrees with the
Python port on all six queries. 203 committed source hashes, four retained
snapshot hashes, memory-row hashes, ranked source pointers and unique canonical
IDs checked; negative applicability cases retained. Python compilation,
both scripts' clj-kondo (0 errors/warnings),
`futon4/dev/check-parens.el` and diff whitespace check pass. No production test
suite or Lean run was needed for this evidence-only packet. Historical memory
bodies/reviews were not edited. Concurrent coordinator/session edits, coined
drafts and codex-10 RUN4 work were left untouched. The completion response
auto-routes to codex-17; no duplicate bell is sent.
