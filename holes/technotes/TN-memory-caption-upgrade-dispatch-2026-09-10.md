# Commission: structured applicability captions and their learning loop

2026-09-10, Codex-17. Two Agency jobs commissioned by Joe: historical enrichment
and apparatus support for new memories and caption updates. Earlier evidence:
`TN-APM-memory-current-audit-2026-09-10.md` (330315fe),
`TN-APM-memory-first-transfer-probe-2026-09-10.md` (74622e23),
`TN-APM-memory-descriptive-captions-2026-09-10.md` (cc62da46).
Implementation is authorized; the field names and initial size budgets below are
engineering proposals for the two owners to reconcile, not separate Joe rulings.

## Joe's instruction, verbatim

> Yeah, and I think it'd be good to put the notes. In a structured way, so rather than saying, not useful for this problem because we lack continuity, we would want to put it in a more positive form. This memory is useful when... Continuity is present. Something like that. Or may be useful, as you pointed out, with suggested applications. Anyway, I wonder if we have enough information now to start to upgrade the way the memory system is handled procedurally with... Both captions and... These updated captions so that we might see. And better retrieval. And like I was saying, there's a historical deep dive process to go through and examine each memory in place. And update its... Caption. So that would be a caption. Ah. Job to Dispatch. As well as an apparatus job that would. Keep it fresh and Relithum. What's up? Yes, heh-heh. Relevant.

The immediately preceding discussion proposed a bounded searchable caption,
linked to an append-only log of contextual use judgments, with periodic reviewed
compression. Joe's correction asks for positively expressed applicability rather
than contextless rejection prose. His uncertain transcription above is retained.

## Shared contract and example

Each applicability observation binds a memory ID and revision, problem/task,
attempt/search receipt, author, time and evidence pointer. Its structured content:

- `useful-when`: positively expressed condition, such as “comparing continuous
  ODE trajectories with equal initial values and a Lipschitz vector field when
  derivative information is available only in the interval interior.”
- `epistemic-status`: supported / suggested / unknown. Supported names the
  evidence actually checked; suggested states a possible application explicitly.
- `conditions`: individual prerequisites and whether each was observed,
  established, absent or unchecked in this attempt.
- `task-observation`: what happened here (used, considered, not used, unresolved)
  and why, with the original evidence retained.
- `suggested-contexts`: optional proposals, each with its own status and basis.
- `scope-limit`: what the memory does not establish; revision/supersession links.

Positive wording must not erase contrary evidence or turn “we did not establish
continuity” into “continuity is absent.” Nor does one successful use establish
that a listed condition is logically necessary, or sufficient by itself. The
caption describes a supported setting, with uncertainty preserved.

The searchable caption is a bounded projection of reviewed observations, not
the raw growing log. Initial engineering budget: at most 120 words; the owners
may choose a more appropriate enforceable token/byte budget and record why.
Compression retains source links, qualifications, contradictions and the previous
caption version. Over-budget output is revised or refused, never blindly cut in
mid-claim. Historical evidence and signed memory bodies remain immutable.

A caption hit resolves to the original currently admissible reviewed memory;
domain, role and depositor holdout checks remain in force. Caption metadata is
not a new mathematical memory, proof, or authored pattern edge. A suggested
application cannot become supported merely through repeated compression.

## Job H — historical captioning (owner/review coordination: claude-1)

1. Census and pin the APM-relevant historical memory population. Include older
   retained memories, not only the latest 57; scope by actual APM provenance/
   exposure and current admissibility. Report boundaries, pagination and unknowns.
2. First bounded delivery: the 57 memories in the audited f190–f213 deposit
   cohort. For each read the original memory, problem, actual source/use artifact
   where retained, and independent review. Draft caption and structured conditions.
   Never infer a solved problem from a partial attempt or name alone.
3. Produce a coverage manifest: drafted / already adequate / missing evidence /
   conflict / review pending, with every scoped member accounted for. Missing
   sources result in an explicit disposition, not invented supporting language.
4. Independent content review before publishing. Keep candidate products in lab
   files while the apparatus publication contract is being built. Coordinate its
   schema with Job A. Then process the remainder in bounded, resumable slices.
5. Prepare held-out query/relevance judgments before caption authors see those
   queries. Compare original/captioned retrieval on the same memory population;
   include precision, misses, suggested-only cases and irrelevant-query controls.
   The already tuned “ODE uniqueness endpoint” probe is development evidence only.

Deliver the pinned census, reviewed first-slice artifacts, coverage and validation
results. Do not silently bulk-rewrite store entries or historic campaign records.

## Job A — apparatus implementation (owner/review coordination: claude-5)

1. Reconcile the shared schema with H; trace all consumers before modifying it.
   Current text index already includes the whole evidence body. Candidate storage
   drops unknown fields (`src/futon3c/apm/promotion_candidate_store.clj:38`), so
   changing a role-card prompt alone cannot deliver captions to search.
2. Implement versioned caption and applicability-log admission, persistence,
   read-back, independent review, supersession and caption-hit-to-memory lookup.
   Keep original memory and current review authority; no duplicate mathematical
   assertions created just to get a caption through the search validator.
3. Update the forward procedural path: scribes draft captions; reviewers check
   them; students may emit a small number of structured applicability observations
   for the closest memories they actually considered. Bind them to real search/
   exposure receipts. Do not require fake usage or observations on unseen results.
   A student proposes an update; it does not approve its own searchable revision.
4. Add bounded, reviewed compression. Record contributing observation IDs,
   original/caption revisions and basis. Trigger on a declared evidence threshold
   or review event; no unbounded rewrite on every search. Keep the append-only log.
5. Demonstrate next consumption: a reviewed update is persisted and retrievable
   by an ordinary task query; record the caption revision in the retrieval receipt
   and preserve downstream use/outcome evidence. Unreviewed, contradicted and
   retracted captions cannot silently serve as current supported descriptions.
6. Tests: positive-worded observation with absent vs unchecked prerequisite;
   suggestion preserved across compression; provenance completeness; author/reviewer
   separation; immutable history; size limit; caption-to-original-memory join;
   holdout/domain/visibility preservation; unknown source; disconnected consumer;
   absent-caption compatibility; relevant query and negative-query controls.

Use normal coding handoff: Codex implements, a separate Claude owner reviews.
No live shared-JVM loads from branches, no restart, no mid-frame input changes.
Land bounded commits with appropriate namespace tests, clj-kondo and check-parens;
any futon1b work follows its local instructions and ownership. A new schema may
not bypass existing authority or publication gates. Surface a genuine conflict.
Wire capability and agree a safe future-frame activation boundary with campaign
ownership; do not interpret this dispatch as authorization to pause or cancel
an active campaign. H publication follows the reviewed admission machinery.

## Completion and relationship to learning

The two jobs share an interface; historical authoring can proceed while A builds
it, but publication waits for valid admission and review. Owners report actual
SHAs, tests, coverage and any blocked seam. This commission is for both jobs,
not merely two further design notes. Initial bounded deliveries are the start
of the historical pass, not a claim that all memories have been enriched.

This creates a specific learning loop: retrieval/use evidence → attributed
applicability observation → reviewed caption revision → observed next retrieval
and use. It does not by itself certify AIF parameter learning or improved solver
success. Those claims need their own checks; the immediate aim is better grounded
and more discoverable descriptions without loss of evidence.
