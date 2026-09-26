# APM memory now: retained learning, weak transfer, expensive repeated offers

Codex-17, 2026-09-10. Requested by Joe: audit the recent APM memory system as
potential groundwork for War Machine learning, keeping the four Cascade Live
requirements in view. **Read-only audit; no policy change or experiment activated.**

The write/review/retrieval machinery is demonstrably operating. There is evidence
of memory-associated work on the same problem, including a later frame. There
is not yet evidence in this window of useful transfer between different problems,
or of any use of the cascade additions. The latter repeatedly offers essentially
the same small set at substantial cost. Reuse the evidence and review contracts;
do not yet treat the cascade as a validated general learning mechanism.

## Scope and reproducible evidence

Fixed cohort: **f190–f213 of jit-all-open-v3**, the 24 numbered frame directories
preceding f214 at inspection. This is not a selection of 24 successful closes.
Nine have certified close receipts; 15 do not. There are 35 student-attempt
records, 32 certified and three still recorded as dispatched. Missing student
attempts are not scored as failures to use memory. No experiment was rerun.

Artifacts are in
`futon3c/holes/labs/M-apm-demonstration/analysis/memory-audit-2026-09-10/`:

- `census.bb` reads structured EDN and emits sanitized `census.json`; it does not
  copy packet credentials or contact a service. The two census runs were
  byte-identical, with 396 input files hashed and checked unchanged after reading.
- `fingerprint.json` is a fresh execution of the existing fingerprint analyzer
  over the campaign; its parsed result equals the retained campaign audit.
  `summary.json` separates the cohort and certified receipt uses below.
- `readback.json` records fresh GET-only checks of each of the 57 new memories
  and its review. `basis.json` pins code, artifacts and the apm-lean origin/master
  used for the fingerprint rarity baseline. This is a time-stamped observation,
  not a promise that a mutable store will give identical responses forever.
- `checks.txt` retains test tails. No live process was loaded or restarted.

Run the census from futon3c:

```sh
bb holes/labs/M-apm-demonstration/analysis/memory-audit-2026-09-10/census.bb > /tmp/apm-memory-census.json
python3 holes/labs/M-apm-demonstration/analysis/fingerprint_audit.py --campaign jit-all-open-v3 --write /tmp/apm-memory-fingerprint.json
```

The fingerprint analyzer reads retained attempts, their archived Lean sources,
pinned base files and the substrate evidence endpoint. It does not establish
causality: token association can arise without the memory causing the proof.
It also depends on the current rarity corpus and current evidence retrieval;
those limits apply even though this run reproduced the retained result.

## What changed since the previous technical notes

| Earlier finding | Current observation | Boundary |
|---|---|---|
| Cascade built but absent from student path (TN-APM-cascades-exist-unused, including its later shipping addenda) | 34/35 attempt records carry 19 or 20 route-labelled offers; all 35 archived packets exist and include every offered ID | Delivery of an offer is not reading its body or using it |
| f42 shelf hash order | Snapshot code has explicit ordering and typed-kind observation | The *cascade* still orders equal-route candidates by memory ID before its cap |
| f37 irrelevant shelf, correct zero uptake | Large shelves persist, 304–361 accessible IDs per attempt; the cascade supplies only 20 distinct IDs across all 34 nonempty deliveries | Volume and connectivity do not establish relevance |
| F66–F70 close-frame audit produced empty rows while reporting success | Fixed in `973881e6`; current retained audit has 32 campaign rows and fresh regeneration agrees | Four cohort rows describe unaccepted reports, not certified uses |
| Earlier Solver had no memory channel | No Solver shelf canary is active in these frame records; role-memory search permits student/scribe/zai-scribe/proctor, excluding Solver | This audit does not establish absence of every generic agent tool; it establishes the missing APM-controlled Solver channel |
| August 3 static analysis predicted uninvited pull tools would go unused | Student packets explicitly invite authenticated search; 218 cohort role-search receipts exist | Availability and search activity do not establish subsequent use |

Sources: `futon3c/holes/technotes/TN-APM-cascades-exist-unused.md:1`,
`futon3c/holes/technotes/TN-f37-zero-uptake-is-relevance.md:1`,
`futon3c/holes/technotes/TN-fable-F66-F70-review.md:1`,
`futon3c/holes/technotes/TN-solver-shelf-canary.md:1`, and
`futon3c/holes/labs/M-memory-retrieval/memory-system-static-analysis-20260803.md:1`.
These are historical observations with dated amendments, not all current defects.

## The learning path, boundary by boundary

**Write and independent review: exercised.** The snapshots contain 57 distinct
new memories attributed to cohort frames: 54 substitutive and three regulative.
All 57 retain materialization receipts whose content/persisted/read-back digests
agree, and all name a reviewer distinct from the depositor. Fresh endpoint reads
found all 57 memory records and all 57 review records, with matching IDs and
expected authors and nonempty bodies. This verifies present retrievability and
recorded read-back agreement; it does not recompute the entire admission proof
or certify the mathematical correctness of every memory.

**Retention and exposure: exercised.** Every student packet is archived.
Every cascade offer has a name and hook; full bodies are not embedded in the
offer maps. They are references for retrieval, not 19/20 complete lessons pasted
into the prompt. Shelf growth and preserved depositor provenance permit later
reads without relabelling old memories as newly learned. For the provenance
contract see `futon3c/holes/technotes/TN-apm-authority-seam-repair.md:48`.

**Active retrieval: exercised.** There are 218 authenticated search receipts:
95 student, 85 promotion-proctor, 20 scribe and 18 zai-scribe. Seventy-four return
zero result IDs (41 student, 29 proctor, three zai-scribe, one scribe). Zero
results are not necessarily defects; the query's relevance and available corpus
must be assessed. Receipt result IDs include patterns as well as memories.
The 85 queries in certified student use receipts are a different denominator
from the 95 durable student searches; failed/unaccepted work must not vanish
from a retrieval census. Search authentication, holdout enforcement and durable
receipt generation are at `futon3c/src/futon3c/apm/role_memory_search.clj:80`.

**Use: exercised, narrowly.** Thirteen of the 32 certified attempts report use,
for 22 accepted (attempt, memory) events. None names a used cascade offer.
Every accepted used ID is in that receipt's controller-derived surfaced set.
The independently rerun fingerprint classification for these 22 events is:

| Classification | Events | Interpretation |
|---|---:|---|
| Fingerprinted | 6 | Five within-frame; one prior-frame, same problem |
| Already in base | 7 | Does not witness novel contribution by the memory |
| Unwitnessed | 7 | Claimed use without the required artifact fingerprint |
| Regulative, not adjudicable by token | 2 | Needs a process-effect criterion, not a token-use verdict |

The only accepted cross-problem use is already-in-base. Thus **zero
fingerprinted cross-problem transfers in this cohort**. That is neither a claim
that transfer has never happened nor a reason to force citations to irrelevant
memories. Example of the positive boundary: f211 attempt 3 names its own guide
memory `e-apm-promotion-a2c13a611d0b62d8d7755fd1ccb27cef`; the artifact contains
novel interval-integral API identifiers named by that memory. f207 attempt 2
has the prior-frame same-problem fingerprint from f108. Neither is a clean
causal contrast, and f211's reported outcome is partial.

**Outcome-driven revision and next consumption: not established.** Existing
shared-memory lifecycle operations represent challenges, corrections and
supersession without deleting history
(`futon3c/src/futon3c/peripheral/memory_lifecycle.clj:1`). This census follows
creation, access and use; it does not demonstrate a low-use outcome revising a
memory, its attachment or its retrieval policy and then changing a subsequent
consumer. No such end-to-end learning claim follows merely from those APIs.
This is the next audit/experiment boundary, not an assertion that no external
consumer exists anywhere in the stack.

## Two current findings that need attention

**1. The cascade is reliable enough to deliver, but repeatedly delivers the same
small set.** Twenty-one requests have 20 offers, thirteen have 19, one has zero
with a failed outcome. All nonempty offers use the sibling route. There are only
20 distinct offered IDs over these requests, despite the changing problems and
large shelves. Recorded expansion latency is median **326.568 seconds**, maximum
534.952 seconds; summed per-attempt elapsed times are 197.1 minutes, not a claim
about nonoverlapping wall-clock delay or CPU usage. Recorded cascade use is zero.

The code explains a plausible mechanism: after route choice, candidates are
sorted by route key plus memory ID, then capped
(`futon3c/src/futon3c/apm/conductor.clj:670`). The 20 include integration,
measurability, polar-form and measure/volume topics. This supports investigating
selection before the cap; it does not prove those topics are irrelevant to every
problem. A controlled alternative must measure *relevance and useful next-use*,
not reward a larger offer set or a higher citation rate. Do not silently change
an already minted frame's supply.

**2. The fingerprint audit's extraction population is broader than accepted
uses.** Its regex scans every `:used-ids` block in the entire attempt file
(`futon3c/holes/labs/M-apm-demonstration/analysis/fingerprint_audit.py:492`).
That includes four reports in f191/a2, f192/a2 and f206/a2 whose states have no
certified receipt or archived source. The raw cohort count is therefore 26;
the structured certified-receipt count is 22. All four extras are labelled
no-source by the analyzer, not falsely fingerprinted, but they should be a
separate diagnostic population rather than silently entering a learning-use
rate. `summary.json` preserves their identities and both counts. No checker
was edited during this audit.

## What to carry into War Machine learning and Cascade Live

This is usable groundwork for **experience extraction and retrieval**, not an
AIF-conformance proof or evidence of a learned generative parameter update.
The corrected learning scope still needs each learned object's evidence,
update/objective, schedule, persistent version and next consumer. Memory content,
attachment structure, retrieval parameters and policy habits are different
learned objects; do not call their common storage format a common update rule.

| Requirement | Concrete memory-system lesson | Computational form to discuss |
|---|---|---|
| R-A records carry warrant | Keep origin, reviewed evidence, read-back identity and artifact-use evidence; separate claimed use from certified use | Institutional admission rule, implemented by executable checking procedures |
| R-B one queryable self-account | Join deposit → review → exposure/search → accepted use → outcome → revision; show missing links and separate denominators | Query/provenance contract; maintenance patterns; not merely a summary score |
| R-C feedback reaches every participant | Explicit search-capable roles exclude Solver; no evidence here of human/stakeholder acknowledgment or next use | Participation and feedback obligations with observable receipts; delivery is not understanding |
| R-D accountable next action including goal formation | A capped route/ID order is a real selection rule, but it is not evidence of task relevance or accountable goal choice | Patterns can propose; institutions authorize; a preference needs a defined outcome and observation model |

No new weights are inferred from these counts. The pattern/institution
separation of the apex excursion remains intact: the running search/review code
can enforce particular rules, while broader participant obligations still need
an explicit institutional account.

Recommended first bounded learning demonstration: choose a relevant successor
task before seeing its result; freeze a reviewed memory and its evidence; record
whether it is retrieved and what action it changes; evaluate the resulting
artifact or process with the correct kind-specific criterion; let an adverse
outcome create a reviewed correction; then observe the next consumer using
that corrected version. A disconnected-consumer control must fail. This is a
proposal, not an activation or a claim that a supplied lesson is Bayesian
parameter learning. It directly tests the missing feedback-to-next-use link.

For immediate follow-up, separate three small jobs: classify the analyzer's
accepted/diagnostic populations; assess task relevance before the cascade cap
under a declared experimental boundary; design the reviewed correction and
next-consumption record. Broader Solver exposure needs its existing experimental
authority and control, rather than an unnoticed role-permission change.

## Checks and limits

Fresh read-only census twice, byte-identical; fresh fingerprint run reproduced
the retained campaign result. In separate local processes, role-memory-search
8 tests/42 assertions, frame-fingerprint-audit 4/31, memory-access-gate 1/7 all
passed; Python fingerprint analyzer tests 18 passed. These test refusal and
observation boundaries, not live efficacy. Census clj-kondo 0 errors/0 warnings
and check-parens OK. No Lean build is needed for this note/census change.

An initial search-receipt census looked one directory too high and returned zero;
reading `receipt-path` corrected it to `receipts/` before these results were
recorded. One diagnostic REPL expression had an unmatched parenthesis and was
corrected. Neither failed probe is evidence against the memory implementation.
Concurrent edits to the coordinator and session-compaction code were left alone.
