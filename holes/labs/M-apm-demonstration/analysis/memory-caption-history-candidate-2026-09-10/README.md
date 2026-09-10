# Historical memory caption candidates: first bounded slice

Status: candidate artifacts only. Nothing in this directory is published to the
memory store or search index. Publication waits for the independently reviewed
caption-admission apparatus owned by Job A.

## What is pinned

`historical-census.edn` pins all four terminal f211 role snapshots. They contain
359, 360, 361, and 364 retained memories. Their union has 364 distinct memory
IDs, their intersection has 359, and five entries are role-dependent. Both sets
are retained explicitly rather than selecting one role's view as the corpus.

This is the observed `jit-all-open-v3` retained population at f211, not a claim
about every memory ever written or every memory currently reachable through a
live service. The historical work remains open for older retained memories.

The first authoring slice is the independently audited f190--f213 cohort from
`memory-audit-2026-09-10`: 57 distinct promoted memories across 13 frames. The
57 are all members of the pinned union. Frames with no promoted memory remain
represented in the upstream census; they do not create synthetic caption rows.

## Candidate record semantics

`cohort-57.edn` contains one append-only hook-bootstrap proposal per audited
memory, accompanied by proposed `:apm-memory-applicability-v1` records. Each row
binds:

- the immutable memory ID and original content digest;
- a positive `Useful when ...` caption derived from the original reviewed hook;
- source problem, frame, and retained source-attempt evidence IDs;
- the original independent review evidence ID and reviewer;
- a source-derivation applicability observation with unresolved use status; and
- every accepted later-use event in the frozen fingerprint audit that names the
  cohort memory.

These bootstrap field names are a lab interchange proposal, not an admission
claim. The implemented Job A store uses `:grounded-caption-v1`, requires a
nonempty condition vector and authenticated observation IDs, and currently has
no historical-import path. `source-read-enrichments.edn` therefore presents the
three enriched drafts in the store's six descriptive fields without forging
runtime IDs or claiming that admission succeeds. The caption is `:supported`
only as a description of the original reviewed memory. It does not say the
memory closed its whole source problem.

The 54 bootstrap-only rows have an empty condition vector with the disposition
`:none-authored-no-absence-inferred`; this records that this slice did not add
condition claims and does not assert that prerequisites are absent. Three
source-read enrichments—including one regulative memory—carry individually
supported or unchecked conditions, scope limits, and byte-verified source
artifacts. Later-use
observations preserve the audit's limits. `:fingerprinted` becomes a
`:supported` observation because rare memory tokens occur in retained attempt
source. `:already-in-base`, `:unwitnessed`, `:no-source`, and
`:not-adjudicable-by-token` remain `:unknown`, with the original verdict and an
explanation. An accepted-use record is therefore not silently upgraded to a
causal success claim. Memories with no accepted later-use event in that audit
have the explicit disposition `:no-audited-accepted-use-record`; this means no
event in the bounded audit, not “never useful.”

The earlier isolated caption experiment for
`e-apm-promotion-5fdb99169bd788313841375c797c302c` remains separate. Its m96J04
reference proof is a suggested analyst-validated application, not historical
student use, and is not folded into these frozen campaign-use counts.

## Coverage and evidence

`coverage.edn` is produced by `build.clj` and currently records:

- 57/57 accounted with hook-bootstrap drafts;
- three source-read enriched drafts pending new caption review;
- 57/57 with retained source-attempt links;
- 57/57 with independent review IDs and reviewers;
- 14 with one or more accepted-use records in the fingerprint audit;
- 43 with the explicit no-audited-use disposition; and
- zero snapshot identity or original self-review conflicts detected.

Conflict reporting is computed, not literal: repeated snapshot records are
compared across identity, content, provenance, and review fields. Planted body
mismatch and self-review controls must each be detected before generation can
succeed. This checks the inherited memory review only; every new caption still
has `:caption-independent-review :pending`.

The upstream evidence authorities are:

- commit `330315fe`: cohort census, snapshot/read-back digest checks, attempt
  sources, and accepted-use fingerprint audit;
- commit `74622e23`: the separate cross-problem reference proof and retrieval
  probe; and
- commit `cc62da46`: the isolated caption vocabulary experiment.

The generator checks exact 57-member identity against `readback.json`, unique
IDs, membership in the historical union, retained source-attempt references,
independent-review fields, and positive caption wording. Generated data is
deterministic from committed snapshots and audit artifacts. It also checks that
the proposed searchable text (useful-when plus scope limit) is at most 720
UTF-8 bytes and refuses oversize records rather than truncating them.
Source-attempt identifiers are retained as identifiers with digest status
`:unknown`; substrings of IDs and filenames are no longer mislabeled as verified
content digests. The three enrichment inputs have SHA-256 values recomputed from
their actual bytes on every build.

## Resume boundary

The next historical slice begins from the 364-ID pinned union minus these 57
IDs, with role-dependent membership kept visible. It must repeat original-body,
source, use/attempt, and independent-review checks; it must not infer captions
from names alone. Any schema translation required by Job A belongs in a later,
reviewed adapter commit. These candidate keys are an interchange proposal and
do not authorize registry, worklist, campaign, or historical-record edits.

## Rebuild and validate

From the `futon3c` checkout:

```sh
clojure -M holes/labs/M-apm-demonstration/analysis/memory-caption-history-candidate-2026-09-10/build.clj
clj-kondo --lint holes/labs/M-apm-demonstration/analysis/memory-caption-history-candidate-2026-09-10/build.clj
emacs -Q --batch -l /home/joe/code/futon4/dev/check-parens.el \
  --eval '(arxana-check-parens-cli)' -- \
  holes/labs/M-apm-demonstration/analysis/memory-caption-history-candidate-2026-09-10/build.clj
```
