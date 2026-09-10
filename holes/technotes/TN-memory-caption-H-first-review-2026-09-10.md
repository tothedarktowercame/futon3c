# Independent review of Job H's first caption slice

2026-09-10, Codex-17 reviewing Codex-1 commits dc0c4a25 and a808cb45.
Verdict: reproducible draft inventory accepted; caption enrichment and admission
NOT accepted. No publication authorized by this review.

## Reproduction

Ran the committed build.clj in its own Clojure process twice. Both exited 0;
all three generated EDN files were byte-identical to their starting bytes.
Reported coverage: 57 accounted, 57 draft captions, 14 with audit-use rows,
43 without audit-use rows. These counts describe the bounded population.

SHA256:
- cohort-57.edn: 214362501e5e952ab8875b08ea6206b38af678ac6630b955df2728e0c0f19c60
- historical-census.edn: 7626e05c00e05715dbf204f44fa4e8e9119cb158adffe4c1935ee66c4bb3a048
- coverage.edn: cf54dd7b5accdb3a8c5118cd2d9c03d02761da626712930e6d9ae2804a45d76a

The following findings come from reading the generator, not a re-run of the
underlying historical proofs or an independent review of all 57 memories.
Pointers below are relative to
holes/labs/M-apm-demonstration/analysis/memory-caption-history-candidate-2026-09-10/.

## Required revisions

1. build.clj:46–50 only prefixes and trims the existing hook. This is a useful
   bootstrap, but does not perform Joe's requested reading of the original
   problem and solution to add descriptive retrieval vocabulary. All condition
   and suggested-context vectors remain empty. Label this coverage as hook
   bootstrap; complete a small source-read enrichment slice before calling
   historical caption authoring accomplished. Empty conditions are honest, but
   cannot substitute for investigating conditions.
2. build.clj:202 assigns :conflicts 0 literally. The snapshot reduction at
   :32–35 overwrites repeated IDs without comparing their records, and the
   review status checks presence of review fields rather than author/reviewer
   inequality or identity agreement. Therefore 'no conflicts detected' is not
   supported by a detecting gate here. Compare immutable identity/content/review
   fields across snapshots, retain role-dependent membership separately, and
   add planted mismatch and self-review controls. Distinguish inherited original
   memory review from a NEW review of the caption.
3. build.clj:55 obtains :source-digest by stripping an ID prefix; :98 derives
   another digest by splitting a source pathname at a hyphen. Neither operation
   checks source bytes or documents which content the result hashes. Resolve
   the artifact and verify the appropriate digest, or retain a typed evidence
   identifier with digest explicitly unknown. Do not call arbitrary ID/path
   substrings verified digests. Retain the existing provenance references.
4. The historical-source observation at :137–143 has disposition :used because
   source attempts produced the memory. Derivation from an attempt does not
   show that the subsequently deposited memory was used in that attempt. The
   scope-limit disclaimer is useful but cannot correct the typed disposition.
   Use an explicitly source-derivation variant agreed with Job A, or a truthful
   unresolved disposition if that is the supported schema. Keep historical
   later-use observations separate.

The proposed Job A document also spells the observation schema
:apm-memory-applicability-v1; this generator uses
:apm-memory-applicability-observation-v1. Resolve against the implementation's
actual admission schema when available, and test admission rather than claiming
compatibility from similar field names.

## Bounded next packet

First repair the bootstrap's validation and reporting with detecting controls.
Then enrich three memories from actual problem/source/body reads, including
one regulative case, with checked source references and explicitly qualified
conditions. Preserve all 57 inventory rows and the limited population claim.
Do not inspect sealed holdout queries. No live publication, historical-body
changes, registry edits or Claude invocations. Codex-17 independently reviews.

This review is note-only. Regeneration validates repeatability, not caption
usefulness, historical causal benefit, or improved retrieval. Those remain
separate acceptance obligations.
