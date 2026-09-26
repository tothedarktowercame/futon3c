# Codex-16 handoff: pattern-first APM learning and retrieval

Joe commissioned this exploration on 2026-09-10, via codex-17. This is a
Codex-only handoff for working with Joe; do not dispatch to Claude agents.

## Joe's question and intended direction

Joe proposes treating mathematical memories as specific examples/implementations
indexed through more general patterns. His words: “It's not just a memory-first
solution, but a pattern-first solution”; “the memories are like leaf nodes that
are indexed by patterns.” These are a proposed organization to investigate, not
a claim that the current store already implements that graph.

He wants to inspect the patterns mined during prelim problem solving, including
relationships among concepts and the conditions under which they apply. For
example, continuity changes which mathematical techniques are applicable;
definitions and prerequisite concepts must be available and understandable.
He expects a useful layer between a few very broad strategies and isolated
facts. Investigate whether directing Zai Students through that layer would
improve cross-problem reuse. Existing captions remain complementary work.

Do not adopt “almost certainly” as an experimental finding. Zero observed
transfer in a bounded audit neither proves transfer impossible nor proves a
pattern-first intervention would succeed. Determine what exists and make a
bounded, executable proposal Joe can assess.

## Read first: retained evidence and implementation

All paths below are relative to `/home/joe/code/futon3c` unless repo-qualified.

- `holes/technotes/TN-APM-memory-current-audit-2026-09-10.md`: f190–f213
  bounded audit, 57 new memories, same-problem later use but no detected
  cross-problem transfer in that window. Offered, retrieved, read, cited and
  used are different observations. Fingerprints are not causal attribution.
- `holes/technotes/TN-APM-memory-first-transfer-probe-2026-09-10.md` and
  `holes/labs/M-apm-demonstration/analysis/memory-first-probe-2026-09-10/`:
  analyst-selected m00A05→m96J04 demonstration; development evidence, not
  autonomous discovery, a prevalence estimate or a fresh-worker control.
- `holes/technotes/TN-APM-memory-descriptive-captions-2026-09-10.md` and
  `holes/technotes/TN-memory-caption-jobA-consumer-trace-2026-09-10.md`.
  Read current code too: the caption apparatus was subsequently implemented
  by codex-12 (cde65520 through d35acfcf), with current admissibility and
  independent-review checks. These commits are implementation history, not
  evidence of live activation or complete independent acceptance.
- `holes/labs/M-apm-demonstration/analysis/memory-caption-history-candidate-2026-09-10/`:
  57-row hook bootstrap, three source-read enrichments; do not call all 57
  deep-read captions. Repairs 13f9824e / 3629db7d. Full population pin
  8d270113 covers 364 retained APM-relevant memories; 57 is a subset.
- Discover the actual Student prompt, pattern offers, retrieval routes,
  original-memory joins and evidence receipts in current source. Do not infer
  access/use from prompt prose or an available endpoint.
- Start library discovery at `futon3/library/`, including
  `math-strategy/`, `math-formalization/`, `math-formalization-CA/`,
  `math-formalization-FA/`, and `proof-search/`. These are observed locations,
  not an exhaustive prelim library census. Locate other relevant stores,
  promotion records, drafts and historical versions before naming the corpus.

## First bounded product: evidence-backed TN and small retrieval probe

Commit a new TN and reproducible, sanitized local census/probe artifacts.
No production code changes or live campaign changes in this first packet.

1. **Census the actual pattern population.** Pin sources and distinguish
   authored/promoted patterns, drafts, memories, definitions and implementations.
   For each relevant family, show representative real IDs and IF/HOWEVER/THEN
   spans, prerequisites, known examples and available retrieval fields. Is the
   proposed middle layer present, thin, fragmented, or merely not being offered?
   Do not invent pattern→memory relations from prose tokens or ID resemblance.
2. **Trace Student consumption.** Where can a Zai Student discover a pattern,
   read it, check its applicability, retrieve its examples/implementations, and
   actually use it? Trace exact producer→packet/endpoint→receipt→next consumer.
   Distinguish missing content, missing indexing, poor ranking, inaccessible
   tools, unchecked prerequisites and unobserved use. Include costs/caps and
   repeated-offer behavior where evidenced.
3. **Two concrete cross-problem candidates.** From committed patterns and
   retained problems, identify at least two plausible pairs, one with a
   contrasting/missing prerequisite. Read the problem statements and relevant
   proof evidence. Explain why the shared pattern applies or does not apply;
   distinguish established, absent and unchecked conditions. Suggested transfer
   remains suggested until executed. Do not spend the packet proving whole problems.
4. **Run a bounded offline discovery probe.** Freeze a small corpus and queries
   derived from those task statements; compare current available retrieval with
   pattern-first retrieval over exactly those inputs. Use existing search where
   possible; a local exploratory index must name its scoring and status. Log
   candidates, ranking, source pointers and applicability checks, including a
   negative case. This is development evidence, not held-out evaluation or a
   causal proof. If no executable retrieval path exists, report the precise
   missing port and provide a reproducible census instead of invented hits.
5. **Propose the smallest Student adjustment.** Pattern retrieval → contextual
   prerequisite check → resolve examples/implementation memories → proof attempt
   → applicability/use observation. Name which parts already run, which require
   code, and how definitions would be supplied without treating them as patterns
   or validated applications by fiat. Propose controls separating retrieval,
   exposure and observed use; comparison must freeze task/corpus/model/budget.
   Present prompt-only changes separately from structured endpoint/receipt work.

The report should answer Joe plainly: what useful patterns are already there,
why they have or have not reached Students, and what single implementation slice
would let us observe pattern-first cross-problem use. Negative findings count.

## Boundaries and coordination

- **Never open sealed holdout material**, including
  `/home/joe/apm-caption-holdout-2026-09-10.edn`. Do not search its contents,
  copy query/answer keys, or ask caption authors for them. Use a separately
  labelled development probe. Prior tuned ODE example is not a holdout.
- Historical memory bodies/reviews remain immutable. No caption publication,
  promotion, new authored graph edges, live campaign pause/resume, Student
  dispatch or JVM reload. Ask Joe in this session if he wants a later live
  experiment; first deliver a concrete reviewable proposal.
- Do not touch RUN4 runner/auth/admission work: codex-10 owns that chain.
  APM exploration and the War Machine trial remain distinct evidence claims.
- Follow workspace/repo AGENTS. Use Agency bells, not internal subagents.
  No Claude calls. Further Zai/Codex dispatch only when needed and with clear
  independent author/reviewer roles; do not duplicate caption authoring.
- Source read pins, reproducible commands, pointer checks, diff check; lint and
  paren checks if adding Clojure. Tests use isolated processes/stores. APM Lean
  workspace lifecycle rules apply if Lean is needed; do not invoke it merely to
  inspect sources. Never emit credentials from archived Student packets.

Return the commit SHA, census scope, actual probe output, three main findings,
and the proposed first Student implementation slice. Joe may then continue the
exploration directly with codex-16.
