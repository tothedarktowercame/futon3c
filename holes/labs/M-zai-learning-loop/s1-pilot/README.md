# S1 scribe pilot

This directory contains operator-review drafts mined from zai-1's APM Lean
sessions. Nothing here has been written to the memory store.

## Corpus and method

The pilot used only `apm-lean/problems/a95*/` and `a96*/` artifacts and their
git history. For each selected problem, the scribe compared the committed
`lean/Main.lean`, `status.json`, and `proof-outline.md` with the corresponding
`turn-round` evidence returned by the futon evidence endpoint. BPM problems
were deliberately excluded as held-out evaluation material.

The 12 drafts are balanced across the three evidence lanes in the mission:

- Solve lane: three strategy drafts and three Mathlib lemma-location drafts.
- Arc lane: three tactic drafts whose before/after forms are checked against
  the final `a95A04` artifact, not merely copied from self-talk.
- Trajectory lane: three process-and-cost drafts based primarily on partial
  runs and repeated dependency frontiers.

Counts by requested memory level are:

| Level | Drafts |
| --- | ---: |
| strategy | 3 |
| tactic | 3 |
| lemma-location | 3 |
| process | 3 |

Every draft cites an evidence-store `:evidence/id`, a git commit, and one or
more APM problem IDs. Confidence is deliberately conservative: only patterns
observed in more than one problem are marked `:n-instances`.

## Corpus gaps and cautions

- The mission text describes two complete and roughly fourteen partial runs,
  but the current `apm-lean` history contains additional later a95/a96 partial
  commits. This pilot sampled the clearest traceable arcs rather than claiming
  exhaustive coverage of the moving corpus.
- `a95A07` is a zero-sorry proof, but it proves the Basel value from
  Mathlib's existing zeta evaluation rather than by the requested contour
  method. Its lemma-location draft preserves that boundary.
- Turn `:round` values are not reliable elapsed-cost measurements across
  problems: they occur within a long shared zai session and may include
  orchestration turns. Process drafts therefore report observed terminal
  round values as cost signals, not as exact “rounds burned.”
- Several final summaries report that an API is absent (Young's inequality,
  Schwarz equality, Hadamard gap theorem). These are scoped observations from
  the checked environment, not timeless claims about every Mathlib version.
- The evidence endpoint intermittently returned `:expensive-read-busy`.
  Queries were retried after five seconds and narrowed by time window; cited
  IDs came only from successful reads.
