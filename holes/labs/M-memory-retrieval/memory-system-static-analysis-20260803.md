# The memory system, reconstructed from code (2026-08-03)

Written without database access, from source only. Every claim below cites a
file and is checkable by reading it. Where the code cannot tell us something,
that is said explicitly rather than guessed.

Sources: `futon2/src/futon2/aif/memory_contract.clj` (the cross-domain
contract), `futon3c/src/futon3c/peripheral/memory_{write,lifecycle,recall,backend}.clj`,
`futon3c/src/futon3c/dispatch_with_recall.clj`, `futon3c/src/futon3c/agents/zai_api.clj`,
`futon3c/scripts/memory_outcome_sweeper.py`.

---

## 0. Two channels, not one

The code implements **two independent ways** a memory can reach a runner:

| channel | mechanism | who initiates |
|---|---|---|
| **push** | recall at dispatch renders memories into the prompt | ground control, before the runner starts |
| **pull** | `memory_search` / `pattern_memory` / `evidence_graph` / `psr_search` tool calls | the runner, mid-session |

The pull channel is real: `zai_api.clj` defines `memory_record`, `memory_search`,
`pattern_memory`, `evidence_graph`, `tool_history` plus `psr_search` /
`psr_record` / `pur_record`, and `specs-for-mode` can strip that family per
`:memory-mode` (`:full` / `:files` / `:none` — the M-custom-harness §8.4
conditions). The Agency invoke path (`transport/http.clj:2490`) never passes
`:memory-mode`, so every zai invoke gets `:full` and therefore **has the pull
tools available**.

Nothing in the dispatch packets invites the runner to pull. The cohort-2 packet
says only "If memories are listed above…" — pure push. So a static reading
predicts: pull tools present, pull tools unused. That is a code-level
prediction, independent of what the store contains.

## 1. Creation — `memory_write.clj`

`record-memory!` writes an evidence entry plus a `memory/assert` hyperedge.
Payload validation requires name / body / subjects. The author is whoever
records; a runner's own realizations are therefore **self-asserted** by
construction, which is why the design puts a separate scribe seat in the loop.

## 2. Attachment and review — `memory_lifecycle.clj`

`review-attachment!` is where the separation of powers is *enforced in code*,
not merely documented:

- **author ≠ reviewer** — throws `"memory author cannot review their own attachment"`
  (line 133).
- The review evidence must name the exact memory as its subject, carry
  `:review/event :memory-attachment-review`, match the verdict, and match the
  attachment's pattern set exactly (`exact-patterns?`).
- Verdict ∈ `{:approve :challenge :reject}` → attachment-status ∈
  `{:reviewed :challenged :rejected}`.
- An approval **must** state `:review/witness-status` ∈
  `{:self-asserted :independently-witnessed}`.
- `:memory-use/kind` ∈ `{:substitutive :regulative}` is validated here and
  **projected only after approval**; absence is retained as unknown. The
  docstring is explicit that it is never "guessed from the lane or memory prose".
- Success has a **cache postcondition**: after posting, a fresh endpoint read
  must observe the new review version, or the result comes back
  `:stale-after-successful-repost`. A write that succeeded but is not yet
  visible is reported as such rather than as success.

## 3. Recall at dispatch — `dispatch_with_recall.clj`

This is the push channel, and it is where retrieval quality is decided.

Term selection (`text-keywords`, line 221):

1. tokenize `[A-Za-z][A-Za-z0-9_/-]{3,}` over lower-cased text,
2. drop a hand-maintained stopword set (line 65),
3. rank by **raw frequency**, descending,
4. take the limit.

Query assembly (line ~366): `terrain` ++ interleaved subjects and source terms,
distinct, **`(take 4)`**, joined by spaces.

So the retrieval key is *a bag of at most four frequency-ranked words*. No
mathematical structure enters it. This is a structural ceiling, not a tuning
issue: a problem about summability of `1/(n+1)` yields the query
`"sequence convergence infinity only"` — three generic analysis words and one
survivor of the stopword list.

The code carries its own failure history: `normalize-math-text` strips `\command`
sequences with the comment *"so cdot/langle/rangle etc never become query
tokens"* — that is cohort-2's S4 finding ("drowned by TeX fragments … pollution
not absence") fixed in place.

The dispatcher writes the **offered half** of the receipt: surfaced ids, unused
ids, inclusion reasons, `recall-ladder-rung`, and a `recall-query` block
recording term-sources, the chosen terms, and the receipt-ranking factor.

## 4. Use and attribution — `memory_contract.clj/use-receipt`

The contract validates, and refuses, rather than accepting whatever arrives:

- used ⊆ surfaced (`"used memories must have been surfaced"`),
- every surfaced memory needs a nonblank inclusion reason,
- rejected is disjoint from used, also ⊆ surfaced, and needs reasons,
- whatever remains is explicitly classified **unused** — silence is not allowed
  to mean anything,
- `:memory-use-kinds` may classify only surfaced memories, with values from the
  closed vocabulary,
- `outcome-id` is a *reference*; the independently witnessed outcome stays a
  separate record. The receipt cannot launder its own success.

Runner-side, the dispatch packet requires exactly one `USED <id>: <mechanism>`
or `IGNORED <id>: <reason>` line per surfaced id. That is the human-legible
half of the same contract.

## 5. Outcome sweeping — `memory_outcome_sweeper.py`

A1 ships as an **offline cron-side sweeper**, not a serving-JVM hook: the same
append-only path does historical backfill and ongoing outcomes, idempotent on
deterministic evidence ids.

## 6. Lifecycle transitions — `memory_lifecycle.clj`

States: `{:current :challenged :retracted :superseded}`.

- `challenge-memory!` — appends a durable challenge episode, then projects the
  edge `:challenged` with `:witness-status :challenged` and a
  `correction-lag-ms`. The docstring is pointed: it "neither deletes the episode
  nor converts the failed use into success training".
- `supersede-memory!` — records the correcting memory, projects it `:current`,
  marks the original `:superseded` with `:superseded-by`. Reviewed status is
  **inherited only if** the correcting edge points at exactly the same reviewed
  pattern endpoints — a correction cannot silently acquire review it never had.
- `retract-memory!` — ends the assert edge at `valid-from`.

Nothing is deleted. The header states the model: challenges and corrections are
durable evidence; the `memory/assert` edge is a *bitemporal projection* over
them.

---

## What the static reading establishes

1. **The integrity machinery is real and enforced in code** — author ≠ reviewer,
   used ⊆ surfaced, unknown-kind preserved as unknown, no deletion, witness
   status required on approval, cache visibility as a postcondition. These are
   not conventions that a sloppy run could bypass.
2. **Retrieval is the weak joint.** A ≤4-term frequency bag decides what a
   runner ever sees. Every logged cohort-2 recall failure (`recall-empty`,
   "pollution not absence", own-terrain miss) is consistent with this one
   mechanism, and so is today's a96J01 offer.
3. **The pull channel is provisioned but never invited.** Tools are present on
   every zai invoke; no packet asks for them. That is a one-line packet
   experiment, not a build.
4. **Kind classification is deliberately sparse.** Because `:memory-use/kind` is
   only set at approval and never inferred, any analysis that reports
   substitutive-vs-regulative rates is reporting on *reviewed* memories only.

## What code alone cannot settle

- Whether memories were *actually* surfaced, used, or pulled in any past run —
  that is store content, and the store is what is currently in question.
- Whether the offered-half receipt for a given dispatch was persisted.
- The pre-pause (07-25/27) tool-call question.

These need the database, or a replay of it. Everything above does not.

---

# Validation against the store (same day, after the database was unstuck)

The static model above was written without database access. Every checkable
claim was then tested against the store. Query note: the evidence endpoint's
`offset` and `tag` parameters are **silently ignored** (identical bytes across
offsets; `tag=` counts return the whole store), while `type`, `claim-type`,
`author`, `session-id` and `since` filter correctly. An earlier inference of
mine — that today's traffic was burying July entries — was an artefact of the
ignored `offset`, not a fact about the store.

## Confirmed

| claim | evidence |
|---|---|
| the offered half persists at dispatch | today's a96J01 dispatch wrote exactly one `memory-offered` receipt, with `recall-query` and `recall-ladder-rung` |
| review machinery is live | 305 `:memory-attachment-review` events over 166 distinct memories |
| approvals carry a witness status | 237 `:independently-witnessed`, 67 `:self-asserted` |
| supersession is used, deletion is not | 5 `:memory-challenge` episodes, all `:memory/action :superseded`; no retractions |
| `superseded-by` is a projection, not an episode | absent from evidence entries, lives on the hyperedge — as the code implies |
| **the pull channel is real and was exercised** | see below |

## The pull channel: positive and negative controls

Session `zai-1050483251df4a6ebce46b4b8a4130af` (zai-3, 2026-07-22/23, 913
entries) called:

    run_shell 170 · edit_file 62 · read_file 38 · write_file 19
    memory_record 12 · psr_search 3 · memory_search 1 · par_punctuate 1

Today's cohort-2 S3 session (zai-1, a96J01) called:

    run_shell · read_file · list_files · edit_file · write_file · search · run_readonly
    — zero memory-family calls

Same tool availability (`:memory-mode :full` on both, since the invoke path
never passes one). Different behaviour. The difference is in the **packet**:
the 07-22 era had an explicit recording contract — one of zai-3's own memories
is literally named `memory-record-p0-contract` — whereas the cohort-2 packet
only says "If memories are listed above … name used/ignored in your final
summary". It never asks the runner to record or to search.

This confirms the static prediction: *tools provisioned, never invited*. It is
a packet-level variable, not a wiring defect, and therefore a one-line
experiment rather than a build.

## Refuted / revised

- **`:memory-use/kind` has zero occurrences** across 305 approvals. The field
  ships (B4, 2026-08-01) but predates almost none of this corpus, so the
  substitutive/regulative split currently has no data behind it. Any analysis
  reporting those rates is reporting on an empty set.
- **The code's guarantees hold only for writes that go through the code.**
  `e-review-math-pudding-v2-tendsto-in-measure-ae-subsequence-api` (author:
  joe, 2026-07-26) carries `:review/witness-status :in-use-evaluation-delegated`
  with `:review/provenance {:kind :operator-delegated-in-use-evaluation …}`.
  That value is outside the closed vocabulary and `validate-review!` would
  refuse it, so the entry was written directly to the store rather than through
  `review-attachment!`. Deliberate operator action, but it means "enforced in
  code" must be read as "enforced on the code path".

## Corpus shape

522 `type=memory` entries: 212 `:assert`, 305 `:observation`, 5 `:challenge`.
Authors: claude-9 200, claude-6 136, codex-5 55, joe 55, codex-2 40, codex-4 11,
zai-3 8, runner-gate 7, claude-4 5. Dates 2026-07-22 … 2026-08-02, peaking
07-30. The zai lane contributes 8 of 522 entries (4 distinct memories, recorded
twice each).


---

## Correction (2026-08-03)

This document originally named a zai tool **`library_search`**. **No such tool
exists** — `grep '"library_search"' src/futon3c/agents/zai_api.clj` returns
zero. The tool that searches the pattern library and "returns scored candidates
plus bounded hooks for reviewed attached memories" is **`psr_search`**
(`zai_api.clj:150`); I attributed its description to a name I invented.

The error propagated into the V3 arm design and into the spec codex-6 built
from, so the shipped `:push+pull` / `:pull-only` invitation named a tool that
does not exist. Corrected in `dispatch_with_recall.clj` with the invitation
version bumped `v1` → `v2`; no dispatch had used v1, so no receipt is affected.
Recorded here rather than silently fixed, because the wrong name reached
experimental material — a runner told to call a nonexistent tool could
reasonably conclude the memory tools are broken, which is precisely the
behaviour the pull arm is trying to measure.
