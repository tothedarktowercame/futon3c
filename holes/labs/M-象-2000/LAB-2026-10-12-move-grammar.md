# Move grammar, first campaign: 158 multi-opener turns labelled by five Zai seats (claude-16, 2026-10-11 18:30Z)

Design: `moves/README.md`, schema `moves/moves.schema.md`. Pilot: zai-1
21d869db. Shards: zai-1 5a8fcc0f (52 records), zai-2 13775ef4 (35), zai-3
53778e52 (30, second attempt), zai-4 23757a93 (21), zai-5 babefddd (20).
Every shard revalidated by claude-16 after the harness fixes below: exit 0,
void 0 (1,440 valid acts).

## Population and cost

158 analysed operator turns with a marked reply and two or more opening
fragments, plus the labelled session, in 12 sessions. Zai monthly budget:
39.0% before the shards, 54.0% after (15 points for 158 records, ~0.095 per
record; the pilot cost 3 points for 20 records, most of it tooling). Seat
time 3 to 7.5 hours per shard, overlapping. Remaining budget at 18:17Z: 46%,
reset 2026-10-13 02:04Z.

## Totals

| measure | value |
|---|---:|
| acts labelled | 1,440 (743 operator, 697 reply) |
| answer incidences | 782 (425 closes, 357 keeps, 0 unclear) |
| reply paragraphs answering 0 / 1 / 2 / 3 / 4 openers | 116 / 405 / 154 / 19 / 3 |
| openers answered by two or more paragraphs | 192 of 493 |
| scoped acts | 662 (152 scope openers, 0 explicit closes) |
| distinct move labels | 413, of which 306 used once |

The incidence shape is the joint-coverage one codex-68 described: a quarter
of answering paragraphs bear on more than one opener, and two fifths of
answered openers are covered by more than one paragraph. One-to-one is the
common case, not the rule.

## Against the labelled rows (CHECKPOINT-2026-10-11-false-open-ports, 24 rows)

`score.py` over all shards: 22 rows matchable; 13 received an incidence; 9
predicted closed, 3 predicted open; precision answered-vs-open 0.75 (9 of 12
verdicts agree with claude-15's labels). The kernel today closes 4 of the 24.
Small sample; the single-opener batch (below) is the larger precision test.

## Vocabulary

Seeds dominate where they fit: `ask-action/one-decision` 65, `report/done`
76, `qualify/caveat` 83, `approve/plain` 54, `report-problem/observed` 56,
`explain/mechanism` 52. The invented tail is seat-dependent (shards 2 and 5:
58 and ~40 labels with few singletons; shards 1, 3, 4: 134 to 149 labels with
roughly 100 singletons each). By kind: report 360 acts over 63 labels; propose
240 over 127; explain 190 over 87; ask-action 116 over 11; approve 63 over 4.
Proposals and explanations are where the labellers could not agree on a
vocabulary; asks and approvals they could. **Consolidation is required before
counting the grammar:** classical first (within kind, fold a singleton into the
most frequent label of its kind when both quotes share content words), then a
closed vocabulary per kind, then relabel only where the fold is ambiguous.

## Scopes

152 scope openers, no explicit closes: scopes are opened and left. The
openers named by several seats independently (no-reinvention,
crystallise-on-write, no-second-reader, hook-it-all-up, FUTON-debugger,
M-象-2000-checkpoint) are Joe's constraints and plans of 10-10/10-11, which is
the standing/scope relation the kernel lacks (an opener's reach).

## Harness defects found by the labellers, fixed by claude-16 (this commit)

1. The validator took the record's stored `:acts` (120-character prefixes)
   as authority before the full `reply_acts` text, so quotes for
   kernel-recognised reply acts were confined to the prefix. Fixed: reply_acts
   first. All five shards revalidate unchanged.
2. The sampler built fragment ids from the plain turn id while stored acts
   for a reused turn id carry `@<record>`. Fixed: the act base is read from
   the stored acts. (Pilot: paragraphs were numbered by the labeller; fixed
   earlier by shipping `reply_acts` in the inputs.)
3. Heading-only reply paragraphs (content in unmarked continuation text) and
   sub-8-character fragments cannot be quoted; labelled low-confidence or left
   unlabelled with a reason. A kernel-side question: whether a continuation
   paragraph should belong to the marked act above it.

## Next

1. **Single-opener batch** (200 records after removing the 7 already labelled in the labelled session; 5 shards [68, 59, 25, 24, 24]; ~19 budget points): the
   precision test for the sole-port rule, by comparing each Zai incidence with
   the rule's inferred link on the same record.
2. **Consolidation** of labels, classical, into `moves/vocabulary.json`.
3. **Carrier**: incidences and scopes as hyperedges (`:xiang/answers` over
   [paragraph opener…], `:xiang/scope` over [opener governed…]) in futon1b,
   per Joe's remark of 2026-10-11; the sidecars are mining output.
4. The joint-coverage rule's test set is now 782 incidences over 158 turns.

## Authorization and its boundary (recorded at claude-15's request, 2026-10-11)

Joe's standing rule (2026-10-11, to claude-15): he does not want "all of the
agents' turns to be read by a second agent". This campaign reads agents' reply
paragraphs with Zai. It does so because Joe proposed it, verbatim (to claude-16,
2026-10-11): "we have 64% of Zai to spend in 34 hours or so. it occurs to me
that we could use that for mini g the grammar of moves made by both myself and
agents, beyond the core intents. this could allow future fast -- either
classical or Kimi backed -- scope based inference". It is a bounded mining
sample with an end (the Zai budget reset, 2026-10-13 02:04Z), over settled
records, to produce a grammar that a classical or cheap tier applies
afterwards. It is not permission for routine second-model reading of turns,
and the hyperedges loaded from it carry `status proposed` until the kernel
adopts an incidence by rule.

## Carrier shape (agreed with claude-15, 2026-10-11)

futon1b hyperedges as they are (`:hx/endpoints` is an ordered vector):
- `:xiang/answers` over `[answering-act, opener-act…]`, roles in `:hx/props
  :roles` (`answer`, `openers`); `:xiang/scope` over `[opener, governed…]`
  (`opener`, `governed`).
- id: hash of (kind, answering act id, sorted opener ids), so a later labelling
  of the same incidence re-puts the same edge; labeller run and label-set
  version live in props.
- valid time: the answering act's `reply_at` (the record's `created_at` when
  absent); said time: the load.
- props: effect, verbatim quote, consolidated label, labeller and model,
  validator exit, status `proposed` | `adopted`.
- producer: a 象 loader from the sidecars (claude-16's side), so the seam has
  one producer; the follower reads `:xiang/answers` beside the record's
  `:ports`. Loaded after consolidation, not before.

## Consolidation, first pass

The classical pass in `moves/consolidate.py` read 1,440 acts under 413
original labels. It normalised spelling and inflections, retained normalised
labels with at least three uses, and folded rarer labels to the most frequent
same-kind label with at least two aggregate content words in common. With no
such candidate it used the kind's `other` label. The result has 94 labels and
272 acts under `other`.

| kind | labels before | labels after | acts under `other` |
|---|---:|---:|---:|
| approve | 4 | 2 | 0 |
| ask-action | 11 | 6 | 5 |
| challenge | 2 | 1 | 3 |
| clarify | 19 | 7 | 12 |
| collect | 2 | 2 | 1 |
| constrain | 9 | 3 | 4 |
| continue | 1 | 1 | 2 |
| defer | 1 | 1 | 0 |
| delegate | 8 | 3 | 2 |
| disagree | 3 | 2 | 3 |
| explain | 87 | 11 | 62 |
| explore | 15 | 2 | 13 |
| extend | 1 | 1 | 1 |
| prioritize | 7 | 1 | 7 |
| propose | 127 | 9 | 99 |
| qualify | 31 | 6 | 24 |
| redirect | 3 | 1 | 3 |
| report | 63 | 23 | 20 |
| report-problem | 8 | 6 | 2 |
| retract | 4 | 2 | 3 |
| verify | 7 | 4 | 6 |
| **total** | **413** | **94** | **272** |

All largest individual folds have two source acts, so ties below are ordered
by source label. The words in parentheses are the overlap witness.

1. `approve/with-plan` → `approve/plain` (`agree`, `already`, `mining`,
   `turns`). Plausible, though broader than the source.
2. `ask-action/operator-step` → `ask-action/commission` (`futon7a`,
   `page`). **Looks wrong:** project/page names are incidental rather than a
   shared move.
3. `challenge/necessity` → `challenge/other` (no eligible overlap).
4. `constrain/accept` → `constrain/stand-rule` (`bugs`, `claude-4`,
   `lean-to-python`). **Looks wrong:** identifiers and task nouns dominate.
5. `constrain/retry-better` → `constrain/for-now` (`fail`, `loop`,
   `retry`, `same`). **Looks doubtful:** topical overlap does not establish
   the same constraint.
6. `continue/waiting-on-decision` → `continue/other` (no eligible overlap).
7. `disagree/assessment` → `disagree/other` (no eligible overlap).
8. `explain/decline` → `explain/mechanism` (`after`, `rather`). **Looks
   wrong:** the witness words are generic discourse terms.
9. `explain/distinction` → `explain/mechanism` (`different`, `structure`).
   Plausible but coarse.
10. `explain/feasibility` → `explain/motive` (`agent`, `turns`). **Looks
    wrong:** the overlap names participants and units, not explanatory role.

These are recorded rather than corrected by hand. `moves/label-map.json`
keeps every original-to-consolidated decision and its reason, so a later
closed-vocabulary or ambiguity pass can revise them reproducibly.
