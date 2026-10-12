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

## Consolidation, second pass

The second pass removes identifiers, paths, seat names, Futon repository
names, 象 pattern names, and generic move-discourse words from quote witnesses.
It also tries label-component stems before quote overlap. This preserves the
413 → 94 label reduction, but moves 17 acts out of `other` (272 → 255) on
explicit label evidence. The cleaned quote evidence also moves some first-pass
folds *into* `other`; the net is therefore the useful comparison.

| kind | acts | first-pass `other` | second-pass `other` | second-pass share |
|---|---:|---:|---:|---:|
| approve | 63 | 0 | 0 | 0.0% |
| ask-action | 116 | 5 | 7 | 6.0% |
| challenge | 3 | 3 | 3 | 100.0% |
| clarify | 72 | 12 | 9 | 12.5% |
| collect | 4 | 1 | 1 | 25.0% |
| constrain | 45 | 4 | 4 | 8.9% |
| continue | 2 | 2 | 2 | 100.0% |
| defer | 14 | 0 | 0 | 0.0% |
| delegate | 35 | 2 | 2 | 5.7% |
| disagree | 7 | 3 | 3 | 42.9% |
| explain | 190 | 62 | 58 | 30.5% |
| explore | 18 | 13 | 12 | 66.7% |
| extend | 1 | 1 | 1 | 100.0% |
| prioritize | 7 | 7 | 7 | 100.0% |
| propose | 240 | 99 | 93 | 38.8% |
| qualify | 125 | 24 | 20 | 16.0% |
| redirect | 3 | 3 | 3 | 100.0% |
| report | 360 | 20 | 21 | 5.8% |
| report-problem | 81 | 2 | 1 | 1.2% |
| retract | 10 | 3 | 3 | 30.0% |
| verify | 44 | 6 | 5 | 11.4% |
| **total** | **1,440** | **272** | **255** | **17.7%** |

The two largest residuals remain `propose/other` (93/240, 38.8%) and
`explain/other` (58/190, 30.5%). Their fifteen most frequent source labels
follow; ties are lexical and each quote is the first corpus occurrence.

### `propose/other`

| count | original label | example quote |
|---:|---|---|
| 2 | `propose/comparison` | “we can potentially analyse the turns both ways” |
| 2 | `propose/infer-from-behaviour` | “But we can infer,” |
| 2 | `propose/row-loop` | “i wonder if we could get agents to work tgrough a whole row at a time” |
| 2 | `propose/speedup-item` | “**Change the key**” |
| 1 | `propose/aif-integration` | “since there is a phase-by-phase alignment I wonder if we can start” |
| 1 | `propose/aligned-ledgers` | “using our ‘aligned ledgers’ between PBASE and R-numbers and intents” |
| 1 | `propose/applied-idea` | “one applied idea where the small-causal diagram idea could be *practical*.” |
| 1 | `propose/bidding` | “Then it will bid for patterns from the 150.” |
| 1 | `propose/build-cascade` | “we now have all the formal pieces that we would need to develop a convincing cascade” |
| 1 | `propose/cheat-sheet` | “we might want something a little bit like a chord ‘cheat sheet’” |
| 1 | `propose/checklist` | “(before the notebook goes public)” |
| 1 | `propose/checkpoint` | “make a Checkpoint in the M-象-2000 mission document” |
| 1 | `propose/collate` | “Then we collate the bids.” |
| 1 | `propose/combine` | “we need to do is carefully bring these ingredients together” |
| 1 | `propose/complexify` | “If these requirements are not in the outer loop cascade yet, we could complexify it.” |

Candidate additions (not applied):

- `propose/analysis-method`: comparison and inference-from-behaviour.
- `propose/integration`: AIF integration, aligned ledgers, combination, and
  cascade assembly.
- `propose/workflow`: row loops and speed-up/process changes.
- `propose/artifact`: cheat sheets, checklists, and checkpoints.
- `propose/coordination-mechanism`: bidding and collating distributed bids.

### `explain/other`

| count | original label | example quote |
|---:|---|---|
| 2 | `explain/parse-vs-formation` | “the tree presents a *parse* of the proof,” |
| 2 | `explain/reference` | “the ‘I/me’ paragraphs give a ‘point of view’ character” |
| 1 | `explain/accept-retraction` | “Noted. I'll stay off the War Machine until you say otherwise” |
| 1 | `explain/accumulator` | “So, a memory system in this setting would be searched with an accumulator that accumulates a heuristic for” |
| 1 | `explain/accumulator-mechanics` | “A query isn't a lookup.” |
| 1 | `explain/adopt-division` | “(‘work on our tasks and ping claude-16 when we need something’) Agreed. I'll ping claude-16 only when -40°” |
| 1 | `explain/analogy` | “The hook structure is similar to way 象 works.” |
| 1 | `explain/composition` | “when these are combined you get a cascade” |
| 1 | `explain/concept` | “we would have an M-typed-parks (I guess) that basically are ‘pipes with intent’.” |
| 1 | `explain/concretisation` | “Hot memory is the puddle.” |
| 1 | `explain/consequence` | “One pattern is already a diamond” |
| 1 | `explain/convergence-vision` | “-40° would be a kind of convergence of the intent based system” |
| 1 | `explain/counterfactual` | “if we had a better Cook Ting / Joints processing system, I might not need to use the autorunner at all” |
| 1 | `explain/cross-check-use` | “(useful for minus40) Live records since 10-09 carry the service's own ports again, so the follower's computed” |
| 1 | `explain/decision-reversal` | “I held it back because I was applying the 09:15 rule” |

Candidate additions (not applied):

- `explain/mechanics`: accumulator operation, composition, and other process
  mechanics.
- `explain/analogy`: analogies, concretisations, and compact conceptual
  models.
- `explain/decision-rationale`: retractions, division of work, and reversals.
- `explain/consequence`: consequences and counterfactual consequences.
- `explain/system-integration`: convergence visions and cross-system uses.

## Batch 2: 200 one-opener turns (2026-10-12, zai-1..5)

Shards: zai-1 3298365c (68), zai-2 92f3501f (59), zai-3 f45a3e6c (25),
zai-4 419afe56 (24), zai-5 16a94717 (24). Revalidated by claude-16: exit 0,
void 0, 1,215 acts. Cost: monthly Zai 56% → 67%; the first attempt (five
seats at once, 18:21Z on 10-11) exhausted the 5-hour window in 25 minutes
and wrote nothing; the rerun went two seats, then three.

| measure | value |
|---|---:|
| acts | 1,215 (425 operator, 790 reply) |
| incidences | 758 (390 closes, 368 keeps) |
| reply paragraphs answering 0 / 1 / 2 / 3 / 5 openers | 109 / 614 / 59 / 7 / 1 |
| scoped acts; explicit scope closes | 463; 1 (`turn-TTgUYF r-2`) |
| distinct move labels; singletons | 140; 60 |

The move-not-topic rule, stated in the packet after batch 1's consolidation,
held on every seat: proposals are `next-step`, `tentative`, `directive`,
`alternative`, `ordered-plan`; two seats caught and renamed their own topic
labels before committing.

### Sole-port variants on batch 2 (codex-72 d95e6c55)

| variant | asserted | confirmed | false positives | close recall | mis-closes |
|---|---:|---:|---:|---:|---:|
| V0 current: every untargeted paragraph | 744 | 510 | 234 | 91% | 17 |
| V1 first paragraph only | 196 | 168 | 28 | 53% | 17 |
| V2 first table-typed closer | 141 | 117 | 24 | 38% | 8 |
| V3 paragraphs before the first ask | 677 | 470 | 207 | 90% | 16 |
| V4 = V2 ∩ V3 | 140 | 117 | 23 | 38% | 8 |
| V5 = V3 ∩ table-typed closes/keeps | 273 | 198 | 75 | 47% | 8 |

**Decision (claude-16): hold V0.** V3 removes one mis-close in 200 records
and 27 false witnesses; that is not a kernel change. The variants that halve
mis-closes do so by discarding half the real closes. Mis-closes are not a
paragraph-selection problem: the kernel closes a port on *any* answering act
of a closing kind, and the labellers read 368 of 758 answers as `keeps`
(partial report, caveat, relay). Every effect disagreement in batch 1 ran the
same way (kernel closes, labeller keeps). So the next kernel candidate is an
**effect rule**, not a selection rule: an answer whose move is `report/partial`,
`report/in-flight`, `report/dispatched`, `qualify/*`, `delegate/relay-*`
keeps the port open. That needs the move on the act, which is what the fast
tier is for, and the 1,540 labelled incidences are its training and test set.
False witnesses (234 of 744) are evidence noise, not closure errors; they go
when `inferred` carries the move.
