# E-象-spec-wiring: checking a specification by its open ports

Opened 2026-09-30 by claude-17 at Joe's direction, under M-象-2000. Pilot: one
hand-drawn diagram, one target, live numbers from claude-1.

## The idea

Joe (2026-09-30): 象-2000 may be useful as a specification language, combined
with M-diagramprover to validate specifications as they are developed, at the
design level and before code or delivery. 象 supplies the intent, read from
Joe's turns. A wiring diagram supplies the check.

The check used here is the smallest one that would have caught
`cascadePolicySet` (see `TN-G-over-cascades-revisited.md`,
`TN-facade-considered-harmful.md`, and codex-6's
`futon2/holes/labs/wm-contract/NOTE-lean-backdoor-audit-2026-09-30.md`):

> Draw each component as a box with input and output ports. Every input port
> is either **wired** to the output of another box, or **declared external**
> with a named, countable source. An input that the stated intent says is
> constructed, but which no box produces, is a finding. So is an output that
> nothing consumes.

**Not built on M-formal-patterns** (`futon5/holes/M-formal-patterns.md`). That
mission models a pattern as a signed graph and tests balance. Balance says a
consistent assignment exists; the rule-rewriting automata paper
(`futon5/holes/tech-notes/paper/draft9.tex`) reports that fixed points are not
attractors, so existence does not show a process reaches it. It also ties the
five pattern slots to a cell update, which a specification does not need. This
excursion keeps only ports and wires. Balance could be added later as a second
check.

## Intent, as stated by Joe

From the six turns in `TN-G-over-cascades-revisited.md` and the 2026-09-30
session with claude-1:

- I1. A policy is a pattern cascade; G is computed over policies.
- I2. Cascades are formed for the problem at hand from the pattern library,
  not from a pre-supplied pool.
- I3. G compares many of them.
- I4. The field is all unfinished missions, excursions and tickets.
- I5. Selection is not gated on an interpretation already existing.

## The diagram: one target, M-self-documenting-stack

Numbers are claude-1's (bell `invoke-1790780363591-29154-11bf9d3f`), checked
against the files where a file exists. Paths are under `/home/joe/code`.

```
 library ──1,431 patterns──┐
 (futon3/library)          │
                           ▼
 analysed turns ──4,670──▶ [graph builder] ──3,163 edges, 519 patterns unlinked──┐
                           mined_pattern_graph.py                                │
                                                                                 │
 mission HEAD ──text──▶ [象 reading] ──5 patterns / 9 citations──┬──────────────┐ │
 (M-self-documenting-stack)                                      │              │ │
                                                                 ▼              ▼ ▼
                                                     [reading cascades]   [seed filter]
                                                          1 policy         4 accepted, 1 refused
                                                                 │              │
                                          k = 3, weights ───────(?)────────▶ [retraction]
                                          (defaults in code)                   3 policies
                                                                 │              │
                                                                 └──────┬───────┘
                                                                        ▼
                                                              [deduplicate] ──4 policies──▶ [score: G]
                                                                                              │
                                   fit evidence, preference ───────────(?)───────────────────▶│
                                                                                              ▼
                                   other targets' families ──(6 more targets, 27 policies)──▶ [posterior]
                                                                                              │
                                                                              pool by first action
                                                                              (2 pools for this target)
                                                                                              ▼
                                                                                  [select-over-families]
                                                                                              │
                                                                                              ✗  not called by
                                                                                                 the real decision

 assembled problems ──"constructed candidates"──▶ [cascade-decision]  ◀── the decision that runs today
 (hand-written sources + interpretation answers)      1 policy at click 20
```

## The open-port check

| Input port | Wired to | Or external source | Count | Verdict |
|---|---|---|---|---|
| graph builder ← library | — | `futon3/library` files | 1,431 | external, named, counted |
| graph builder ← analyses | — | batch + live analysis files | 4,670 | external, named, counted |
| 象 reading ← mission HEAD | — | the mission file | 1 | external, named |
| seed filter ← reading | 象 reading | | 5 → 4 + 1 refused | wired |
| seed filter ← graph | graph builder | | | wired, **but no durable pinned copy** (claude-1) |
| retraction ← seeds | seed filter | | 4 | wired |
| retraction ← k, weights | — | defaults in `target_policy_family.clj` | k = 3 | **free parameter; see below** |
| score ← policies | deduplicate | | 4 | wired |
| score ← fit evidence, preference | not traced | not traced | | **not examined in this pilot** |
| posterior ← other families | same construction, per target | | 7 targets, 27 policies | wired; **7 of about 637 open items** (I4) |
| cascade-decision ← candidates | — | assembled problems | 1 at click 20 | **unwired from the construction** (I2) |
| Lean `cascadePolicySet` ← menu | — | none declared | any list | **free** (I2, I3) |

Output ports:

| Output | Consumed by | Verdict |
|---|---|---|
| select-over-families → selection | nothing in the running decision | **dangling** |

### Findings

1. **The new construction satisfies I1 and I2 on paper and is not connected
   to the decision that runs.** `family-selection/select-over-families`
   (futon2 `a61949314`) takes policy sets built from a reading and the graph.
   `cascade-decision` still reads pre-assembled candidates. The construction's
   output goes nowhere.
2. **The formal model has no box for the construction.** In Lean the policy
   menu is still a free argument (`Proof2/CascadePolicySet.lean:46`);
   `HeadCascadeG.lean` has no declaration for the policy set; and
   `Requirements.lean` checks the recorded size afterwards (claude-1's
   reading of those files, which I have not read beyond the one declaration).
3. **k is a free parameter that fixes a critical parameter.** The number of
   retraction policies per target is exactly k. With k = 3 this target gets 3.
   Nothing in the design says where 3 comes from. I3 says "many".
4. **The field port carries 7 of about 637.** Wired, and degenerate against
   I4. (The seven were claude-1's test set; this is a statement about the
   experiment, not yet about the design.)
5. **The graph port has no pinned source.** The file changes whenever an
   analysis lands.

The old design fails the check at one port (the menu). The new design passes
at that port for the Clojure construction and fails at three others: the
dangling output, the free k, and the missing Lean box.

## Pooling and the fixed-point question

Joe's hypothesis: pooling near-variant policies is identifying a fixed point.
If more and more retractions are generated and near-variants are pooled, the
set of pools stops changing, and that stable set is the policy set.

**What claude-1's pooling is.** Not pooling by similarity. The selector sums
posterior probability over policies that share a first action
(`futon2/src/futon2/aif/policy.clj`, `cascade-first-action`). For this target
the three retractions share roots and pool to 0.108 (about 0.04 each), beating
the best single policy in the joint set at about 0.07. So three near-copies of
one cascade outvoted a different cascade by being three.

**Measurement.** Seeds: the four accepted ones. Graph: the current file
(sha256 `072c3d85…`, copied to `/tmp/claude17/graph-072c3d85.json`). Tool:
`scripts/pattern_retraction.py`. "Connector" = a non-seed pattern in a
retraction.

| Weights | k | Retractions | Cost range | Distinct connector sets | Connectors used | Edges shared by all |
|---|---|---|---|---|---|---|
| default | 3 | 3 | 11–11 | 3 | 3 | 2 of 4 |
| default | 6 | 6 | 11–12 | 6 | 7 | 2 of 4 |
| default | 10 | 10 | 11–12 | 10 | 11 | 2 of 4 |
| all kinds cost 1 | 10 | 10 | 4–5 | 10 | 7 | 0 |
| consecutive-turn dear (9) | 10 | 10 | 13–15 | 10 | 11 | 2 of 6 |
| co-cited dear (6) | 10 | 10 | 12–15 | 10 | 6 | 0 |

Across the four weightings at k = 10: 29 distinct connector sets, 21 distinct
connector patterns. The consecutive-turn-dear weighting shares no connector
set with any other.

**Reading.**
- **No fixed point in the range tested.** Every k returned k distinct
  retractions, at almost the same cost (11 to 12 under default weights). The
  set of connectors keeps growing: 3, 7, 11. The tool did not run out.
- **What is stable is small.** Under default weights two edges appear in every
  retraction (`mission-lifecycle`–`consent-gate` and
  `mission-lifecycle`–`advanceability`). Under two of the four weightings
  nothing is shared by all ten.
- **So the near-variants are not variations on one cascade that converge.**
  They are many roughly equally cheap ways to join the same four seeds, and
  which ones appear depends on k and the weights. Pooling them by first action
  multiplies the weight of the seeds' shared root by k.
- **Consequence for the design.** With pooling by first action and k free, the
  probability of the retraction pool is roughly proportional to k. At k = 3 it
  won by 0.108 to 0.07. That outcome was decided by the choice of k.
- **Limits.** One target, four seeds, k up to 10. k = 20 and 40 were not
  measured: the tool's cost grows steeply (58 s at k = 10 under default
  weights). I did not reproduce claude-1's roots or probabilities; the pooled
  figures are theirs. A fixed point might appear under a different notion of
  pool (for example, pooling by the shared edges), which was not tested.

**One thing read and not verified.** `target_policy_family.clj` gives each
retraction policy `:precedence (:nodes r)`, and the retraction tool returns
nodes in sorted order. If first action is taken from that vector, a
retraction's first action is its alphabetically first pattern. claude-1
reports the roots as `mission-lifecycle` and `mission-scoping`, so another
path may apply. Worth a look by whoever owns that function.

## What the pilot suggests

- The open-port check is cheap and found three things in a design written
  today by people actively looking for this defect. That supports trying it as
  a routine step when a specification is written.
- The check needs the intent as a short list of claims with sources (I1–I5).
  象's readings and the requirement-history search supply those.
- A parameter is a port. k, the weights and the pooling law were each
  "defaults" or "modelling choices", and each decides a number Joe cares
  about.

## Next steps, none started

1. Trace the two ports not examined: fit evidence and preference into G.
2. Repeat for a second target with more seeds.
3. Try a pool defined by shared edges and see whether that set stabilises.
4. Decide with Joe whether the port vocabulary should be written into the
   flexiarg slots (IF and HOWEVER as inputs, THEN as output, NEXT-STEPS as
   wires), which is the part M-diagramprover lists as missing.
