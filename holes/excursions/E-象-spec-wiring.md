# E-象-spec-wiring: checking a specification by its open ports

Opened 2026-09-30 by claude-17 at Joe's direction, under M-象-2000. It connects
象's reading of intent to diagram checking through a typed model of a design
pattern. Step 1, a hand-drawn pilot with live numbers from claude-1, is done.

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

## The three parts

| Part | Supplies | State |
|---|---|---|
| 象 (M-象-2000) | Intent: Joe's turns read into fragments with roles and patterns, plus the history of a requirement across sessions | Built for operator turns |
| A formal model of a design pattern (this excursion) | A typed shape for each pattern and each stated claim: what it takes in, what it produces, what it leaves to be supplied | Not built; sketched below |
| Diagram checking (M-diagramprover, Layer 1.5) | Composition checked before anything runs | Designed; the mission lists pattern-to-diagram translation and typed ports as missing |

Joe (2026-09-30): this connection is the point of the excursion. The
fixed-point question that came up with claude-1's pooling is a side matter and
is kept in the appendix.

## The middle layer: a pattern as a typed box

A flexiarg already has the slots. The proposal is to read them as ports:

| Slot | Read as | Note |
|---|---|---|
| IF | Input ports: what must be present for the pattern to apply | Each input is marked **constructed** (some other box must produce it) or **external** (named source, countable) |
| HOWEVER | The input that would otherwise be left open | This is where a backdoor shows: the thing the pattern exists to stop being supplied freely |
| THEN | Output ports: what the pattern produces | |
| BECAUSE | Not a port. The source for the box: the turns or files that justify it | |
| NEXT-STEPS | Wires to other patterns | The checks that show the pattern did its job |

A claim from Joe's turns gets the same shape. "Cascades are formed for the
problem from the library" is a box with inputs *problem* and *library* and
output *policy set*, and it says the policy-set input of G is **constructed**.
A design that leaves that input external contradicts the claim. That
contradiction is what the check reports.

Fragment roles from 象's readings give direction between boxes. codex-4's rule
(futon2 `28f6adc15`) already turns roles into relations: context, condition,
dependency and goal enable what follows; rationale justifies what precedes.

What this layer leaves out on purpose: signs, balance, and the correspondence
between the five slots and a cell update (see the note on M-formal-patterns
above).

## Steps

1. **Pilot, done below.** One hand-drawn diagram of selection for one target,
   with live counts, and the open-port check run by eye.
2. **Ports for a handful of real patterns, by hand.** Take the five patterns in
   the pilot's reading (`futon-theory/mission-scoping`,
   `futon-theory/mission-lifecycle`, `war-machine/advanceability`,
   `orchestration/consent-gate`, `coordination/intent-to-mission-binding`) and
   write IF/THEN as ports from their flexiarg text. Test: do the slots carry
   enough to say whether two of them compose? If not, that says what the
   flexiarg format lacks.
3. **Intent as a claim list, from 象.** Produce the I1–I5 list below from the
   requirement's turns with the existing readings, and compare it with the
   hand-written one.
4. **A checker.** The diagram as data (boxes, ports, wires, external sources,
   claims); a script that reports open inputs, dangling outputs, and claims
   with no matching wire. Planted cases: the old `cascadePolicySet` must fail,
   codex-6's proposed construction must pass.
5. **Translation.** Pattern text to box, which is the part M-diagramprover
   lists as missing. Only after steps 2 to 4 show what a box needs to hold.

Steps 2 to 5 are not started.

## Pilot: intent, as stated by Joe

From the six turns in `TN-G-over-cascades-revisited.md` and the 2026-09-30
session with claude-1:

- I1. A policy is a pattern cascade; G is computed over policies.
- I2. Cascades are formed for the problem at hand from the pattern library,
  not from a pre-supplied pool.
- I3. G compares many of them.
- I4. The field is all unfinished missions, excursions and tickets.
- I5. Selection is not gated on an interpretation already existing.

## Pilot: the diagram for one target, M-self-documenting-stack

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

## Pilot: the open-port check

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

## What the pilot suggests

- The open-port check is cheap and found three things in a design written
  today by people actively looking for this defect. That supports trying it as
  a routine step when a specification is written.
- The check needs the intent as a short list of claims with sources (I1–I5).
  象's readings and the requirement-history search supply those.
- A parameter is a port. The retraction count k, the weights and the pooling
  law were each "defaults" or "modelling choices", and each decides a number
  Joe cares about (appendix).
- The check was done by eye. Two ports were not traced, and one finding came
  from claude-1's account of files I did not read. A checker over a written
  diagram (step 4) removes that dependence.

## Appendix: pooling and the fixed-point question (side measurement)

Not part of the 象-to-diagram line. Kept because it produced finding 3 and a
defect report.

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

**Outcome.** claude-1 confirmed by reading the code that direction, roots and
first action of every retraction policy followed the alphabetical order of
pattern names (through sorted edge endpoints). A fix is dispatched to codex-4
(job `invoke-1790780752104-29159-8b342179`).
