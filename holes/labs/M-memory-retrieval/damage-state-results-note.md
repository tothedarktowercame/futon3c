# First memory-topology perturbation sweep — D_state only

Date: 2026-07-30  
Status: first frozen offline result; descriptive, not calibrated

## Question and claim boundary

Does removing one reviewed memory edge or one memory→pattern role change the
ordered candidate set produced by the current lexical-proposal plus
reviewed-pattern projection?

This experiment measures only **D_state**: causal divergence in the ordered
candidate ids under an exact frozen perturbation. It does **not** measure
D_functional, memory usefulness, outcome lift, Salingaros T/H, or liveness.
Historical use labels identify the two fixtures but are not used to fit or
score the perturbations.

The executable is `damage_state_sweep.bb`. Its `--capture` mode performed
read-only store queries once and wrote
`damage-state-fixture-20260730.edn`; ordinary runs read only that fixture.
`damage-state-results-20260730.edn` is write-once and deterministic: an
immediate offline rerun reproduced it byte-for-value.

## Frozen cases

The fixture was captured at `2026-07-30T21:19:07Z` from the current reviewed
mathematics projection (revision 129, generation 128). Text-index
`index-as-of` was `2026-07-30T13:29:32Z`.

1. **Run 25 diagnosed miss** — query `roots outside`. Outcome evidence
   `7ed99fe8-c65b-4239-9c18-3c9f11ac82bd` records that this exact ladder query
   retrieved
   `e-codexpilot-close-a92J05-by-transferring-the-unit-disk-zero-count`,
   which historical pattern arbitration then discarded.
2. **Lemniscate known-good** — query `card route connectedcomponents`, the
   first strict ladder tier formed from offered evidence
   `e-a2cb197e-b14a-41c9-89b6-4b6ac16a9b3a`. Outcome evidence
   `7fd3850e-959a-4c6c-85d9-f40aec83963d` records four surfaced and four used,
   with
   `e-codexpilot-count-polynomial-lemniscate-components-by-roots-plus-one-exterior`
   named as the architecture memory.

The replay deliberately excludes packet term extraction, problem/subject
endpoint arms, receipt ranking, body hydration, and the live timeout path.
It therefore isolates the relation between lexical nomination and the
reviewed pattern projection. It is not a full historical dispatch replay.

## Operator and perturbations

For each frozen query:

1. retain the first ten text-search rows of an admissible type;
2. validate memory rows through current, reviewed, mathematics
   `:memory/assert` edges;
3. form direct content matches and pattern proposals;
4. expand the top five proposed patterns through the frozen reviewed
   projection;
5. merge content matches before pattern memories, deduplicate, and retain five
   candidates.

The sweep then independently:

- removes each reviewed `:memory/assert` edge from every frozen view;
- removes each individual memory→pattern role while retaining the rest of its
  reviewed edge;
- ablates the content arm as a whole; and
- ablates the pattern arm as a whole.

For each fork it reports lost/gained ids, set symmetric difference, Jaccard
distance, and reciprocal-rank damage. No live store write or dispatch occurs.

## Results

| case | baseline composition | without content | without pattern |
|---|---|---|---|
| run 25 | 2 direct + 3 pattern | loses both direct matches, including the historically missed memory; Jaccard damage 0.571 | loses 3 pattern neighbours; Jaccard damage 0.600 |
| Lemniscate | architecture memory direct + 3 pattern neighbours | unchanged: the architecture memory also returns through its pattern | loses the 3 neighbours; Jaccard damage 0.750 |

### Run 25

The fixed operator's baseline retains the historically missed memory at rank
2 as a direct content match. Removing the content arm loses it and the other
direct match, replacing both with memories from the pattern projection. This
reproduces the *shape* of the diagnosed failure: without direct nomination,
the exact lexical hits are overwritten by neighbourhood members.

Only 5 of 55 single-edge removals change the top-five state, and only 1 of 55
pattern-role removals does. Most of the frozen neighbourhood is outside the
query's active candidate boundary. The exceptional pattern-role removal is
the attachment of
`finish-an-outside-root-count-from-an-inside-count-and-no-boundary-roots` to
`tactic-algebra-interference`; removing it changes three of five candidates
and admits three replacements (Jaccard damage 0.750). Removing that whole edge
changes four of five candidates on each side (Jaccard damage 0.889), because
it removes both a direct match and the support that nominated the winning
pattern.

This is a discontinuity in proposal selection, not diffusive propagation:
one matched memory can pivot which neighbourhood consumes the remaining
candidate budget.

### Lemniscate

The baseline exactly reproduces the four historically used ids. The
architecture memory is the sole lexical match and nominates
`math/connectedness-component-api`; that pattern supplies the other three
memories.

The pattern arm therefore has clear **state-level causal reach**: ablating it
removes three of four candidates. But the nomination has a single structural
bottleneck:

- removing the architecture memory's whole reviewed edge changes the
  candidate set from all four ids to empty (Jaccard damage 1.0);
- removing only its pattern role retains the direct architecture memory but
  loses all three neighbours (Jaccard damage 0.750);
- removing any other member edge loses only that member (Jaccard damage
  0.250).

The observed known-good neighbourhood is thus internally useful according to
the historical receipt, but operational access to it is brittle in this
frozen operator: one content-matched attachment gates the whole cluster.
Whether that counterfactual would have changed the proof outcome is
D_functional and remains unmeasured.

## Reading

The two arms are complementary, not competitors:

- direct content matching protects exact nominations from arbitration;
- pattern expansion supplies related memories that lexical search did not
  nominate.

The first sweep also supplies a more precise object for future “damage”
work. The relevant causal unit is not just a memory node. It can be a
**nomination attachment** whose removal changes which whole neighbourhood
enters a bounded top-k set. Candidate-state damage is correspondingly sparse
but sometimes large.

No Salingaros score should be fitted to two cases. The immediate next use of
this apparatus is to run the same offline sweep over the frozen,
transport-clean relevance benchmark proposed in `E-memory-topology.md`.
Only then can D_functional be computed and structural features tested as
predictors rather than stipulated as liveness.
