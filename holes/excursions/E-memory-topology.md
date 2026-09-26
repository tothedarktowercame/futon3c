# Findings
Admitting the subject relation does *not* clear the floor — λ₂ 0.0754 → 0.0836 — and on the scale-robust normalised Laplacian conduction gets **worse**, −24.5%. My method is validated: it reproduces the meter's own reading to 0.0754 against their 0.0755. The cause is mundane — concrete handles average 2.7 uses and **116 of 161 are used exactly once**, so they attach pendant nodes rather than shortcuts. I've withdrawn that paragraph in §4.1 rather than softening it.

**But the repair exposed something worth more than either verdict.** Projecting the relation instead — joining two memories directly when they share a handle — *does* clear the meter's criterion, 0.0754 → **0.2002** against a 0.1 floor. And normalised conductance simultaneously **falls 39%**.

| graph | components | unnormalised λ₂ | normalised λ₂ |
|---|---|---|---|
| baseline | 19 | 0.0754 | 0.0386 |
| + subjects as nodes | 4 | 0.0836 | 0.0291 |
| + shared-subject projection | 4 | **0.2002** ✅ | **0.0235** ❌ |

The meter computes λ₂ on the **unnormalised** Laplacian, where adding edges raises algebraic connectivity almost by construction, and a projection turns every k-memory handle into a k-clique. **Had I shipped the projection and re-run the meter, I'd have reported a green light earned by degree inflation.** WS2's criterion is preregistered and load-bearing, and as written it can be gamed. It needs normalising — or at minimum reporting both — before it gates any claim, including any future claim by us that the floor is cleared.

**So: what do we do about the topology? Not the graph, yet.**

1. **The operational fix is already in flight** — codex-2's content-match change routes around the topology entirely and has a live regression case. That's what actually gets the right memory to the runner.
2. **Normalise the meter** before trusting it again. Cheap, and I found the need the hard way.
3. **Make topology a standing per-pass metric** — bucket concentration, patterns-per-memory, λ₂ both ways. Nothing was watching this until this afternoon; that's the failure that let it drift for 20 passes.
4. **No graph surgery yet.** Two cheap re-relations of existing data have now been measured and neither improves genuine conductance. I'm withdrawing the prediction rather than replacing it — inventing a third intervention before understanding why these two failed would be guessing, and I've done enough guessing today.

The one thing I'd flag as *your* call rather than mine: pattern minting flatlined at 10 while the corpus grew to 90, which is what drives the 34% concentration. Whether the scribe should mint patterns more aggressively is a policy question with a real downside — more patterns means finer buckets but also more arbitration losers under the current single-winner recall. I'd rather settle it after the content-match fix lands, since that changes the cost of being in a losing bucket.

## Prior work: VSATlatarium as a curatorial precedent

Joe's VSATlatarium work in `~/vsat` is a close architectural precedent for
the proposed Librarian role. VSAT does not rewrite a story when someone sees a
new relationship. It preserves canonical story/scene content and adds a
separate interpretive layer of typed, rationale-bearing links between stories.
Links are proposed, reviewed, accepted or rejected, and may later be retired;
the steward curates this overlay, while the planetarium makes it navigable and
inspectable.

The correspondence is:

| VSATlatarium / VSATLAS | Memory system |
|---|---|
| canonical story and scene content | append-only memory evidence |
| interpretive story link | typed inter-memory relation |
| link type and rationale | relation type, reason, and provenance |
| proposed link | Librarian-generated relation proposal |
| accept / reject / retire | review / challenge / supersession lifecycle |
| steward | Librarian |
| pilot-filtered constellation | query- or domain-specific retrieval projection |
| planetarium | inspectable memory atlas |

The transferable principle is:

> Narrative extraction creates an interpretive proposal, not a canonical fact.

When the Librarian notices that one memory supersedes, supports, refines, or
contrasts another, it should emit a proposed relation rather than silently
rewrite either memory or make the relation immediately traversable. The
proposal should name its source and target memory ids, relation type,
rationale, exact narrative evidence, proposer, transaction and valid times,
and review/witness state. Only its reviewed projection should become
operational retrieval structure.

The transfer should preserve Futon's stronger epistemic and temporal
invariants rather than copy the VSAT implementation mechanically:

- VSAT's vote threshold is not sufficient warrant for a memory relation;
  author/proposer and reviewer should remain distinct.
- Link retirement in memory should be append-first and bitemporal, not a
  destructive status toggle.
- A `causal` relation requires stronger evidence than thematic similarity or
  contrast.
- Adjacency or thematic context may remain visible without automatically
  becoming a propagation channel.

This suggests that the Librarian is not merely tidying metadata. It is
performing for the memory corpus the interpretive stewardship already
embodied by VSATLAS's link-review-and-curation loop: maintaining a revisable
atlas over canonical narratives.

## Long-term suggestions (Codex-3, 2026-07-30)

The long-term target should not be one globally well-connected graph. It
should be **one authoritative evidence hypergraph with several purpose-built,
rebuildable retrieval projections**. A useful topology is one in which a task
seed can reach the right current, reviewed memories through meaningful typed
paths within a bounded read budget. Global connectivity is secondary: domain
boundaries, specialist communities, and one-use subjects can all be legitimate
structure.

The retrieval architecture suggested by the present failure is:

```text
task/context
    ↓
candidate generators
  FTS · semantic search · explicit subjects · selected patterns
    ↓
admissibility/warrant
  domain · current/as-of · reviewed · provenance · witness
    ↓
bounded typed expansion
  instantiates · supports · challenges · refines · supersedes · contrasts
    ↓
rank + diversify under one fixed budget
    ↓
memories with explanation paths
    ↓
use/outcome receipts → Librarian proposals and relation-weight evidence
```

Candidate generation may therefore be plural while evidential warrant remains
singular. The content-match fix is a good instance of that division: text
search proposes a memory, but the reviewed graph must still warrant it before
it can be returned. Direct matches and graph-expanded candidates should be
merged and deduplicated under one global bound rather than forced through one
exclusive pattern winner.

### Relation roles and projections

Relation types should state what work they are allowed to do.

- Subject, session, mission, and provenance relations usually provide context,
  filtering, or explanation; they need not be propagation channels.
- Pattern attachment says that a memory instantiates a reusable mechanism.
- `supports`, `challenges`, `supersedes`, `refines`, and `contrasts` can
  support controlled traversal when independently warranted.
- `distills` should preserve provenance whether or not it participates in a
  retrieval projection.

The store remains the canonical typed, bitemporal record. Each retrieval
operator declares which relation types it projects, their direction, its
domain and temporal basis, and its read budget. Derived adjacency or search
indexes are valid only when they are faithfully rebuildable from that record.

Do not clique-project a shared handle for operational diffusion. A handle
touching \(k\) memories is one incidence relation, not \(k(k-1)/2\)
independent memory relations. Retain the bipartite/hypergraph incidence
structure and, when propagation is evaluated, use a degree-normalised
hypergraph or random-walk operator. Longer term, relation-specific incidence
matrices \(B_r\) and separately calibrated coefficients \(\theta_r\) fit the
typed-memory theory better than one untyped adjacency matrix.

### Meter v2

Preserve the preregistered WS2 result as historical evidence, but retire
"global unnormalised λ₂ > 0.1" as a promotion gate. Version the meter rather
than rewriting its criterion after the fact. The next meter should report:

- unnormalised and normalised spectra, explicitly labelled;
- degree distributions by node class and relation type;
- component and isolate counts by domain and projection;
- hub concentration, memories per pattern, and patterns per memory;
- query-relative reachability from frozen benchmark seeds;
- expansion precision and latency at a fixed read budget; and
- held-out hit@k/MRR and outcome lift under per-relation-type ablations.

Spectra and conductance remain useful structural diagnostics. The principal
gate should be whether a projection improves held-out retrieval or
independently witnessed outcomes at the same budget. This prevents legitimate
modularity from looking like failure and prevents added edges from looking
like improvement merely because they raise degree.

### Pattern minting and gardening

Do not mint patterns to flatten buckets or improve a topology meter. Mint when
repeated, independently arising residual misses expose a reusable distinction
that the existing pattern vocabulary cannot express. A candidate should carry
triggering contexts, supporting and challenging memories, its nearest
existing patterns, and a predicted distinction or outcome on which it can
fail. It begins with an explicitly unearned prior and gains standing only
through use and independently witnessed outcomes.

The mature pattern lifecycle should:

- permit multi-label memory attachment where several mechanisms genuinely
  apply;
- retrieve from several bounded pattern neighbourhoods alongside direct
  content matches;
- split patterns whose triggering contexts or outcomes become internally
  heterogeneous;
- merge patterns that held-out behaviour cannot distinguish; and
- supersede or retire patterns bitemporally rather than erase them.

This removes the present bad incentive in which finer buckets manufacture
more arbitration losers. Pattern count and attachment mass remain observations,
not value evidence.

### Suggested sequence

1. Land meter v2 and freeze a chronological query/relevance benchmark
   containing the known retrieval misses.
2. Stabilise a bounded multi-source candidate-union contract: direct matches
   plus the top few pattern neighbourhoods, with reason-bearing inclusion and
   deduplication.
3. Have the Librarian lift genuine narrative relations such as supersession,
   resolution, contrast, and refinement into reviewed typed edges. Do not
   create bridge edges merely to improve connectivity.
4. Measure the held-out contribution of each edge type. Relations that add no
   retrieval value may remain provenance or context relations rather than
   propagation channels.
5. Once the relation census and receipt corpus are adequate, compare typed
   \(k\)-step propagation with the hybrid lookup baseline.

The governing acceptance rule should be:

> A structural change is good only when it improves held-out retrieval or
> outcomes at the same read budget while preserving domain, temporal, review,
> provenance, and witness invariants.

This lets graph structure earn its role. Lexical or semantic search may remain
the best seed generator, while the hypergraph supplies admissibility,
explanation, contradiction, lifecycle, and selective multi-hop transfer.

### Further ideas

The labelled corpus is 9 rows. Six where recall completed and surfaced something; three where a memory was demonstrably used.** That's everything — the metric-3 fields only began recording today, after the attachment fix.

So the damage sweep's own success criteria — "relevant-memory hit@k loss", "eventual outcome loss where receipts permit" — currently rest on **three positive labels**. And the proposal asks us to compute a six-component T and a five-component H against that. This lane has already published a `:below-calibration-minimum` verdict once, on the Ψ-v2 replay with one metric-bearing row. Fitting an eleven-component liveness scorecard to n=3 would be the same failure one level up — and structurally the *same* failure I made today: a plausible structural score, uncalibrated, pointing the wrong way.

**What I'd adopt immediately, because it costs nothing:**

- **`D_state` vs `D_functional`.** This is the discipline, not the infrastructure. It's precisely what I violated: I measured reorganisation and claimed usefulness. Adopting it as the acceptance framing is free and I'd apply it retroactively to everything in §4.1/§5.
- **"Does an edge type pay rent."** Ablate `contrasts`, `distills`, subject edges; if retrieval doesn't move, they're explanatory decoration and must not be counted as operator food. That single test would have prevented today's λ₂ episode.
- **The scale-hierarchy reframing** — `memory → tactic pattern → strategy family → terrain`. This is the best idea in the document. It reads 90-on-10 as a *missing intermediate level* rather than bucket imbalance, and crucially it gets there **without inventing cross-memory edges** — which is exactly what my clique projection did wrong, and what Codex-3 had already warned against.

**What I'd invert:** don't compute T/H first. **Run the perturbation sweep first, then ask which structural features predict the damage.** As proposed, T/H are fitted and *then* validated against damage — which risks confirming the scorecard. Damage is the ground truth; T and H are hypotheses about what predicts it. That ordering also means we don't need to resolve the judgement-laden components ("coherence between triggering contexts and outcomes") before doing anything.

**Two additions I'd make load-bearing:**

1. **The sweep must not run against live dispatch.** With 21 timeouts and 13 store-unavailable in the offered records, a perturbation sweep on the production path would measure store latency as damage. Codex-3 says "freeze a store snapshot" — that's necessary but not sufficient; recall must also execute off the production timeout path, or every perturbation gets a noisy null.

2. **Preregister the abandonment condition.** Today's lesson isn't "λ₂ was the wrong metric", it's "a metric can be *inverted* and still look meaningful." So state in advance what pattern of T/H against damage would mean liveness doesn't transfer to memory — before we compute either.

**On sequencing, this strengthens rather than competes with my earlier pushback: the frozen benchmark is now the blocking dependency for two independent proposals.** But it can't come from receipts alone — n=9 is a seed, not a benchmark. It needs relevance labels, and the honest way to get them is back-labelling from proof content on rows we've already closed, where the ground truth is in the Lean file.

We do have real calibration anchors now that didn't exist this morning: **LemniscateComponents (4 surfaced, 4 used, one load-bearing as proof architecture)** as known-good, **run 25 (recall `:ok`, right memory discarded by arbitration)** as known-bad-and-diagnosed, and the 21 timeouts as known-excluded. That's the beginning of Codex-3's "known-good, dead and messy" set.

If you want it, the well-scoped next job is the benchmark: back-label relevance on the closed rows, exclude by `:recall-reason`, freeze it. That unblocks meter v2, the damage sweep, and the rent test simultaneously. I'd bell that before anything computes a T.

## First perturbation experiment: D_state only (Codex-3, 2026-07-30)

The smallest offline sweep proposed above has now run. Executable, frozen
fixture, write-once result, and full reading live in
`holes/labs/M-memory-retrieval/`:

- `damage_state_sweep.bb`
- `damage-state-fixture-20260730.edn`
- `damage-state-results-20260730.edn`
- `damage-state-results-note.md`

The live store was read once to capture the fixture. The experiment itself
replays the lexical-proposal plus current-reviewed pattern projection entirely
offline; an immediate rerun reproduced the frozen result byte-for-value. It
excludes packet term extraction, problem/subject endpoint arms, receipt
ranking, body hydration, and the live timeout path. Its claim boundary is
explicit: it measures candidate-state divergence only, not D_functional,
usefulness, outcome lift, T/H, or liveness.

Two evidence-backed queries were swept:

1. Run 25's diagnosed `roots outside` content match.
2. LemniscateComponents' first strict ladder tier,
   `card route connectedcomponents`.

For each, the sweep independently removed every reviewed memory edge, every
individual memory→pattern role, the whole content arm, and the whole pattern
arm.

The first result is a complementary-arm result. In run 25, direct content
matching retains the historically discarded a92J05 memory at rank 2; removing
the content arm loses both exact matches and replaces them with pattern
neighbours. In LemniscateComponents, the pattern arm supplies three of the
four historically used memories; removing it leaves only the directly matched
architecture memory.

The second result is a nomination-bottleneck result. The Lemniscate
architecture memory is the sole lexical match and its
`math/connectedness-component-api` attachment nominates the neighbourhood:

- remove its whole reviewed edge → four candidates become zero
  (Jaccard damage 1.0);
- remove only its pattern role → the direct memory remains but all three
  neighbours disappear (Jaccard damage 0.75);
- remove any other member edge → only that member disappears
  (Jaccard damage 0.25).

Run 25 shows the same mechanism in a less concentrated form. Only 5 of 55
single-edge removals and 1 of 55 pattern-role removals alter the top-five
state, but removing the attachment that nominates the winning neighbourhood
changes four candidates on each side (Jaccard damage 0.889). State damage is
therefore sparse but can be large because the current bounded operator makes a
discrete neighbourhood-selection decision.

This is not yet a liveness result. It identifies a more precise causal unit
for the future benchmark: not only a memory node, but a **nomination
attachment** whose presence determines which cluster consumes the bounded
candidate budget. D_functional remains blocked on the transport-clean,
back-labelled relevance benchmark, exactly as `Further ideas` requires.
