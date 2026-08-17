# Cascade formation: a pattern-based analysis of declared vs derived edges

*claude-7 (analyst), 2026-08-17, at the operator's direction: "maybe we
need to develop some pattern-based analysis of exactly this problem
before we make a decision." Method: the pattern form itself (condition /
action / because, per Moran's reading of Alexander) applied to the
question of how cascade edges should come to exist. Evidence base: this
mission's own record. No decision is made here; the patterns are the
decision's raw material.*

## The tension, stated once

The `upstream:` file field (plan §9 proposal) makes cascade edges
DECLARED: authored, versioned, watcher-synced, auditable, and stable
enough for Programme S to take a Laplacian of. The operator's objection:
upstreams change dynamically, and cascades are often wanted ON-THE-FLY —
built as needed against the current problem state (which is also Moran's
own conclusion: the wall chart fails at scale; AA *queries*). A
hard-coded upstream freezes one context's judgment of a relation that is
contextual. Both sides are right about different halves of the lifecycle.

---

## Pattern 1: DECLARED SKELETON

**Condition.** Some pattern-to-pattern relations are stable across
contexts (layer-cake-crossover-split specializes
separate-into-independent-pieces in every context we can imagine), and
downstream machinery needs a definite graph: spectral measurement
(Programme S), reviewer navigation, reproducible retrieval.

**Action.** Declare the stable minority of edges in the pattern files
(plural `upstreams:`, each with its own one-line because), watcher-synced
to the store as typed edges. Keep the skeleton SPARSE — declare only
what you would defend in review.

**Because.** Declaration is what makes an edge witnessable, versionable,
and refusable — the same properties the whole mission has bet on (the
witness standard, W.3-era; P16's witnessed union chosen over live
queries precisely for auditability). A cascade with NO declared edges
cannot be measured, reviewed, or diffed; W.67's fragmentation was only
visible because attachments were declared.

**Counter-force it must respect.** Alexander: patterns have multiple
parents; a single mandatory upstream re-imposes the tree he rejected.
Hence plural, optional, defeasible — never exactly-one.

---

## Pattern 2: ON-THE-FLY CASCADE (derived filigree)

**Condition.** A seat facing a specific problem needs the cascade FOR
THAT problem — which patterns apply, in what enablement order — and the
relevant relations depend on the problem state (Moran: past modest
scale, the dependency graph must be queried dynamically; a thousand-
pattern wall chart is useless).

**Action.** Derive cascade edges per query: given the problem state and
the pattern set, compute candidate enablement/specialization relations
(lexical, structural, or model-judged) and hand the seat a cascade
SLICE. The derivation is ephemeral — except see Pattern 3.

**Because.** Contextual relevance cannot be pre-declared: the f8 shelf
carried nine patterns whose relevance to a03J04 no author could have
wired in advance; push recall's lexical-anchor failure (W.61) shows
static matching losing to context. Deriving keeps the store's declared
layer sparse and honest instead of bloated with speculative edges.

**Counter-force it must respect.** An unwitnessed derivation is exactly
the class of silent machinery this mission spent two frames eliminating.
A cascade that existed only inside one query can't be audited, can't be
reproduced (the dispatch-time-snapshot problem, V2 §4.3's caption, all
over again), and can't teach the store anything.

---

## Pattern 3: EDGES EARN PERMANENCE (the bridge pattern)

**Condition.** Patterns 1 and 2 coexist; the question is how an edge
moves from derived to declared — and who decides.

**Action.** Receipt every derived edge that gets USED: when a cascade
slice feeds a dispatch, the slice (edges + derivation inputs) goes into
the dispatch receipt, exactly as eligibility provenance does today
(P16). Edges that accumulate use-receipts become PROMOTION CANDIDATES
for the declared skeleton — proposed, reviewed (does this relation
generalize beyond the contexts that used it?), then declared in the
file with its because; rejected candidates simply remain derivable.

**Because.** This is the demand-side principle already in the whitepaper
(§6: votes, builds, callbacks — kept because used) applied to structure
instead of content, and it is the same attach-then-review economy the
operator ruled for memories (W.24): nothing becomes durable without a
judged review, nothing is judged without evidence of use. The cascade
then grows exactly as fast as its usefulness is demonstrated, no faster.

---

## Pattern 4: REPOINTABLE DECLARATIONS

**Condition.** A declared edge goes stale: the upstream splits, merges,
or turns out wrong (the operator's first objection).

**Action.** Repointing is a first-class, receipted operation with the
filesystem as source of truth: edit the file's upstreams (a reviewed
editorial act, like pattern authoring today), watcher syncs, and the
store REPOINTS rather than duplicates — old edge retired with a
tombstone referencing the edit, not silently dropped (the projection-gap
lesson: silently missing edges poison every measurement downstream).

**Because.** Moran's best-aged sentence: transparent beats correct —
knowledge quality is a property of the REVISION PROCESS. An edge that
cannot be repointed will be routed around (agents will re-derive and
ignore the declaration), and the declared layer will rot into exactly
the wall chart Moran condemned.

---

## Pattern 5: THE SLICE IS THE UNIT OF USE

**Condition.** Seats never need "the cascade"; they need the cascade
slice relevant to one problem at one moment (Moran's AA: retrieval
against the project model).

**Action.** The queryable object is always a slice: declared skeleton ∪
derived filigree, computed for this problem state, delivered with
provenance separating the two edge classes (so the seat and the
measurement both know which edges are load-bearing declarations and
which are today's conjecture).

**Because.** This dissolves the either/or: the operator's on-the-fly
cascades and Programme S's measurable store are different VIEWS — the
slice for acting, the skeleton for measuring — connected by Pattern 3's
promotion economy. It also matches the mission's provenance idiom
exactly: f8's eligible set was snapshot ∪ cycle-promoted WITH the
sources labeled; a cascade slice is skeleton ∪ derived with the same
labeling.

---

## What would falsify this analysis

- If derived-edge use-receipts turn out to be mostly noise (edges used
  once, never again), Pattern 3's promotion economy has no signal and
  the skeleton should grow editorially instead.
- If the declared skeleton stays so sparse that slices are ~100% derived,
  declaration is overhead and Programme S should measure accumulated
  RECEIPTED edges instead.
- If repointing turns out to be frequent (upstreams churning per frame),
  the file-as-source-of-truth assumption is wrong and edges belong in
  the store natively with the files as documentation.

Each falsifier is observable within a few frames of running Pattern 3's
receipts. **Proposed next step: implement ONLY the receipts (Pattern 3's
observation half — no declared edges, no derivation engine yet), run 2-3
frames, and let the use data choose between the patterns.** Decision
deferred to evidence, per the operator's instinct.
