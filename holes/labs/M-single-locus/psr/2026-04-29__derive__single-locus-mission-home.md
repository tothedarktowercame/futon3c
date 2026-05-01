# PSR: single-locus/mission-home — first sibling (algorithm trial run)

context:
  First execution of `~/code/algorithms/next-invariant.md` against the
  rank-4 priority candidate `home-repo` in family
  `atomic-inspectable-units`. Shape-first IDENTIFY surfaced the
  `single-locus` shape — uniqueness of attribute mapping per identity
  — which generalizes existing operational patterns
  (`agency/single-routing-authority`, `single-boundary`) to a new
  artifact-class.

patterns considered:
  - **invariant-coherence/single-locus** (just authored) — primary
    new shape. Cardinality-1 per (I, A) pair. **Selected.**
  - **invariant-coherence/shape-first-identify** — methodology;
    surfaces the shape; led to multi-instance scope. **Selected.**
  - **invariant-coherence/protocol-family-naming** — namespace IDs
    `single-locus/<artifact-class>`. **Selected.**
  - **agency/single-routing-authority** (futon3 prior art) — single-
    locus-instance ahead of shape's naming; back-applied as a
    referenced exemplar. **Selected as accompanying.**

decision:
  Build `check-mission-home-locus` in a new namespace
  `futon3c.logic.locus` (kept separate from `archaeology.clj` to
  signal the family/shape distinction; locus shapes will accumulate
  more siblings via Codex handoff). The check enumerates active
  mission files in `holes/missions/M-*.md` per repo, parses the first
  10 lines for `Home-repo:` annotations (markdown-tolerant regex), and
  flags identities (mission file paths) where two distinct annotation
  values appear.

  Sub-choice: implicit-default = mission file's own filesystem repo.
  An annotation merely makes the implicit explicit (or overrides
  with a deliberate cross-repo claim). Missing annotation is NOT a
  violation.

  Sub-choice: per Joe's "future-proofed needs an instance" directive,
  the apparatus ships with three concrete annotated inhabitants
  (M-single-locus, M-archaeology-control, M-bounded-disposition all
  carry `Home-repo: futon3c`). Live system has 109 missions with 3
  explicit and 106 implicit homes — non-trivial discrimination.

  Sub-choice: regex tolerates markdown emphasis (`**Home-repo:**
  futon3c`) since mission files use this style. Avoids forcing a new
  documentation convention.

alternatives:
  - **Detect via cross-references in mission body.** Reject; too
    heuristic. Annotation-based is unambiguous and operator-driven.
  - **Treat missing annotation as violation.** Reject; would force
    annotation on 100+ missions for no signal. Implicit-default is
    correct for most missions.
  - **Reuse `archaeology.clj` namespace.** Reject; archaeology is
    already 900+ lines and exhibits a different shape. Separate
    namespace per shape keeps the family/shape boundary visible.

outcome (target):
  - `single-locus/mission-home` registered as probe-tap + boot-time
    check; `:status :operational-when-enabled` in inventory.
  - At least 3 explicit annotations live in mission files.
  - Codex handoff opens for `single-locus/agent-routing` +
    `single-locus/artifact-live-copy`.
  - The algorithm trial run produces a complete trace (mission, PSR,
    PUR, code, tests, inventory, handoff issue, checkpoint).

confidence: high.
  Pattern lineage is explicit (mirrors archaeology load-time +
  registration patterns; cardinality test is universal). The only
  novelty is the `single-locus` shape itself, which has a documented
  pattern with exemplar table for future siblings.
