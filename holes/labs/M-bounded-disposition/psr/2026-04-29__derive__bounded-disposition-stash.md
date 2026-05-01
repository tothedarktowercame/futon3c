# PSR: bounded-disposition/stash — first sibling

context:
  M-bounded-disposition IDENTIFY surfaced the bounded-disposition shape
  by reading rank-2 and rank-3 candidates (`recover-or-drop` +
  `stash-debt-bounded`) together as two obligations of one shape:
  per-artifact disposition + bounded undecided population. The first
  worked sibling is the stash slice.

patterns considered:
  - **invariant-coherence/bounded-disposition** (just authored) — the
    primary shape (NEW). Two-obligation invariant, distinct from
    subsumption-witness. **Selected as primary.**
  - **invariant-coherence/shape-first-identify** — methodology;
    surfaces the shape; led to the absorption rather than literal-
    sibling implementation. **Selected.**
  - **invariant-coherence/protocol-family-naming** — namespace IDs
    `bounded-disposition/<artifact-class>`. **Selected.**
  - **archaeology-control / obsolescence-recognition** (M-archaeology-
    control precedent) — same family, different shape. The existence
    of two shapes under one family is the methodological payoff Joe
    flagged. **Selected as accompanying.**

decision:
  Build `check-stash-disposition` as a sibling factory in
  `futon3c.logic.archaeology` next to the three obsolescence-recognition
  factories. The check enumerates `git stash list` per repo, parses
  each stash's message for a `[disposition: ...]` tag (case-insensitive),
  and aggregates per-repo violations on two axes (count of awaiting,
  age of unclassified-old).

  Sub-choice: vocabulary `{:kept :parked-on-branch :dropped
  :awaiting-decision}` with `:awaiting-decision` as default. Bound:
  `|undecided| <= 5` per repo AND no awaiting stash older than 14
  days. Both configurable at registration time.

  Sub-choice: absorb `recover-or-drop` and `stash-debt-bounded` into
  the new sibling rather than keep them as separate invariants.
  Reason: they're the two obligations of one shape; expressing them
  separately defeats the methodology. The ratchet treats the rename as
  `:removed` + `:added` (informational, not demotion).

  Sub-choice: stash-message tag is the disposition source (rather than
  a sidecar file or git-config). Reason: tags survive
  `git stash show / pop / apply / drop` because they live in the
  message itself. No extra coordination surface.

alternatives:
  - **Two literal siblings** mirroring the inventory's
    `recover-or-drop` and `stash-debt-bounded`. Rejected: produces a
    god-pair where neither sibling is meaningful alone (per-artifact
    classification without bound is too lax; bound without
    classification is too crude).
  - **A sidecar file (.git/futon-stash-dispositions.edn)** for
    disposition records. Rejected: stash messages survive normal git
    operations; sidecar files create an out-of-band coordination
    surface that drifts from stash existence.
  - **Auto-classify based on stash creation context** (e.g. autostash
    → `:dropped`-after-merge). Rejected: collapses the discipline back
    into count-cap-only failure mode. Disposition records should
    reflect operator intent.
  - **Building all three siblings (stash + branch + mission-doc) in
    one go.** Rejected per scope discipline; first sibling lands as
    worked example, others follow via Codex handoff.

outcome (target):
  - `bounded-disposition/stash` registered as probe-tap, returning
    `:outcome :ok` on the live system (no current stashes anywhere).
  - Inventory: removed two narrow candidates, added one richer sibling
    at `:status :operational-when-enabled`.
  - Codex handoff opens for `bounded-disposition/branch` and
    `bounded-disposition/mission-doc` (the other artifact-classes from
    bounded-disposition.flexiarg's exemplar table).

confidence: high.
  Pattern lineage is well-trodden — mirrors the
  `check-autostash-obsolescence` / `check-deferred-stub-obsolescence`
  shape from M-archaeology-control. The only novelty is the bounded-
  disposition shape itself, which has a documented pattern with
  exemplar table for backwards-application.
