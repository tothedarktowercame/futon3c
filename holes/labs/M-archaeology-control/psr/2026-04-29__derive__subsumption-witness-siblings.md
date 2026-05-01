# PSR: Subsumption-witness siblings under archaeology-control

context:
  M-archaeology-control IDENTIFY surfaced the subsumption-witness shape
  (artifact A is obsolete relative to canonical record P). Three sibling
  invariants under family `archaeology-control` need check-fns:
  `obsolescence-recognition/{autostash,deferred-stub,pipeline-tracer}`.
  All share the probe-result shape `{:outcome :ok|:violation :detail}`,
  so they slot directly into `futon3c.logic.probe`'s family-check-fns
  registry without apparatus changes.

patterns considered:
  - **invariant-coherence/subsumption-witness** (just authored) — the
    primary shape. Each sibling implements three components: enumerate-A,
    locate-P, subsumption-test. **Selected as primary.**
  - **invariant-coherence/protocol-family-naming** (just authored) —
    namespace-discriminated IDs `<shape>/<instance>` rather than new
    structural elements. **Selected for inventory naming.**
  - **invariant-coherence/shape-first-identify** (just authored) —
    methodology: build all three siblings together rather than picking
    a "first." **Selected — exhibit the shape across all three forms.**
  - **agency/single-routing-authority** (M-invariant-queue-unstuck
    precedent) — every emit through one boundary. The probe taps emit
    through `boundary/append!` already (via the canary apparatus); no
    new emit paths needed. **Selected as accompanying.**
  - **storage/durability-first** — verify-persisted before declaring
    success. The canary apparatus already binds this; new check-fns
    inherit the binding. **Selected as accompanying.**

decision:
  Build `futon3c.logic.archaeology` as a sibling of
  `futon3c.logic.probe-taps`, with three check-fn factories:

    - `check-autostash-obsolescence repo-paths` — shells out to git per
      repo; uses `git stash show -p N | git apply --reverse --check` as
      the subsumption test. Real stash-tree subsumption.
    - `check-deferred-stub-obsolescence inventory-path` — walks
      `family-check-fns`; for each fn returning `:deferred? true`,
      cross-references the structural-law inventory for `:status
      :operational` matches. Inventory-as-canonical-record proxy.
    - `check-pipeline-tracer-obsolescence` — queries evidence by tag for
      open vs closed `:pipeline-tracer-item` entries; flags open with
      matching close OR past target-date with no close.

  Plus `register-archaeology-control-taps!` mirroring the existing
  `register-default-taps!` / `register-deferred-taps!` activation
  pattern.

  Sub-choice: pre-commit hook for the autostash slice only —
  `scripts/check-autostash-obsolescence.sh` mirroring the existing
  `check-coverage-ratchet.sh`. The strong-mode binding gives a real
  computational guarantee: a commit can't land while obsolete
  autostashes remain. The other two siblings stay probe-only.

  Sub-choice: namespace IDs in the inventory. The existing
  `:invariant id obsolescence-recognition` is renamed to
  `:obsolescence-recognition/autostash` (carrier of the original
  intent) and two new sibling entries are added with `:status
  :operational-when-enabled`. The rename is an ID change, not a status
  decrease — the coverage-ratchet treats it as :added + :removed,
  not as a demotion.

alternatives:
  - **One god-function `is-obsolete?(artifact)`** — rejected; per-class
    edge cases would balloon. The probe shape gives us natural per-
    class isolation.
  - **Auto-cleanup on detection** — rejected; conflates detection with
    cleanup. `futon0/scripts/futon-sync.clj` keeps its operator-driven
    role; the invariant tells you when to run it.
  - **A new structural element ("subfamily") in the inventory** —
    rejected per `protocol-family-naming` pattern. Namespace IDs
    suffice; schema bloat avoided.
  - **Pick a "first" sibling and defer the others** — rejected per Joe's
    "exhibit it in any of these forms, ideally all of them." Building
    all three together also tests the namespace pattern end-to-end.

outcome (target):
  - Three sibling check-fns under `archaeology-control` registered as
    probe taps; each callable with the standard probe-result shape.
  - Inventory carries three `:operational-when-enabled` entries with
    namespace-discriminated IDs.
  - Pre-commit hook for autostash slice; `chmod +x` and documented
    activation via symlink.
  - Tests cover the empty-input, the violation-input, and the
    cross-check edge cases per sibling. Mission test sweep clean.

confidence: high.
  All three check-fns build on apparatus that already exists and is
  bound (`boundary/append!`, `family-check-fns`, evidence query). The
  subsumption tests are straightforward (git plumbing, registry
  inspection, evidence-tag query). The risk is in the inventory rename
  (:obsolescence-recognition → :obsolescence-recognition/autostash) —
  if downstream tools break on namespace-discriminated IDs, we'll find
  out at the next probe sweep or arxana view refresh.
