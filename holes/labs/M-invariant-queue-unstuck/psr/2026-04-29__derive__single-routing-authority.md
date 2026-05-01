# PSR: Single-routing-authority for evidence appends

context:
  All evidence appends in futon3c currently go through ~30 direct call sites of `futon3c.evidence.store/append*`. Each call site is responsible for shape correctness; each can fail silently if the shape is wrong (the validator returns a SocialError map; most callers don't check). The bot-evidence bug (M-peeragogy-rewrite) and the partial fix attempted in `dev/futon3c/dev/invoke.clj` (uncommitted) are concrete instances. The boundary discipline that has kept futon1a's L0–L4 invariants rock-solid (`pipeline.clj/run-write!` is the only path) is missing here.

patterns considered:
  - `futon3/library/agency/single-routing-authority.flexiarg` — exactly the pattern that names "for any agent-id, exactly one routing entry exists at any time" applied to writes: for any write-class, exactly one boundary owns it. **Selected.**
  - `futon3/library/agency/loud-failure.flexiarg` — complements the above: failures must surface; no silent catch-and-swallow. Already operational in the existing store via `social-error :invalid-entry`, but most callers don't propagate. **Selected as accompanying pattern.**
  - `futon3/library/agency/delivery-receipt.flexiarg` — every send returns a receipt or explicit failure. Adopted as the boundary's return-shape contract. **Selected as accompanying pattern.**
  - `futon3/library/storage/durability-first.flexiarg` — appends must be durable before counting as success. Maps onto `verify-persisted` which is already in `futon3c.evidence.invariant`. **Selected as accompanying pattern.**

decision:
  Implement `futon3c.evidence.boundary` as the single routing authority for evidence appends. All future emits go through `boundary/append!`. The existing `peripheral/common/maybe-append-evidence!` is preserved as a back-compat alias that delegates to `boundary/append!` (so callers don't all need to refactor at once). New direct callers of `store/append*` are forbidden by the I-single-boundary check (a static grep that runs in CI).

  Sub-choice: shape-coercion happens at the boundary, not at each caller. Coercions: string→keyword for `:evidence/tags` items, `:evidence/subject :ref/type`, `:evidence/type`, `:evidence/claim-type`, `:evidence/pattern-id`. Unknown coercions throw (loud failure: an unknown shape mismatch is a bug, not silent best-effort).

  Sub-choice: the boundary is synchronous and returns a result map. Async wrapping (the previous `(future ...)` shape in `emit-invoke-evidence!`) is rejected — the agent's earlier partial fix already correctly chose synchronous; we keep that.

alternatives:
  - Refactor each of the ~30 call sites independently to do shape coercion in place. Rejected: that's the failure mode the agent's partial fix took, leaving 28 sites still broken. Single boundary is the pattern.
  - Loosen the EvidenceEntry shape to accept strings or keywords for tags / ref-types. Rejected: shape-strictness is what makes invariant violations detectable; loosening it makes the system less observable, not more.
  - Have each peripheral install its own per-peripheral mini-boundary. Rejected: this is what produced the partial-coverage problem in the first place.

outcome (target):
  - All 30 existing direct callers of `store/append*` route through `boundary/append!` (refactor batched, separate INSTANTIATE step).
  - The boundary produces evidence entries that pass shape-validation and are durably persisted on success; loud, structured failure on shape rejection or non-readback.
  - I-single-boundary holds: `grep -rn 'estore/append\*' src dev | grep -v 'evidence/boundary'` returns zero matches.
  - I-evidence-per-turn binds at the boundary, not at 30 places.

confidence: high.
  The pattern is exactly what futon1a's pipeline.clj already implements for entity / relation / hyperedge writes. The stack has months of evidence that the discipline works. The risk is in the audit/refactor of the 30 call sites — each could have its own shape quirks not covered by the boundary's coercion. Mitigation: each call site converted in a separate small commit, with a regression test that exercises the converted call site.
