# The Single-Boundary Pattern

A reusable recipe for any write-class in the futon stack. Documents the
discipline that landed in `futon3c.evidence.boundary` (M-invariant-queue-unstuck,
2026-04-29) and the conditions under which it generalizes.

## Statement

For any write-class (evidence appends, bell receipts, agent registrations,
mission state transitions, etc.), there should be **exactly one function
that performs the write**, and **all other code calls it**. The boundary
function:

1. Coerces commonly-misshaped inputs to the canonical shape **before**
   validation.
2. Validates against the shape contract.
3. Performs the write through the underlying store/transport.
4. Verifies durable persistence (read-back through the same backend) for
   the writes that admit verification.
5. Returns a structured delivery-receipt — `{:ok bool ...}` — that
   callers can destructure.
6. Surfaces structured **VIOLATION** lines on stderr for every failure
   mode.

A **static check** — typically a grep that runs in CI — confirms that
no direct call to the underlying store function survives outside the
boundary's namespace. This static check IS the I-single-boundary
invariant for that write-class.

## When to apply

Use this pattern when:

- A write operation has multiple call sites across a codebase (more than
  ~5).
- The shape contract for the written object is non-trivial (anything
  beyond a primitive or a flat string-keyed map).
- Failures of the write should be observable rather than silent.
- You expect the population of call sites to grow over time.

The cost of a single boundary is N+1 lines (the boundary function +
import in each caller); the cost of NOT having one scales as
*sites × failure-modes*. The former is constant; the latter is
quadratic-ish. Past 5 sites, the boundary almost always pays.

## Recipe (in 6 steps)

1. **Identify the underlying primitive.** What is the single function
   that actually performs the write? `store/append*`, `bell/send!`,
   `registry/register-agent!`, etc. Whatever it is, the boundary will
   wrap it.

2. **Identify the shape contract.** What does the written object have
   to satisfy? Find the validator. (In futon3c: `social.shapes`.)

3. **Identify the recurring shape mismatches.** Read existing call sites
   and find the common ways callers get the shape wrong. (Evidence's
   case: strings instead of keywords for `:tags`, `:subject :ref/type`,
   etc.) These become the boundary's coercion responsibilities — the
   boundary fixes them automatically rather than rejecting them.

4. **Build the boundary.** A namespace `<domain>.boundary` with one
   `<verb>!` function (the public API) plus per-field coercion helpers
   (private). The function:
   - Calls coercion (any unrecoverable shape throws, returning a
     structured violation).
   - Calls the underlying primitive.
   - Verifies persistence if applicable.
   - Returns `{:ok true ...}` or `{:ok false :error/code ... :invariant/violation ...}`.
   - Prints `[boundary] I-single-boundary VIOLATION:` on any failure.

5. **Refactor the call sites.** Highest leverage: any helper that ~most
   callers use (e.g. `peripheral.common/maybe-append-evidence!`). One
   delegation rewrite there auto-routes many indirect callers. Then
   per-site sweeps for the remaining direct callers. Each conversion
   is mechanical for fire-and-forget callers; per-site-careful for
   callers that destructure the return value.

6. **Add the static check.** A grep (or clj-kondo rule, or pre-commit
   hook) that fails if any direct call to the underlying primitive
   appears outside the boundary's namespace. This is the canary.

## Lineage

This pattern is exactly what `futon1a/core/pipeline.clj/run-write!` does
for entity / relation / hyperedge writes — every write goes through one
function, every layer's invariants are checked once, failures abort the
write loudly. futon1a's invariants have stayed rock-solid for months
because of this discipline. The evidence-write boundary in futon3c is
the same discipline applied to a different write-class.

The pattern composes with companion patterns:

- **agency/single-routing-authority** (`futon3/library/agency/`) —
  generalises beyond writes to "for any responsibility, exactly one
  owner."
- **agency/loud-failure** (`futon3/library/agency/`) — every failure
  surfaces; no silent catch-and-swallow.
- **agency/delivery-receipt** (`futon3/library/agency/`) — return value
  is a structured receipt.
- **storage/durability-first** (`futon3/library/storage/`) — verify
  persistence before declaring success.

Apply all four as accompanying patterns when boundary-izing a new
write-class.

## What this pattern is NOT

- **Not a workflow facade.** A boundary is not a wrapper that hides
  complexity by stuffing several operations into one call. It is a
  single-source-of-routing for one specific write.
- **Not a replacement for the shape contract.** The shape contract
  remains the source of truth; the boundary's coercion is a service to
  callers, not a permission to be sloppy. Coercions are bounded to the
  small set of recurring mismatches; unknown shapes throw.
- **Not a place to add business logic.** The boundary is plumbing.
  Decisions about *whether* to write or *what* to write belong in
  callers; the boundary handles *how* to write correctly.

## Existing instances

| Domain | Boundary | Underlying primitive | I-* invariant |
|---|---|---|---|
| evidence (futon3c) | `futon3c.evidence.boundary/append!` | `store/append*` | I-single-boundary, I-evidence-per-turn |
| entity / relation / hyperedge (futon1a) | `pipeline.clj/run-write!` | various low-level KV ops | L0–L4 stack |

## Plausible follow-on instances

- bell receipts (futon3c)
- agent registration (futon3c)
- mission state transitions (futon3c)
- gate traversals (futon3b)

Each costs one mission's worth of work. Each one that lands extends
the projection apparatus's coverage of structural-law-relevant writes.

## Mission cross-reference

The pattern was extracted from M-invariant-queue-unstuck
(`futon3c/holes/missions/M-invariant-queue-unstuck.md`), specifically
INSTANTIATE-1 and INSTANTIATE-2. The mission's PSR/PUR for
single-routing-authority is at
`futon3c/holes/labs/M-invariant-queue-unstuck/psr|pur/2026-04-29__*single-routing-authority.md`.
