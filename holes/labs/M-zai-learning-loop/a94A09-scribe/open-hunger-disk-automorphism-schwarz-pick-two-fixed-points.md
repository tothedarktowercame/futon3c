# Open hunger: disk automorphism / Schwarz–Pick / two fixed points

- Requested memory level: open-hunger
- Lane: desk research / missing dependency
- Confidence: `n=1` unmet query, status open
- Problem: `a94A09`
- Job: `invoke-1786369654355-3517-0994cab1`
- Commit: `22c5b80c064ae36e83a3b8759607ccf430c76169`
- Evidence: `e-fab2e3d9-6877-444a-9949-a11720305918`,
  `e-memory-outcome-sweeper-6e8a041ab7506a025951c3b4`

## Hungry query

Literal vocabulary: **disk automorphisms Schwarz-Pick two fixed points**.

At the uniqueness stage, the runner searched the Zulip archive for a packaged
unit-disk Möbius automorphism, Schwarz–Pick theorem, or two-interior-fixed-point
rigidity bridge and found no relevant result. The search was not reported as
degraded under load.

## What was sought

For `z` in the open unit disk, formalize

```text
phi_z(u) = (u - z) / (1 - conj z * u)
```

and its inverse as differentiable self-maps of the disk. Conjugating a disk
self-map fixing `z` should produce a map fixing zero. A second interior fixed
point then forces equality in Schwarz's lemma, making the conjugate the
identity; the original no-boundary-fixed-point hypothesis rules that out.

## Current boundary

The concept was not grounded later in this chain. Mathlib's nearby Möbius
actions concern the upper half-plane, and no importable unit-disk package or
Schwarz–Pick fixed-point uniqueness theorem was identified. This is a
revision-scoped open demand, not a claim that the result is permanently
unroutable. A future grounded memory should supersede this entry and inherit
the literal tags `disk`, `automorphisms`, `Schwarz-Pick`, `two`, `fixed`, and
`points`.
