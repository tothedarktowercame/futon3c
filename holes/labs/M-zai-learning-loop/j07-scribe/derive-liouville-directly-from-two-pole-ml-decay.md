# Memory: derive Liouville directly from two-pole ML decay

- Requested memory level: `strategy`
- Lane: math
- Confidence: single compiled problem (`n=1`)
- Problem: `a96J07`
- Git commit: `462b48a3e1047b0fec1fa13436cee9391236599d`
- Job: `invoke-1785772570109-947-08373761`
- Evidence IDs: `e-6822cd1c-1cef-4dbd-ac5b-4a0a627b4e43`,
  `e-51f2dd3a-b182-46a3-87c9-fe4f40adc263`
- Consultation-log items: 3, 6–10, 13–14

## Memory

When a problem specifically asks to derive Liouville's theorem from a
two-pole contour identity, do not close the goal with the packaged Liouville
theorem. Fix `a ≠ 0`, put the second pole at zero, and normalize by `2πi`.
On the radius-`R` circle,

`R - ‖a‖ ≤ ‖z-a‖` and `‖z‖ = R`,

so the circle ML estimate simplifies to

`‖(2πi)⁻¹ ∮ f(z)/((z-a)z) dz‖ ≤ M/(R-‖a‖)`.

The two-pole evaluation makes the left side
`‖(f(a)-f(0))/a‖`. For `ε>0`, take
`R = ‖a‖ + (M/ε + 1)`; the bound is at most `ε`. The quotient has norm zero,
hence `f(a)=f(0)`. Handle `a=0` separately.

## When and why

Use this route when the derivation from the requested contour result is itself
part of the theorem contract, or when a difference-quotient proof is desired.
The specialization to a zero pole cancels one factor of `R` and avoids a
separate general two-parameter rational-limit lemma.

## Boundary

This is one compiled instance (`n=1`). The strategy assumes a global constant
norm bound and a two-pole identity for arbitrarily large circles. It does not
supersede Mathlib's packaged Liouville theorem when the derivation is not part
of the requested content.
