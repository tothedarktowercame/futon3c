# Memory: reuse a partial-fraction contour pattern across problems

- Requested memory level: `tactic`
- Lane: math
- Confidence: two compiled problem instances (`n=2`) for the underlying pattern
- Problems: `a92J06`, `a96J07`
- Origin commit: `146de41665ed60d3e0a5a43709e194e50c9c9e0d`
- Reuse commit: `462b48a3e1047b0fec1fa13436cee9391236599d`
- Job: `invoke-1785772570109-947-08373761`
- Evidence IDs: `e-6822cd1c-1cef-4dbd-ac5b-4a0a627b4e43`,
  `e-51f2dd3a-b182-46a3-87c9-fe4f40adc263`
- Consultation-log items: 4, 5, 7, 8

## Memory

For a rational contour integrand whose denominator splits into distinct linear
factors, search prior solved contour problems before rebuilding the analytic
machinery. The reusable Lean pattern is:

1. prove each pole is off the contour;
2. establish `CircleIntegrable` for each Cauchy-kernel term from continuity on
   the sphere;
3. prove the partial-fraction identity pointwise under the nonvanishing facts;
4. use `circleIntegral.integral_congr`, constant multiplication, and integral
   subtraction to expose separate kernels;
5. discharge each kernel with the appropriate circle Cauchy formula (or the
   exterior-pole zero theorem).

In a92J06 this evaluates a quadratic-denominator trigonometric contour with one
interior and one exterior pole. In a96J07 the same proof skeleton evaluates
`f z / ((z-a)(z-b))` with both poles interior. The second application cited the
first in the committed source.

## Confidence and boundary

The `n=2` claim is for the underlying partial-fraction/Cauchy-kernel proof
pattern, witnessed by two compiled, zero-sorry artifacts. It is not a claim
that the two theorem statements or pole configurations are identical, nor
that all rational contour integrals reduce this way.
