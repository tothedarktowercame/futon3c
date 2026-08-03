# Memory: record consultation discards with reasons

- Requested memory level: `process`
- Lane: desk-research
- Confidence: three discards in one session (`n=1` session)
- Problem: `a96J07`
- Git commit: `462b48a3e1047b0fec1fa13436cee9391236599d`
- Job: `invoke-1785772570109-947-08373761`
- Evidence IDs: `e-6822cd1c-1cef-4dbd-ac5b-4a0a627b4e43`,
  `e-51f2dd3a-b182-46a3-87c9-fe4f40adc263`
- Consultation-log items: 6, 9–11

## Memory

For each consulted resource that is not used, record the concrete mismatch
rather than omitting it or manufacturing relevance. This keeps desk research
bounded and makes route selection auditable.

The a96J07 consultation log records three distinct discards:

1. Mathlib's packaged Liouville theorem was applicable to bounded entire
   functions, but using it would not derive Liouville from the two-pole result
   required by the problem.
2. a96J04's fresh open-component/rational-witness decomposition was genuinely
   nearby in the day's work, but it solves a real open-set decomposition
   problem and supplies no contour machinery.
3. Mathlib's `atTop` quotient-limit API could formalize `R→∞`, but the direct
   choice `R = ‖a‖ + (M/ε + 1)` produced a shorter proof with a smaller API
   surface.

## Application rule

Log `resource → returned fact → contract fit/mismatch → used/discarded`. A
discard is successful research when its reason narrows the proof route. Do not
claim a resource is absent or bad merely because a more direct route won.

## Boundary

All three examples come from one session (`n=1` at the process level). They
show an auditable discipline instance, not evidence that logging discards
always reduces proof time.
