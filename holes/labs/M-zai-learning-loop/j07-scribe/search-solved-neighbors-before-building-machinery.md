# Memory: search solved neighbors before building machinery

- Requested memory level: `process`
- Lane: desk-research
- Confidence: single documented payoff (`n=1`)
- Problem: `a96J07`
- Git commit: `462b48a3e1047b0fec1fa13436cee9391236599d`
- Job: `invoke-1785772570109-947-08373761`
- Evidence IDs: `e-6822cd1c-1cef-4dbd-ac5b-4a0a627b4e43`,
  `e-51f2dd3a-b182-46a3-87c9-fe4f40adc263`
- Consultation-log items: 4–8, 11–12

## Memory

Before writing new formal machinery, search compiled neighboring problems by
the mathematical family and key API terms, then open only the promising hits.
For a96J07, a repository-wide search over `problems/*/lean/*.lean` for circle
integrals and Liouville located a92J06. Reading it supplied the exact compiled
partial-fraction, denominator-nonvanishing, circle-integrability, and integral-
linearity skeleton used in the new proof. The committed a96J07 source cites
that reuse.

The cheap sequence is:

1. search filenames and source text broadly enough to find nearby statements;
2. inspect the smallest promising set in detail;
3. verify the candidate is compiled/current before copying its proof shape;
4. cite the reused artifact in the new source and record the origin/reuse
   chain.

## Boundary

This is one session with one decisive cross-problem payoff (`n=1` for the
process practice). The mathematical tactic it found has `n=2`, but that does
not establish a general completion-rate benefit for neighbor-first search.
Empty or irrelevant searches should remain cheap and should not be forced into
the proof.
