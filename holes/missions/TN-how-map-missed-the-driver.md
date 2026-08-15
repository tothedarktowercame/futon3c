# TN — How MAP missed the driver

**Written** 2026-08-15 by claude-2, answering Joe: *"how did we get past MAP in
M-apm-demonstration and not discover this?"*

Not a memory failure and not a length problem. The mechanism is visible in the
mission file, in one table, and it is a **scoping decision that was locally
reasonable and globally fatal.**

## The register was split into certified and uncertified

`M-apm-demonstration.md:213-219` sorts the nine N-nodes into two columns:

| certified (scaffolding) | uncertified (the thesis) |
|---|---|
| N1 extra resources fill Mathlib holes — n=3 | **N5 retrieval serves the need — weak** |
| N2 work transports between agents — n=4 | **N6 transports to held-out — designed** |
| N3 the store records learning — n=4 | **N7 outcomes mechanically scoreable — designed** |
| N4 agents consult when instructed — n=1 (ctrl) | **N8 learns at ability level — designed** |
| N9 the pipeline runs — n=2 | |

The reasoning given is good: the certified column holds *existence* claims,
which need no denominator; the uncertified column holds *rate* and *trend*
claims, which cannot be stated without one. MAP is about denominators. So MAP
went where the denominators were.

## MAP then mapped only the right-hand column

MAP's stated exit condition (`:346`):

> every row of the N-register (§1.4) resolved to *already-done / partly-done /
> greenfield*, with evidence

"MAP consolidation — the register is resolved" (`:1050`) declares that criterion
**met**, on a table with **four rows: N5, N6, N7, N8.**

Measured across MAP's whole span (lines 344–1344):

    N1 mentions: 0    N2: 0    N3: 0    N4: 0    N9: 0

Five of nine nodes were never resolved to already-done / partly-done /
greenfield. The register was declared resolved at 4/9.

## Why that specific five is the whole problem

**The certified column is the column where an implementation already exists.**
A node earns an inductive warrant by something having *run*. N9 — "the pipeline
runs, n=2" — is the apm-driver: 8,571 lines of Python, two witnessed chains, 213
problems audited axiom-clean, still running today. N1–N4 are warranted by that
same driver's chains.

So the filter MAP applied — *skip what is already certified, map what is weak* —
selected precisely against the artifacts that were already built. **Strong
warrant is a proxy for working code, and MAP treated strong warrant as a reason
not to look.**

Had N9 been resolved, its verdict could only have been **already-done**, with
`gates.py` and `driver.py` as the evidence. That single row is the one that would
have said: the closing half of this cycle exists, do not rebuild it.

## MAP reached the right conclusion anyway, and it was dropped

Line `:1109`, answering "can cycle one be countable?":

> **Not with the currently assembled path.** Not greenfield either — **the
> components exist; five bindings are missing, and none is a build.**

That is tonight's finding, written a day early, by MAP, in MAP's own voice. The
string *"five bindings"* occurs **once** in 9,204 lines. It was never carried
into DERIVE, never restated at the DERIVE→ARGUE gate, and never appeared in
INSTANTIATE, which then built a second machine.

An exit condition met at 4/9 and a load-bearing sentence with no downstream
citation are the same defect the mission diagnoses in its own subject matter at
`:1090` — *"Documents assert what the code and data do not"* — and the same one
this session has hit all day: **a check whose shape passes while its content is
absent.** The mission caught it in the corpus, in the ledgers, in the
registration pins, and in `status.json`. It did not run the check on itself.

## What would have caught it

Not more context and not a longer REPL. One mechanical assertion at the MAP exit
gate: *the resolved-register table has as many rows as §1.4 has nodes.* That is
`4 ≠ 9`, computable, and it is the same class of guard as F1 — prefer an
invariant that holds by construction over a criterion a reader confirms by
reading.
