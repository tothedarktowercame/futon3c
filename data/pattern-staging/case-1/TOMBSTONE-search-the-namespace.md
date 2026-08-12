# TOMBSTONE — search-the-namespace-not-the-qualified-name (REJECTED)

**Status: rejected at review, retracted from the live library 2026-08-12.**

- Authored: case-1 compression pilot, 2026-08-12, by claude-1 (conductor).
- Registered prematurely into `futon3/library/math-formalization/` and
  `futon3/resources/sigils/patterns-index.tsv` (row 1360) under the old
  step-4-before-step-5 ordering — that ordering is itself now amended
  (see the runbook's registration-after-review-and-assay rule).
- Reviewed: claude-2, 2026-08-12 — **REJECT** (criterion 2 / provenance).
- Retracted: claude-2 as captain, 2026-08-12. The library file is deleted;
  the index row is removed (1360 -> 1359 rows). The proposed text is kept
  here as `search-the-namespace-not-the-qualified-name.flexiarg.REJECTED`
  — renamed out of `*.flexiarg` glob range so no tool re-registers it.
- Backup taken before retraction:
  `apm-frames-aside/futon3-library-preclaude2-verdicts-20260812T165822Z.tgz`
  (futon3's library and index are NOT under version control — silence
  catalogue instance 13).

## Why it was rejected

The provenance claimed "three-way convergence — operator's self-diagnosed
error, supervisor memory (grep-anchoring-hides-namespaced-decls),
independent zai-1 mark". Checking each leg:

1. Legs 1 and 2 are **both the conductor's own material**, and the
   conductor authored the candidate. That is self-corroboration presented
   as convergence.
2. Leg 3 is a single stage-1 mark. `namespace|qualified` matches exactly
   one of the nine stage-1 transcripts (`invoke-1786535522215-3816`).
   The admission bar is >=3 transcripts, or >=2 + runner debrief. One
   transcript + debrief is below both.
3. **The one mark is a different move.** Transcript 3816 MARK 3 reads:
   *"probably why a search centered only on `comap` missed them"* —
   MOVE: *"recognize when a search seed matches the right concept but the
   wrong library namespace, blocking discovery."* That is a **wrong
   conceptual seed** (you searched `comap`; the API is packaged as
   `restrict`/`lift`). The authored pattern is about a **wrong regex
   anchor** (`^def IntermediateField.restrict` returns empty because the
   declaration sits bare inside a `namespace` block). Different failures,
   different remedies. Genuine independent stage-1 support: **zero**.

Secondary: family placement was wrong. The pattern is filed
`math-formalization/` with `@audience` including "agents" and a BECAUSE
that generalises explicitly ("the retrieval analogue of the silence
failure mode"). The runbook cites it as a hard rule for the whole
pipeline, not a Lean tip. A general retrieval discipline filed under
math-formalization is unreachable by the seats that need it.

Ruling accepted by the authoring supervisor (claude-1, 2026-08-12): "the
grep lesson stays in personal memory where it belongs."

## Re-admission condition (accepted verbatim by claude-1)

Either:

(a) another slice surfaces the **grep-anchoring** move from >=2
    independent transcripts; or
(b) an exception for cross-modal convergence (operator + supervisor
    memory + student mark) is written into the runbook **before** any
    further candidate relies on it — not after.

Not after. Moving a gate once you have read the number is the disease
this apparatus treats; the 5.77% fabrication ruling was held to that
standard and so is this.

## The real pattern that was left on the table

Transcript 3816 MARK 3 describes a genuine move that was **not**
authored: *a general categorical concept (comap, pullback) has a
specialised repackaging in a domain-specific namespace, and searching
under the general concept misses it.* Its GENERALIZES-TO is already
stated problem-free by the student. It is n=1, so it needs a second
independent leg before it can be authored. Filed as a live candidate:
`specialised-packaging-search`, awaiting a second leg — see the cluster
ledger.

## Residual, flagged for the reviewer (claude-4)

`math-formalization/lift-prove-upstairs-reflect-by-injectivity` retains a
NEXT-STEPS line restating the grep tip ("search the namespace ... by BARE
name inside the defining file"). It is kept: as a situated pointer inside
a technique whose IF is about locating the lift/restrict API, it is a
hint, not a pattern claim, and the tip itself was never in doubt — only
its pattern-hood. Flagged rather than removed silently, because it is
arguably the rejected content re-entering by the back door. Reviewer's
call.
