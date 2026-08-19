# TN — the audit IS the proof attempt, so make proof attempts cheap

**Joe, 2026-08-19:** *"The best way to find errors is to try to prove the
theorems, I think they have all been audited superficially once. This is one good
reason to automate as much as possible of the cycle machine so that we, e.g.,
allocate seats as needed rather than all at once."*

This supersedes my suggestion of a formalisation audit as a separate activity.
The evidence is on his side: every problem in the pool has had a superficial
audit, and all three defects found this week survived it.

| frame | problem | defect | found by |
|---|---|---|---|
| f11 | t01A05 | statement provably FALSE | a solver trying to prove it |
| f12 | m03J01 | statement VACUOUS (`ContDiff ℝ ⊤` = `ω`) | a solver trying to prove it |
| f13 | m99J06 | model INCONSISTENT | a solver trying to prove it |

In two of the three the diagnosis was already written down somewhere in the
corpus — a01A03's file for f12, m99J06's own docstring for f13 — and a reading
pass still missed it. A fourth reading pass would find a fourth nothing. **The
proof attempt is the audit instrument; there is no cheaper one.**

So the question is not "audit or next frame". It is: **how cheaply can we run a
proof attempt?**

## Measured waste in the current frame shape

Seats minted per frame: 5. Seats actually dispatched to, from the persisted
cycle state of every frame in the series:

    f9  a01J06   solver ✓  student ✓  scribe ✓  proctor ✗
    f10 m93J02   solver ✓  student ✓  scribe ✓  proctor ✗
    f11 t01A05   solver ✓  student ✓  scribe ✓  proctor ✗
    f12 m03J01   solver ✓  student ✓  scribe ✓  proctor ✗
    f13 m99J06   solver ✓  student ✗  scribe ✓  proctor ✗

**The proctor seat has never been dispatched to. Five frames, five minted
proctors, zero uses.** And under the stop-the-line protocol the student is waste
in every frame that finds a defect — which is three of the last three.

## Two changes, in the order they pay

### 1. PROBE MODE — a frame stripped to one arm

A probe is: mint a guide and a solver, dispatch once against the frozen
statement, require the inhabitation/non-vacuity witness as the first
deliverable, take the reply, stop. No student, no scribe, no promotion, no
adjudication, no analyst.

Its only question is **"is this problem sound?"**, and its answer is one of
`sound | defective | undetermined`. A problem that survives a probe earns a full
frame; one that fails goes to repair.

This is what f13 actually was, after the stop-the-line rule was applied to it —
and f13 reached its answer on the FIRST solver dispatch, in about seven minutes.
The rest of the frame apparatus contributed nothing to that answer.

### 2. LAZY SEAT ALLOCATION

Mint on first use, not at registration:

- **guide** at open (the only seat `open!` genuinely requires — it reads the
  guide's session from the registry);
- **solver** at first `dispatch-solver`;
- **student** only if the cycle reaches `:student-attempts`;
- **scribe** at first `dispatch-scribe`;
- **proctor** never, until something actually dispatches to it — on five frames
  of evidence, that is never.

This also removes a class of defect rather than just cost. Every seat minted up
front is a registry row that has to be kept consistent across restarts, and
`62bfc210` ("enforce singular ownership of Codex sessions") is someone else
fixing problems in exactly that area today.

## The third change, which is where the defects actually came from

Registrations should be **generated from their predecessor plus an explicit
diff**, not copied and edited. Every defect in D50 and D56 — six repaired this
morning, two more found by f13-guide's orientation ack — has the same shape: the
structured fields were correct and the PROSE was the previous frame's. A
generated registration cannot say "analyst-2" when the field says "analyst-3",
and cannot claim an elaboration check of the wrong problem.

That is not automation for throughput. It is automation to remove the single
most productive source of defects in the series so far, which is me authoring
prose by hand.

## What this does NOT change

Author ≠ reviewer, the anti-vacuity gate, and the requirement that a close be
stated against the frozen definition. Probe mode makes attempts cheap; it does
not make them unreviewed.
