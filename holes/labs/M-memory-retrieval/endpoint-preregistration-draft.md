# V3 primary endpoint — preregistration draft (v0.1, for claude-12's edit, decision = Joe)

Drafted 2026-08-03 (claude-10) at claude-12's request, with their three
constraints from today's data built in (each named to its incident).
This discharges the DECISION half of SEQ-0.1; the CAPTURE half (sweeper
fields) is specced in §5 for whoever implements it.

## 1. The endpoint, delta form (constraint 1: S6 would score success under an absolute)

Per dispatch d, with target set T_d frozen in the dispatch packet at
dispatch time:

**closure(d) = 1** iff ALL of:

- **(pre)** at `sha-pre` (HEAD at dispatch), every t ∈ T_d is OPEN —
  mechanically read: sorry present / statement unproved, lexical count
  comment-stripped (the sorry-loop counting discipline). *Rows failing
  (pre) are class `stale-assignment` and leave the denominator — the
  S6 case: a96A04 was closed by 07-31/08-01 commits before dispatch;
  under an absolute definition it scores as a win.*
- **(post)** at `sha-post` (the dispatch's final committed state):
  `lake` exit 0 on T_d's module set; sorry count 0 within those
  modules; axiom check clean **on the named declarations** — the
  target theorem(s) for a fresh problem, the newly-closed declarations
  for a continuation (constraint 4).
- **(one-shot)** `sha-post` was committed by the dispatched session
  before cap or termination, with no operator continuation (see §3).

## 2. Mechanically swept, never runner-reported (constraint 2: S3-first)

The witness is EXECUTED by the sweeper, not read from the transcript:
checkout `sha-post`, run the build, count the sorries, run the axiom
check, record exit codes. Runner testimony never enters the endpoint.
*S3's first attempt: a 352-line file with 0 sorries that did not
compile — three errors in final assembly. Self-reported sorry-count
looks perfect; only the executed `lake` exit catches it.* Uncommitted
work is invisible to the witness BY DESIGN: unbanked work is not
closure (see §3).

## 3. Cap-death + continuation rule (constraint 3: S3-first + S5, twice already)

Two options, decision = Joe:

- **Option A (recommended): strict one-shot primary.** Cap-death with
  work uncommitted = closure(d)=0. Operator-continued dispatches form
  a SEPARATE preregistered class `continuation-closure`, counted and
  reported alongside, never pooled into the primary. Rationale: the
  primary keeps its ITT-like reading (binary one-shot under
  randomization — claude-12's own identification-by-design argument),
  and an operator "continue" is a co-intervention: informative, but a
  different treatment.
- **Option B: windowed.** The unit is the dispatch lineage including
  operator continuations; one-shot becomes the secondary. Cheaper to
  count, but the primary then measures dispatch+operator jointly, and
  arm contrasts inherit operator behavior as a nuisance.

Either option: the cap-death-uncommitted class gets its own count in
every report (observed twice in one day; it is not rare).

## 4. What this endpoint does NOT measure (stated at preregistration)

Partial progress (sorry-count reduction short of zero), proof quality,
memory contribution (that's the arms' job to contrast), and anything
about problems whose T_d was mis-frozen. Secondary endpoints, if
wanted, are separate preregistrations — this document defines only the
primary.

## 5. Witness-record schema (the CAPTURE spec — sweeper fields)

Per dispatch, written by the sweeper at sweep time:

```
:endpoint/sha-pre            commit sha at dispatch
:endpoint/sha-post           final committed sha of the dispatch session (nil if none)
:endpoint/target-set         T_d as frozen in the packet
:endpoint/pre-open?          per-t mechanical pre-state (the stale-assignment guard)
:endpoint/lake-exit          integer exit code, executed at sha-post
:endpoint/sorry-counts       per-module lexical counts, comment-stripped, at sha-post
:endpoint/axiom-verdicts     per named declaration (§1's naming rule)
:endpoint/one-shot?          true iff no cap-death, no operator continuation
:endpoint/continuation?      true iff operator-continued lineage (§3)
:endpoint/swept-at           timestamp; :endpoint/sweeper-version
```

Field names indicative; the invariant is: every conjunct of §1 has its
own recorded, mechanically-produced field, and the classes of §1(pre)
and §3 are recoverable from the record alone. (This is also the
denominator check for the endpoint estimand: the guard's
requirement-2 machinery can verify these fields exist per arm before
dispatch — the SEQ-0.1 and SEQ-0.2 instruments compose.)

## 6. Failure modes acknowledged

- `sha-post` unset (nothing committed): closure=0, class
  `no-committed-work` — distinct from a committed failure.
- Sweep-time environment drift (toolchain differs from dispatch-time):
  record the toolchain hash; a mismatch flags the row, not silently.
- The witness is free but not currently captured (the mission's
  "free, incorruptible" line): this schema is exactly the capture.
