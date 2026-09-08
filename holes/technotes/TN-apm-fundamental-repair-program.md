# The fundamental repair program

Written 2026-09-08 by claude-5, from claude-9's design consultation, at Joe's
instruction. It exists because the alternative was another night of individual
fixes.

Joe, 2026-09-08:

  "I don't want to refuse student dispatches when the cascade did not serve
   memory. I want to fix the underlying problem... I'm just sick of these hot
   fixes for individual issues that don't address the fundamentals. All these
   little individual fixes are just adding up to a giant miasma."

The miasma is real and measurable: 126 decided frame parks, 111 (88%)
apparatus, across ~40 distinct classes most of which were seen once or twice.
Each class is a patch applied after its own incident. The count of classes is
the symptom.

## The diagnosis I missed

The normal path was designed to produce evidence shapes indistinguishable from
faults. **Success is recorded in the vocabulary of failure.**

The typed-submission wrapper cancels a role job once an authenticated
submission exists, so a DELIVERED turn normally ends with the invoke ledger
saying `cancelled`, `session-id nil`, and the collection recording the raw
pre-cancel `:running`. The primary record lies about the commonest good
outcome. Once it lies, every consumer must be clever -- and that is the entire
origin of the five divergent joins, of attribution needing inference at all,
and of 13 of the 126 parks (`:student-terminal-session-id-erased`).

Every fix I made on 2026-09-07 -- f28523b6, 2feb07e1, db393de0, ec97a42b --
was a consumer learning to be clever about that lie. They are correct and they
are not the fix.

## The program, in order

### 1. Make success stop resembling failure

Give the invoke ledger a first-class terminal state meaning
`delivered-by-submission`, so `cancelled + submission-present` stops existing
as a shape. Most of the inferential burden evaporates rather than being made
rigorous. Upstream of the most parks and of nearly all the inference.

### 2. One disposition record, not one predicate

Not one join function -- codex-17 was right to refuse that, because the five
sites ask genuinely different questions of the same evidence. But one RECORD.

At terminal collection, construct a single role-turn disposition from both
authorities: a small CLOSED enumeration of the product space --
`delivered-clean`, `delivered-then-wrapper-cancelled`,
`process-died-no-submission`, `submission-without-process-record`,
`cancelled-no-submission`, and so on. Every consumer pattern-matches on that,
totally, and loses the right to touch raw evidence.

A new evidence surface then changes ONE constructor, and either every consumer
handles the new case or a lint fails loudly. Clojure gives no compiler
exhaustiveness; build the lint, or the closed enum quietly reopens.

This subsumes the student-cascade guard I proposed and Joe rejected: a
disposition that already records whether memory was served makes a bespoke
dispatch guard unnecessary, and the exhaustiveness lint forces the case to be
handled rather than forgotten.

### 3. Carry the mechanism, not the blame

The origin cannot know blame either. futon1b knows "query-permit wait exceeded
under load"; whether that is apparatus or agent is a judgment relative to the
campaign's contract, and judgments change with the contract.

So: the origin records the MECHANISM -- a namespaced fault code plus
blame-relevant facts, as a structured envelope in the response body -- and
blame is a pure function from mechanism to attribution, evaluated in exactly
one place, versioned with the contract.

Two rules make it survive a process boundary:

  - **HTTP status is transport routing and may never be an input to blame.**
    Only the envelope's fault code may. Intermediaries may wrap an envelope
    with hop information, never translate it. Translation is where provenance
    died in the 504 case, and `ec97a42b` -- which maps 5xx to :transport -- is
    a violation of this rule that will break at the next surface.
  - **Charging a role requires affirmative evidence of role fault; anything
    unclassifiable defaults to :apparatus.** Justified by asymmetry: wrongly
    charging a role spends an unrepairable budget and contaminates the
    measurement, while wrongly blaming apparatus costs a re-check. This
    default alone would have made both of 2026-09-07's surface-flips harmless.

### 4. Detectors

  - **A progress cursor must be a function of the object of work, and absence
    of the object's identity is an ALARM, not a degradation.** The watchdog's
    real failure was not reading the tick id; it was silently proceeding with
    a cursor whose frame fields were all nil. A monitor is the one component
    where absence of evidence is itself the alarm condition. This is
    mechanically checkable; "no field that changes when no work happened" is
    only checkable once you already suspect the field.
  - **A detector is commissioned only by witnessing it detect an INDUCED real
    failure.** Kill the frame, watch the alarm fire, then trust it. Fire
    alarms are tested with smoke, not by reading the wiring diagram. Defect
    register A10 already recorded why review cannot substitute: F-triangle
    arms its own watchdog on its own coordinator, and eight green runs proved
    nothing about the production path.

### 5. A comparator for "built but never switched on"

Four instances on 2026-09-07: `reopen-posthoc-rejection` (written, tested, no
production caller, and unable to clear the latch it would need to); a futon1b
fix committed but never loaded; a voxterm display fix committed but never
loaded; and `topology-supervisor.sh`, written specifically so loop stoppages
get repaired by an agent instead of reaching Joe, which was not running --
which is why the stoppage reached Joe.

These are not four incidents. They are defect-register **A11** and **M7**,
both logged `**unfixed**`, guarantee `**none**`, weeks earlier: "Commits reach
disk without reaching the running JVM, and nothing detects the gap... nothing
compares the loaded value of a declaration against its committed source."

Two sources of truth with nothing diffing them. Discipline is what we are
substituting for a missing comparator. Build one: every mechanism declares
itself in a manifest the repo owns; one conformance check on a timer diffs
declared-should-run against live-is-running and loaded against committed; a
mismatch is a first-class apparatus fault. Plus the commissioning rule from
(4): a packet delivering a mechanism is not done until the reviewer has
observed the mechanism ACT in the live system.

## The metric

The park-class census should be a closed, small, SHRINKING set. Its growth
rate is what tells us whether the constraint is being obeyed. A new park class
is a design defect to be eliminated, not a species to be catalogued.

Every layer of catch-and-park is capitulation to a failure that should have
been made impossible one layer down. The latch is telling us where that layer
is.

## The worked plan (2026-09-08, claude-5 drafting / claude-9 reviewing)

The five steps above are the shape. These are the slices, each naming the
principle it enacts and taking that principle's violation signature made
absent as its acceptance bar. "The failure I was dispatched about no longer
occurs" is not an acceptance bar — that bar is what produced patch after
patch. Scoring lives in `TN-apm-principle-scoring-ledger.md`.

Campaign `jit-all-open-v3` halted 2026-09-08T02:54:34Z on Joe's authorization
— `stop!` returned `:status :stopped` with a quiescence witness at epoch 11,
tick 26977, `tick-claim nil`; scheduler and watchdog stopped. The halt is paid
for; the register is the work queue until the gates pass.

| Slice | Principle | State |
|---|---|---|
| S1 unwrap before blaming | P9 | **done** — `0c46297b`, loaded and observed acting in :7070 |
| S2 carry the class, not just the message | P9 | **done** — same commit |
| S3 "none served" ≠ "expansion incomplete" | P2 | next — `voxterm/server.py:2165-2169` |
| S4 commission the watchdog both directions | P7 | owed since the watchdog repair |
| S5 terminal-vocabulary migration | P9, P2 | discovery done (codex-17); one consumer group per dispatch |
| S6 declared-vs-loaded comparator | P8 | closes register A11 and M7 |
| S7 bound the `:awaiting-substrate` wait | P6 | verifiably absolute deadline, expiry routed to a decision |
| S8 substrate I/O survives substrate unavailability | P12 | Joe's inversion of our restart rule — see below |
| S9 scheduled drain points | P13 | design note only, unbriefed build forbidden |

### S8 — why it replaced a rule about people

We proposed forbidding a shared-JVM restart while a durable operation is in
flight. Joe rejected it: "futon1b should be durable enough. Work that's
getting sent in there should get queued if it's not available, and then sent,
and then processed." The obligation is client-side durability, not operator
etiquette. A rule a human must remember has a human forgetting as its failure
mode, which is the catch layer P12 forbids, proposed by the two agents
applying P12 to everything else that night.

Cascade expansion goes first. The halt state shows why the shape matters: the
coordinator was in `:awaiting-substrate` with `:attempt 1 :max-attempts 3`,
history `:hyperedge-unreachable` then `:memory-snapshot-visibility-not-obtained`.
The substrate answers the identical query in ~1.1s. Three strikes spent a
frame on an indexing lag. S8 depends on S1/S2: retryable-vs-terminal is only
decidable once errors carry their mechanism.

### S9 — manufacture the quiet time

Joe kept the quiet-time requirement for the futon3c JVM and named finding one
an open problem he has no answer to. Same elimination one level up: if a quiet
time must be hunted, produce it on a schedule instead. The coordinator exposes
a bounded quiescent window at frame boundaries and reloads land there. Sketch
goes to Joe before anything is built.
