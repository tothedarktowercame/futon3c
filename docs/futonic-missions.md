# Futonic Missions: A Methodology for Theory-Grounded Software Development

## Introduction

### Opening: A Tale of Two Databases

futon1 is a knowledge-graph database built by AI agents using standard
agentic programming methods. Over 187 commits it accumulated silent
durability drift, dangling relation hydration, data-root divergence across
entry points, questions treated as assertions, RocksDB lock contention, and
three JVM segfault crashes. Its test suite - 88 tests, 428 assertions -
currently reports 6 failures and 6 errors. Debugging windows ranged from
11 minutes for a watchdog fix to 26 days for ingest hardening.

futon1a is the same database, rebuilt using futonic missions. In 63
commits - a third of the effort - it reached 99 tests, 695 assertions,
0 failures, 0 errors. It successfully migrated 17,564 documents with
checksum verification. When tricky edge cases arose (whitespace rejection,
nil entity handling, external-id conflicts under stress), they were
resolved quickly because every design decision had a traceable argument
chain.

The difference is not that the programmer improved. The difference is
the process.

### What Changed

futon1's problems are documented *after the fact*: issue files written
once something broke, limitation docs cataloguing known risks, crash logs
sitting in the repo root. The system failed and then someone wrote down
why.

futon1a's reasoning was recorded *before* execution and validated *after*.
Seven Pattern Selection Records (PSRs) document which pattern was chosen
for each task and why. Seven matching Pattern Use Records (PURs) document
whether the pattern worked. A traceability chain - evidence -> pattern ->
PSR -> module -> test -> doc -> error - is encoded as a first-class artifact
in the repository.

The structural catches illustrate the difference concretely:

- A legacy descriptor verification test explicitly notes "would fail
  pre-repair" - a verification-phase catch that would have been a runtime
  bug in futon1.
- A pipeline-order test prevents Layer 4 from preempting Layer 3 - a class
  of bug that in futon1 required a debugging window of over four days.
- An error hierarchy test enforces correct HTTP status codes per layer
  (L0=503, L1=409, L2=500, L3=403, L4=400) - the equivalent problem in
  futon1 (silent durability drift) was warning-only, not fail-stop.

These are not just tests. They are *derived from architectural
commitments* - specific decisions made in the DERIVE phase of the mission,
each with a documented IF/HOWEVER/THEN/BECAUSE argument structure. In
futon1, the equivalent problems were discovered through painful debugging.
In futon1a, they were prevented by design.

### The Donor Body

futon1 was not wasted work. It was the necessary first pass that produced
the evidence the methodology needed to exist. Its failure modes - the
26-day debugging window, the crash logs, the silent drift - became the
selection pressure that shaped the methodology's defences. futon1 donated
its failure modes to futon1a's immune system.

In the vocabulary of futonic methodology, futon1 is a phenotype: observable
behaviour in the environment, failures included. Its problems became
genotype material - patterns encoding "what goes wrong when you build
without derivations." futon1a served as the first exotype for the theory:
the interface where the methodology met a real system and had to prove it
could connect. And the methodology itself is a xenotype - a portable
process that transferred from theory-space into implementation-space. The
only way to know a xenotype is real is to watch it transfer. futon1a is
that evidence.

This paper describes the methodology, its components, and why it exists.

### What Is a Futon?

A futon is a historical trend with clear generative potential - an element
of the future that is visible in the present if you know how to read the
developmental trajectory.

Steam engines in the 1860s. The abolition of slavery. The internet in
1993. These are futons: trends whose generative course was legible to
those paying attention, even when the specific outcomes were not yet
determined. The discipline of futonic thinking is reading those
trajectories and building with them - not prediction, but something closer
to morphological reading in the tradition of D'Arcy Thompson, who looked
at biological forms and saw the mathematical forces that shaped them.

The relevant futon for this work is computational mathematics - and more
broadly, the mechanisation of knowledge production. Large language models
are the current expression of this trend, but the trend predates them by
decades. Alan Turing wrote about machine intelligence in 1951. Christopher
Alexander formalised design patterns in the 1970s. Elinor Ostrom
described institutional action arenas - bounded contexts where rules,
participants, and outcomes co-evolve - and explicitly acknowledged the
resonance with Alexander's patterns. William Gibson observed that the
future is already here, just not evenly distributed.

Futonic methodology draws on all of these: Thompson's morphological
reading of developmental form, Turing's vision of machine reasoning,
Alexander's patterns as microcosms of change, Ostrom's action arenas as
the minimal institutional context for coordination, and Gibson's insight
that the future is a distribution problem, not a prediction problem.

### Why Now

LLMs are powerful components. But components are not machines. An LLM can
generate code, but it cannot do engineering - it cannot trace design
decisions to theoretical ground, validate architecture against structural
constraints, or learn from accumulated evidence of what works.

Futonic missions supply the missing architecture. They are the methodology
that makes LLM capabilities compose into something that develops over
time. The key mechanism is evolutionary: a Baldwin cycle applied to the
evolving codebase rather than to frozen model weights. Patterns are not
designed top-down - they are distilled from accumulated evidence of what
works, through a glacial learning loop that scans proof paths for
recurring structural tensions and promotes them to new patterns.

The result is a system that improves through use. futon1a is the first
demonstration. This paper describes how it works and why.

## 1. The Problem with Tickets

A ticket says "build X." A good ticket says "build X because user Y needs Z."
Neither says:

- What theory or principle makes X the right thing to build
- What patterns from prior work apply (or why none do)
- What the argument structure is — what tensions exist, what tradeoffs were
  considered, what was rejected and why
- What evidence the work produces, in a form that future decisions can consume
- How to tell, structurally, whether the work is *done* versus merely *shipped*

Tickets are imperative: do this. Missions are declarative: this follows from
that, and here is the proof.

## 2. The Four Types

Futonic methodology is built on four biological types, adapted from
evolutionary developmental biology to software systems:

| Type | Origin | In Software |
|------|--------|-------------|
| **Genotype** | Replicable internal representation | Patterns, rules, DSLs, priors — the library |
| **Phenotype** | Observable behavior in environment | Outputs, test results, API responses — what the system does |
| **Exotype** | Externally visible interface shape | Layer contracts, wiring diagrams, event protocols — how components connect |
| **Xenotype** | Cross-domain portable structure | Reusable adaptation skeletons — processes that work in any domain |

The relationships form a cycle:

```
xenotype (portable skeleton)
    │ projects to
genotype (concrete patterns)
    │ executes as
phenotype (observable behavior)
    ↔ connects via
exotype (interface shape)
```

The derivation xenotype — the process of creating a mission — is itself a
xenotype. It was first applied to derive coordination patterns, then social
patterns, then portfolio inference. Each application produced domain-specific
genotypes (patterns), phenotypes (passing tests), and exotypes (validated
wiring diagrams). The process is the same; the domains differ.

## 3. The Derivation Xenotype

Every futonic mission follows six phases. Each phase has a specific purpose
and produces specific artifacts:

### IDENTIFY

Survey the domain. What exists? What's been tried? What worked, what failed?

This is not brainstorming — it's archaeology. The discipline is: *read before
you write*. Search the codebase, the pattern library, the mission history.
Count files, name functions, trace dependencies. The output is an inventory
with specific counts and file paths, not vague impressions.

**Artifact**: Domain audit with evidence counts.

### MAP

Map theoretical concepts to domain concerns. For each concept from the
theory (invariants, patterns, prior art), identify the domain-specific
analogue.

The MAP phase cross-references three sources:
1. **Theory patterns** — what the futon-theory library says about this domain
2. **Prior implementations** — what exists in the codebase already
3. **External patterns** — relevant patterns from the library that constrain
   the design space

The output is a comprehensive mapping: what exists, what's missing, what
transfers from other domains, and what needs to be built new.

**Artifact**: Mapping table + pattern cross-reference + implementation plan.

### DERIVE

Make concrete architectural commitments. Each decision follows the
IF/HOWEVER/THEN/BECAUSE argument form:

- **IF**: The precondition — what we know from the MAP phase
- **HOWEVER**: The obstacle — why the obvious approach doesn't work directly.
  This is a reframe, not a mere objection. It says "the naive mapping (IF
  implies THEN directly) is wrong; this complication changes the target."
- **THEN**: The commitment — what we will actually build
- **BECAUSE**: The justification — why this commitment follows from the
  evidence, grounded in theory axioms or prior pattern outcomes

Each derivation is numbered (D-1, D-2, ...) and self-contained. The
collection of derivations defines the architecture.

**Artifact**: Numbered derivations in argument form.

### ARGUE

Compose the derivations into a flowing argument. The ARGUE phase is not
a summary — it is a *textual proof* that the architecture follows from the
evidence. It weaves together the `!conclusion` blocks from referenced
patterns into a coherent narrative.

The argument has a specific structure:
1. What gap does this mission close?
2. What grounding commitments does it make? (3-5 key principles)
3. How does each architectural choice follow from those commitments?
4. What failure modes does it prevent?
5. Why should this exist at all?

The discipline: if a claim appears in the ARGUE section that isn't grounded
in a specific derivation or pattern conclusion, it doesn't belong.

**Artifact**: Prose argument document.

### VERIFY

Check the architecture against machine-readable constraints. There are two
levels of verification:

**Structural verification**: Does the architecture satisfy the wiring
diagram? Exotype diagrams (`.edn` files) define components, edges,
timescales, and compositional rules. A verification tool checks:
completeness, coverage, no orphan inputs, type safety, spec coverage,
timescale ordering, exogeneity, and compositional closure.

**Empirical verification**: Do the tests pass? Does the implementation
match the derivations? Can the system actually produce the outputs it
claims to produce?

In the portfolio inference mission, VERIFY also included a core.logic
relational layer — structural reasoning expressed as logic relations rather
than imperative code, making "what is possible" a declarative query.

**Artifact**: Verification report (structural checks + test results).

### INSTANTIATE

Build the code. By this point, the architecture is fully specified, the
patterns are selected, the argument is written, and the constraints are
checked. INSTANTIATE is the *least creative* phase — it's translation from
a well-specified design into working code.

This is deliberate. Creativity happens in MAP and DERIVE. Implementation
should be mechanical. If INSTANTIATE requires novel design decisions, the
earlier phases were incomplete.

**Artifact**: Working code + passing tests.

## 4. Evidence at Every Layer

A futonic mission doesn't just produce code. It produces typed evidence
records that feed back into the system's own decision-making.

### Pattern Selection Record (PSR)

Before executing any task within a mission, the agent records which pattern
was selected and why:

- Which patterns were considered
- Which was chosen (or: no pattern matches — a "gap PSR")
- The rationale for selection
- Confidence level
- Success criteria (what "done" looks like)

A task without a PSR is rejected at gate G3. This isn't bureaucracy — it's
the system ensuring that work is pattern-backed or explicitly novel.

### Pattern Use Record (PUR)

After completing a task, the agent records whether the pattern worked:

- Were success criteria met?
- What was the prediction error? (Did the pattern do what we expected?)
- What was the outcome?

The PUR closes the loop opened by the PSR. Together they form a feedback
signal: which patterns work in which contexts, and how reliably.

### Post-Action Review (PAR)

At session close, a structured review captures:

- What prediction errors occurred (with severity)
- What worked well
- What didn't work
- Feedback on the patterns used
- Proposed new patterns (if the session revealed a gap)
- Time budgeted vs. time spent

The PAR is not optional — it is an obligation. Every completed proof path
must produce one before closing. PARs feed into the glacial learning loop
(L1), where recurring tensions are promoted to new patterns.

## 5. The Gate Pipeline

Evidence flows through six gates, from task specification to durable storage:

```
G5  Task Specification ── Is the task well-defined?
 │
G4  Agent Authorization ─ Is the agent registered and capable?
 │
G3  Pattern Reference ─── Has a pattern been selected? (PSR required)
 │
G2  Execution ─────────── Do the work (the only gate that touches environment)
 │
G1  Validation ────────── Does output satisfy criteria? (PUR required)
 │
G0  Evidence Durability ─ Is everything persisted? (PAR required)
```

Each gate has a dual reading — an *agency* reading (about the agent doing
the work) and a *pattern* reading (about the pattern guiding the work):

| Gate | Agency Reading | Pattern Reading |
|------|---------------|-----------------|
| G5 | Is the agent's task well-defined? | Is the task shaped so a pattern can guide it? |
| G4 | Can this agent do this work? | Does this pattern apply to this domain? |
| G3 | Has the agent committed to an approach? | Has a pattern been engaged with? |
| G2 | Agent executes within budget | Pattern application stays within scope |
| G1 | Agent's work gets evaluated | Pattern's guidance gets evaluated |
| G0 | Agent's session is recoverable | Pattern use evidence is persistent |

The pipeline ensures that every piece of work — no matter how small —
produces structured evidence that future missions can query.

## 6. Three Nested Loops

The futon system operates at three timescales, each running its own
active inference loop:

```
Social (seconds)  ──→  Task (minutes)  ──→  Glacial (weeks)
       ↑                                          │
       └──────── library constrains ──────────────┘
```

**Social loop** (futon3c): Agents coordinate in real time — hand off work,
maintain shared awareness, route messages across surfaces (IRC, Emacs, WS).
Social-timescale events produce typed records into the proof-path store.

**Task loop** (futon3b, L0): The gate pipeline validates, executes, and
persists. Each task produces PSR/PUR/PAR evidence. The output: artifacts,
evidence, proof paths.

**Glacial loop** (futon3b, L1): Scans accumulated proof paths for structural
tensions — recurring gap-PSRs, repeated gate rejections, patterns that appear
across multiple missions. When a tension crosses a threshold (frequency ≥ 3,
contexts ≥ 2, evidence ≥ 2), the canonicalizer promotes it to a new pattern
in the library. The evolved library then constrains future social and task
coordination.

This is a Baldwin cycle: the library (genotype) constrains behavior
(phenotype), but successful behaviors feed back into the library through
the glacial loop. Patterns are not designed top-down — they are distilled
from accumulated evidence of what works.

## 7. Flexiformal Argument

Futonic patterns are written in a "flexiformal" style — they admit three
readings at three levels of formality:

| Level | Reading | Example |
|-------|---------|---------|
| Informal | Prose for humans | "Every task must carry a PSR before execution" |
| Flexiformal | Queryable logic | `(run* [t] (task t) (nafc psr-for-task _ t))` finds violations |
| Formal | Compiled gate code | G3 rejects if no PSR attached |

The IF/HOWEVER/THEN/BECAUSE structure bridges these levels. The IF/THEN is
a logic rule. The HOWEVER captures subtlety that remains prose — the case
the rule must handle despite counterevidence. The BECAUSE is queryable
provenance ("why does this rule exist?").

This is important: the patterns are not just documentation. They are
simultaneously human-readable arguments, machine-queryable relations, and
compiled validation code. The same pattern exists at all three levels.

## 8. Active Inference as Organizing Principle

Futonic missions are organized around active inference (AIF) — the principle
that an agent maintains a generative model of its world, computes prediction
errors when observations surprise it, and selects actions that minimize
expected free energy (a combination of goal progress and uncertainty
reduction).

At mission level, this means:

- **Observe**: Gather normalized signals about the portfolio — completion
  ratios, coverage trajectories, evidence velocity, dependency depths,
  stall counts
- **Perceive**: Compute prediction error — what we expected vs. what we see.
  This is not a dashboard metric; it's a signal that drives belief revision
- **Affect**: Determine the system's behavioral mode — BUILD (there are gaps
  to fill), MAINTAIN (things are stable), or CONSOLIDATE (entropy has
  accumulated and needs attention)
- **Policy**: Rank candidate actions by expected free energy — which action
  best combines goal progress with uncertainty reduction, given what we
  currently believe?

The system doesn't just track metrics. It *predicts*, is *surprised*, and
*learns*. When the system recommends "review the portfolio" over "start a
new mission," it can trace exactly which signals (rising stall count, aging
reviews) drove that recommendation, through which precision weights, at
what confidence level.

## 9. Worked Example: M-portfolio-inference

M-portfolio-inference is a mission that was derived using this methodology
and simultaneously serves as a computational model of the methodology itself.

### IDENTIFY

The mission identified a five-part gap in the existing portfolio tools:
no generative model, no prediction error, no belief update, no policy
selection, no adjacent-possible computation. The audit surveyed 6 Chapter 0
invariants, the ant AIF loop (futon2), 12 potential sensory channels, and
the coordination exotype embedding.

### MAP

The MAP phase cross-referenced three legacy systems (FuLab AIF bridge,
Nonstarter desire market, futon5a weekly cycle) and 19 patterns across
5 library categories. It identified what transfers (the EFE math from
FuLab, the vote decay from Nonstarter, the bid/clear cycle from futon5a),
what needs adaptation (ant modes → portfolio modes, food channels →
coverage channels), and what needs building (the portfolio-specific AIF
loop).

### DERIVE

Ten numbered derivations (D-1 through D-10) defined:
- Location: futon3c (where the data lives)
- Observation vector: 12 normalized channels from mc-backend
- Generative model: μ mirrors observation shape + mode + focus + urgency
- Precision: per-channel weights reflecting measurement reliability
- Mode dynamics: BUILD/MAINTAIN/CONSOLIDATE with hysteresis
- EFE: 4-term decomposition (pragmatic + epistemic + upvote + effort)
- Adjacent-possible: 5-condition gate
- Timing: anytime query + weekly heartbeat
- Arena: one arena now, expandable (Ostrom IAD structure)
- Evidence: belief state as first-class evidence type

### ARGUE

The argument showed how each derivation follows from pattern conclusions
and theoretical invariants, how the architecture prevents specific failure
modes (dashboard without inference, habitual collapse to familiar missions,
building without consolidating), and why the system should exist (closing
the AIF loop at portfolio level).

### VERIFY

A core.logic relational layer was built, expressing the adjacent-possible
boundary as declarative logic relations rather than imperative code. Eight
relation types, seven structural query types (adjacent set, what-if,
critical path, pattern co-occurrence, shared blockers, repo distribution,
evidence ranking). The verification showed that the architecture composes
correctly: core.logic computes what is structurally possible, AIF computes
how to prioritize among possibilities.

### INSTANTIATE

Seven modules were built:
- `observe.clj` — 12 normalized channels from mc-backend
- `perceive.clj` — predictive coding with precision-weighted prediction error
- `affect.clj` — BUILD/MAINTAIN/CONSOLIDATE with hysteresis and urgency coupling
- `policy.clj` — 4-term EFE with softmax selection and abstain threshold
- `adjacent.clj` — 5-condition boundary computation
- `logic.clj` — core.logic relational layer
- `core.clj` — full AIF loop orchestrator

Running live against the real portfolio (40 missions, 26 adjacent), the
system produced its first recommendation:
```
Mode: CONSOLIDATE | Urgency: 0.54 | τ: 1.38
Recommendation: review
Top actions:
  review:       G=-0.520  p=23.6%
  consolidate:  G=-0.340  p=20.7%
  wait:         G=-0.300  p=20.1%
```

57 new tests across 7 test files. 917 total suite passing.

## 10. What Makes This Different

A futonic mission is not:
- A ticket (it derives from theory, not from a backlog)
- A spec (it includes the *argument* for the spec, not just the spec)
- A design doc (it produces machine-checkable evidence, not just prose)
- A research paper (it produces working code, not just analysis)

It is all of these at once, held together by the derivation xenotype — a
portable process that works the same way whether you're deriving
coordination patterns, social protocols, or portfolio inference algorithms.

The key structural properties:

1. **Theory-grounded**: Every design decision traces to an invariant, a
   pattern, or prior evidence. "Because it seemed like a good idea" is
   not an acceptable BECAUSE.

2. **Pattern-backed**: Every task carries a PSR documenting which pattern
   was selected. Work without a pattern selection is either gap-PSR
   (explicitly novel) or rejected.

3. **Machine-checkable**: Wiring diagrams are validated by categorical
   checks. Tests are necessary but not sufficient — the architecture
   itself must pass structural verification.

4. **Evidence-producing**: Every phase produces typed evidence that future
   missions can query. The system learns from its own history through
   the glacial loop.

5. **Flexiformal**: Patterns exist simultaneously as prose, logic queries,
   and compiled code. The same truth is expressed at three formality levels.

6. **Self-improving**: The Baldwin cycle means that accumulated evidence
   evolves the pattern library, which constrains future missions, which
   produce new evidence. The methodology improves itself through use.

## 11. The Futonic Loop

The methodology can be expressed as a single compositional structure, using
the futonic vocabulary:

```
(futonic-loop
  (input   象  := domain to be derived)
  (choose  部  := IDENTIFY the decomposition: archaeology, audit, survey)
  (articulate 咅 := MAP + DERIVE: patterns and requirements from evidence)
  (if (forms 鹽) := ARGUE: requirements (鹵) + argument structure (皿)
                    compose into a valid case)
  (and (sense 香) := during argument, perceive convergences)
  (and (regulate 🔮) := VERIFY: check invariants, validate diagrams)
  (then
    (act-within 未知 := INSTANTIATE: build the code)
    (evaluate 味 := tests pass, checks pass, round-trip proof))
  (else
    (apply 捨 := park mission, record blocker, explicit exit)))
```

The loop captures both the happy path (derive → argue → verify → build →
evaluate) and the explicit exit (捨: when a mission should be parked, it
is parked deliberately with a recorded reason, not silently abandoned).

## 12. Getting Started

To create a futonic mission:

1. **Start with IDENTIFY**. Search the codebase, the pattern library, the
   mission history. Count files, name functions, trace dependencies. Do not
   skip this — the discipline of reading before writing prevents
   reimplementing solved problems.

2. **Write the MAP**. For each theoretical concept, identify the
   domain-specific analogue. Cross-reference at least the theory library,
   the relevant domain library, and any prior implementations.

3. **Write the DERIVE**. Each decision in IF/HOWEVER/THEN/BECAUSE form.
   Number them. Make each self-contained. If you can't write the BECAUSE,
   the decision isn't grounded — go back to MAP.

4. **Write the ARGUE**. Weave the derivations into a flowing argument.
   Every claim must trace to a specific derivation or pattern conclusion.

5. **Run VERIFY**. Check against wiring diagrams if they exist. Run tests.
   If structural checks fail, the architecture needs revision — go back
   to DERIVE.

6. **INSTANTIATE**. By now, implementation should be mechanical. If it
   requires novel design decisions, earlier phases were incomplete.

At every task boundary, record a PSR (before) and PUR (after). At session
close, write a PAR. These evidence records are not overhead — they are the
learning signal that makes future missions better.
