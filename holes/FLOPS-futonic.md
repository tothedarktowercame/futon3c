# Futonic flops: missions and experiments that did not work, kept as arguments

**Opened 2026-11-21 by claude-5, at Joe's suggestion.** Joe: "there's a fairly large
number of missions which, frankly, have failed and maybe we should also start to
maintain something like a futonic flops record akin to the [Peeragogy] flops ... the
exercises where we tried to get zai agents to use memories which had been distilled from
Codex agents solving APM problems and almost nothing about that actually worked very
well, which isn't to say that it was a bad experience ... these failed missions or flops
or tragedies ... don't make for bad theater, bad watching, or bad learning ... they're
quite interesting."

Model: `~/notes/peeragogy-flops/ARGUMENTS.md` (claude-16, 2026-10-29), which reads each
flop as an argument: thesis, supports, defeaters (recorded or inferred), what happened,
what changes now. Each entry here adds: the benefit and harm ledger (who gained, who
bore the cost, including compute), and the kind of defeater (feasibility, demand or
who-benefits, measurement, or design).

Rules: quote the record; mark anything from memory; a flop is not a fault to assign but
an argument that did not hold, and the interesting part is why.

## Entry form

    ## N. <name> (<mission or excursion>, <dates>)
    **Thesis.** what it set out to show or build.
    **Supports.** what it rested on.
    **Defeaters.** recorded (with source) or inferred (marked).
    **What happened.** in the record's words.
    **Ledger.** benefits, harms (compute, time, people), and to whom.
    **Kind of defeater.** feasibility / demand / measurement / design.
    **What changes now.** what we know or can do that we could not before.

## 1. Memories distilled from Codex APM solving, given to Zai agents (draft; sources to settle)

**Thesis.** Memories distilled from Codex agents' APM solving would help other agents
(Zai seats) solve problems.
**What happened.** Joe, 2026-11-21: "almost nothing about that actually worked very
well". The record of the specific runs is still to be located; two related findings in
the record:
- `futon3c/holes/excursions/E-memory-v3-programme.md`: "V2 tested two *lexical*
  mechanisms and both failed"; co-occurrence came out with the "right magnitude, wrong
  sign".
- `futon3c/holes/missions/M-apm-demonstration.md`: "Every positive result on record is a
  **numerator**. The denominator has never been stated."
**Kind of defeater (first reading).** Measurement: without denominators, neither a
success nor a failure of memory use could be stated as a rate.
**What happened (Joe, 2026-11-21).** "hundreds and hundreds of APM attempts and
everything to do with white papers, version 2, version 3, the memory system ... I had
attempted something like a memory system before we had minus 40 ... it just didn't work
... the Zai agents were getting the memories but not using them or they weren't even
getting them or if they got them and tried to use them, they weren't useful so the whole
thing was really quite badly executed, which doesn't mean that it was a bad idea and it
doesn't mean that it couldn't be salvaged".
Three failures, at three points in one chain: delivery (memories not reaching the
agent), uptake (reaching it but not used), usefulness (used but not helping). Each needs
its own measurement, which is the denominator problem above.
**Kind of defeater.** Execution and measurement, not the idea; minus40 is the later,
working memory store, so the idea was partly salvaged in another form.
**Still to fill.** Which runs and seats; the compute spent; which of the three failures
dominated.

## Why flops go unnamed while they run: the try-harder loop (Joe, 2026-11-21)

"in the process of running a mission, we wouldn't admit that it was a flop so we say,
well, you know, I'll just try harder next time and that whole try harder pattern from our
first proof exercise applies to missions just as well as it applies to problem solving".

The lifecycle has no state for "tried, and it did not work": its states are COMPLETE,
RE-OPENED, BLOCKED, DEFERRED and NONSTARTER (`futon4/holes/mission-lifecycle.md`), and
NONSTARTER is for a gap that was not real or an approach found wrong, usually early. A
mission that keeps going round stays OPEN. So a flop is mostly recognised in
retrospect, "considerably after the fact", which is what this file is for. A CTRL read
could flag candidates while they run: missions with many rounds of work and no
completion criterion met.

## Ancestor flops: good ideas the technology of their time could not carry

Joe: "the re-architecting of Xanadu or Elephant 2000 as examples were actually a frankly
quite brilliant idea, just didn't work because the technology at the time wasn't very
suitable." Feasibility defeaters, in ARGUMENTS.md's terms, and both revived in the stack.
(Facts below from memory; check before quoting.)

## 2. Xanadu (Ted Nelson, from 1960)

**Thesis.** A docuverse of documents joined by two-way links and transclusion, with
every quotation tied to its source and its author credited.
**What happened.** Decades of development; code released as open source (Udanax) in
1999; the Web took the one-way link instead.
**Kind of defeater.** Feasibility, at the time.
**What changes now.** Arxana carries the line (benefit line P11: "links and transclusion
keep a pattern joined to its sources"); per-word provenance and notes resolved in their
sources (M-arxana-together) are pieces of it working.

## 3. Elephant 2000 (John McCarthy, 1989)

**Thesis.** A programming language based on speech acts: programs that ask, answer,
promise and commit, with the acts as first-class.
**What happened.** A proposal and a paper; not built as a working system.
**Kind of defeater.** Feasibility: no machinery then for programs to perform and keep
track of speech acts at scale.
**What changes now.** The 象 marks and M-象-2000 are the revival: speech acts recorded
and read in every turn.

## 4. An expert water plan for Mumbai (story told to Joe, 2026-11-21; unverified)

Joe, from his father: Bill Gates, in his charitable work, brought experts together on a
detailed plan to bring clean water to Mumbai, "something to do with ripping up every 5th
street to add clean water piping that would go to hubs", and it did not work. Not yet
checked: I do not know of a Gates project matching this description (the Gates
Foundation's best-known water and sanitation work is on sanitation); find the source
before using it. Kept here because its shape is a known one, whatever the facts turn out
to be: a plan made by outside experts for a whole city, against infrastructure that
residents and local officials already work around. Joe's question: "find those policy
folks who are laughing at Bill Gates right now and see if we could do some work for
them."
