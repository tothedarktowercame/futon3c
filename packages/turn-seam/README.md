# The turn-annotation seam

A package you can implement against, and a worked example of how it was
arrived at.

## What this is

A **seam** in Michael Feathers' sense is a place where you can alter behaviour
without editing in that place. Feathers' seams are found *retroactively* in
legacy code, which is why the word carries a salvage flavour. This package is
the opposite discipline, and Rob put it best: the boundary is declared
**before** there is an implementation to salvage, so the first version is
already swappable. His analogy is the better one — mains voltage is a
published contract that appliance makers and generator operators both conform
to, so neither has to know the other. Same structure, opposite direction, and
only the second one costs nothing.

This package declares one such boundary: **what an operator turn is**, and
**how its interpretation is dispatched**. Two files carry the contract, one
checks it, one shows a real instance.

| file | what it is |
|---|---|
| `turn-record.schema.json` | what a turn record **is**, independent of who writes it and who reads it |
| `dispatch-contract.md` | how interpretation is delegated, and what fire-and-forget owes you |
| `conformance.py` | run it against **your** records; no dependencies, no paths belonging to anyone |
| `example/` | one real record, passing unmodified |

## Using it

```sh
python3 conformance.py --dir /path/to/your/records
python3 conformance.py one-record.json
```

There is no install step and nothing to configure. If you produce records that
pass, your implementation and this one agree about what a turn is, and either
can be replaced without the other being edited. That is the whole claim.

The schema was **derived from real records and checked against all of them**,
not designed in advance. Where a field exists because of a mistake, the
description says which mistake.

That claim is a measurement with a date, not a constant, so here is how to
repeat it. It was 159 of 159 on 2026-09-24, against the session-turn-analysis
store of the implementation that produced this package; the same store read
162 of 162 on 2026-09-25, three turns having been recorded in between. The
records are run data and are not shipped here -- they are one operator's
conversations -- so the number you can check is the one over your own:

    python3 conformance.py --dir PATH/TO/YOUR/RECORDS

A figure nobody can re-derive is a decoration, and this one was one: it
travelled as "159 of 159" with no input named, which an outside reader called
out (H-WITNESS-ii, futon2 417bfb4f).

## If you are implementing the other side

Three things will bite, in the order they bit here:

1. **Offsets.** `offset_unit` is codepoints, zero-based, end-exclusive. A
   JavaScript client counts UTF-16 units and will disagree on any emoji or CJK
   character, silently, producing annotations that land one or two characters
   off. Convert at your boundary.
2. **Two texts, not one.** A turn that quotes something has `source_text`
   (with `QUOTE` tokens) *and* `original_text`. An interpreter must not read
   quoted words as the operator's own; a display must show what was on screen.
   Designing this field around either reader alone is the mistake that
   produced it — see below.
3. **Delivered is not done.** A dispatch that returns cleanly means the brief
   arrived, not that the work happened. Record the job id; ask later.

## The mistake this was built after, and why it is in the README

The `QUOTE` convention was first designed as *"strip the quoted text"* —
a verb belonging to one consumer, the interpreter. An hour later the display
needed that text back, and a side-channel had to be retrofitted to carry what
had already been thrown away.

The generalisable rule: **a seam fails when you design it around what one
consumer needs rather than around what the thing is.** "Strip the quote" is a
consumer's verb. "A turn has a said part and a quoted part" is what the thing
is, and it serves both readers without either being privileged.

That is why `source_text` and `original_text` both exist, and why the schema
describes them as two readers rather than as a field and its backup.

## Where it came from

This package is the DOCUMENT phase of
[`M-futon-seams`](../../holes/missions/M-futon-seams.md), a mission about
places where an interface should have been declared and was not. The mission
found eight; this is the seventh, chosen because Rob asked for the example.

The method it followed is written out in that mission's DERIVE section as nine
steps — hit the coupling, measure it, choose the grain, write the cascade,
wire it, choose a target, predict, enact, observe — each with the tool that
does it or `by hand`, and each with a check. Four steps are judgements made by
a person; the claim is not that judgement can be automated away but that a
judgement must be **recorded in a form something else can contradict**.

Applied here:

- **The coupling**, measured: the turn-record format has **five** readers and
  writers agreeing by convention, and the `>>>` fence convention is
  implemented in **one** of them. A turn recorded live has its quoted block
  elided; the same turn recorded through the batch path keeps it inline as
  though the operator had said it. Two writers of one format, disagreeing
  about what the format means.
- **The grain**: the **turn**. One turn is what a single run changes, and its
  facts — offsets, quoted regions, sentence spans — are observable in the
  record. The buffer is where it happens to be typed.
- **The check**: `conformance.py`. The contract is a contract because
  something fails when an implementation disagrees.

What is **not** claimed: that the seam is enacted. The five existing readers
have not been converted to this schema. It is declared, checked against real
output, and reusable — which is the state the mission argues is cheap to
reach and expensive to skip.
