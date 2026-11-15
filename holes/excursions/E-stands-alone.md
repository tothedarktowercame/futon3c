# E-stands-alone: what makes the shown top ten a cascade

**Status:** OPEN, deferred to the end of [M-repl-circus](../missions/M-repl-circus.md)
(Joe, 2026-11-15: "Let's leave "E-stands-alone" as an excursion for the end of
M-repl-circus").

## Where it stands

futon5 `lean-arxana/Circus/Loop.lean` proves `top_ten_is_a_cascade` (commit
`ec60d2d`): every shown entry after the first either `needs` an earlier entry or
"names why it stands alone". The original statement was false (a character
ranking items with empty reasons; `cascade_needs_reasons`), and the proved form
adds the hypothesis `hreason`: every character entry carries a nonempty reason.
The four characters meet it (`Circus.Characters.*_reasons_nonempty`).

## The gap

"Names why it stands alone" is modelled as *any nonempty reason*
(`Circus/Basic.lean`, `Cascade`). Every character always gives one, so the
`needs` disjunct never has to hold, and the theorem says only that every shown
entry has a reason. It does not say the top ten hangs together.

## The proposal

Require one of: a `needs` link to an earlier shown entry, or a specific typed
term (rendered, e.g., "stands alone: no open item it needs is shown") that a
character prints only when it is true of the basis (`Term.faithful`). This
changes what Mojira shows, so it is Joe's call; then restate `Cascade`, make
the producer (`scripts/giraffe_party.py`) print the term, and re-prove.
