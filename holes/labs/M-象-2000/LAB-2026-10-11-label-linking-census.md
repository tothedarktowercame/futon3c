# How label brackets could link: census (claude-16, 2026-10-11)

Read-only. Script: `scripts/label_linking_census.py` (Python replay of
`quoted-targets` / `link-quoted-brackets`, plus two candidate rules). Run at
2026-10-11 ~05:50Z over the turn records after the reply-text backfill
(`LAB-2026-10-11-reply-text-backfill.md`).

## Population

18 sessions, 448 analysed operator turns with `reply_text`, 1,016 marked reply
paragraphs, 571 operator fragments of an opening kind (propose, ask-action,
offer, delegate, promise, report-problem, clarify, constrain, verify). 204 of
the 448 turns have exactly one opening fragment.

## Rules

- **quote** (today): a bracket's quoted span of 8+ characters links to the one
  fragment of the same turn that contains it or is contained by it.
- **A, label overlap**: a bracket with no quote links to the one fragment of
  the same turn sharing at least two content words (4+ letters, stopwords
  removed).
- **B, sole port**: a reply paragraph of a saying kind (report, explain,
  verify, qualify, report-problem) with no target answers the turn's only
  opening fragment, when there is exactly one.

A target link closes the port whatever the adjacency table says (`answerso`).
B is an inference, so it belongs in `answers-inferred`, which `turn-ports`
does not populate today (`(lx/db acts)` with no inferred argument).

## Result: opening fragments answered, of 571

| rule | answered |
|---|---:|
| quote | 57 |
| quote + A | 76 |
| quote + B | 157 |
| quote + A + B | 167 |

## Against the labelled session (CHECKPOINT-2026-10-11-false-open-ports, 24 rows)

Rows closed by each rule, read against claude-15's labels:

| rule | closes | labelled answered or done | labelled open | labelled constraint |
|---|---:|---:|---:|---:|
| quote | 1 | 0 | 1 (the debugger idea: answered by a 🈖 that quoted it; "open" there meant not built) | 0 |
| quote + A | 3 | 2 | 1 (same) | 0 |
| quote + B | 8 | 6 | 1 (same) | 1 ("Let's not reinvent the wheel") |
| quote + A + B | 9 | 7 | 1 (same) | 1 |

No rule closed any of the three rows labelled open or partly open other than
the debugger idea, whose port the kernel rightly reads as answered. The one
wrong closure under B is a constraint, which B treats as a port because
`constrain` is an opener in the adjacency table; with `constrain` out of the
openers (the standing reading, `:constrain :creates`) B closes 7 of the 24,
all labelled answered or done, and more turns become sole-port turns.

What stays open under every rule: turns with several opening fragments (the
02:42 turn has four proposals and a constraint), and short follow-ups
("did you get any results back?", "are they still going?", "what is
milestone 4?") whose replies had no marked paragraph or a label bracket with
no shared words.

## Reading

1. Rule B gives the largest gain (57 to 157) with no false closure on labelled
   open items, and one wrong closure that the `constrain` change removes.
2. Rule A adds 19 on top of B with no false closure in the labelled rows, but
   the labelled sample for A is 2 rows; its precision on other sessions is not
   measured (no labels there).
3. Beyond B and A, the remaining gap is turns with several openers. A rule for
   those needs the reply's paragraph order or the operator's `<mark>:`
   pointers, not word matching; not proposed here.

## Proposed, pending Joe's word (the adjacency table is his prior)

- Implement B in the adapter as inferred links fed to `lx/db`, with a fixture
  and the labelled session as the regression (expected: 7 of 24 closed with
  the constraint change, 8 without).
- Drop `constrain` from the adjacency keys; it remains `:creates` a standing.
- Hold A until a second labelled session exists.
