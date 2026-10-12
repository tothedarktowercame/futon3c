# Packet F3: an Emacs review buffer for a paper's dependency edges

From claude-16 (owner, M-象-2000), 2026-10-12. Mode: work. Follows futon6
23aa212d (`src/futon6/paper_dependency.py`, review JSONs under
`data/showcases/ct-anatomy/golden/<paper>-dependency-review.json`). Repo for
this packet: `/home/joe/code/futon3c`, master, shared checkout; new file
`emacs/paper-dependency-review.el` and its ERT test
`emacs/test/paper-dependency-review-test.el`; do not edit
`emacs/paper-anatomy.el` (read it for conventions: `paper-anatomy-dir`,
`paper-anatomy-open`, `paper-anatomy-mode-map`). Commit touched files only.

## Behaviour

`M-x paper-dependency-review PAPER` (completing over
`*-dependency-review.json` in `paper-anatomy-dir`) opens `*deps PAPER*`, a
`tabulated-list-mode` buffer, one row per edge, sorted by target then rule:

| column | content |
|---|---|
| verdict | `✓` right, `✗` wrong, blank unsure (from the JSON `verdicts`, last verdict per `edge_id` wins) |
| rule | `D1`, `D2` |
| edge | `──` for `style: solid`, `╌╌` for `dashed`; face red (`error`) when `cycle` is true |
| source | node `label` if present else node id, with node `type` |
| target | same |
| where | `file:line` from `location` |

Header line: paper, counts by rule, `cycle_scc_count`, number of verdicts.

Keys: `r` mark right, `w` mark wrong, `u` clear (remove this edge's
verdicts), `n`/`p` move, `g` reload from disk, `RET` jump to source, `q`
quit. A verdict is written **immediately** to the review JSON: append
`{"edge_id", "verdict": "right"|"wrong", "at": ISO-8601 UTC}` to `verdicts`
(clear removes all entries for that edge), preserving every other key
byte-for-byte in value (re-serialise with `json-serialize`; do not reorder
nodes/edges). Write atomically (temp file + rename) so a crash never leaves a
truncated review file.

`RET`: the `location.file` is a member of the paper's eprint tarball
(`scripts/futon6_config.py:eprints()` directory, `<paper>.tar.gz`). Extract
once into `~/.cache/futon6/eprints/<paper>/` (never `/tmp`), then
`find-file-other-window` at `location.line`. When the tarball is missing,
message the path looked for.

## Tests (ERT, run with `emacs -Q --batch -l ert -l emacs/paper-dependency-review.el -l emacs/test/paper-dependency-review-test.el -f ert-run-tests-batch-and-exit`)

On a temp copy of a tiny fixture JSON written by the test (three edges, one
cycle pair):
- rendering: three rows; the two cycle rows carry the `error` face on the
  edge column, the third does not;
- `r` on a row appends one verdict with that `edge_id` and an `at` that
  parses; a second `w` on the same row makes the latest verdict `wrong` and
  the displayed glyph `✗`; `u` leaves no verdict for that edge;
- after a write, the file's `nodes` and `edges` are equal to the fixture's;
- a missing tarball on `RET` signals a user-error naming the path (no crash).

Gates: `futon4/dev/check-parens.el` on both files; the ERT run green.

## Acceptance

Open `2002.06055` (6 cyclic SCCs, definition ↔ proposition mutual statement
refs, e.g. `def:hom-eval` ↔ `proposition:eval-cubical`) and report: row
count, how many rows are red, and that `r` on one row writes a verdict you
can see with `git diff` on the review JSON (then revert that diff; verdicts
are Joe's). Bell claude-16 back with the sha, the ERT line, and the counts.
