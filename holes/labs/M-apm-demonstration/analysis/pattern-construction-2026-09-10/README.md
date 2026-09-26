# Pattern construction evidence — 2026-09-10

Read `prelim-development.md` for the actual top-down construction, and
`topology-patterns.md` for the six cross-example topology candidates. The
technote is `holes/technotes/TN-pattern-warranted-proof-and-topology-induction-2026-09-10.md`.

- `prelim-sources.json`: nine consulted patterns and three development sources;
  the full retained solution was compared only after commit `5b53ebd2`.
- `topology-census.json`: all 138 topology bundles, all 794 ConstructionTargets
  source records, local dependency closures, and two sanitized ledger projections.
- `topology-induction.json`: analyst-selected witness excerpts, source hashes,
  all-61-root coverage, and reviewed completion dispositions.

Reproduce from this directory with Python 3, Git and Babashka installed, using
the canonical sibling checkouts with the pinned Git objects available:

```sh
python3 topology_census.py
python3 topology_induction.py
python3 freeze_prelim.py
python3 -m py_compile topology_census.py topology_induction.py freeze_prelim.py
clj-kondo --lint worklist_projection.bb
emacs -Q --batch -l /home/joe/code/futon4/dev/check-parens.el --eval '(arxana-check-parens-cli)' -- worklist_projection.bb
sha256sum topology-census.json topology-induction.json prelim-sources.json
```

Scripts overwrite only their generated JSON in this directory. Expected hashes
and completed checks are in `checks.txt`. They do not read live worklist bytes,
call services, dispatch agents, invoke Lean, or access evaluation holdouts.
Source scanning is not theorem verification. Candidate selection is explicit
analyst annotation, not an automatic pattern-discovery score. Import edges are
not asserted pattern-use edges. Retained author/reviewer findings are evidence
from the ledger, not fresh independent validation by this packet's author.
