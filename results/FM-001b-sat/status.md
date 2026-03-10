# FM-001b SAT status — 2026-03-10

| n | q = 2n-1 | CNF size (vars / clauses) | Solver | State (UTC) | Notes |
|---|-----------|--------------------------|--------|-------------|-------|
| 11 | 21 | 568,260 vars / 1,308,760 clauses | maplesat via `ramsey_book_sat.py` | running (launched 2026-03-10 03:49Z) | Witness target `results/FM-001b-sat/n11-witness.json`; SAT harness PID: see `ps` output in shell (`.venv/bin/python ... 11 --solver maplesat`). |
| 23 | 45 | TBD (CNF build next) | — | queued | Will kick off once n=11 completes. |
| 50 | 99 | TBD | — | queued | Needs Glucose/Kissat solve after n=23. |
