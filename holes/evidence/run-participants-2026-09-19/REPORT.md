# Issuer provenance retained

Implementation `5c4f17e1`, merged to master `80ed103d`.

HTTP-to-worker-to-writer controls: 1 test, 14 assertions, zero failures/errors.
Warrant: `test-registry-b1bcd8d5d44a3b1f9e6023390159a456f9fa07bcdd99f42977707e7363b44fa9` (HTTP check true).
Additional changed RUN4 boundary tests: 4 tests/29 assertions; commissioned
adapter: 6 tests/32 assertions, all pass. clj-kondo 0/0; check-parens OK.

Full semantics and control scope are documented in
`/home/joe/code/futon2/holes/labs/wm-contract/runs/run-participants-2026-09-19/REPORT.md`.

No click issued, no serving reload performed. Reload canonical master and its
changed Futon2 runner dependency to activate.
