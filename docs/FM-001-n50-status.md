# FM-001 / n=50 SAT harness status — 2026-03-09

## Process + log check
- 22:56Z `ps aux | grep python` showed only the long-running `scripts/corpus_ws_bridge.py`; no SAT job or FM-001 processes are active.
- `log/` only contains `sidecar-audit.edn`, and there are no `FM-001b` or FM harness logs anywhere in the repo tree, so the relaunch never emitted errors or witness artifacts locally.

## Harness scaling snapshot
- `.venv/bin/python scripts/fm001/ramsey_book_sat.py 10 --no-solve` → `vars=379,620` and `clauses=885,816`.
- `.venv/bin/python scripts/fm001/ramsey_book_sat.py 20 --no-solve` → `vars=6,846,840` and `clauses=14,834,896`.
- The builder adds ~392 helper variables per edge. For `n=50` (198 vertices, 19,503 edges) that is ~7.6M helpers even before cardinality encodings. Extrapolating the observed `O(n^4)` growth from the n=10/20 runs yields >2.6e8 variables and ≈6e8 clauses for n=50, well beyond what this host or Glucose can handle without sharding.

## Recommendation
Given the explosive CNF size, rerunning the direct SAT search at n=50 is impractical on the current node. Instead:
1. Focus on the composite-modulus queue at n ∈ {23, 26, 28, 29} to validate the 2-block-circulant pipeline with logging turned on (each remains 2–3 orders of magnitude smaller).
2. Extract structural constraints (max common-edge neighborhoods, Paley perturbations) and encode them manually for n=50 rather than enumerating every triangle; this becomes a dedicated “FM-001b manual strategy” issue.
3. Keep the SAT harness reserved for n ≤ 30 until we have a distributed solver or additional symmetry reductions.
