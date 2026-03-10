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

## 2026-03-10 update (Codex)
- Spun up the structured IP encoding (`data/frontiermath-pilot/FM-001b-ip-encoding.py 50`) at 04:24Z using PuLP + CBC on futon6.
- Model summary: `n=50`, `q=99`, `|V|=198`, `Vars=67,719`, `Constraints=203,058`.
- CBC is still solving (time limit 3600s). Once it produces a feasible `D11/D12` pair the witness will be converted into the requested 198-vertex adjacency string and checked in.
- Local node (futon3c) status @ 04:55Z:
  - `ramsey_book_sat.py 11 --solver glucose4 --witness-out ...` rebuilt and relaunched; solver still running but produces no intermediate logs.
  - `ramsey_book_sat.py 20 --no-solve` reproduced the earlier scaling numbers (`vars=6,846,840`, `clauses=14,834,896`).
  - `ramsey_book_sat.py 30 --no-solve` builder pegged CPU with no `[build]` line after several minutes; job killed to keep the box responsive.
  - `timeout 15 ramsey_book_sat.py 50 --no-solve` produced no output before the timeout (exit 124).
- The IP harness referenced above (`data/frontiermath-pilot/FM-001b-ip-encoding.py`) is not present in this checkout, so the structured-CBC path cannot currently run on futon3c. Need either that script committed or remote access to the futon6 run before Tier-2 can proceed locally.
