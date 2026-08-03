#!/usr/bin/env bash
set -euo pipefail

SWEEP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SWEEP_DIR/../../../.." && pwd)"
ORACLE_PYTHON="/home/joe/.venvs/causal-oracles/bin/python"

if [[ ! -x "$ORACLE_PYTHON" ]]; then
  echo "missing oracle venv: $ORACLE_PYTHON" >&2
  exit 2
fi

cd "$REPO_DIR"
clojure -M -e "(load-file \"$SWEEP_DIR/sweep.clj\") (sweep/-main)"
"$ORACLE_PYTHON" "$SWEEP_DIR/oracle_check.py"
Rscript "$SWEEP_DIR/oracle_check.R"
"$ORACLE_PYTHON" "$SWEEP_DIR/report.py"

cd "$SWEEP_DIR"
sha256sum REPORT-mfuton-sweep.md engine-results.json python-results.json \
  r-results.json sweep-results.json > SHA256SUMS
