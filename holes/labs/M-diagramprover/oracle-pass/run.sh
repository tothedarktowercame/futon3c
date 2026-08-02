#!/usr/bin/env bash
set -euo pipefail

ORACLE_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$ORACLE_DIR/../../../.." && pwd)"
ORACLE_PYTHON="/home/joe/.venvs/causal-oracles/bin/python"

if [[ ! -x "$ORACLE_PYTHON" ]]; then
  echo "missing oracle venv: $ORACLE_PYTHON" >&2
  exit 2
fi

cd "$REPO_DIR"
clojure -M -e "(load-file \"$ORACLE_DIR/export.clj\") (export/-main)"
"$ORACLE_PYTHON" "$ORACLE_DIR/oracle_check.py"
Rscript "$ORACLE_DIR/oracle_check.R"
clojure -M -e "(load-file \"$ORACLE_DIR/export.clj\") (export/verify-converse)"
"$ORACLE_PYTHON" "$ORACLE_DIR/report.py"
