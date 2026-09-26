#!/usr/bin/env bash
set -euo pipefail

HERE="holes/labs/M-memory-retrieval/falsification-with-data"
ORACLE_PYTHON="/home/joe/.venvs/causal-oracles/bin/python"

[[ "$(sha256sum holes/labs/M-memory-retrieval/receipts-export-20260731-all-authors.edn | cut -c1-8)" == "0cc527e2" ]]
clojure -M -e "(load-file \"$HERE/project.clj\") (project/-main)"
Rscript "$HERE/dagitty_check.R"
"$ORACLE_PYTHON" "$HERE/dowhy_check.py"
"$ORACLE_PYTHON" "$HERE/report.py"
