#!/usr/bin/env bash
set -euo pipefail

HERE="holes/labs/M-memory-retrieval/attachment-export"
PREVIOUS="${1:-}"

clojure -M -e "(load-file \"$HERE/export.clj\") (export/-main)"
python3 "$HERE/report.py"

if [[ -n "$PREVIOUS" ]]; then
  python3 "$HERE/compare_modulo_watermark.py" \
    "$PREVIOUS" "$HERE/attachment-state.json"
fi
