#!/usr/bin/env bash
set -euo pipefail

ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

echo "[discipline-live] running gate harness..."
OUT="$(clojure -M scripts/discipline_live_gate.clj)"
echo "$OUT"

ARTIFACT_PATH="$(printf '%s\n' "$OUT" | sed -n 's/^ARTIFACT_PATH=//p' | tail -n1)"
if [[ -z "${ARTIFACT_PATH}" ]]; then
  echo "[discipline-live] failed: harness did not print ARTIFACT_PATH=..." >&2
  exit 1
fi

echo "[discipline-live] updating alleycat scorecard..."
clojure -M scripts/update_alleycat_scorecard.clj "$ARTIFACT_PATH"
echo "[discipline-live] done"
