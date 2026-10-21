#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd)
output="$root/holes/labs/M-wm-wiring/R20WitnessFuton2.lean"
edges="${R20_LEAN_EDGES:-holes/labs/M-wm-wiring/r20-lean-edges.edn}"
cd "$root"

classpath=$(tr -d '\n' < "$HOME/notes/lean-led/review/s3a/cp.txt" \
  | sed "s#/home/joe/worktrees/futon3c-s3a-typed-ingest#$root#g")
java -cp "$classpath" clojure.main -m futon3c.diagramprover.r20-witness \
  "${R20_FIELD_FLOW_MAP:-holes/labs/M-wm-wiring/r20-field-flow.edn}" \
  "$edges" \
  "$output"

if [ -d "$HOME/code/mathlib4" ]; then
  (cd "$HOME/code/mathlib4" && lake env lean "$output")
fi
