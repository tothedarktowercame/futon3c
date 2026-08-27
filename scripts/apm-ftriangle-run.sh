#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd -P)
cd "$root"
config="$root/data/apm-campaigns/ftriangle-live-smoke-v1/config.edn"

if [[ ! -f "$config" ]]; then
  echo "REFUSED: missing isolated Ftriangle config: $config" >&2
  exit 2
fi

form="(do (require '[clojure.edn :as edn] 'futon3c.apm.ftriangle-live-smoke) (futon3c.apm.ftriangle-live-smoke/run-live! (edn/read-string (slurp \"$config\"))))"
result=$(printf '%s' "$form" | scripts/proof-eval.sh -)
printf '%s\n' "$result"

[[ "$result" == *":ok true"* ]]
