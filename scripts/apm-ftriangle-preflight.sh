#!/usr/bin/env bash
set -euo pipefail

root=$(cd "$(dirname "$0")/.." && pwd -P)
cd "$root"

form="(do (require 'futon3c.apm.ftriangle-live-smoke) (futon3c.apm.ftriangle-live-smoke/preflight))"
result=$(printf '%s' "$form" | scripts/proof-eval.sh -)
printf '%s\n' "$result"

[[ "$result" == *":error/code :ftriangle-preconditions-unmet"* ]] && exit 2
[[ "$result" == *":ok true"* ]]
