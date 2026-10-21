#!/usr/bin/env bash
set -euo pipefail

repo_root=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
mathlib_root=${MATHLIB4_ROOT:-/home/joe/code/mathlib4}
source_root=${MATHLIB4_SOURCE_ROOT:-$mathlib_root}
lean_path_prefix=${R20_LEAN_PATH_PREFIX:-}
exporter=DarkTower/WarMachine/R20WitnessExport.lean
expected="$repo_root/holes/labs/M-wm-wiring/r20-lean-edges.edn"
generated=$(mktemp "${TMPDIR:-/tmp}/r20-lean-edges.XXXXXX")
trap 'rm -f "$generated"' EXIT

source_commit=$(git -C "$source_root" log -1 --format=%H -- \
  DarkTower/WarMachine/R20Graph.lean \
  DarkTower/WarMachine/R20Witness.lean \
  "$exporter")

{
  printf ';; Generated from mathlib4 commit %s\n' "$source_commit"
  if [[ -n "$lean_path_prefix" ]]; then
    lean_path=$(cd "$mathlib_root" && lake env printenv LEAN_PATH)
    (cd "$mathlib_root" && lake env env \
      LEAN_PATH="$lean_path_prefix:$lean_path" lean "$source_root/$exporter")
  else
    (cd "$mathlib_root" && lake env lean "$source_root/$exporter")
  fi
} > "$generated"

diff -u "$expected" "$generated"
