#!/usr/bin/env bash
# Focused Lean 4.29 gate for M-formal-arxana F3a. Never rebuilds Mathlib.
set -euo pipefail

here="$(cd "$(dirname "$0")/.." && pwd)"
apm_root="${1:?usage: formal-f3a-lean-gate.sh APM_WORKTREE OUTPUT.edn}"
output="${2:?usage: formal-f3a-lean-gate.sh APM_WORKTREE OUTPUT.edn}"
packages="${APM_LEAN_PACKAGES:-/home/joe/code/apm-lean/.lake/packages}"
created_link=0

[ -f "$apm_root/lean-toolchain" ] || { echo "missing apm-lean worktree" >&2; exit 1; }
[ -d "$packages/mathlib" ] || { echo "missing pinned Mathlib package" >&2; exit 1; }
mkdir -p "$apm_root/.lake"
if [ ! -e "$apm_root/.lake/packages" ]; then
  ln -s "$packages" "$apm_root/.lake/packages"
  created_link=1
fi
[ "$(readlink -f "$apm_root/.lake/packages")" = "$(readlink -f "$packages")" ] \
  || { echo "REFUSED: worktree package link has an unexpected target" >&2; exit 1; }

cleanup() {
  if [ "$created_link" -eq 1 ]; then rm "$apm_root/.lake/packages"; fi
}
trap cleanup EXIT INT TERM

cd "$here"
clojure -M -m futon3c.arxana.formal-f3 lean-gate "$apm_root" "$output"
