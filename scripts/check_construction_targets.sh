#!/usr/bin/env bash
# Assert every ConstructionTargets module is REACHABLE, i.e. imported by the
# library root so `lake build` produces an olean for it.
#
# WHY THIS EXISTS. Twice on 2026-07-30 a construction lemma was proved and then
# could not be used, because the module was not on the import path:
#   - the whole directory had no `lean_lib` entry and no root module, so four
#     proved lemmas sat unreachable for two days (fixed in apm-lean a270a2a);
#   - hours later a new file, UnivalentDeriv.lean, was added and compiled green
#     standalone while still not imported by the root (fixed in dda4556).
# `lake env lean <file>` exiting 0 does NOT mean the module is reachable — that
# is the whole trap. Both incidents were logged in the register and the second
# happened anyway, so this is the mechanism rather than another note.
set -euo pipefail

APM=${APM:-/home/joe/code/apm-lean}
ROOT="$APM/ConstructionTargets.lean"
DIR="$APM/ConstructionTargets"

[ -f "$ROOT" ] || { echo "FAIL: no library root at $ROOT"; exit 1; }
grep -q '^\[\[lean_lib\]\]' "$APM/lakefile.toml" || { echo "FAIL: no lean_lib in lakefile"; exit 1; }
grep -q 'name = "ConstructionTargets"' "$APM/lakefile.toml" \
  || { echo "FAIL: lakefile has no ConstructionTargets lean_lib"; exit 1; }

missing=0
for f in "$DIR"/*.lean; do
  mod=$(basename "$f" .lean)
  if ! grep -q "^import ConstructionTargets\.$mod\b" "$ROOT"; then
    echo "UNREACHABLE: ConstructionTargets/$mod.lean is not imported by the root"
    echo "   fix: add 'import ConstructionTargets.$mod' to $ROOT"
    missing=$((missing + 1))
  fi
done

# A stale import is the mirror failure: root names a module that no longer exists.
while read -r mod; do
  [ -f "$DIR/$mod.lean" ] || { echo "DANGLING: root imports $mod but the file is gone"; missing=$((missing+1)); }
done < <(sed -n 's/^import ConstructionTargets\.\([A-Za-z0-9_]*\).*/\1/p' "$ROOT")

if [ "$missing" -eq 0 ]; then
  n=$(ls -1 "$DIR"/*.lean 2>/dev/null | wc -l)
  echo "OK: all $n ConstructionTargets modules are imported by the root and reachable"
else
  echo "FAIL: $missing unreachable or dangling module(s)"; exit 1
fi
