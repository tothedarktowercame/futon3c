#!/usr/bin/env bash
# Fast mission-scope view: run the projection inside the live futon3c JVM via
# Drawbridge (~1s) instead of spinning a fresh JVM (~20s). Prints the same
# JSON as `clojure -M -m futon3c.scripts.mission-scope-view --mission M`.
# NOTE: calls the view's inner fns, NOT its -main (which System/exit's — fatal
# through Drawbridge). Falls back to the slow JVM if Drawbridge is down.
set -euo pipefail

mission=""
types=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --mission) mission="$2"; shift 2 ;;
    --types) types="$2"; shift 2 ;;
    *) shift ;;
  esac
done
[[ -n "$mission" ]] || { echo "usage: $0 --mission <mission-id>" >&2; exit 2; }

cd /home/joe/code/futon3c

# --types pattern,psr,pur fetches only those scope types (one substrate read
# each) -- the ※ m p view needs patterns only.
if [[ -n "$types" ]]; then
  [[ "$types" =~ ^[a-z0-9,-]+$ ]] || { echo "bad --types: $types" >&2; exit 2; }
  binders="[$(sed 's/[^,]*/"&"/g; s/,/ /g' <<<"$types")]"
  view="(futon3c.scripts.mission-scope-view/fetch-view \"$mission\" (futon3c.scripts.mission-scope-view/substrate-url) $binders)"
else
  view="(futon3c.scripts.mission-scope-view/fetch-view \"$mission\")"
fi

expr="(do (when-not (find-ns 'futon3c.scripts.mission-scope-view)
        (load-file \"src/futon3c/scripts/mission_scope_view.clj\"))
      (cheshire.core/generate-string
       $view))"

if out=$(bash scripts/proof-eval.sh "$expr" 2>/dev/null) && [[ "$out" == *":ok true"* ]]; then
  # The view is passed on stdin, not argv: a large mission's JSON exceeds
  # ARG_MAX ("Argument list too long"), and that bash error used to reach
  # Emacs mixed into the JSON (2026-11-19, M-arxana-incubator).
  printf '%s' "$out" | python3 -c '
import sys, re
s = sys.stdin.read()
m = re.search(r":value \"(.*)\"}\s*$", s, re.S)
if not m:
    sys.exit(1)
# Unescape the pr-str-ed string in one pass (\" \\ \n).
print(re.sub(r"\\([\"\\n])",
             lambda mm: {"\"": "\"", "\\": "\\", "n": "\n"}[mm.group(1)],
             m.group(1)))
' && exit 0
fi

# Fallback: slow standalone JVM.
exec clojure -M -m futon3c.scripts.mission-scope-view --mission "$mission"
