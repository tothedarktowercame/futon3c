#!/usr/bin/env bash
# Fast mission-scope view: run the projection inside the live futon3c JVM via
# Drawbridge (~1s) instead of spinning a fresh JVM (~20s). Prints the same
# JSON as `clojure -M -m futon3c.scripts.mission-scope-view --mission M`.
# NOTE: calls the view's inner fns, NOT its -main (which System/exit's — fatal
# through Drawbridge). Falls back to the slow JVM if Drawbridge is down.
set -euo pipefail

mission=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    --mission) mission="$2"; shift 2 ;;
    *) shift ;;
  esac
done
[[ -n "$mission" ]] || { echo "usage: $0 --mission <mission-id>" >&2; exit 2; }

cd /home/joe/code/futon3c

expr="(do (when-not (find-ns 'futon3c.scripts.mission-scope-view)
        (load-file \"src/futon3c/scripts/mission_scope_view.clj\"))
      (let [client (java.net.http.HttpClient/newHttpClient)
            hxs (mapcat #(#'futon3c.scripts.mission-scope-view/hyperedges-by-type
                          client \"http://localhost:7071\" %)
                        futon3c.scripts.mission-scope-view/structural-binders)]
        (cheshire.core/generate-string
         (futon3c.scripts.mission-scope-view/project-hyperedges \"$mission\" hxs))))"

if out=$(bash scripts/proof-eval.sh "$expr" 2>/dev/null) && [[ "$out" == *":ok true"* ]]; then
  python3 - "$out" <<'EOF' && exit 0
import sys, re
s = sys.argv[1]
m = re.search(r':value "(.*)"}\s*$', s, re.S)
if not m:
    sys.exit(1)
# Unescape the pr-str'd string in one pass (\" \\ \n).
print(re.sub(r'\\(["\\n])',
             lambda mm: {'"': '"', '\\': '\\', 'n': '\n'}[mm.group(1)],
             m.group(1)))
EOF
fi

# Fallback: slow standalone JVM.
exec clojure -M -m futon3c.scripts.mission-scope-view --mission "$mission"
