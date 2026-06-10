#!/usr/bin/env bash
# Fast scope-lane reingest for one mission doc: re-detect (python) + per-binder
# ingest through the live futon3c JVM via Drawbridge (no JVM spin-up).
# Usage: mission-scope-reingest.sh <path-to-mission.md>
# (The ingest's -main has no System/exit, so it is Drawbridge-safe; the
#  per-binder route is deliberate — plain full-run skips enrichments, W2′.)
set -euo pipefail

doc="$1"
mission="$(basename "$doc" .md)"

cd /home/joe/code/futon6
python3 scripts/mission_scope_detect.py "$doc"

binders=$(python3 - "$mission" <<'EOF'
import json, sys
d = json.load(open(f"/home/joe/code/futon6/data/mission-scope-trees/{sys.argv[1]}.json"))
print(" ".join(sorted(d["scope-count-by-binder-type"].keys())))
EOF
)

cd /home/joe/code/futon3c
for b in $binders; do
  bash scripts/proof-eval.sh "(do (when-not (find-ns 'futon3c.scripts.mission-scope-ingest)
        (load-file \"src/futon3c/scripts/mission_scope_ingest.clj\"))
      (with-out-str
        (futon3c.scripts.mission-scope-ingest/-main \"--binder\" \"$b\" \"$mission\"))
      :ok)" >/dev/null
  echo "[reingest] $mission $b ok"
done
echo "[reingest] $mission complete (${binders})"
