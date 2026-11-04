#!/usr/bin/env bash
# Run every declared ways-of-working probe except the two that
# publish-cascade-snapshot.sh runs itself (they need the fetched
# summary/graph). Each probe writes its own result file to the default
# results dir, ~/.cache/futon3c/ways-of-working/results, which --assemble
# reads. Called by ways-of-working-probes.timer.
set -uo pipefail
DECL="${1:-/home/joe/code/futon3c/holes/excursions/ways-of-working.json}"
PROBE=/home/joe/code/futon3c/scripts/ways-of-working-probe.py

# the probe names come from the declaration, not this script
PUBLISH_RUNS="cascade-standards draw-pile"
mapfile -t names < <(python3 - "$DECL" "$PUBLISH_RUNS" <<'PY'
import json, sys
decl, skip = sys.argv[1], set(sys.argv[2].split())
d = json.load(open(decl))
for name in dict.fromkeys(w.get("probe") for w in d["ways"]):
    if name and name != "none" and name not in skip:
        print(name)
PY
)
if [ "${#names[@]}" -eq 0 ]; then
  echo "no declared probes to run in $DECL" >&2
  exit 0
fi
rc=0
for p in "${names[@]}"; do
  # one failing probe must not stop the others; its broken result is written
  python3 "$PROBE" "$DECL" --run "$p" || { echo "probe $p failed" >&2; rc=1; }
done
exit "$rc"
