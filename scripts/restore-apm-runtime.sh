#!/usr/bin/env bash
# Restore the shared APM runtime only from canonical, clean master.
set -euo pipefail

ROOT="$(cd "$(dirname "$0")/.." && pwd -P)"
cd "$ROOT"

if [[ "$(git branch --show-current)" != master ]]; then
  echo 'REFUSED: canonical futon3c checkout is not on master' >&2
  exit 2
fi
if [[ -n "$(git status --porcelain -- src/futon3c/apm src/futon3c/transport/http.clj)" ]]; then
  echo 'REFUSED: APM/HTTP runtime sources are not clean' >&2
  exit 2
fi

# A stable dependency order: repeatedly choose namespaces whose local APM
# requirements have already been chosen. Cycles retain lexical order.
mapfile -t NS_ROWS < <(python3 - <<'PY'
import pathlib, re
root = pathlib.Path('src/futon3c/apm')
items = {}
for path in sorted(root.glob('*.clj')):
    text = path.read_text()
    m = re.search(r'\(ns\s+([^\s\)]+)', text)
    if not m: continue
    ns = m.group(1)
    deps = set(re.findall(r'\[\s*(futon3c\.apm\.[^\s\]\)]+)', text))
    items[ns] = (path, deps)
done = []
remaining = dict(items)
while remaining:
    ready = sorted(n for n, (_, ds) in remaining.items()
                   if not (ds & set(remaining)))
    if not ready: ready = [sorted(remaining)[0]]
    for n in ready:
        done.append(n); remaining.pop(n)
for n in done: print(f'{n}|{items[n][0]}')
PY
)
NS_ROWS+=("futon3c.transport.http|src/futon3c/transport/http.clj")

quoted=""
for row in "${NS_ROWS[@]}"; do
  ns="${row%%|*}"; path="${row#*|}"
  quoted+=" '$ns"
  resource="${path#src/}"
  CHECK="(do (require '[clojure.java.io :as io]) (let [u (io/resource \"$resource\") expected \"$ROOT/$path\"] (when-not (= expected (some-> u .getPath)) (throw (ex-info \"runtime source mismatch\" {:namespace '$ns :expected expected :observed (some-> u .getPath)}))) (require '$ns :reload) true))"
  RESULT="$(printf '%s' "$CHECK" | scripts/proof-eval.sh -)"
  if [[ "$RESULT" != *":ok true"* ]]; then
    echo "REFUSED: failed restoring $ns: $RESULT" >&2
    exit 3
  fi
done

if ! curl -fsS -o /dev/null http://localhost:7070/api/alpha/agents; then
  echo 'REFUSED: master-only Agency route probe failed after reload' >&2
  exit 3
fi

HEAD="$(git rev-parse HEAD)"
FORM="(do (require '[clojure.java.io :as io] 'futon3c.apm.durable-coordinator) (let [r (futon3c.apm.durable-coordinator/recover-all! \"data/apm-coordinators/registry.edn\") receipt {:restored (mapv str [$quoted]) :mismatched [] :readopted (vec (for [[id x] (:results r) :when (:ok x)] id)) :recovery-ok (:ok r)} path \"data/apm-runtime/restorations/$HEAD.edn\"] (io/make-parents path) (spit path (str (pr-str receipt) \"\\n\")) receipt))"
EVAL_RESULT="$(printf '%s' "$FORM" | scripts/proof-eval.sh -)"
if [[ "$EVAL_RESULT" != *":ok true"* ]]; then
  echo "REFUSED: restoration returned no receipt: $EVAL_RESULT" >&2
  exit 3
fi
cat "data/apm-runtime/restorations/${HEAD}.edn"
