#!/usr/bin/env bash
# Publish the live pipeline-pattern-cascade as a SELF-CONTAINED page under the
# Caddy docroot, with its data inlined.
#
# Why a snapshot rather than the live-fetching page: the source HTML fetches
# http://localhost:7070. Served over HTTPS from zone.hyperreal.enterprises that
# is blocked as mixed content, and "localhost" would mean the *visitor's*
# machine, not this host. So we fetch here, inline the JSON, and publish static.
#
# Zero hand-typed rows: every number comes from the live endpoints below.
set -euo pipefail

BASE="${CASCADE_BASE:-http://localhost:7070}"
SRC="${CASCADE_SRC:-/home/joe/code/futon3c/holes/excursions/pipeline-pattern-cascade-live.html}"
WIP="${CASCADE_WIP:-/home/joe/code/futon3c/holes/excursions/wip-cards.json}"
THESIS="${CASCADE_THESIS:-/home/joe/code/futon3c/holes/excursions/apex-thesis.json}"
ARGUMENT_DIR="${CASCADE_ARGUMENT_DIR:-/home/joe/code/futon7/holes/argument-graph}"
OUT="${CASCADE_OUT:-/var/www/zone.hyperreal.enterprises/wip/pipeline-pattern-cascade.html}"
TRIES="${CASCADE_TRIES:-6}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# Cold substrate reads may degrade individual graph sections. HTTP 200 is still
# a valid honest-hole snapshot; the renderer makes each failed section visible.
fetch() {
  local name="$1" path="$2" required="$3"
  local dest="$WORK/$name.json"
  local i=1 code
  while [ "$i" -le "$TRIES" ]; do
    code=$(curl -s -m 90 -o "$dest" -w '%{http_code}' "$BASE$path" || echo 000)
    if [ "$code" = "200" ]; then
      echo "  $path -> HTTP 200 ($(stat -c%s "$dest") bytes, try $i)" >&2
      return 0
    fi
    echo "  $path -> HTTP $code (try $i/$TRIES): $(head -c 120 "$dest" 2>/dev/null)" >&2
    i=$((i + 1))
    sleep $((i * 3))
  done
  if [ "$required" = "required" ]; then
    echo "FATAL: $path never returned 200 in $TRIES tries; refusing to publish a stale page." >&2
    exit 1
  fi
  echo "  $path optional - omitted from snapshot" >&2
  echo "null" > "$dest"
}

echo "Fetching live cascade from $BASE ..." >&2
fetch summary "/api/alpha/cascade-real"       required
fetch graph   "/api/alpha/cascade-real/graph" required
fetch forward "/api/alpha/forward-model"      optional

WOW_DECL="${CASCADE_WOW:-/home/joe/code/futon3c/holes/excursions/ways-of-working.json}"
WOW_PROBE="${CASCADE_WOW_PROBE:-/home/joe/code/futon3c/scripts/ways-of-working-probe.py}"
echo "Probing the two ways that need the fetched data; assembling the rest from result files ..." >&2
# Only cascade-standards and draw-pile read the summary/graph fetched above.
# The other probes are refreshed on their own cadence by
# ways-of-working-probes.timer; --assemble reads their result files.
wow_ok=1
for p in cascade-standards draw-pile; do
  python3 "$WOW_PROBE" "$WOW_DECL" --run "$p" --summary "$WORK/summary.json" --graph "$WORK/graph.json" >&2 || wow_ok=0
done
if [ "$wow_ok" = 1 ] && ! python3 "$WOW_PROBE" "$WOW_DECL" "$WORK/wow.json" --assemble >&2; then
  wow_ok=0
fi
if [ "$wow_ok" != 1 ]; then
  echo "  ways-of-working probe failed; the panel will say so" >&2
  echo 'null' > "$WORK/wow.json"
fi

# regrow the tickets tree from the latest placements and the graph just fetched (claude-16's script, in the repo since 2eb1a4187)
python3 "$(dirname "$0")/partition-tree.py" "$WORK/graph.json" "$WORK/partition-tree.json" >&2 || { echo "FATAL: partition-tree.py failed" >&2; exit 1; }

# The argument labelling is live when Babashka is available to recompute it.
# Keep the committed report as a typed, visibly marked snapshot fallback for
# stripped-down timer environments.
argument_live=false
if command -v bb >/dev/null 2>&1 && (cd "$ARGUMENT_DIR" && bb report.bb apex.edn >/dev/null); then
  argument_live=true
else
  echo "  argument report could not be regenerated; using committed report.json as a snapshot" >&2
fi
cp "$ARGUMENT_DIR/report.json" "$WORK/argument-report.json"
if command -v bb >/dev/null 2>&1; then
  bb -e '(require (quote [cheshire.core :as json]) (quote [clojure.edn :as edn]))
          (let [g (edn/read-string (slurp (first *command-line-args*)))]
            (print (json/generate-string (select-keys g [:nodes :edges]))))' \
    "$ARGUMENT_DIR/apex.edn" > "$WORK/argument-source.json"
else
  echo 'null' > "$WORK/argument-source.json"
fi
printf '{"live":%s}\n' "$argument_live" > "$WORK/argument-meta.json"

python3 - "$SRC" "$WIP" "$THESIS" "$WORK" "$OUT" "$BASE" <<'PY'
import json, pathlib, sys, datetime

src, wip, thesis, work, out, base = sys.argv[1], pathlib.Path(sys.argv[2]), pathlib.Path(sys.argv[3]), pathlib.Path(sys.argv[4]), sys.argv[5], sys.argv[6]
html = pathlib.Path(src).read_text()
graph = json.loads((work / "graph.json").read_text())
section_status = graph.get("section-status")
if not isinstance(section_status, dict):
    sys.exit("FATAL: graph snapshot has no section-status; refusing an ambiguous page.")
failed = sorted(k for k, v in section_status.items() if v.get("status") != "ok")
print("graph section status: " + ("all reads OK" if not failed else "FAILED " + ", ".join(failed)))

snapshot = {
    f"{base}/api/alpha/cascade-real":       json.loads((work / "summary.json").read_text()),
    f"{base}/api/alpha/cascade-real/graph": graph,
    f"{base}/api/alpha/forward-model":      json.loads((work / "forward.json").read_text()),
    "wip-cards.json":                       json.loads(wip.read_text()),
    "apex-thesis.json":                     json.loads(thesis.read_text()),
    "ways-of-working-live.json":            json.loads((work / "wow.json").read_text()),
    "giraffe-partition-2026-11-02.json":    json.loads(pathlib.Path("/home/joe/code/futon3c/holes/excursions/giraffe-partition-2026-11-02.json").read_text()),
    "layers.json":                          json.loads(pathlib.Path("/home/joe/code/futon3c/holes/excursions/layers.json").read_text()),
    "intermediate-theses.json":             json.loads(pathlib.Path("/home/joe/code/futon3c/holes/excursions/intermediate-theses.json").read_text()),
    "argument-report.json":                 json.loads((work / "argument-report.json").read_text()),
    "argument-source.json":                 json.loads((work / "argument-source.json").read_text()),
    "argument-meta.json":                   json.loads((work / "argument-meta.json").read_text()),
    # claude-16's tickets tree (status > way or wing > cluster > item; requisitions apart; repair tickets one class;
    # unplaced live), regrown from A1-placements.tsv just before inlining (Joe's judgement on the Tickets section, 2026-11-02)
    "partition-tree.json":                  json.loads((work / "partition-tree.json").read_text()),
}
stamp = datetime.datetime.now().astimezone().isoformat(timespec="seconds")

anchor = 'var BASE="http://localhost:7070";'
if anchor not in html:
    sys.exit("FATAL: BASE anchor not found - the source page changed shape.")

# Inline the data and short-circuit requestJSON. The renderer is untouched:
# every panel still reads exactly the JSON the live endpoints returned.
html = html.replace(anchor, anchor + "\n  var CASCADE_SNAPSHOT=" + json.dumps(snapshot)
                    + ";\n  var SNAPSHOT_AT=" + json.dumps(stamp) + ";", 1)

hook = "  function requestJSON(name,url,optional){"
if hook not in html:
    sys.exit("FATAL: requestJSON hook not found - the source page changed shape.")
html = html.replace(hook, hook + """
    if(typeof CASCADE_SNAPSHOT!=="undefined" && Object.prototype.hasOwnProperty.call(CASCADE_SNAPSHOT,url)){
      var snap=CASCADE_SNAPSHOT[url];
      if(snap===null && !optional) return Promise.reject(new Error(name+" absent from snapshot"));
      return Promise.resolve(snap);
    }""", 1)

html = html.replace("<title>Pipeline Pattern Cascade — LIVE (regenerated)</title>",
                    "<title>Pipeline Pattern Cascade — snapshot</title>", 1)
html = html.replace("</header>",
                    '<div class="sub">Static snapshot of the live cascade, generated '
                    + stamp + ' from <code>' + base + '</code>. '
                    + 'Regenerate: <code>futon3c/scripts/publish-cascade-snapshot.sh</code></div></header>', 1)

# atomic publish: never let a reader see a half-written page
import os
out_path = pathlib.Path(out)
tmp_out = out_path.with_name(out_path.name + ".tmp." + str(os.getpid()))
tmp_out.write_text(html)
os.replace(tmp_out, out_path)
print(f"wrote {out} ({len(html)} bytes), snapshot at {stamp}")
PY
