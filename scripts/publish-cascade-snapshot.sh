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
OUT="${CASCADE_OUT:-/var/www/zone.hyperreal.enterprises/wip/pipeline-pattern-cascade.html}"
TRIES="${CASCADE_TRIES:-6}"
WORK="$(mktemp -d)"
trap 'rm -rf "$WORK"' EXIT

# The cascade endpoints are flaky on cold substrate reads (five sequential
# hyperedge fetches under a per-request deadline). Retry with backoff rather
# than publishing a page that says "offline".
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

python3 - "$SRC" "$WORK" "$OUT" "$BASE" <<'PY'
import json, pathlib, sys, datetime

src, work, out, base = sys.argv[1], pathlib.Path(sys.argv[2]), sys.argv[3], sys.argv[4]
html = pathlib.Path(src).read_text()

snapshot = {
    f"{base}/api/alpha/cascade-real":       json.loads((work / "summary.json").read_text()),
    f"{base}/api/alpha/cascade-real/graph": json.loads((work / "graph.json").read_text()),
    f"{base}/api/alpha/forward-model":      json.loads((work / "forward.json").read_text()),
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

pathlib.Path(out).write_text(html)
print(f"wrote {out} ({len(html)} bytes), snapshot at {stamp}")
PY
