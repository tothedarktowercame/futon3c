#!/usr/bin/env bash
# APM process tap: sample what the campaign and its substrate are doing, to a
# JSONL trajectory that can be read AFTER the fact.
#
# Joe, 2026-09-07: "if it's possible to add measurements or a tap ... that can
# make sense of these processes in future rounds. We can do that. But right now
# it seems progress is being made. It's just that I lack the visibility that I
# would need to give any confidence on that."
#
# WHY A SAMPLER AND NOT IN-JVM INSTRUMENTATION. Every phase file already holds
# the current state; what nothing holds is its TRAJECTORY. When f188's
# promotion failed on :memory-snapshot-visibility-not-obtained, answering "was
# the substrate saturated at the time, and for how long?" needed numbers nobody
# had kept. Diagnosing it live meant hand-timing curls, and two of my readings
# were wrong precisely because a one-shot sample cannot distinguish service
# time from queue wait. A cheap external sampler fixes that for next time
# without touching the campaign's own code paths.
#
#   apm-tap.sh run [interval_s]   append samples (default 15s)
#   apm-tap.sh report [minutes]   summarise the last N minutes (default 30)
#   apm-tap.sh probes [phase]     raw per-sample probe series for PHASE, by
#                                 frame, for before/after comparison
set -uo pipefail
F3C=/home/joe/code/futon3c
TAP="$F3C/data/apm-tap/tap.jsonl"
CAMP="${VOXTERM_APM_CAMPAIGN:-jit-all-open-v3}"
CDIR="$F3C/data/apm-campaigns/$CAMP"

sample() {
python3 - "$CDIR" "$CAMP" <<'PY'
import json, os, re, sys, time, urllib.request
cdir, camp = sys.argv[1], sys.argv[2]
def get(url, t=3):
    try:
        with urllib.request.urlopen(url, timeout=t) as r:
            return r.read().decode("utf-8", "replace")
    except Exception:
        return ""
def read(p, tail=20000):
    try:
        with open(p, "rb") as f:
            f.seek(0, 2); n = f.tell(); f.seek(max(0, n - tail))
            return f.read().decode("utf-8", "replace")
    except Exception:
        return ""

# newest frame dir
frames = []
for d in os.listdir(cdir):
    m = re.match(re.escape(camp) + r"-f(\d+)$", d)
    if m: frames.append((int(m.group(1)), os.path.join(cdir, d)))
frames.sort()
row = {"at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())}
if frames:
    num, fdir = frames[-1]
    row["frame"] = "f%d" % num
    live = os.path.join(fdir, "live")
    # phase = newest *.edn in live/, which is what the frame is working on
    newest, newest_m = None, 0
    try:
        for fn in os.listdir(live):
            if fn.endswith(".edn"):
                m = os.path.getmtime(os.path.join(live, fn))
                if m > newest_m: newest, newest_m = fn[:-4], m
    except Exception: pass
    row["phase"] = newest
    row["phase_age_s"] = int(time.time() - newest_m) if newest_m else None
    # cascade trajectory: the stage/seed-count pair is what moves during a wave
    casc = read(os.path.join(live, "memory-cascade-operation.edn"))
    if casc:
        st = re.search(r':status :([a-z-]+)', casc)
        stg = re.search(r':stage :([a-z-]+)', casc)
        sc = re.search(r':seed-count (\d+)', casc)
        row["cascade"] = {"status": st.group(1) if st else None,
                          "stage": stg.group(1) if stg else None,
                          "seed_count": int(sc.group(1)) if sc else None}
    if newest:
        pf = read(os.path.join(live, newest + ".edn"))
        stg = re.search(r':stage :([a-z-]+)', pf)
        ec = re.search(r':error/code :([a-z-/]+)', pf)
        ra = re.search(r':transport-retry/attempt (\d+)', pf)
        if stg or ec or ra:
            row["phase_state"] = {"stage": stg.group(1) if stg else None,
                                  "error_code": ec.group(1) if ec else None,
                                  "retry_attempt": int(ra.group(1)) if ra else None}
# substrate permits: the number that explained the promotion failure
h = get("http://127.0.0.1:7073/health")
if h:
    tot = re.search(r':permits/total (\d+)', h)
    av = re.search(r':permits/available (\d+)', h)
    row["substrate"] = {"permits_total": int(tot.group(1)) if tot else None,
                        "permits_free": int(av.group(1)) if av else None,
                        "holders": len(re.findall(r':age-ms (\d+)', h)),
                        "holder_ages_ms": [int(x) for x in re.findall(r':age-ms (\d+)', h)][:4]}
# a real timed read, so service time is separated from queue wait
t0 = time.time()
probe = get("http://127.0.0.1:7073/api/alpha/hyperedges?type=memory&limit=1&include-total=false", t=10)
row["substrate_probe_ms"] = int((time.time() - t0) * 1000) if probe else None
# jvm memory, from the agency's O(1) health block
ah = get("http://127.0.0.1:7070/health")
if ah:
    try:
        j = (json.loads(ah) or {}).get("jvm") or {}
        row["jvm"] = {k: j.get(k) for k in
                      ("direct-used-mb", "direct-max-mb", "direct-count", "heap-used-mb")}
    except Exception: pass
# coordinator ticks: frozen ticks + a working phase is normal, and only a
# trajectory can tell that apart from a wedge
co = read(os.path.join(cdir, "coordinator.edn"), tail=4000)
t = re.search(r':regulator/ticks (\d+)', co)
st = re.search(r':regulator/status :([a-z-]+)', co)
row["regulator"] = {"ticks": int(t.group(1)) if t else None,
                    "status": st.group(1) if st else None}
print(json.dumps(row))
PY
}

case "${1:-run}" in
run)
  iv="${2:-15}"
  mkdir -p "$(dirname "$TAP")"
  echo "[apm-tap] sampling $CAMP every ${iv}s -> $TAP"
  while true; do s=$(sample); [ -n "$s" ] && echo "$s" >> "$TAP"; sleep "$iv"; done ;;
report)
  mins="${2:-30}"
  [ -f "$TAP" ] || { echo "no tap at $TAP"; exit 1; }
  python3 - "$TAP" "$mins" <<'PY'
import json, sys, time, collections
path, mins = sys.argv[1], int(sys.argv[2])
cut = time.time() - mins * 60
rows = []
for line in open(path):
    try: r = json.loads(line)
    except Exception: continue
    try: ts = time.mktime(time.strptime(r["at"], "%Y-%m-%dT%H:%M:%SZ"))
    except Exception: continue
    if ts >= cut: rows.append(r)
if not rows:
    print("  no samples in the last %d min" % mins); raise SystemExit
print("  window: %s .. %s  (%d samples)" % (rows[0]["at"], rows[-1]["at"], len(rows)))
ph = collections.Counter(r.get("phase") for r in rows)
print("  phases:", dict(ph))
tick = [r["regulator"]["ticks"] for r in rows if r.get("regulator", {}).get("ticks")]
if tick: print("  ticks: %d -> %d (delta %d)" % (tick[0], tick[-1], tick[-1] - tick[0]))
casc = [r["cascade"] for r in rows if r.get("cascade")]
if casc:
    print("  cascade: %s/%s seeds %s -> %s" % (
        casc[-1].get("status"), casc[-1].get("stage"),
        casc[0].get("seed_count"), casc[-1].get("seed_count")))
pr = [r["substrate_probe_ms"] for r in rows if r.get("substrate_probe_ms") is not None]
if pr:
    pr_s = sorted(pr)
    print("  substrate probe ms: min %d  p50 %d  p90 %d  max %d" % (
        pr_s[0], pr_s[len(pr_s)//2], pr_s[int(len(pr_s)*0.9)], pr_s[-1]))
    print("  probes over the 5000ms visibility bound: %d/%d" % (sum(1 for x in pr if x > 5000), len(pr)))
sat = [r for r in rows if r.get("substrate", {}).get("permits_free") == 0]
print("  substrate saturated (0 permits free): %d/%d samples" % (len(sat), len(rows)))
dm = [r["jvm"]["direct-used-mb"] for r in rows if r.get("jvm", {}).get("direct-used-mb") is not None]
if dm: print("  jvm direct mb: %d -> %d (max %d of %s)" % (
    dm[0], dm[-1], max(dm), rows[-1].get("jvm", {}).get("direct-max-mb")))
errs = collections.Counter(r["phase_state"]["error_code"] for r in rows
                           if r.get("phase_state", {}).get("error_code"))
if errs: print("  phase errors seen:", dict(errs))
PY
  ;;
probes)
  # claude-7, 2026-09-07, on the promotion self-contention fix: "keep the raw
  # probe series. I want a before/after on (3) rather than my own six-sample
  # curl."
  #
  # `report` collapses probes to percentiles over a time window, and percentiles
  # cannot show QUEUEING. Queueing shows only in the raw ordered series read
  # next to permits_free: service time stays flat while the wait grows behind a
  # full permit pool. bounded-visibility-mapv batches by CANDIDATE while
  # candidate-visible? makes 3+ substrate requests each, so ~6 requests contend
  # for futon1b's 2 permits against a 5000ms per-read bound. This emits the
  # evidence per frame, so the same command after the fix is directly
  # comparable.
  phase="${2:-promote-solver}"
  [ -f "$TAP" ] || { echo "no tap at $TAP"; exit 1; }
  python3 - "$TAP" "$phase" <<'PROBES'
import json, sys, collections
path, phase = sys.argv[1], sys.argv[2]
runs = collections.OrderedDict()
for line in open(path):
    try: r = json.loads(line)
    except Exception: continue
    if r.get("phase") != phase: continue
    runs.setdefault(r.get("frame"), []).append(r)
if not runs:
    print("no samples for phase %r" % phase); raise SystemExit
print("# frame phase at probe_ms permits_free holders")
for frame, rows in runs.items():
    for r in rows:
        sub = r.get("substrate") or {}
        print("%s %s %s %s %s %s" % (frame, phase, r.get("at"),
              r.get("substrate_probe_ms"), sub.get("permits_free"),
              sub.get("holders")))
print()
for frame, rows in runs.items():
    pr = sorted(x for x in (r.get("substrate_probe_ms") for r in rows)
                if x is not None)
    sat = sum(1 for r in rows
              if (r.get("substrate") or {}).get("permits_free") == 0)
    if not pr:
        print("%-6s %s: no probes" % (frame, phase)); continue
    print("%-6s %-20s n=%-4d min=%-5d p50=%-5d p90=%-5d max=%-6d "
          "over-1000ms=%-3d over-5000ms-bound=%-3d saturated=%d/%d" % (
        frame, phase, len(pr), pr[0], pr[len(pr)//2], pr[int(len(pr)*0.9)],
        pr[-1], sum(1 for x in pr if x > 1000),
        sum(1 for x in pr if x > 5000), sat, len(rows)))
PROBES
  ;;
*) echo "usage: $0 run [interval_s] | report [minutes] | probes [phase]"; exit 2 ;;
esac
