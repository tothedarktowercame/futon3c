#!/usr/bin/env python3
"""Emit one line per notable APM frame event. Each stdout line is a wake signal.

Selective by design: only events a watcher would act on.
  FRAME-ACTIVE   a new frame directory appeared
  PHASE          a phase receipt landed (which, and its verdict tally)
  UNRESOLVED     a review pass carries :cannot-judge  <-- stop-the-line
  CERTIFIED      close-frame reached :live-job-certified
  BABYSIT        the babysitter logged an alert / queue transition
  REGULATOR      status changed, or :regulator/failures grew
Failure coverage: emits on regulator failure and on status leaving :running,
so a crash or stall is not silence.
"""
import os, re, glob, time, sys

CAMP = os.environ.get(
    "APM_CAMPAIGN",
    "/home/joe/code/futon3c/data/apm-campaigns/jit-all-open-nontopology-v1")
BABYLOG = os.environ.get("APM_BABYSIT_LOG", "")
REVIEW = ("promote-solver", "scribe-reduce",
          "guide-intervention-1-review", "guide-intervention-2-review")

def out(s):
    print(s, flush=True)

def read(p):
    try:
        return open(p, encoding="utf-8", errors="replace").read()
    except OSError:
        return ""

seen_frames, seen_phase, baby_pos = set(), {}, 0
last_emit = {}   # path -> last emitted line, so an in-progress record is not re-announced
last_status, last_fail_count = None, None
for d in glob.glob(os.path.join(CAMP, "*-f*")):
    seen_frames.add(d)
    for p in glob.glob(os.path.join(d, "live", "*.edn")):
        seen_phase[p] = os.path.getmtime(p)
if BABYLOG and os.path.exists(BABYLOG):
    baby_pos = os.path.getsize(BABYLOG)

while True:
    try:
        coord = os.path.join(CAMP, "coordinator.edn")
        c = read(coord)
        st = re.findall(r":regulator/status\s*(:[\w-]+)", c)
        st = st[-1] if st else None
        if st != last_status and last_status is not None:
            out(f"REGULATOR status {last_status} -> {st}")
        last_status = st
        nfail = len(re.findall(r":failed-at", c))
        if last_fail_count is not None and nfail > last_fail_count:
            m = re.findall(r':error/code\s*(:[\w/-]+)', c)
            out(f"REGULATOR failure recorded ({nfail}); latest code {m[-1] if m else '?'}")
        last_fail_count = nfail

        for d in sorted(glob.glob(os.path.join(CAMP, "*-f*"))):
            if not os.path.isdir(os.path.join(d, "live")):
                continue
            fid = re.search(r"-(f\d+)$", d)
            fid = fid.group(1) if fid else os.path.basename(d)
            if d not in seen_frames:
                seen_frames.add(d)
                out(f"FRAME-ACTIVE {fid}")
            for p in sorted(glob.glob(os.path.join(d, "live", "*.edn"))):
                mt = os.path.getmtime(p)
                if seen_phase.get(p) == mt:
                    continue
                seen_phase[p] = mt
                name = os.path.basename(p)[:-4]
                t = read(p)
                if name in REVIEW:
                    vs = re.findall(r":verdict\s*:([\w-]+)", t)
                    if vs:
                        tally = {}
                        for v in vs:
                            tally[v] = tally.get(v, 0) + 1
                        cj = tally.get("cannot-judge", 0)
                        if cj:
                            out(f"UNRESOLVED {fid} {name} {tally} "
                                f"-- {cj} cannot-judge; this pass resolved nothing")
                        else:
                            out(f"PHASE {fid} {name} {tally}")
                elif name == "close-frame":
                    stt = re.search(r":state/type\s*(:[\w-]+)", t)
                    if stt and "certified" in stt.group(1):
                        out(f"CERTIFIED {fid}")
                elif name.startswith("student-attempt") and not name.endswith("review"):
                    # Only report usage for a COMPLETED attempt. The dispatch
                    # record is written first and has an empty :used-ids, so
                    # reporting it reads as "student used nothing" when the
                    # student has not started. Emitting that is the same error
                    # this watcher exists to catch.
                    st = re.search(r":state/type\s*(:[\w-]+)", t)
                    stt = st.group(1) if st else ""
                    if "dispatched" in stt:
                        line = f"PHASE {fid} {name} dispatched"
                        if last_emit.get(p) != line:
                            last_emit[p] = line
                            out(line)
                    else:
                        u = re.findall(r":used-ids\s*\[([^\]]*)\]", t)
                        a = re.findall(r":accessible-memory-ids\s*\[([^\]]*)\]", t)
                        n = len(set(re.findall(r'"(e-[^"]+)"', u[0]))) if u else 0
                        na = len(re.findall(r'"e-[^"]+"', a[0])) if a else 0
                        flag = "  <-- ZERO UPTAKE" if (na and not n) else ""
                        line = f"PHASE {fid} {name} used={n}/{na}{flag}"
                        if last_emit.get(p) != line:
                            last_emit[p] = line
                            out(line)

        if BABYLOG and os.path.exists(BABYLOG):
            sz = os.path.getsize(BABYLOG)
            if sz > baby_pos:
                with open(BABYLOG, encoding="utf-8", errors="replace") as f:
                    f.seek(baby_pos)
                    for line in f:
                        if re.search(r"ALERT|RESUM|PAUSED|DISCOVERED|BELL|stale|fail",
                                     line, re.I):
                            out("BABYSIT " + line.strip()[:200])
                baby_pos = sz
            elif sz < baby_pos:
                baby_pos = 0
    except Exception as e:
        out(f"WATCHER-ERROR {type(e).__name__}: {e}")
    time.sleep(30)
