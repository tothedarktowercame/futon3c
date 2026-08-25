#!/usr/bin/env python3
"""One-shot QUALITY read of a campaign's active frame.

Complements scripts/apm-watch-projection.sh, which checks liveness only --
transition-log freshness, coordinator presence, staleness. Nothing in the
apparatus watches whether a frame is any GOOD, which is how f33 certified
:result "closed" with 11 of 11 review candidates :cannot-judge and how three
of the four f32-f35 frames were certified with an unresolved review pass.

Reports, per frame:
  verdicts      the tally per review phase, and whether any pass is UNRESOLVED
                (tier-A condition 2 of prereg amendment 4: zero unresolved
                passes in a wave)
  supply        candidates deposited vs approved (tier-A condition 1)
  memory        accessible vs used per student attempt
  attempts      outcome and wall-clock per attempt (tier-B condition 6:
                timings trending down)
  failures      :regulator/failures entries

Read-only. Exits 1 if any completed review pass is unresolved, so it can be
used as a gate as well as a report.

usage: apm-frame-pulse.py [CAMPAIGN_DIR] [--frame fNN]
"""
import os, re, sys, glob, time

ROOT = os.environ.get("APM_CAMPAIGNS",
                      os.path.expanduser("~/code/futon3c/data/apm-campaigns"))
REVIEW_PHASES = ["promote-solver", "scribe-reduce",
                 "guide-intervention-1-review", "guide-intervention-2-review"]


def newest_campaign():
    best, best_m = None, -1
    for d in glob.glob(os.path.join(ROOT, "*")):
        c = os.path.join(d, "coordinator.edn")
        if os.path.exists(c) and os.path.getmtime(c) > best_m:
            best, best_m = d, os.path.getmtime(c)
    return best


def read(p):
    try:
        with open(p, encoding="utf-8", errors="replace") as f:
            return f.read()
    except OSError:
        return ""


def frames(camp):
    fs = []
    for d in glob.glob(os.path.join(camp, "*-f*")):
        m = re.search(r"-f(\d+)$", d)
        if m and os.path.isdir(os.path.join(d, "live")):
            fs.append((int(m.group(1)), d))
    return [d for _, d in sorted(fs)]


def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    want = None
    for a in sys.argv[1:]:
        if a.startswith("--frame"):
            want = a.split("=", 1)[1] if "=" in a else None
    camp = args[0] if args else newest_campaign()
    if not camp:
        print("no campaign found"); return 2
    coord = read(os.path.join(camp, "coordinator.edn"))
    status = re.findall(r":regulator/status\s*(:[\w-]+)", coord)
    age = time.time() - os.path.getmtime(os.path.join(camp, "coordinator.edn"))

    fl = frames(camp)
    if want:
        fl = [d for d in fl if d.endswith("-" + want)] or fl
    frame = fl[-1] if fl else None
    print(f"campaign  {os.path.basename(camp)}")
    print(f"regulator {status[-1] if status else '?'}   coordinator touched {int(age)}s ago")
    if not frame:
        print("no frames yet"); return 0
    live = os.path.join(frame, "live")
    fid = re.search(r"-(f\d+)$", frame).group(1)
    print(f"frame     {fid}")

    unresolved_total = 0
    print("  verdicts:")
    for ph in REVIEW_PHASES:
        p = os.path.join(live, ph + ".edn")
        if not os.path.exists(p):
            continue
        t = read(p)
        vs = re.findall(r":verdict\s*:([\w-]+)", t)
        if not vs:
            continue
        tally = {}
        for v in vs:
            tally[v] = tally.get(v, 0) + 1
        cj = tally.get("cannot-judge", 0)
        unresolved_total += cj
        flag = "  <-- UNRESOLVED" if cj else ""
        print(f"    {ph:30} {tally}{flag}")
    approved = 0
    for ph in REVIEW_PHASES:
        t = read(os.path.join(live, ph + ".edn"))
        approved += len(re.findall(r":verdict\s*:approve", t))
    print(f"  supply:   {approved} approved this frame")

    # Solve progress: rounds alone cannot separate real work from spinning,
    # because :solver/outcome :progress is the solver's own claim and reads the
    # same either way. Distinct :final-head values and the sorry trajectory are
    # artifact facts, so they can.
    sp = os.path.join(live, "solve.edn")
    if os.path.exists(sp):
        t = read(sp)
        rounds = re.findall(r":solver/round (\d+)", t)
        sorries = re.findall(r":sorry-warnings (\d+)", t)
        heads = re.findall(r':final-head "([0-9a-f]+)"', t)
        rem = re.findall(r":solver/remaining-rounds (\d+)", t)
        if rounds:
            moved = len(set(sorries)) > 1
            traj = f"{sorries[0]}->{sorries[-1]}" if sorries else "?"
            note = ""
            if heads and len(set(heads)) < max(1, len(heads)) // 2:
                note = "  <-- heads repeating; solver may be spinning"
            elif not moved and len(rounds) >= 20:
                note = "  (sorries flat; check heads/apm-lean before calling it a wall)"
            print(f"  solve:    round {rounds[-1]}, {rem[-1] if rem else '?'} left; "
                  f"sorries {traj}; {len(set(heads))} distinct heads/{len(heads)} "
                  f"rounds{note}")

    print("  attempts:")
    for n in (1, 2, 3):
        p = os.path.join(live, f"student-attempt-{n}.edn")
        if not os.path.exists(p):
            continue
        t = read(p)
        acc = re.findall(r":accessible-memory-ids\s*\[([^\]]*)\]", t)
        used = re.findall(r":used-ids\s*\[([^\]]*)\]", t)
        na = len(re.findall(r'"e-[^"]+"', acc[0])) if acc else 0
        nu = len(set(re.findall(r'"(e-[^"]+)"', used[0]))) if used else 0
        sor = re.search(r":sorries?\s*(\d+)", t)
        out = re.search(r":outcome\s*:?\"?([\w-]+)", t)
        mins = int((os.path.getmtime(p) - os.path.getmtime(
            os.path.join(live, "preflight.edn")))/60) if os.path.exists(
            os.path.join(live, "preflight.edn")) else -1
        print(f"    a{n}: memory {nu}/{na} used   "
              f"{'sorries='+sor.group(1) if sor else ''} "
              f"{'outcome='+out.group(1) if out else ''}")

    # Only surface failures that are NEW relative to this frame. The
    # coordinator accumulates :regulator/failures for the whole campaign, so
    # printing the latest unconditionally shows a stale entry on every run --
    # a permanently-displayed old failure is what hides a fresh one.
    stamps = re.findall(r':failed-at\s*"([0-9T:.\-]+)Z?"', coord)
    codes = re.findall(r':error/code\s*(:[\w/-]+)', coord)
    if stamps:
        newest = sorted(stamps)[-1]
        try:
            import datetime
            t = datetime.datetime.fromisoformat(newest[:19]).replace(tzinfo=datetime.timezone.utc)
            age_h = (datetime.datetime.now(datetime.timezone.utc) - t).total_seconds() / 3600.0
        except Exception:
            age_h = None
        n = len(stamps)
        if age_h is not None and age_h < 2:
            print(f"  failures: {n} recorded; NEWEST {age_h*60:.0f}m ago "
                  f"{codes[-1] if codes else ''}  <-- RECENT")
        else:
            print(f"  failures: {n} recorded, newest "
                  f"{'%.0fh' % age_h if age_h is not None else '?'} ago (stale, "
                  f"pre-dates this frame)")

    if unresolved_total:
        print(f"\nGATE: {unresolved_total} unresolved verdict(s) — "
              f"under prereg tier-A condition 2 this frame should NOT certify.")
        return 1
    print("\nGATE: no unresolved verdicts.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
