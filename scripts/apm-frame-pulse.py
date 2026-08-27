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
import os, re, sys, glob, time, subprocess

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


def dispositions(text):
    """Distinct (memory-id, verdict) pairs in a review record.

    Counting raw ":verdict" occurrences over-counts from f42 onward: the
    promotion receipt gained :candidate-materialization and
    :review-materialization sub-records (the artifact read-back that makes a
    promotion checkable), and EACH carries its own copy of the disposition.
    f42's two rejected memories rendered as "reject: 8", and its one approval
    as "approve: 3". Frames f28-f41 predate the schema and are unaffected --
    verified, both countings agree on every one of them.

    Each :verdict belongs to the nearest :memory-id before it, which is the
    association the receipt actually encodes.
    """
    ids = [(m.start(), m.group(1)) for m in re.finditer(r':memory-id "([^"]+)"', text)]
    pairs = set()
    for m in re.finditer(r":verdict\s*:([\w-]+)", text):
        prior = [i for i in ids if i[0] < m.start()]
        if prior:
            pairs.add((prior[-1][1], m.group(1)))
    return pairs


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
        pairs = dispositions(t)
        if not pairs:
            continue
        # Sorted, so the rendering is a function of the verdicts alone. An
        # insertion-ordered dict made the line churn between runs with identical
        # content ({'approve': 1, 'reassign': 1} one run, reversed the next),
        # which a watcher diffing this output reports as a change. A channel
        # that cries wolf on key order stops being read.
        tally = {}
        for _mid, v in sorted(pairs, key=lambda x: (x[1], x[0])):
            tally[v] = tally.get(v, 0) + 1
        cj = tally.get("cannot-judge", 0)
        unresolved_total += cj
        flag = "  <-- UNRESOLVED" if cj else ""
        print(f"    {ph:30} {tally}{flag}")
    # :reassign is an ACCEPT, not a rejection. promotion_pipeline.clj groups it
    # with :approve everywhere -- both map to status :reviewed, and a reassign
    # is refused unless its target attachment is already reviewed. It means the
    # candidate duplicates an existing reviewed pattern, so attach it there.
    # Counting only "approve" reported f49's three reassigns as "0 approved",
    # i.e. reported cross-frame pattern recognition as no supply at all.
    approved = 0
    reassigned = 0
    for ph in REVIEW_PHASES:
        t = read(os.path.join(live, ph + ".edn"))
        verdicts = [v for _mid, v in dispositions(t)]
        approved += verdicts.count("approve")
        reassigned += verdicts.count("reassign")
    extra = f" (+{reassigned} reassigned to existing patterns)" if reassigned else ""
    print(f"  supply:   {approved} approved this frame{extra}")

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
        # Resolve the solver worktree first: the sorry trajectory and the
        # growth check both need it, and an empty trajectory means different
        # things depending on whether the file is even there.
        pid = re.search(r':problem-id "([^"]+)"', t)
        wt = mainlean = None
        if pid:
            wt = f"/home/joe/code/apm-frames/{fid}-{pid.group(1)}-solver"
            mainlean = os.path.join(wt, "problems", pid.group(1),
                                    "lean", "Main.lean")
        if rounds:
            moved = len(set(sorries)) > 1
            # A bare "?" here used to cover every way the trajectory could be
            # empty -- no Main.lean, no worktree, and apm-lean simply not
            # reporting -- which are different situations and want different
            # responses. Name which one it is (TN-apm-watcher, open items).
            if sorries:
                traj = f"{sorries[0]}->{sorries[-1]}"
            elif mainlean is None:
                traj = "no-problem-id"
            elif not os.path.isdir(wt):
                traj = "no-worktree"
            elif not os.path.exists(mainlean):
                traj = "no-Main.lean"
            else:
                traj = "unreported"
            note = ""
            if heads and len(set(heads)) < max(1, len(heads)) // 2:
                note = "  <-- heads repeating; solver may be spinning"
            elif not moved and len(rounds) >= 20:
                note = "  (sorries flat; judge by worktree growth, not by apm-lean)"
            # The receipts' :final-head values live on the frame's own worktree
            # branch, NOT apm-lean master -- frame work never lands there until
            # promotion. Reading apm-lean for progress shows commits that
            # predate the frame and is how this check first misread f36.
            grow = ""
            if mainlean:
                if os.path.exists(mainlean):
                    try:
                        n = sum(1 for _ in open(mainlean, encoding="utf-8",
                                                errors="replace"))
                        # Baseline against the frame's OWN opening revision,
                        # not HEAD~k. HEAD~k counts commits, which answers a
                        # different question and reaches outside the frame: with
                        # no rounds yet it floors at HEAD~1, which is a pre-frame
                        # apm-lean library commit. :base-revision is the commit
                        # the frame was actually handed, so the comparison is
                        # "how far has this frame moved the file" at every round
                        # count including zero -- which is when worktree growth
                        # is the ONLY progress signal, the case this line exists
                        # to serve.
                        prev = None
                        rev = re.search(r':base-revision "([0-9a-f]{7,})"', t)
                        if rev:
                            base = subprocess.run(
                                ["git", "show",
                                 f"{rev.group(1)}:problems/"
                                 f"{pid.group(1)}/lean/Main.lean"],
                                cwd=wt, capture_output=True, text=True, timeout=15)
                            prev = (len(base.stdout.splitlines())
                                    if base.returncode == 0 else None)
                        grow = (f"; worktree Main.lean {n} lines"
                                + (f" ({n - prev:+d} vs frame base {prev})"
                                   if prev is not None else ""))
                    except Exception:
                        pass
            print(f"  solve:    round {rounds[-1]}, {rem[-1] if rem else '?'} left; "
                  f"sorries {traj}; {len(set(heads))} distinct heads/{len(heads)} "
                  f"rounds{grow}{note}")

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
        # An attempt with no :used-ids KEY has not reported yet; one with an
        # empty :used-ids has reported zero uptake.  Printing both as
        # "0/N used" makes an in-flight attempt look like the campaign's most
        # serious finding -- these must never share a rendering.
        if ":used-ids" not in t:
            print(f"    a{n}: IN FLIGHT, {na} accessible, no result yet")
            continue
        flag = "   <-- ZERO UPTAKE" if (na and not nu) else ""
        print(f"    a{n}: memory {nu}/{na} used   "
              f"{'sorries='+sor.group(1) if sor else ''} "
              f"{'outcome='+out.group(1) if out else ''}{flag}")

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
