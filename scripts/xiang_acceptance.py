#!/usr/bin/env python3
"""McCarthy-derived acceptance checks over one session's 象 stepper frames.

Each check is one requirement from DERIVE-1 (holes/labs/M-象-2000/
DERIVE-1-mccarthy-requirements.md), asked of the frames turn_frames.py
builds, i.e. of what the stepper shows. A check that the stack cannot
answer yet is reported PENDING with the condition that re-arms it, not
skipped.

Usage:
  xiang_acceptance.py SESSION-ID          # builds the frames (slow)
  xiang_acceptance.py --frames FILE.json  # frames already built
"""
import argparse
import json
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
CODE = os.path.abspath(os.path.join(HERE, "..", ".."))


def _ev(row):
    s = row.get("summary") or {}
    return s.get("event") if isinstance(s, dict) else None


def check_recording_is_acting(frames):
    """R17: the program's own actions are in the history. Every turn that
    was answered (another turn follows it) has the agent's reply among its
    HAPPENED rows."""
    bad = [f["turn"]["at"] for f in frames[:-1]
           if not any(_ev(h) == "chat-turn" for h in f["happened"])]
    return bad


def check_past_is_ordered(frames):
    """R18: functions of the past need a well-ordered history. Frames are in
    time order and each HAPPENED row falls between its turn and the next."""
    bad = []
    for i, f in enumerate(frames):
        lo = f["turn"]["at"]
        hi = frames[i + 1]["turn"]["at"] if i + 1 < len(frames) else None
        if hi is not None and hi < lo:
            bad.append(f"{lo}: next turn is earlier ({hi})")
        for h in f["happened"]:
            if h["at"] < lo or (hi is not None and h["at"] >= hi):
                bad.append(f"{lo}: row {h['at']} outside the window")
    return bad


def check_outputs_are_acts(frames):
    """R16/R29: legacy outputs are read as typed acts. No HAPPENED row is
    left as a bare type with no named event or intent."""
    bad = []
    for f in frames:
        for h in f["happened"]:
            s = h.get("summary") or {}
            name = _ev(h) or s.get("intent")
            if not name or name == h.get("type") == "coordination":
                bad.append(f"{h['at']}: unnamed {h.get('type')} row")
    return bad


def check_records_are_true(frames, code=CODE):
    """R3/R4: assertions truthful. Every commit a turn-commits row names
    exists in its repo."""
    bad = []
    for f in frames:
        for h in f["happened"]:
            if _ev(h) != "turn-commits":
                continue
            for c in h["summary"].get("commits", []):
                repo = os.path.join(code, c.get("repo") or "?")
                ok = subprocess.run(["git", "-C", repo, "cat-file", "-e",
                                     f"{c.get('sha')}^{{commit}}"],
                                    capture_output=True).returncode == 0
                if not ok:
                    sha = c.get("sha") or ""
                    why = ("sha has surrounding whitespace" if sha != sha.strip()
                           else "not found")
                    bad.append(f"{f['turn']['at']}: {c.get('repo')} {sha.strip()[:12]} {why}")
    return bad


def open_promises(frames):
    """R6/R19: exists(t, commitment). A park made is owed until a later
    released, lapsed or fulfilled row for the same park id. Returns the
    parks still open at the end of the session, with the turn that made them."""
    made, ended = {}, set()
    for f in frames:
        for h in f["happened"]:
            pid = (h.get("summary") or {}).get("park-id")
            if not pid:
                continue
            ev = _ev(h)
            if ev == "promise/park-made":
                made.setdefault(pid, f["turn"]["at"])
            elif ev in ("promise/released", "promise/lapsed", "promise/fulfilled"):
                ended.add(pid)
    return {p: at for p, at in made.items() if p not in ended}


def check_promises_accounted(frames):
    """Every park made in the session is either ended or listed as open;
    a park row without an id cannot be accounted for and is a failure."""
    bad = []
    for f in frames:
        for h in f["happened"]:
            if _ev(h) == "promise/park-made" and not h["summary"].get("park-id"):
                bad.append(f"{h['at']}: park-made row has no park id")
    return bad


def check_rewind_pins(frames):
    """R19 (rewind): to go back to the start of turn k, its turn-commits row
    must carry each repo's starting commit."""
    bad = []
    for f in frames:
        for h in f["happened"]:
            if _ev(h) == "turn-commits" and not h["summary"].get("start-heads"):
                bad.append(f["turn"]["at"])
    return bad


CHECKS = [
    ("R17 recording is a side effect of acting", check_recording_is_acting, None),
    ("R18 the past is well ordered", check_past_is_ordered, None),
    ("R16/R29 outputs are read as acts", check_outputs_are_acts, None),
    ("R3/R4 records are true (commits exist)", check_records_are_true, None),
    ("R6/R19 every promise is accounted for", check_promises_accounted, None),
    ("R19 each turn can be rewound to its start", check_rewind_pins,
     "re-arms when turn-commits rows carry start-heads (M-象-2000 packet 2)"),
]


def run(frames):
    results = []
    for name, fn, pending in CHECKS:
        bad = fn(frames)
        status = "PASS" if not bad else ("PENDING" if pending else "FAIL")
        results.append({"check": name, "status": status, "failures": len(bad),
                        "examples": bad[:3], "re_arm": pending if bad else None})
    return results


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("session_id", nargs="?")
    ap.add_argument("--frames")
    a = ap.parse_args(argv)
    if a.frames:
        frames = json.load(open(a.frames))
    elif a.session_id:
        out = subprocess.run([sys.executable, os.path.join(HERE, "turn_frames.py"),
                              a.session_id], capture_output=True, text=True, check=True)
        frames = json.loads(out.stdout)
    else:
        ap.error("give a session id or --frames")
    results = run(frames)
    for r in results:
        print(f"{r['status']:8} {r['check']}  ({r['failures']} failing)")
        for e in r["examples"]:
            print(f"           e.g. {e}")
        if r["re_arm"]:
            print(f"           {r['re_arm']}")
    op = open_promises(frames)
    print(f"open promises at the end of the session: {len(op)}")
    for p, at in list(op.items())[:5]:
        print(f"           {p} (made in the turn at {at})")
    return 1 if any(r["status"] == "FAIL" for r in results) else 0


if __name__ == "__main__":
    sys.exit(main())
