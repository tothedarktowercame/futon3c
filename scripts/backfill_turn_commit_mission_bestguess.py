#!/usr/bin/env python3
"""Best-guess historical turn->commit + turn->mission backfill from git.

For the turns we have logged (claude jsonl transcripts), attribute each stack
commit to the nearest-preceding user-turn within a window, and best-guess the
mission via (a) a touched M-*.md doc or (b) mission->file mention overlap.

Choppy by design (no clock-in historically, concurrent sessions) — gives a real
best-guess dataset to work with now; clean ground truth accrues going forward
once §8.1 clock-in + per-turn commit logging land. (C-substrate-completion §8.)
"""
import json, subprocess, glob, os, bisect, pathlib
from datetime import datetime

HOME = os.path.expanduser("~")
REPOS = ["futon0","futon1a","futon2","futon3","futon3a","futon3c","futon4","futon5","futon5a","futon6","futon7"]
WINDOW = 90 * 60  # s: a commit attributes to a user-turn at most 90 min earlier
OUT = f"{HOME}/code/futon5a/data/turn-commit-mission-backfill.json"

def iso_epoch(s):
    try: return datetime.fromisoformat(s.replace("Z", "+00:00")).timestamp()
    except Exception: return None

# 1) user-turn events from logged claude transcripts (the turns we have)
turns = []  # (epoch, session-id, project)
for f in glob.glob(f"{HOME}/.claude/projects/*/*.jsonl"):
    proj = os.path.basename(os.path.dirname(f))
    try:
        for line in open(f, errors="replace"):
            if '"type":"user"' not in line and '"type": "user"' not in line:
                continue
            try: r = json.loads(line)
            except Exception: continue
            if r.get("type") == "user" and r.get("timestamp"):
                e = iso_epoch(r["timestamp"])
                if e: turns.append((e, r.get("sessionId") or os.path.basename(f)[:8], proj))
    except Exception: continue
turns.sort()
turn_times = [t[0] for t in turns]

# 2) git commits across the stack (all branches)
commits = []
for repo in REPOS:
    base = f"{HOME}/code/{repo}"
    if not os.path.isdir(base): continue
    out = subprocess.run(["git", "-C", base, "log", "--all", "--no-merges",
                          "--pretty=format:__C__%H|%ct|%s", "--name-only"],
                         capture_output=True, text=True, errors="replace").stdout
    cur = None
    for ln in out.splitlines():
        if ln.startswith("__C__"):
            if cur: commits.append(cur)
            parts = ln[5:].split("|", 2)
            if len(parts) == 3:
                cur = {"repo": repo, "sha": parts[0][:10], "ct": int(parts[1]),
                       "msg": parts[2], "files": []}
            else: cur = None
        elif ln.strip() and cur is not None:
            cur["files"].append(ln.strip())
    if cur: commits.append(cur)

# 3) mission->file mention map -> file->missions (repo-prefixed paths)
mfe = json.load(open(f"{HOME}/code/futon5a/holes/tech-notes/mission-file-edges.json"))
file_missions = {}
for m, fs in mfe.items():
    mid = os.path.basename(m)[:-3] if m.endswith(".md") else os.path.basename(m)
    for fp in fs:
        file_missions.setdefault(fp, set()).add(mid)

def guess_mission(c):
    for fp in c["files"]:
        b = os.path.basename(fp)
        if b.startswith("M-") and b.endswith(".md"):
            return b[:-3], "touched-mission-doc"
    cnt = {}
    for fp in c["files"]:
        for mid in file_missions.get(f"{c['repo']}/{fp}", ()):
            cnt[mid] = cnt.get(mid, 0) + 1
    if cnt:
        best = max(cnt, key=cnt.get)
        return best, f"mention-overlap:{cnt[best]}"
    return None, None

# 4) attribute each commit to the nearest-preceding user-turn within WINDOW
recs = []
for c in commits:
    i = bisect.bisect_right(turn_times, c["ct"]) - 1
    t = turns[i] if (i >= 0 and c["ct"] - turn_times[i] <= WINDOW) else None
    mid, msrc = guess_mission(c)
    recs.append({"sha": c["sha"], "repo": c["repo"], "ct": c["ct"], "msg": c["msg"][:80],
                 "turn_epoch": (t[0] if t else None), "session": (t[1] if t else None),
                 "project": (t[2] if t else None),
                 "bestguess_mission": mid, "mission_source": msrc})

attributed = sum(1 for r in recs if r["session"])
with_mission = sum(1 for r in recs if r["bestguess_mission"])
out = {"generated_note": "best-guess; choppy by design (no historical clock-in)",
       "n_user_turns_logged": len(turns), "n_commits": len(commits),
       "window_minutes": WINDOW // 60,
       "commits_attributed_to_turn": attributed,
       "commits_with_bestguess_mission": with_mission, "records": recs}
pathlib.Path(os.path.dirname(OUT)).mkdir(parents=True, exist_ok=True)
json.dump(out, open(OUT, "w"), indent=1)
print(f"user-turns={len(turns)}  commits={len(commits)}  "
      f"turn-attributed={attributed} ({100*attributed//max(1,len(commits))}%)  "
      f"mission-guessed={with_mission} ({100*with_mission//max(1,len(commits))}%)")
print("wrote", OUT)
