#!/usr/bin/env python3
"""harvest_turn_origins.py — recover who actually authored each "joe" turn.

Every chat-turn is written to the evidence store with author "joe", because
agent-chat sets the author from $USER for any user-role turn.  The provenance
lives on a different record: the :invoke-start event, whose prompt-preview
carries the surface header --

    Surface: emacs-repl / From: joe   / Origin: operator / Caller: joe
    Surface: bell       / From: zai-1 / Origin: agent    / Caller: zai-1

This harvests those headers and joins them onto the turn corpus by agent and
time, so the back catalogue can be split into operator turns and turns an
agent (or the harness) injected under Joe's name.

  harvest_turn_origins.py [TURNS.jsonl] [OUT_DIR]
"""
import json, os, re, sys, urllib.request
from bisect import bisect_left

F1B = "http://localhost:7073"
TURNS = sys.argv[1] if len(sys.argv) > 1 else \
    "/home/joe/code/storage/operator-turns/operator-turns.jsonl"
OUTDIR = sys.argv[2] if len(sys.argv) > 2 else \
    "/home/joe/code/storage/operator-turns"

HEAD = {k: re.compile(r"%s: ([^\\\n]+)" % k) for k in
        ("Surface", "From", "To", "Origin", "Caller")}
USERMSG = re.compile(r"User message:\\n(.*)$", re.S)
MATCH_WINDOW = 180.0        # seconds between the invoke and its turn record


def get(url):
    with urllib.request.urlopen(url, timeout=180) as r:
        return r.read().decode()


def ts(s):
    """ISO-8601 to epoch seconds, tolerant of nanosecond precision."""
    import datetime
    s = re.sub(r"(\.\d{6})\d+", r"\1", s.replace("Z", "+00:00"))
    return datetime.datetime.fromisoformat(s).timestamp()


def harvest_invokes():
    rows, cursor, seen = [], None, set()
    while True:
        u = f"{F1B}/api/alpha/evidence?tags=invoke-start&limit=1000"
        if cursor:
            u += f"&before={cursor}"
        page = get(u)
        ats = re.findall(r':evidence/at "([^"]+)"', page)
        if not ats:
            break
        for e in re.split(r"(?=\{:evidence/)", page):
            if "invoke-start" not in e:
                continue
            eid = re.search(r':evidence/id "([^"]+)"', e)
            at = re.search(r':evidence/at "([^"]+)"', e)
            agent = re.search(r'\\"agent-id\\" \\"([^\\"]+)\\"', e)
            if not (eid and at) or eid.group(1) in seen:
                continue
            seen.add(eid.group(1))
            rec = {"id": eid.group(1), "at": at.group(1),
                   "agent": agent.group(1) if agent else None}
            for k, rx in HEAD.items():
                m = rx.search(e)
                rec[k.lower()] = m.group(1).strip() if m else None
            m = USERMSG.search(e)
            rec["preview"] = (m.group(1)[:160]
                              .encode().decode("unicode_escape", "ignore")
                              if m else "")
            rows.append(rec)
        oldest = min(ats)
        print(f"invoke page: {len(ats)} entries, total {len(rows)}, oldest {oldest}",
              flush=True)
        if oldest == cursor:
            break
        cursor = oldest
    return rows


def main():
    invokes = harvest_invokes()
    os.makedirs(OUTDIR, exist_ok=True)
    with open(f"{OUTDIR}/invoke-origins.jsonl", "w") as f:
        for r in invokes:
            f.write(json.dumps(r) + "\n")

    # index invokes by agent, sorted by time, for a nearest-in-time lookup
    by_agent = {}
    for r in invokes:
        if r["agent"] and r["at"]:
            by_agent.setdefault(r["agent"], []).append((ts(r["at"]), r))
    for v in by_agent.values():
        v.sort()

    turns = [json.loads(l) for l in open(TURNS)]
    counts = {}
    out = []
    for t in turns:
        tid, at = t.get("turn_id"), t.get("at")
        agent = re.sub(r"-turn-\d+$", "", tid) if tid else None
        origin = caller = surface = None
        if agent and at and agent in by_agent:
            arr = by_agent[agent]
            k = ts(at)
            i = bisect_left(arr, (k,))
            best, bestd = None, MATCH_WINDOW
            for j in (i - 1, i, i + 1):
                if 0 <= j < len(arr):
                    d = abs(arr[j][0] - k)
                    if d < bestd:
                        best, bestd = arr[j][1], d
            if best:
                origin, caller, surface = best["origin"], best["caller"], best["surface"]
        label = origin or "unmatched"
        counts[label] = counts.get(label, 0) + 1
        out.append({"turn_id": tid, "at": at, "agent": agent, "origin": origin,
                    "caller": caller, "surface": surface,
                    "words": len((t.get("text") or "").split())})

    with open(f"{OUTDIR}/turn-origins.jsonl", "w") as f:
        for r in out:
            f.write(json.dumps(r) + "\n")
    print("\nturn origin split:", json.dumps(counts, indent=2))
    print(f"wrote {OUTDIR}/turn-origins.jsonl and invoke-origins.jsonl")


if __name__ == "__main__":
    main()
