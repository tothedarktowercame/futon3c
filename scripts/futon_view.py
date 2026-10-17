#!/usr/bin/env python3
"""FUTON's view of its agents, for the stepper's zoomed preview (馬 speaks for FUTON).

Derives what substrate-2 does not record directly:
  mission -> pattern  from session -> pattern (each turn's retrieved and
                      象-matched patterns) and session -> mission (Cascade
                      Live's clock lineage), aggregated over sessions;
  agent   -> card     the latest 馬 agent card of each clocked session.

  futon_view.py [--out PATH] [--publish]

Frames are built in-process (turn_frames.build_frames), not through the
JVM's frames cache, so a run does not load every session into the server.
Joe, 2026-10-17: "we should be able to collect that info via
session-to-pattern-to-mission links".
"""
import argparse
import collections
import datetime
import json
import os
import shutil
import sys
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import turn_frames as tf  # noqa: E402

BASE = os.environ.get("FUTON3C_BASE", "http://127.0.0.1:7070")
OUT = os.path.expanduser("~/code/data/futon3c/futon-view.json")
PUBLISH = "/var/www/zone.hyperreal.enterprises/wip/stepper/futon-view.json"


def get_json(path, timeout=60):
    with urllib.request.urlopen(BASE + path, timeout=timeout) as r:
        return json.load(r)


def session_patterns(session):
    """{pattern: {"turns": n, "sources": set}} for one session, plus its newest card."""
    rows = tf.fetch_evidence(session, base=BASE)
    analyses = tf.load_analyses(session)
    try:
        tf.merge_xiang_readings(analyses, tf.fetch_xiang_readings(session, base=BASE), session)
    except Exception:
        pass
    frames = tf.build_frames(rows, analyses, session_id=session)
    out = collections.defaultdict(lambda: {"turns": 0, "sources": set()})
    for f in frames:
        seen = set()
        for r in f.get("retrieved") or []:
            if r.get("id"):
                out[r["id"]]["sources"].add("header" if "header" in (r.get("method") or "") else "embedding")
                seen.add(r["id"])
        for m in (f.get("patterns") or {}).get("matched") or []:
            pid = m if isinstance(m, str) else (m.get("id") or m.get("pattern"))
            if pid:
                out[pid]["sources"].add("象")
                seen.add(pid)
        for pid in seen:
            out[pid]["turns"] += 1
    cards = [(tf.at_key(r), tf.body_of(r)) for r in rows if tf.body_of(r).get("event") == "agent-card"]
    card = max(cards, key=lambda x: x[0])[1] if cards else None
    return out, card


def build(log=print):
    graph = get_json("/api/alpha/cascade-real/graph")
    lineage = [r for r in graph.get("lineage") or [] if r.get("session")]
    by_session = {}
    for r in lineage:                      # newest clock edge per session
        if r["session"] not in by_session or (r.get("at") or 0) > (by_session[r["session"]].get("at") or 0):
            by_session[r["session"]] = r
    links = collections.defaultdict(lambda: collections.defaultdict(lambda: {"sessions": 0, "turns": 0, "sources": set()}))
    agents = {}
    for session, r in by_session.items():
        try:
            pats, card = session_patterns(session)
        except Exception as exc:
            log("futon_view: %s: %s" % (session[:12], exc))
            continue
        for pid, v in pats.items():
            cell = links[r["target"]][pid]
            cell["sessions"] += 1
            cell["turns"] += v["turns"]
            cell["sources"] |= v["sources"]
        agent = r["agent"].replace("agent:", "")
        if agent not in agents or (r.get("at") or 0) > agents[agent]["at"]:
            st = ((card or {}).get("card") or {}).get("standing") or {}
            agents[agent] = {"session": session, "target": r["target"], "at": r.get("at") or 0,
                             "purpose": (st.get("purpose") or {}).get("text") or None}
    return {
        "built_at": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "sessions": len(by_session),
        "mission_patterns": {t: sorted(([p, c["sessions"], c["turns"], sorted(c["sources"])] for p, c in ps.items()),
                                       key=lambda x: (-x[1], -x[2], x[0]))
                             for t, ps in links.items()},
        "agents": agents,
    }


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--out", default=OUT)
    ap.add_argument("--publish", action="store_true")
    args = ap.parse_args(argv)
    view = build()
    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    tmp = args.out + ".tmp"
    with open(tmp, "w", encoding="utf-8") as fh:
        json.dump(view, fh, ensure_ascii=False)
    os.replace(tmp, args.out)
    if args.publish:
        shutil.copyfile(args.out, PUBLISH + ".tmp")
        os.replace(PUBLISH + ".tmp", PUBLISH)
    print("futon_view: %d sessions, %d missions with derived patterns, %d agents" % (
        view["sessions"], len(view["mission_patterns"]), len(view["agents"])))
    return 0


if __name__ == "__main__":
    sys.exit(main())
