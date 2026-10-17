#!/usr/bin/env python3
"""FUTON's view of its agents, for the stepper's zoomed preview (馬 speaks for FUTON).

Derives what substrate-2 does not record directly:
  mission -> pattern  from session -> pattern (each turn's retrieved and
                      象-matched patterns) and session -> mission (Cascade
                      Live's clock lineage), aggregated over sessions;
  agent   -> card     the latest 馬 agent card of each clocked session.

  futon_view.py [--session SID ...] [--out PATH] [--publish]

Per event (a turn's 象 reading, a 馬 card), one session's contribution is
recomputed and the aggregate re-summed; with no --session (or no saved
state) every clocked session is recomputed.

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


STATE = os.path.expanduser("~/code/data/futon3c/futon-view-sessions.json")


def session_entry(session, clock_row):
    pats, card = session_patterns(session)
    st = ((card or {}).get("card") or {}).get("standing") or {}
    return {"target": (clock_row or {}).get("target"),
            "agent": ((clock_row or {}).get("agent") or "").replace("agent:", "") or None,
            "at": (clock_row or {}).get("at") or 0,
            "purpose": (st.get("purpose") or {}).get("text") or None,
            "patterns": {p: {"turns": v["turns"], "sources": sorted(v["sources"])} for p, v in pats.items()}}


def clock_rows():
    """Newest clock edge per session, from Cascade Live's lineage."""
    by_session = {}
    for r in get_json("/api/alpha/cascade-real/graph").get("lineage") or []:
        sid = r.get("session")
        if sid and (sid not in by_session or (r.get("at") or 0) > (by_session[sid].get("at") or 0)):
            by_session[sid] = r
    return by_session


def aggregate(sessions):
    """Re-sum the per-session contributions (cheap: no fetches)."""
    links = collections.defaultdict(lambda: collections.defaultdict(lambda: {"sessions": 0, "turns": 0, "sources": set()}))
    agents = {}
    for sid, e in sessions.items():
        if e.get("target"):
            for p, v in (e.get("patterns") or {}).items():
                cell = links[e["target"]][p]
                cell["sessions"] += 1
                cell["turns"] += v["turns"]
                cell["sources"] |= set(v["sources"])
        a = e.get("agent")
        if a and (a not in agents or e["at"] > agents[a]["at"]):
            agents[a] = {"session": sid, "target": e.get("target"), "at": e["at"], "purpose": e.get("purpose")}
    return {
        "built_at": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
        "sessions": len(sessions),
        "mission_patterns": {t: sorted(([p, c["sessions"], c["turns"], sorted(c["sources"])] for p, c in ps.items()),
                                       key=lambda x: (-x[1], -x[2], x[0]))
                             for t, ps in links.items()},
        "agents": agents,
    }


def load_state():
    try:
        with open(STATE, encoding="utf-8") as fh:
            return json.load(fh)
    except (OSError, ValueError):
        return None


def save_json(path, value):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    tmp = path + ".tmp"
    with open(tmp, "w", encoding="utf-8") as fh:
        json.dump(value, fh, ensure_ascii=False)
    os.replace(tmp, path)


def update(sessions_to_refresh=None, log=print):
    """Refresh the named sessions' contributions (all clocked sessions when
    None or when there is no state yet) and return the aggregate."""
    clocks = clock_rows()
    state = load_state()
    if state is None or sessions_to_refresh is None:
        state, todo = {}, list(clocks)
    else:
        todo = list(sessions_to_refresh)
    for sid in todo:
        try:
            state[sid] = session_entry(sid, clocks.get(sid))
        except Exception as exc:
            log("futon_view: %s: %s" % (sid[:12], exc))
    for sid, r in clocks.items():          # a clock edge may have moved
        if sid in state:
            state[sid]["target"] = r.get("target")
            state[sid]["at"] = r.get("at") or 0
            state[sid]["agent"] = (r.get("agent") or "").replace("agent:", "") or state[sid].get("agent")
    save_json(STATE, state)
    return aggregate(state)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--out", default=OUT)
    ap.add_argument("--publish", action="store_true")
    ap.add_argument("--session", action="append",
                    help="refresh only this session's contribution (repeatable); default: all")
    args = ap.parse_args(argv)
    view = update(args.session)
    save_json(args.out, view)
    if args.publish:
        shutil.copyfile(args.out, PUBLISH + ".tmp")
        os.replace(PUBLISH + ".tmp", PUBLISH)
    print("futon_view: %d sessions, %d missions with derived patterns, %d agents" % (
        view["sessions"], len(view["mission_patterns"]), len(view["agents"])))
    return 0


if __name__ == "__main__":
    sys.exit(main())
