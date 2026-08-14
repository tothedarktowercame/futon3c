#!/usr/bin/env python3
"""Operator control for the Agency's per-agent turn queues.

Two things the jobs window (`/api/alpha/invoke/jobs`) cannot give you:

  1. VISIBILITY into what is *queued*. A bell waiting behind a slow turn has
     no job yet — it is an entry in the durable turn queue, invisible until it
     starts running. `status` shows the backlog itself.
  2. A HOLD. `hold` pauses a recipient's queue after the turn currently in
     flight finishes — that turn runs to completion and finalizes normally.
     Nothing new is popped until `release`. Bells keep arriving and queue up
     behind the hold in FIFO order; none are lost.

A hold is NOT a cancel. To end the turn actually running, use
`POST /api/alpha/invoke/jobs/<id>/cancel` (README-agency-cap.md).

Usage:
  scripts/agency_queue.py status [--all] [--json]
  scripts/agency_queue.py hold <agent> [--reason R] [--ttl MIN] [--by WHO]
  scripts/agency_queue.py release <agent>

Holds are durable across a JVM restart, so they default to a 30-minute TTL —
a hold set and forgotten silently piles bells up behind it. `--ttl 0` holds
indefinitely.
"""

import argparse
import json
import os
import sys
import urllib.error
import urllib.request

BASE = os.environ.get("FUTON3C_BASE", "http://127.0.0.1:7070")


def call(path, payload=None):
    url = f"{BASE}{path}"
    data = json.dumps(payload).encode() if payload is not None else None
    req = urllib.request.Request(
        url, data=data,
        headers={"Content-Type": "application/json"} if data else {},
        method="POST" if data is not None else "GET",
    )
    try:
        with urllib.request.urlopen(req, timeout=15) as r:
            return json.load(r)
    except urllib.error.HTTPError as e:
        body = e.read().decode(errors="replace")
        try:
            return json.loads(body)
        except Exception:
            return {"ok": False, "error": f"http-{e.code}", "body": body[:400]}
    except Exception as e:
        return {"ok": False, "error": "unreachable", "message": str(e)}


def die_unless_ok(res):
    if not res.get("ok"):
        print(f"error: {res.get('error')} {res.get('message') or res.get('body') or ''}",
              file=sys.stderr)
        sys.exit(1)
    return res


def cmd_status(args):
    res = die_unless_ok(call(f"/api/alpha/agency/queue{'?all=1' if args.all else ''}"))
    if args.json:
        print(json.dumps(res, indent=2))
        return
    agents = res.get("agents", [])
    if not agents:
        print("no queued turns, no drains in flight, no holds")
        return
    print(f"total pending: {res.get('total-pending', 0)}")
    for a in agents:
        hold = a.get("held")
        marks = []
        if a.get("draining"):
            marks.append("DRAINING")
        if hold:
            reason = hold.get("reason")
            marks.append("HELD" + (f" ({reason})" if reason else ""))
        print(f"\n{a['agent-id']}: {a['pending']} queued"
              + (f"  [{', '.join(marks)}]" if marks else ""))
        if hold and hold.get("expires-at"):
            print(f"  hold set {hold.get('held-at')} by {hold.get('by')}"
                  f" — auto-releases after {int(hold['ttl-ms']) // 60000}m")
        for e in a.get("queued", []):
            print(f"  #{e.get('seq')} from {e.get('from')} via {e.get('surface')}"
                  f" @ {e.get('accepted-at')}")
            if e.get("preview"):
                print(f"      {e['preview']}")


def cmd_hold(args):
    payload = {"agent": args.agent, "by": args.by}
    if args.reason:
        payload["reason"] = args.reason
    if args.ttl:
        payload["ttl-minutes"] = args.ttl
    res = die_unless_ok(call("/api/alpha/agency/queue/hold", payload))
    ttl = res.get("hold", {}).get("ttl-ms")
    print(f"HELD {res['agent-id']} — {res.get('pending', 0)} turn(s) waiting; "
          f"the turn in flight (if any) will finish."
          + (f" Auto-releases in {int(ttl) // 60000}m." if ttl else " No auto-release."))
    print(f"resume with: scripts/agency_queue.py release {res['agent-id']}")


def cmd_release(args):
    res = die_unless_ok(call("/api/alpha/agency/queue/release", {"agent": args.agent}))
    if not res.get("released"):
        print(f"{res['agent-id']} was not held; {res.get('pending', 0)} turn(s) pending")
        return
    print(f"RELEASED {res['agent-id']} — {res.get('pending', 0)} queued turn(s) resuming")


def main():
    p = argparse.ArgumentParser(description=__doc__,
                                formatter_class=argparse.RawDescriptionHelpFormatter)
    sub = p.add_subparsers(dest="cmd", required=True)

    s = sub.add_parser("status", help="show queued turns, drains in flight, holds")
    s.add_argument("--all", action="store_true", help="include idle agents")
    s.add_argument("--json", action="store_true")
    s.set_defaults(fn=cmd_status)

    h = sub.add_parser("hold", help="pause an agent's queue after the current turn")
    h.add_argument("agent")
    h.add_argument("--reason")
    h.add_argument("--by", default="joe")
    h.add_argument("--ttl", type=int, default=30,
                   help="auto-release after N minutes (0 = indefinite; default 30)")
    h.set_defaults(fn=cmd_hold)

    r = sub.add_parser("release", help="lift a hold and resume the backlog")
    r.add_argument("agent")
    r.set_defaults(fn=cmd_release)

    args = p.parse_args()
    args.fn(args)


if __name__ == "__main__":
    main()
