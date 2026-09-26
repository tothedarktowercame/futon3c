#!/usr/bin/env python3
"""Clear an Agency seat's conversation, but only while it is idle.

  reset_seat_if_idle.py AGENT-ID

Prints "reset" when the session was cleared and "busy" when the seat was
working (a reset mid-job would cut the job's conversation out from under it).
Always exits 0, so a caller can chain a dispatch after it regardless; the
caller retries on its next dispatch when it sees "busy".
"""
import json
import sys
import urllib.parse
import urllib.request

BASE = "http://localhost:7070/api/alpha/agents"


def main():
    aid = sys.argv[1]
    try:
        with urllib.request.urlopen(BASE, timeout=10) as r:
            d = json.load(r)
        agents = d.get("agents", d)
        if isinstance(agents, list):
            agents = {a.get("id"): a for a in agents}
        if (agents.get(aid) or {}).get("status") != "idle":
            print("busy")
            return
        req = urllib.request.Request(
            f"{BASE}/{urllib.parse.quote(aid)}/reset-session", method="POST", data=b"")
        with urllib.request.urlopen(req, timeout=10) as r:
            print("reset" if json.load(r).get("ok") else "busy")
    except Exception as e:  # never block the dispatch that follows
        print(f"busy ({e})")


if __name__ == "__main__":
    main()
