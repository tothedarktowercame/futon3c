#!/usr/bin/env python3
"""Keep the closer lane alive and leave a durable trace, with no agent in the loop.

The overnight run must not depend on my wakes. Twice this evening a supervision
park failed to deliver (wrong surface, then wrong agent id); each time the lane
itself was fine, but nothing would have noticed if it had not been. So the
liveness check and the record are mechanical: this appends one line per tick to
a heartbeat log whether or not anyone is awake to read it, and restarts the lane
if systemd has given up on it (Restart=always still stops at StartLimitBurst).

A lost wake then costs review latency, not throughput.
"""
import collections
import json
import subprocess
import sys
import datetime as dt

sys.path.insert(0, "/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
import statement_campaign as sc  # noqa: E402
import gates  # noqa: E402

HEARTBEAT = "/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver/closer-heartbeat.log"
UNIT = "apm-closer.service"


def unit_state() -> str:
    return subprocess.run(["systemctl", "--user", "is-active", UNIT],
                          capture_output=True, text=True).stdout.strip()


def main() -> int:
    state = unit_state()
    action = ""
    if state not in ("active", "activating"):
        subprocess.run(["systemctl", "--user", "restart", UNIT],
                       capture_output=True, text=True)
        action = " RESTARTED(was=%s)" % state

    hops = collections.Counter()
    proved_recent = []
    cutoff = (dt.datetime.now(dt.timezone.utc) - dt.timedelta(minutes=20)).isoformat()
    for line in sc.MANIFEST.read_text(encoding="utf-8").splitlines():
        r = json.loads(line)
        gate = str(r.get("gate", ""))
        if not gate.startswith("closer-hop"):
            continue
        hops[(gate, r["status"])] += 1
        if r["status"] == "proved" and r["at"] > cutoff:
            proved_recent.append(r["problem-id"])

    open_sorries = sum(
        1 for p in sc.REPO.glob("problems/*/lean/Main.lean")
        if gates.count_sorries(p.read_text(errors="replace")) != 0)
    targets = len(sc.closer_targets())
    total = sum(hops.values())
    proved = sum(v for (g, s), v in hops.items() if s == "proved")

    line = ("%s unit=%s%s targets=%d open-sorries=%d hops=%d proved=%d rate=%.1f%%"
            % (dt.datetime.now(dt.timezone.utc).isoformat(timespec="seconds"),
               state, action, targets, open_sorries, total, proved,
               100.0 * proved / total if total else 0.0))
    if proved_recent:
        line += " NEW=" + ",".join(sorted(proved_recent))
    with open(HEARTBEAT, "a", encoding="utf-8") as fh:
        fh.write(line + "\n")
    print(line)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
