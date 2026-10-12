#!/usr/bin/env python3
"""Plan or apply mission_id stamps to existing Xiang turn records.

The current roster mission is only a proxy for the mission at the time of an
older turn: seats can move between missions.  Dry-run is the default.  Apply
never changes a record which already contains the ``mission_id`` key.
"""

import argparse
import collections
import csv
import datetime
import json
import os
import pathlib
import tempfile
import urllib.request

ROOT = pathlib.Path(__file__).resolve().parents[1]
DEFAULT_RECORD_DIR = pathlib.Path.home() / ".emacs-graph/session-turn-analysis"
DEFAULT_SNAPSHOT = ROOT / "data/roster-2026-10-12.json"
DEFAULT_LEDGER = ROOT / "data/mission-id-backfill-2026-10-12.tsv"
ROSTER_URL = "http://localhost:7070/api/alpha/agents"
PROXY_WARNING = ("The current roster mission is a proxy for the mission at the time "
                 "of the turn; seats can move between missions.")


def record_paths(record_dir):
    return sorted(path for path in record_dir.glob("turn-*.json")
                  if path.name.count(".") == 1)


def mission_for(agent, roster):
    entry = roster.get(str(agent))
    if not isinstance(entry, dict):
        return None
    mission = entry.get("mission-id")
    if mission is None:
        return None
    mission = str(mission).strip()
    return mission or None


def plan(record_dir, roster):
    rows = []
    existing = 0
    no_mission = collections.Counter()
    off_roster = collections.Counter()
    paths = record_paths(record_dir)
    for path in paths:
        record = json.loads(path.read_text(encoding="utf-8"))
        if "mission_id" in record:
            existing += 1
            continue
        agent = str(record.get("agent_id") or "")
        if agent not in roster:
            off_roster[agent or "(missing agent_id)"] += 1
            continue
        mission = mission_for(agent, roster)
        if mission is None:
            no_mission[agent] += 1
            continue
        rows.append({"record": path.stem, "agent": agent, "mission": mission,
                     "path": path})
    return {"rows": rows, "existing": existing, "no_mission": no_mission,
            "off_roster": off_roster, "total": len(paths)}


def atomic_stamp(path, mission):
    """Add mission_id through a same-directory atomic rewrite."""
    original = path.read_bytes()
    record = json.loads(original.decode("utf-8"))
    if "mission_id" in record:
        return False
    record["mission_id"] = mission
    mode = path.stat().st_mode
    fd, temp_name = tempfile.mkstemp(prefix=".mission-id-", suffix=".tmp",
                                     dir=str(path.parent))
    try:
        with os.fdopen(fd, "w", encoding="utf-8") as handle:
            json.dump(record, handle, ensure_ascii=False, indent=2,
                      separators=(",", " : "))
            handle.write("\n")
            handle.flush()
            os.fsync(handle.fileno())
        os.chmod(temp_name, mode)
        os.replace(temp_name, path)
    finally:
        try:
            os.unlink(temp_name)
        except FileNotFoundError:
            pass
    return True


def write_ledger(path, rows):
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as handle:
        handle.write("# " + PROXY_WARNING + "\n")
        writer = csv.DictWriter(handle, fieldnames=("record", "agent", "mission"),
                                delimiter="\t", lineterminator="\n")
        writer.writeheader()
        writer.writerows({key: row[key] for key in writer.fieldnames} for row in rows)


def apply_plan(result, ledger):
    write_ledger(ledger, result["rows"])
    applied = 0
    for row in result["rows"]:
        if atomic_stamp(row["path"], row["mission"]):
            applied += 1
    return applied


def report(result, applied=None):
    by_mission = collections.Counter(row["mission"] for row in result["rows"])
    action = "applied" if applied is not None else "would_stamp"
    print("mission\t" + action)
    for mission, count in sorted(by_mission.items()):
        print(f"{mission}\t{count}")
    print(f"TOTAL\t{applied if applied is not None else len(result['rows'])}")
    print(f"already_has_mission_id\t{result['existing']}")
    print(f"agent_has_no_mission\t{sum(result['no_mission'].values())}")
    print(f"agent_not_on_roster\t{sum(result['off_roster'].values())}")
    if result["no_mission"]:
        print("\nAgents on roster without a mission:")
        for agent, count in sorted(result["no_mission"].items()):
            print(f"{agent}\t{count}")
    if result["off_roster"]:
        print("\nAgents not on roster:")
        for agent, count in sorted(result["off_roster"].items()):
            print(f"{agent}\t{count}")


def fetch_roster(url):
    with urllib.request.urlopen(url, timeout=30) as response:
        payload = json.load(response)
    agents = payload.get("agents")
    if not isinstance(agents, dict):
        raise RuntimeError("Agency response has no agents map")
    return payload, agents


def write_snapshot(path, payload):
    path.parent.mkdir(parents=True, exist_ok=True)
    snapshot = dict(payload)
    snapshot["_snapshot"] = {
        "fetched_at": datetime.datetime.now(datetime.timezone.utc).isoformat(),
        "source": ROSTER_URL,
        "warning": PROXY_WARNING,
    }
    path.write_text(json.dumps(snapshot, ensure_ascii=False, indent=2) + "\n",
                    encoding="utf-8")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--apply", action="store_true")
    mode.add_argument("--dry-run", action="store_true",
                      help="plan only (the default); never rewrite records")
    parser.add_argument("--record-dir", type=pathlib.Path, default=DEFAULT_RECORD_DIR)
    parser.add_argument("--roster-url", default=ROSTER_URL)
    parser.add_argument("--snapshot", type=pathlib.Path, default=DEFAULT_SNAPSHOT)
    parser.add_argument("--ledger", type=pathlib.Path, default=DEFAULT_LEDGER)
    args = parser.parse_args()
    payload, roster = fetch_roster(args.roster_url)
    write_snapshot(args.snapshot, payload)
    result = plan(args.record_dir, roster)
    applied = apply_plan(result, args.ledger) if args.apply else None
    report(result, applied)


if __name__ == "__main__":
    main()
