#!/usr/bin/env python3
"""Plan/apply IDs from the per-session clock export; never use roster fallback.

The join is exact on (agent_id, session_id). Dry-run is the default. Apply
never changes a record which already contains the clock-selected target key.
"""

import argparse
import collections
import csv
import json
import os
import pathlib
import tempfile

ROOT = pathlib.Path(__file__).resolve().parents[1]
DEFAULT_RECORD_DIR = pathlib.Path.home() / ".emacs-graph/session-turn-analysis"
DEFAULT_CLOCKS = ROOT / "data/clock-sessions-2026-10-12.json"
DEFAULT_LEDGER = ROOT / "data/mission-id-backfill-2026-10-12.tsv"
LEDGER_NOTE = ("Source: exact (agent_id, session_id) join to the exported clock "
               "store; no current-roster fallback.")


def record_paths(record_dir):
    return sorted(p for p in record_dir.glob("turn-*.json")
                  if p.name.count(".") == 1)


def clean(value):
    value = None if value is None else str(value).strip()
    return value or None


def load_clocks(path):
    payload = json.loads(path.read_text(encoding="utf-8"))
    rows = payload.get("rows") if isinstance(payload, dict) else payload
    if not isinstance(rows, list):
        raise ValueError("clock export must be a list or contain a rows list")
    clocks = {}
    for row in rows:
        pair = (clean(row.get("agent")), clean(row.get("session")))
        if pair in clocks:
            raise ValueError(f"duplicate clock row for {pair!r}")
        clocks[pair] = row
    return clocks


def clock_target(row):
    mission = clean(row.get("mission_id"))
    if mission:
        return "mission_id", mission
    excursion = clean(row.get("excursion_id"))
    if excursion:
        return "excursion_id", excursion
    return None


def plan(record_dir, clocks):
    rows = []
    existing = collections.Counter()
    no_clock = collections.Counter()
    no_target = collections.Counter()
    paths = record_paths(record_dir)
    for path in paths:
        record = json.loads(path.read_text(encoding="utf-8"))
        pair = (clean(record.get("agent_id")), clean(record.get("session_id")))
        clock = clocks.get(pair)
        if clock is None:
            no_clock[pair] += 1
            continue
        target = clock_target(clock)
        if target is None:
            no_target[pair] += 1
            continue
        key, value = target
        if key in record:
            existing[key] += 1
            continue
        rows.append({"record": path.stem, "agent": pair[0] or "",
                     "session": pair[1] or "", "key": key, "value": value,
                     "decided_at": clean(clock.get("decided_at")) or "",
                     "path": path})
    return {"rows": rows, "existing": existing, "no_clock": no_clock,
            "no_target": no_target, "total": len(paths)}


def atomic_stamp(path, key, value):
    record = json.loads(path.read_text(encoding="utf-8"))
    if key in record:
        return False
    record[key] = value
    mode = path.stat().st_mode
    fd, temp_name = tempfile.mkstemp(prefix=".clock-id-", suffix=".tmp",
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
        handle.write("# " + LEDGER_NOTE + "\n")
        fields = ("record", "agent", "session", "key", "value", "decided_at")
        writer = csv.DictWriter(handle, fieldnames=fields, delimiter="\t",
                                lineterminator="\n")
        writer.writeheader()
        writer.writerows({key: row[key] for key in fields} for row in rows)


def apply_plan(result, ledger):
    write_ledger(ledger, result["rows"])
    return sum(atomic_stamp(row["path"], row["key"], row["value"])
               for row in result["rows"])


def report(result, applied=None):
    action = "applied" if applied is not None else "would_stamp"
    for key, label in (("mission_id", "mission"),
                       ("excursion_id", "excursion")):
        print(f"{label}\t{action}")
        counts = collections.Counter(row["value"] for row in result["rows"]
                                     if row["key"] == key)
        for value, count in sorted(counts.items()):
            print(f"{value}\t{count}")
        print(f"{label.upper()}_TOTAL\t{sum(counts.values())}\n")
    print(f"TOTAL\t{applied if applied is not None else len(result['rows'])}")
    print(f"already_has_target_key\t{sum(result['existing'].values())}")
    print(f"no_clock_row\t{sum(result['no_clock'].values())}")
    print(f"clock_has_no_target\t{sum(result['no_target'].values())}")
    print(f"record_files\t{result['total']}")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    mode = parser.add_mutually_exclusive_group()
    mode.add_argument("--apply", action="store_true")
    mode.add_argument("--dry-run", action="store_true")
    parser.add_argument("--record-dir", type=pathlib.Path, default=DEFAULT_RECORD_DIR)
    parser.add_argument("--clock-file", type=pathlib.Path, default=DEFAULT_CLOCKS)
    parser.add_argument("--ledger", type=pathlib.Path, default=DEFAULT_LEDGER)
    args = parser.parse_args()
    result = plan(args.record_dir, load_clocks(args.clock_file))
    applied = apply_plan(result, args.ledger) if args.apply else None
    report(result, applied)


if __name__ == "__main__":
    main()
