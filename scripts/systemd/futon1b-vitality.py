#!/usr/bin/env python3
"""Bounded, append-only vitality sampling for the futon1b store cgroup."""

import json
import os
import pathlib
import subprocess
import time
import urllib.error
import urllib.request
from datetime import datetime, timezone


UNIT = "futon1b-server.service"
HEALTH_URL = "http://127.0.0.1:7073/health"
STATE_DIR = pathlib.Path(os.environ.get("XDG_STATE_HOME", pathlib.Path.home() / ".local/state")) / "futon1b"
LOG_PATH = STATE_DIR / "vitality.jsonl"
STATE_PATH = STATE_DIR / "vitality-state.json"
MAX_LOG_BYTES = 10 * 1024 * 1024


def systemctl_property(name):
    result = subprocess.run(
        ["systemctl", "--user", "show", UNIT, "-p", name, "--value"],
        check=True,
        capture_output=True,
        text=True,
    )
    return result.stdout.strip()


def read_int(path):
    value = path.read_text().strip()
    return None if value == "max" else int(value)


def read_pairs(path):
    return {key: int(value) for key, value in (line.split() for line in path.read_text().splitlines())}


def health_probe():
    started = time.monotonic()
    try:
        with urllib.request.urlopen(HEALTH_URL, timeout=2) as response:
            response.read()
            return response.status, round((time.monotonic() - started) * 1000, 1), None
    except (urllib.error.URLError, TimeoutError, OSError) as error:
        return 0, round((time.monotonic() - started) * 1000, 1), str(error)


def load_state():
    try:
        return json.loads(STATE_PATH.read_text())
    except (FileNotFoundError, json.JSONDecodeError, OSError):
        return {}


def write_private(path, text):
    temporary = path.with_suffix(path.suffix + ".tmp")
    temporary.write_text(text)
    os.chmod(temporary, 0o600)
    temporary.replace(path)


def append_bounded(record):
    if LOG_PATH.exists() and LOG_PATH.stat().st_size >= MAX_LOG_BYTES:
        rotated = LOG_PATH.with_suffix(".jsonl.1")
        if rotated.exists():
            rotated.unlink()
        LOG_PATH.replace(rotated)
    with LOG_PATH.open("a") as handle:
        handle.write(json.dumps(record, sort_keys=True) + "\n")
    os.chmod(LOG_PATH, 0o600)


def main():
    STATE_DIR.mkdir(parents=True, exist_ok=True, mode=0o700)
    os.chmod(STATE_DIR, 0o700)
    active_state = systemctl_property("ActiveState")
    control_group = systemctl_property("ControlGroup")
    cgroup = pathlib.Path("/sys/fs/cgroup") / control_group.lstrip("/")
    if active_state != "active" or not cgroup.exists():
        status, latency_ms, health_error = health_probe()
        record = {
            "at": datetime.now(timezone.utc).isoformat(),
            "unit": UNIT,
            "active_state": active_state,
            "health": {"status": status, "elapsed_ms": latency_ms, "error": health_error},
            "alerts": ["unit-inactive"],
        }
        append_bounded(record)
        print(f"[futon1b-vitality] ALERT {json.dumps(record, sort_keys=True)}")
        return
    current = read_int(cgroup / "memory.current")
    high = read_int(cgroup / "memory.high")
    maximum = read_int(cgroup / "memory.max")
    events = read_pairs(cgroup / "memory.events")
    stats = read_pairs(cgroup / "memory.stat")
    previous = load_state()
    status, latency_ms, health_error = health_probe()
    high_delta = events.get("high", 0) - int(previous.get("memory_events_high", 0))
    ratio = (current / high) if high else None
    alerts = []
    if ratio is not None and ratio >= 0.80:
        alerts.append("memory-high-ratio")
    if high_delta > 0:
        alerts.append("memory-high-throttled")
    if status != 200:
        alerts.append("health-failed")
    elif latency_ms >= 500:
        alerts.append("health-slow")
    record = {
        "at": datetime.now(timezone.utc).isoformat(),
        "unit": UNIT,
        "memory": {
            "current": current,
            "high": high,
            "max": maximum,
            "ratio_to_high": round(ratio, 4) if ratio is not None else None,
            "anon": stats.get("anon"),
            "file": stats.get("file"),
            "swapcached": stats.get("swapcached"),
            "events": events,
            "high_delta": high_delta,
        },
        "pressure": (cgroup / "memory.pressure").read_text().splitlines(),
        "health": {"status": status, "elapsed_ms": latency_ms, "error": health_error},
        "alerts": alerts,
    }
    append_bounded(record)
    write_private(STATE_PATH, json.dumps({"memory_events_high": events.get("high", 0)}) + "\n")
    prefix = "ALERT" if alerts else "ok"
    print(f"[futon1b-vitality] {prefix} {json.dumps(record, sort_keys=True)}")


if __name__ == "__main__":
    main()
