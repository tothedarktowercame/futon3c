#!/usr/bin/env python3
"""Bounded, append-only vitality sampling for the futon1b store cgroup."""

import argparse
import json
import os
import pathlib
import subprocess
import time
import urllib.error
import urllib.request
from datetime import datetime, timezone


UNIT = "futon1b-server.service"
MAIN_HEALTH_URL = "http://127.0.0.1:7073/health"
LIVENESS_URL = "http://127.0.0.1:7072/health"
STATE_DIR = pathlib.Path(os.environ.get("XDG_STATE_HOME", pathlib.Path.home() / ".local/state")) / "futon1b"
LOG_PATH = STATE_DIR / "vitality.jsonl"
STATE_PATH = STATE_DIR / "vitality-state.json"
MAX_LOG_BYTES = 10 * 1024 * 1024
DEFAULT_JOURNAL_LOOKBACK_SECONDS = 300


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


def health_probe(url):
    started = time.monotonic()
    try:
        with urllib.request.urlopen(url, timeout=2) as response:
            response.read()
            return response.status, round((time.monotonic() - started) * 1000, 1), None
    except (urllib.error.URLError, TimeoutError, OSError) as error:
        return 0, round((time.monotonic() - started) * 1000, 1), str(error)


def summarize_evidence_append_errors(journal_text):
    rejected = [
        line
        for line in journal_text.splitlines()
        if "end method=POST uri=/api/alpha/evidence" in line
        and "outcome=error" in line
    ]
    return {
        "count": len(rejected),
        "invalid_edn_count": sum("Invalid token:" in line for line in rejected),
        "statuses": sorted(
            {
                token.removeprefix("status=")
                for line in rejected
                for token in line.split()
                if token.startswith("status=")
            }
        ),
    }


def recent_evidence_append_errors(since_epoch):
    result = subprocess.run(
        [
            "journalctl",
            "--user",
            "-u",
            UNIT,
            "--since",
            f"@{since_epoch:.3f}",
            "--no-pager",
            "-o",
            "cat",
        ],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return {
            "count": None,
            "invalid_edn_count": None,
            "statuses": [],
            "error": result.stderr.strip() or f"journalctl exit {result.returncode}",
        }
    return summarize_evidence_append_errors(result.stdout)


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


def concise_summary(record):
    memory = record.get("memory", {})
    health = record.get("health", {})
    liveness = record.get("independent_liveness", {})
    evidence_errors = record.get("evidence_append_errors", {})
    state = "OK" if not record.get("alerts") else "DEGRADED"
    ratio = memory.get("ratio_to_high")
    ratio_text = "n/a" if ratio is None else f"{ratio * 100:.1f}%"
    return (
        f"futon1b {state}"
        f" unit={record.get('active_state')}"
        f" main={health.get('status')}/{health.get('elapsed_ms')}ms"
        f" liveness={liveness.get('status')}/{liveness.get('elapsed_ms')}ms"
        f" memory-high={ratio_text}"
        f" recent-evidence-errors={evidence_errors.get('count', 'unknown')}"
        f" alerts={','.join(record.get('alerts', [])) or 'none'}"
    )


def main(check_mode=False):
    STATE_DIR.mkdir(parents=True, exist_ok=True, mode=0o700)
    os.chmod(STATE_DIR, 0o700)
    sampled_at_epoch = time.time()
    active_state = systemctl_property("ActiveState")
    control_group = systemctl_property("ControlGroup")
    cgroup = pathlib.Path("/sys/fs/cgroup") / control_group.lstrip("/")
    if active_state != "active" or not cgroup.exists():
        status, latency_ms, health_error = health_probe(LIVENESS_URL)
        record = {
            "at": datetime.now(timezone.utc).isoformat(),
            "unit": UNIT,
            "active_state": active_state,
            "health": {"status": status, "elapsed_ms": latency_ms, "error": health_error},
            "alerts": ["unit-inactive"],
        }
        if not check_mode:
            append_bounded(record)
        print(
            concise_summary(record)
            if check_mode
            else f"[futon1b-vitality] ALERT {json.dumps(record, sort_keys=True)}"
        )
        return 1 if check_mode else 0
    current = read_int(cgroup / "memory.current")
    high = read_int(cgroup / "memory.high")
    maximum = read_int(cgroup / "memory.max")
    events = read_pairs(cgroup / "memory.events")
    stats = read_pairs(cgroup / "memory.stat")
    previous = load_state()
    previous_sample_epoch = (
        sampled_at_epoch - DEFAULT_JOURNAL_LOOKBACK_SECONDS
        if check_mode
        else float(
            previous.get(
                "sampled_at_epoch",
                sampled_at_epoch - DEFAULT_JOURNAL_LOOKBACK_SECONDS,
            )
        )
    )
    status, latency_ms, health_error = health_probe(MAIN_HEALTH_URL)
    liveness_status, liveness_latency_ms, liveness_error = health_probe(LIVENESS_URL)
    evidence_append_errors = recent_evidence_append_errors(previous_sample_epoch)
    high_delta = events.get("high", 0) - int(previous.get("memory_events_high", 0))
    ratio = (current / high) if high else None
    alerts = []
    if ratio is not None and ratio >= 0.80:
        alerts.append("memory-high-ratio")
    if high_delta > 0:
        alerts.append("memory-high-throttled")
    if status != 200:
        alerts.append("main-health-failed")
    elif latency_ms >= 500:
        alerts.append("main-health-slow")
    if liveness_status != 200:
        alerts.append("independent-liveness-failed")
    elif liveness_latency_ms >= 500:
        alerts.append("independent-liveness-slow")
    if evidence_append_errors.get("error"):
        alerts.append("evidence-append-journal-unavailable")
    elif evidence_append_errors["count"] > 0:
        alerts.append("evidence-append-rejected")
    record = {
        "at": datetime.now(timezone.utc).isoformat(),
        "unit": UNIT,
        "active_state": active_state,
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
        "independent_liveness": {
            "status": liveness_status,
            "elapsed_ms": liveness_latency_ms,
            "error": liveness_error,
        },
        "evidence_append_errors": evidence_append_errors,
        "alerts": alerts,
    }
    if not check_mode:
        append_bounded(record)
        write_private(
            STATE_PATH,
            json.dumps(
                {
                    "memory_events_high": events.get("high", 0),
                    "sampled_at_epoch": sampled_at_epoch,
                }
            )
            + "\n",
        )
    prefix = "ALERT" if alerts else "ok"
    print(
        concise_summary(record)
        if check_mode
        else f"[futon1b-vitality] {prefix} {json.dumps(record, sort_keys=True)}"
    )
    return 1 if check_mode and alerts else 0


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--check",
        action="store_true",
        help="print one concise status line and exit nonzero when degraded",
    )
    arguments = parser.parse_args()
    raise SystemExit(main(check_mode=arguments.check))
