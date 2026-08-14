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
DEFAULT_EVIDENCE_WRITE_STALE_SECONDS = 15 * 60


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


def summarize_evidence_writes(journal_records):
    """Summarize accepted Landscape writes from journal JSON records."""
    accepted = []
    for record in journal_records:
        message = record.get("MESSAGE", "")
        if (
            "end method=POST uri=/api/alpha/evidence" in message
            or "end method=POST uri=/api/alpha/hyperedge" in message
        ) and "outcome=ok" in message:
            accepted.append(record)
    latest = max(
        (int(record.get("__REALTIME_TIMESTAMP", 0)) for record in accepted),
        default=None,
    )
    return {
        "count": len(accepted),
        "last_accepted_at": (
            datetime.fromtimestamp(latest / 1_000_000, timezone.utc).isoformat()
            if latest
            else None
        ),
    }


def recent_evidence_writes(since_epoch):
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
            "json",
        ],
        check=False,
        capture_output=True,
        text=True,
    )
    if result.returncode != 0:
        return {
            "count": None,
            "last_accepted_at": None,
            "error": result.stderr.strip() or f"journalctl exit {result.returncode}",
        }
    try:
        records = [json.loads(line) for line in result.stdout.splitlines() if line]
    except json.JSONDecodeError as error:
        return {"count": None, "last_accepted_at": None, "error": str(error)}
    return summarize_evidence_writes(records)


def normalize_url(url):
    return url.rstrip("/").replace("localhost", "127.0.0.1") if url else None


def dual_write_status(environment):
    """Mirror file-ingest's self-dual-write normalization and guard."""
    primary = environment.get("FUTON_SUBSTRATE_URL") or environment.get("FUTON1A_URL")
    primary = primary or "http://localhost:7071"
    secondary = environment.get("FUTON1B_URL")
    normalized_primary = normalize_url(primary)
    normalized_secondary = normalize_url(secondary)
    reason = None
    if not secondary:
        reason = "secondary-unset"
    elif normalized_primary == normalized_secondary:
        reason = "same-target"
    return {
        "primary_url": primary,
        "secondary_url": secondary,
        "normalized_primary": normalized_primary,
        "normalized_secondary": normalized_secondary,
        "disabled": reason is not None,
        "reason": reason,
    }


def read_process_environment(pid):
    try:
        values = pathlib.Path(f"/proc/{pid}/environ").read_bytes().split(b"\0")
        return {
            key.decode(): value.decode()
            for item in values
            if b"=" in item
            for key, value in [item.split(b"=", 1)]
        }
    except (FileNotFoundError, PermissionError, OSError, UnicodeDecodeError):
        return {}


def read_process_command(pid):
    try:
        return pathlib.Path(f"/proc/{pid}/cmdline").read_bytes().decode(errors="ignore")
    except (FileNotFoundError, PermissionError, OSError):
        return ""


def futon3c_environment():
    """Read the actual serving process environment, with direct env for tests."""
    if os.environ.get("FUTON1B_URL") or os.environ.get("FUTON_SUBSTRATE_URL"):
        return dict(os.environ), "sampler-environment"
    result = subprocess.run(
        ["systemctl", "--user", "show", "futon3c-server.service", "-p", "MainPID", "--value"],
        check=False,
        capture_output=True,
        text=True,
    )
    candidates = []
    if result.returncode == 0 and result.stdout.strip().isdigit():
        candidates.append(int(result.stdout.strip()))
    candidates.extend(
        int(path.name)
        for path in pathlib.Path("/proc").glob("[0-9]*")
        if path.name.isdigit()
    )
    seen = set()
    for pid in candidates:
        if pid <= 0 or pid in seen:
            continue
        seen.add(pid)
        environment = read_process_environment(pid)
        if (
            environment.get("FUTON1B_URL")
            and "futon3c" in read_process_command(pid)
        ):
            return environment, f"process:{pid}"
    return {}, "unavailable"


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
        f" recent-writes={record.get('evidence_writes', {}).get('count', 'unknown')}"
        f" alerts={','.join(record.get('alerts', [])) or 'none'}"
    )


def persist_record(record, sampled_at_epoch, memory_events_high=None):
    append_bounded(record)
    state = {
        "sampled_at_epoch": sampled_at_epoch,
        "latest_record": record,
    }
    if memory_events_high is not None:
        state["memory_events_high"] = memory_events_high
    write_private(STATE_PATH, json.dumps(state, sort_keys=True) + "\n")


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
            persist_record(record, sampled_at_epoch)
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
    stale_seconds = int(
        os.environ.get("FUTON1B_EVIDENCE_WRITE_STALE_SECONDS", DEFAULT_EVIDENCE_WRITE_STALE_SECONDS)
    )
    evidence_writes = recent_evidence_writes(sampled_at_epoch - stale_seconds)
    server_environment, environment_source = futon3c_environment()
    dual_write = dual_write_status(server_environment)
    dual_write["environment_source"] = environment_source
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
    if evidence_writes.get("error"):
        alerts.append("evidence-write-journal-unavailable")
    elif evidence_writes["count"] == 0:
        alerts.append("evidence-write-stale")
    if dual_write["disabled"]:
        alerts.append("dual-write-disabled")
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
        "evidence_writes": {
            **evidence_writes,
            "window_seconds": stale_seconds,
        },
        "dual_write": dual_write,
        "alerts": alerts,
    }
    if not check_mode:
        persist_record(record, sampled_at_epoch, events.get("high", 0))
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
