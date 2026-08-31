#!/usr/bin/env python3
"""bg.py — durable background processes for REPL-inhabiting agents.

Spawn long-running work as a child of the futon3c JVM (durable across pouch
teardown) instead of as a child of your ephemeral warm pouch (which is evicted
between turns and reaps even setsid-detached children). See
futon3c.agency.bg-process and futon3c/CLAUDE.md "Durable background work".

Usage:
  scripts/bg.py launch "<shell command>" [--agent <id>] [--label <l>] [--dir <d>]
  scripts/bg.py launch-test "<shell command>" [--agent <id>] [--label <l>]
                       [--dir <d>] [--tasks-max <n>]
                       [--window production|measurement|control]
  scripts/bg.py test-status <id>
  scripts/bg.py test-list
  scripts/bg.py test-health
  scripts/bg.py test-kill <id>
  scripts/bg.py status <id>
  scripts/bg.py tail   <id> [n]
  scripts/bg.py list   [agent-id]
  scripts/bg.py kill   <id>
  scripts/bg.py forget <id>

Thin wrapper over the Drawbridge eval endpoint (the agent->JVM channel).
"""
import sys
import os
import json
import subprocess
import datetime
import hashlib
import time

DRAWBRIDGE = os.environ.get("FUTON3C_DRAWBRIDGE_URL", "http://127.0.0.1:6768/eval")
TOKEN_FILE = os.path.join(os.path.dirname(os.path.dirname(os.path.abspath(__file__))),
                          ".admintoken")
BOUNDED_DIR = "/tmp/futon-bounded-tests"
BOUNDED_RECORDS = os.path.join(BOUNDED_DIR, "jobs.json")
BOUNDED_TASKS_MAX = 1280
BOUNDED_ADMISSION_MAX = 2


def _token():
    with open(TOKEN_FILE) as f:
        return f.read().strip()


def _cljstr(s):
    """Render a Python string as a Clojure string literal."""
    return '"' + str(s).replace("\\", "\\\\").replace('"', '\\"') + '"'


def _eval(form):
    out = subprocess.run(
        ["curl", "-s", "-X", "POST", DRAWBRIDGE,
         "-H", "x-admin-token: " + _token(),
         "-H", "Content-Type: text/plain",
         "--data-binary", form],
        capture_output=True, text=True).stdout
    return out


def _load_bounded():
    try:
        with open(BOUNDED_RECORDS) as f:
            return json.load(f)
    except (OSError, ValueError):
        return {}


def _save_bounded(records):
    os.makedirs(BOUNDED_DIR, exist_ok=True)
    tmp = BOUNDED_RECORDS + ".tmp"
    with open(tmp, "w") as f:
        json.dump(records, f, indent=2, sort_keys=True)
        f.write("\n")
    os.replace(tmp, BOUNDED_RECORDS)


def _service_counter(service, filename, key):
    show = subprocess.run(["systemctl", "--user", "show", service,
                           "--property=ControlGroup", "--value"],
                          capture_output=True, text=True)
    try:
        with open("/sys/fs/cgroup" + show.stdout.strip() + "/" + filename) as f:
            values = dict(line.split() for line in f)
            return int(values[key])
    except (OSError, KeyError, ValueError):
        return None


def _unit_scalar(unit, prop):
    p = subprocess.run(["systemctl", "--user", "show", unit,
                        "--property=" + prop, "--value"],
                       capture_output=True, text=True)
    return p.stdout.strip() if p.returncode == 0 else None


def _configuration(tasks_max, slice_tasks_max, admission_max):
    basis = {"tasks-max": int(tasks_max), "slice-tasks-max": int(slice_tasks_max),
             "admission-max": int(admission_max)}
    encoded = json.dumps(basis, sort_keys=True, separators=(",", ":")).encode()
    return dict(basis, **{"configuration-hash": hashlib.sha256(encoded).hexdigest()})


def _unit_props(unit):
    keys = ["ActiveState", "SubState", "Result", "ExecMainCode", "ExecMainStatus",
            "ControlGroup", "ExecMainStartTimestampMonotonic"]
    p = subprocess.run(["systemctl", "--user", "show", unit,
                        "--property=" + ",".join(keys)], capture_output=True, text=True)
    if p.returncode:
        return {"ActiveState": "missing", "error": p.stderr.strip()}
    return dict(line.split("=", 1) for line in p.stdout.splitlines() if "=" in line)


def _bounded_public(record):
    props = _unit_props(record["unit"])
    receipt = None
    try:
        with open(record["receipt-file"]) as f:
            receipt = json.load(f)
    except (OSError, ValueError):
        pass
    public = dict(record, systemd=props, receipt=receipt)
    if receipt:
        try:
            submitted = datetime.datetime.fromisoformat(record["submitted-at"])
            started = datetime.datetime.fromisoformat(receipt["started-at"])
            public["submission-to-start-ms"] = int((started - submitted).total_seconds() * 1000)
        except (KeyError, ValueError):
            pass
        current = _service_counter("futon3c-zone.service", "pids.events", "max")
        before = record.get("agency-pids-events-max-before")
        public["agency-pids-events-max-current"] = current
        public["agency-pids-events-max-delta"] = (None if before is None or current is None
                                                   else current - before)
    return public


def _launch_bounded(shell_cmd, opts):
    records = _load_bounded()
    active = [r for r in records.values()
              if _unit_props(r["unit"]).get("ActiveState") in ("active", "activating")]
    if len(active) >= BOUNDED_ADMISSION_MAX:
        return {"ok": False, "reason": "admission-cap", "active": len(active)}
    os.makedirs(BOUNDED_DIR, exist_ok=True)
    stamp = int(time.time() * 1000)
    safe = "".join(c if c.isalnum() else "-" for c in opts.get("label", "job"))[:32]
    job_id = "bounded-%s-%s" % (stamp, safe or "job")
    unit = "futon-test-%s.service" % job_id
    output = os.path.join(BOUNDED_DIR, job_id + ".log")
    receipt = os.path.join(BOUNDED_DIR, job_id + ".receipt.json")
    runner = os.path.join(os.path.dirname(os.path.abspath(__file__)), "bounded_test_job.py")
    tasks_max = int(opts.get("tasks-max", BOUNDED_TASKS_MAX))
    window_kind = opts.get("window-kind", "unclassified")
    if window_kind not in ("production", "measurement", "control", "unclassified"):
        return {"ok": False, "reason": "invalid-window-kind", "value": window_kind}
    config = _configuration(tasks_max, _unit_scalar("futon-testing.slice", "TasksMax"),
                            BOUNDED_ADMISSION_MAX)
    cmd = ["systemd-run", "--user", "--unit=" + unit,
           "--slice=futon-testing.slice", "--property=TasksMax=%d" % tasks_max,
           "--property=RuntimeMaxSec=45min", "--property=KillMode=control-group",
           sys.executable, runner, "--receipt", receipt, "--output", output]
    if opts.get("dir"):
        cmd += ["--cwd", opts["dir"]]
    cmd += [shell_cmd]
    submitted = datetime.datetime.now(datetime.timezone.utc).isoformat()
    agency_max = _service_counter("futon3c-zone.service", "pids.events", "max")
    p = subprocess.run(cmd, capture_output=True, text=True)
    if p.returncode:
        return {"ok": False, "reason": "submission-failed", "stderr": p.stderr.strip()}
    record = {"id": job_id, "unit": unit, "cmd": shell_cmd,
              "agent-id": opts.get("agent-id"), "label": opts.get("label"),
              "dir": opts.get("dir"), "tasks-max": tasks_max,
              "window-kind": window_kind, "configuration": config,
              "submitted-at": submitted, "output-file": output,
              "receipt-file": receipt, "submission-to-start-ms": None}
    record["agency-pids-events-max-before"] = agency_max
    records[job_id] = record
    _save_bounded(records)
    return {"ok": True, "value": _bounded_public(record)}


def _health():
    records = [_bounded_public(r) for r in _load_bounded().values()]
    current = _configuration(BOUNDED_TASKS_MAX,
                             _unit_scalar("futon-testing.slice", "TasksMax"),
                             BOUNDED_ADMISSION_MAX)
    terminal = [r for r in records if r.get("receipt")]
    production = [r for r in terminal if r.get("window-kind") == "production"]
    scoped = [r for r in production
              if r.get("configuration", {}).get("configuration-hash") ==
              current["configuration-hash"]]
    def counts(xs):
        return {"runs": len(xs),
                "passes": sum(r["receipt"]["outer-exit"] == 0 for r in xs),
                "containment-failures": sum(r["receipt"].get("reason") ==
                                            "resource-limit-failure" for r in xs),
                "test-failures": sum(r["receipt"].get("reason") ==
                                     "test-failure" for r in xs)}
    result = counts(scoped)
    result.update({"minimum-window": 30, "eligible": len(scoped) >= 30,
                   "retire": (len(scoped) >= 30 and
                              result["containment-failures"] > result["test-failures"])})
    return {"current-configuration": current, "current-window": result,
            "superseded-production": [r["id"] for r in production if r not in scoped],
            "excluded-controls": sum(r.get("window-kind") == "control" for r in terminal),
            "excluded-measurements": sum(r.get("window-kind") == "measurement"
                                         for r in terminal),
            "unclassified-terminal": sum(r.get("window-kind") in (None, "unclassified")
                                         for r in terminal)}


def main(argv):
    if not argv:
        print(__doc__)
        return 2
    cmd = argv[0]
    if cmd == "launch":
        if len(argv) < 2:
            print("launch needs a command string"); return 2
        shell_cmd = argv[1]
        opts = {}
        i = 2
        while i < len(argv) - 1:
            if argv[i] == "--agent": opts["agent-id"] = argv[i + 1]; i += 2
            elif argv[i] == "--label": opts["label"] = argv[i + 1]; i += 2
            elif argv[i] == "--dir": opts["dir"] = argv[i + 1]; i += 2
            else: i += 1
        parts = [":cmd " + _cljstr(shell_cmd)]
        for k, v in opts.items():
            parts.append(":" + k + " " + _cljstr(v))
        form = "(futon3c.agency.bg-process/launch! {" + " ".join(parts) + "})"
    elif cmd == "launch-test":
        if len(argv) < 2:
            print("launch-test needs a command string"); return 2
        opts = {}
        i = 2
        while i < len(argv) - 1:
            if argv[i] == "--agent": opts["agent-id"] = argv[i + 1]; i += 2
            elif argv[i] == "--label": opts["label"] = argv[i + 1]; i += 2
            elif argv[i] == "--dir": opts["dir"] = argv[i + 1]; i += 2
            elif argv[i] == "--tasks-max": opts["tasks-max"] = int(argv[i + 1]); i += 2
            elif argv[i] == "--window": opts["window-kind"] = argv[i + 1]; i += 2
            else: i += 1
        print(json.dumps(_launch_bounded(argv[1], opts), indent=2, sort_keys=True))
        return 0
    elif cmd == "test-status":
        record = _load_bounded().get(argv[1])
        print(json.dumps(_bounded_public(record) if record else None, indent=2, sort_keys=True))
        return 0
    elif cmd == "test-list":
        print(json.dumps([_bounded_public(r) for r in _load_bounded().values()],
                         indent=2, sort_keys=True))
        return 0
    elif cmd == "test-health":
        print(json.dumps(_health(), indent=2, sort_keys=True))
        return 0
    elif cmd == "test-kill":
        record = _load_bounded().get(argv[1])
        if not record:
            print(json.dumps(None)); return 1
        p = subprocess.run(["systemctl", "--user", "stop", record["unit"]])
        record["cancelled-at"] = datetime.datetime.now(datetime.timezone.utc).isoformat()
        records = _load_bounded(); records[argv[1]] = record; _save_bounded(records)
        print(json.dumps(dict(_bounded_public(record), cancellation=True), indent=2))
        return p.returncode
    elif cmd == "status":
        form = "(futon3c.agency.bg-process/status " + _cljstr(argv[1]) + ")"
    elif cmd == "tail":
        n = argv[2] if len(argv) > 2 else "40"
        form = "(futon3c.agency.bg-process/tail " + _cljstr(argv[1]) + " " + str(int(n)) + ")"
    elif cmd == "list":
        form = ("(futon3c.agency.bg-process/list-tasks " + _cljstr(argv[1]) + ")"
                if len(argv) > 1 else "(futon3c.agency.bg-process/list-tasks)")
    elif cmd == "kill":
        form = "(futon3c.agency.bg-process/kill! " + _cljstr(argv[1]) + ")"
    elif cmd == "forget":
        form = "(futon3c.agency.bg-process/forget! " + _cljstr(argv[1]) + ")"
    else:
        print("unknown command: " + cmd); print(__doc__); return 2
    print(_eval("(do (require 'futon3c.agency.bg-process) " + form + ")"))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
