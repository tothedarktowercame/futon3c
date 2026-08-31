#!/usr/bin/env python3
"""Run one test command and make cgroup resource failures part of its verdict."""
import argparse
import datetime
import json
import os
import subprocess
import sys


def now():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def cgroup_dir():
    with open("/proc/self/cgroup") as f:
        rel = next(line.split(":", 2)[2].strip() for line in f if line.startswith("0:"))
    return "/sys/fs/cgroup" + rel


def read_int(path, default=None):
    try:
        with open(path) as f:
            return int(f.read().strip())
    except (OSError, ValueError):
        return default


def event_max(root):
    try:
        with open(os.path.join(root, "pids.events")) as f:
            return int(dict(line.split() for line in f)["max"])
    except (OSError, KeyError, ValueError):
        return None


def edn_string(value):
    return json.dumps(value, ensure_ascii=False)


def write_certificate_resource(path, receipt):
    """Emit the certificate's EDN resource port from this measured receipt."""
    status = ":clean" if receipt["outer-exit"] == 0 else ":dirty"
    reason = "nil" if receipt["reason"] is None else ":" + receipt["reason"]
    form = ("{:schema 1 :source-schema :futon-bounded-test-v1 "
            ":status %s :reason %s :command-exit %d :wrapper-exit %d "
            ":pids-events-max-delta %s :native-thread-exhaustion %s "
            ":tasks-peak %d :source-receipt %s}\n" %
            (status, reason, receipt["inner-exit"], receipt["outer-exit"],
             "nil" if receipt["pids-events-max-delta"] is None
             else receipt["pids-events-max-delta"],
             "true" if receipt["native-thread-markers"] else "false",
             receipt["pids-peak"], edn_string(receipt["receipt-path"])))
    tmp = path + ".tmp"
    with open(tmp, "w") as f:
        f.write(form)
    os.replace(tmp, path)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--receipt", required=True)
    p.add_argument("--certificate-resource")
    p.add_argument("--output", required=True)
    p.add_argument("--cwd")
    p.add_argument("command")
    args = p.parse_args()
    root = cgroup_dir()
    before = event_max(root)
    peak = read_int(os.path.join(root, "pids.peak"), 0)
    started = now()
    with open(args.output, "a", buffering=1) as log:
        proc = subprocess.Popen(["bash", "-lc", args.command], cwd=args.cwd,
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT,
                                text=True)
        markers = []
        assert proc.stdout is not None
        for line in proc.stdout:
            log.write(line)
            sys.stdout.write(line)
            if "pthread_create failed" in line or "Failed to start the native thread" in line:
                markers.append(line.strip())
            peak = max(peak, read_int(os.path.join(root, "pids.current"), 0))
        inner = proc.wait()
    peak = max(peak, read_int(os.path.join(root, "pids.peak"), 0))
    after = event_max(root)
    max_delta = None if before is None or after is None else after - before
    reason = None
    if inner != 0:
        reason = "test-failure"
    if (max_delta or 0) > 0 or markers:
        reason = "resource-limit-failure"
    receipt = {"schema": "futon-bounded-test-v1", "started-at": started,
               "finished-at": now(), "command": args.command,
               "inner-exit": inner, "outer-exit": 0 if reason is None else 125,
               "verdict": "pass" if reason is None else "fail", "reason": reason,
               "pids-peak": peak, "pids-events-max-before": before,
               "pids-events-max-after": after, "pids-events-max-delta": max_delta,
               "native-thread-markers": markers}
    receipt["receipt-path"] = args.receipt
    tmp = args.receipt + ".tmp"
    with open(tmp, "w") as f:
        json.dump(receipt, f, indent=2, sort_keys=True)
        f.write("\n")
    os.replace(tmp, args.receipt)
    if args.certificate_resource:
        write_certificate_resource(args.certificate_resource, receipt)
    return receipt["outer-exit"]


if __name__ == "__main__":
    sys.exit(main())
