#!/usr/bin/env python3
"""Run one test command and make cgroup resource failures part of its verdict."""
import argparse
import datetime
import hashlib
import json
import os
import subprocess
import sys


def now():
    return datetime.datetime.now(datetime.timezone.utc).isoformat()


def repository_basis(cwd):
    """Observe the exact Git basis available to the command, without asserting it."""
    def git(*args):
        return subprocess.run(["git", *args], cwd=cwd, text=False,
                              stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    head = git("rev-parse", "HEAD")
    tree = git("rev-parse", "HEAD^{tree}")
    tracked = git("diff", "HEAD", "--")
    status = git("status", "--porcelain")
    readable = all(p.returncode == 0 for p in (head, tree, tracked, status))
    return {"readable": readable,
            "head": head.stdout.decode().strip() if head.returncode == 0 else None,
            "tree-sha": tree.stdout.decode().strip() if tree.returncode == 0 else None,
            "tracked-diff-sha256": (hashlib.sha256(tracked.stdout).hexdigest()
                                     if tracked.returncode == 0 else None),
            "dirty": bool(status.stdout.strip()) if status.returncode == 0 else None,
            "tracked-dirty": bool(tracked.stdout) if tracked.returncode == 0 else None}


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


def resource_status(receipt):
    """Classify run-level resource evidence independently of the test verdict."""
    delta = receipt.get("pids-events-max-delta")
    if delta is None:
        return "unavailable"
    if delta > 0 or receipt.get("native-thread-markers"):
        return "dirty"
    return "clean"


def failure_resource_correlation(receipt):
    """Join an outer failure to the resource evidence for the same run."""
    if receipt.get("outer-exit") == 0:
        return None
    return {"scope": "whole-run-not-per-test",
            "resource-status": resource_status(receipt),
            "command-exit": receipt.get("inner-exit"),
            "pids-events-max-delta": receipt.get("pids-events-max-delta"),
            "native-thread-exhaustion": bool(receipt.get("native-thread-markers")),
            "limitation": ("run-level correlation cannot identify which test "
                           "caused resource pressure")}


def correlation_line(correlation):
    return ("bounded-test failure/resource correlation: "
            "command-exit=%s resource-status=:%s pids.events:max-delta=%s "
            "scope=:%s; %s\n" %
            (correlation["command-exit"], correlation["resource-status"],
             correlation["pids-events-max-delta"], correlation["scope"],
             correlation["limitation"]))


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
    basis_started = repository_basis(args.cwd)
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
    basis_finished = repository_basis(args.cwd)
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
    receipt["repository-basis-start"] = basis_started
    receipt["repository-basis-finish"] = basis_finished
    receipt["repository-basis-stable"] = (basis_started == basis_finished)
    receipt["resource-status"] = resource_status(receipt)
    receipt["failure-resource-correlation"] = failure_resource_correlation(receipt)
    receipt["receipt-path"] = args.receipt
    tmp = args.receipt + ".tmp"
    with open(tmp, "w") as f:
        json.dump(receipt, f, indent=2, sort_keys=True)
        f.write("\n")
    os.replace(tmp, args.receipt)
    if args.certificate_resource:
        write_certificate_resource(args.certificate_resource, receipt)
    if receipt["failure-resource-correlation"]:
        line = correlation_line(receipt["failure-resource-correlation"])
        with open(args.output, "a") as log:
            log.write(line)
        sys.stdout.write(line)
    return receipt["outer-exit"]


if __name__ == "__main__":
    sys.exit(main())
