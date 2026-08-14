#!/usr/bin/env python3
"""What has the bridge lane produced that I have not reviewed yet?

The overnight loop is: lane dispatches on a timer, I wake every 30 minutes and
review. That only works if a wake is CHEAP, so this does the mechanical part --
job state, outcome letter, sorry count, statement-hash integrity, opaque/
native_decide, bridges added -- and leaves me the judgement: is the statement
faithful, is a bridge TRUE and does it actually unblock.

Reviewed ids are recorded so a wake never re-reads settled work.
"""
import json
import pathlib
import re
import subprocess
import sys

sys.path.insert(0, "/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
import gates
import statement_campaign as sc

DRIVER = pathlib.Path("/home/joe/code/futon3c/holes/labs/M-diagramprover/apm-driver")
REPO = pathlib.Path("/home/joe/code/apm-lean")
JOBS = DRIVER / "bridge-pilot-jobs.jsonl"
SEEN = DRIVER / "bridge-reviewed.txt"

OUTCOME = re.compile(r"outcome\s*[:\-]?\s*([AB]\b|failed|statement-defective)", re.I)
DEFECT = re.compile(r"statement[- ]defective|frozen statement is (?:defective|universe-defective|false)", re.I)


def job_result(job_id: str) -> tuple[str, str]:
    out = subprocess.run(["curl", "-s", f"localhost:7070/api/alpha/invoke/jobs/{job_id}"],
                         capture_output=True, text=True).stdout
    try:
        data = json.loads(out)
        job = data.get("job", data)
        return str(job.get("state") or "unknown"), " ".join(str(job.get("result") or "").split())
    except Exception:
        return "unreachable", ""


def main() -> int:
    jobs = [json.loads(l) for l in JOBS.read_text().splitlines() if l.strip()]
    seen = set(SEEN.read_text().split()) if SEEN.exists() else set()
    pending, running = [], 0
    for rec in jobs:
        if rec["job-id"] in seen:
            continue
        state, result = job_result(rec["job-id"])
        if state != "done":
            running += 1
            continue
        pending.append((rec, result))

    print(f"jobs total {len(jobs)} | reviewed {len(seen)} | still running {running} | "
          f"AWAITING REVIEW {len(pending)}")
    print()
    for rec, result in pending:
        pid = rec["problem-id"]
        path = REPO / "problems" / pid / "lean" / "Main.lean"
        src = path.read_text(encoding="utf-8") if path.exists() else ""
        frozen = sc.frozen_hash(pid)
        try:
            cur = gates.statement_hash(src, pid)[2]
        except Exception:
            cur = None
        stmt = "SAME" if (frozen and cur == frozen) else ("MOVED!" if frozen else "no-contract")
        bridges = re.findall(r"theorem\s+(apm_\w*bridge\w*)", src)
        outcome = OUTCOME.search(result)
        flag = "STATEMENT-DEFECTIVE" if DEFECT.search(result) else (
            outcome.group(1).upper() if outcome else "?")
        print(f"[{rec['tier']}] {pid:8s} outcome={flag:20s} sorries={gates.count_sorries(src):<2d} "
              f"stmt={stmt:12s} opaque={len(gates.opaque_declarations(src))} "
              f"nd={src.count('native_decide')} bridges={len(bridges)}")
        print(f"     {result[:400]}")
        print()
    if pending:
        print("To mark reviewed:  python3 bridge_review.py --seen " +
              " ".join(r["job-id"] for r, _ in pending))
    return 0


if __name__ == "__main__":
    if "--seen" in sys.argv:
        with SEEN.open("a", encoding="utf-8") as fh:
            for job_id in sys.argv[sys.argv.index("--seen") + 1:]:
                fh.write(job_id + "\n")
        print("recorded as reviewed")
        raise SystemExit(0)
    raise SystemExit(main())
