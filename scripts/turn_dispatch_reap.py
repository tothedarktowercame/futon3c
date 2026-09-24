#!/usr/bin/env python3
"""turn_dispatch_reap.py — find out what happened to a dispatched turn analysis.

  turn_dispatch_reap.py                 # every record still `requested`
  turn_dispatch_reap.py FILE...         # just these
  turn_dispatch_reap.py --apply         # write the findings back

The dispatch is fire-and-forget, which is right: it must not delay the
conversation. What was missing is the other end. A seat that is BUSY and a
seat that REFUSED both left the record at `requested`, so a pile of
un-interpreted turns accumulated with no reason attached to any of them --
153 of them before this existed. Delivery is not accomplishment.

States written:
  requested  the job is still running, or has no job id to trace
  refused    the job reached a terminal state without producing an analysis
  failed     the job errored
Only `analyzed` means the work was done, and only the analysis writer sets it.
"""
import argparse, glob, json, os, sys, urllib.request

RECORDS = os.path.expanduser("~/.emacs-graph/session-turn-analysis/turn-*.json")
BASE = os.environ.get("AGENCY_BASE", "http://127.0.0.1:7070")
TERMINAL_BAD = {"failed", "refused", "error", "cancelled", "timeout"}


def job(job_id):
    try:
        with urllib.request.urlopen(f"{BASE}/api/alpha/invoke/jobs/{job_id}", timeout=10) as r:
            return json.load(r).get("job") or {}
    except Exception as e:
        return {"_unreachable": str(e)}


def reason_of(j):
    """The last thing the job said, which is what a person actually needs."""
    for ev in reversed(j.get("events") or []):
        for k in ("text", "message", "error", "reason"):
            if ev.get(k):
                return f"{ev.get('type', '?')}: {str(ev[k])[:400]}"
    return j.get("state") or "no events recorded"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("files", nargs="*")
    ap.add_argument("--apply", action="store_true")
    ap.add_argument("--set-job", nargs=2, metavar=("RECORD", "JOB_ID"),
                    help="record the job a dispatch created, so it can be traced later")
    a = ap.parse_args()

    if a.set_job:
        rec, jid = a.set_job
        d = json.load(open(rec, encoding="utf-8"))
        d.setdefault("analysis_dispatch", {})["job_id"] = jid
        json.dump(d, open(rec, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
        print(f"{os.path.basename(rec)}: job {jid}")
        return

    files = a.files or sorted(glob.glob(RECORDS))
    counts = {"analyzed": 0, "no-job-id": 0, "running": 0, "refused": 0,
              "failed": 0, "unreachable": 0}
    for f in files:
        try:
            d = json.load(open(f, encoding="utf-8"))
        except Exception:
            continue
        if d.get("analysis_status") not in (None, "requested"):
            counts["analyzed"] += 1
            continue
        jid = (d.get("analysis_dispatch") or {}).get("job_id")
        if not jid:
            counts["no-job-id"] += 1
            continue
        j = job(jid)
        if j.get("_unreachable"):
            counts["unreachable"] += 1
            continue
        state = (j.get("state") or j.get("status") or "").lower()
        executed = (j.get("execution") or {}).get("executed")
        if state in TERMINAL_BAD or (state in ("done", "finished") and executed is False):
            new = "failed" if state in ("failed", "error") else "refused"
            counts[new] += 1
            print(f"  {new.upper():8s} {os.path.basename(f)}  {reason_of(j)[:110]}")
            if a.apply:
                d["analysis_status"] = new
                d.setdefault("analysis_dispatch", {})["outcome"] = {
                    "state": state, "reason": reason_of(j)}
                json.dump(d, open(f, "w", encoding="utf-8"), ensure_ascii=False, indent=1)
        else:
            counts["running"] += 1

    print(f"\n{len(files)} records — " + ", ".join(f"{v} {k}" for k, v in counts.items() if v))
    if counts["no-job-id"]:
        print(f"  {counts['no-job-id']} were dispatched before the job id was recorded; "
              f"they cannot be traced and stay `requested`.")
    if not a.apply and (counts["refused"] or counts["failed"]):
        print("  (dry run — pass --apply to write these back)")


if __name__ == "__main__":
    main()
