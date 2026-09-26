#!/usr/bin/env python3
"""What failed, how often, and whether anyone wrote it down.

The invoke-job ledger is durable and typed: every terminal job carries a
`terminal-code`, and it survives restarts. Nothing reads it. On 2026-08-24 one
caller belled one unregistered id 57 times and the ledger recorded all 57,
under a code that no ticket in the draw pile mentions.

That is the gap between a system that records failures and one that can heal:
recording is done, noticing is not. This is the noticing half -- a census, not
a fix. It creates no tickets and changes nothing; it reports which failure
codes are recurring and which of them nobody has written down.

  python3 scripts/failure-census.py            # last 2000 jobs
  python3 scripts/failure-census.py --json     # machine-readable
"""
import json, os, sys, collections, urllib.request

AGENCY = os.environ.get("AGENCY", "http://localhost:7070")
TICKETS = os.environ.get("TICKETS", os.path.join(os.path.dirname(
    os.path.dirname(os.path.abspath(__file__))), "holes", "tickets"))
LIMIT = int(os.environ.get("JOB_LIMIT", "2000"))


def ticket_text():
    """Every draw-pile ticket, concatenated. A failure counts as written down
    if some ticket names its code -- a weak test, deliberately: the point is to
    find codes nobody has mentioned at all, not to adjudicate coverage."""
    out = {}
    if not os.path.isdir(TICKETS):
        return out
    for fn in sorted(os.listdir(TICKETS)):
        if fn.startswith("T-") and fn.endswith(".md"):
            with open(os.path.join(TICKETS, fn), encoding="utf-8", errors="replace") as f:
                out[fn[:-3]] = f.read()
    return out


def fetch_jobs(limit=None, agency=None):
    with urllib.request.urlopen(
            f"{agency or AGENCY}/api/alpha/invoke/jobs?limit={limit or LIMIT}",
            timeout=60) as r:
        return json.load(r)["jobs"]


def classes(jobs, tix=None):
    """Failure CLASSES, not failure events -- one row per terminal-code with a
    multiplicity, which is what keeps this channel from flooding a queue.

    A repeat is not new information: the code was typed the first time someone
    understood the failure well enough to name it, so the 72nd agent-not-found
    tells you the class is heavier, not that there is another thing to decide
    about. The ceiling on this channel is therefore the size of the code
    vocabulary -- 7 codes have reached terminal-code in the four days held --
    and not the number of failures.
    """
    tix = ticket_text() if tix is None else tix
    bad = [j for j in jobs if j.get("state") in ("failed", "cancelled")]
    rows = []
    for code, n in sorted(collections.Counter(
            j.get("terminal-code") or "(none)" for j in bad).items(),
            key=lambda kv: -kv[1]):
        js = [j for j in bad if (j.get("terminal-code") or "(none)") == code]
        rows.append({
            "terminal_code": code,
            "count": n,
            "first": min(j["created-at"] for j in js)[:16],
            "last": max(j["created-at"] for j in js)[:16],
            "targets": collections.Counter(j.get("agent-id") for j in js).most_common(3),
            "callers": collections.Counter(j.get("caller") for j in js).most_common(3),
            "tickets": sorted(k for k, v in tix.items() if code in v),
        })
    return rows, bad


def main():
    jobs = fetch_jobs()
    assert jobs, "job ledger came back empty -- check the Agency is up"
    rows, bad = classes(jobs)

    if "--json" in sys.argv:
        json.dump({"jobs_scanned": len(jobs), "not_clean": len(bad),
                   "window": [min(j["created-at"] for j in jobs)[:16],
                              max(j["created-at"] for j in jobs)[:16]],
                   "codes": rows}, sys.stdout, indent=2)
        print()
        return

    w = min(j["created-at"] for j in jobs)[:16], max(j["created-at"] for j in jobs)[:16]
    print(f"{len(jobs)} jobs {w[0]} -> {w[1]}; {len(bad)} not clean "
          f"in {len(rows)} class(es)\n")
    print(f"  {'count':>5}  {'terminal-code':<34} {'ticket?':<9} target <- caller")
    for r in rows:
        tk = ",".join(r["tickets"])[:24] if r["tickets"] else "NONE"
        tgt = r["targets"][0][0] if r["targets"] else "?"
        clr = r["callers"][0][0] if r["callers"] else "?"
        print(f"  {r['count']:>5}  {r['terminal_code']:<34} {tk:<9} {tgt} <- {clr}")
    un = [r for r in rows if not r["tickets"] and r["count"] > 1]
    if un:
        print(f"\n  {sum(r['count'] for r in un)} failures across {len(un)} recurring "
              f"class(es) that no ticket names:")
        for r in un:
            print(f"    {r['terminal_code']}  x{r['count']}  {r['first']} -> {r['last']}")


if __name__ == "__main__":
    main()
