#!/usr/bin/env python3
"""Probe the declared ways of working and write their live state.

Usage: ways-of-working-probe.py DECLARATIONS OUT [--summary summary.json] [--graph graph.json]

Reads holes/excursions/ways-of-working.json, runs one probe per way that
declares one, and writes a JSON with, per way: monitor (on/off/broken/not-built),
invariant (holding/failing/unknown), last-observation, violations, evidence.
A way with probe "none" is reported as monitor not-built, invariant unknown:
the page shows that honestly rather than guessing. No state is changed.
"""
import datetime, json, os, pathlib, subprocess, sys

HOME = pathlib.Path.home()
CODE = HOME / "code"
NOW = datetime.datetime.now(datetime.timezone.utc)

def iso(dt): return dt.replace(microsecond=0).isoformat()

def run(cmd, cwd=None, timeout=60):
    try:
        p = subprocess.run(cmd, cwd=cwd, capture_output=True, text=True, timeout=timeout)
        return p.returncode, p.stdout.strip(), p.stderr.strip()
    except Exception as e:  # noqa: BLE001
        return 99, "", str(e)

def probe_git_clean():
    rows, no_upstream = [], []
    for d in sorted(CODE.iterdir()):
        if not (d / ".git").exists(): continue
        rc, ab, _ = run(["git", "rev-list", "--left-right", "--count", "@{u}...HEAD"], cwd=d)
        if rc != 0:
            no_upstream.append(d.name); continue
        behind, ahead = (ab.split() + ["0", "0"])[:2]
        rc, st, _ = run(["git", "status", "--porcelain"], cwd=d)
        lines = [l for l in st.splitlines() if l.strip()]
        untracked = sum(1 for l in lines if l.startswith("??"))
        modified = len(lines) - untracked
        if untracked or modified or int(ahead):
            rows.append({"repo": d.name, "untracked": untracked, "modified": modified,
                         "ahead": int(ahead), "behind": int(behind)})
    rc, active, _ = run(["systemctl", "--user", "is-active", "futon-sync-check-clean.timer"])
    rc2, enabled, _ = run(["systemctl", "--user", "is-enabled", "futon-sync-check-clean.timer"])
    log = CODE / "futon0" / "data" / "inbox-zero-delta.log"
    last = iso(datetime.datetime.fromtimestamp(log.stat().st_mtime, datetime.timezone.utc)) if log.exists() else None
    monitor = "on" if active == "active" else "off"
    evidence = [f"timer futon-sync-check-clean.timer: {active or 'absent'} ({enabled or 'unknown'})",
                f"delta notifier last wrote: {last or 'never'}",
                f"repos with no upstream (push undefined): {len(no_upstream)}"]
    return {"monitor": monitor, "invariant": "failing" if rows else "holding",
            "last-observation": last, "observed-at": iso(NOW),
            "violations": rows, "violation-count": len(rows), "evidence": evidence,
            "denominator": f"{len(rows) + 0} dirty of {len(list(p for p in CODE.iterdir() if (p/'.git').exists())) - len(no_upstream)} repos with an upstream"}

def probe_cascade_standards(summary):
    if not summary:
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "violations": [], "violation-count": 0, "evidence": ["summary.json absent"]}
    std = summary.get("standards", {})
    failing = [k for k, v in std.items() if k.startswith("s") and "-" in k and v is False]
    holes = summary.get("holes", {})
    ev = [f"{k}: {v}" for k, v in std.items() if not isinstance(v, dict)]
    ev.append(f"honest holes: {len(holes) if isinstance(holes, (list, dict)) else holes}")
    return {"monitor": "on", "invariant": "failing" if failing else "holding",
            "last-observation": summary.get("s4-evidence", {}).get("as-of") or std.get("s4-evidence", {}).get("as-of"),
            "observed-at": iso(NOW), "violations": failing, "violation-count": len(failing), "evidence": ev}

def probe_draw_pile(graph):
    if not graph:
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "violations": [], "violation-count": 0, "evidence": ["graph.json absent"]}
    t = graph.get("tickets", {})
    total = t.get("count-total") or len(t.get("items", []))
    placed = set()
    for f in (HOME / "notes" / "commons-map").glob("placements-*.tsv"):
        if f.name.startswith("placements-who"): continue
        for i, line in enumerate(f.read_text().splitlines()):
            if i == 0 or not line.strip(): continue
            placed.add(line.split("\t")[0])
    committed = False  # placements live under ~/notes, not on a committed path
    unplaced = max(total - len(placed), 0)
    ev = [f"draw pile (unclocked missions, excursions, tickets): {total}",
          f"placements read by the commons map: {len(placed)} ids (advisory; on a committed path: {committed})",
          "the pile on this page is the evidence of this failure"]
    return {"monitor": "on", "invariant": "failing" if unplaced or not committed else "holding",
            "last-observation": iso(NOW), "observed-at": iso(NOW),
            "violations": [{"unplaced": unplaced}], "violation-count": unplaced, "evidence": ev,
            "denominator": f"{unplaced} of {total} unplaced"}

def main():
    args = sys.argv[1:]
    decl, out = pathlib.Path(args[0]), pathlib.Path(args[1])
    summary = graph = None
    if "--summary" in args: summary = json.loads(pathlib.Path(args[args.index("--summary") + 1]).read_text())
    if "--graph" in args: graph = json.loads(pathlib.Path(args[args.index("--graph") + 1]).read_text())
    d = json.loads(decl.read_text())
    probes = {"git-clean": lambda: probe_git_clean(),
              "cascade-standards": lambda: probe_cascade_standards(summary),
              "draw-pile": lambda: probe_draw_pile(graph)}
    ways = []
    for w in d["ways"]:
        w = dict(w)
        p = w.get("probe", "none")
        if p in probes:
            try: w["live"] = probes[p]()
            except Exception as e:  # noqa: BLE001
                w["live"] = {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                             "violations": [], "violation-count": 0, "evidence": [f"probe error: {e}"]}
        else:
            w["live"] = {"monitor": "not-built", "invariant": "unknown", "observed-at": iso(NOW),
                         "violations": [], "violation-count": 0,
                         "evidence": ["no probe declared: the way of working is prose until one is built"]}
        if w.get("scope-note"):
            w["live"]["evidence"].append("scope: " + w["scope-note"])
        ways.append(w)
    counts = {}
    for w in ways:
        k = w["live"]["monitor"] + "/" + w["live"]["invariant"]; counts[k] = counts.get(k, 0) + 1
    out.write_text(json.dumps({"schema": "ways-of-working-live-v1", "declared": d["as-of"],
                               "probed-at": iso(NOW), "states": d["states"], "counts": counts,
                               "provenance": d["provenance"], "ways": ways}, indent=1))
    print(f"wrote {out}: " + ", ".join(f"{k} {v}" for k, v in sorted(counts.items())))

if __name__ == "__main__":
    main()
