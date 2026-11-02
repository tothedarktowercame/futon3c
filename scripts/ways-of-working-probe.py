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
    failing.append("s6-readable")
    ev.append("s6-readable: False (Joe 2026-11-02: cannot keep a picture of the whole stack; sampled-question test not built)")
    return {"monitor": "on", "invariant": "failing" if failing else "holding",
            "last-observation": summary.get("s4-evidence", {}).get("as-of") or std.get("s4-evidence", {}).get("as-of"),
            "observed-at": iso(NOW), "violations": failing, "violation-count": len(failing), "evidence": ev}

def probe_draw_pile(graph):
    if not graph:
        return {"monitor": "broken", "invariant": "unknown", "observed-at": iso(NOW),
                "violations": [], "violation-count": 0, "evidence": ["graph.json absent"]}
    t = graph.get("tickets", {})
    # the endpoint lists worktree copies (futon2-<branch>, futon3c-index-check, ...) as separate items;
    # one stem is one piece of work, so count distinct stems, preferring canonical repo paths (claude-16, 2026-11-02)
    import re as _re
    items = t.get("items", [])
    stems = {i.get("stem") for i in items if i.get("stem")}
    raw_total = t.get("count-total") or len(items)
    total = len(stems) if stems else raw_total
    canonical = {i.get("stem") for i in items if _re.match(r"/home/joe/code/futon[0-9a-z]*/", i.get("path") or "")}
    placed = set()
    for f in (HOME / "notes" / "commons-map").glob("placements-*.tsv"):
        if f.name.startswith("placements-who"): continue
        for i, line in enumerate(f.read_text().splitlines()):
            if i == 0 or not line.strip(): continue
            placed.add(line.split("\t")[0])
    # ant placements (claude-16, 2026-11-02 21:3xZ): an item counts as placed if a reader gave it a what,
    # a status and a cited way; those rows are committed under futon3c/holes/excursions/ant-reads/
    import json as _json
    ant_dir = pathlib.Path(__file__).resolve().parent.parent / "holes" / "excursions" / "ant-reads"
    ant_placed = set()
    for f in ant_dir.glob("*.A1.json") if ant_dir.exists() else []:
        try: r = _json.loads(f.read_text()).get("row") or {}
        except Exception: continue
        if all(isinstance(r.get(k), dict) and ("value" in r[k]) for k in ("what", "status", "way")) and r["way"].get("cite"):
            ant_placed.add(r.get("stem") or f.name.split(".")[0])
    placed |= ant_placed
    committed = bool(ant_placed)  # the ant rows are on a committed path; the commons-map TSVs are not
    unplaced = len(stems - placed) if stems else max(total - len(placed), 0)  # ids placed but not in the pile must not be subtracted (claude-16)
    # claude-16 (2026-11-02 21:1xZ): over half the unplaced stems say closed in their own status line.
    # A closed item needs an attestation upward (IT-4), not a placement; count the two apart.
    _closed_re = _re.compile(r"^\*{0,2}status\*{0,2}:?\*{0,2}\s*.*\b(done|closed|complete[d]?|retired|superseded)\b", _re.I)
    path_of = {}
    for i in items:
        st, pth = i.get("stem"), i.get("path") or ""
        if st and st not in path_of or (st and _re.match(r"/home/joe/code/futon[0-9a-z]*/", pth)): path_of[st] = pth
    closed_unplaced = set()
    for st in (stems - placed):
        try:
            head = pathlib.Path(path_of.get(st, "")).read_text(errors="replace").splitlines()[:15]
        except Exception:
            continue
        if any(_closed_re.search(l) for l in head): closed_unplaced.add(st)
    open_unplaced = len(stems - placed) - len(closed_unplaced)
    ev = [f"of the unplaced: {open_unplaced} open (need a placement), {len(closed_unplaced)} closed in their own status line (need an attestation upward, IT-4, not a placement; claude-16 finding 2026-11-02)",f"draw pile (unclocked missions, excursions, tickets): {total} distinct stems ({raw_total} items listed; the rest are worktree copies; {len(canonical)} stems on canonical repo paths)",
          f"placements read by the commons map: {len(placed) - len(ant_placed)} ids (advisory, under ~/notes) plus {len(ant_placed)} ant placements (what + status + cited way; committed under ant-reads/); {len(stems & placed) if stems else 0} of all placed ids are in the pile",
          "the pile on this page is the evidence of this failure"]
    return {"monitor": "on", "invariant": "failing" if unplaced or not committed else "holding",
            "last-observation": iso(NOW), "observed-at": iso(NOW),
            "violations": [{"unplaced-open": open_unplaced, "unplaced-closed-unattested": len(closed_unplaced)}], "violation-count": unplaced, "evidence": ev,
            "denominator": f"{unplaced} of {total} unplaced: {open_unplaced} open, {len(closed_unplaced)} closed without attestation"}

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
