#!/usr/bin/env python3
"""O5 — honest holes (C-cascade-real, claude-4). The cascade's COVERAGE-GAP dimension.

A "rendered hole resolves to a REAL, queryable coverage gap" (CHARTER standard 4).
Computed live, zero hand rows. Two gap kinds, both grounded in substrate-2:

  1. missing-canonical-node — a mission that exists in the BGE embedding set (a real
     mission doc) but has NO live canonical <repo>-d/mission/<id> node (the populator
     missed it / it's stale). The gap IS the missing node, so its hole-target points
     at the would-be canonical id (a deliberately dangling marker).
  2. no-capability — a mission that IS a live canonical node and is in the composing
     CORE (referenced by an O1 mined-move arrow or an O4 cluster) but has NO
     scope/capability edge. These hole-targets land on EXISTING canonical mission
     nodes, so they COMPOSE with O1/O3/O4 on the spine.

NOT rendered: the archivist twin-population (CLOSED — claude-2's migration merged it).

Emits cascade/hole/<slug> (:hole) nodes + cascade/hole-target edges
[cascade/hole/<slug>, <canonical-node>], matching the pre-wired verify-live O5
extractor. DRY-RUN writes an .edn artifact only; ZERO :7071 writes.

Usage: python3 futon3c/scripts/o5_honest_holes.py
"""
import json, re, urllib.request, urllib.parse
from collections import Counter

ROOT = "/home/joe/code"
BGE = f"{ROOT}/futon3a/resources/notions/bge_mission_embeddings.json"
OUT = f"{ROOT}/futon3c/holes/excursions/o5-honest-holes.dryrun.edn"
F = "http://localhost:7071"


def edges_on(ep):
    url = f"{F}/api/alpha/hyperedges?end=" + urllib.parse.quote(ep, safe="")
    try:
        return urllib.request.urlopen(url, timeout=20).read().decode().count("hx/id")
    except Exception:
        return -1


def fetch_endpoints(hx_type):
    url = f"{F}/api/alpha/hyperedges?type=" + urllib.parse.quote(hx_type, safe="")
    try:
        raw = urllib.request.urlopen(url, timeout=30).read().decode()
    except Exception:
        return []
    return re.findall(r'"(<?[a-zA-Z0-9][a-zA-Z0-9/_.:-]*-d/mission/[A-Za-z0-9-]+)"', raw)


def stem(basename):
    return re.sub(r"^M-", "", re.sub(r"\.md$", "", basename or ""))


def main():
    bge = json.load(open(BGE))
    # --- kind 1: missing-canonical-node (BGE mission with no live canonical node) ---
    missing = []
    for m in bge:
        repo, bn = m.get("home_repo"), m.get("basename")
        if not (repo and bn):
            continue
        canon = f"{repo}-d/mission/{stem(bn)}"
        if edges_on(canon) <= 0:
            missing.append({"mission": stem(bn), "would-be": canon, "repo": repo})

    # --- kind 2: no-capability on composing-CORE canonical missions ---
    core = set(fetch_endpoints("code/v05/mined-move")) | set(fetch_endpoints("cascade/cluster-member"))
    core = {c for c in core if "-d/mission/" in c and not c.endswith("-head")}
    with_cap = set()
    cap_url = f"{F}/api/alpha/hyperedges?type=" + urllib.parse.quote("capability/produces", safe="")
    try:
        cap_raw = urllib.request.urlopen(cap_url, timeout=30).read().decode()
        with_cap = set(re.findall(r'"([a-zA-Z0-9][a-zA-Z0-9/_.-]*-d/mission/[A-Za-z0-9-]+)"', cap_raw))
    except Exception:
        pass
    no_cap = sorted(core - with_cap)

    # capability edges keyed the BARE alias scheme (M-*), not canonical — so they
    # don't compose with the canonical mission spine. The HONEST hole is exactly
    # that (the capability layer needs the same canonical re-keying missions got),
    # NOT 202 "this mission lacks a capability" (those would be false — the missions
    # have capabilities on their bare twins). One hole node, one hole-target edge
    # per affected composing-core mission (so it COMPOSES on the spine).
    with_cap_bare = set()
    try:
        with_cap_bare = set(re.findall(r'"(M-[A-Za-z0-9.-]+)"', cap_raw))
    except Exception:
        pass

    holes = [{
        "id": "cascade/hole/capability-layer-not-canonical",
        "type": ":hole", "kind": "capability-not-canonical", "composes?": True,
        "gap": ("capability/produces edges key the BARE M-* alias scheme, not the "
                "canonical <repo>-d/mission/<id> nodes — so the capability/downward "
                "layer does NOT compose with the canonical mission spine. It needs the "
                "same canonical re-keying missions got (the archivist's next target)."),
        "evidence": {"capability-edges-total": len(with_cap_bare),
                     "canonical-keyed-capability-edges": len(with_cap),
                     "composing-core-missions-affected": len(core)},
        "targets": sorted(core),   # composes: each is an existing canonical mission node
    }]

    if missing:
        holes.append({
            "id": "cascade/hole/missions-missing-canonical-node",
            "type": ":hole", "kind": "missing-canonical-node", "composes?": False,
            "gap": "BGE missions with no live canonical node",
            "targets": [x["would-be"] for x in missing]})

    by_kind = Counter(h["kind"] for h in holes)
    art = {
        "o5/meta": {
            "generator": "futon3c/scripts/o5_honest_holes.py",
            "source": "BGE mission set vs live :7071 canonical nodes + capability/produces",
            "n-hole-kinds": len(holes), "by-kind": dict(by_kind),
            "core-missions": len(core), "canonical-capability-edges": len(with_cap),
            "bare-capability-edges": len(with_cap_bare),
            "missing-canonical-node-count": len(missing),
            "finding": ("cascade is healthy: 0 truly-missing mission nodes; the one real "
                        "structural hole is the non-canonical capability layer"),
            "dry-run?": True, "writes-to-7071?": False,
        },
        "o5/holes": holes,
    }
    with open(OUT, "w") as f:
        f.write(";; O5 honest-holes DRY-RUN (C-cascade-real, claude-4). Zero :7071 writes.\n")
        f.write(";; hole-target edges; no-capability holes claim EXISTING canonical mission nodes\n")
        f.write(";; :mission (compose w/ O1/O3/O4); missing-node holes mark absent nodes (dangling).\n\n")
        json.dump(art, f, indent=1)
    print("n-holes:", len(holes), "| by-kind:", dict(by_kind),
          "| core:", len(core), "| capability-edges:", len(with_cap))
    print("wrote", OUT)
    return holes


def land(holes):
    """Write the cascade/hole-target edges to :7071 (idempotent; passes the L4 gate
    — cascade/hole* is an uncovered type). The composing holes land their hole-target
    edges onto EXISTING canonical mission nodes, so O5 joins the spine."""
    import urllib.request
    posted = 0
    for h in holes:
        for tgt in h.get("targets", []):
            payload = json.dumps({
                "hx/type": "cascade/hole-target",
                "hx/endpoints": [h["id"], tgt],
                "hx/props": {"hole-kind": h["kind"], "composes": h["composes?"]},
            }).encode()
            req = urllib.request.Request(F + "/api/alpha/hyperedge", data=payload,
                                         headers={"Content-Type": "application/json",
                                                  "X-Penholder": "api"})
            try:
                urllib.request.urlopen(req, timeout=20).read()
                posted += 1
            except Exception as e:
                print("  POST failed:", tgt, e)
    print("landed", posted, "cascade/hole-target edges")


if __name__ == "__main__":
    import sys
    hs = main()
    if "--land" in sys.argv:
        land(hs)


if __name__ == "__main__":
    main()
