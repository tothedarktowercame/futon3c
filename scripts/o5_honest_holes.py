#!/usr/bin/env python3
"""O5 — honest holes (C-cascade-real, claude-4). The cascade's COVERAGE-GAP dimension.

A "rendered hole resolves to a REAL, queryable coverage gap" (CHARTER standard 4).
Computed live, zero hand rows. Two gap kinds, both grounded in substrate-2:

  1a. missing-canonical-node — a BGE mission with NO entity at its canonical
     <repo>-d/mission/<id>. The gap IS the missing node, so its hole-target points
     at the would-be canonical id (a deliberately dangling marker); does not compose.
  1b. canonical-node-without-edges — the canonical entity EXISTS but sits in zero
     hyperedges: present in the substrate, absent from the graph. Its hole-target
     lands on a REAL node, so it composes with O1/O3/O4.
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
import json, os, re, urllib.error, urllib.request, urllib.parse
from collections import Counter
from time import monotonic

ROOT = "/home/joe/code"
BGE = f"{ROOT}/futon3a/resources/notions/bge_mission_embeddings.json"
OUT = f"{ROOT}/futon3c/holes/excursions/o5-honest-holes.dryrun.edn"
F = os.environ.get("FUTON1B_URL", "http://localhost:7073")
PAGE_LIMIT = int(os.environ.get("O5_PAGE_LIMIT", "1000"))


class SubstrateReadError(RuntimeError):
    """A substrate read failed or returned an internally inconsistent page."""


def get(url, timeout):
    try:
        with urllib.request.urlopen(url, timeout=timeout) as response:
            return response.read().decode()
    except Exception as exc:
        raise SubstrateReadError(f"GET {url} failed: {exc}") from exc


def edn_int(raw, key):
    match = re.search(rf":{re.escape(key)}\s+(\d+)", raw)
    return int(match.group(1)) if match else None


def edn_string(raw, key):
    match = re.search(rf':{re.escape(key)}\s+"([^"]*)"', raw)
    return match.group(1) if match else None


def fetch_pages(hx_type):
    """Fetch a complete type window, failing if paging loses or duplicates rows."""
    pages = []
    after = None
    expected = None
    seen_ids = set()
    while True:
        params = {
            "type": hx_type,
            "include-total": "true",
            "limit": str(PAGE_LIMIT),
        }
        if after is not None:
            params["after"] = after
        raw = get(f"{F}/api/alpha/hyperedges?{urllib.parse.urlencode(params)}", 30)
        total = edn_int(raw, "count")
        if total is None:
            raise SubstrateReadError(f"missing :count in {hx_type!r} response")
        if expected is None:
            expected = total
        elif total != expected:
            raise SubstrateReadError(
                f"{hx_type!r} count changed during paging: {expected} -> {total}"
            )
        page_ids = re.findall(r':hx/id\s+"([^"]+)"', raw)
        duplicates = seen_ids.intersection(page_ids)
        if duplicates:
            raise SubstrateReadError(
                f"{hx_type!r} paging repeated ids: {sorted(duplicates)[:3]}"
            )
        seen_ids.update(page_ids)
        pages.append(raw)
        cursor = edn_string(raw, "next-cursor")
        if cursor is None:
            break
        if cursor == after:
            raise SubstrateReadError(f"{hx_type!r} paging cursor did not advance")
        after = cursor
    if len(seen_ids) != expected:
        raise SubstrateReadError(
            f"{hx_type!r} paging returned {len(seen_ids)} rows, expected {expected}"
        )
    return pages, {"rows": len(seen_ids), "limit": PAGE_LIMIT, "pages": len(pages)}


def entity_exists(entity_id):
    """True if a canonical node is present at ENTITY_ID.

    A 404 is a real ANSWER (the node is absent), not a failed read. Anything
    else is a read failure and must abort — an infrastructure error must never
    enter the artifact as a finding.
    """
    url = f"{F}/api/alpha/entity/" + urllib.parse.quote(entity_id, safe="")
    try:
        with urllib.request.urlopen(url, timeout=20) as response:
            return 200 <= response.status < 300
    except urllib.error.HTTPError as exc:
        if exc.code == 404:
            return False
        raise SubstrateReadError(f"GET {url} failed: {exc}") from exc
    except Exception as exc:
        raise SubstrateReadError(f"GET {url} failed: {exc}") from exc


def edges_on(ep):
    params = {"end": ep, "include-total": "true", "limit": "1"}
    url = f"{F}/api/alpha/hyperedges?{urllib.parse.urlencode(params)}"
    raw = get(url, 20)
    count = edn_int(raw, "count")
    if count is None:
        raise SubstrateReadError(f"missing :count in endpoint response for {ep!r}")
    return count


def fetch_endpoints(hx_type):
    pages, paging = fetch_pages(hx_type)
    endpoints = set()
    for raw in pages:
        endpoints.update(re.findall(
            r'(<?[a-zA-Z0-9][a-zA-Z0-9/_.:-]*-d/mission/[A-Za-z0-9-]+)',
            raw,
        ))
    return endpoints, paging


def capability_keys():
    pages, paging = fetch_pages("mission-scope/capability-scope")
    canonical_missions = set()
    bare_missions = set()
    canonical_edges = 0
    bare_edges = 0
    for raw in pages:
        for endpoints in re.findall(
            r":hx/endpoints\s+\[(.*?)\](?=,\s+:[a-zA-Z])", raw, re.DOTALL
        ):
            canonical = set(re.findall(
                r'([a-zA-Z0-9][a-zA-Z0-9/_.:-]*-d/mission/[A-Za-z0-9-]+)',
                endpoints,
            ))
            bare = set(re.findall(r'(?<![A-Za-z0-9])(M-[A-Za-z0-9.-]+)', endpoints))
            if canonical:
                canonical_edges += 1
                canonical_missions.update(canonical)
            if bare:
                bare_edges += 1
                bare_missions.update(bare)
    classified = canonical_edges + bare_edges
    if classified > paging["rows"]:
        raise SubstrateReadError("capability edge key classifications overlap")
    return {
        "canonical-missions": canonical_missions,
        "bare-missions": bare_missions,
        "canonical-edges": canonical_edges,
        "bare-edges": bare_edges,
        "unclassified-edges": paging["rows"] - classified,
        "paging": paging,
    }


def stem(basename):
    return re.sub(r"^M-", "", re.sub(r"\.md$", "", basename or ""))


def main():
    started = monotonic()
    with open(BGE) as stream:
        bge = json.load(stream)
    # --- kind 1: canonical-node gaps, split by CAUSE ---
    # Two different gaps hide behind one BGE miss, and `edges_on(canon) == 0`
    # cannot tell them apart:
    #   absent      — no entity at the canonical id. The gap IS the missing
    #                 node, so the hole-target is a deliberately dangling
    #                 marker and does NOT compose with the mission spine.
    #   unconnected — the canonical entity EXISTS but sits in zero hyperedges.
    #                 The node is real, so this hole-target lands on an
    #                 existing node and DOES compose with O1/O3/O4.
    # Measured 2026-08-23: 6 of the 7 BGE misses were `unconnected`, not
    # absent. Reporting those as "missing canonical node" publishes a false
    # claim on the live cascade page, and points a marker documented as
    # "would-be / dangling" at a node that is actually there.
    absent = []
    unconnected = []
    for m in bge:
        repo, bn = m.get("home_repo"), m.get("basename")
        if not (repo and bn):
            continue
        mission = stem(bn)
        if mission.endswith("-head"):
            continue  # `*-head` are template/index docs, not missions
        canon = f"{repo}-d/mission/{mission}"
        if edges_on(canon) != 0:
            continue
        row = {"mission": mission, "canonical": canon, "repo": repo}
        (unconnected if entity_exists(canon) else absent).append(row)

    # --- kind 2: no-capability on composing-CORE canonical missions ---
    mined, mined_paging = fetch_endpoints("code/v05/mined-move")
    clustered, clustered_paging = fetch_endpoints("cascade/cluster-member")
    core = set(mined) | set(clustered)
    core = {c for c in core if "-d/mission/" in c and not c.endswith("-head")}
    capability = capability_keys()
    with_cap = capability["canonical-missions"]
    no_cap = sorted(core - with_cap)

    holes = []
    if capability["bare-edges"]:
        holes.append({
            "id": "cascade/hole/capability-layer-not-canonical",
            "type": ":hole", "kind": "capability-not-canonical", "composes?": True,
            "gap": ("mission-scope/capability-scope contains edges keyed by BARE "
                    "M-* aliases rather than canonical <repo>-d/mission/<id> nodes"),
            "evidence": {
                "capability-edges-total": capability["paging"]["rows"],
                "bare-keyed-capability-edges": capability["bare-edges"],
                "canonical-keyed-capability-edges": capability["canonical-edges"],
                "unclassified-capability-edges": capability["unclassified-edges"],
                "composing-core-missions-affected": len(core),
            },
            "targets": sorted(core),
        })

    if no_cap:
        holes.append({
            "id": "cascade/hole/core-missions-without-capability",
            "type": ":hole", "kind": "no-capability", "composes?": True,
            "gap": "Composing-core canonical missions with no capability-scope edge",
            "targets": no_cap,
        })

    if absent:
        holes.append({
            "id": "cascade/hole/missions-missing-canonical-node",
            "type": ":hole", "kind": "missing-canonical-node", "composes?": False,
            "gap": "BGE missions with no entity at their canonical id",
            "targets": [x["canonical"] for x in absent]})

    if unconnected:
        holes.append({
            "id": "cascade/hole/canonical-node-without-edges",
            "type": ":hole", "kind": "canonical-node-without-edges",
            "composes?": True,
            "gap": ("the canonical mission node EXISTS but participates in zero "
                    "hyperedges — the mission is in the substrate and absent "
                    "from the graph, so no cascade dimension can reach it"),
            "targets": [x["canonical"] for x in unconnected]})

    by_kind = Counter(h["kind"] for h in holes)
    elapsed = monotonic() - started
    finding = (
        f"{len(holes)} honest hole kinds: {dict(by_kind)}; "
        f"{len(absent)} BGE missions have no canonical entity, "
        f"{len(unconnected)} have one with zero hyperedges; "
        f"capability keys are {capability['canonical-edges']} canonical, "
        f"{capability['bare-edges']} bare, {capability['unclassified-edges']} unclassified"
    )
    art = {
        "o5/meta": {
            "generator": "futon3c/scripts/o5_honest_holes.py",
            "source": ("BGE mission set vs live :7073 canonical nodes + "
                       "mission-scope/capability-scope"),
            "n-hole-kinds": len(holes), "by-kind": dict(by_kind),
            "core-missions": len(core),
            "canonical-capability-edges": capability["canonical-edges"],
            "bare-capability-edges": capability["bare-edges"],
            "unclassified-capability-edges": capability["unclassified-edges"],
            "paging": {
                "code/v05/mined-move": mined_paging,
                "cascade/cluster-member": clustered_paging,
                "mission-scope/capability-scope": capability["paging"],
            },
            "missing-canonical-node-count": len(absent),
            "canonical-node-without-edges-count": len(unconnected),
            "finding": finding,
            "wall-clock-seconds": round(elapsed, 3),
            "dry-run?": True, "writes-to-7073?": False,
        },
        "o5/holes": holes,
    }
    with open(OUT, "w") as f:
        f.write(";; O5 honest-holes DRY-RUN (C-cascade-real). Zero :7073 writes.\n")
        f.write(";; hole-target edges; no-capability holes claim EXISTING canonical mission nodes\n")
        f.write(";; :mission (compose w/ O1/O3/O4); missing-node holes mark absent nodes (dangling);\n"
        f";; without-edges holes land on REAL nodes and compose.\n\n")
        json.dump(art, f, indent=1)
    print("n-holes:", len(holes), "| by-kind:", dict(by_kind),
          "| core:", len(core), "| capability-edges:", capability["paging"]["rows"],
          "| absent:", len(absent), "| unconnected:", len(unconnected),
          "| seconds:", round(elapsed, 3))
    print("paging:", art["o5/meta"]["paging"])
    print("wrote", OUT)
    return holes


def land(holes):
    """Write the cascade/hole-target edges to :7073 (idempotent; passes the L4 gate
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
