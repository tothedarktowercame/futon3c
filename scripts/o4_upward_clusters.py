#!/usr/bin/env python3
"""O4 — the cascade's UPWARD layer (C-cascade-real, claude-10).

Cluster the canonical mission nodes into high-level "basins" above individual
missions, REGENERATED from live data with ZERO hand rows, keyed on the canonical
`<repo>-d/mission/<id>` nodes so O4 composes with O1/O3 on the mission spine.

The mission population and its pattern weights come from the complete, paged
`mission-scope/pattern` layer in futon1b.  Mission geometry remains the historical
BGE embedding model and fixed 12-cluster parameter, so regenerations are comparable.

Emits THREE artifacts (zero substrate writes — landing is the bb lander):
  - o4-upward-clusters.dryrun.edn   the readable cluster/edge view
  - o4-land-payloads.edn            canonical POST payloads (writer shape:
                                    :hx/type STRING, :hx/endpoints {:role :entity-id})
  - o4-unresolved.edn               cited canonical missions with no BGE vector
                                    (O4's honest-hole report; O5 feed)

Dedup: one mission → one basin (first cluster wins), so the node-twins
(e.g. interest-network-coupling-aif-wiring ×2) write a single member edge.

Usage: futon3a/.venv/bin/python futon3c/scripts/o4_upward_clusters.py
"""
import json, os, re, sys, urllib.request, urllib.parse
from collections import Counter
import numpy as np
from sklearn.cluster import AgglomerativeClustering

ROOT = "/home/joe/code"
BGE = f"{ROOT}/futon3a/resources/notions/bge_mission_embeddings.json"
EXC = f"{ROOT}/futon3c/holes/excursions"
OUT_DRY = f"{EXC}/o4-upward-clusters.dryrun.edn"
OUT_LAND = f"{EXC}/o4-land-payloads.edn"
OUT_UNRES = f"{EXC}/o4-unresolved.edn"
FUTON1B = os.environ.get("FUTON1B_URL", "http://localhost:7073")
N_CLUSTERS = 12
PAGE_SIZE = 250
PAGE_BUDGET = 20
GENERIC = {"the", "a", "of", "to", "and", "mission", "m", "for", "in", "on", "as"}


class Kw(str):
    pass


def slugify(s):
    return re.sub(r"[^a-z0-9]+", "-", re.sub(r"^M-", "", str(s)).lower()).strip("-")


def fetch_json(path, params):
    url = f"{FUTON1B}{path}?" + urllib.parse.urlencode(params)
    req = urllib.request.Request(url, headers={"Accept": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=60) as response:
            if response.status < 200 or response.status >= 300:
                raise RuntimeError(f"GET {url} returned HTTP {response.status}")
            return json.load(response)
    except Exception as exc:
        raise RuntimeError(f"required futon1b read failed: {url}: {exc}") from exc


def fetch_hyperedges(hx_type):
    """Exact cursor walk. A failed, partial, repeated, or inconsistent read aborts."""
    rows, after, seen_cursors, expected = [], None, set(), None
    for request_no in range(PAGE_BUDGET):
        params = {"type": hx_type, "limit": PAGE_SIZE}
        if after:
            params["after"] = after
        body = fetch_json("/api/alpha/hyperedges", params)
        if body.get("count-exact?") is not True:
            raise RuntimeError(f"{hx_type}: server did not provide an exact total")
        if expected is None:
            expected = body.get("count")
        elif body.get("count") != expected:
            raise RuntimeError(f"{hx_type}: total changed during paging")
        page = body.get("hyperedges")
        if not isinstance(page, list):
            raise RuntimeError(f"{hx_type}: response has no hyperedge vector")
        rows.extend(page)
        cursor = body.get("next-cursor")
        if not cursor:
            break
        if cursor in seen_cursors:
            raise RuntimeError(f"{hx_type}: repeated cursor {cursor}")
        seen_cursors.add(cursor)
        after = cursor
    else:
        raise RuntimeError(f"{hx_type}: exceeded {PAGE_BUDGET}-request paging budget")
    ids = [row.get("hx/id") for row in rows]
    if len(rows) != expected or len(set(ids)) != len(ids) or None in ids:
        raise RuntimeError(
            f"{hx_type}: incomplete/duplicate read: rows={len(rows)} total={expected} "
            f"distinct-ids={len(set(ids))}"
        )
    return rows


def role_end(hyperedge, role):
    for end in hyperedge.get("hx/ends") or []:
        if end.get("role", "").lstrip(":") == role:
            return end.get("entity-id")


def cited_pattern(hyperedge):
    target = role_end(hyperedge, "target-pattern")
    if target:
        return target
    ref = (hyperedge.get("hx/props") or {}).get("pattern/ref")
    match = re.fullmatch(r"[^/]+/library/(.+)\.flexiarg", str(ref or ""))
    return match.group(1) if match else None


def live_citations():
    hyperedges = fetch_hyperedges("mission-scope/pattern")
    citations = []
    for edge in hyperedges:
        mission = role_end(edge, "entity")
        pattern = cited_pattern(edge)
        if not mission or not re.search(r"-d/mission/", mission) or not pattern:
            raise RuntimeError(f"unreadable citation {edge.get('hx/id')}: {mission=} {pattern=}")
        citations.append({"mission": mission, "pattern": pattern, "hx/id": edge["hx/id"]})
    return hyperedges, citations


def cluster_label(basenames):
    toks = Counter(t for b in basenames for t in slugify(b).split("-") if t not in GENERIC and len(t) > 2)
    return "-".join(t for t, _ in toks.most_common(2)) or "cluster"


def edn(x, q='"'):
    if isinstance(x, Kw):
        return str(x)
    if isinstance(x, dict):
        return "{" + " ".join(f"{edn(k)} {edn(v)}" for k, v in x.items()) + "}"
    if isinstance(x, (list, tuple)):
        return "[" + " ".join(edn(v) for v in x) + "]"
    if isinstance(x, bool):
        return "true" if x else "false"
    if isinstance(x, (int, float)):
        return str(x)
    return q + str(x).replace("\\", "\\\\").replace('"', '\\"') + q


def main():
    bge = json.load(open(BGE))
    hyperedges, citations = live_citations()
    cited_missions = sorted(set(c["mission"] for c in citations))
    cited_patterns = sorted(set(c["pattern"] for c in citations))
    live = {mid.split("/mission/", 1)[1]: mid for mid in cited_missions}
    print(
        f"paged mission-scope/pattern: {len(hyperedges)} | "
        f"citations: {len(citations)} | missions: {len(cited_missions)} | "
        f"patterns: {len(cited_patterns)}"
    )

    bge_by_slug = {}
    for mission in bge:
        bge_by_slug.setdefault(slugify(mission["basename"]), mission)
    rows, unresolved = [], []
    for slug, cid in sorted(live.items()):
        mission = bge_by_slug.get(slug)
        if mission and mission.get("vector"):
            rows.append((cid, np.asarray(mission["vector"], dtype=float), mission["basename"]))
        else:
            unresolved.append({Kw(":mission"): cid, Kw(":slug"): slug,
                               Kw(":reason"): ("no-bge-mission" if not mission else "no-vector")})
    print(f"RESOLVED cited missions with BGE vectors: {len(rows)} | unresolved: {len(unresolved)}")
    if len(rows) < N_CLUSTERS:
        print("too few resolved to cluster", file=sys.stderr); sys.exit(1)

    V = np.vstack([r[1] for r in rows])
    Vn = V / (np.linalg.norm(V, axis=1, keepdims=True) + 1e-12)   # L2-norm ⇒ euclidean ward ≈ cosine
    labels = AgglomerativeClustering(n_clusters=N_CLUSTERS, linkage="ward").fit_predict(Vn)

    # one mission → one basin (first occurrence wins) — dedups node-twins
    mission_cluster, base_by_cluster = {}, {}
    for i, (mid, _, b) in enumerate(rows):
        if mid not in mission_cluster:
            mission_cluster[mid] = int(labels[i])
            base_by_cluster.setdefault(int(labels[i]), []).append(b)
    by_cluster = {}
    for mid, c in mission_cluster.items():
        by_cluster.setdefault(c, []).append(mid)

    clusters, edges, entity_payloads, edge_payloads = [], [], [], []
    for c in sorted(by_cluster):
        members = sorted(set(by_cluster[c]))
        lab = cluster_label(base_by_cluster.get(c, []))
        cid = f"cascade/cluster/{c:02d}-{lab}"
        member_set = set(members)
        pattern_counts = Counter(
            citation["pattern"] for citation in citations
            if citation["mission"] in member_set
        )
        most_cited = [
            {Kw(":pattern"): pattern, Kw(":citations"): count}
            for pattern, count in sorted(pattern_counts.items(), key=lambda item: (-item[1], item[0]))[:10]
        ]
        clusters.append({Kw(":id"): cid, Kw(":type"): Kw(":cluster"), Kw(":label"): lab,
                         Kw(":member-count"): len(members), Kw(":members"): members,
                         Kw(":citation-count"): sum(pattern_counts.values()),
                         Kw(":most-cited-patterns"): most_cited})
        entity_payloads.append({Kw(":id"): cid, Kw(":name"): lab,
                                Kw(":type"): "cascade/cluster",
                                Kw(":props"): {Kw(":label"): lab, Kw(":member-count"): len(members),
                                               Kw(":citation-count"): sum(pattern_counts.values()),
                                               Kw(":most-cited-patterns"): most_cited,
                                               Kw(":o4/generated"): True}})
        for mid in members:
            edges.append({Kw(":hx/type"): Kw(":cascade/cluster-member"), Kw(":hx/endpoints"): [cid, mid]})
            edge_payloads.append({
                Kw(":hx/id"): f"hx|cascade-cluster-member|{cid}|{mid}",
                Kw(":hx/type"): "cascade/cluster-member",
                Kw(":hx/endpoints"): [{Kw(":role"): Kw(":cluster"), Kw(":entity-id"): cid},
                                      {Kw(":role"): Kw(":mission"), Kw(":entity-id"): mid}],
                Kw(":hx/labels"): ["cascade/cluster-member"],
                Kw(":props"): {Kw(":cluster"): cid, Kw(":mission"): mid}})

    meta = {Kw(":source"): "BGE mission embeddings + paged futon1b mission-scope/pattern",
            Kw(":generator"): "futon3c/scripts/o4_upward_clusters.py",
            Kw(":n-clusters"): N_CLUSTERS, Kw(":resolved"): len(rows),
            Kw(":missions"): len(mission_cluster), Kw(":edges"): len(edge_payloads),
            Kw(":input-hyperedges"): len(hyperedges),
            Kw(":input-citations"): len(citations),
            Kw(":input-missions"): len(cited_missions),
            Kw(":input-patterns"): len(cited_patterns),
            Kw(":unresolved"): len(unresolved), Kw(":dry-run?"): True}

    with open(OUT_DRY, "w") as f:
        f.write(";; O4 upward-structure DRY-RUN view (C-cascade-real, claude-10).\n\n")
        f.write(edn({Kw(":o4/meta"): meta, Kw(":o4/clusters"): clusters,
                     Kw(":o4/cluster-member-edges"): edges}) + "\n")
    with open(OUT_LAND, "w") as f:
        f.write(";; O4 canonical LAND payloads (writer shape) — POST to futon1b via the bb lander.\n")
        f.write(";; entities → /api/alpha/entity ; hyperedges → /api/alpha/hyperedge ; idempotent ids.\n\n")
        f.write(edn({Kw(":entities"): entity_payloads, Kw(":hyperedges"): edge_payloads}) + "\n")
    with open(OUT_UNRES, "w") as f:
        f.write(";; O4 honest-hole report: cited missions with no BGE vector (O5 feed).\n\n")
        f.write(edn({Kw(":o4/unresolved-count"): len(unresolved), Kw(":o4/unresolved"): unresolved}) + "\n")

    print(f"\nwrote {OUT_DRY}\n      {OUT_LAND}\n      {OUT_UNRES}")
    print(f"clusters: {len(clusters)} | missions (deduped): {len(mission_cluster)} | edges: {len(edge_payloads)}")
    for cl in clusters:
        print(f"  {cl[Kw(':id')]}  ({cl[Kw(':member-count')]})")
    for cl in clusters:
        if any("autoclock-in" in m for m in cl[Kw(":members")]):
            print(f"3-way compose: autoclock-in ∈ {cl[Kw(':id')]}")


if __name__ == "__main__":
    main()
