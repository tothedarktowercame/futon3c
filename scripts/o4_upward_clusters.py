#!/usr/bin/env python3
"""O4 — the cascade's UPWARD layer (C-cascade-real, claude-10).

Cluster the canonical mission nodes into high-level "basins" above individual
missions, REGENERATED from live data with ZERO hand rows, keyed on the canonical
`<repo>-d/mission/<id>` nodes so O4 composes with O1/O3 on the mission spine.

IDENTIFY corrected the plan: `mission-scope-in/out` edges are empty in live :7071,
and the proven mission clustering is embedding-based — so this adapts
`stack_semilattice_clusters.py`'s agglomerative clustering over the BGE mission
embeddings, then keys each clustered mission to its LIVE canonical node id (via the
:7071 mined-move endpoints).

Emits THREE artifacts (zero :7071 writes — landing is the bb lander):
  - o4-upward-clusters.dryrun.edn   the readable cluster/edge view
  - o4-land-payloads.edn            canonical POST payloads (writer shape:
                                    :hx/type STRING, :hx/endpoints {:role :entity-id})
  - o4-unresolved.edn               the 35 BGE missions with no live canonical node
                                    (O4's honest-hole report; O5 feed)

Dedup: one mission → one basin (first cluster wins), so the node-twins
(e.g. interest-network-coupling-aif-wiring ×2) write a single member edge.

Usage: futon3a/.venv/bin/python futon3c/scripts/o4_upward_clusters.py
"""
import json, re, sys, urllib.request, urllib.parse
from collections import Counter
import numpy as np
from sklearn.cluster import AgglomerativeClustering

ROOT = "/home/joe/code"
BGE = f"{ROOT}/futon3a/resources/notions/bge_mission_embeddings.json"
EXC = f"{ROOT}/futon3c/holes/excursions"
OUT_DRY = f"{EXC}/o4-upward-clusters.dryrun.edn"
OUT_LAND = f"{EXC}/o4-land-payloads.edn"
OUT_UNRES = f"{EXC}/o4-unresolved.edn"
FUTON1A = "http://localhost:7071"
N_CLUSTERS = 12
GENERIC = {"the", "a", "of", "to", "and", "mission", "m", "for", "in", "on", "as"}


class Kw(str):
    pass


def slugify(s):
    return re.sub(r"[^a-z0-9]+", "-", re.sub(r"^M-", "", str(s)).lower()).strip("-")


def fetch_live_mission_index():
    idx = {}
    for t in ("code/v05/mined-move", "clock/clocked-on"):
        url = f"{FUTON1A}/api/alpha/hyperedges?type=" + urllib.parse.quote(t, safe="")
        try:
            raw = urllib.request.urlopen(url, timeout=30).read().decode()
        except Exception as e:
            print(f"  WARN live query {t} failed: {e}", file=sys.stderr); continue
        for cid in re.findall(r"[a-z0-9-]+-d/mission/[a-z0-9-]+", raw):
            idx.setdefault(cid.split("/mission/")[1], cid)
    return idx


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
    live = fetch_live_mission_index()
    print(f"BGE missions: {len(bge)} | live canonical mission nodes: {len(live)}")

    rows, unresolved = [], []
    for m in bge:
        s = slugify(m["basename"])
        cid = live.get(s)
        if cid and m.get("vector"):
            rows.append((cid, np.asarray(m["vector"], dtype=float), m["basename"]))
        else:
            unresolved.append({Kw(":basename"): m["basename"], Kw(":slug"): s,
                               Kw(":reason"): ("no-live-node" if not cid else "no-vector")})
    print(f"RESOLVED (compose w/ live nodes): {len(rows)} | unresolved: {len(unresolved)}")
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
        clusters.append({Kw(":id"): cid, Kw(":type"): Kw(":cluster"), Kw(":label"): lab,
                         Kw(":member-count"): len(members), Kw(":members"): members})
        entity_payloads.append({Kw(":id"): cid, Kw(":name"): lab, Kw(":type"): "cluster",
                                Kw(":props"): {Kw(":label"): lab, Kw(":member-count"): len(members),
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

    meta = {Kw(":source"): "BGE mission embeddings + live :7071 canonical nodes",
            Kw(":generator"): "futon3c/scripts/o4_upward_clusters.py",
            Kw(":n-clusters"): N_CLUSTERS, Kw(":resolved"): len(rows),
            Kw(":missions"): len(mission_cluster), Kw(":edges"): len(edge_payloads),
            Kw(":unresolved"): len(unresolved), Kw(":dry-run?"): True}

    with open(OUT_DRY, "w") as f:
        f.write(";; O4 upward-structure DRY-RUN view (C-cascade-real, claude-10).\n\n")
        f.write(edn({Kw(":o4/meta"): meta, Kw(":o4/clusters"): clusters,
                     Kw(":o4/cluster-member-edges"): edges}) + "\n")
    with open(OUT_LAND, "w") as f:
        f.write(";; O4 canonical LAND payloads (writer shape) — POST to :7071 via the bb lander.\n")
        f.write(";; entities → /api/alpha/entity ; hyperedges → /api/alpha/hyperedge ; idempotent ids.\n\n")
        f.write(edn({Kw(":entities"): entity_payloads, Kw(":hyperedges"): edge_payloads}) + "\n")
    with open(OUT_UNRES, "w") as f:
        f.write(";; O4 honest-hole report: BGE missions with no live canonical node (O5 feed).\n\n")
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
