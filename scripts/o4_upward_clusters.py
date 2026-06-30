#!/usr/bin/env python3
"""O4 — the cascade's UPWARD layer (C-cascade-real, claude-10).

Cluster the canonical mission nodes into the high-level "basins" above individual
missions, REGENERATED from live data with ZERO hand rows, keyed on the canonical
`<repo>-d/mission/<id>` nodes so O4 composes with O1/O3 on the mission spine.

Approach (corrected during IDENTIFY): `mission-scope-in/out` edges are empty in
live :7071, and the proven mission clustering is EMBEDDING-based — so this adapts
`stack_semilattice_clusters.py`'s AgglomerativeClustering over the BGE mission
embeddings, then keys each clustered mission to its LIVE canonical node id (via the
:7071 mined-move endpoints), emitting:
  - cluster nodes   `cascade/cluster/<nn-slug>`   type :cluster
  - member edges    `:cascade/cluster-member`  [cluster, <repo>-d/mission/<id>]

DRY-RUN: writes an .edn artifact only; ZERO :7071 writes. The edges claim the
canonical mission nodes :mission (composing with O1/O3); cluster nodes are a new
disjoint :cluster type.

Usage: futon3a/.venv/bin/python futon3c/scripts/o4_upward_clusters.py
"""
import json, re, sys, urllib.request, urllib.parse
from collections import Counter
import numpy as np
from sklearn.cluster import AgglomerativeClustering

ROOT = "/home/joe/code"
BGE = f"{ROOT}/futon3a/resources/notions/bge_mission_embeddings.json"
OUT = f"{ROOT}/futon3c/holes/excursions/o4-upward-clusters.dryrun.edn"
FUTON1A = "http://localhost:7071"
N_CLUSTERS = 12
GENERIC = {"the", "a", "of", "to", "and", "mission", "m", "for", "in", "on", "as"}


def slugify(s):
    return re.sub(r"[^a-z0-9]+", "-", re.sub(r"^M-", "", str(s)).lower()).strip("-")


def fetch_live_mission_index():
    """Live canonical mission node ids from :7071 mined-move endpoints → slug→id."""
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
    if isinstance(x, dict):
        return "{" + " ".join(f"{edn(k)} {edn(v)}" for k, v in x.items()) + "}"
    if isinstance(x, (list, tuple)):
        return "[" + " ".join(edn(v) for v in x) + "]"
    if isinstance(x, Kw):
        return str(x)
    if isinstance(x, bool):
        return "true" if x else "false"
    if isinstance(x, (int, float)):
        return str(x)
    return q + str(x).replace("\\", "\\\\").replace('"', '\\"') + q


class Kw(str):
    pass


def main():
    bge = json.load(open(BGE))
    live = fetch_live_mission_index()
    print(f"BGE missions: {len(bge)} | live canonical mission nodes: {len(live)}")

    rows = []  # (canonical-id, vector, basename)
    unresolved = []
    for m in bge:
        s = slugify(m["basename"])
        cid = live.get(s)
        if cid and m.get("vector"):
            rows.append((cid, np.asarray(m["vector"], dtype=float), m["basename"]))
        else:
            unresolved.append(m["basename"])
    print(f"RESOLVED (compose with live nodes): {len(rows)} | unresolved (no live node): {len(unresolved)}")
    if len(rows) < N_CLUSTERS:
        print("too few resolved missions to cluster", file=sys.stderr); sys.exit(1)

    V = np.vstack([r[1] for r in rows])
    # L2-normalize so euclidean ward ≈ cosine; ward gives balanced basins
    # (average-linkage cosine chained into one 115-mission mega-basin).
    Vn = V / (np.linalg.norm(V, axis=1, keepdims=True) + 1e-12)
    labels = AgglomerativeClustering(n_clusters=N_CLUSTERS, linkage="ward").fit_predict(Vn)

    clusters, edges = [], []
    for c in sorted(set(labels)):
        members = [rows[i] for i in range(len(rows)) if labels[i] == c]
        lab = cluster_label([b for _, _, b in members])
        cid = f"cascade/cluster/{c:02d}-{lab}"
        clusters.append({Kw(":id"): cid, Kw(":type"): Kw(":cluster"), Kw(":label"): lab,
                         Kw(":member-count"): len(members),
                         Kw(":members"): [mid for mid, _, _ in members]})
        for mid, _, _ in members:
            edges.append({Kw(":hx/id"): f"hx:cascade/cluster-member:{cid}.{mid}",
                          Kw(":hx/type"): Kw(":cascade/cluster-member"),
                          Kw(":hx/endpoints"): [cid, mid],
                          Kw(":hx/ends"): [{Kw(":entity-id"): cid}, {Kw(":entity-id"): mid}]})

    doc = {Kw(":o4/meta"): {Kw(":source"): "BGE mission embeddings + live :7071 canonical nodes",
                            Kw(":generator"): "futon3c/scripts/o4_upward_clusters.py",
                            Kw(":n-clusters"): N_CLUSTERS, Kw(":resolved"): len(rows),
                            Kw(":unresolved"): len(unresolved), Kw(":dry-run?"): True,
                            Kw(":writes-to-7071?"): False},
           Kw(":o4/clusters"): clusters,
           Kw(":o4/cluster-member-edges"): edges}
    with open(OUT, "w") as f:
        f.write(";; O4 upward-structure DRY-RUN (C-cascade-real, claude-10). Zero :7071 writes.\n")
        f.write(";; cluster-member edges claim canonical mission nodes :mission (compose w/ O1/O3).\n\n")
        f.write(edn(doc) + "\n")

    print(f"\nwrote {OUT}")
    print(f"clusters: {len(clusters)} | cluster-member edges: {len(edges)}")
    print("\nsample clusters:")
    for cl in clusters[:4]:
        print(f"  {cl[Kw(':id')]}  ({cl[Kw(':member-count')]} missions)")
    # 3-way compose check
    for cl in clusters:
        ac = [m for m in cl[Kw(":members")] if "autoclock-in" in m]
        if ac:
            print(f"\n3-way compose: {ac[0]} is in {cl[Kw(':id')]} (O4×O1×O3 on the shared mission node)")
    print("\nsample edge:", edn(edges[0]))


if __name__ == "__main__":
    main()
