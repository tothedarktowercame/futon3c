#!/usr/bin/env python3
"""operator_turn_cluster.py — hierarchical clustering of the operator corpus.

Ward linkage over 16.5k turns directly would need a ~1.1 GB condensed distance
matrix, so this takes the standard two-stage route: tf-idf -> SVD -> k-means
microclusters -> Ward over the microcluster centroids.  The dendrogram is the
output that matters; flat cuts are conveniences taken from it.

Function words are KEPT.  In dictated operator turns the hedges, negations and
modals are what separate an approval from a deferral, so a stopword list would
throw away the signal we are looking for.

No register filter is applied.  The corpus mixes conversational direction with
templated dispatch (wake checklists, deadline blocks); letting the clustering
separate those is the point, rather than deciding the split by hand first.

Needs scikit-learn: run under /home/joe/code/futon6/.venv/bin/python.

  operator_turn_cluster.py [CORPUS.jsonl] [OUTDIR]
"""
import collections, json, os, re, sys
import numpy as np
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.decomposition import TruncatedSVD
from sklearn.cluster import MiniBatchKMeans
from scipy.cluster.hierarchy import linkage, fcluster, dendrogram

CORPUS = sys.argv[1] if len(sys.argv) > 1 else \
    "/home/joe/code/storage/operator-turns/operator-turns.jsonl"
OUT = sys.argv[2] if len(sys.argv) > 2 else \
    "/home/joe/code/storage/operator-turns/analysis"

MACHINE = re.compile(r"\[Session-mode structural analysis request.*?\[End structural analysis request\]", re.S)
RESUMED = re.compile(r"\n?-{3} resumed:.*\Z", re.S)
CODEISH = re.compile(r"```.*?```", re.S)

MICRO = 600          # microclusters; ~27 turns each at this corpus size
SVD_DIMS = 200
CUTS = [6, 12, 24, 48, 96]


def clean(t):
    return CODEISH.sub(" ", RESUMED.sub(" ", MACHINE.sub(" ", t or "")))


def main():
    rows = [json.loads(l) for l in open(CORPUS)]
    docs = [clean(r.get("text")) for r in rows]
    keep = [i for i, d in enumerate(docs) if len(d.split()) >= 3]
    print(f"{len(rows)} turns, {len(keep)} with >=3 words after cleaning", flush=True)

    vec = TfidfVectorizer(lowercase=True, ngram_range=(1, 3), min_df=5,
                          max_df=0.6, sublinear_tf=True, stop_words=None,
                          token_pattern=r"[A-Za-z0-9']+")
    X = vec.fit_transform([docs[i] for i in keep])
    print(f"tf-idf {X.shape}", flush=True)

    Z = TruncatedSVD(n_components=SVD_DIMS, random_state=0).fit_transform(X)
    Z /= (np.linalg.norm(Z, axis=1, keepdims=True) + 1e-9)
    print(f"svd {Z.shape}, explained-variance kept", flush=True)

    km = MiniBatchKMeans(n_clusters=MICRO, random_state=0, n_init=5,
                         batch_size=4096).fit(Z)
    print(f"microclusters {MICRO}", flush=True)

    link = linkage(km.cluster_centers_, method="ward")
    np.save(f"{OUT}/linkage.npy", link)
    np.save(f"{OUT}/micro-assign.npy", km.labels_)

    terms = np.array(vec.get_feature_names_out())
    report = {"turns": len(keep), "microclusters": MICRO, "cuts": {}}
    for k in CUTS:
        flat = fcluster(link, k, criterion="maxclust")
        turn_cluster = flat[km.labels_]
        groups = collections.Counter(turn_cluster)
        entry = []
        for c, n in groups.most_common():
            idx = np.where(turn_cluster == c)[0]
            centroid = np.asarray(X[idx].mean(axis=0)).ravel()
            top = terms[np.argsort(centroid)[::-1][:12]].tolist()
            sample = [docs[keep[i]][:120].replace("\n", " ") for i in idx[:3]]
            entry.append({"cluster": int(c), "turns": int(n),
                          "terms": top, "sample": sample})
        report["cuts"][str(k)] = entry
        print(f"cut k={k}: sizes {[e['turns'] for e in entry][:12]}", flush=True)

    json.dump(report, open(f"{OUT}/cluster-report.json", "w"), indent=2)
    with open(f"{OUT}/turn-clusters.tsv", "w") as f:
        flat = fcluster(link, CUTS[-1], criterion="maxclust")
        turn_cluster = flat[km.labels_]
        f.write("turn_id\tat\tmicro\tcluster_k%d\n" % CUTS[-1])
        for j, i in enumerate(keep):
            r = rows[i]
            f.write(f"{r.get('turn_id')}\t{r.get('at')}\t{km.labels_[j]}\t{turn_cluster[j]}\n")
    print(f"wrote {OUT}/cluster-report.json and turn-clusters.tsv")


if __name__ == "__main__":
    main()
