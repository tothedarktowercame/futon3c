#!/usr/bin/env python3
"""Preregistered S1 git-history hypergraph experiment.

The vertices are commits.  Every relation is an incidence hyperedge; this file
never clique-expands a file or other many-commit relation.  The null rewires
the bipartite incidence graph by double-edge swaps, preserving every commit
degree and every hyperedge degree exactly.
"""

from __future__ import annotations

import argparse
import hashlib
import itertools
import json
import math
import os
import random
import re
import subprocess
from collections import Counter, defaultdict
from pathlib import Path

import numpy as np
import scipy
from scipy import sparse
from scipy.sparse.linalg import eigsh

PIN = "d722f772ede949719948aec76839d4d5e83586b0"
SIGNAL = "git-history-topology"
COCHANGE_MIN_COUNT = 3
COCHANGE_MIN_JACCARD = 0.10
SUBSYSTEM_DEPTH = 3
TEMPORAL_WINDOW = "author-date-utc-day"
SEED = 20260731
CONTROL_SRC = "src/futon3c/transport/http.clj"
CONTROL_TEST = "test/futon3c/transport/http_test.clj"
JOE_ALIASES = {"Joe Corneli", "Joseph Corneli"}


def git(repo: Path, *args: str) -> str:
    return subprocess.check_output(
        ["git", "-C", str(repo), *args], text=True, encoding="utf-8"
    )


def subsystem(path: str) -> str:
    parts = path.split("/")
    return "/".join(parts[: min(SUBSYSTEM_DEPTH, max(1, len(parts) - 1))])


def load_commits(repo: Path) -> list[dict]:
    fmt = "%x1e%H%x1f%P%x1f%aN%x1f%aE%x1f%aI%x1f%cI%x1f%s"
    raw = git(repo, "log", PIN, "--reverse", "--format=" + fmt, "--name-only")
    commits = []
    for record in raw.split("\x1e"):
        record = record.strip("\n")
        if not record:
            continue
        header, *tail = record.splitlines()
        fields = header.split("\x1f")
        if len(fields) != 7:
            raise ValueError(f"malformed git record: {header!r}")
        sha, parents, author, email, author_at, commit_at, subject = fields
        files = sorted({p.strip() for p in tail if p.strip()})
        commits.append({
            "sha": sha, "parents": parents.split(), "author": author,
            "email": email, "author_at": author_at, "commit_at": commit_at,
            "subject": subject, "files": files,
        })
    return commits


def derive(commits: list[dict]) -> tuple[list[dict], dict]:
    by_sha = {c["sha"]: i for i, c in enumerate(commits)}
    file_commits: dict[str, list[int]] = defaultdict(list)
    subsystem_commits: dict[str, list[int]] = defaultdict(list)
    day_commits: dict[str, list[int]] = defaultdict(list)
    refs: dict[str, list[int]] = defaultdict(list)
    pair_count: Counter[tuple[str, str]] = Counter()
    ref_re = re.compile(r"(?<![0-9a-f])(?:[0-9a-f]{7,40})(?![0-9a-f])|\b[A-Z][0-9]?(?:-[A-Za-z0-9]+)+\b")

    for i, c in enumerate(commits):
        for path in c["files"]:
            file_commits[path].append(i)
            subsystem_commits[subsystem(path)].append(i)
        day_commits[c["author_at"][:10]].append(i)
        for token in sorted(set(ref_re.findall(c["subject"]))):
            refs[token].append(i)
        for a, b in itertools.combinations(c["files"], 2):
            pair_count[(a, b)] += 1

    edges: list[dict] = []
    def add(kind: str, key: str, vertices) -> None:
        vs = sorted(set(vertices))
        if len(vs) >= 2:
            edges.append({"kind": kind, "key": key, "vertices": vs})

    for i, c in enumerate(commits):
        add("parent", c["sha"], [i] + [by_sha[p] for p in c["parents"] if p in by_sha])
    for key, vs in sorted(file_commits.items()):
        add("same-file", key, vs)
    for key, vs in sorted(subsystem_commits.items()):
        add("same-subsystem", key, vs)
    qualifying = []
    for (a, b), count in sorted(pair_count.items()):
        union_n = len(set(file_commits[a]) | set(file_commits[b]))
        jaccard = count / union_n
        if count >= COCHANGE_MIN_COUNT and jaccard >= COCHANGE_MIN_JACCARD:
            joint = sorted(set(file_commits[a]) & set(file_commits[b]))
            add("co-change", a + " <-> " + b, joint)
            qualifying.append({"a": a, "b": b, "count": count, "jaccard": jaccard})
    for key, vs in sorted(day_commits.items()):
        add("temporal", key, vs)
    for key, vs in sorted(refs.items()):
        add("references", key, vs)
    edges.sort(key=lambda e: (e["kind"], e["key"]))

    control = next((x for x in qualifying
                    if {x["a"], x["b"]} == {CONTROL_SRC, CONTROL_TEST}), None)
    ranked = sorted(qualifying, key=lambda x: (-x["count"], x["a"], x["b"]))
    if control:
        control = dict(control, rank=ranked.index(control) + 1)
    return edges, {
        "file_churn": {CONTROL_SRC: len(file_commits[CONTROL_SRC]),
                       CONTROL_TEST: len(file_commits[CONTROL_TEST])},
        "cochange": control,
        "qualifying_cochange_pairs": len(qualifying),
    }


def incidence(n: int, edges: list[dict]) -> sparse.csr_matrix:
    rows, cols = [], []
    for j, edge in enumerate(edges):
        rows.extend(edge["vertices"])
        cols.extend([j] * len(edge["vertices"]))
    return sparse.csr_matrix((np.ones(len(rows)), (rows, cols)), shape=(n, len(edges)))


def lambda2_pair(H: sparse.csr_matrix) -> tuple[float, float]:
    dv = np.asarray(H.sum(axis=1)).ravel()
    de = np.asarray(H.sum(axis=0)).ravel()
    if np.any(dv == 0) or np.any(de == 0):
        raise ValueError("isolated vertex or empty hyperedge")
    A = H @ sparse.diags(1.0 / de) @ H.T
    Lu = sparse.diags(dv) - A
    invsqrt = sparse.diags(1.0 / np.sqrt(dv))
    Ln = sparse.eye(H.shape[0], format="csr") - invsqrt @ A @ invsqrt

    def second(L) -> float:
        vals = np.sort(eigsh(L, k=3, which="SM", return_eigenvectors=False,
                             v0=np.ones(L.shape[0]), tol=1e-9, maxiter=100000))
        return float(vals[1])
    return second(Lu), second(Ln)


def rewire(H: sparse.csr_matrix, rng: random.Random, swaps_per_incidence: int = 3) -> sparse.csr_matrix:
    coo = H.tocoo()
    pairs = list(zip(map(int, coo.row), map(int, coo.col)))
    occupied = set(pairs)
    target = swaps_per_incidence * len(pairs)
    successes = attempts = 0
    cap = target * 20
    while successes < target and attempts < cap:
        attempts += 1
        i, j = rng.randrange(len(pairs)), rng.randrange(len(pairs))
        if i == j:
            continue
        v1, e1 = pairs[i]
        v2, e2 = pairs[j]
        if v1 == v2 or e1 == e2 or (v1, e2) in occupied or (v2, e1) in occupied:
            continue
        occupied.remove((v1, e1)); occupied.remove((v2, e2))
        occupied.add((v1, e2)); occupied.add((v2, e1))
        pairs[i], pairs[j] = (v1, e2), (v2, e1)
        successes += 1
    if successes < target:
        raise RuntimeError(f"rewiring mixed only {successes}/{target} requested swaps")
    rows, cols = zip(*pairs)
    out = sparse.csr_matrix((np.ones(len(pairs)), (rows, cols)), shape=H.shape)
    if not (np.array_equal(np.asarray(out.sum(1)), np.asarray(H.sum(1))) and
            np.array_equal(np.asarray(out.sum(0)), np.asarray(H.sum(0)))):
        raise AssertionError("configuration-model rewire changed a degree")
    return out


def canonical_write(path: Path, obj) -> str:
    data = (json.dumps(obj, sort_keys=True, separators=(",", ":"), ensure_ascii=False) + "\n").encode()
    path.write_bytes(data)
    return hashlib.sha256(data).hexdigest()


def main() -> None:
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", type=Path, default=Path(__file__).resolve().parents[3])
    ap.add_argument("--nulls", type=int, default=200)
    ap.add_argument("--out", type=Path, default=Path(__file__).resolve().parent)
    args = ap.parse_args()
    if args.nulls < 200:
        raise SystemExit("preregistration requires at least 200 null rewires")
    if git(args.repo, "rev-parse", PIN).strip() != PIN:
        raise SystemExit("pinned commit is unavailable")
    commits = load_commits(args.repo)
    if len(commits) != 1828:
        raise SystemExit(f"expected 1828 commits, got {len(commits)}")
    edges, controls = derive(commits)
    if controls["cochange"] is None:
        raise SystemExit("positive control failed: source/test co-change pair not surfaced")
    # Author names are metadata only.  The incidence derivation never reads them.
    alias_names = {c["author"] for c in commits} & JOE_ALIASES
    alias_control = {
        "aliases_present": sorted(alias_names),
        "pass": alias_names == JOE_ALIASES,
        "method": "all non-author incidence derivation ignores author identity",
    }
    if not alias_control["pass"]:
        raise SystemExit("alias positive control failed")
    corpus = {
        "schema": 1, "signal": SIGNAL, "pin": PIN,
        "parameters": {"cochange_min_count": COCHANGE_MIN_COUNT,
                       "cochange_min_jaccard": COCHANGE_MIN_JACCARD,
                       "subsystem_depth": SUBSYSTEM_DEPTH,
                       "temporal_window": TEMPORAL_WINDOW},
        "commits": commits, "edges": edges,
    }
    args.out.mkdir(parents=True, exist_ok=True)
    corpus_hash = canonical_write(args.out / "s1-corpus.json", corpus)
    H = incidence(len(commits), edges)
    real_u, real_n = lambda2_pair(H)
    rng = random.Random(SEED)
    null_u, null_n = [], []
    for i in range(args.nulls):
        hu, hn = lambda2_pair(rewire(H, rng))
        null_u.append(hu); null_n.append(hn)
        if (i + 1) % 10 == 0:
            print(f"null {i + 1}/{args.nulls}", flush=True)

    def summary(real: float, xs: list[float]):
        lo, hi = np.quantile(xs, [0.025, 0.975], method="linear")
        return {"real": real, "null_mean": float(np.mean(xs)),
                "null_sd": float(np.std(xs, ddof=1)), "null_95": [float(lo), float(hi)],
                "outside_95": bool(real < lo or real > hi)}
    kinds = Counter(e["kind"] for e in edges)
    result = {
        "schema": 1, "signal": SIGNAL, "pin": PIN, "seed": SEED,
        "versions": {"python": os.sys.version.split()[0], "numpy": np.__version__,
                     "scipy": scipy.__version__},
        "corpus_sha256": corpus_hash, "commits": len(commits),
        "hyperedges": len(edges), "incidences": int(H.nnz),
        "hyperedges_by_relation": dict(sorted(kinds.items())),
        "controls": {"cochange": controls, "alias": alias_control},
        "operators": {"unnormalized": summary(real_u, null_u),
                      "zhou_degree_normalized": summary(real_n, null_n)},
        "nulls": args.nulls,
        "verdict": "potentially-rehabilitable" if (
            summary(real_u, null_u)["outside_95"] or summary(real_n, null_n)["outside_95"]
        ) else "criterion-stays-retracted",
    }
    result_hash = canonical_write(args.out / "s1-results.json", result)
    print(json.dumps({"corpus_sha256": corpus_hash, "results_sha256": result_hash,
                      "operators": result["operators"], "controls": result["controls"],
                      "verdict": result["verdict"]}, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
