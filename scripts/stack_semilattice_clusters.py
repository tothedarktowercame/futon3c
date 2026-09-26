#!/usr/bin/env python3
"""Cluster missions and patterns, then join them with mission->pattern warrants.

This is a bounded exploratory derivation for E-pipeline-pipecleaner.  It keeps
mission and pattern spaces separate:

* missions use the existing BGE mission embeddings;
* patterns use local TF-IDF/SVD text embeddings, because no pattern BGE file was
  found in the current workspace;
* warrant links come from explicit :applied mission->pattern entries in
  futon6/data/mission-pattern-scopes.edn.
"""

from __future__ import annotations

import json
import math
import re
import subprocess
from collections import Counter, defaultdict
from datetime import datetime
from pathlib import Path

import numpy as np
from sklearn.cluster import AgglomerativeClustering
from sklearn.decomposition import TruncatedSVD
from sklearn.feature_extraction.text import TfidfVectorizer
from sklearn.feature_extraction.text import ENGLISH_STOP_WORDS
from sklearn.preprocessing import normalize


ROOT = Path("/home/joe/code")
MISSION_EMBEDDINGS = ROOT / "futon3a/resources/notions/bge_mission_embeddings.json"
MISSION_PATTERN_SCOPES = ROOT / "futon6/data/mission-pattern-scopes.edn"
PATTERN_ROOT = ROOT / "futon3/library"
OUT_EDN = ROOT / "futon3c/holes/excursions/pipeline-semilattice-clusters.edn"
OUT_MD = ROOT / "futon3c/holes/excursions/pipeline-semilattice-clusters.md"


TOKEN_RE = re.compile(r"[A-Za-z][A-Za-z0-9_-]{2,}")
DATE_RE = re.compile(r"\d{4}-\d{2}-\d{2}")
MISSION_REF_RE = re.compile(r"\bM-[A-Za-z0-9][A-Za-z0-9_-]*\b")
PREREQ_RE = re.compile(
    r"\b(depends?\s+on|prereq(?:uisite)?|requires?|blocked\s+by|needed|needs|"
    r"couldn'?t\s+exist\s+before|made\s+possible\s+by|built\s+on|builds\s+on)\b",
    re.I,
)
ELABORATION_RE = re.compile(
    r"\b(follow-?on|future|deferred|later|next|extends?|extension|elaborat(?:e|es|ion)|"
    r"companion|feeds?|enables?|unblocks?)\b",
    re.I,
)
WARRANT_RE = re.compile(
    r"\b(source|witness|evidence|attest(?:s|ed)?|warrant(?:s|ed)?|pattern|"
    r"cites?|cross-?ref(?:erence)?|see also|prior art|supersedes?)\b",
    re.I,
)
STOP_WORDS = sorted(
    ENGLISH_STOP_WORDS
    | {
        "archived",
        "complete",
        "current",
        "futon",
        "futon0",
        "futon1",
        "futon2",
        "futon3",
        "futon3c",
        "futon4",
        "futon5",
        "futon5a",
        "futon6",
        "futon7",
        "in-progress",
        "joe",
        "mission",
        "missions",
        "status",
        "unknown",
    }
)

GIT_ROOT_CACHE: dict[Path, Path | None] = {}


def clj_edn_to_json(path: Path) -> dict:
    form = (
        "(require '[clojure.edn :as edn] '[clojure.data.json :as json]) "
        f"(do (json/write (edn/read-string (slurp \"{path}\")) *out*) nil)"
    )
    proc = subprocess.run(
        ["clojure", "-M", "-e", form],
        check=True,
        capture_output=True,
        text=True,
        cwd=str(ROOT / "futon3c"),
    )
    return json.loads(proc.stdout)


def read_patterns() -> list[dict]:
    rows = []
    for path in sorted(PATTERN_ROOT.glob("**/*.flexiarg")):
        rel = path.relative_to(PATTERN_ROOT)
        pid = "/".join(rel.with_suffix("").parts)
        text = path.read_text(errors="replace")
        rows.append(
            {
                "id": pid,
                "slug": path.stem,
                "family": rel.parts[0],
                "path": str(path),
                "text": f"{pid}\n{text}",
            }
        )
    return rows


def read_missions() -> list[dict]:
    rows = json.loads(MISSION_EMBEDDINGS.read_text())
    usable = []
    for row in rows:
        vector = row.get("vector")
        basename = row.get("basename")
        if basename and isinstance(vector, list) and vector:
            usable.append(row)
    return usable


def git_root(path: Path) -> Path | None:
    for parent in [path.parent, *path.parents]:
        if parent in GIT_ROOT_CACHE:
            return GIT_ROOT_CACHE[parent]
        if (parent / ".git").exists():
            GIT_ROOT_CACHE[parent] = parent
            return parent
    return None


def git_first_commit_date(path: Path) -> str | None:
    root = git_root(path)
    if root is None:
        return None
    try:
        rel = path.relative_to(root)
    except ValueError:
        return None
    proc = subprocess.run(
        ["git", "-C", str(root), "log", "--follow", "--format=%cs", "--", str(rel)],
        check=False,
        capture_output=True,
        text=True,
    )
    dates = [line.strip() for line in proc.stdout.splitlines() if DATE_RE.fullmatch(line.strip())]
    return dates[-1] if dates else None


def piano_roll_anchor(path: str) -> tuple[str | None, str | None]:
    """Return a coarse mission date plus its provenance."""
    p = Path(path)
    if not p.exists():
        return None, None
    text = p.read_text(errors="replace")
    status_stamps = re.findall(r"Status:.*?\((\d{4}-\d{2}-\d{2})\)", text)
    if status_stamps:
        return status_stamps[-1], "status-stamp"
    date_m = re.search(r"\*\*Date:\*\*\s*(\d{4}-\d{2}-\d{2})", text)
    if date_m:
        return date_m.group(1), "date-field"
    any_date = DATE_RE.search(text[:2000])
    if any_date:
        return any_date.group(0), "early-text-date"
    git_date = git_first_commit_date(p)
    if git_date:
        return git_date, "git-first-commit"
    return None, None


def temporal_stats(missions: list[dict], idxs: list[int]) -> dict:
    dated = []
    for i in idxs:
        d = missions[i].get("piano-roll-anchor")
        if d:
            dated.append(d)
    dated.sort()
    if not dated:
        return {"dated": 0, "first": None, "median": None, "last": None}
    return {
        "dated": len(dated),
        "first": dated[0],
        "median": dated[len(dated) // 2],
        "last": dated[-1],
    }


def cluster_vectors(vectors: np.ndarray, coarse_k: int, sub_k_max: int) -> tuple[list[int], dict[int, list[int]]]:
    coarse = AgglomerativeClustering(n_clusters=coarse_k, metric="cosine", linkage="average")
    coarse_labels = coarse.fit_predict(vectors)
    sublabels: dict[int, list[int]] = {}
    for c in sorted(set(coarse_labels)):
        idxs = [i for i, label in enumerate(coarse_labels) if label == c]
        if len(idxs) < 5:
            for idx in idxs:
                sublabels[idx] = [int(c), 0]
            continue
        k = min(sub_k_max, max(2, int(round(math.sqrt(len(idxs))))))
        sub = AgglomerativeClustering(n_clusters=k, metric="cosine", linkage="average")
        local = sub.fit_predict(vectors[idxs])
        for idx, s in zip(idxs, local):
            sublabels[idx] = [int(c), int(s)]
    return [int(x) for x in coarse_labels], sublabels


def top_terms(texts: list[str], labels: list[int], n: int = 8) -> dict[int, list[str]]:
    vec = TfidfVectorizer(
        tokenizer=lambda s: TOKEN_RE.findall(s.lower()),
        token_pattern=None,
        stop_words=STOP_WORDS,
        min_df=2,
        max_df=0.65,
        max_features=5000,
    )
    mat = vec.fit_transform(texts)
    terms = np.array(vec.get_feature_names_out())
    out = {}
    for label in sorted(set(labels)):
        idxs = [i for i, x in enumerate(labels) if x == label]
        centroid = np.asarray(mat[idxs].mean(axis=0)).ravel()
        top = centroid.argsort()[-n:][::-1]
        out[int(label)] = [str(terms[i]) for i in top if centroid[i] > 0]
    return out


def pattern_vectors(patterns: list[dict]) -> np.ndarray:
    vec = TfidfVectorizer(
        tokenizer=lambda s: TOKEN_RE.findall(s.lower()),
        token_pattern=None,
        stop_words=STOP_WORDS,
        min_df=2,
        max_df=0.75,
        max_features=12000,
    )
    mat = vec.fit_transform([p["text"] for p in patterns])
    n_components = min(96, mat.shape[0] - 1, mat.shape[1] - 1)
    svd = TruncatedSVD(n_components=n_components, random_state=17)
    dense = svd.fit_transform(mat)
    return normalize(dense)


def cluster_summary(rows: list[dict], labels: list[int], terms: dict[int, list[str]], kind: str) -> list[dict]:
    by_label: dict[int, list[int]] = defaultdict(list)
    for i, label in enumerate(labels):
        by_label[label].append(i)
    result = []
    for label, idxs in sorted(by_label.items()):
        if kind == "mission":
            samples = [rows[i]["basename"] for i in idxs[:10]]
            statuses = Counter(str(rows[i].get("status", "unknown")) for i in idxs)
            homes = Counter(str(rows[i].get("home_repo", "?")) for i in idxs)
            extra = {
                "top-statuses": statuses.most_common(5),
                "top-home-repos": homes.most_common(5),
                "temporal": temporal_stats(rows, idxs),
            }
        else:
            samples = [rows[i]["id"] for i in idxs[:10]]
            families = Counter(rows[i]["family"] for i in idxs)
            extra = {"top-families": families.most_common(8)}
        result.append(
            {
                "cluster": int(label),
                "size": len(idxs),
                "terms": terms.get(label, []),
                "sample": samples,
                **extra,
            }
        )
    return result


def hierarchy_summary(rows: list[dict], labels: list[int], sublabels: dict[int, list[int]], kind: str) -> list[dict]:
    grouped: dict[int, dict[int, list[int]]] = defaultdict(lambda: defaultdict(list))
    for i, label in enumerate(labels):
        grouped[int(label)][int(sublabels[i][1])].append(i)
    out = []
    for label in sorted(grouped):
        children = []
        for sub, idxs in sorted(grouped[label].items(), key=lambda item: (-len(item[1]), item[0])):
            if kind == "mission":
                sample = [rows[i]["basename"] for i in idxs[:8]]
            else:
                sample = [rows[i]["id"] for i in idxs[:8]]
            child = {"subcluster": int(sub), "size": len(idxs), "sample": sample}
            if kind == "mission":
                child["temporal"] = temporal_stats(rows, idxs)
            children.append(child)
        out.append({"cluster": int(label), "children": children})
    return out


def temporal_levels(missions: list[dict], labels: list[int]) -> list[dict]:
    by_label: dict[int, list[int]] = defaultdict(list)
    for i, label in enumerate(labels):
        by_label[int(label)].append(i)
    rows = []
    for label, idxs in by_label.items():
        stats = temporal_stats(missions, idxs)
        rows.append({"cluster": int(label), **stats})
    rows.sort(key=lambda row: row["median"] or "9999-99-99")
    for level, row in enumerate(rows):
        row["temporal-level"] = level
    return rows


def anchor_source_counts(missions: list[dict]) -> dict:
    counts = Counter(m.get("piano-roll-anchor-source") or "missing" for m in missions)
    return dict(sorted(counts.items()))


def local_ref_context(text: str, start: int, end: int) -> str:
    left = max(text.rfind("\n", 0, start), text.rfind(".", 0, start), text.rfind(";", 0, start))
    right_candidates = [p for p in (text.find("\n", end), text.find(".", end), text.find(";", end)) if p != -1]
    right = min(right_candidates) if right_candidates else min(len(text), end + 160)
    return " ".join(text[left + 1 : right].split())


def classify_ref_context(context: str, target_name: str) -> str:
    target_pos = context.find(target_name)
    before = context[:target_pos] if target_pos >= 0 else context
    after = context[target_pos:] if target_pos >= 0 else ""
    if PREREQ_RE.search(before[-120:]) or re.search(
        rf"\b{re.escape(target_name)}\b.{0,80}\b(provides?|made|enabled|unblocked|supplied)\b",
        after,
        re.I,
    ):
        return "completion-prerequisite"
    if ELABORATION_RE.search(context):
        return "elaboration"
    if WARRANT_RE.search(context):
        return "retrospective-warrant"
    return "cross-reference"


def temporal_relation(source_date: str | None, target_date: str | None) -> str:
    if not source_date or not target_date:
        return "undated"
    if target_date > source_date:
        return "target-later"
    if target_date < source_date:
        return "target-earlier"
    return "same-day"


def temporal_edge_audit(missions: list[dict], labels: list[int]) -> dict:
    by_name = {m["basename"]: i for i, m in enumerate(missions)}
    edges = []
    edge_counts: Counter[tuple[str, str]] = Counter()
    verdicts = Counter()
    kind_counts = Counter()

    for source_idx, source in enumerate(missions):
        path = Path(source.get("path", ""))
        if not path.exists():
            continue
        text = path.read_text(errors="replace")
        seen_in_source = set()
        for match in MISSION_REF_RE.finditer(text):
            target_name = match.group(0)
            target_idx = by_name.get(target_name)
            if target_idx is None or target_idx == source_idx:
                continue
            dedupe_key = (source["basename"], target_name)
            if dedupe_key in seen_in_source:
                continue
            seen_in_source.add(dedupe_key)
            context = local_ref_context(text, match.start(), match.end())
            kind = classify_ref_context(context, target_name)
            relation = temporal_relation(
                source.get("piano-roll-anchor"),
                missions[target_idx].get("piano-roll-anchor"),
            )
            verdict = "ok"
            if relation == "undated":
                verdict = "needs-dates"
            elif kind == "completion-prerequisite" and relation == "target-later":
                verdict = "suspicious-forward-prerequisite"
            elif kind != "completion-prerequisite" and relation == "target-later":
                verdict = "ok-as-elaboration-or-reference"

            edge = {
                "source-mission": source["basename"],
                "source-cluster": int(labels[source_idx]),
                "source-date": source.get("piano-roll-anchor"),
                "source-date-source": source.get("piano-roll-anchor-source"),
                "target-mission": target_name,
                "target-cluster": int(labels[target_idx]),
                "target-date": missions[target_idx].get("piano-roll-anchor"),
                "target-date-source": missions[target_idx].get("piano-roll-anchor-source"),
                "edge-kind": kind,
                "temporal-relation": relation,
                "verdict": verdict,
                "context": context[:260],
            }
            edges.append(edge)
            edge_counts[(edge["source-cluster"], edge["target-cluster"])] += 1
            verdicts[verdict] += 1
            kind_counts[kind] += 1

    suspicious = [e for e in edges if e["verdict"] == "suspicious-forward-prerequisite"]
    needs_dates = [e for e in edges if e["verdict"] == "needs-dates"]
    cluster_edges = [
        {"source-cluster": int(s), "target-cluster": int(t), "weight": int(w)}
        for (s, t), w in edge_counts.most_common(80)
    ]
    return {
        "summary": {
            "edges": len(edges),
            "edge-kinds": dict(sorted(kind_counts.items())),
            "verdicts": dict(sorted(verdicts.items())),
            "suspicious-forward-prerequisites": len(suspicious),
            "needs-dates": len(needs_dates),
        },
        "cluster-edges": cluster_edges,
        "suspicious-forward-prerequisites": suspicious[:80],
        "needs-dates-sample": needs_dates[:40],
        "edges-sample": edges[:120],
        "method": {
            "source": "explicit M-* references in mission markdown files",
            "classification": "regex over local reference context; conservative data-quality signal, not canonical dependency extraction",
            "suspicious-rule": "edge-kind completion-prerequisite and target piano-roll anchor later than source anchor",
        },
    }


def build_warrant_links(missions: list[dict], mission_labels: list[int], patterns: list[dict], pattern_labels: list[int]) -> dict:
    scopes = clj_edn_to_json(MISSION_PATTERN_SCOPES)
    slug_to_pattern_idxs: dict[str, list[int]] = defaultdict(list)
    for i, pattern in enumerate(patterns):
        slug_to_pattern_idxs[pattern["slug"]].append(i)
    mission_label_by_basename = {m["basename"]: mission_labels[i] for i, m in enumerate(missions)}
    mission_idx_by_basename = {m["basename"]: i for i, m in enumerate(missions)}

    cluster_links = Counter()
    unresolved = Counter()
    mission_links = []
    direct_cluster_patterns: dict[int, dict[str, set[str]]] = defaultdict(lambda: defaultdict(set))
    direct_cluster_pattern_counts: Counter[tuple[int, str]] = Counter()
    mission_pattern_cluster_counts: dict[str, Counter[int]] = defaultdict(Counter)
    mission_patterns_by_pattern_cluster: dict[str, dict[int, set[str]]] = defaultdict(lambda: defaultdict(set))
    for row in scopes.get("missions", []):
        mission = row.get("mission")
        if mission not in mission_label_by_basename:
            continue
        mcluster = mission_label_by_basename[mission]
        applied = row.get("applied", []) or []
        resolved = []
        for slug in applied:
            idxs = slug_to_pattern_idxs.get(slug, [])
            if not idxs:
                unresolved[slug] += 1
                continue
            for pidx in idxs:
                pattern_id = patterns[pidx]["id"]
                pcluster = pattern_labels[pidx]
                cluster_links[(mcluster, pcluster)] += 1
                mission_pattern_cluster_counts[mission][int(pcluster)] += 1
                mission_patterns_by_pattern_cluster[mission][int(pcluster)].add(pattern_id)
                direct_cluster_patterns[int(mcluster)][pattern_id].add(mission)
                direct_cluster_pattern_counts[(int(mcluster), pattern_id)] += 1
                resolved.append({"pattern": pattern_id, "pattern-cluster": int(pcluster)})
        if resolved:
            mission_links.append(
                {
                    "mission": mission,
                    "mission-cluster": int(mcluster),
                    "patterns": resolved[:30],
                    "n-patterns": len(resolved),
                    "mission-title": missions[mission_idx_by_basename[mission]].get("title", ""),
                }
            )

    top_links = [
        {"mission-cluster": int(m), "pattern-cluster": int(p), "weight": int(w)}
        for (m, p), w in cluster_links.most_common(80)
    ]
    by_mission_cluster = []
    for mcluster in sorted(direct_cluster_patterns):
        pattern_rows = []
        for pattern_id, citing_missions in direct_cluster_patterns[mcluster].items():
            pattern_rows.append(
                {
                    "pattern": pattern_id,
                    "count": int(direct_cluster_pattern_counts[(mcluster, pattern_id)]),
                    "cited-by": sorted(citing_missions)[:20],
                }
            )
        pattern_rows.sort(key=lambda row: (-row["count"], row["pattern"]))
        by_mission_cluster.append(
            {
                "mission-cluster": int(mcluster),
                "n-patterns": len(pattern_rows),
                "n-citing-missions": len({m for row in pattern_rows for m in row["cited-by"]}),
                "top-patterns": pattern_rows[:30],
            }
        )

    dominant_breakdown: dict[int, dict[int, list[dict]]] = defaultdict(lambda: defaultdict(list))
    for mission, counts in mission_pattern_cluster_counts.items():
        if not counts:
            continue
        mcluster = int(mission_label_by_basename[mission])
        top_count = max(counts.values())
        dominant_pclusters = [p for p, c in counts.items() if c == top_count]
        dominant_pcluster = sorted(dominant_pclusters)[0]
        dominant_breakdown[mcluster][dominant_pcluster].append(
            {
                "mission": mission,
                "mission-title": missions[mission_idx_by_basename[mission]].get("title", ""),
                "dominant-pattern-cluster": int(dominant_pcluster),
                "dominant-count": int(top_count),
                "total-cited-patterns": int(sum(counts.values())),
                "patterns": sorted(mission_patterns_by_pattern_cluster[mission][dominant_pcluster])[:12],
            }
        )

    dominant_rows = []
    for mcluster in sorted(dominant_breakdown):
        groups = []
        for pcluster, mission_rows in dominant_breakdown[mcluster].items():
            pattern_counter = Counter()
            for mission_row in mission_rows:
                pattern_counter.update(mission_row["patterns"])
            groups.append(
                {
                    "refined-basin": f"M{mcluster}P{pcluster}",
                    "pattern-cluster": int(pcluster),
                    "n-missions": len(mission_rows),
                    "missions": sorted(
                        mission_rows,
                        key=lambda row: (-row["dominant-count"], -row["total-cited-patterns"], row["mission"]),
                    )[:20],
                    "top-patterns": [
                        {"pattern": pattern, "count": int(count)}
                        for pattern, count in pattern_counter.most_common(12)
                    ],
                }
            )
        groups.sort(key=lambda row: (-row["n-missions"], row["pattern-cluster"]))
        dominant_rows.append({"mission-cluster": int(mcluster), "groups": groups})
    return {
        "cluster-links": top_links,
        "by-mission-cluster": by_mission_cluster,
        "dominant-pattern-cluster-breakdown": dominant_rows,
        "mission-links": sorted(mission_links, key=lambda x: (-x["n-patterns"], x["mission"]))[:80],
        "unresolved-pattern-slugs": unresolved.most_common(50),
        "n-applied-cluster-links": int(sum(cluster_links.values())),
    }


def edn_str(x, indent: int = 0) -> str:
    sp = " " * indent
    if isinstance(x, dict):
        parts = []
        for k, v in x.items():
            key = ":" + k if isinstance(k, str) and re.match(r"^[A-Za-z0-9_.?/-]+$", k) else edn_str(k)
            parts.append(f"{sp} {key} {edn_str(v, indent + 1)}")
        return "{\n" + "\n".join(parts) + f"\n{sp}}}"
    if isinstance(x, list) or isinstance(x, tuple):
        if not x:
            return "[]"
        return "[\n" + "\n".join(f"{sp} {edn_str(v, indent + 1)}" for v in x) + f"\n{sp}]"
    if isinstance(x, bool):
        return "true" if x else "false"
    if x is None:
        return "nil"
    if isinstance(x, (int, float)):
        return str(x)
    s = str(x)
    return json.dumps(s)


def write_markdown(data: dict) -> None:
    lines = [
        "# Pipeline Semilattice Clusters",
        "",
        "Derived for `E-pipeline-pipecleaner`: mission clusters, pattern clusters, and explicit mission->pattern warrant links.",
        "",
        "## Sources",
        "",
        f"- Missions: `{MISSION_EMBEDDINGS}` ({data['counts']['missions']} embedded missions, BGE).",
        f"- Patterns: `{PATTERN_ROOT}` ({data['counts']['patterns']} flexiarg files, TF-IDF/SVD fallback embeddings).",
        f"- Warrant links: `{MISSION_PATTERN_SCOPES}` ({data['counts']['applied-warrant-links']} resolved applied links).",
        "",
        f"Temporal anchor sources: `{data['temporal-anchor-sources']}`.",
        "",
        "## Mission Clusters",
        "",
    ]
    mission_children = {h["cluster"]: h["children"] for h in data["mission-hierarchy"]}
    pattern_children = {h["cluster"]: h["children"] for h in data["pattern-hierarchy"]}

    for c in data["mission-clusters"]:
        lines.append(
            f"- M{c['cluster']} size={c['size']} terms={', '.join(c['terms'][:6])}; sample={', '.join(c['sample'][:5])}"
        )
        for child in mission_children.get(c["cluster"], [])[:4]:
            lines.append(
                f"  - M{c['cluster']}.{child['subcluster']} size={child['size']}; sample={', '.join(child['sample'][:4])}"
            )
    lines += ["", "## Pattern Clusters", ""]
    for c in data["pattern-clusters"]:
        lines.append(
            f"- P{c['cluster']} size={c['size']} terms={', '.join(c['terms'][:6])}; sample={', '.join(c['sample'][:5])}"
        )
        for child in pattern_children.get(c["cluster"], [])[:4]:
            lines.append(
                f"  - P{c['cluster']}.{child['subcluster']} size={child['size']}; sample={', '.join(child['sample'][:4])}"
            )
    lines += ["", "## Strongest Cluster-Level Warrant Links", ""]
    for link in data["warrant-links"]["cluster-links"][:25]:
        lines.append(
            f"- M{link['mission-cluster']} -> P{link['pattern-cluster']} weight={link['weight']}"
        )
    lines += [
        "",
        "## Direct Pattern Warrants By Mission Cluster",
        "",
        "For the upward cascade, this section is stronger than pattern-cluster proximity: it lists the concrete `futon3/library` patterns actually cited by missions inside each mission cluster.",
        "",
    ]
    mission_cluster_meta = {c["cluster"]: c for c in data["mission-clusters"]}
    for row in data["warrant-links"]["by-mission-cluster"]:
        meta = mission_cluster_meta.get(row["mission-cluster"], {})
        terms = ", ".join(meta.get("terms", [])[:5])
        lines.append(
            f"- M{row['mission-cluster']} terms={terms}; cited-patterns={row['n-patterns']}; citing-missions={row['n-citing-missions']}"
        )
        for pattern in row["top-patterns"][:5]:
            cited_by = ", ".join(pattern["cited-by"][:4])
            lines.append(
                f"  - `{pattern['pattern']}` count={pattern['count']}; cited-by={cited_by}"
            )
    dominant_by_cluster = {
        row["mission-cluster"]: row["groups"]
        for row in data["warrant-links"]["dominant-pattern-cluster-breakdown"]
    }
    if 0 in dominant_by_cluster:
        lines += [
            "",
            "## M0 Breakdown By Dominant Cited Pattern Cluster",
            "",
            "M0 is too broad to read as one feature. This breakdown refactors M0 into explicit refined basins of the form `M0P<n>`, assigning each citing M0 mission to the pattern cluster it cites most often. Pattern clusters are secondary vocabulary labels here, not the primary upward edge.",
            "",
        ]
        for group in dominant_by_cluster[0][:10]:
            mission_names = ", ".join(row["mission"] for row in group["missions"][:6])
            top_patterns = ", ".join(f"`{p['pattern']}`" for p in group["top-patterns"][:4])
            lines.append(
                f"- {group['refined-basin']}: missions={group['n-missions']}; examples={mission_names}"
            )
            if top_patterns:
                lines.append(f"  - top dominant patterns: {top_patterns}")
    lines += [
        "",
        "## Temporal Levels",
        "",
        "Temporal anchors use the same coarse mission date rule as `piano_roll.py`: latest dated status stamp, falling back to `**Date:**`.",
        "If a mission has no in-document date, the audit falls back to the mission file's first Git commit date and records that provenance as `:git-first-commit`.",
        "",
    ]
    for row in data["temporal-levels"]:
        lines.append(
            f"- L{row['temporal-level']} M{row['cluster']}: median={row['median']} first={row['first']} last={row['last']} dated={row['dated']}"
        )
    audit = data["temporal-edge-audit"]
    lines += [
        "",
        "## Temporal Edge Audit",
        "",
        "This audit reads explicit `M-*` references in mission docs as candidate mission->mission edges, classifies the local wording, and flags only the strongest temporal problem: a completion-prerequisite edge whose target mission is later than the source mission in piano-roll time.",
        "",
        f"- Candidate mission-reference edges: `{audit['summary']['edges']}`.",
        f"- Edge kinds: `{audit['summary']['edge-kinds']}`.",
        f"- Verdicts: `{audit['summary']['verdicts']}`.",
        f"- Suspicious forward prerequisites: `{audit['summary']['suspicious-forward-prerequisites']}`.",
        "",
    ]
    for edge in audit["suspicious-forward-prerequisites"][:12]:
        lines.append(
            f"- `{edge['source-mission']}` ({edge['source-date']}) -> `{edge['target-mission']}` ({edge['target-date']}), context: {edge['context']}"
        )
    lines += [
        "",
        "## Reading For M-stack-stereolithography",
        "",
        "Mission clusters are candidate feature basins. Pattern clusters are warrant constellations. The cluster-level links are not mere proximity: they are explicit applied-pattern citations, so they can be read as a first approximation to `warranted-by` edges in a capability semilattice.",
        "",
        "Temporal levels should be read as a completion-prerequisite sanity check: an earlier mission can be fully elaborated by a later mission, but it is suspicious for the earlier mission's completion to require a later mission as a prerequisite.",
        "",
        "The hierarchy is intentionally shallow in this first pass: each side has coarse clusters plus within-cluster subclusters recorded in the EDN membership rows. This is enough to compare with `M-capability-star-map.graph.edn` without pretending the clusters already are the canonical star map.",
    ]
    OUT_MD.write_text("\n".join(lines) + "\n")


def main() -> None:
    missions = read_missions()
    for mission in missions:
        anchor, source = piano_roll_anchor(mission.get("path", ""))
        mission["piano-roll-anchor"] = anchor
        mission["piano-roll-anchor-source"] = source
    patterns = read_patterns()

    mission_vecs = normalize(np.array([m["vector"] for m in missions], dtype=np.float32))
    pattern_vecs = pattern_vectors(patterns)

    mission_k = 12
    pattern_k = 18
    mission_labels, mission_sub = cluster_vectors(mission_vecs, mission_k, 4)
    pattern_labels, pattern_sub = cluster_vectors(pattern_vecs, pattern_k, 5)

    mission_texts = [
        " ".join(str(m.get(k) or "") for k in ("basename", "title", "summary"))
        for m in missions
    ]
    mission_terms = top_terms(mission_texts, mission_labels)
    pattern_terms = top_terms([p["text"] for p in patterns], pattern_labels)
    warrants = build_warrant_links(missions, mission_labels, patterns, pattern_labels)
    edge_audit = temporal_edge_audit(missions, mission_labels)

    data = {
        "source": "pipeline-semilattice-clusters",
        "created": "2026-06-16",
        "method": {
            "missions": "existing BGE mission embeddings clustered with agglomerative cosine/average linkage",
            "patterns": "TF-IDF over flexiarg text -> TruncatedSVD -> agglomerative cosine/average linkage",
            "warrants": "explicit :applied mission->pattern entries from mission-pattern-scopes.edn",
        },
        "counts": {
            "missions": len(missions),
            "patterns": len(patterns),
            "mission-clusters": mission_k,
            "pattern-clusters": pattern_k,
            "applied-warrant-links": warrants["n-applied-cluster-links"],
        },
        "mission-clusters": cluster_summary(missions, mission_labels, mission_terms, "mission"),
        "pattern-clusters": cluster_summary(patterns, pattern_labels, pattern_terms, "pattern"),
        "mission-hierarchy": hierarchy_summary(missions, mission_labels, mission_sub, "mission"),
        "pattern-hierarchy": hierarchy_summary(patterns, pattern_labels, pattern_sub, "pattern"),
        "temporal-anchor-sources": anchor_source_counts(missions),
        "temporal-levels": temporal_levels(missions, mission_labels),
        "temporal-edge-audit": edge_audit,
        "mission-membership": [
            {
                "mission": m["basename"],
                "cluster": int(mission_labels[i]),
                "subcluster": int(mission_sub[i][1]),
                "title": m.get("title", ""),
                "status": m.get("status", "unknown"),
                "piano-roll-anchor": m.get("piano-roll-anchor"),
                "piano-roll-anchor-source": m.get("piano-roll-anchor-source"),
                "path": m.get("path", ""),
            }
            for i, m in enumerate(missions)
        ],
        "pattern-membership": [
            {
                "pattern": p["id"],
                "cluster": int(pattern_labels[i]),
                "subcluster": int(pattern_sub[i][1]),
                "path": p["path"],
            }
            for i, p in enumerate(patterns)
        ],
        "warrant-links": warrants,
    }
    OUT_EDN.write_text(edn_str(data) + "\n")
    write_markdown(data)
    print(f"wrote {OUT_EDN}")
    print(f"wrote {OUT_MD}")
    print(json.dumps(data["counts"], indent=2))


if __name__ == "__main__":
    main()
