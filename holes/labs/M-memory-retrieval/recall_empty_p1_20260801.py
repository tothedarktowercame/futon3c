#!/usr/bin/env python3
"""Preregistered P1 analysis of dispatch-time recall receipts.

This program is deliberately read-only with respect to its EDN input and FTS
database.  The database argument must name a copied snapshot, never the live
writer's file.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import math
import random
import re
import sqlite3
import sys
from collections import Counter, defaultdict
from collections.abc import Mapping, Sequence
from pathlib import Path

import edn_format

SEED = 20260731
SHUFFLES = 10_000
ALPHA = 0.05
FAMILY_RE = re.compile(r"(?i)(a\d{2}[aj]\d{2})")


def key(name: str) -> edn_format.Keyword:
    return edn_format.Keyword(name)


def sha256(path: Path) -> str:
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def offered_rows(path: Path) -> list[Mapping]:
    document = edn_format.loads(path.read_text())
    rows = []
    for wrapper in document[key("entries")]:
        body = wrapper[key("evidence/body")]
        if body.get(key("phase")) == key("offered"):
            rows.append(body)
    return rows


def fts_match_string(term: str) -> str:
    """Mirror futon1b_text.clj's match-string for one term."""
    tokens = term.split()
    return " ".join(f'"{token.replace(chr(34), chr(34) * 2)}"' for token in tokens)


def document_frequencies(db_path: Path, terms: Sequence[str]) -> tuple[dict[str, int], int]:
    uri = f"file:{db_path}?mode=ro&immutable=1"
    with sqlite3.connect(uri, uri=True) as connection:
        indexed = connection.execute("SELECT count(*) FROM ev_fts").fetchone()[0]
        frequencies = {
            term: connection.execute(
                "SELECT count(*) FROM ev_fts WHERE ev_fts MATCH ?",
                (fts_match_string(term),),
            ).fetchone()[0]
            for term in sorted(set(terms))
        }
    return frequencies, indexed


def empty(row: Mapping) -> int:
    return int(row[key("recall-status")] == key("recall-empty"))


def query_terms(row: Mapping) -> list[str] | None:
    query = row.get(key("recall-query"))
    if not isinstance(query, Mapping):
        return None
    terms = query.get(key("terms"))
    return [str(term) for term in terms] if terms else None


def rate(values: Sequence[int]) -> float:
    return sum(values) / len(values)


def permutation_p(observed: float, statistics: Sequence[float]) -> float:
    return (1 + sum(value >= observed for value in statistics)) / (1 + len(statistics))


def h1_analysis(rows: list[Mapping], frequencies: Mapping[str, int], rng: random.Random) -> dict:
    observations = []
    for row in rows:
        terms = query_terms(row)
        if terms:
            observations.append(
                {
                    "job_id": row[key("job-id")],
                    "min_df": min(frequencies[term] for term in terms),
                    "empty": empty(row),
                }
            )
    ordered_df = sorted(item["min_df"] for item in observations)
    low_cut = ordered_df[(len(ordered_df) - 1) // 3]
    high_cut = ordered_df[(2 * len(ordered_df) - 1) // 3]
    buckets: dict[str, list[int]] = {"rare": [], "middle": [], "common": []}
    for item in observations:
        label = (
            "rare"
            if item["min_df"] <= low_cut
            else "middle"
            if item["min_df"] <= high_cut
            else "common"
        )
        buckets[label].append(item["empty"])
    rates = {name: rate(values) for name, values in buckets.items()}
    observed_difference = rates["rare"] - rates["common"]
    labels = [item["empty"] for item in observations]
    bucket_names = []
    for item in observations:
        bucket_names.append(
            "rare"
            if item["min_df"] <= low_cut
            else "middle"
            if item["min_df"] <= high_cut
            else "common"
        )
    null = []
    for _ in range(SHUFFLES):
        shuffled = labels.copy()
        rng.shuffle(shuffled)
        grouped: dict[str, list[int]] = defaultdict(list)
        for label, value in zip(bucket_names, shuffled, strict=True):
            grouped[label].append(value)
        null.append(rate(grouped["rare"]) - rate(grouped["common"]))
    monotone = rates["rare"] > rates["middle"] > rates["common"]
    return {
        "classification": "CONFIRMED" if monotone else "FALSIFIED",
        "analyzable_n": len(observations),
        "missing_query_n": len(rows) - len(observations),
        "bucket_rule": "tertiles of observed min-DF; ties remain in the lower-DF bucket",
        "cutpoints": {"rare_max_df": low_cut, "middle_max_df": high_cut},
        "buckets": {
            name: {"n": len(values), "empty_n": sum(values), "empty_rate": rate(values)}
            for name, values in buckets.items()
        },
        "rare_minus_common": observed_difference,
        "one_sided_permutation_p": permutation_p(observed_difference, null),
        "decision_rule": "CONFIRMED iff rare > middle > common empty rates",
    }


def h2_analysis(rows: list[Mapping]) -> dict:
    tier_fields = 0
    for row in rows:
        query = row.get(key("recall-query"))
        if isinstance(query, Mapping) and (
            key("recall/tier") in query or key("recall/query-used") in query
        ):
            tier_fields += 1
    surfaced_counts = []
    for row in rows:
        use = row.get(key("memory-use"), {})
        surfaced_counts.append(len(use.get(key("memory-use/surfaced-ids"), [])))
    return {
        "classification": "UNTESTABLE",
        "reason": (
            "The fired rung is absent from every offered receipt. The implementation "
            "associates recall/tier and recall/query-used with proposals but persists "
            "only the original query-data. Replaying against the current index would "
            "not recover the historical rung."
        ),
        "receipts_with_fired_rung": tier_fields,
        "receipts_n": len(rows),
        "surfaced_count_distribution": dict(sorted(Counter(surfaced_counts).items())),
        "mandatory_caption": (
            "Any use rate computed from used-ids would be a FLOOR: outcome used-ids "
            "is populated only about 16% of the time. No H2 use rate is reported "
            "because the comparison rung is not recorded."
        ),
    }


def family_statistic(families: Sequence[str], labels: Sequence[int]) -> float:
    overall = rate(labels)
    grouped: dict[str, list[int]] = defaultdict(list)
    for family, value in zip(families, labels, strict=True):
        grouped[family].append(value)
    return sum(len(values) * (rate(values) - overall) ** 2 for values in grouped.values())


def h3_analysis(rows: list[Mapping], rng: random.Random) -> dict:
    matched = []
    unmatched = []
    for row in rows:
        problem = str(row[key("problem")])
        match = FAMILY_RE.search(problem)
        if match:
            matched.append((match.group(1).lower(), empty(row)))
        else:
            unmatched.append(problem)
    families = [family for family, _ in matched]
    labels = [value for _, value in matched]
    observed = family_statistic(families, labels)
    null = []
    for _ in range(SHUFFLES):
        shuffled = labels.copy()
        rng.shuffle(shuffled)
        null.append(family_statistic(families, shuffled))
    p_value = permutation_p(observed, null)
    family_rows: dict[str, list[int]] = defaultdict(list)
    for family, value in matched:
        family_rows[family].append(value)
    return {
        "classification": "CONFIRMED" if p_value < ALPHA else "FALSIFIED",
        "matched_n": len(matched),
        "unmatched_n": len(unmatched),
        "family_count": len(family_rows),
        "statistic": observed,
        "permutation_p": p_value,
        "shuffles": SHUFFLES,
        "alpha": ALPHA,
        "family_rates": {
            family: {"n": len(values), "empty_n": sum(values), "empty_rate": rate(values)}
            for family, values in sorted(family_rows.items())
        },
        "unmatched_problems": sorted(unmatched),
    }


def analyze(input_path: Path, db_path: Path) -> dict:
    rows = offered_rows(input_path)
    all_terms = [term for row in rows for term in (query_terms(row) or [])]
    frequencies, indexed = document_frequencies(db_path, all_terms)
    h1_rng = random.Random(SEED + 1)
    h3_rng = random.Random(SEED + 3)
    return {
        "study": "P1 recall-empty mechanism and bias",
        "input": {
            "path": input_path.name,
            "sha256": sha256(input_path),
            "offered_n": len(rows),
        },
        "fts_snapshot": {
            "source_policy": "queried a copied snapshot, never the live writer database",
            "copy_filename": db_path.name,
            "indexed_rows_at_copy": indexed,
            "index_as_of_reported_by_service": "2026-07-31T04:44:43Z",
            "caveat": (
                "DF values are from the current-index copy made 2026-08-01, while "
                "dispatches are historical; they are not dispatch-time DF snapshots."
            ),
        },
        "runtime": {
            "python": sys.version.split()[0],
            "edn_format": getattr(edn_format, "__version__", "0.7.5"),
            "sqlite": sqlite3.sqlite_version,
            "seed": SEED,
        },
        "h1": h1_analysis(rows, frequencies, h1_rng),
        "h2": h2_analysis(rows),
        "h3": h3_analysis(rows, h3_rng),
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", required=True, type=Path)
    parser.add_argument("--db-copy", required=True, type=Path)
    parser.add_argument("--output", required=True, type=Path)
    args = parser.parse_args()
    result = analyze(args.input, args.db_copy)
    args.output.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n")


if __name__ == "__main__":
    main()
