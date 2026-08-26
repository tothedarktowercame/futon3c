#!/usr/bin/env python3
"""Score archived APM used-memory positions under four shelf orderings."""

import argparse
import json
from pathlib import Path
import statistics
import subprocess
import sys
import time

from fingerprint_audit import (LEAN_TOKEN, attempts, base_file, body_text,
                               fetch_memory)


HERE = Path(__file__).resolve().parent
REPO = HERE.parents[3]
CAMPAIGN_ID = "jit-all-open-nontopology-v1"
CAMPAIGN = REPO / "data" / "apm-campaigns" / CAMPAIGN_ID
DEFAULT_OUTPUT = HERE / "shelf-order-2026-08-26.json"
FRAME_NUMBERS = range(28, 43)
ORDERS = ("delivered", "same_problem", "identifier_overlap", "combined")


def read_edn_files(paths):
    """Parse each EDN file structurally in one Clojure process."""
    form = r'''(require '[clojure.edn :as edn] '[cheshire.core :as json])
(doseq [path (line-seq (java.io.BufferedReader. *in*))]
  (println (json/generate-string
            {:path path :data (edn/read-string (slurp path))})))'''
    result = subprocess.run(
        ["clojure", "-M", "-e", form], cwd=REPO,
        input="".join(f"{path}\n" for path in paths),
        capture_output=True, text=True, check=True,
    )
    parsed = {}
    for line in result.stdout.splitlines():
        item = json.loads(line)
        parsed[item["path"]] = item["data"]
    return parsed


def solver_frames():
    found = []
    for number in FRAME_NUMBERS:
        frame = f"f{number}"
        frame_dir = CAMPAIGN / f"{CAMPAIGN_ID}-{frame}"
        solver = frame_dir / "snapshots" / f"{frame}-solver-memory.edn"
        if solver.exists():
            found.append((frame, frame_dir, solver))
    return found


def distinct_identifiers(text):
    return set(LEAN_TOKEN.findall(text or ""))


def position(order, memory_id):
    try:
        return order.index(memory_id) + 1
    except ValueError:
        return "not-on-shelf"


def ordering_stats(rows, cross_problem=False):
    scope = [row for row in rows if (not cross_problem or row["cross_problem"])]
    result = {"rows": len(scope), "not_on_shelf": 0, "orderings": {}}
    for order in ORDERS:
        values = [row["positions"][order] for row in scope
                  if isinstance(row["positions"][order], int)]
        result["not_on_shelf"] = max(result["not_on_shelf"], len(scope) - len(values))
        result["orderings"][order] = {
            "count": len(values),
            "median": statistics.median(values) if values else None,
            "mean": (sum(values) / len(values)) if values else None,
            "top_5": sum(value <= 5 for value in values),
            "top_10": sum(value <= 10 for value in values),
        }
    return result


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--write", type=Path, default=DEFAULT_OUTPUT)
    args = parser.parse_args()
    started = time.monotonic()

    frames = solver_frames()
    receipt_meta = {(frame, number): (problem, revision, problem_path)
                    for frame, number, problem, _ids, _source, revision, problem_path
                    in attempts(str(CAMPAIGN))}
    receipt_paths = []
    snapshot_paths = []
    for frame, frame_dir, _solver in frames:
        receipt_paths.extend(sorted((frame_dir / "live").glob("student-attempt-[123].edn")))
        snapshot_paths.extend(sorted((frame_dir / "snapshots").glob("*-memory.edn")))
    documents = read_edn_files([str(path) for path in receipt_paths + snapshot_paths])

    frame_entries = {}
    snapshot_orders = {}
    for frame, _frame_dir, solver in frames:
        entries = {}
        for path in snapshot_paths:
            if path.parent.parent.name.endswith(f"-{frame}"):
                for entry in documents[str(path)].get("snapshot/memories", []):
                    entries[entry["memory-id"]] = entry
        frame_entries[frame] = entries
        snapshot_orders[frame] = [entry["memory-id"] for entry in
                                  documents[str(solver)].get("snapshot/memories", [])]

    rows = []
    fallback_rows = []
    shelf_comparisons = []
    missing_entry_ids = set()
    textless_entry_ids = set()
    for receipt_path in receipt_paths:
        frame = receipt_path.parent.parent.name.rsplit("-", 1)[-1]
        attempt_number = int(receipt_path.stem.rsplit("-", 1)[-1])
        receipt = documents[str(receipt_path)].get("receipt", {})
        memory_use = receipt.get("receipt/memory-use", {})
        used_ids = list(memory_use.get("used-ids", []))
        if not used_ids:
            continue
        problem = receipt.get("receipt/problem-id")
        delivered = memory_use.get("accessible-memory-ids")
        shelf_source = "accessible-memory-ids"
        if delivered is None:
            delivered = snapshot_orders[frame]
            shelf_source = "snapshot-fallback"
            fallback_rows.append({"frame": frame, "attempt": attempt_number})
        delivered = list(dict.fromkeys(delivered))
        solver_order = snapshot_orders[frame]
        shelf_comparisons.append({
            "frame": frame, "attempt": attempt_number, "source": shelf_source,
            "delivered_size": len(delivered), "solver_snapshot_size": len(solver_order),
            "matches_solver_snapshot": delivered == solver_order,
            "added_vs_solver": [mid for mid in delivered if mid not in solver_order],
            "missing_vs_solver": [mid for mid in solver_order if mid not in delivered],
        })
        _meta_problem, revision, problem_path = receipt_meta[(frame, attempt_number)]
        problem_text = base_file(revision, problem_path) or ""
        problem_identifiers = distinct_identifiers(problem_text)
        entries = frame_entries[frame]

        scores = {}
        same_problem = {}
        provenance = {}
        for memory_id in delivered:
            entry = entries.get(memory_id)
            if entry:
                text = "\n".join(str(entry.get(key, "")) for key in ("name", "hook", "body"))
                provenance_problem = (entry.get("provenance") or {}).get("problem-id")
                # 586 of 1,099 snapshot entries carry no name/hook/body (older
                # promotions are recorded by content-digest only); score them
                # from the evidence store rather than as zero (claude-19 review).
                if len(text.strip()) < 20:
                    textless_entry_ids.add(memory_id)
                    text = body_text(fetch_memory(memory_id))
            else:
                missing_entry_ids.add(memory_id)
                text = body_text(fetch_memory(memory_id))
                provenance_problem = None
            scores[memory_id] = len(distinct_identifiers(text) & problem_identifiers)
            provenance[memory_id] = provenance_problem
            same_problem[memory_id] = provenance_problem == problem

        same_order = sorted(delivered, key=lambda mid: (not same_problem[mid], mid))
        overlap_order = sorted(delivered, key=lambda mid: (-scores[mid], mid))
        combined_order = sorted(delivered,
                                key=lambda mid: (not same_problem[mid], -scores[mid], mid))
        orders = {"delivered": delivered, "same_problem": same_order,
                  "identifier_overlap": overlap_order, "combined": combined_order}

        for memory_id in used_ids:
            entry = entries.get(memory_id)
            provenance_problem = ((entry or {}).get("provenance") or {}).get("problem-id")
            if memory_id not in scores:
                missing_entry_ids.add(memory_id)
                raw = body_text(fetch_memory(memory_id))
                score = len(distinct_identifiers(raw) & problem_identifiers)
            else:
                score = scores[memory_id]
            rows.append({
                "frame": frame, "attempt": attempt_number, "problem": problem,
                "memory_id": memory_id, "memory_short": memory_id[:18],
                "provenance_problem": provenance_problem,
                "cross_problem": bool(provenance_problem and provenance_problem != problem),
                "shelf_source": shelf_source, "shelf_size": len(delivered),
                "positions": {name: position(order, memory_id)
                              for name, order in orders.items()},
                "identifier_overlap_score": score,
                "snapshot_entry_present": entry is not None,
            })

    overall = ordering_stats(rows)
    cross = ordering_stats(rows, cross_problem=True)
    comparison = {
        "combined_beats_delivered_mean":
        overall["orderings"]["combined"]["mean"] < overall["orderings"]["delivered"]["mean"],
        "combined_beats_delivered_median":
        overall["orderings"]["combined"]["median"] < overall["orderings"]["delivered"]["median"],
    }
    f42_rows = [row for row in rows if row["frame"] == "f42"]
    comparison["f42_rows"] = [{"attempt": row["attempt"],
                                "memory_id": row["memory_id"],
                                "delivered": row["positions"]["delivered"],
                                "identifier_overlap": row["positions"]["identifier_overlap"]}
                               for row in f42_rows]
    output = {
        "campaign": CAMPAIGN_ID,
        "frames_expected_by_handoff": 13,
        "frames_found": [frame for frame, _directory, _solver in frames],
        "frame_count": len(frames),
        "method": {
            "same_problem": "provenance problem equals frame problem; descending boolean",
            "identifier_overlap": "distinct LEAN_TOKEN matches shared by memory name+hook+body and base problem file; descending count",
            "combined": "same_problem, then identifier_overlap, then memory-id",
            "tie_break": "memory-id",
        },
        "rows": rows,
        "summary": {"all": overall, "cross_problem": cross},
        "comparison": comparison,
        "fallback_rows": fallback_rows,
        "shelf_comparisons": shelf_comparisons,
        "accessible_differs_from_solver_snapshot_count":
        sum(not item["matches_solver_snapshot"] for item in shelf_comparisons),
        "textless_snapshot_entry_count": len(textless_entry_ids),
        "missing_snapshot_entry_ids": sorted(missing_entry_ids),
        "wall_clock_seconds": time.monotonic() - started,
    }
    args.write.parent.mkdir(parents=True, exist_ok=True)
    args.write.write_text(json.dumps(output, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(output["summary"], indent=2))
    print(json.dumps(output["comparison"], indent=2))
    print(f"wrote {args.write}; {len(rows)} rows; {output['wall_clock_seconds']:.3f}s",
          file=sys.stderr)


if __name__ == "__main__":
    main()
