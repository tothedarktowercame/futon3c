#!/usr/bin/env python3
"""Score sidecar answer incidences against the fixed 24-row labels."""
import argparse
import json
from pathlib import Path


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--moves", required=True, type=Path)
    parser.add_argument("--labels", required=True, type=Path)
    args = parser.parse_args()
    labels = json.loads(args.labels.read_text(encoding="utf-8"))
    incidences = set()
    for path in args.moves.rglob("*.moves.json"):
        doc = json.loads(path.read_text(encoding="utf-8"))
        for act in doc.get("acts", []):
            for answer in act.get("answers", []):
                incidences.add((doc.get("record"), answer.get("act")))
    matched = [row for row in labels if row.get("act")]
    predicted = [row for row in matched if (row["record"], row["act"]) in incidences]
    positives = {"answered", "done", "done-later"}
    evaluable = [row for row in predicted
                 if row["label"] in positives | {"open", "partly-open"}]
    true_positive = sum(row["label"] in positives for row in evaluable)
    precision = true_positive / len(evaluable) if evaluable else 0.0
    closed = sum(row["label"] in positives for row in predicted)
    print(json.dumps({"labelled": len(labels), "matchable": len(matched),
                      "incidence": len(predicted), "closed": closed,
                      "predicted_open": sum(row["label"] in {"open", "partly-open"}
                                            for row in predicted),
                      "precision_answered_vs_open": precision,
                      "unmatchable": [row.get("row") for row in labels if not row.get("act")]},
                     indent=2))


if __name__ == "__main__":
    main()
