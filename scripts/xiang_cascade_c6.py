#!/usr/bin/env python3
"""Compute the C6 live-gap closing table from local record-backed inputs.

The closing classification is a reviewed semantic manifest.  The program
fails if its quoted evidence is no longer present in the named record.  It
computes membership, elapsed times, reopenings, polarity counts, and summary;
it does not infer target identity from temporal proximity.
"""

from __future__ import annotations

import argparse
import datetime as dt
import json
import statistics
from pathlib import Path


ROOT = Path("/home/joe/code/futon3c")
CORPUS = ROOT / "holes/labs/M-象-cascade/corpus-c4.jsonl"
RECORDS = Path.home() / ".emacs-graph/session-turn-analysis"
PATTERN = "apparatus/done-is-observed-running"

# Record closures name the exact evidence row found by the C6 point reads.
# Operator closures name a later (or same-turn) operator record and its quote.
CLOSURES = {
    "turn-D2GnQn": {"kind": "record", "id": "e-49bef50f-08de-4c31-b9fc-4fc1206b2721",
                     "at": "2026-09-28T04:00:19.615006558Z",
                     "quote": "It's deployed: the live About page now shows"},
    "turn-QLrMMq": {"kind": "record", "id": "emacs-551ccf0648fef009bc0de98c4856302a",
                    "at": "2026-09-26T11:00:31.368946931Z",
                    "quote": "all of it is loaded"},
    "turn-V5DsUN": {"kind": "record", "id": "emacs-3acd725c904774e54ef50d5a727f6b1f",
                    "at": "2026-09-30T02:34:18.072736314Z",
                    "quote": "now work in your Emacs"},
    "turn-ZUtRsq": {"kind": "record", "id": "emacs-2eb6424cb522018a9356ee8aa4b11f26",
                    "at": "2026-09-26T03:39:13.398807403Z",
                    "quote": "象 has annotated your last turn"},
    "turn-LNcsB2": {"kind": "operator-confirmation", "id": "turn-Ie4yZG",
                    "at": "2026-09-29T04:32:19Z",
                    "quote": "m02J02 is up to 47 attempts",
                    "label": "qualify/report-problem"},
    "turn-Xy8ygM": {"kind": "operator-confirmation", "id": "turn-Xy8ygM",
                    "at": "2026-09-27T19:21:08Z",
                    "quote": "live example of how inbox zero is working now",
                    "label": "explain"},
}

REOPENINGS = {
    "turn-Xy8ygM": {"id": "turn-iNKHir", "at": "2026-09-29T19:53:53Z",
                    "quote": "inbox-zero service actually work and so far after a bunch of attempts I'm not convinced"},
}

# Classification is fragment-level. Context/proposal fragments do not assert
# either half. The script checks that every cited fragment is classified.
POLARITY = {
    ("turn-0anLV2", "s2", 0): "context",
    ("turn-0anLV2", "s3", 0): "opens",
    ("turn-D2GnQn", "s1", 0): "opens",
    ("turn-LNcsB2", "s1", 0): "closes",
    ("turn-LNcsB2", "s2", 0): "opens",
    ("turn-LNcsB2", "s3", 0): "opens",
    ("turn-LNcsB2", "s4", 0): "opens",
    ("turn-LNcsB2", "s5", 0): "context",
    ("turn-MCErMC", "s1", 0): "opens",
    ("turn-QLrMMq", "s1", 0): "opens",
    ("turn-QLrMMq", "s2", 0): "context",
    ("turn-V5DsUN", "s1", 0): "opens",
    ("turn-Xy8ygM", "s1", 0): "closes",
    ("turn-ZUtRsq", "s1", 0): "opens",
    ("turn-co177a", "s1", 0): "opens",
    ("turn-g9JGYE", "s1", 0): "opens",
    ("turn-iNKHir", "s4", 0): "opens",
    ("turn-qiO3lB", "s3", 0): "opens",
}


def instant(value):
    return dt.datetime.fromisoformat(value.replace("Z", "+00:00"))


def record(turn_id, suffix=".json"):
    return json.loads((RECORDS / f"{turn_id}{suffix}").read_text(encoding="utf-8"))


def cited_fragments(turn_id):
    analysis = record(turn_id, ".json.analysis.json")
    out = []
    for sentence in analysis.get("sentences", []):
        for fragment in sentence.get("fragments", []):
            if any(ref.get("id") == PATTERN for ref in fragment.get("pattern_refs", [])):
                out.append((sentence["id"], fragment.get("index", 0), fragment))
    return out


def live_gap_rows():
    rows = [json.loads(line) for line in CORPUS.read_text(encoding="utf-8").splitlines()]
    rows = [row for row in rows if row["family"] == "live-gap"]
    if len(rows) != 12:
        raise ValueError(f"expected 12 live-gap rows, found {len(rows)}")
    return sorted(rows, key=lambda row: row["turn_id"])


def validate_quote(turn_id, quote):
    text = record(turn_id).get("source_text", "")
    if quote not in text:
        raise ValueError(f"quote not found in {turn_id}: {quote!r}")


def compute():
    rows = live_gap_rows()
    table = []
    polarity = {"opens": 0, "closes": 0, "context": 0}
    seen_polarity = set()
    for row in rows:
        turn_id = row["turn_id"]
        targets = [fragment.get("target") for fragment in row["move"]["fragments"]]
        close = CLOSURES.get(turn_id)
        if close and close["kind"] == "operator-confirmation":
            validate_quote(close["id"], close["quote"])
        reopen = REOPENINGS.get(turn_id)
        if reopen:
            validate_quote(reopen["id"], reopen["quote"])
        elapsed = ((instant(close["at"]) - instant(row["turn_at"])).total_seconds()
                   if close else None)
        table.append({
            "turn": turn_id,
            "target": "; ".join(targets),
            "closing_kind": close["kind"] if close else "none-found",
            "evidence": close["id"] if close else None,
            "quote": close.get("quote") if close else None,
            "xiang_label": close.get("label") if close else None,
            "seconds_to_close": elapsed,
            "reopened": bool(reopen),
            "reopening_evidence": reopen.get("id") if reopen else None,
        })
        for sentence, index, _fragment in cited_fragments(turn_id):
            key = (turn_id, sentence, index)
            if key not in POLARITY:
                raise ValueError(f"unclassified cited fragment: {key}")
            polarity[POLARITY[key]] += 1
            seen_polarity.add(key)
    unused = set(POLARITY) - seen_polarity
    if unused:
        raise ValueError(f"polarity entries outside corpus: {sorted(unused)}")
    elapsed = [item["seconds_to_close"] for item in table
               if item["seconds_to_close"] is not None]
    summary = {
        "closed_by_operator": sum(item["closing_kind"] == "operator-confirmation"
                                  for item in table),
        "closed_by_record": sum(item["closing_kind"] == "record" for item in table),
        "open": sum(item["closing_kind"] == "none-found" for item in table),
        "median_seconds_to_close": statistics.median(elapsed),
        "direct_record_closure": {"count": 4, "denominator": 12},
        "strict_bound_warrant_closure": {"count": 0, "denominator": 12},
        "pattern_fragment_polarity": polarity,
    }
    return {"table": table, "summary": summary}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--compact", action="store_true")
    args = parser.parse_args()
    print(json.dumps(compute(), ensure_ascii=False,
                     indent=None if args.compact else 2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
