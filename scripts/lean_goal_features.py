#!/usr/bin/env python3
"""Normalize Lean plain-goal captures into alignment features.

The result contains only formal-side evidence. It does not read a dialogue or
scope label, preserving the pilot's blinded comparison boundary.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
from pathlib import Path

IDENT = re.compile(r"(?<![\w'])[A-Za-z_][\w'.]*(?![\w'])")


def compact(text: str) -> str:
    return " ".join(text.split())


def parse_goal(text: str, declaration_names: list[str]) -> dict:
    before, marker, after = text.partition("⊢")
    if not marker:
        raise ValueError("Lean goal has no turnstile")
    lines = [line.strip() for line in before.splitlines() if line.strip()]
    case = None
    if lines and lines[0].startswith("case "):
        case = lines.pop(0)[5:].strip()
    hypotheses = []
    for line in lines:
        binders, separator, type_ = line.partition(" : ")
        if separator:
            hypotheses.append({"binders": binders.split(), "type": compact(type_)})
        else:
            hypotheses.append({"binders": [], "type": compact(line)})
    target = compact(after)
    references = sorted(name for name in declaration_names
                        if re.search(rf"(?<![\w']){re.escape(name)}(?![\w'])", text))
    return {
        "case": case,
        "hypotheses": hypotheses,
        "target": target,
        "local_declaration_refs": references,
        "target_identifiers": sorted(set(IDENT.findall(target))),
        "goal_sha256": hashlib.sha256(text.encode()).hexdigest(),
    }


def features(capture: dict) -> dict:
    declarations = capture["reachability"]["reachable"]
    regions = []
    for item in capture["goal_captures"]:
        regions.append({
            "region_id": item["region_id"], "line": item["line"],
            "goals": [parse_goal(goal, declarations) for goal in item["goals"]],
        })
    return {
        "schema": "futon3c.lean-goal-features/v1",
        "source": capture["source"],
        "source_sha256": capture["source_sha256"],
        "capture_schema": capture["schema"],
        "blinding": "formal-side-only; no dialogue text or labels read",
        "regions": regions,
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("capture", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    result = features(json.loads(args.capture.read_text()))
    args.output.write_text(json.dumps(result, ensure_ascii=False,
                                      sort_keys=True, indent=2) + "\n")


if __name__ == "__main__":
    main()
