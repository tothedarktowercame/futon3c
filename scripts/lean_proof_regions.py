#!/usr/bin/env python3
"""Extract stable, source-grounded regions from a Lean proof file."""
from __future__ import annotations

import argparse
import hashlib
import json
import re
from pathlib import Path

DECL = re.compile(r"^(?:noncomputable\s+)?(abbrev|def|lemma|theorem)\s+([\w'.]+)")
BULLET = re.compile(r"^(\s*)([·-])\s+(.*)$")
LOCAL = re.compile(r"^\s*(have|obtain|choose|intro|refine|exact)\b")


def extract(path: Path, source_ref: str | None = None) -> dict:
    raw = path.read_bytes()
    lines = raw.decode("utf-8").splitlines()
    starts = []
    for number, line in enumerate(lines, 1):
        match = DECL.match(line)
        if match:
            starts.append((number, match.group(1), match.group(2)))

    names = {name for _, _, name in starts}
    declarations = []
    for index, (start, kind, name) in enumerate(starts):
        end = starts[index + 1][0] - 1 if index + 1 < len(starts) else len(lines)
        text = "\n".join(lines[start - 1:end])
        declarations.append({
            "id": f"decl:{name}", "kind": kind, "name": name,
            "start_line": start, "end_line": end,
            "declared_uses": sorted(n for n in names - {name}
                                    if re.search(rf"(?<![\w']){re.escape(n)}(?![\w'])", text)),
        })

    regions = []
    for declaration in declarations:
        stack: list[tuple[int, str]] = []
        ordinal = 0
        for number in range(declaration["start_line"], declaration["end_line"] + 1):
            line = lines[number - 1]
            bullet = BULLET.match(line)
            if bullet:
                indent = len(bullet.group(1))
                while stack and stack[-1][0] >= indent:
                    stack.pop()
                ordinal += 1
                region_id = f"{declaration['id']}:branch:{ordinal}"
                regions.append({
                    "id": region_id, "declaration": declaration["id"],
                    "kind": "tactic-branch", "start_line": number,
                    "indent": indent, "parent": stack[-1][1] if stack else declaration["id"],
                    "opener": bullet.group(3).strip(),
                })
                stack.append((indent, region_id))
            elif LOCAL.match(line):
                regions.append({
                    "id": f"{declaration['id']}:step:{number}",
                    "declaration": declaration["id"], "kind": "named-tactic-step",
                    "start_line": number, "parent": stack[-1][1] if stack else declaration["id"],
                    "opener": line.strip(),
                })

    return {
        "schema": "futon3c.lean-proof-regions/v1",
        "extraction": "syntactic-source-regions",
        "source": source_ref or str(path),
        "source_sha256": hashlib.sha256(raw).hexdigest(),
        "line_count": len(lines),
        "declarations": declarations,
        "regions": regions,
        "limitations": [
            "No tactic-state or goal reconstruction.",
            "declared_uses records references only to declarations in this source file.",
            "Semantic dependency closure must be supplied by Lean declaration reachability.",
        ],
    }


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("source", type=Path)
    parser.add_argument("--source-ref")
    parser.add_argument("--output", type=Path)
    args = parser.parse_args()
    payload = json.dumps(extract(args.source, args.source_ref), ensure_ascii=False,
                         sort_keys=True, indent=2) + "\n"
    if args.output:
        args.output.write_text(payload)
    else:
        print(payload, end="")


if __name__ == "__main__":
    main()
