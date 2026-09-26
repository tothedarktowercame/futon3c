#!/usr/bin/env python3
"""Read-only census of declared and executable Lean `sorry` tokens in APM bundles."""

import argparse
import json
import re
from pathlib import Path


SORRY = re.compile(r"(?<![\w'])sorry(?![\w'])")


def code_without_comments_or_strings(text):
    """Mask Lean line/nested-block comments and strings, preserving newlines."""
    out = []
    index = 0
    block_depth = 0
    line_comment = False
    string = False
    escaped = False
    while index < len(text):
        char = text[index]
        following = text[index + 1] if index + 1 < len(text) else ""
        if line_comment:
            if char == "\n":
                line_comment = False
                out.append("\n")
            else:
                out.append(" ")
            index += 1
        elif block_depth:
            if char == "/" and following == "-":
                block_depth += 1
                out.extend("  ")
                index += 2
            elif char == "-" and following == "/":
                block_depth -= 1
                out.extend("  ")
                index += 2
            else:
                out.append("\n" if char == "\n" else " ")
                index += 1
        elif string:
            out.append("\n" if char == "\n" else " ")
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif char == '"':
                string = False
            index += 1
        elif char == "-" and following == "-":
            line_comment = True
            out.extend("  ")
            index += 2
        elif char == "/" and following == "-":
            block_depth = 1
            out.extend("  ")
            index += 2
        elif char == '"':
            string = True
            out.append(" ")
            index += 1
        else:
            out.append(char)
            index += 1
    return "".join(out)


def sorry_count(path):
    if path is None or not path.exists():
        return 0
    return len(SORRY.findall(code_without_comments_or_strings(
        path.read_text(encoding="utf-8", errors="replace"))))


def naive_sorry_count(path):
    if path is None or not path.exists():
        return 0
    return len(SORRY.findall(path.read_text(encoding="utf-8", errors="replace")))


def census(problems):
    rows = []
    for status_path in sorted(problems.glob("*/status.json")):
        status = json.loads(status_path.read_text(encoding="utf-8"))
        lean = status.get("lean", {})
        declared = lean.get("sorry_count_total")
        # Match the measured population: an actual Main.lean plus an integer
        # declared total. Some old statuses omit :main despite having the file.
        main = status_path.parent / "lean" / "Main.lean"
        if not main.exists() or not isinstance(declared, int):
            continue
        scratch_rel = lean.get("scratch")
        scratch = status_path.parent / scratch_rel if scratch_rel else None
        main_count = sorry_count(main)
        scratch_count = sorry_count(scratch)
        actual = main_count + scratch_count
        naive = naive_sorry_count(main) + naive_sorry_count(scratch)
        rows.append({
            "problem_id": status_path.parent.name,
            "declared": declared,
            "lexical": actual,
            "main": main_count,
            "scratch": scratch_count,
            "naive": naive,
            "classification": status.get("classification"),
            "direction": ("agree" if declared == actual else
                          "declared-high" if declared > actual else
                          "declared-low"),
        })
    return rows


def summary(rows):
    return {
        "bundles": len(rows),
        "agree": sum(row["direction"] == "agree" for row in rows),
        "disagree": sum(row["direction"] != "agree" for row in rows),
        "declared_high": sum(row["direction"] == "declared-high" for row in rows),
        "declared_low": sum(row["direction"] == "declared-low" for row in rows),
        "lexically_open": sum(row["lexical"] > 0 for row in rows),
        "lexically_closed": sum(row["lexical"] == 0 for row in rows),
        "declared_open_but_lexically_closed": sum(
            row["declared"] > 0 and row["lexical"] == 0 for row in rows),
        "declared_closed_but_lexically_open": sum(
            row["declared"] == 0 and row["lexical"] > 0 for row in rows),
        "naive_diff_files": sum(row["naive"] != row["lexical"] for row in rows),
        "naive_excess_tokens": sum(row["naive"] - row["lexical"] for row in rows),
    }


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--problems", type=Path,
                        default=Path("/home/joe/code/apm-lean/problems"))
    parser.add_argument("--rows", action="store_true",
                        help="include all per-bundle rows")
    args = parser.parse_args()
    rows = census(args.problems)
    report = {"summary": summary(rows)}
    if args.rows:
        report["rows"] = rows
    print(json.dumps(report, indent=2, sort_keys=True))


if __name__ == "__main__":
    main()
