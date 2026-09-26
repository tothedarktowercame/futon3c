#!/usr/bin/env python3
"""Census and normalize connective ``have`` steps in the APM Lean corpus."""

from __future__ import annotations

import argparse
import json
import re
import unicodedata
from collections import defaultdict
from dataclasses import dataclass
from pathlib import Path
from typing import Iterator


APM_ROOT = Path("/home/joe/code/apm-lean")
CORPUS_GLOB = "problems/*/lean/Main.lean"
OUTPUT_ROOT = Path("/home/joe/code/futon3c/data/glue-census")

DECL_RE = re.compile(
    r"^\s*(?:(?:private|protected|noncomputable|unsafe|local)\s+)*"
    r"(?:theorem|lemma|def)\s+([^\s:{(]+)",
    re.MULTILINE,
)
HAVE_RE = re.compile(r"\bhave\b")
IDENT_RE = re.compile(r"(?u)[^\W\d]\w*'*(?:\.[^\W\d]\w*'*)*")
NUMBER_RE = re.compile(r"(?<![\w'])\d+(?:\.\d+)?(?:[eE][+-]?\d+)?")
APM_REF_RE = re.compile(r"(?iu)\b(?:apm|bpm)_[\w'.]+")


@dataclass(frozen=True)
class HaveStep:
    problem: str
    decl: str
    line: int
    raw: str
    normalized: str


def mask_noncode(text: str) -> str:
    """Replace comments/string contents with spaces while preserving newlines."""
    chars = list(text)
    result = list(text)
    i = 0
    block_depth = 0
    in_string = False
    escaped = False
    while i < len(chars):
        if block_depth:
            if i + 1 < len(chars) and chars[i] == "/" and chars[i + 1] == "-":
                result[i] = result[i + 1] = " "
                block_depth += 1
                i += 2
            elif i + 1 < len(chars) and chars[i] == "-" and chars[i + 1] == "/":
                result[i] = result[i + 1] = " "
                block_depth -= 1
                i += 2
            else:
                if chars[i] != "\n":
                    result[i] = " "
                i += 1
        elif in_string:
            if chars[i] != "\n":
                result[i] = " "
            if escaped:
                escaped = False
            elif chars[i] == "\\":
                escaped = True
            elif chars[i] == '"':
                in_string = False
            i += 1
        elif i + 1 < len(chars) and chars[i] == "/" and chars[i + 1] == "-":
            result[i] = result[i + 1] = " "
            block_depth = 1
            i += 2
        elif i + 1 < len(chars) and chars[i] == "-" and chars[i + 1] == "-":
            result[i] = result[i + 1] = " "
            i += 2
            while i < len(chars) and chars[i] != "\n":
                result[i] = " "
                i += 1
        elif chars[i] == '"':
            result[i] = " "
            in_string = True
            i += 1
        else:
            i += 1
    return "".join(result)


def declaration_positions(masked: str) -> list[tuple[int, str]]:
    return [(match.start(), match.group(1)) for match in DECL_RE.finditer(masked)]


def enclosing_declaration(positions: list[tuple[int, str]], offset: int) -> str:
    current = "<top-level>"
    for position, name in positions:
        if position > offset:
            break
        current = name
    return current


def statement_end(masked: str, start: int) -> int | None:
    """Find a top-level ``:=`` or ``by`` ending a have statement header."""
    stack: list[str] = []
    matching = {")": "(", "]": "[", "}": "{"}
    i = start + len("have")
    while i < len(masked):
        char = masked[i]
        if char in "([{":
            stack.append(char)
        elif char in ")]}":
            if stack and stack[-1] == matching[char]:
                stack.pop()
        elif not stack and masked.startswith(":=", i):
            return i
        elif not stack and masked.startswith("by", i):
            before = masked[i - 1] if i else " "
            after = masked[i + 2] if i + 2 < len(masked) else " "
            if not (before.isalnum() or before in "_'") and not (
                after.isalnum() or after in "_'"
            ):
                return i
        i += 1
    return None


def binder_name_spans(text: str) -> list[tuple[int, int]]:
    """Find simple names bound before a colon in (), {}, or [] binders."""
    spans: list[tuple[int, int]] = []
    for match in re.finditer(r"[({[]\s*([^:(){}\[\]]+?)\s*:", text):
        prefix = match.group(1)
        prefix_start = match.start(1)
        for ident in IDENT_RE.finditer(prefix):
            spans.append((prefix_start + ident.start(), prefix_start + ident.end()))
    return spans


def normalize(raw: str) -> str:
    text = re.sub(r"\s+", " ", raw).strip()
    text = APM_REF_RE.sub("APMREF", text)
    binder_spans: list[tuple[int, int]] = binder_name_spans(text)
    have_name = re.match(r"have\s+([^\s:({]+)", text)
    have_span = have_name.span(1) if have_name else None

    def normalize_ident(match: re.Match[str]) -> str:
        start, end = match.span()
        token = match.group(0)
        if token == "APMREF":
            return token
        if have_span and start >= have_span[0] and end <= have_span[1]:
            return "H"
        if any(start >= left and end <= right for left, right in binder_spans):
            if token.startswith("h"):
                return "H"
        if (len(token) == 1 and unicodedata.category(token) == "Ll") or re.fullmatch(
            r"[A-Za-zα-ω]'*", token
        ):
            return "V"
        return token

    text = IDENT_RE.sub(normalize_ident, text)
    text = NUMBER_RE.sub("N", text)
    return text


def extract_file(path: Path) -> Iterator[HaveStep]:
    text = path.read_text(encoding="utf-8")
    masked = mask_noncode(text)
    declarations = declaration_positions(masked)
    problem = path.parents[1].name
    for match in HAVE_RE.finditer(masked):
        end = statement_end(masked, match.start())
        if end is None:
            continue
        raw = re.sub(r"\s+", " ", text[match.start() : end]).strip()
        if not raw:
            continue
        yield HaveStep(
            problem=problem,
            decl=enclosing_declaration(declarations, match.start()),
            line=text.count("\n", 0, match.start()) + 1,
            raw=raw,
            normalized=normalize(raw),
        )


def write_outputs(steps: list[HaveStep]) -> dict[str, list[HaveStep]]:
    OUTPUT_ROOT.mkdir(parents=True, exist_ok=True)
    corpus_path = OUTPUT_ROOT / "have-corpus.jsonl"
    clusters_path = OUTPUT_ROOT / "clusters.tsv"
    with corpus_path.open("w", encoding="utf-8") as stream:
        for step in steps:
            row = {
                "problem": step.problem,
                "decl": step.decl,
                "line": step.line,
                "raw": step.raw,
                "normalized": step.normalized,
            }
            stream.write(json.dumps(row, ensure_ascii=False, separators=(",", ":")) + "\n")

    clusters: dict[str, list[HaveStep]] = defaultdict(list)
    for step in steps:
        clusters[step.normalized].append(step)
    ordered = sorted(
        clusters.items(),
        key=lambda item: (-len(item[1]), -len({s.problem for s in item[1]}), item[0]),
    )
    with clusters_path.open("w", encoding="utf-8", newline="") as stream:
        for shape, members in ordered:
            problems = sorted({member.problem for member in members})
            stream.write(
                f"{shape}\t{len(members)}\t{len(problems)}\t{','.join(problems[:5])}\n"
            )
    return clusters


def print_summary(steps: list[HaveStep], clusters: dict[str, list[HaveStep]]) -> None:
    print(f"files: {len({step.problem for step in steps})}")
    print(f"have steps: {len(steps)}")
    print(f"normalized shapes: {len(clusters)}")
    for threshold in (2, 3, 5, 10):
        count = sum(
            1 for members in clusters.values()
            if len({member.problem for member in members}) >= threshold
        )
        print(f"shapes in >={threshold} distinct problems: {count}")
    print("top 20 recurring shapes:")
    recurring = [
        (shape, members) for shape, members in clusters.items()
        if len({member.problem for member in members}) >= 2
    ]
    recurring.sort(
        key=lambda item: (-len(item[1]), -len({s.problem for s in item[1]}), item[0])
    )
    for index, (shape, members) in enumerate(recurring[:20], 1):
        problem_count = len({member.problem for member in members})
        print(f"{index:2d}. count={len(members)} problems={problem_count} :: {shape}")


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument(
        "--limit", type=int, metavar="N",
        help="scan only the first N problem files in deterministic path order",
    )
    args = parser.parse_args()
    paths = sorted(APM_ROOT.glob(CORPUS_GLOB))
    if args.limit is not None:
        if args.limit < 0:
            parser.error("--limit must be nonnegative")
        paths = paths[: args.limit]
    steps = [step for path in paths for step in extract_file(path)]
    steps.sort(key=lambda step: (step.problem, step.line, step.decl, step.raw))
    clusters = write_outputs(steps)
    print_summary(steps, clusters)


if __name__ == "__main__":
    main()
