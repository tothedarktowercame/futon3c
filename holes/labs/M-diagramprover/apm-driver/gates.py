#!/usr/bin/env python3
"""Mechanical Lean gates for the APM driver.

The apm-lean repository is read-only.  The only modified Lean source is a
scratch copy used for ``#print axioms`` under the system temporary directory.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import re
import subprocess
import tempfile
from pathlib import Path
from typing import Any, Iterable


DEFAULT_REPO = Path("/home/joe/code/apm-lean")
DEFAULT_TIMEOUT_SECONDS = 900
STDERR_TAIL_LINES = 30
THEOREM_RE = re.compile(r"(?m)^\s*theorem\s+([A-Za-z_][A-Za-z0-9_'.]*)\b")
SORRY_RE = re.compile(r"\bsorry\b")
PROTOCOL_WORD_RE = re.compile(r"\b(tried|searched|route|blocker|requires)\b", re.I)
MATHLIB_IDENTIFIER_RE = re.compile(r"(?:`[A-Za-z_][A-Za-z0-9_'.]*`|\b[A-Za-z][A-Za-z0-9']*_[A-Za-z0-9_']+\b)")


class GateError(ValueError):
    """Raised when a Lean source cannot supply a mechanical gate input."""


def _scan_lean(source: str) -> tuple[str, list[str]]:
    """Strip nested Lean comments while preserving line positions.

    Returns comment-free code and the comment text associated with each source
    line.  Strings and character literals protect comment-looking text.
    """

    code: list[str] = []
    comments: list[list[str]] = [[]]
    index = 0
    block_depth = 0
    in_string = False
    in_char = False
    escaped = False
    line_comment = False

    while index < len(source):
        char = source[index]
        pair = source[index : index + 2]

        if char == "\n":
            code.append("\n")
            comments.append([])
            line_comment = False
            escaped = False
            index += 1
            continue

        if line_comment:
            comments[-1].append(char)
            code.append(" ")
            index += 1
            continue

        if block_depth:
            if pair == "/-":
                comments[-1].append(pair)
                code.extend("  ")
                block_depth += 1
                index += 2
            elif pair == "-/":
                code.extend("  ")
                block_depth -= 1
                index += 2
            else:
                comments[-1].append(char)
                code.append(" ")
                index += 1
            continue

        if in_string or in_char:
            code.append(char)
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif in_string and char == '"':
                in_string = False
            elif in_char and char == "'":
                in_char = False
            index += 1
            continue

        if pair == "--":
            code.extend("  ")
            line_comment = True
            index += 2
        elif pair == "/-":
            code.extend("  ")
            block_depth = 1
            index += 2
        else:
            code.append(char)
            if char == '"':
                in_string = True
            elif char == "'":
                # Lean identifiers may contain apostrophes.  Treat a quote as
                # a character literal only when a closing quote is nearby.
                in_char = "'" in source[index + 1 : index + 5]
            index += 1

    if block_depth:
        raise GateError("unterminated block comment")
    return "".join(code), ["".join(parts).strip() for parts in comments]


def strip_comments(source: str) -> str:
    """Return Lean source with line and nested block comments removed."""

    return _scan_lean(source)[0]


def sorry_sites(source: str) -> list[int]:
    """Return one-based source lines containing executable ``sorry`` tokens."""

    stripped = strip_comments(source)
    return [stripped.count("\n", 0, match.start()) + 1 for match in SORRY_RE.finditer(stripped)]


def count_sorries(source: str) -> int:
    """Count executable ``sorry`` tokens after stripping Lean comments."""

    return len(sorry_sites(source))


def extract_main_statement(source: str, problem_id: str | None = None) -> tuple[str, str]:
    """Return the main theorem name and declaration through its first ``:=``.

    Discovery (chain-3 fix, 2026-08-04): a theorem whose name contains the
    problem id wins; else a file with exactly one ``theorem`` uses it; a
    multi-theorem file with no problem-named theorem has NO identifiable
    main statement and raises — the statement-first contract, mechanized.
    """

    stripped = strip_comments(source)
    matches = list(THEOREM_RE.finditer(stripped))
    if not matches:
        raise GateError("no theorem declaration found")
    match = None
    if problem_id:
        wanted = problem_id.lower()
        for candidate in matches:
            if wanted in candidate.group(1).lower():
                match = candidate
                break
    if match is None:
        if len(matches) == 1:
            match = matches[0]
        else:
            raise GateError(
                "no-main-statement: multiple theorems, none named for the problem")
    declaration_end = stripped.find(":=", match.end())
    if declaration_end < 0:
        raise GateError(f"theorem {match.group(1)} has no := delimiter")
    declaration = stripped[match.start() : declaration_end + 2]
    normalized = " ".join(declaration.split())
    return match.group(1), normalized


def statement_hash(source: str, problem_id: str | None = None) -> tuple[str, str, str]:
    """Return theorem name, normalized declaration, and prefixed SHA-256."""

    theorem_name, normalized = extract_main_statement(source, problem_id)
    digest = hashlib.sha256(normalized.encode("utf-8")).hexdigest()
    return theorem_name, normalized, f"sha256:{digest}"


def qualified_theorem_name(source: str, theorem_name: str) -> str:
    """Resolve the namespace active at the main theorem declaration."""

    stripped = strip_comments(source)
    theorem_match = THEOREM_RE.search(stripped)
    if theorem_match is None:
        raise GateError("no theorem declaration found")
    stack: list[tuple[str, str | None]] = []
    for line in stripped[: theorem_match.start()].splitlines():
        namespace = re.match(r"^\s*namespace\s+([A-Za-z_][A-Za-z0-9_.']*)\s*$", line)
        section = re.match(r"^\s*(?:noncomputable\s+)?section(?:\s+\S+)?\s*$", line)
        ending = re.match(r"^\s*end(?:\s+\S+)?\s*$", line)
        if namespace:
            stack.append(("namespace", namespace.group(1)))
        elif section:
            stack.append(("section", None))
        elif ending and stack:
            stack.pop()
    namespaces = [name for kind, name in stack if kind == "namespace" and name]
    return ".".join([*namespaces, theorem_name]) if namespaces else theorem_name


def boundary_conformance(source: str) -> dict[str, Any]:
    """Check the boundary protocol at every executable sorry site.

    A site conforms when its nearest preceding comment block contributes at
    least five nonempty lines within ten physical lines, includes a plausible
    Mathlib identifier, and names search/route/blocker work.
    """

    stripped, comment_lines = _scan_lean(source)
    stripped_lines = stripped.splitlines()
    original_lines = source.splitlines()
    sites = sorry_sites(source)
    details = []
    for line_number in sites:
        start = max(0, line_number - 11)
        cursor = line_number - 2
        block: list[str] = []
        while cursor >= start:
            comment = comment_lines[cursor] if cursor < len(comment_lines) else ""
            code = stripped_lines[cursor].strip() if cursor < len(stripped_lines) else ""
            original = original_lines[cursor].strip() if cursor < len(original_lines) else ""
            if comment:
                block.append(comment)
            elif original.startswith("--") or (not code and not original):
                pass
            else:
                break
            cursor -= 1
        block.reverse()
        joined = "\n".join(block)
        detail = {
            "line": line_number,
            "comment-lines": len(block),
            "has-mathlib-identifier": bool(MATHLIB_IDENTIFIER_RE.search(joined)),
            "has-protocol-word": bool(PROTOCOL_WORD_RE.search(joined)),
        }
        detail["conforming"] = (
            detail["comment-lines"] >= 5
            and detail["has-mathlib-identifier"]
            and detail["has-protocol-word"]
        )
        details.append(detail)
    conforming = all(detail["conforming"] for detail in details)
    # A conforming protocol write-up in the main declaration's docstring
    # also counts (trial chain 2, 2026-08-04: the runner put the full
    # bridge/APIs-searched/routes account in the theorem docstring — the
    # WHERE was too rigid, not the work).
    docstring_conforming = False
    if not conforming and details:
        # Any block comment counts: /-- docstrings, /-! module docs, and
        # plain /- blocks (chain-4 fix: the conforming boundary lived in a
        # /-! header the /-- -only scan missed).
        for doc_body in re.findall(r"/-[-!]?(.*?)-/", source, re.S):
            doc_lines = [l for l in doc_body.splitlines() if l.strip()]
            if (
                len(doc_lines) >= 5
                and MATHLIB_IDENTIFIER_RE.search(doc_body)
                and PROTOCOL_WORD_RE.search(doc_body)
            ):
                docstring_conforming = True
                break
    return {
        "conforming": conforming or docstring_conforming,
        "docstring-conforming": docstring_conforming,
        "sites": details,
    }


def _tail(text: str, lines: int = STDERR_TAIL_LINES) -> str:
    return "\n".join(text.splitlines()[-lines:])


def _run_lean(
    lean_file: Path,
    *,
    repo_root: Path,
    timeout_seconds: int,
) -> dict[str, Any]:
    command = ["lake", "env", "lean", str(lean_file)]
    try:
        completed = subprocess.run(
            command,
            cwd=repo_root,
            capture_output=True,
            text=True,
            timeout=timeout_seconds,
            check=False,
        )
        return {
            "exit-code": completed.returncode,
            "timed-out": False,
            "stdout": completed.stdout,
            "stderr": completed.stderr,
            "stderr-tail": _tail(completed.stderr),
        }
    except subprocess.TimeoutExpired as exc:
        stdout = exc.stdout.decode() if isinstance(exc.stdout, bytes) else exc.stdout or ""
        stderr = exc.stderr.decode() if isinstance(exc.stderr, bytes) else exc.stderr or ""
        return {
            "exit-code": 124,
            "timed-out": True,
            "stdout": stdout,
            "stderr": stderr,
            "stderr-tail": _tail(stderr),
        }


def _axiom_line(output: str, theorem_name: str) -> str | None:
    for line in output.splitlines():
        if theorem_name in line and (
            "depends on axioms:" in line or "does not depend on any axioms" in line
        ):
            return line
    return None


def run_axiom_probe(
    source: str,
    theorem_name: str,
    *,
    repo_root: Path = DEFAULT_REPO,
    timeout_seconds: int = DEFAULT_TIMEOUT_SECONDS,
) -> dict[str, Any]:
    """Run ``#print axioms`` on a system-temporary copy of ``source``."""

    qualified_name = qualified_theorem_name(source, theorem_name)
    with tempfile.TemporaryDirectory(prefix="apm-driver-axioms-") as directory:
        scratch = Path(directory) / "Main.lean"
        scratch.write_text(
            source.rstrip() + f"\n\n#print axioms {qualified_name}\n",
            encoding="utf-8",
        )
        result = _run_lean(
            scratch,
            repo_root=repo_root,
            timeout_seconds=timeout_seconds,
        )
    combined = result["stdout"] + "\n" + result["stderr"]
    return {
        "exit-code": result["exit-code"],
        "timed-out": result["timed-out"],
        "line": _axiom_line(combined, qualified_name),
        "stderr-tail": result["stderr-tail"],
    }


def _classify(
    build: dict[str, Any],
    sorry_count: int,
    boundary: dict[str, Any],
    axioms: dict[str, Any],
) -> tuple[str, list[str]]:
    reasons = []
    axiom_line = axioms["line"] or ""
    has_sorry_axiom = "sorryAx" in axiom_line

    if build["exit-code"] != 0:
        reasons.append("build-failed")
    if axioms["exit-code"] != 0 or axioms["line"] is None:
        reasons.append("axiom-probe-failed")
    if sorry_count == 0 and has_sorry_axiom:
        # The impossible direction: a clean source cannot depend on sorryAx.
        reasons.append("sorry-count-axiom-contradiction")
    # NOTE (chain-3 fix): sorries>0 with a sorryAx-free main theorem is NOT
    # a contradiction — the sorry may live in a helper the main theorem
    # does not use. Discovery now guarantees the checked theorem IS the
    # problem statement, so a clean main + sorried helpers classifies by
    # sorry count and boundary conformance like any partial.

    if reasons:
        return "defective", reasons
    if sorry_count == 0:
        return "closed", []
    if boundary["conforming"]:
        return "partial", []
    return "defective", ["boundary-nonconforming"]


def gate_path(
    lean_file: Path,
    *,
    repo_root: Path = DEFAULT_REPO,
    timeout_seconds: int = DEFAULT_TIMEOUT_SECONDS,
    problem_id: str | None = None,
) -> dict[str, Any]:
    """Run all gates for a Lean file and return driver.py's exact payload."""

    source = lean_file.read_text(encoding="utf-8")
    try:
        theorem_name, _normalized_statement, digest = statement_hash(source, problem_id)
    except GateError as exc:
        # A file we cannot locate a main statement in is a DEFECTIVE
        # classification, not a driver crash (chain-5 fix).
        return {
            "outcome": "defective",
            "statement-hash": "sha256:" + "0" * 64,
            "gate-results": {
                "build": {"exit-code": None},
                "sorries": count_sorries(source),
                "boundary-conforming": False,
                "axioms": {"exit-code": None, "line": None},
                "theorem-name": None,
                "reasons": [f"statement-discovery-failed: {exc}"],
            },
        }
    sorry_count = count_sorries(source)
    boundary = boundary_conformance(source)
    build_raw = _run_lean(
        lean_file,
        repo_root=repo_root,
        timeout_seconds=timeout_seconds,
    )
    build = {
        "exit-code": build_raw["exit-code"],
        "timed-out": build_raw["timed-out"],
        "stderr-tail": build_raw["stderr-tail"],
    }
    axioms = run_axiom_probe(
        source,
        theorem_name,
        repo_root=repo_root,
        timeout_seconds=timeout_seconds,
    )
    outcome, reasons = _classify(build, sorry_count, boundary, axioms)
    return {
        "outcome": outcome,
        "statement-hash": digest,
        "gate-results": {
            "build": build,
            "sorries": sorry_count,
            "boundary-conforming": boundary["conforming"],
            "boundary-sites": boundary["sites"],
            "axioms": axioms,
            "theorem-name": theorem_name,
            "reasons": reasons,
        },
    }


def gate_fn(
    problem_id: str,
    *,
    repo_root: Path = DEFAULT_REPO,
    timeout_seconds: int = DEFAULT_TIMEOUT_SECONDS,
) -> dict[str, Any]:
    """H1 injection implementation: mechanically gate one APM problem."""

    lean_file = repo_root / "problems" / problem_id / "lean" / "Main.lean"
    return gate_path(
        lean_file,
        repo_root=repo_root,
        timeout_seconds=timeout_seconds,
        problem_id=problem_id,
    )


def main(argv: Iterable[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("problem_id")
    parser.add_argument("--repo", type=Path, default=DEFAULT_REPO)
    parser.add_argument("--timeout", type=int, default=DEFAULT_TIMEOUT_SECONDS)
    args = parser.parse_args(argv)
    print(
        json.dumps(
            gate_fn(args.problem_id, repo_root=args.repo, timeout_seconds=args.timeout),
            indent=2,
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
