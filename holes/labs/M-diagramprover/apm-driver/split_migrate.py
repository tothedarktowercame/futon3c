#!/usr/bin/env python3
"""Split an APM Lean artifact into bank-owned statement and prover files."""

from __future__ import annotations

import re
from pathlib import Path

import gates


HELPER_RE = re.compile(
    r"(?m)^\s*(?:@\[[^\n]*\]\s*)*"
    r"(?:(?:noncomputable|private|protected|local)\s+)*"
    r"(?:def|abbrev|structure|instance|inductive)\b"
)
IMPORT_RE = re.compile(r"(?m)^\s*(?:public\s+)?import\s+[^\n]*(?:\n|$)")
PROBLEM_ID_RE = re.compile(r"^[A-Za-z][A-Za-z0-9_]*$")


def _module_id(problem_id: str) -> str:
    if not PROBLEM_ID_RE.fullmatch(problem_id):
        raise gates.GateError(f"invalid problem id: {problem_id!r}")
    return problem_id[0].upper() + problem_id[1:]


def _main_theorem_span(source: str, problem_id: str, theorem_name: str) -> tuple[int, int]:
    """Return the main theorem's start and the end of its ``:=`` token."""

    stripped = gates.strip_comments(source)
    matches = [
        match for match in gates.THEOREM_RE.finditer(stripped)
        if match.group(1) == theorem_name
    ]
    if len(matches) != 1:
        raise gates.GateError(
            f"expected one declaration of {theorem_name!r}, found {len(matches)}"
        )
    match = matches[0]
    declaration_end = stripped.find(":=", match.end())
    if declaration_end < 0:
        raise gates.GateError(f"theorem {theorem_name} has no := delimiter")
    return match.start(), declaration_end + 2


def _last_top_level_colon(text: str) -> int:
    """Find the declaration colon while respecting Lean binder brackets."""

    opening = {"(": ")", "[": "]", "{": "}", "⟨": "⟩"}
    closing = set(opening.values())
    stack: list[str] = []
    candidates: list[int] = []
    in_string = False
    in_char = False
    escaped = False

    for index, char in enumerate(text):
        if in_string or in_char:
            if escaped:
                escaped = False
            elif char == "\\":
                escaped = True
            elif in_string and char == '"':
                in_string = False
            elif in_char and char == "'":
                in_char = False
            continue
        if char == '"':
            in_string = True
            continue
        if char == "'" and "'" in text[index + 1:index + 5]:
            in_char = True
            continue
        if char in opening:
            stack.append(opening[char])
            continue
        if char in closing:
            if not stack or stack[-1] != char:
                raise gates.GateError("unbalanced brackets in theorem declaration")
            stack.pop()
            continue
        if char == ":" and not stack:
            previous = text[index - 1] if index else ""
            following = text[index + 1] if index + 1 < len(text) else ""
            if previous != ":" and following not in {":", "="}:
                candidates.append(index)

    if stack or in_string or in_char:
        raise gates.GateError("unterminated bracket or literal in theorem declaration")
    if not candidates:
        raise gates.GateError("main theorem has no top-level result colon")
    return candidates[-1]


def _statement_parts(declaration: str, theorem_name: str) -> tuple[str, str]:
    prefix = f"theorem {theorem_name}"
    if not declaration.startswith(prefix) or not declaration.endswith(":="):
        raise gates.GateError("unexpected normalized main theorem declaration")
    body = declaration[len(prefix):-2].strip()
    colon = _last_top_level_colon(body)
    binders = body[:colon].strip()
    conclusion = body[colon + 1:].strip()
    if not conclusion:
        raise gates.GateError("main theorem has an empty conclusion")
    return binders, conclusion


def _with_newline(text: str) -> str:
    return text if not text or text.endswith("\n") else text + "\n"


def split_source(source: str, problem_id: str) -> dict[str, str]:
    """Purely split one unsplit Lean source file.

    The statement declaration is obtained from the same extractor used by the
    statement-hash gate.  Any ambiguity fails closed with ``GateError``.
    """

    module_id = _module_id(problem_id)
    import_line = f"import ApmStatements.{module_id}"
    if re.search(rf"(?m)^\s*{re.escape(import_line)}\s*$", source):
        raise gates.GateError("source is already split")

    theorem_name, declaration = gates.extract_main_statement(source, problem_id)
    if theorem_name.endswith("_stmt"):
        raise gates.GateError("statement declaration cannot be split again")
    binders, conclusion = _statement_parts(declaration, theorem_name)
    if not binders and conclusion == f"{theorem_name}_stmt":
        raise gates.GateError("source is already split")
    theorem_start, declaration_end = _main_theorem_span(
        source, problem_id, theorem_name
    )

    prefix = source[:theorem_start]
    proof_tail = source[declaration_end:]
    helper = HELPER_RE.search(gates.strip_comments(prefix))
    header_end = helper.start() if helper else len(prefix)
    header = prefix[:header_end]
    main_header = IMPORT_RE.sub("", header).lstrip("\n")

    statement_rhs = f"∀ {binders}, {conclusion}" if binders else conclusion
    statement_module = (
        _with_newline(prefix).rstrip()
        + f"\n\ndef {theorem_name}_stmt : Prop := {statement_rhs}\n"
    )
    main_file = (
        import_line
        + "\n\n"
        + _with_newline(main_header).rstrip()
        + f"\n\ntheorem {theorem_name} : {theorem_name}_stmt :="
        + proof_tail
    )
    return {
        "statement_module": statement_module,
        "main_file": main_file,
        "theorem_name": theorem_name,
    }


def migrate(problem_id: str, repo: Path, dry_run: bool) -> dict[str, object]:
    """Return, and optionally write, the two artifacts for ``problem_id``."""

    repo = Path(repo)
    module_id = _module_id(problem_id)
    main_path = repo / "problems" / problem_id / "lean" / "Main.lean"
    statement_path = repo / "ApmStatements" / f"{module_id}.lean"
    if not main_path.is_file():
        raise gates.GateError(f"missing problem source: {main_path}")

    result: dict[str, object] = split_source(
        main_path.read_text(encoding="utf-8"), problem_id
    )
    result.update({
        "statement_path": statement_path,
        "main_path": main_path,
        "dry_run": dry_run,
    })
    if not dry_run:
        statement_path.parent.mkdir(parents=True, exist_ok=True)
        statement_path.write_text(str(result["statement_module"]), encoding="utf-8")
        main_path.write_text(str(result["main_file"]), encoding="utf-8")
    return result
