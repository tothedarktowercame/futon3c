#!/usr/bin/env python3
"""Mechanical Lean gates for the APM driver.

The apm-lean repository is read-only.  The only modified Lean source is a
scratch copy used for ``#print axioms`` under the system temporary directory.
"""

from __future__ import annotations

import argparse
import hashlib
import json
import os
import re
import subprocess
import tempfile
from pathlib import Path
from typing import Any, Iterable


DEFAULT_REPO = Path("/home/joe/code/apm-lean")
DEFAULT_TIMEOUT_SECONDS = 900
STDERR_TAIL_LINES = 30
THEOREM_RE = re.compile(r"(?m)^\s*theorem\s+([A-Za-z_][A-Za-z0-9_'.]*)\b")
CLAIM_RE = re.compile(
    r"(?m)^\s*(?:private\s+|protected\s+)?(?:theorem|lemma)\s+([A-Za-z_][A-Za-z0-9_'.]*)\b")
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


_DECL_OPEN = {"(": ")", "[": "]", "{": "}", "⟨": "⟩", "⦃": "⦄"}
_DECL_CLOSE = set(_DECL_OPEN.values())


def _declaration_delimiter(stripped: str, start: int) -> int:
    """Index OF the ``:`` in the ``:=`` that ends a theorem's declaration.

    Same contract as the ``str.find(":=")`` it replaces: the caller adds 2.

    NOT simply the first ``:=``. Lean named-argument syntax puts one INSIDE the
    statement — `M99A05WeaklyConverges (𝕜 := ℂ) …`, `apm_m01J04_… (Ω := Ω) …` —
    and `let x := …` does too. Cutting at the first occurrence truncated the
    declaration for 15 of 269 banked problems, so their statement hash covered
    only a prefix: on 2026-08-06 an entire conclusion clause of m99A05 was
    replaced by `True` without moving its hash. Only a ``:=`` at bracket depth
    zero ends the declaration.
    """

    depth = 0
    i = start
    while i < len(stripped):
        char = stripped[i]
        if char in _DECL_OPEN:
            depth += 1
        elif char in _DECL_CLOSE:
            depth -= 1
        elif char == ":" and stripped[i + 1:i + 2] == "=" and depth == 0:
            return i
        i += 1
    return -1


def extract_main_statement(source: str, problem_id: str | None = None,
                           expected_name: str | None = None) -> tuple[str, str]:
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
    if expected_name:
        # A frozen chain knows its main theorem by name (hop-0 freeze);
        # re-discovery must not be confused by helper theorems a closer
        # legitimately adds (chain-6 fix).
        for candidate in matches:
            if candidate.group(1) == expected_name:
                match = candidate
                break
        if match is None:
            raise GateError(
                f"frozen main theorem {expected_name!r} not found in source")
    if match is None and problem_id:
        wanted = problem_id.lower()
        # EXACT `apm_<id>` first, substring only as a fallback. The substring
        # scan takes the FIRST theorem containing the id, so a closer that
        # legitimately factors out `apm_a94J06_at_zero` above the main theorem
        # silently re-keys the contract to that helper: a94J06's statement was
        # then byte-identical yet hashed differently, the gate returned
        # void-statement-changed, and a real close was discarded. Same defect
        # the axiom probe had (t97J01). Exactly 2 of 447 artifacts were
        # mis-keyed when this was written, and both are re-frozen alongside it.
        exact = f"apm_{wanted}"
        for candidate in matches:
            if candidate.group(1).lower() == exact:
                match = candidate
                break
        if match is None:
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
    declaration_end = _declaration_delimiter(stripped, match.end())
    if declaration_end < 0:
        raise GateError(f"theorem {match.group(1)} has no := delimiter")
    declaration = stripped[match.start() : declaration_end + 2]
    normalized = " ".join(declaration.split())
    return match.group(1), normalized


def statement_hash(source: str, problem_id: str | None = None,
                   expected_name: str | None = None) -> tuple[str, str, str]:
    """Return theorem name, normalized declaration, and prefixed SHA-256."""

    theorem_name, normalized = extract_main_statement(source, problem_id, expected_name)
    digest = hashlib.sha256(normalized.encode("utf-8")).hexdigest()
    return theorem_name, normalized, f"sha256:{digest}"


def declaration_hashes(source: str) -> dict[str, str]:
    """Hash EVERY claim (theorem and lemma) in the file, keyed by name.

    ``statement_hash`` protects one main theorem, which is all a bank problem
    needs — but 120 pre-campaign artifacts predate the ``apm_<id>`` naming
    convention, so ``extract_main_statement`` cannot tell which of their five
    or twenty declarations is the claim, and raises. Reviewing those left them
    approved with NO contract, i.e. still fully substitutable.

    A set contract fixes that without renaming anything: freeze the whole
    declaration set, then require it to be PRESERVED. Adding a claim is legal
    (that is what a closer does when it factors out a helper); weakening or
    deleting one that a reviewer read is not. Includes ``lemma`` because these
    files often carry their real content there — a01J04 has no ``theorem`` at
    all.
    """

    stripped = strip_comments(source)
    out: dict[str, str] = {}
    for match in CLAIM_RE.finditer(stripped):
        end = _declaration_delimiter(stripped, match.end())
        if end < 0:
            continue
        normalized = " ".join(stripped[match.start():end + 2].split())
        digest = hashlib.sha256(normalized.encode("utf-8")).hexdigest()
        out[match.group(1)] = f"sha256:{digest}"
    return out


def declaration_set_drift(frozen: dict[str, str], source: str) -> list[str]:
    """Claims that were reviewed and have since been removed or changed.

    Empty list means the contract holds. Names absent from ``frozen`` are new
    and therefore fine — the asymmetry is the point.
    """

    current = declaration_hashes(source)
    drift = []
    for name, digest in sorted(frozen.items()):
        if name not in current:
            drift.append(f"{name}: REMOVED")
        elif current[name] != digest:
            drift.append(f"{name}: CHANGED")
    return drift


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

    def _cap_child() -> None:
        # Bound the CHILD's address space so a pathological artifact fails
        # itself instead of taking the gate down with it. b00J02 proves
        # `Fintype.card (GL (Fin 3) (ZMod 4)) = 86016` by `native_decide`,
        # which enumerates 4^9 matrices: it hit 48G and the cgroup OOM killer
        # killed the whole run three times, recording zero outcomes each time.
        # An RLIMIT makes Lean die with an allocation failure, which surfaces
        # as a normal non-zero build exit and lets the gate carry on.
        try:
            import resource
            cap = int(os.environ.get("APM_LEAN_MEM_CAP_BYTES", 12 * 1024 ** 3))
            resource.setrlimit(resource.RLIMIT_AS, (cap, cap))
        except Exception:
            pass

    try:
        completed = subprocess.run(
            command,
            cwd=repo_root,
            capture_output=True,
            text=True,
            timeout=timeout_seconds,
            check=False,
            preexec_fn=_cap_child,
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


OPAQUE_RE = re.compile(r"(?m)^\s*opaque\s+([A-Za-z_][A-Za-z0-9_'.]*)")


def opaque_declarations(source: str) -> list[str]:
    """Names introduced by ``opaque``, i.e. constants with NO definition.

    A statement quantifying over one is not about the object the source names:
    `opaque windingNumber : ℤ` makes `windingNumber γ ≠ 2` a claim about an
    unknown integer. Eight artifacts did this, one fabricating an entire
    differential-forms apparatus (wedge, oriented integral, exterior
    derivative) so that nothing in the statement referred to forms at all.

    Neither existing gate sees this: the file has no ``sorry``, and ``opaque``
    introduces no axioms — a theorem about one prints "does not depend on any
    axioms", so the axiom sweep passes it.

    Comments are stripped first: several of these files DISCUSS opacity in
    prose, and a raw grep counts that as a declaration.
    """

    return OPAQUE_RE.findall(strip_comments(source))


AXIOM_WHITELIST = frozenset({"propext", "Classical.choice", "Quot.sound"})


def impure_axioms(output: str, theorem_name: str) -> list[str]:
    """Axioms outside the accepted kernel set, from ``#print axioms`` output.

    Read the COMBINED output, not the single line ``_axiom_line`` returns:
    Lean wraps a long bracketed list across lines, so a line-scoped match
    silently truncates it. b95J01 carried seven ``native_decide`` axioms and
    the list wrapped after the second.

    Split on commas and strip per element rather than deleting spaces — a
    wrapped element arrives as ``"\\nClassical.choice"``, which fails the
    whitelist and reads as a defect in every artifact.
    """

    match = re.search(
        rf"'{re.escape(theorem_name)}' depends on axioms: \[([^\]]*)\]",
        output,
        re.S,
    )
    if not match:
        return []
    found = {part.strip() for part in match.group(1).split(",") if part.strip()}
    return sorted(found - AXIOM_WHITELIST)


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
    # `.get`, not `[...]`: the probe must never be the thing that takes the
    # gate lane down. A result without captured output is simply a probe that
    # told us nothing, which the caller already handles as `axiom-probe-failed`.
    combined = (result.get("stdout") or "") + "\n" + (result.get("stderr") or "")
    return {
        "exit-code": result.get("exit-code", 1),
        "timed-out": result.get("timed-out", False),
        "line": _axiom_line(combined, qualified_name),
        "impure": impure_axioms(combined, qualified_name),
        "stderr-tail": result.get("stderr-tail", ""),
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
    expected_name: str | None = None,
) -> dict[str, Any]:
    """Run all gates for a Lean file and return driver.py's exact payload."""

    source = lean_file.read_text(encoding="utf-8")
    try:
        theorem_name, _normalized_statement, digest = statement_hash(
            source, problem_id, expected_name)
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
    expected_name: str | None = None,
) -> dict[str, Any]:
    """H1 injection implementation: mechanically gate one APM problem."""

    lean_file = repo_root / "problems" / problem_id / "lean" / "Main.lean"
    return gate_path(
        lean_file,
        repo_root=repo_root,
        timeout_seconds=timeout_seconds,
        problem_id=problem_id,
        expected_name=expected_name,
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
