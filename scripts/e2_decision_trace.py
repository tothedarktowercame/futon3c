#!/usr/bin/env python3
"""Extract a tactic-insensitive E2 decision sequence from a Lean git history.

The hash commits only to ordered ``[declaration, edit-kind, build-outcome]``
triples. Commit ids, messages, whitespace, and tactic text are deliberately not
part of the identity endpoint.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import re
import shutil
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import Callable, Iterable, Sequence


EDIT_KINDS = frozenset({
    "add-decl", "modify-body", "modify-signature", "add-import",
    "remove-decl", "sorry-introduced", "sorry-removed",
})
BUILD_OUTCOMES = frozenset({"success", "error", "sorry-present"})

_DECL_RE = re.compile(
    r"(?m)^[ \t]*(?P<mods>(?:(?:private|protected|noncomputable|unsafe|partial)\s+)*)"
    r"(?P<kind>theorem|lemma|def|abbrev|opaque|axiom|structure|class|inductive|"
    r"coinductive|instance)\s+(?P<name>[^\s:{(\[]+)"
)
_IMPORT_RE = re.compile(r"(?m)^[ \t]*import[ \t]+(?P<modules>[^\n]+)$")
_SORRY_RE = re.compile(r"\b(?:sorry|admit)\b")


class TraceError(RuntimeError):
    """A history cannot be represented faithfully by the registered schema."""


@dataclass(frozen=True)
class Declaration:
    name: str
    position: int
    signature: str
    body: str
    private: bool

    @property
    def has_sorry(self) -> bool:
        return bool(_SORRY_RE.search(self.body))


@dataclass(frozen=True)
class Edit:
    path: str
    position: int
    declaration: str
    kind: str


@dataclass(frozen=True)
class CommitEdits:
    commit: str
    edits: tuple[Edit, ...]
    lean_paths: tuple[str, ...]


def _run(args: Sequence[str], *, cwd: Path, check: bool = True) -> subprocess.CompletedProcess[str]:
    result = subprocess.run(
        list(args), cwd=cwd, text=True, stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT, check=False,
    )
    if check and result.returncode != 0:
        raise TraceError(f"command failed ({result.returncode}): {' '.join(args)}\n{result.stdout}")
    return result


def _git(repo: Path, *args: str, check: bool = True) -> subprocess.CompletedProcess[str]:
    # The staged tree belongs to the isolated account and is read from the
    # operator's side, so git's dubious-ownership guard would refuse it.  Scope
    # the exemption to this one repository with -c; never write a git config.
    return _run(
        ("git", "-c", "core.hooksPath=/dev/null",
         "-c", f"safe.directory={Path(repo).resolve()}", *args),
        cwd=repo, check=check,
    )


def _masked(source: str) -> str:
    """Replace Lean comments and strings with spaces while preserving offsets."""
    out = list(source)
    i = 0
    block_depth = 0
    in_string = False
    while i < len(source):
        if block_depth:
            if source.startswith("/-", i):
                out[i:i + 2] = "  "
                block_depth += 1
                i += 2
            elif source.startswith("-/", i):
                out[i:i + 2] = "  "
                block_depth -= 1
                i += 2
            else:
                if source[i] != "\n":
                    out[i] = " "
                i += 1
        elif in_string:
            if source[i] == "\\" and i + 1 < len(source):
                if source[i] != "\n":
                    out[i] = " "
                if source[i + 1] != "\n":
                    out[i + 1] = " "
                i += 2
            else:
                if source[i] == '"':
                    in_string = False
                if source[i] != "\n":
                    out[i] = " "
                i += 1
        elif source.startswith("/-", i):
            out[i:i + 2] = "  "
            block_depth = 1
            i += 2
        elif source.startswith("--", i):
            end = source.find("\n", i)
            end = len(source) if end < 0 else end
            out[i:end] = " " * (end - i)
            i = end
        elif source[i] == '"':
            out[i] = " "
            in_string = True
            i += 1
        else:
            i += 1
    return "".join(out)


def _normalized(text: str) -> str:
    return " ".join(text.split())


def _body_boundary(chunk: str) -> int:
    """Find a top-level declaration body marker in already-masked text."""
    pairs = {")": "(", "]": "[", "}": "{"}
    stack: list[str] = []
    i = 0
    while i < len(chunk):
        char = chunk[i]
        if char in "([{":
            stack.append(char)
        elif char in pairs and stack and stack[-1] == pairs[char]:
            stack.pop()
        elif not stack and chunk.startswith(":=", i):
            return i
        i += 1
    where = re.search(r"(?m)^[ \t]*where\b", chunk)
    return where.start() if where else len(chunk)


def parse_declarations(source: str) -> list[Declaration]:
    masked = _masked(source)
    matches = list(_DECL_RE.finditer(masked))
    declarations: list[Declaration] = []
    for index, match in enumerate(matches):
        end = matches[index + 1].start() if index + 1 < len(matches) else len(source)
        raw_chunk = source[match.start():end]
        masked_chunk = masked[match.start():end]
        boundary = _body_boundary(masked_chunk)
        declarations.append(Declaration(
            name=match.group("name"),
            position=match.start(),
            signature=_normalized(masked_chunk[:boundary]),
            body=_normalized(masked_chunk[boundary:]),
            private="private" in match.group("mods").split(),
        ))
    names = [decl.name for decl in declarations]
    if len(names) != len(set(names)):
        repeated = sorted({name for name in names if names.count(name) > 1})
        raise TraceError(f"ambiguous duplicate declaration names: {', '.join(repeated)}")
    return declarations


def parse_imports(source: str) -> list[tuple[str, int]]:
    masked = _masked(source)
    imports: list[tuple[str, int]] = []
    for match in _IMPORT_RE.finditer(masked):
        for module in match.group("modules").split():
            imports.append((module, match.start()))
    return imports


def _show(repo: Path, revision: str, path: str) -> str:
    result = _git(repo, "show", f"{revision}:{path}", check=False)
    return result.stdout if result.returncode == 0 else ""


def structural_edits(path: str, before: str, after: str) -> list[Edit]:
    old_decls = {decl.name: decl for decl in parse_declarations(before)}
    new_decls = {decl.name: decl for decl in parse_declarations(after)}
    edits: list[Edit] = []

    old_imports = {module: pos for module, pos in parse_imports(before)}
    new_imports = {module: pos for module, pos in parse_imports(after)}
    removed_imports = sorted(set(old_imports) - set(new_imports))
    if removed_imports:
        raise TraceError(
            "the registered edit vocabulary has no remove-import category: "
            + ", ".join(removed_imports)
        )
    for module in set(new_imports) - set(old_imports):
        edits.append(Edit(path, new_imports[module], f"import:{module}", "add-import"))

    for name in set(old_decls) | set(new_decls):
        old = old_decls.get(name)
        new = new_decls.get(name)
        if old is None:
            assert new is not None
            edits.append(Edit(path, new.position, name, "add-decl"))
        elif new is None:
            edits.append(Edit(path, old.position, name, "remove-decl"))
        elif old.signature != new.signature or old.body != new.body:
            if not old.has_sorry and new.has_sorry:
                kind = "sorry-introduced"
            elif old.has_sorry and not new.has_sorry:
                kind = "sorry-removed"
            elif old.signature != new.signature:
                kind = "modify-signature"
            else:
                kind = "modify-body"
            edits.append(Edit(path, new.position, name, kind))
    return sorted(edits, key=lambda edit: (edit.position, edit.declaration, edit.kind))


def commit_range(repo: Path, base: str, head: str) -> list[str]:
    _git(repo, "rev-parse", "--verify", f"{base}^{{commit}}")
    _git(repo, "rev-parse", "--verify", f"{head}^{{commit}}")
    merge = _git(repo, "rev-list", "--min-parents=2", f"{base}..{head}").stdout.strip()
    if merge:
        raise TraceError("merge commits are not a canonical attempt sequence")
    output = _git(repo, "rev-list", "--reverse", "--first-parent", f"{base}..{head}").stdout
    commits = output.split()
    if not commits:
        raise TraceError("commit range contains no attempts")
    return commits


def edits_for_commit(repo: Path, commit: str) -> CommitEdits:
    parent_line = _git(repo, "rev-list", "--parents", "-n", "1", commit).stdout.split()
    if len(parent_line) != 2:
        raise TraceError(f"attempt commit must have exactly one parent: {commit}")
    parent = parent_line[1]
    changed_paths = _git(
        repo, "diff", "--name-only", "--no-renames", parent, commit,
    ).stdout.splitlines()
    unsupported = [path for path in changed_paths if not path.endswith(".lean")]
    if unsupported:
        raise TraceError(
            "attempt changes files outside the registered Lean trace domain: "
            + ", ".join(sorted(unsupported))
        )
    paths = _git(
        repo, "diff", "--name-only", "--no-renames", "--diff-filter=ADM",
        parent, commit, "--", "*.lean",
    ).stdout.splitlines()
    edits: list[Edit] = []
    lean_paths: list[str] = []
    for path in sorted(paths):
        before = _show(repo, parent, path)
        after = _show(repo, commit, path)
        file_edits = structural_edits(path, before, after)
        if file_edits:
            edits.extend(file_edits)
        if after:
            lean_paths.append(path)
    if not edits:
        raise TraceError(f"attempt commit has no representable Lean structural edit: {commit}")
    return CommitEdits(commit, tuple(edits), tuple(lean_paths))


def _source_has_sorry(worktree: Path, paths: Iterable[str]) -> bool:
    for path in paths:
        source = (worktree / path).read_text(encoding="utf-8")
        if _SORRY_RE.search(_masked(source)):
            return True
    return False


def _axiom_probe(worktree: Path, path: str) -> tuple[int, str]:
    source_path = worktree / path
    source = source_path.read_text(encoding="utf-8")
    public_names = [decl.name for decl in parse_declarations(source) if not decl.private]
    suffix = "\n" + "\n".join(f"#print axioms {name}" for name in public_names) + "\n"
    probe = source_path.with_name(f".{source_path.stem}.e2-axioms.lean")
    probe.write_text(source + suffix, encoding="utf-8")
    try:
        result = _run(("lake", "env", "lean", str(probe.relative_to(worktree))),
                      cwd=worktree, check=False)
        return result.returncode, result.stdout
    finally:
        probe.unlink(missing_ok=True)


class LeanOutcomeEvaluator:
    """Rebuild commits in an isolated clone; never trust runner-authored receipts."""

    def __init__(self, source_repo: Path):
        self.source_repo = source_repo.resolve()
        self._temporary = tempfile.TemporaryDirectory(prefix="e2-decision-trace-")
        self.worktree = Path(self._temporary.name) / "repo"
        # `git clone <local path>` runs upload-pack as a child that does not
        # inherit the scoped safe.directory exemption, so cloning the isolated
        # account's tree directly is refused however the exemption is passed.
        # Bundle the history out with a single git process — which does honour
        # it — and clone from the operator-owned bundle instead.  The bundle
        # carries the baseline tag the extractor resolves --base against.
        bundle = Path(self._temporary.name) / "history.bundle"
        bundled = _run(
            ("git", "-c", "core.hooksPath=/dev/null",
             "-c", f"safe.directory={self.source_repo}",
             "bundle", "create", str(bundle), "--all"),
            cwd=self.source_repo, check=False,
        )
        if bundled.returncode != 0:
            self.close()
            raise TraceError(f"could not bundle the staged history:\n{bundled.stdout}")
        result = subprocess.run(
            ["git", "-c", "core.hooksPath=/dev/null", "clone", "--quiet",
             "--no-checkout", str(bundle), str(self.worktree)],
            text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, check=False,
        )
        if result.returncode != 0:
            self.close()
            raise TraceError(f"isolated clone failed:\n{result.stdout}")
        lake_cache = self.source_repo / ".lake"
        if lake_cache.is_dir() and not (self.worktree / ".lake").exists():
            (self.worktree / ".lake").symlink_to(lake_cache, target_is_directory=True)

    def close(self) -> None:
        self._temporary.cleanup()

    def __enter__(self) -> "LeanOutcomeEvaluator":
        return self

    def __exit__(self, *_: object) -> None:
        self.close()

    def __call__(self, commit: CommitEdits) -> str:
        checkout = _git(self.worktree, "checkout", "--quiet", "--detach", commit.commit,
                        check=False)
        if checkout.returncode != 0:
            return "error"
        saw_sorry = _source_has_sorry(self.worktree, commit.lean_paths)
        if not commit.lean_paths:
            result = _run(("lake", "build"), cwd=self.worktree, check=False)
            return "error" if result.returncode else ("sorry-present" if saw_sorry else "success")
        for path in commit.lean_paths:
            exit_code, output = _axiom_probe(self.worktree, path)
            if exit_code != 0:
                return "error"
            if "sorryAx" in output or "declaration uses 'sorry'" in output:
                saw_sorry = True
        return "sorry-present" if saw_sorry else "success"


def canonical_bytes(sequence: Sequence[Sequence[str]]) -> bytes:
    return json.dumps(sequence, ensure_ascii=False, separators=(",", ":")).encode("utf-8")


def extract_trace(
    repo: Path,
    base: str,
    head: str = "HEAD",
    *,
    outcome_provider: Callable[[CommitEdits], str] | None = None,
) -> dict:
    repo = repo.resolve()
    commits = [edits_for_commit(repo, commit) for commit in commit_range(repo, base, head)]
    owned_evaluator: LeanOutcomeEvaluator | None = None
    if outcome_provider is None:
        owned_evaluator = LeanOutcomeEvaluator(repo)
        outcome_provider = owned_evaluator
    try:
        sequence: list[list[str]] = []
        for commit in commits:
            outcome = outcome_provider(commit)
            if outcome not in BUILD_OUTCOMES:
                raise TraceError(f"invalid build outcome: {outcome}")
            for edit in commit.edits:
                if edit.kind not in EDIT_KINDS:
                    raise TraceError(f"invalid edit kind: {edit.kind}")
                sequence.append([edit.declaration, edit.kind, outcome])
    finally:
        if owned_evaluator is not None:
            owned_evaluator.close()
    payload = canonical_bytes(sequence)
    return {
        "schema": "e2-decision-sequence/v1",
        "attempt-count": len(commits),
        "sequence": sequence,
        "sha256": hashlib.sha256(payload).hexdigest(),
    }


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("run_dir", type=Path, help="git repository containing one E2 run")
    parser.add_argument("--base", required=True,
                        help="baseline commit excluded from the attempt sequence")
    parser.add_argument("--head", default="HEAD", help="last attempt commit (default: HEAD)")
    parser.add_argument("--output", type=Path, help="write canonical JSON here")
    return parser


def main(argv: Sequence[str] | None = None) -> int:
    args = _parser().parse_args(argv)
    try:
        trace = extract_trace(args.run_dir, args.base, args.head)
    except TraceError as error:
        print(f"e2-decision-trace: {error}", file=sys.stderr)
        return 2
    rendered = json.dumps(trace, sort_keys=True, separators=(",", ":")) + "\n"
    if args.output:
        args.output.write_text(rendered, encoding="utf-8")
    else:
        sys.stdout.write(rendered)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
