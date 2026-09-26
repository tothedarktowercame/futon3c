#!/usr/bin/env python3
"""Re-derive the machine-checkable fields of codex-sorry-queue.edn from the repo.

Written 2026-08-01 by claude-9, prompted by claude-2's finding that three
preregistration fields had semantics stated only in docstrings and enforced by
nothing. The same was true here: every gate field on a queue row
(:sorries-after, :status :resolved, :line, :autoimplicit-gate) is ASSERTED by
whoever wrote the row and checked by nothing. Most of this session's ground-
control errors were of exactly that shape -- stale :dispatched statuses, a row
claimed updated that was not, duplicate rows disagreeing about status.

This does not verify proofs. It checks that the row agrees with the repository:

  D1  :status :resolved      => the file has zero holes at committed HEAD
  D2  :sorries-after N       => the file has N holes at committed HEAD
  D3  :line [a b ...]        => those are the actual hole lines at HEAD
  D4  :last-commit SHA       => SHA exists in git
  D5  :file PATH             => PATH exists at HEAD
  D6  duplicate rows         => two rows for one :file must not both be live
  D7  :status :dispatched    => its :job-id must not be in a terminal state

Holes are counted with Lean comments stripped (nested block comments and line
comments), matching the harness gate's semantics -- a naive grep counts the
word "sorry" in docstrings and over-reports. Counting is against
`git show HEAD:<path>`, never the working tree, because runners hold files open.

Usage:  python3 scripts/queue_audit.py [--repo /home/joe/code/apm-lean]
Exit 0 if no disagreements, 1 otherwise.
"""

from __future__ import annotations

import argparse
import json
import re
import subprocess
import sys
import urllib.request
from pathlib import Path

QUEUE = Path(__file__).resolve().parent.parent / "data" / "codex-sorry-queue.edn"
AGENCY = "http://localhost:7070"
TERMINAL = {"done", "failed", "cancelled"}


def top_level_maps(text: str) -> list[str]:
    """Split the EDN vector into top-level {...} rows, respecting strings."""
    rows, depth, start, i, in_str = [], 0, None, 0, False
    while i < len(text):
        c = text[i]
        if in_str:
            if c == "\\":
                i += 2
                continue
            if c == '"':
                in_str = False
        else:
            if c == '"':
                in_str = True
            elif c == "{":
                if depth == 0:
                    start = i
                depth += 1
            elif c == "}":
                depth -= 1
                if depth == 0:
                    rows.append(text[start : i + 1])
        i += 1
    return rows


def strip_comments(src: str) -> str:
    """Blank out Lean line comments and nested block comments.

    Comment characters become spaces and NEWLINES ARE PRESERVED, so the result
    is line-for-line aligned with the input. (Deleting the newlines instead
    silently shifts every reported line number -- this checker had that bug on
    its first run and reported two false stale-`:line` findings.)
    """
    out, i, depth = [], 0, 0
    while i < len(src):
        c = src[i]
        if src.startswith("/-", i):
            depth += 1
            out.append("  ")
            i += 2
            continue
        if src.startswith("-/", i) and depth:
            depth -= 1
            out.append("  ")
            i += 2
            continue
        if depth == 0 and src.startswith("--", i):
            nl = src.find("\n", i)
            end = len(src) if nl < 0 else nl
            out.append(" " * (end - i))
            i = end
            continue
        out.append(c if depth == 0 else ("\n" if c == "\n" else " "))
        i += 1
    return "".join(out)


SORRY = re.compile(r"(?<![\w.])sorry(?![\w])")
OPAQUE = re.compile(r"^\s*opaque [^\n]*$", re.M)


def hole_lines(src: str) -> list[int]:
    """1-indexed lines carrying an executable hole, comments excluded.

    `opaque` without `:=` counts as a hole -- it states a constant exists with
    no definition, which is a `sorry` wearing a different hat (a96A02, 07-31).
    """
    blanked = strip_comments(src).split("\n")
    hits = []
    for n, clean in enumerate(blanked, start=1):
        if SORRY.search(clean):
            hits.append(n)
        elif OPAQUE.match(clean) and ":=" not in clean:
            hits.append(n)
    return hits


def git(repo: Path, *args: str) -> tuple[int, str]:
    p = subprocess.run(["git", *args], cwd=repo, capture_output=True, text=True)
    return p.returncode, p.stdout


def field(row: str, key: str) -> str | None:
    m = re.search(
        r'(?<![\w-]):' + key + r'(?![\w-])\s*("(?:[^"\\]|\\.)*"|nil|:[\w-]+|-?\d+|\[[^\]]*\])',
        row,
    )
    return m.group(1) if m else None


def unquote(v: str | None) -> str | None:
    if v is None or v == "nil":
        return None
    return v[1:-1] if v.startswith('"') else v


def job_state(job_id: str) -> str | None:
    try:
        with urllib.request.urlopen(
            f"{AGENCY}/api/alpha/invoke/jobs/{job_id}", timeout=5
        ) as r:
            return (json.load(r).get("job") or {}).get("state")
    except Exception:
        return None


LIVE = {":dispatched", ":partial-continue", ":untouched"}


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--repo", default="/home/joe/code/apm-lean", type=Path)
    ap.add_argument("--no-jobs", action="store_true", help="skip D7 (no Agency calls)")
    args = ap.parse_args()

    text = QUEUE.read_text()
    rows = top_level_maps(text)
    print(f"queue: {len(rows)} rows")

    rc, head = git(args.repo, "rev-parse", "HEAD")
    if rc:
        print("FATAL: not a git repo", file=sys.stderr)
        return 2
    print(f"repo:  {args.repo} @ {head.strip()[:8]} (committed state, not working tree)")

    tracked = set(git(args.repo, "ls-tree", "-r", "--name-only", "HEAD")[1].split("\n"))
    findings: list[str] = []
    by_file: dict[str, list[tuple[str, str]]] = {}
    cache: dict[str, list[int]] = {}

    for row in rows:
        rid = unquote(field(row, "id")) or "?"
        status = field(row, "status") or "?"
        path = unquote(field(row, "file"))
        if path:
            by_file.setdefault(path, []).append((rid, status))

        sha = unquote(field(row, "last-commit"))
        if sha and git(args.repo, "cat-file", "-e", f"{sha}^{{commit}}")[0]:
            findings.append(f"D4 {rid}: :last-commit {sha[:12]} is not a commit in this repo")

        if not path:
            continue
        if path not in tracked:
            findings.append(f"D5 {rid}: :file {path} does not exist at HEAD")
            continue

        if path not in cache:
            cache[path] = hole_lines(git(args.repo, "show", f"HEAD:{path}")[1])
        holes = cache[path]

        if status == ":resolved" and holes:
            findings.append(
                f"D1 {rid}: :status :resolved but {path} has {len(holes)} hole(s) "
                f"at HEAD, lines {holes}"
            )

        after = field(row, "sorries-after")
        if after is not None and after.lstrip("-").isdigit() and int(after) != len(holes):
            findings.append(
                f"D2 {rid}: :sorries-after {after} but {path} has {len(holes)} at HEAD"
            )

        line = field(row, "line")
        if line and line.startswith("[") and status in LIVE:
            claimed = [int(x) for x in re.findall(r"-?\d+", line)]
            if claimed and claimed != holes:
                findings.append(
                    f"D3 {rid}: :line {claimed} but actual hole lines at HEAD are {holes}"
                )

        if status == ":dispatched" and not args.no_jobs:
            jid = unquote(field(row, "job-id"))
            if jid:
                st = job_state(jid)
                if st in TERMINAL:
                    findings.append(
                        f"D7 {rid}: :status :dispatched but job {jid} is {st} "
                        f"-- stale status, the result was never written back"
                    )

    for path, entries in sorted(by_file.items()):
        if len(entries) > 1:
            live = [e for e in entries if e[1] != ":superseded-by-working-row"]
            if len(live) > 1:
                findings.append(
                    f"D6 {path}: {len(live)} live rows disagree or duplicate: {live}"
                )

    if findings:
        print(f"\n{len(findings)} DISAGREEMENT(S) between the queue and the repo:\n")
        for f in findings:
            print("  " + f)
        return 1
    print("\nno disagreements: every machine-checkable field matches the repo")
    return 0


if __name__ == "__main__":
    sys.exit(main())
