#!/usr/bin/env python3
"""Mechanical check of task-link batches (Giraffe population work).

Usage: python3 -I check_edges.py batch-NN.jsonl [more.jsonl ...]

Each line is one record for one population member (members.json):
  {"id": "E-...",                       # must be a member
   "source": {"path": ..., "sha256": ...},  # must equal the member's pin
   "status_as_read": "open|done-unmarked|abandoned|unclear",
   "purpose": "one or two sentences",
   "edges": [ EDGE, ... ],              # required; [] means "read, no links"
   "unresolved": [ UNRESOLVED, ... ]}   # required; [] if none

  EDGE       = {"to": "M-...|E-...|T-...", "kind": KIND, "basis": "declared|inferred",
                "evidence": EVIDENCE, "note": "..."}
  UNRESOLVED = {"raw_target": "text as written", "kind": KIND, "basis": ...,
                "evidence": EVIDENCE, "reason": "why it does not resolve"}
  EVIDENCE   = {"path": absolute file path, "sha256": sha256 of that file,
                "line": 1-based line where the quote starts,
                "quote": verbatim text, >= 12 chars, within lines line..line+5}

The six-line window matches Giraffe's quote-at-line? rule.
Checks: member binding, file pins, quote window, target resolution, vocabulary,
one record per member across all files given.
Prints one line per problem and a summary; exit 1 if any problem.
"""
import hashlib, json, os, re, sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
KINDS = {"depends-on", "unblocks", "part-of", "advances", "relates-to",
         "duplicates", "superseded-by"}
BASES = {"declared", "inferred"}
STATUS = {"open", "done-unmarked", "abandoned", "unclear"}
CONFIGURED_CODE_ROOT = os.environ.get("GIRAFFE_CHECK_CODE_ROOT")
CODE = Path(CONFIGURED_CODE_ROOT or (Path.home() / "code"))
CANONICAL_CODE = Path("/home/joe/code").resolve()
WINDOW = 6


def index_ids():
    ids = {}
    for repo in CODE.iterdir():
        # canonical checkouts only: worktree copies have a .git file, not a dir
        if not (repo / ".git").is_dir() or not (repo / "holes").is_dir():
            continue
        # task documents live both in holes/<kind>/ and directly in holes/
        for f in (repo / "holes").rglob("*.md"):
            if re.match(r"^[MET]-", f.stem):
                ids.setdefault(f.stem, str(f))
    return ids


def norm(s):
    return re.sub(r"\s+", " ", s).strip()


def local(p):
    """Relocate canonical corpus pins only when an explicit code root exists."""
    path = Path(p)
    if not CONFIGURED_CODE_ROOT:
        return path
    try:
        relative = path.resolve().relative_to(CANONICAL_CODE)
    except ValueError:
        return path
    return CODE / relative


def sha(p):
    return hashlib.sha256(local(p).read_bytes()).hexdigest()


def main(paths):
    members = {m["id"]: m for m in json.loads((HERE / "members.json").read_text())}
    ids = index_ids()
    problems, seen = [], {}
    records = edges = unresolved = 0
    lines_cache, sha_cache = {}, {}

    def file_lines(p):
        if p not in lines_cache:
            try:
                lines_cache[p] = local(p).read_text(errors="replace").splitlines()
            except OSError:
                lines_cache[p] = None
        return lines_cache[p]

    def file_sha(p):
        if p not in sha_cache:
            sha_cache[p] = sha(p) if local(p).is_file() else None
        return sha_cache[p]

    def check_evidence(tag, ev):
        p = ev.get("path", "")
        if not Path(p).is_absolute():
            problems.append(f"{tag} evidence path must be absolute")
            return
        lines = file_lines(p)
        if lines is None:
            problems.append(f"{tag} evidence file missing: {p}")
            return
        if ev.get("sha256") != file_sha(p):
            problems.append(f"{tag} evidence sha256 does not match the file")
        line = ev.get("line")
        q = norm(ev.get("quote", ""))
        if not isinstance(line, int) or line < 1 or line > len(lines):
            problems.append(f"{tag} evidence line missing or out of range")
        elif len(q) < 12 or q not in norm(" ".join(lines[line - 1:line - 1 + WINDOW])):
            problems.append(f"{tag} quote not found within lines {line}..{line + WINDOW - 1}")

    def check_vocab(tag, x):
        if x.get("kind") not in KINDS:
            problems.append(f"{tag} kind {x.get('kind')!r} not allowed")
        if x.get("basis") not in BASES:
            problems.append(f"{tag} basis {x.get('basis')!r} not allowed")

    for batch in paths:
        for n, line in enumerate(Path(batch).read_text().splitlines(), 1):
            if not line.strip():
                continue
            where = f"{batch}:{n}"
            try:
                r = json.loads(line)
            except json.JSONDecodeError as e:
                problems.append(f"{where} bad json: {e}")
                continue
            records += 1
            rid = r.get("id")
            m = members.get(rid)
            if m is None:
                problems.append(f"{where} {rid} is not a population member")
            else:
                src = r.get("source", {})
                if src.get("path") != m["path"] or src.get("sha256") != m["sha256"]:
                    problems.append(f"{where} {rid} source does not match the member pin")
                elif file_sha(m["path"]) != m["sha256"]:
                    problems.append(f"{where} {rid} source file changed since the census")
            if rid in seen:
                problems.append(f"{where} {rid} duplicate record (also {seen[rid]})")
            seen[rid] = where
            if r.get("status_as_read") not in STATUS:
                problems.append(f"{where} {rid} status_as_read not in {sorted(STATUS)}")
            for field in ("edges", "unresolved"):
                if not isinstance(r.get(field), list):
                    problems.append(f"{where} {rid} {field} must be a list ([] if none)")
            for e in r.get("edges") or []:
                edges += 1
                tag = f"{where} {rid} -> {e.get('to')}"
                check_vocab(tag, e)
                if e.get("to") not in ids:
                    problems.append(f"{tag} target id does not resolve (use unresolved)")
                if e.get("to") == rid:
                    problems.append(f"{tag} self-link")
                check_evidence(tag, e.get("evidence", {}))
            for u in r.get("unresolved") or []:
                unresolved += 1
                tag = f"{where} {rid} -> unresolved {u.get('raw_target')!r}"
                check_vocab(tag, u)
                if not u.get("raw_target") or not u.get("reason"):
                    problems.append(f"{tag} needs raw_target and reason")
                check_evidence(tag, u.get("evidence", {}))
    for p in problems:
        print(p)
    print(f"checked {records} records, {edges} edges, {unresolved} unresolved, "
          f"{len(problems)} problems")
    return 1 if problems else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))
