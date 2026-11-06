#!/usr/bin/env python3
"""Mechanical check of combined Giraffe link and placement readings.

Usage: python3 -I check_edges-v2.py batch-NN.jsonl [more.jsonl ...]

Each line is one record for one population member (members.json):
  {"id": "E-...",                       # must be a member
   "source": {"path": ..., "sha256": ...},  # must equal the member's pin
   "status_as_read": "open|done-unmarked|abandoned|unclear",
   "purpose": "one or two sentences",
   "edges": [ EDGE, ... ],              # required; [] means "read, no links"
   "unresolved": [ UNRESOLVED, ... ],   # required; [] if none
   "placement": PLACEMENT}              # required

  EDGE       = {"to": "M-...|E-...|T-...", "kind": KIND, "basis": "declared|inferred",
                "evidence": EVIDENCE, "note": "..."}
  UNRESOLVED = {"raw_target": "text as written", "kind": KIND, "basis": ...,
                "evidence": EVIDENCE, "reason": "why it does not resolve"}
  PLACEMENT  = {"what": "one sentence", "status": PLACEMENT_STATUS,
                "ways": [WAY, ...], "wing": WING, "action": ACTION,
                "need_goes_to": "item id or solved" | null,
                "finish": "what done looks like", "beneficiary": BENEFICIARY,
                "confidence": CONFIDENCE, "evidence": [EVIDENCE, EVIDENCE, ...]}
  EVIDENCE   = {"path": absolute file path, "sha256": sha256 of that file,
                "line": 1-based line where the quote starts,
                "quote": verbatim text, >= 12 chars, within lines line..line+5}

The placement evidence list has at least two citations into the member source:
one for its way and one for its finish. The six-line window matches Giraffe's
quote-at-line? rule.
"""
import hashlib
import json
import re
import sys
from pathlib import Path

HERE = Path(__file__).resolve().parent
KINDS = {"depends-on", "unblocks", "part-of", "advances", "relates-to",
         "duplicates", "superseded-by"}
BASES = {"declared", "inferred"}
STATUS = {"open", "done-unmarked", "abandoned", "unclear"}
VOCAB = json.loads((HERE / "placement-vocabularies.json").read_text())
WAYS_PATH = (HERE / "ways-of-working.json"
             if (HERE / "ways-of-working.json").is_file()
             else HERE.parents[1] / "holes" / "excursions" / "ways-of-working.json")
WAYS_DECL = json.loads(WAYS_PATH.read_text())
WAYS = {way["id"] for way in WAYS_DECL["ways"]}
WINGS = set(VOCAB["wings"])
ACTIONS = set(VOCAB["actions"])
ACTION_PREFIXES = tuple(VOCAB["action_prefixes"])
NEED_ACTIONS = set(VOCAB["need_goes_to_actions"])
NEED_PREFIXES = tuple(VOCAB["need_goes_to_prefixes"])
PLACEMENT_STATUS = set(VOCAB["statuses"])
BENEFICIARIES = set(VOCAB["beneficiaries"])
CONFIDENCE = set(VOCAB["confidence_levels"])
CODE = Path.home() / "code"
WINDOW = 6


def index_ids():
    ids = {}
    for repo in CODE.iterdir():
        if not (repo / ".git").is_dir() or not (repo / "holes").is_dir():
            continue
        for f in (repo / "holes").rglob("*.md"):
            if re.match(r"^[MET]-", f.stem):
                ids.setdefault(f.stem, str(f))
    return ids


def norm(s):
    return re.sub(r"\s+", " ", s).strip()


def sha(path):
    return hashlib.sha256(Path(path).read_bytes()).hexdigest()


def allowed_action(action):
    return action in ACTIONS or (isinstance(action, str)
                                 and action.startswith(ACTION_PREFIXES)
                                 and len(action.split(":", 1)[-1].strip()) > 0)


def needs_destination(action):
    return action in NEED_ACTIONS or (isinstance(action, str)
                                      and action.startswith(NEED_PREFIXES))


def main(paths):
    members = {m["id"]: m for m in json.loads((HERE / "members.json").read_text())}
    ids = index_ids()
    problems, seen = [], {}
    records = edges = unresolved = 0
    lines_cache, sha_cache = {}, {}

    def file_lines(path):
        if path not in lines_cache:
            try:
                lines_cache[path] = Path(path).read_text(errors="replace").splitlines()
            except OSError:
                lines_cache[path] = None
        return lines_cache[path]

    def file_sha(path):
        if path not in sha_cache:
            sha_cache[path] = sha(path) if Path(path).is_file() else None
        return sha_cache[path]

    def check_evidence(tag, evidence, required_path=None):
        if not isinstance(evidence, dict):
            problems.append(f"{tag} evidence must be an object")
            return
        path = evidence.get("path", "")
        if not Path(path).is_absolute():
            problems.append(f"{tag} evidence path must be absolute")
            return
        if required_path is not None and path != required_path:
            problems.append(f"{tag} placement evidence must cite the member source")
        lines = file_lines(path)
        if lines is None:
            problems.append(f"{tag} evidence file missing: {path}")
            return
        if evidence.get("sha256") != file_sha(path):
            problems.append(f"{tag} evidence sha256 does not match the file")
        line = evidence.get("line")
        quote = norm(evidence.get("quote", ""))
        if not isinstance(line, int) or line < 1 or line > len(lines):
            problems.append(f"{tag} evidence line missing or out of range")
        elif len(quote) < 12 or quote not in norm(" ".join(lines[line - 1:line - 1 + WINDOW])):
            problems.append(f"{tag} quote not found within lines {line}..{line + WINDOW - 1}")

    def check_link_vocab(tag, value):
        if value.get("kind") not in KINDS:
            problems.append(f"{tag} kind {value.get('kind')!r} not allowed")
        if value.get("basis") not in BASES:
            problems.append(f"{tag} basis {value.get('basis')!r} not allowed")

    def check_placement(where, rid, placement, source):
        tag = f"{where} {rid} placement"
        if not isinstance(placement, dict):
            problems.append(f"{tag} missing or not an object")
            return
        what = placement.get("what")
        if not isinstance(what, str) or not what.strip():
            problems.append(f"{tag} what must be a non-empty sentence")
        if placement.get("status") not in PLACEMENT_STATUS:
            problems.append(f"{tag} status {placement.get('status')!r} not allowed")
        ways = placement.get("ways")
        if not isinstance(ways, list) or len(ways) > 2:
            problems.append(f"{tag} ways must be a list of at most two values")
        elif len(set(ways)) != len(ways):
            problems.append(f"{tag} ways must not contain duplicates")
        else:
            for way in ways:
                if way not in WAYS:
                    problems.append(f"{tag} way {way!r} not allowed")
        if placement.get("wing") not in WINGS:
            problems.append(f"{tag} wing {placement.get('wing')!r} not allowed")
        action = placement.get("action")
        if not allowed_action(action):
            problems.append(f"{tag} action {action!r} not allowed")
        destination = placement.get("need_goes_to")
        if needs_destination(action) and not (isinstance(destination, str) and destination.strip()):
            problems.append(f"{tag} need_goes_to is required for action {action!r}")
        if not isinstance(placement.get("finish"), str) or not placement["finish"].strip():
            problems.append(f"{tag} finish must be non-empty")
        if placement.get("beneficiary") not in BENEFICIARIES:
            problems.append(f"{tag} beneficiary {placement.get('beneficiary')!r} not allowed")
        if placement.get("confidence") not in CONFIDENCE:
            problems.append(f"{tag} confidence {placement.get('confidence')!r} not allowed")
        evidence = placement.get("evidence")
        if not isinstance(evidence, list) or len(evidence) < 2:
            problems.append(f"{tag} evidence must contain way and finish citations")
        else:
            for number, citation in enumerate(evidence, 1):
                check_evidence(f"{tag} evidence {number}", citation, source.get("path"))

    for batch in paths:
        for number, line in enumerate(Path(batch).read_text().splitlines(), 1):
            if not line.strip():
                continue
            where = f"{batch}:{number}"
            try:
                record = json.loads(line)
            except json.JSONDecodeError as error:
                problems.append(f"{where} bad json: {error}")
                continue
            records += 1
            rid = record.get("id")
            member = members.get(rid)
            source = record.get("source", {})
            if member is None:
                problems.append(f"{where} {rid} is not a population member")
            else:
                if source.get("path") != member["path"] or source.get("sha256") != member["sha256"]:
                    problems.append(f"{where} {rid} source does not match the member pin")
                elif file_sha(member["path"]) != member["sha256"]:
                    problems.append(f"{where} {rid} source file changed since the census")
            if rid in seen:
                problems.append(f"{where} {rid} duplicate record (also {seen[rid]})")
            seen[rid] = where
            if record.get("status_as_read") not in STATUS:
                problems.append(f"{where} {rid} status_as_read not in {sorted(STATUS)}")
            for field in ("edges", "unresolved"):
                if not isinstance(record.get(field), list):
                    problems.append(f"{where} {rid} {field} must be a list ([] if none)")
            for edge in record.get("edges") or []:
                edges += 1
                tag = f"{where} {rid} -> {edge.get('to')}"
                check_link_vocab(tag, edge)
                if edge.get("to") not in ids:
                    problems.append(f"{tag} target id does not resolve (use unresolved)")
                if edge.get("to") == rid:
                    problems.append(f"{tag} self-link")
                check_evidence(tag, edge.get("evidence", {}))
            for item in record.get("unresolved") or []:
                unresolved += 1
                tag = f"{where} {rid} -> unresolved {item.get('raw_target')!r}"
                check_link_vocab(tag, item)
                if not item.get("raw_target") or not item.get("reason"):
                    problems.append(f"{tag} needs raw_target and reason")
                check_evidence(tag, item.get("evidence", {}))
            check_placement(where, rid, record.get("placement"), source)
    for problem in problems:
        print(problem)
    print(f"checked {records} records, {edges} edges, {unresolved} unresolved, "
          f"{len(problems)} problems")
    return 1 if problems else 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
