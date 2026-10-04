#!/usr/bin/env python3
"""Measure record-backed pre/post facts for M-象-cascade D0.

This is deliberately a local, read-only census.  It reads the immutable input
files under ``~/.emacs-graph/session-turn-analysis`` and does not infer facts
from conversational wording except where a fact is explicitly labelled as an
analysis proxy.  Output is JSON so the discovery note's numbers can be checked
or diffed without scraping prose.
"""

from __future__ import annotations

import argparse
import datetime as dt
import glob
import json
import os
import re
import subprocess
from collections import defaultdict
from pathlib import Path


GO_AHEAD_IDS = {
    "editing/approve-as-gate",
    "operator/ratify-the-recommendation-inline",
    "orchestration/lightweight-ack-advance",
    "operator/curt-ok-before-the-real-report",
    "operator/approve-then-ask",
    "operator/in-that-case-proceed",
    "operator/yes-push-it",
}
CORRECTION_IDS = {"apparatus/done-is-observed-running"}
COMMIT_LINE = re.compile(r"(?m)^-\s+([A-Za-z0-9_.-]+)\s+([0-9a-f]{7,40})\s+")


def read_json(path: Path):
    with path.open(encoding="utf-8") as stream:
        return json.load(stream)


def turn_name(path: Path) -> str:
    name = path.name
    return name[: -len(".json.analysis.json")] if name.endswith(".json.analysis.json") else name[: -len(".analysis.json")]


def base_path(root: Path, tid: str) -> Path:
    return root / f"{tid}.json"


def candidate_path(root: Path, tid: str) -> Path | None:
    for path in (root / f"{tid}.candidates.json", root / f"{tid}.json.candidates.json"):
        if path.exists():
            return path
    return None


def fragments(analysis):
    for sentence in analysis.get("sentences", []):
        for index, fragment in enumerate(sentence.get("fragments", [])):
            yield sentence.get("id"), index, fragment


def occurrences(root: Path, ids: set[str]):
    """Return unique turns aligned to IDS, using the census membership rule.

    A candidate file is primary.  Once an id exists as a proposal, xlate.py's
    census also counts a later rationale that explicitly names it; we use the
    same rule and retain the matching fragment(s).
    """
    found = {}
    for analysis_path in sorted(root.glob("*.analysis.json")):
        tid = turn_name(analysis_path)
        analysis = read_json(analysis_path)
        matched_sentence_ids = set()
        cp = candidate_path(root, tid)
        if cp:
            for candidate in read_json(cp).get("candidates", []):
                if candidate.get("id") in ids:
                    matched_sentence_ids.add(candidate.get("fragment"))
        matched = []
        for sentence_id, index, fragment in fragments(analysis):
            rationale = str(fragment.get("rationale", ""))
            if sentence_id in matched_sentence_ids or any(pid in rationale for pid in ids):
                matched.append({"sentence": sentence_id, "index": index, **fragment})
        if matched:
            base = read_json(base_path(root, tid)) if base_path(root, tid).exists() else {}
            found[tid] = {"turn_id": tid, "base": base, "analysis": analysis, "fragments": matched}
    return found


def cited_occurrences(root: Path, ids: set[str]):
    """Turns whose analysis directly cites a library pattern in IDS."""
    found = {}
    for analysis_path in sorted(root.glob("*.analysis.json")):
        tid = turn_name(analysis_path)
        analysis = read_json(analysis_path)
        matched = []
        for sentence_id, index, fragment in fragments(analysis):
            if any(ref.get("id") in ids for ref in fragment.get("pattern_refs", [])):
                matched.append({"sentence": sentence_id, "index": index, **fragment})
        if matched:
            base = read_json(base_path(root, tid)) if base_path(root, tid).exists() else {}
            found[tid] = {"turn_id": tid, "base": base, "analysis": analysis, "fragments": matched}
    return found


def parse_time(value):
    if not value:
        return None
    return dt.datetime.fromisoformat(value.replace("Z", "+00:00"))


def next_turns(all_bases):
    by_session = defaultdict(list)
    for tid, base in all_bases.items():
        when = parse_time(base.get("created_at"))
        if base.get("session_id") and when:
            by_session[base["session_id"]].append((when, tid, base))
    answer = {}
    for rows in by_session.values():
        rows.sort()
        for left, right in zip(rows, rows[1:]):
            answer[left[1]] = right[2]
    return answer


def nonblank(value):
    return isinstance(value, str) and bool(value.strip())


def has_origin(base):
    origin = base.get("origin") or base.get("turn_origin") or {}
    return isinstance(origin, dict) and nonblank(origin.get("kind"))


def commits(summary):
    return COMMIT_LINE.findall(summary or "")


def commits_resolve(summary):
    rows = commits(summary)
    if not rows:
        return False
    for repo, sha in rows:
        checkout = Path("/home/joe/code") / repo
        if not checkout.is_dir():
            return False
        result = subprocess.run(
            ["git", "-C", str(checkout), "cat-file", "-e", f"{sha}^{{commit}}"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
            check=False,
        )
        if result.returncode:
            return False
    return True


def coverage(turns, successors):
    checks = {
        "seat_identity": lambda row, nxt: nonblank(row["base"].get("agent_id")) and nonblank(row["base"].get("session_id")),
        "operator_evidence_id": lambda row, nxt: nonblank(row["base"].get("evidence_id")),
        "interpreted_target": lambda row, nxt: any(nonblank(f.get("target")) for f in row["fragments"]),
        "preceding_work_summary": lambda row, nxt: nonblank(row["base"].get("happened_summary")),
        "preceding_commit": lambda row, nxt: commits_resolve(row["base"].get("happened_summary", "")),
        "structured_turn_origin": lambda row, nxt: has_origin(row["base"]),
        "following_agent_response": lambda row, nxt: bool(nxt and nonblank(nxt.get("happened_summary"))),
        "following_commit": lambda row, nxt: bool(nxt and commits_resolve(nxt.get("happened_summary", ""))),
        "following_park_or_promise": lambda row, nxt: bool(nxt and re.search(r"(?i)\b(?:park|promise)[-:/ ]", nxt.get("happened_summary", ""))),
    }
    covered = {}
    for name, check in checks.items():
        ids = [tid for tid, row in turns.items() if check(row, successors.get(tid))]
        if ids:  # D0's bad-case rule: never advertise a zero-coverage fact.
            covered[name] = {"covered": len(ids), "turns": ids}
    return covered


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--records", default="~/.emacs-graph/session-turn-analysis")
    args = parser.parse_args()
    root = Path(os.path.expanduser(args.records))
    all_bases = {}
    for path in root.glob("turn-*.json"):
        if re.fullmatch(r"turn-[^.]+\.json", path.name):
            try:
                all_bases[path.stem] = read_json(path)
            except (OSError, json.JSONDecodeError):
                pass
    successors = next_turns(all_bases)
    families = {
        "go-ahead": occurrences(root, GO_AHEAD_IDS),
        "observed-running-correction": cited_occurrences(root, CORRECTION_IDS),
    }
    result = {
        "records": str(root),
        "families": {
            name: {
                "pattern_ids": sorted(GO_AHEAD_IDS if name == "go-ahead" else CORRECTION_IDS),
                "turn_count": len(turns),
                "turn_ids": sorted(turns),
                "coverage": coverage(turns, successors),
            }
            for name, turns in families.items()
        },
    }
    print(json.dumps(result, indent=2, sort_keys=True, ensure_ascii=False))


if __name__ == "__main__":
    main()
