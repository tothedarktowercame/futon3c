#!/usr/bin/env python3
"""Build the C1 record-backed triple corpus for M-象-cascade.

The only store reader is scripts/turn_frames.py.  Its per-session output is
cached under /tmp; corpus assembly below is pure and is exercised by fixtures.
"""

from __future__ import annotations

import argparse
import datetime as dt
import hashlib
import json
import os
import subprocess
import sys
import time
from collections import Counter
from pathlib import Path

import xiang_cascade_d0 as d0


REPO_ROOT = Path("/home/joe/code")
HERE = Path(__file__).resolve().parent
DEFAULT_RECORDS = Path(os.path.expanduser("~/.emacs-graph/session-turn-analysis"))
DEFAULT_OUTPUT = REPO_ROOT / "futon3c/holes/labs/M-象-cascade/corpus-c1.jsonl"
DEFAULT_CACHE = Path("/tmp/xiang-cascade-c1-frames")


def instant(value):
    if not value:
        return None
    return dt.datetime.fromisoformat(value.replace("Z", "+00:00"))


def cache_path(cache_dir: Path, session_id: str) -> Path:
    digest = hashlib.sha256(session_id.encode()).hexdigest()[:16]
    return cache_dir / f"{digest}.json"


def load_session_frames(session_id, cache_dir=DEFAULT_CACHE, sleeper=time.sleep):
    """Read cached frames, else invoke turn_frames.py once (one 504 retry)."""
    cache_dir.mkdir(parents=True, exist_ok=True)
    path = cache_path(cache_dir, session_id)
    if path.exists():
        return {"frames": json.loads(path.read_text(encoding="utf-8")), "source": "cache"}
    command = [sys.executable, str(HERE / "turn_frames.py"), session_id]
    attempts = 0
    while True:
        attempts += 1
        result = subprocess.run(command, capture_output=True, text=True, check=False)
        if result.returncode == 0:
            frames = json.loads(result.stdout)
            temporary = path.with_suffix(".tmp")
            temporary.write_text(json.dumps(frames, ensure_ascii=False, sort_keys=True), encoding="utf-8")
            os.replace(temporary, path)
            return {"frames": frames, "source": "store"}
        combined = f"{result.stdout}\n{result.stderr}"
        if "504" in combined and attempts == 1:
            sleeper(60)
            continue
        reason = "store-504" if "504" in combined else "frames-read-failed"
        return {"missing": reason, "detail": combined.strip()[-500:]}


def family_rows(records=DEFAULT_RECORDS):
    return {
        "go-ahead": d0.occurrences(records, d0.GO_AHEAD_IDS),
        "observed-running-correction": d0.cited_occurrences(records, d0.CORRECTION_IDS),
    }


def find_frame(frames, row):
    evidence_id = row["base"].get("evidence_id")
    if evidence_id:
        hits = [i for i, frame in enumerate(frames)
                if frame.get("turn", {}).get("evidence_id") == evidence_id]
        if len(hits) == 1:
            return hits[0], "evidence-id"
    wanted = instant(row["base"].get("created_at"))
    if wanted:
        timed = [(abs((instant(frame.get("turn", {}).get("at")) - wanted).total_seconds()), i)
                 for i, frame in enumerate(frames) if instant(frame.get("turn", {}).get("at"))]
        # Record creation and evidence event times are the same operator event,
        # but different writers can round fractions. A minute is a join bound,
        # not an unbounded nearest-neighbour guess.
        if timed:
            distance, index = min(timed)
            if distance <= 60 and sum(1 for d, _ in timed if d == distance) == 1:
                return index, "event-time"
    return None, None


def commit_rows(frame):
    out = []
    for happened in (frame or {}).get("happened", []):
        summary = happened.get("summary") or {}
        if summary.get("event") == "turn-commits":
            out.extend(summary.get("commits") or [])
    return out


def resolve_commit(commit, repo_root=REPO_ROOT):
    repo, raw_sha = commit.get("repo"), commit.get("sha")
    sha = raw_sha.strip() if isinstance(raw_sha, str) else ""
    resolved = False
    if repo and sha and (repo_root / repo).is_dir():
        check = subprocess.run(
            ["git", "-C", str(repo_root / repo), "cat-file", "-e", f"{sha}^{{commit}}"],
            stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, check=False)
        resolved = check.returncode == 0
    return {"repo": repo, "sha": sha, "resolved": resolved}


def frame_facts(frame, repo_root=REPO_ROOT):
    happened = (frame or {}).get("happened", [])
    commits = [resolve_commit(c, repo_root) for c in commit_rows(frame)]
    promise_rows = []
    for item in happened:
        event = (item.get("summary") or {}).get("event") or ""
        if isinstance(event, str) and event.startswith("promise/"):
            promise_rows.append({"at": item.get("at"), "event": event,
                                 "summary": item.get("summary")})
    return {
        "agent_reply_present": any((h.get("summary") or {}).get("event") == "chat-turn"
                                   for h in happened),
        "commits": commits,
        "commit_resolved": sum(c["resolved"] for c in commits),
        "commit_unresolved": sum(not c["resolved"] for c in commits),
        "promise_park_rows": promise_rows,
        "parks_made": sum(p["event"] == "promise/park-made" for p in promise_rows),
        "parks_released": sum(p["event"] == "promise/released" for p in promise_rows),
    }


def move_facts(row):
    return {
        "proxy": True,
        "source": "象-analysis",
        "fragments": [{"sentence": f.get("sentence"), "index": f.get("index"),
                       "intent": f.get("intent"), "target": f.get("target"),
                       "pattern_refs": [r.get("id") for r in f.get("pattern_refs", [])]}
                      for f in row["fragments"]],
    }


def build_entry(family, turn_id, row, session_result, repo_root=REPO_ROOT):
    base = {"family": family, "turn_id": turn_id,
            "session_id": row["base"].get("session_id"),
            "turn_at": row["base"].get("created_at")}
    if session_result.get("missing"):
        return {**base, "missing": session_result["missing"]}
    frames = session_result.get("frames") or []
    if not frames:
        return {**base, "missing": "session-no-frames"}
    index, joined_by = find_frame(frames, row)
    if index is None:
        return {**base, "missing": "turn-not-found"}
    if index == 0:
        return {**base, "missing": "pre-frame-missing",
                "frame_evidence_id": frames[index].get("turn", {}).get("evidence_id")}
    return {
        **base,
        "frame_evidence_id": frames[index].get("turn", {}).get("evidence_id"),
        "joined_by": joined_by,
        "pre": frame_facts(frames[index - 1], repo_root),
        "move": move_facts(row),
        "post": frame_facts(frames[index], repo_root),
    }


def assemble(rows_by_family, frames_by_session, repo_root=REPO_ROOT):
    entries = []
    for family in sorted(rows_by_family):
        for turn_id, row in sorted(rows_by_family[family].items()):
            sid = row["base"].get("session_id")
            result = frames_by_session.get(sid, {"missing": "session-unavailable"})
            entries.append(build_entry(family, turn_id, row, result, repo_root))
    return entries


def summarize(entries):
    result = {}
    for family in sorted({e["family"] for e in entries}):
        rows = [e for e in entries if e["family"] == family]
        full = [e for e in rows if "missing" not in e]
        missing = Counter(e["missing"] for e in rows if "missing" in e)
        result[family] = {
            "turns": len(rows), "full_triples": len(full),
            "pre_agent_reply": sum(e["pre"]["agent_reply_present"] for e in full),
            "pre_commit": sum(bool(e["pre"]["commits"]) for e in full),
            "pre_promise_or_park": sum(bool(e["pre"]["promise_park_rows"]) for e in full),
            "post_agent_reply": sum(e["post"]["agent_reply_present"] for e in full),
            "post_commit": sum(bool(e["post"]["commits"]) for e in full),
            "post_parks_made": sum(bool(e["post"]["parks_made"]) for e in full),
            "post_parks_released": sum(bool(e["post"]["parks_released"]) for e in full),
            "resolved_commits": sum(e[side]["commit_resolved"] for e in full for side in ("pre", "post")),
            "unresolved_commits": sum(e[side]["commit_unresolved"] for e in full for side in ("pre", "post")),
            "missing": dict(sorted(missing.items())),
        }
    return result


def write_jsonl(entries, output):
    text = "".join(json.dumps(e, ensure_ascii=False, sort_keys=True) + "\n" for e in entries)
    output.parent.mkdir(parents=True, exist_ok=True)
    output.write_text(text, encoding="utf-8")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--records", type=Path, default=DEFAULT_RECORDS)
    parser.add_argument("--cache", type=Path, default=DEFAULT_CACHE)
    parser.add_argument("--output", type=Path, default=DEFAULT_OUTPUT)
    args = parser.parse_args()
    rows = family_rows(args.records)
    sessions = sorted({row["base"].get("session_id") for family in rows.values()
                       for row in family.values() if row["base"].get("session_id")})
    frame_results = {sid: load_session_frames(sid, args.cache) for sid in sessions}
    entries = assemble(rows, frame_results)
    write_jsonl(entries, args.output)
    print(json.dumps(summarize(entries), indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
