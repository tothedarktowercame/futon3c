#!/usr/bin/env python3
"""Prepare/apply the bounded reply-text backfill through Agency HTTP only."""

import argparse
import csv
import datetime as dt
import json
import pathlib
import re
import sys
import time
import urllib.error
import urllib.parse
import urllib.request
from collections import defaultdict


RECORD_DIR = pathlib.Path.home() / ".emacs-graph/session-turn-analysis"
EVIDENCE = "http://127.0.0.1:7073/api/alpha/evidence"
AGENCY = "http://127.0.0.1:7070/api/alpha/xiang/turns"
DRAWBRIDGE = "http://127.0.0.1:6768/eval"
FIELDS = [
    "record_id", "turn_id", "session_id", "created_at", "join_used",
    "reply_evidence_id", "unified_turn_id", "reply_chars", "commits_json",
    "still_open_before", "session_last_record_id", "session_last_still_open_before",
    "reply_sha256", "still_open_after", "session_last_still_open_after",
    "readback", "ports_changed",
]
COMMIT = re.compile(r"^- (\S+) ([0-9a-f]{7,40}) (.+)$")
NUMSTAT = re.compile(r"\s+\(\+\d+ -\d+ over \d+ files\)$")


def request_json(url, method="GET", payload=None, timeout=60, attempts=5):
    data = None if payload is None else json.dumps(payload, ensure_ascii=False).encode()
    headers = {"Accept": "application/json"}
    if data is not None:
        headers["Content-Type"] = "application/json"
    request = urllib.request.Request(url, data=data, method=method, headers=headers)
    for attempt in range(attempts):
        try:
            with urllib.request.urlopen(request, timeout=timeout) as response:
                return response.status, json.load(response)
        except urllib.error.HTTPError as error:
            if method != "GET" or error.code not in (429, 502, 503, 504) or attempt + 1 == attempts:
                return error.code, json.loads(error.read().decode() or "{}")
        except (urllib.error.URLError, TimeoutError):
            if method != "GET" or attempt + 1 == attempts:
                raise
        time.sleep(1.5 * (attempt + 1))


def compute_ports_wide(record_id):
    """Call the live service function, widening its scoped historical window."""
    token = pathlib.Path(".admintoken").read_text(encoding="utf-8").strip()
    code = ("(let [svc (deref (var-get (ns-resolve 'futon3c.transport.http "
            "'!xiang-turn-service))) compute (requiring-resolve "
            "'futon3c.xiang.turn-service/compute-ports!) cap-var "
            "(ns-resolve 'futon3c.xiang.turn-service 'ports-session-cap)] "
            f"(with-redefs-fn {{cap-var 10000}} #(compute svc \"{record_id}\")) "
            "{:ok true :cap-after (var-get cap-var)})")
    request = urllib.request.Request(DRAWBRIDGE, data=code.encode(), method="POST",
                                     headers={"x-admin-token": token,
                                              "Content-Type": "text/plain",
                                              "Accept": "application/edn"})
    with urllib.request.urlopen(request, timeout=120) as response:
        reply = response.read().decode()
    if response.status != 200 or ":ok true" not in reply:
        raise RuntimeError(f"wide compute failed for {record_id}: {response.status} {reply[:200]}")


def evidence_pages():
    params = {"tags": "chat,turn", "since": "2026-10-01T00:00:00Z", "limit": "1000"}
    entries = []
    while True:
        status, page = request_json(EVIDENCE + "?" + urllib.parse.urlencode(params))
        if status != 200:
            raise RuntimeError(f"evidence page HTTP {status}")
        entries.extend(page.get("entries", []))
        cursor = page.get("next-cursor")
        if not cursor:
            return entries
        params.update({"cursor-at": cursor["at"], "cursor-id": cursor["id"]})


def body(entry):
    return entry.get("evidence/body") or {}


def reply_key(entry):
    return body(entry).get("unified-turn-id") or entry.get("evidence/id")


def evidence_index():
    entries = evidence_pages()
    by_id = {e.get("evidence/id"): e for e in entries if e.get("evidence/id")}
    groups = defaultdict(list)
    for entry in entries:
        if body(entry).get("role") == "assistant":
            groups[reply_key(entry)].append(entry)
    return by_id, groups


def reply_text(entry, groups):
    group = groups[reply_key(entry)]
    indexed = sorted((e for e in group if isinstance(body(e).get("segment-index"), int)),
                     key=lambda e: body(e)["segment-index"])
    indices = [body(e)["segment-index"] for e in indexed]
    finals = [e for e in indexed if body(e).get("segment-final") is True]
    complete = indices == list(range(len(indexed))) and len(finals) == 1 and len(indexed) > 1
    if complete:
        return str(body(finals[0]).get("unified-text") or ""), reply_key(entry)
    return str(body(entry).get("text") or ""), ""


def parse_commits(summary):
    marker = re.search(r"Commits during the turn[^\n]*:\n", str(summary or ""))
    if not marker:
        return []
    found = []
    for line in str(summary)[marker.end():].splitlines():
        match = COMMIT.fullmatch(line.strip())
        if match:
            repo, sha, subject = match.groups()
            found.append({"repo": repo, "sha": sha, "subject": NUMSTAT.sub("", subject)})
    return found


def load_record(record_id):
    return json.loads((RECORD_DIR / f"{record_id}.json").read_text(encoding="utf-8"))


def open_count(record):
    ports = record.get("ports")
    return "" if not isinstance(ports, dict) else len(ports.get("still_open") or [])


def all_session_records(session_id):
    found = []
    for path in RECORD_DIR.glob("turn-*.json"):
        if path.name.count(".") != 1:
            continue
        try:
            record = json.loads(path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError):
            continue
        if str(record.get("session_id") or "") == session_id and record.get("analysis_status") in ("analyzed", "drafted", "declared"):
            found.append((str(record.get("created_at") or ""), path.stem, record))
    return sorted(found)


def prepare(coverage_path, ledger_path):
    coverage = json.loads(pathlib.Path(coverage_path).read_text(encoding="utf-8"))
    selected = [r for r in coverage["rows"] if r["outcome"] in ("join-1", "join-2")]
    if len(selected) != 232:
        raise RuntimeError(f"expected frozen 232 matches, got {len(selected)}")
    by_id, groups = evidence_index()
    session_tails = {}
    for row in selected:
        sid = row["session_id"]
        if sid not in session_tails:
            records = all_session_records(sid)
            session_tails[sid] = records[-1] if records else ("", "", {})
    rows = []
    for match in sorted(selected, key=lambda r: (r["session_id"], r["created_at"], r["record_id"])):
        record = load_record(match["record_id"])
        entry = by_id.get(match["reply_evidence_id"])
        if entry is None:
            raise RuntimeError(f"missing reply evidence {match['reply_evidence_id']}")
        text, unified = reply_text(entry, groups)
        if not text:
            raise RuntimeError(f"empty reply text for {match['record_id']}")
        _, tail_id, tail_record = session_tails[match["session_id"]]
        import hashlib
        rows.append({
            "record_id": match["record_id"], "turn_id": match["turn_id"],
            "session_id": match["session_id"], "created_at": match["created_at"],
            "join_used": match["outcome"], "reply_evidence_id": match["reply_evidence_id"],
            "unified_turn_id": unified, "reply_chars": len(text),
            "commits_json": json.dumps(parse_commits(record.get("happened_summary")), separators=(",", ":")),
            "still_open_before": open_count(record), "session_last_record_id": tail_id,
            "session_last_still_open_before": open_count(tail_record),
            "reply_sha256": hashlib.sha256(text.encode()).hexdigest(),
            "still_open_after": "", "session_last_still_open_after": "",
            "readback": "", "ports_changed": "",
        })
    ledger_path.parent.mkdir(parents=True, exist_ok=True)
    with ledger_path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=FIELDS, delimiter="\t", lineterminator="\n")
        writer.writeheader(); writer.writerows(rows)
    print(json.dumps({"prepared": len(rows), "sessions": len(session_tails)}, indent=2))


def read_ledger(path):
    with path.open(encoding="utf-8", newline="") as stream:
        return list(csv.DictReader(stream, delimiter="\t"))


def apply(ledger_path, result_path):
    rows = read_ledger(ledger_path)
    by_id, groups = evidence_index()
    results = (json.loads(result_path.read_text(encoding="utf-8"))
               if result_path.exists() else [])
    prior = {r["record_id"]: r for r in results}
    for index, row in enumerate(rows):
        if prior.get(row["record_id"], {}).get("http") == 200:
            continue
        entry = by_id[row["reply_evidence_id"]]
        text, _ = reply_text(entry, groups)
        import hashlib
        if hashlib.sha256(text.encode()).hexdigest() != row["reply_sha256"]:
            raise RuntimeError(f"reply changed since ledger: {row['record_id']}")
        before = load_record(row["record_id"])
        before_ports = json.dumps(before.get("ports"), sort_keys=True, ensure_ascii=False)
        payload = {"reply": text, "commits": json.loads(row["commits_json"])}
        status, response = request_json(f"{AGENCY}/{urllib.parse.quote(row['record_id'])}/happened",
                                        method="POST", payload=payload, timeout=30, attempts=1)
        result = {"record_id": row["record_id"], "http": status, "response": response}
        if status != 200:
            results.append(result)
            result_path.write_text(json.dumps(results, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
            raise RuntimeError(f"stopped at {row['record_id']}: HTTP {status}")
        # The service's normal 50-turn window cannot recompute historical turns
        # in long sessions. Invoke the same live function synchronously with a
        # scoped cap; with-redefs-fn restores 50 before Drawbridge returns.
        compute_ports_wide(row["record_id"])
        # compute_ports_wide is synchronous. Allow a short extra window for
        # the route's already-scheduled ordinary computation to become visible.
        deadline = time.monotonic() + 5
        verified = None
        while time.monotonic() < deadline:
            get_status, view = request_json(f"{AGENCY}/{urllib.parse.quote(row['record_id'])}", attempts=3)
            record = view.get("record") or {}
            if get_status == 200 and record.get("reply_text") == text and record.get("ports") is not None:
                verified = record
                break
            time.sleep(1)
        result.update({"verified": verified is not None,
                       "still_open_after": open_count(verified or {}),
                       "ports_changed": (json.dumps((verified or {}).get("ports"), sort_keys=True, ensure_ascii=False)
                                         != before_ports) if verified else False})
        results = [r for r in results if r["record_id"] != row["record_id"]]
        results.append(result)
        result_path.write_text(json.dumps(results, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
        if index + 1 < len(rows):
            time.sleep(1)
    print(json.dumps({"applied": len(results), "verified": sum(r.get("verified", False) for r in results)}, indent=2))


def finalize(ledger_path, result_path):
    rows = read_ledger(ledger_path)
    results = {r["record_id"]: r for r in json.loads(result_path.read_text(encoding="utf-8"))}
    # Session totals must be read after the actual tail is recomputed; a tail
    # outside the matched set would otherwise retain its stale pre-backfill view.
    for tail in sorted({row["session_last_record_id"] for row in rows}):
        compute_ports_wide(tail)
    tail_after = {}
    for row in rows:
        tail = row["session_last_record_id"]
        if tail not in tail_after:
            tail_after[tail] = open_count(load_record(tail))
        result = results.get(row["record_id"], {})
        row["still_open_after"] = result.get("still_open_after", "")
        row["session_last_still_open_after"] = tail_after[tail]
        row["readback"] = "ok" if result.get("verified") else "failed"
        row["ports_changed"] = str(bool(result.get("ports_changed"))).lower()
    with ledger_path.open("w", encoding="utf-8", newline="") as stream:
        writer = csv.DictWriter(stream, fieldnames=FIELDS, delimiter="\t", lineterminator="\n")
        writer.writeheader(); writer.writerows(rows)
    sessions = {}
    for row in rows:
        sessions[row["session_id"]] = (int(row["session_last_still_open_before"] or 0),
                                        int(row["session_last_still_open_after"] or 0))
    print(json.dumps({"records": len(rows), "readback_ok": sum(r["readback"] == "ok" for r in rows),
                      "ports_changed": sum(r["ports_changed"] == "true" for r in rows),
                      "sessions": len(sessions),
                      "session_tail_open_before": sum(v[0] for v in sessions.values()),
                      "session_tail_open_after": sum(v[1] for v in sessions.values())}, indent=2))


def main():
    parser = argparse.ArgumentParser()
    sub = parser.add_subparsers(dest="command", required=True)
    prep = sub.add_parser("prepare")
    prep.add_argument("coverage_json", type=pathlib.Path); prep.add_argument("ledger", type=pathlib.Path)
    run = sub.add_parser("apply")
    run.add_argument("ledger", type=pathlib.Path); run.add_argument("result", type=pathlib.Path)
    final = sub.add_parser("finalize")
    final.add_argument("ledger", type=pathlib.Path); final.add_argument("result", type=pathlib.Path)
    args = parser.parse_args()
    if args.command == "prepare": prepare(args.coverage_json, args.ledger)
    elif args.command == "apply": apply(args.ledger, args.result)
    else: finalize(args.ledger, args.result)


if __name__ == "__main__":
    main()
