#!/usr/bin/env python3
"""Fill evidence_id on operator turn records that have none, from futon1b.

When futon1b was slow (2026-10-12 to 10-14) Emacs queued each operator row in
its outbox and wrote the turn record with evidence_id null; the row landed
later under its stable id. The join: the row's evidence/session-id equals the
record's session_id, it is the user half of a chat-turn, and its text equals
the record's source text. Among several such rows (an operator says "yes"
more than once) the one whose body turn-id is the record's, or the record's
minus one, wins: until 2026-10-14 Emacs emitted the operator row before it
advanced the turn counter, so every typed row sits one turn id behind its
record. The record gains evidence_id and evidence_delivery "backfilled".
Dry run by default; --apply writes, atomically, and logs each change to LOG.

Usage: xiang_evidence_id_backfill.py [--since 2026-10-12] [--apply] [--dir DIR] [--base URL]
"""
import argparse
import glob
import json
import os
import re
import sys
import urllib.parse
import urllib.request

BASE = "http://localhost:7073"
DIR = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
LOG = os.path.join(os.path.dirname(os.path.abspath(__file__)), "..",
                   "holes/labs/M-象-2000/data/evidence-id-backfill.jsonl")


def session_rows(base, session_id, timeout=30):
    """Every evidence row of SESSION_ID, paged by cursor."""
    rows, after = [], None
    while True:
        q = {"session-id": session_id, "limit": "500", "include-ephemeral": "true"}
        if after:  # {:at .. :id ..}: continue from both halves (API-CONTRACT, evidence paging)
            q["cursor-at"], q["cursor-id"] = after.get("at", ""), after.get("id", "")
        req = urllib.request.Request(base + "/api/alpha/evidence?" + urllib.parse.urlencode(q),
                                     headers={"Accept": "application/json"})
        with urllib.request.urlopen(req, timeout=timeout) as r:
            d = json.load(r)
        page = [e for e in d.get("entries", []) if isinstance(e, dict)]
        rows.extend(page)
        after = d.get("next-cursor")
        if not page or not isinstance(after, dict):
            return rows


def norm(text):
    return " ".join(str(text or "").split())


def user_rows(rows):
    return [r for r in rows
            if isinstance(r.get("evidence/body"), dict)
            and r["evidence/body"].get("event") == "chat-turn"
            and r["evidence/body"].get("role") == "user"]


def previous_turn(turn_id):
    m = re.match(r"^(.*-turn-)(\d+)$", turn_id or "")
    return m.group(1) + str(int(m.group(2)) - 1) if m else None


def match_row(record, rows):
    """The one user row for RECORD, or None. Text must agree; the turn id
    must be the record's or the one before it (see the module docstring)."""
    text = norm(record.get("source_text") or record.get("original_text"))
    same = [r for r in rows if norm(r["evidence/body"].get("text")) == text]
    wanted = {record.get("turn_id"), previous_turn(record.get("turn_id"))}
    hits = [r for r in same if r["evidence/body"].get("turn-id") in wanted]
    return hits[0] if len(hits) == 1 else None


def candidates(directory, since):
    for path in sorted(glob.glob(os.path.join(directory, "turn-*.json"))):
        try:
            with open(path) as fh:
                d = json.load(fh)
        except (OSError, json.JSONDecodeError):
            continue
        if not isinstance(d, dict):
            continue
        if d.get("origin", "operator") != "operator" or d.get("evidence_id"):
            continue
        if str(d.get("created_at", "")) < since or not d.get("session_id") or not d.get("turn_id"):
            continue
        yield path, d


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--since", default="2026-10-12")
    ap.add_argument("--apply", action="store_true")
    ap.add_argument("--dir", default=DIR)
    ap.add_argument("--base", default=BASE)
    a = ap.parse_args(argv)
    todo = list(candidates(a.dir, a.since))
    by_session = {}
    for path, d in todo:
        by_session.setdefault(d["session_id"], []).append((path, d))
    print("%d records without evidence_id since %s in %d sessions" % (len(todo), a.since, len(by_session)))
    filled, unmatched, mismatched = 0, 0, 0
    log = open(LOG, "a") if a.apply else None
    for sid, items in by_session.items():
        try:
            rows = user_rows(session_rows(a.base, sid))
        except Exception as e:
            print("  session %s: fetch failed (%s); skipped %d" % (sid[:8], e, len(items)))
            unmatched += len(items)
            continue
        for path, d in items:
            row = match_row(d, rows)
            if row is None:
                same = sum(1 for r in rows if norm(r["evidence/body"].get("text")) == norm(d.get("source_text")))
                print("  %s %s %s: %s" % (os.path.basename(path), d.get("agent_id"), d["turn_id"],
                                          "no row with this text" if not same else "%d rows with this text, none at turn N or N-1" % same))
                unmatched += (not same)
                mismatched += bool(same)
                continue
            eid, row_turn = row["evidence/id"], row["evidence/body"].get("turn-id")
            print("  %s %s %s -> %s (row turn %s)%s" % (os.path.basename(path), d.get("agent_id"), d["turn_id"],
                                                        eid, row_turn, "" if a.apply else " (dry run)"))
            if a.apply:
                d["evidence_id"] = eid
                d["evidence_delivery"] = "backfilled"
                tmp = path + ".tmp"
                with open(tmp, "w") as fh:
                    json.dump(d, fh, ensure_ascii=False, indent=2)
                os.replace(tmp, path)
                log.write(json.dumps({"record": os.path.basename(path), "evidence_id": eid,
                                      "session_id": sid, "turn_id": d["turn_id"], "row_turn_id": row_turn,
                                      "row_at": row.get("evidence/at")}) + "\n")
            filled += 1
    print("filled %d, no row %d, ambiguous %d%s" % (filled, unmatched, mismatched,
                                                     "" if a.apply else " (dry run; --apply to write)"))
    return 0


if __name__ == "__main__":
    sys.exit(main())
