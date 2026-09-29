#!/usr/bin/env python3
"""turn_frames.py — assemble "frames" for one Claude/Codex REPL session.

One frame per operator turn, as JSON on stdout. Read-only: fetches evidence
rows from futon1b and reads 象's turn-analysis files from disk. No model
calls, no writes.

Usage:
    python3 scripts/turn_frames.py SESSION_ID [--limit N] [--base URL] > frames.json

Frame shape:
{"turn": {"evidence_id", "at", "text"},
 "parse": {"status": "analyzed"|"missing",
           "fragments": [{"sentence", "text",
                          "labels": [{"source": "象/<labeller>", "intent"},
                                     {"source": "negation", "intent"}...],
                          "combined": <intent if all sources agree else null>,
                          "disagree": bool}]},
 "patterns": {"matched": [...], "rejected": [...],
              "proposed_by_parent": {"<parent or (none)>":
                                     [{"id","title","fragment"}]}},
 "happened": [non-operator rows with at in [turn.at, next-turn.at), sorted,
              each {"at","type","summary"}]}
"""

import argparse
import glob
import json
import os
import re
import sys
import urllib.parse
import urllib.request

DEFAULT_BASE = "http://localhost:7073"
ANALYSIS_DIR = os.path.expanduser("~/.emacs-graph/session-turn-analysis")


# ---------------------------------------------------------------- fetching

def fetch_evidence(session_id, base=DEFAULT_BASE, page_limit=1000):
    """Fetch all evidence rows for a session, following next-cursor."""
    rows = []
    params = {"session-id": session_id, "limit": str(page_limit)}
    while True:
        url = base.rstrip("/") + "/api/alpha/evidence?" + urllib.parse.urlencode(params)
        req = urllib.request.Request(url, headers={"Accept": "application/json"})
        with urllib.request.urlopen(req) as resp:
            page = json.load(resp)
        rows.extend(page.get("entries", []))
        cursor = page.get("next-cursor")
        if not cursor:
            return rows
        params["cursor-at"] = cursor["at"]
        params["cursor-id"] = cursor["id"]


# ---------------------------------------------------------------- analysis files

def load_analyses(session_id, analysis_dir=ANALYSIS_DIR):
    """Load 象 records for a session.

    Returns {key: {"record": ..., "analysis": ...|None, "candidates": [...]}}
    where key is evidence_id when the record carries one, else
    (session_id, turn_id). Records without evidence_id are keyed by the
    tuple; the join falls back to that.
    """
    out = {"by_evidence_id": {}, "by_turn_id": {}, "by_text": {}}
    pattern = os.path.join(analysis_dir, "turn-*.json")
    for path in sorted(glob.glob(pattern)):
        base = os.path.basename(path)
        if base.endswith(".analysis.json") or base.endswith(".candidates.json"):
            continue
        try:
            with open(path) as f:
                record = json.load(f)
        except (OSError, ValueError):
            continue
        if record.get("session_id") != session_id:
            continue
        analysis = None
        apath = path + ".analysis.json"
        if os.path.exists(apath):
            try:
                with open(apath) as f:
                    analysis = json.load(f)
            except (OSError, ValueError):
                analysis = None
        candidates = []
        # both spellings exist: turn-XX.json.candidates.json and turn-XX.candidates.json
        for cpath in (path + ".candidates.json",
                      os.path.join(analysis_dir, base[:-len(".json")] + ".candidates.json")):
            if os.path.exists(cpath):
                try:
                    with open(cpath) as f:
                        candidates = json.load(f).get("candidates", [])
                except (OSError, ValueError):
                    candidates = []
                break
        entry = {"record": record, "analysis": analysis, "candidates": candidates}
        if record.get("evidence_id"):
            out["by_evidence_id"][record["evidence_id"]] = entry
        if record.get("turn_id"):
            out["by_turn_id"][(session_id, record["turn_id"])] = entry
        text_key = (record.get("source_text") or "").strip()
        if text_key:
            out["by_text"][text_key] = entry
    return out


# ---------------------------------------------------------------- helpers

def at_key(row):
    """Sortable instant for evidence/at. The store mixes 3- and 9-digit
    fractions ("...00.840Z", "...00.840123456Z"), which sort wrongly as
    strings: 'Z' sorts after every digit. Pad the fraction to 9 digits."""
    at = row.get("evidence/at") or ""
    m = re.match(r"(.*T\d\d:\d\d:\d\d)(?:\.(\d+))?Z$", at)
    return f"{m.group(1)}.{(m.group(2) or '').ljust(9, '0')[:9]}Z" if m else at


def body_of(row):
    b = row.get("evidence/body")
    return b if isinstance(b, dict) else {}


def is_operator_turn(row):
    """Operator turn: coordination chat-turn with role user and origin kind
    'operator'. (Real rows also show user turns with origin kind 'harness'
    actor 'parked-resume' — those are park-resume replays, not the operator
    typing, so they are excluded.)"""
    if row.get("evidence/type") != "coordination":
        return False
    b = body_of(row)
    if b.get("event") != "chat-turn" or b.get("role") != "user":
        return False
    return (row.get("evidence/origin") or {}).get("kind") == "operator"


def negation_rows_for(row_index, turn_evidence_id):
    """interpretation/negation rows that point at this turn."""
    out = []
    for r in row_index:
        t = r.get("evidence/type", "")
        if not t.startswith("interpretation/negation"):
            continue
        subj = r.get("evidence/subject") or {}
        if (subj.get("ref/id") == turn_evidence_id
                or r.get("evidence/in-reply-to") == turn_evidence_id):
            out.append(r)
    return out


def summarize_row(row):
    """Short structured summary for a happened row."""
    t = row.get("evidence/type")
    b = body_of(row)
    if b.get("event") == "turn-commits":
        # Commits made in any repo during the turn window — NOT necessarily
        # by this agent. Keep repo/author/subject verbatim.
        return {"event": "turn-commits",
                "turn-id": b.get("turn-id"),
                "commits": [{"repo": c.get("repo"),
                             "sha": c.get("sha"),
                             "committed-at": c.get("committed-at"),
                             "author": c.get("author"),
                             "subject": c.get("subject")}
                            for c in b.get("commits", [])]}
    if t and t.startswith("promise/"):
        excerpt = {k: b[k] for k in ("park-id", "promise-id", "job-id",
                                     "reason", "status", "dependency")
                   if k in b}
        if not excerpt:
            excerpt = json.dumps(b, ensure_ascii=False)[:200]
        return {"event": t, "excerpt": excerpt}
    if t and t.startswith("interpretation/"):
        return {"intent": b.get("intent"),
                "fragment-id": b.get("fragment-id"),
                "fragment-text": b.get("fragment-text"),
                "target": b.get("target")}
    # any other row: type + at only (caller adds at); give a hint if text exists
    s = {"event": b.get("event") or t}
    if isinstance(b.get("text"), str):
        s["text"] = b["text"][:120]
    return s


def find_fragment(sentences, fragment_id):
    """Resolve a negation fragment-id like 's1:0' to (sentence_id, fragment)."""
    if not fragment_id or ":" not in fragment_id:
        return None
    sid, idx = fragment_id.rsplit(":", 1)
    try:
        idx = int(idx)
    except ValueError:
        return None
    for s in sentences:
        if s.get("id") == sid:
            frags = s.get("fragments", [])
            if 0 <= idx < len(frags):
                return sid, frags[idx]
    return None


# ---------------------------------------------------------------- frame building

def build_frames(rows, analyses, session_id=None, limit=None):
    """Pure frame assembly from evidence rows + loaded analyses."""
    rows = sorted(rows, key=at_key)
    all_turns = [r for r in rows if is_operator_turn(r)]
    # --limit cuts the frames, not the windows: the last frame shown still
    # ends at the next operator turn.
    turns = all_turns[:limit] if limit else all_turns

    frames = []
    for i, turn in enumerate(turns):
        start = at_key(turn)
        end = at_key(all_turns[i + 1]) if i + 1 < len(all_turns) else None
        eid = turn.get("evidence/id")
        b = body_of(turn)

        # join to 象's record: evidence_id first, then session_id + turn_id
        # (verified against source_text: the evidence body's turn-id on an
        # operator row is the preceding agent turn's id, off by one from the
        # record's turn_id), then exact source_text match.
        entry = analyses.get("by_evidence_id", {}).get(eid)
        join_key = "evidence_id" if entry else None
        text = (b.get("text") or "").strip()
        if entry is None:
            cand = analyses.get("by_turn_id", {}).get(
                (session_id, b.get("turn-id")))
            if cand is not None:
                rt = (cand["record"].get("source_text") or "").strip()
                if rt and text and (rt[:80] == text[:80]):
                    entry = cand
                    join_key = "session_id+turn_id"
        if entry is None and text:
            entry = analyses.get("by_text", {}).get(text)
            if entry is not None:
                join_key = "source_text"

        # parse section
        negs = negation_rows_for(rows, eid)
        fragments_out = []
        matched, rejected = [], []
        status = "missing"
        if entry and entry.get("analysis"):
            analysis = entry["analysis"]
            labeller = analysis.get("labeller")
            sentences = analysis.get("sentences", [])
            status = "analyzed"
            for s in sentences:
                for frag in s.get("fragments", []):
                    labels = []
                    if frag.get("intent") is not None:
                        labels.append({"source": "象/" + str(labeller),
                                       "intent": frag.get("intent")})
                    intents = [frag.get("intent")] if frag.get("intent") is not None else []
                    fragments_out.append({
                        "sentence": s.get("id"),
                        "text": frag.get("text"),
                        "labels": labels,
                        "_intents": intents,
                        "_frag": frag,
                        "_sid": s.get("id"),
                    })
                    matched.extend(frag.get("pattern_refs") or [])
                    rejected.extend(frag.get("pattern_rejections") or [])
        # negation labels: attach to the fragment they name (fall back to
        # matching fragment-text)
        for nr in negs:
            nb = body_of(nr)
            if nb.get("intent") is None:
                continue
            target = None
            if entry and entry.get("analysis"):
                sentences = entry["analysis"].get("sentences", [])
                found = find_fragment(sentences, nb.get("fragment-id"))
                if found:
                    sid, frag = found
                    for fo in fragments_out:
                        if fo["_frag"] is frag:
                            target = fo
                            break
                if target is None:
                    for fo in fragments_out:
                        if fo.get("text") == nb.get("fragment-text"):
                            target = fo
                            break
            if target is not None:
                target["labels"].append({"source": "negation",
                                         "intent": nb.get("intent")})
                target["_intents"].append(nb.get("intent"))
        for fo in fragments_out:
            intents = [x for x in fo.pop("_intents") if x is not None]
            fo.pop("_frag", None)
            fo.pop("_sid", None)
            distinct = set(intents)
            fo["disagree"] = len(distinct) > 1
            fo["combined"] = intents[0] if intents and len(distinct) == 1 else None

        # candidates grouped by parent
        proposed = {}
        if entry:
            for c in entry.get("candidates", []):
                parent = c.get("parent") or "(none)"
                proposed.setdefault(parent, []).append(
                    {"id": c.get("id"), "title": c.get("title"),
                     "fragment": c.get("fragment")})

        # happened rows: everything that is not an operator turn in the window
        happened = []
        for r in rows:
            if is_operator_turn(r):
                continue
            at = at_key(r)
            if at < start:
                continue
            if end is not None and at >= end:
                continue
            happened.append({"at": r.get("evidence/at"),
                             "type": r.get("evidence/type"),
                             "summary": summarize_row(r)})
        # rows is already in at_key order, so happened is too

        frames.append({
            "turn": {"evidence_id": eid,
                     "at": turn.get("evidence/at"),
                     "text": b.get("text")},
            "parse": {"status": status, "fragments": fragments_out},
            "patterns": {"matched": matched, "rejected": rejected,
                         "proposed_by_parent": proposed},
            "happened": happened,
            "_join": join_key if entry else None,
        })
    return frames


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("session_id")
    ap.add_argument("--limit", type=int, default=None)
    ap.add_argument("--base", default=DEFAULT_BASE)
    ap.add_argument("--analysis-dir", default=ANALYSIS_DIR)
    args = ap.parse_args(argv)

    rows = fetch_evidence(args.session_id, base=args.base)
    analyses = load_analyses(args.session_id, analysis_dir=args.analysis_dir)
    frames = build_frames(rows, analyses, session_id=args.session_id,
                          limit=args.limit)
    for f in frames:
        f.pop("_join", None)
    json.dump(frames, sys.stdout, ensure_ascii=False, indent=1)
    sys.stdout.write("\n")
    return 0


if __name__ == "__main__":
    sys.exit(main())
