#!/usr/bin/env python3
"""Tension measure over Joe's REPL session-turn records (read-only).

A turn raises a tension on either signal:
  D (declared): a draft fragment with basis "declared" whose intent is one of
     {disagree, qualify, report-problem, redirect, retract} (Joe typed the mark,
     in colon form or plain form), or a leading paragraph mark in source_text.
  X (xiang-annotated): an operator act whose kind is among
     {disagree, qualify, report-problem, redirect, retract, challenge}.

Each tension is identified by its fragment's `about` and text.  Resolution is
judged within the next two operator turns of the same session:
  re-raised  (Jaccard >= 0.3 word overlap on about or text) -> unresolved
  resolved   not re-raised, and a window turn carries an approve/agree/continue
             act, or a window turn moves to a different about
  open       fewer than two later operator turns exist in the session
Anything else (window full, no re-raise, no resolution signal) is reported as
unresolved with no re-raise.

Rules are fixed here; they are not tuned on the data.

Usage: tension_measure.py [--store DIR] [--since YYYY-MM-DD] [OUT.json]
Reads ~/.emacs-graph/session-turn-analysis by default. Writes JSON to OUT.json
or stdout.
"""

import argparse
import collections
import glob
import json
import os
import re
import sys

TENSION_INTENTS = {"disagree", "qualify", "report-problem", "redirect", "retract"}
TENSION_ACT_KINDS = {"disagree", "qualify", "report-problem", "redirect",
                     "retract", "challenge"}
RESOLVE_ACT_KINDS = {"approve", "agree", "continue"}
MARKS = {"🈚": "disagree", "㊟": "qualify", "㊩": "report-problem",
         "🈘": "redirect", "🈹": "retract"}

STOPWORDS = set("""a an and are as at be been but by can could did do does for
from had has have he her him his how i if in into is it its just me more most
my no not of on or our out over own she should so some than that the their
them then there these they this to too us very was we were what when where
which who why will with would you your about""".split())

WORD_RE = re.compile(r"[a-zA-Z][a-zA-Z'-]*")


def content_words(text):
    return {w for w in WORD_RE.findall((text or "").lower())
            if w not in STOPWORDS}


def jaccard(a, b):
    if not a or not b:
        return 0.0
    return len(a & b) / len(a | b)


def similar(t1, t2):
    """Tensions are similar if about or text overlaps (Jaccard >= 0.3)."""
    for f in ("about", "text"):
        if jaccard(content_words(t1.get(f)), content_words(t2.get(f))) >= 0.3:
            return True
    return False


def load_turns(store, since):
    turns = []
    for path in glob.glob(os.path.join(store, "turn-*.json")):
        base = os.path.basename(path)
        if any(s in base for s in (".draft.", ".analysis.", ".candidates.")):
            continue
        if base.startswith("turn-end-"):
            continue
        try:
            with open(path, encoding="utf-8") as fh:
                d = json.load(fh)
        except (OSError, ValueError):
            continue
        if not isinstance(d, dict):
            continue
        if d.get("origin") != "operator":
            continue
        created = d.get("created_at") or ""
        if not created or created < since:
            continue
        turns.append({
            "record": base[: -len(".json")],
            "path": path,
            "session_id": d.get("session_id"),
            "created_at": created,
            "source_text": d.get("source_text") or "",
            "acts": [a for a in (d.get("acts") or []) if isinstance(a, dict)],
            "proposed_patterns": d.get("proposed_patterns") or [],
            "ranked": d.get("ranked"),
            "pattern_verdicts": d.get("pattern_verdicts"),
            "lu_ranked": d.get("lu_ranked"),
            "m40_ranked": d.get("m40_ranked"),
        })
    turns.sort(key=lambda t: (t["session_id"] or "", t["created_at"]))
    return turns


def declared_tensions(turn, store):
    """Signal D: declared-basis draft fragments, or leading paragraph marks."""
    tensions = []
    draft_path = turn["path"] + ".draft.json"
    if os.path.isfile(draft_path):
        try:
            with open(draft_path, encoding="utf-8") as fh:
                draft = json.load(fh)
            for fr in draft.get("fragments", []):
                if fr.get("basis") == "declared" and \
                        fr.get("intent") in TENSION_INTENTS:
                    tensions.append({"about": fr.get("about") or "",
                                     "text": fr.get("text") or ""})
        except (OSError, ValueError):
            pass
    # Leading mark of a paragraph in source_text (colon form or plain form).
    for para in re.split(r"\n\s*\n", turn["source_text"]):
        stripped = para.lstrip()
        for mark, intent in MARKS.items():
            if stripped.startswith(mark):
                rest = stripped[len(mark):]
                rest = rest[1:] if rest.startswith(":") else rest
                if rest.strip():  # plain form: mark alone introduces nothing
                    tensions.append({"about": "", "text": rest.strip()})
                break
    # de-duplicate identical (about, text)
    seen, uniq = set(), []
    for t in tensions:
        key = (t["about"], t["text"])
        if key not in seen:
            seen.add(key)
            uniq.append(t)
    return uniq


def xiang_tensions(turn):
    """Signal X: operator acts of a tension kind."""
    return [{"about": a.get("about") or "", "text": a.get("text") or ""}
            for a in turn["acts"]
            if a.get("kind") in TENSION_ACT_KINDS]


def window_resolves(window, tension):
    for t in window:
        for act in t["acts"]:
            if act.get("kind") in RESOLVE_ACT_KINDS:
                return True
            about = act.get("about") or ""
            if about and tension["about"] and \
                    jaccard(content_words(about),
                            content_words(tension["about"])) < 0.3:
                return True  # the conversation moved to a different about
    return False


def classify(session_turns, signal):
    """signal: 'D', 'X' or 'U' (union). Returns per-tension records."""
    results = []
    for i, turn in enumerate(session_turns):
        d = declared_tensions(turn, None)
        x = xiang_tensions(turn)
        tensions = {"D": d, "X": x,
                    "U": {(t["about"], t["text"]): t for t in d + x}.values()}[signal]
        for tension in tensions:
            window = session_turns[i + 1: i + 3]
            outcome, why = None, ""
            if len(window) < 2:
                outcome = "open"
            else:
                reraise = None
                for wt in window:
                    wd = declared_tensions(wt, None)
                    wx = xiang_tensions(wt)
                    for later in (wd + wx if signal != "U"
                                  else {(t["about"], t["text"]): t
                                        for t in wd + wx}.values()):
                        if similar(tension, later):
                            reraise = wt["record"]
                            break
                    if reraise:
                        break
                if reraise:
                    outcome = "unresolved"
                    why = "re-raised in " + reraise
                elif window_resolves(window, tension):
                    outcome = "resolved"
                else:
                    outcome = "unresolved"
                    why = "no resolution signal in window"
            results.append({"record": turn["record"], "session_id":
                            turn["session_id"], "signal": signal,
                            "about": tension["about"], "text": tension["text"],
                            "outcome": outcome, "why": why,
                            "proposed_patterns": turn["proposed_patterns"],
                            "ranked": turn["ranked"],
                            "pattern_verdicts": turn["pattern_verdicts"],
                            "lu_ranked": turn["lu_ranked"],
                            "m40_ranked": turn["m40_ranked"]})
    return results


def _ids_from(x):
    """Collect pattern ids from a ranking: list of maps with :id, or a map
    with a :ranked list of such (lu_ranked/m40_ranked shape)."""
    if isinstance(x, list):
        return [e.get("id") for e in x if isinstance(e, dict) and e.get("id")]
    if isinstance(x, dict):
        return _ids_from(x.get("ranked"))
    return []


def pattern_ids(turn_fields):
    ids = []
    for proposal in turn_fields.get("proposed_patterns") or []:
        for p in proposal.get("patterns") or []:
            ids.append(p.lstrip("~"))
    for r in (turn_fields.get("ranked"), turn_fields.get("lu_ranked"),
              turn_fields.get("m40_ranked")):
        ids.extend(_ids_from(r))
    return ids


def summarise(results):
    c = collections.Counter(r["outcome"] for r in results)
    return {"raised": len(results), "resolved": c["resolved"],
            "unresolved": c["unresolved"], "open": c["open"]}


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--store",
                    default=os.path.expanduser(
                        "~/.emacs-graph/session-turn-analysis"))
    ap.add_argument("--since", default="2026-09-28")
    ap.add_argument("--out", help="output JSON path (default stdout)")
    ap.add_argument("out_pos", nargs="?", help=argparse.SUPPRESS)
    args = ap.parse_args()
    out_path = args.out or args.out_pos

    turns = load_turns(args.store, args.since)
    sessions = collections.OrderedDict()
    for t in turns:
        sessions.setdefault(t["session_id"], []).append(t)

    report = {"store": args.store, "since": args.since,
              "operator_turns": len(turns), "sessions": len(sessions),
              "totals": {}, "per_session": {}, "agreement": {},
              "patterns_on_resolved_rounds": {}, "examples": {}}

    for signal in ("D", "X", "U"):
        results = []
        for sid, sturns in sessions.items():
            results.extend(classify(sturns, signal))
        report["totals"][signal] = summarise(results)
        per = {}
        for r in results:
            s = per.setdefault(r["session_id"], collections.Counter())
            s[r["outcome"]] += 1
        report["per_session"][signal] = {
            sid: {"raised": sum(c.values()), "resolved": c["resolved"],
                  "unresolved": c["unresolved"], "open": c["open"]}
            for sid, c in per.items()}

    # D/X agreement at turn granularity
    d_by_turn = {t["record"] for t in turns if declared_tensions(t, None)}
    x_by_turn = {t["record"] for t in turns if xiang_tensions(t)}
    report["agreement"] = {
        "both": len(d_by_turn & x_by_turn),
        "D_only": len(d_by_turn - x_by_turn),
        "X_only": len(x_by_turn - d_by_turn),
        "neither": len(turns) - len(d_by_turn | x_by_turn),
    }

    # patterns proposed on the raising turn, by outcome
    union = []
    for sid, sturns in sessions.items():
        union.extend(classify(sturns, "U"))
    pat = {"resolved": collections.Counter(), "unresolved": collections.Counter(),
           "open": collections.Counter()}
    for r in union:
        for pid in pattern_ids(r):
            pat[r["outcome"]][pid] += 1
    top = pat["resolved"].most_common(15)
    report["patterns_on_resolved_rounds"] = [
        {"pattern": pid, "on_resolved_rounds": n,
         "on_unresolved_rounds": pat["unresolved"][pid],
         "on_open_rounds": pat["open"][pid]} for pid, n in top]

    def short(text, n=12):
        words = text.split()
        return " ".join(words[:n])

    report["examples"]["resolved"] = [
        {"record": r["record"], "session": r["session_id"], "about": r["about"],
         "text": short(r["text"])} for r in union if r["outcome"] == "resolved"][:5]
    report["examples"]["unresolved"] = [
        {"record": r["record"], "session": r["session_id"], "about": r["about"],
         "text": short(r["text"]), "why": r["why"]}
        for r in union if r["outcome"] == "unresolved"][:5]

    out = json.dumps(report, indent=1, sort_keys=True)
    if out_path:
        with open(out_path, "w", encoding="utf-8") as fh:
            fh.write(out + "\n")
    else:
        sys.stdout.write(out + "\n")


if __name__ == "__main__":
    main()
