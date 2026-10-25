#!/usr/bin/env python3
"""Evaluate the tension-resolution sensor against 馬-sonnet reference labels.

Stages (run in order):
  sample   draw 60 stratified X tensions (20 resolved-by-act, 20
           resolved-by-different-about, 20 unresolved) across >=10 sessions,
           write sample.json and ma-prompts/whistle-<k>.md (6 batches of 10)
  collect  read ma-replies/reply-<k>.md, parse labels, string-check quotes,
           write labels.json
  sheet    write joe-15.md (15 items, 5 per stratum, shuffled, blind)
  report   print the confusion matrix and shares

Read-only over the turn store; writes only under the experiment directory.
"""

import argparse
import collections
import glob
import json
import os
import random
import re
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__)))
import tension_measure as tm  # noqa: E402

RESOLVE_ACTS = {"approve", "agree", "continue"}
TENSION_KINDS = tm.TENSION_ACT_KINDS
SEED = 7


def resolution_reason(tension, window):
    """'re-raised', 'act', 'about', or 'unresolved' for a full window."""
    for wt in window:
        for later in tm.declared_tensions(wt, None) + tm.xiang_tensions(wt):
            if tm.similar(tension, later):
                return "re-raised"
    for wt in window:
        for act in wt["acts"]:
            if act.get("kind") in RESOLVE_ACTS:
                return "act"
            about = act.get("about") or ""
            if about and tension["about"] and \
                    tm.jaccard(tm.content_words(about),
                               tm.content_words(tension["about"])) < 0.3:
                return "about"
    return "unresolved"


def load_sessions(store, since):
    turns = tm.load_turns(store, since)
    sessions = collections.OrderedDict()
    for t in turns:
        sessions.setdefault(t["session_id"], []).append(t)
    return sessions


def turn_payload(t):
    return {"record": t["record"], "created_at": t["created_at"],
            "source_text": t["source_text"],
            "acts": [{"kind": a.get("kind"), "about": a.get("about")}
                     for a in t["acts"]]}


def stage_sample(exp, store, since):
    sessions = load_sessions(store, since)
    by_stratum = collections.defaultdict(list)
    for sid, sturns in sessions.items():
        for i, turn in enumerate(sturns):
            window = sturns[i + 1: i + 3]
            if len(window) < 2:
                continue
            for tension in tm.xiang_tensions(turn):
                reason = resolution_reason(tension, window)
                if reason == "re-raised":
                    continue
                stratum = ("resolved-act" if reason == "act"
                           else "resolved-about" if reason == "about"
                           else "unresolved")
                by_stratum[stratum].append({
                    "id": "%s/%d" % (turn["record"], len(by_stratum[stratum])),
                    "session_id": sid,
                    "stratum": stratum,
                    "tension": tension,
                    "reply_text": (turn.get("reply_text") or "")[:1500],
                    "window": [turn_payload(w) for w in window]})
    rng = random.Random(SEED)
    picked, used_sessions = [], collections.Counter()

    def spread(items, n):
        # round-robin over sessions so no session dominates
        by_sess = collections.defaultdict(list)
        for it in items:
            by_sess[it["session_id"]].append(it)
        for v in by_sess.values():
            rng.shuffle(v)
        order = sorted(by_sess, key=lambda s: -len(by_sess[s]))
        out = []
        while len(out) < n and any(by_sess.values()):
            for s in list(order):
                if by_sess[s] and len(out) < n:
                    out.append(by_sess[s].pop())
                    used_sessions[s] += 1
        return out[:n]

    sample = []
    for stratum, n in (("resolved-act", 20), ("resolved-about", 20),
                       ("unresolved", 20)):
        sample.extend(spread(by_stratum[stratum], n))
    assert len(used_sessions) >= 10, len(used_sessions)
    with open(os.path.join(exp, "sample.json"), "w") as fh:
        json.dump({"seed": SEED, "store": store, "since": since,
                   "items": sample}, fh, indent=1)

    pdir = os.path.join(exp, "ma-prompts")
    os.makedirs(pdir, exist_ok=True)
    for k in range(6):
        chunk = sample[k * 10:(k + 1) * 10]
        lines = [
            "You are labelling whether an operator tension was resolved. For",
            "each item you get a tension the analyst raised (a paraphrase of",
            "the operator's concern), the assistant's reply that followed, and",
            "the next two operator turns. Label each item:",
            "  re-raised     the operator returns to the same concern",
            "  resolved      the operator accepts the answer, or the concern",
            "                is dealt with",
            "  dropped       the operator moves on without either",
            "  not-a-tension the fragment raises no tension (ordinary",
            "                qualification, question, or aside)",
            "For re-raised / resolved / not-a-tension, give a verbatim quote of",
            "at least 5 words from the later operator turns that supports the",
            "label; null for dropped. Reply with one EDN map per item, keyed by",
            "item id: {:id \"...\" :label \"...\" :quote \"...\" or null}.",
            ""]
        for it in chunk:
            w0, w1 = it["window"]
            lines += ["## item %s" % it["id"],
                      "tension about: %s" % it["tension"]["about"],
                      "tension text: %s" % it["tension"]["text"],
                      "reply (start): %s" % it["reply_text"][:600],
                      "operator turn +1: %s" % w0["source_text"][:700],
                      "  acts: %s" % json.dumps(w0["acts"]),
                      "operator turn +2: %s" % w1["source_text"][:700],
                      "  acts: %s" % json.dumps(w1["acts"]), ""]
        with open(os.path.join(pdir, "whistle-%d.md" % k), "w") as fh:
            fh.write("\n".join(lines))
    print("sample: %d items over %d sessions; prompts in %s"
          % (len(sample), len(used_sessions), pdir))


def norm(s):
    return re.sub(r"\s+", " ", (s or "")).strip().lower()


def parse_labels(text):
    """Accept either a JSON array of {id,label,quote} maps or EDN
    {:id ... :label ... :quote ...} maps, one per line or in an array."""
    try:
        arr = json.loads(text)
        if isinstance(arr, list):
            return [(e.get("id"), e.get("label"), e.get("quote")) for e in arr
                    if isinstance(e, dict)]
    except ValueError:
        pass
    pat = re.compile(r'\{:id\s+"([^"]+)"\s+:label\s+"([^"]+)"\s+:quote\s+'
                     r'((?:null|nil)|(?:"(?:[^"\\]|\\.)*"))\s*\}')
    return [(m.group(1), m.group(2),
             None if m.group(3) in ("null", "nil") else json.loads(m.group(3)))
            for m in pat.finditer(text)]


def stage_collect(exp):
    sample = json.load(open(os.path.join(exp, "sample.json")))["items"]
    by_id = {it["id"]: it for it in sample}
    labels = {}
    rdir = os.path.join(exp, "ma-replies")
    for k in range(6):
        raw = os.path.join(rdir, "raw-%d.json" % k)
        if not os.path.isfile(raw):
            sys.exit("missing %s" % raw)
        d = json.load(open(raw))
        text = d.get("response") or ""
        parsed = parse_labels(text)
        if not parsed:
            sys.exit("no labels parsed from raw-%d.json" % k)
        for iid, label, quote in parsed:
            it = by_id.get(iid)
            if not it:
                continue
            verified = None
            if quote:
                hay = " ".join(norm(w["source_text"]) for w in it["window"])
                verified = norm(quote) in hay and len(quote.split()) >= 5
            labels[iid] = {"label": label, "quote": quote,
                           "quote_verified": verified}
        with open(os.path.join(rdir, "reply-%d.md" % k), "w") as fh:
            fh.write(text)
    with open(os.path.join(exp, "labels.json"), "w") as fh:
        json.dump(labels, fh, indent=1)
    print("collected %d labels" % len(labels))


def stage_sheet(exp):
    sample = json.load(open(os.path.join(exp, "sample.json")))["items"]
    rng = random.Random(11)
    by_stratum = collections.defaultdict(list)
    for it in sample:
        by_stratum[it["stratum"]].append(it)
    sheet = []
    for s in ("resolved-act", "resolved-about", "unresolved"):
        sheet.extend(rng.sample(by_stratum[s], 5))
    rng.shuffle(sheet)
    lines = ["# Joe's tension labels (2026-10-25)", "",
             "For each item: the tension the analyst recorded, the end of the",
             "assistant's reply, and the next operator turn. Write one label",
             "after each `label:` — re-raised / resolved / dropped /",
             "not-a-tension.", ""]
    for n, it in enumerate(sheet, 1):
        closing = it["reply_text"][-400:] if it["reply_text"] else "(no reply)"
        words = it["window"][0]["source_text"].split()[:60]
        lines += ["## %d" % n,
                  "tension: %s" % (it["tension"]["text"] or
                                   it["tension"]["about"]),
                  "reply (closing): %s" % " ".join(closing.split()[:40]),
                  "operator turn +1: %s" % " ".join(words),
                  "", "label:", ""]
    out = os.path.join(exp, "joe-15.md")
    with open(out, "w") as fh:
        fh.write("\n".join(lines))
    print("sheet: %s" % out)


def stage_report(exp):
    sample = json.load(open(os.path.join(exp, "sample.json")))["items"]
    labels = json.load(open(os.path.join(exp, "labels.json")))
    matrix = collections.defaultdict(collections.Counter)
    unverified = 0
    for it in sample:
        lab = labels.get(it["id"])
        if not lab:
            matrix[it["stratum"]]["no-label"] += 1
            continue
        if lab["quote_verified"] is False:
            matrix[it["stratum"]]["unverified"] += 1
            unverified += 1
            continue
        matrix[it["stratum"]][lab["label"]] += 1
    print("confusion matrix (rule stratum x ma label):")
    for s in ("resolved-act", "resolved-about", "unresolved"):
        print("  %-16s %s" % (s, dict(matrix[s])))
    all_labels = collections.Counter()
    for c in matrix.values():
        all_labels.update(c)
    n = sum(v for k, v in all_labels.items()
            if k not in ("no-label", "unverified"))
    print("not-a-tension share: %d/%d" % (all_labels["not-a-tension"], n))
    # re-raise recall: ma re-raised items that the rule left unresolved
    ma_re = [it for it in sample
             if (labels.get(it["id"]) or {}).get("label") == "re-raised"
             and labels[it["id"]]["quote_verified"]]
    hit = sum(1 for it in ma_re if it["stratum"] == "unresolved")
    print("re-raise recall: %d/%d" % (hit, len(ma_re)))
    print("unverified quotes: %d" % unverified)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("stage", choices=["sample", "collect", "sheet", "report"])
    ap.add_argument("--exp",
                    default=os.path.expanduser(
                        "~/notes/experiments/2026-10-25-tension-eval"))
    ap.add_argument("--store",
                    default=os.path.expanduser(
                        "~/.emacs-graph/session-turn-analysis"))
    ap.add_argument("--since", default="2026-09-28")
    args = ap.parse_args()
    os.makedirs(args.exp, exist_ok=True)
    {"sample": lambda: stage_sample(args.exp, args.store, args.since),
     "collect": lambda: stage_collect(args.exp),
     "sheet": lambda: stage_sheet(args.exp),
     "report": lambda: stage_report(args.exp)}[args.stage]()


if __name__ == "__main__":
    main()
