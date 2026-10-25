#!/usr/bin/env python3
"""Evaluate the tension-resolution sensor against 馬-sonnet reference labels.

Stages:
  sample   draw 60 stratified X tensions (20 resolved-by-act, 20
           resolved-by-different-about, 20 unresolved) across >=10 sessions,
           excluding fragments that quote agent text, with reply_text read
           from the record file; writes sample.json (+ prompts)
  collect  read ma-replies/raw-<k>.json, parse labels (new five-label set),
           string-check quotes, write labels.json
  sheet    write joe-15.md (15 items, 5 per stratum, shuffled, blind)
  report   print the confusion matrix and shares

v2 (2026-10-25 follow-up): reply_text read from the record file (the v1 bug:
load_turns never carried it, so every prompt showed an empty reply);
fragments that quote agent text (>>> or a proforma mark followed by a
bracket) are dropped; label set gains re-raised:not-fixed /
re-raised:variation.

Read-only over the turn store; writes only under the experiment directory.
"""

import argparse
import collections
import json
import os
import random
import re
import sys

sys.path.insert(0, os.path.join(os.path.dirname(__file__)))
import tension_measure as tm  # noqa: E402

RESOLVE_ACTS = {"approve", "agree", "continue"}
SEED = 7
LABELS = ("resolved", "re-raised:not-fixed", "re-raised:variation",
          "dropped", "not-a-tension")

# Proforma marks from ~/code/CLAUDE.md's key (agent reply vocabulary).
PROFORMA_MARKS = "㊥㊭㊢㊣🈚㊟🈯🈩㊩🈘㊬㊫🈹🈡🈳㊝㊮🈕🈰🈸🈲🈝"


def is_quoted_agent_text(text):
    """A tension fragment that quotes agent text rather than Joe's speech."""
    t = (text or "").lstrip()
    return (t.startswith(">>>")
            or any(t.startswith(m + "(") for m in PROFORMA_MARKS)
            or t.startswith('("'))


def filtered_xiang_tensions(turn):
    return [t for t in tm.xiang_tensions(turn)
            if not is_quoted_agent_text(t["text"])]


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


def reply_of(turn):
    """reply_text from the record file itself (load_turns drops it)."""
    try:
        with open(turn["path"], encoding="utf-8") as fh:
            return (json.load(fh).get("reply_text") or "")[:1500]
    except (OSError, ValueError, AttributeError):
        return ""


def turn_payload(t):
    return {"record": t["record"], "created_at": t["created_at"],
            "source_text": t["source_text"],
            "acts": [{"kind": a.get("kind"), "about": a.get("about")}
                     for a in t["acts"]]}


def stage_sample(exp, store, since):
    sessions = load_sessions(store, since)
    dropped_quotes = 0
    by_stratum = collections.defaultdict(list)
    for sid, sturns in sessions.items():
        for i, turn in enumerate(sturns):
            window = sturns[i + 1: i + 3]
            if len(window) < 2:
                continue
            tensions = tm.xiang_tensions(turn)
            kept = [t for t in tensions if not is_quoted_agent_text(t["text"])]
            dropped_quotes += len(tensions) - len(kept)
            for tension in kept:
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
                    "reply_text": reply_of(turn),
                    "window": [turn_payload(w) for w in window]})
    rng = random.Random(SEED)
    used_sessions = collections.Counter()

    def spread(items, n):
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
    carried = 0
    v1path = os.path.join(exp, "sample-v1.json")
    if os.path.isfile(v1path):
        # Carry over every v1 item that survives the filter, so the sheet
        # items and the old labels stay comparable; then top up per stratum.
        v1 = json.load(open(v1path))["items"]
        v1_keys = set()
        for it in v1:
            if is_quoted_agent_text(it["tension"]["text"]):
                continue
            t = {"id": it["id"], "session_id": it["session_id"],
                 "stratum": it["stratum"], "tension": it["tension"],
                 "reply_text": None, "window": it["window"]}
            # reply from the record file, looked up below by record id
            sample.append(t)
            v1_keys.add(key_of(it))
            carried += 1
        # attach replies and drop v1 items whose record has vanished
        sample = [t for t in sample]
        by_record = {}
        for sid, sturns in sessions.items():
            for turn in sturns:
                by_record[turn["record"]] = turn
        sample = [t for t in sample if t["id"].split("/")[0] in by_record]
        carried = len(sample)
        for t in sample:
            t["reply_text"] = reply_of(by_record[t["id"].split("/")[0]])
        have = {key_of(t) for t in sample}
        pool = {s: [it for it in by_stratum[s] if key_of(it) not in have
                    and key_of(it) not in v1_keys]
                for s in by_stratum}
        # re-id top-up items so they cannot collide with carried v1 ids
        for s in pool:
            for n, it in enumerate(pool[s]):
                it["id"] = "%s/b%d" % (it["id"].split("/")[0], n)

        def topup(stratum, n):
            need = n - sum(1 for t in sample if t["stratum"] == stratum)
            sample.extend(spread(pool[stratum], max(0, need)))

        for stratum, n in (("resolved-act", 20), ("resolved-about", 20),
                           ("unresolved", 20)):
            topup(stratum, n)
    else:
        for stratum, n in (("resolved-act", 20), ("resolved-about", 20),
                           ("unresolved", 20)):
            sample.extend(spread(by_stratum[stratum], n))
    assert len(used_sessions) >= 10 or carried >= 20, len(used_sessions)
    with open(os.path.join(exp, "sample.json"), "w") as fh:
        json.dump({"seed": SEED, "store": store, "since": since,
                   "dropped_quoted_fragments": dropped_quotes,
                   "items": sample}, fh, indent=1)

    pdir = os.path.join(exp, "ma-prompts")
    os.makedirs(pdir, exist_ok=True)
    for k in range(6):
        chunk = sample[k * 10:(k + 1) * 10]
        lines = [
            "You are labelling whether an operator tension was resolved. For",
            "each item you get a tension the analyst raised (a paraphrase of",
            "the operator's concern), the closing part of the assistant's",
            "reply that followed, and the next two operator turns. Label each",
            "item:",
            "  resolved             the operator accepts the answer, or the",
            "                       concern is dealt with",
            "  re-raised:not-fixed  the operator returns to the same concern",
            "                       because it is not fixed yet (a try-harder",
            "                       push)",
            "  re-raised:variation  the operator accepts the answer but raises",
            "                       a subtly different way of going about it",
            "  dropped              the operator moves on without either",
            "  not-a-tension        the fragment raises no tension (ordinary",
            "                       qualification, question, or aside)",
            "Give a verbatim quote of at least 5 words from the later operator",
            "turns that supports the label (null only for dropped). For",
            "re-raised:variation also give :variation, a verbatim quote of",
            "what the variation changes. Reply with one EDN map per item:",
            '{:id "..." :label "..." :quote "..." :variation "..." or null}.',
            ""]
        for it in chunk:
            w0, w1 = it["window"]
            reply = it["reply_text"]
            lines += ["## item %s" % it["id"],
                      "tension about: %s" % it["tension"]["about"],
                      "tension text: %s" % it["tension"]["text"],
                      "reply (closing): %s" % (reply[-1200:] if reply
                                               else "(no reply recorded)"),
                      "operator turn +1: %s" % w0["source_text"][:700],
                      "  acts: %s" % json.dumps(w0["acts"]),
                      "operator turn +2: %s" % w1["source_text"][:700],
                      "  acts: %s" % json.dumps(w1["acts"]), ""]
        with open(os.path.join(pdir, "whistle-%d.md" % k), "w") as fh:
            fh.write("\n".join(lines))
    print("sample: %d items over %d sessions; %d quoted-agent fragments "
          "dropped; prompts in %s" % (len(sample), len(used_sessions),
                                      dropped_quotes, pdir))


def norm(s):
    return re.sub(r"\s+", " ", (s or "")).strip().lower()


def parse_labels(text):
    """Accept a JSON array of maps or EDN {:id .. :label .. :quote ..
    :variation ..} maps (label may itself contain a colon)."""
    try:
        arr = json.loads(text)
        if isinstance(arr, list):
            return [(e.get("id"), e.get("label"), e.get("quote"),
                     e.get("variation")) for e in arr if isinstance(e, dict)]
    except ValueError:
        pass
    pat = re.compile(r'\{:id\s+"([^"]+)"\s+:label\s+"([^"]+)"\s+:quote\s+'
                     r'((?:null|nil)|(?:"(?:[^"\\]|\\.)*"))'
                     r'(?:\s+:variation\s+((?:null|nil)|(?:"(?:[^"\\]|\\.)*")))'
                     r'\s*\}')
    out = []
    for m in pat.finditer(text):
        quote = None if m.group(3) in ("null", "nil") else json.loads(m.group(3))
        var = None if m.group(4) in (None, "null", "nil") \
            else json.loads(m.group(4))
        out.append((m.group(1), m.group(2), quote, var))
    return out


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
        for iid, label, quote, variation in parsed:
            it = by_id.get(iid)
            if not it:
                continue
            hay = " ".join(norm(w["source_text"]) for w in it["window"])
            verified = None
            if quote:
                verified = norm(quote) in hay and len(quote.split()) >= 5
            var_ok = True if not variation else norm(variation) in hay
            labels[iid] = {"label": label, "quote": quote,
                           "quote_verified": verified,
                           "variation": variation, "variation_verified": var_ok}
        with open(os.path.join(rdir, "reply-%d.md" % k), "w") as fh:
            fh.write(text)
    with open(os.path.join(exp, "labels.json"), "w") as fh:
        json.dump(labels, fh, indent=1)
    print("collected %d labels" % len(labels))


def sheet_items(sample):
    """The seed-11 sheet selection over a sample (5 per stratum, shuffled)."""
    rng = random.Random(11)
    by_stratum = collections.defaultdict(list)
    for it in sample:
        by_stratum[it["stratum"]].append(it)
    sheet = []
    for s in ("resolved-act", "resolved-about", "unresolved"):
        sheet.extend(rng.sample(by_stratum[s], 5))
    rng.shuffle(sheet)
    return sheet


def key_of(item):
    """Stable join key across samples: record + tension text. The printed
    ids embed per-stratum counters, which the fragment filter shifts."""
    return (item["record"] if "record" in item else item["id"].split("/")[0],
            item["tension"]["text"])


def stage_sheet(exp, v2=False):
    sample = json.load(open(os.path.join(exp, "sample.json")))["items"]
    if v2:
        old = json.load(open(os.path.join(exp, "sample-v1.json")))["items"]
        by_key = {key_of(it): it for it in sample}
        items = [by_key.get(key_of(it), it)
                 for it in sheet_items(old)
                 if not is_quoted_agent_text(it["tension"]["text"])
                 and key_of(it) in by_key]
    else:
        items = sheet_items(sample)
    lines = ["# Joe's tension labels (2026-10-25%s)" % (", v2" if v2 else ""),
             "",
             "For each item: the tension the analyst recorded, the closing",
             "part of the assistant's reply, and the next operator turn.",
             "Write one label after each `label:` — resolved /",
             "re-raised:not-fixed / re-raised:variation / dropped /",
             "not-a-tension.", ""]
    for n, it in enumerate(items, 1):
        closing = it.get("reply_text") or ""
        closing = closing[-400:] if closing else "(no reply recorded)"
        words = it["window"][0]["source_text"].split()[:60]
        lines += ["## %d" % n,
                  "tension: %s" % (it["tension"]["text"] or
                                   it["tension"]["about"]),
                  "reply (closing): %s" % " ".join(closing.split()[:40]),
                  "operator turn +1: %s" % " ".join(words),
                  "", "label:", ""]
    out = os.path.join(exp, "joe-15-v2.md" if v2 else "joe-15.md")
    with open(out, "w") as fh:
        fh.write("\n".join(lines))
    print("sheet: %s (%d items)" % (out, len(items)))


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
    re_labels = [l for l in all_labels
                 if l.startswith("re-raised")]
    ma_re = [it for it in sample
             if str((labels.get(it["id"]) or {}).get("label", "")).startswith(
                 "re-raised")
             and labels[it["id"]]["quote_verified"]]
    hit = sum(1 for it in ma_re if it["stratum"] == "unresolved")
    print("re-raise recall: %d/%d" % (hit, len(ma_re)))
    print("unverified quotes: %d" % unverified)


def stage_compare(exp):
    """v2-only: 馬-new vs Joe's sheet labels; 馬-old vs 馬-new."""
    new = json.load(open(os.path.join(exp, "sample.json")))["items"]
    new_labels = json.load(open(os.path.join(exp, "labels.json")))
    v1path = os.path.join(exp, "labels-v1.json")
    v1sample = json.load(open(os.path.join(exp, "sample-v1.json")))["items"]

    # Joe's labels, read positionally from the filled sheet (seed-11 order).
    sheet = open(os.path.join(exp, "joe-15.md")).read()
    joe = [m.group(1).strip() for m in
           re.finditer(r"^label:[ \t]*(.*)$", sheet, re.M)]
    old_items = sheet_items(v1sample)
    new_by_key = {key_of(it): it for it in new}
    agree = seen = 0
    for it, j in zip(old_items, joe):
        if not j:
            continue
        v2 = new_by_key.get(key_of(it))
        if not v2:
            continue
        lab = new_labels.get(v2["id"])
        if not lab or lab["quote_verified"] is False:
            continue
        ml = lab["label"]
        m = ("re-raised" if ml.startswith("re-raised") else ml)
        seen += 1
        if m == j:
            agree += 1
    print("ma-new vs Joe: %d/%d agree (either re-raised subtype matches "
          "Joe's re-raised)" % (agree, seen))

    if os.path.isfile(v1path):
        old_labels = json.load(open(v1path))
        v1_by_key = {key_of(it): it for it in v1sample}
        agree = seen = 0
        mat = collections.defaultdict(collections.Counter)
        for it in new:
            v1 = v1_by_key.get(key_of(it))
            ol = old_labels.get(v1["id"]) if v1 else None
            nl = new_labels.get(it["id"])
            if not ol or not nl or nl["quote_verified"] is False:
                continue
            seen += 1
            old_l = ("re-raised" if str(ol["label"]).startswith("re-raised")
                     else ol["label"])
            mat[old_l][nl["label"]] += 1
            if old_l == nl["label"] or (
                    old_l == "re-raised" and nl["label"].startswith(
                        "re-raised")):
                agree += 1
        print("ma-old vs ma-new: %d/%d agree (re-raised subtypes count as "
              "re-raised)" % (agree, seen))
        for k in ("resolved", "re-raised", "dropped", "not-a-tension"):
            print("  old %-14s %s" % (k, dict(mat[k])))


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("stage", choices=["sample", "collect", "sheet",
                                      "sheet-v2", "report", "compare"])
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
     "sheet-v2": lambda: stage_sheet(args.exp, v2=True),
     "report": lambda: stage_report(args.exp),
     "compare": lambda: stage_compare(args.exp)}[args.stage]()


if __name__ == "__main__":
    main()
