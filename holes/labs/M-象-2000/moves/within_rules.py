#!/usr/bin/env python3
"""within_rules: classical scope-inference rules measured against the
labelled `within` links in the move-grammar sidecars (discovery only).

    python3 -I moves/within_rules.py            # tables to stdout
    python3 -I moves/within_rules.py --self-test

Rules (each predicts at most one within target per act, or none):
  R0 none; R1 first-fragment; R2 first-framing; R3 nearest-opener;
  R4 nearest-framing; R5 reply-to-gist; R6 kernel in_force.
Gold = every sidecar act whose scope.within names an act id (sidecars whose
`within` is a scope NAME are counted separately, not scored).
"""
import collections
import glob
import json
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
DATA = os.path.join(HERE, "..", "data")
FRAMING = {"constrain", "propose", "ask-action", "prioritize", "redirect"}


def load():
    """[(batch, sidecar, input)] for every labelled sidecar."""
    out = []
    for path in sorted(glob.glob(os.path.join(DATA, "moves-scale*", "shard-*", "turn-*.moves.json"))):
        inp = os.path.join(os.path.dirname(path), "input",
                           os.path.basename(path).replace(".moves.json", ".input.json"))
        if not os.path.exists(inp):
            continue
        batch = path.split(os.sep)[-3]
        out.append((batch, json.load(open(path, encoding="utf-8")),
                    json.load(open(inp, encoding="utf-8"))))
    return out


def _kind(k):
    return k if isinstance(k, str) else json.dumps(k, sort_keys=True)[:40]


def universe(rec, inp):
    """act id -> kind, and the ordered id lists the rules need."""
    kinds = {}
    order = []  # document order of this record's own acts (fragments then reply acts)
    for f in inp.get("fragments", []):
        kinds.setdefault(f["id"], _kind(f.get("intent") or "?"))
    for a in inp.get("acts", []):
        kinds.setdefault(a["id"], _kind(a.get("kind") or "?"))
    for a in inp.get("reply_acts", []):
        kinds.setdefault(a["id"], _kind(a.get("kind") or "reply"))
    for a in inp.get("previous_reply_acts", []):
        kinds.setdefault(a["id"], _kind(a.get("kind") or "reply"))
    for p in inp.get("open_ports", []):
        kinds.setdefault(p.get("act") or p.get("id"), _kind(p.get("kind") or "?"))
    for a in rec.get("acts", []):  # sidecar's own kinds as final fallback
        kinds.setdefault(a["act"], _kind(a.get("kind") or "?"))
    frag_ids = [f["id"] for f in inp.get("fragments", [])]
    reply_ids = [a["id"] for a in inp.get("reply_acts", [])]
    order = frag_ids + reply_ids
    in_force = [x["act"] if isinstance(x, dict) else x for x in (inp.get("in_force") or [])]
    if in_force:
        for x in (inp.get("in_force") or []):
            if isinstance(x, dict) and x.get("kind") is not None:
                kinds.setdefault(x["act"], _kind(x.get("kind")))
    ports = [p.get("act") or p.get("id") for p in inp.get("open_ports", [])]
    prev = [a["id"] for a in inp.get("previous_reply_acts", [])]
    own = set(frag_ids) | set(reply_ids) | {a["id"] for a in inp.get("acts", [])}
    return kinds, order, frag_ids, reply_ids, in_force, ports, prev, own


def rules_for(rec, inp):
    """id -> {rule: predicted target or None} for this record's acts."""
    kinds, order, frags, replies, in_force, ports, prev, own = universe(rec, inp)
    pos = {a: i for i, a in enumerate(order)}
    opens_at = {a["act"]: a["scope"].get("opens")
                for a in rec.get("acts", []) if a["scope"].get("opens")}
    first_frag = frags[0] if frags else None
    framing = [f["id"] for f in inp.get("fragments", [])
               if (f.get("intent") or "?").split(":")[0] in FRAMING
               or (f.get("intent") or "?") in FRAMING]
    first_framing = framing[0] if framing else None
    out = {}
    for a in rec.get("acts", []):
        aid = a["act"]
        p = pos.get(aid)
        is_reply = "-r-" in aid
        preds = {}
        preds["R0"] = None
        preds["R1"] = first_frag if (first_frag and first_frag != aid) else None
        preds["R2"] = (first_framing if (first_framing and p is not None
                                         and first_framing in pos and pos[first_framing] < p) else None)
        # R3 nearest earlier act with a labelled scope.opens
        best = None
        if p is not None:
            for b in order[:p]:
                if b in opens_at:
                    best = b
        preds["R3"] = best
        # R4 nearest earlier framing fragment
        best = None
        if p is not None:
            for b in order[:p]:
                if b in framing:
                    best = b
        preds["R4"] = best
        preds["R5"] = (replies[0] if (is_reply and replies and aid != replies[0])
                       else preds["R4"])
        preds["R6"] = in_force[-1] if in_force else None
        out[aid] = preds
    return out, kinds, own


COMBINED = "the nearest earlier act with a labelled scope.opens (R3); if none, a reply act beyond r-0 takes its record's r-0 gist (R5); else the nearest earlier framing fragment (R4); else the first fragment (R1)"


def combined(preds, aid):
    if preds["R3"] is not None:
        return preds["R3"]
    if preds["R5"] is not None and preds["R5"] != preds["R4"]:
        return preds["R5"]
    for r in ("R4", "R1"):
        if preds[r] is not None:
            return preds[r]
    return None


def measure(gold, predict, kinds):
    """gold/predict: act-key -> target. Returns a stats dict."""
    stats = collections.defaultdict(lambda: {"n": 0, "pred": 0, "correct": 0,
                                             "conf": collections.Counter()})
    for key, truth in gold.items():
        batch = key[0]
        aid = key[2]
        atype = "reply" if "-r-" in aid else "fragment"
        p = predict(key)
        for scope in (("all",), (batch,), (atype,), (batch, atype)):
            s = stats[scope]
            s["n"] += 1
            if p is not None:
                s["pred"] += 1
                if p == truth:
                    s["correct"] += 1
                else:
                    s["conf"][(kinds.get(p, "?"), kinds.get(truth, "?"))] += 1
    return stats


def table(name, stats):
    def pr(s):
        return "%.3f" % (s["correct"] / s["pred"]) if s["pred"] else "-"
    def rc(s):
        return "%.3f" % (s["correct"] / s["n"]) if s["n"] else "-"
    rows = [("all",), ("moves-scale",), ("moves-scale2",), ("moves-scale3",),
            ("fragment",), ("reply",), ("moves-scale", "fragment"), ("moves-scale", "reply"),
            ("moves-scale2", "fragment"), ("moves-scale2", "reply"),
            ("moves-scale3", "fragment"), ("moves-scale3", "reply")]
    print("\n### %s" % name)
    print("| slice | gold | predicted | correct | precision | recall |")
    print("|---|---|---|---|---|---|")
    for scope in rows:
        s = stats[scope]
        label = "/".join(scope)
        print("| %s | %d | %d | %d | %s | %s |" % (label, s["n"], s["pred"], s["correct"], pr(s), rc(s)))
    s = stats[("all",)]
    print("\ntop confusions (predicted kind, gold kind):")
    for (pk, gk), n in s["conf"].most_common(3):
        print("- predicted `%s` but gold `%s`: %d" % (pk, gk, n))


def main():
    data = load()
    gold = {}
    named = 0
    all_preds = {}   # (batch, aid) -> {rule: pred}
    kinds = {}
    targets = collections.Counter()
    for i, (batch, rec, inp) in enumerate(data):
        preds, ks, own = rules_for(rec, inp)
        kinds.update(ks)
        same = set(ks)
        in_force = [x["act"] if isinstance(x, dict) else x for x in (inp.get("in_force") or [])]
        ports = {p.get("act") or p.get("id") for p in inp.get("open_ports", [])}
        prev = {a["id"] for a in inp.get("previous_reply_acts", [])}
        recid = "%s#%d" % (rec.get("record"), i)
        for j, a in enumerate(rec.get("acts", [])):
            aid = a["act"]
            key = (batch, recid + "#%d" % j, aid)
            all_preds[key] = preds.get(aid, {})
            w = a["scope"].get("within")
            if w is None:
                continue
            if w in same or w in ports or w in prev or w in in_force:
                gold[key] = w
                if w in own:
                    targets["a: same record"] += 1
                elif w in in_force:
                    targets["b: in_force id"] += 1
                elif w in ports:
                    targets["c: open_ports id"] += 1
                elif w in prev:
                    targets["d: previous_reply_acts id"] += 1
            else:
                named += 1
    print("# within_rules: classical rules vs the labelled scope links")
    print("\nsidecars: %d; gold within links (act ids): %d; within-as-name (not scored): %d"
          % (len(data), len(gold), named))
    total = len(gold)
    print("\n## Gold target breakdown")
    for k in ("a: same record", "b: in_force id", "c: open_ports id", "d: previous_reply_acts id"):
        print("- %s: %d (%.1f%%)" % (k, targets[k], 100.0 * targets[k] / total))
    rest = total - sum(targets.values())
    print("- e: none of these: %d (%.1f%%)" % (rest, 100.0 * rest / total))

    rules = ["R0", "R1", "R2", "R3", "R4", "R5", "R6"]
    desc = {"R0": "none (baseline)", "R1": "first-fragment", "R2": "first-framing",
            "R3": "nearest-opener", "R4": "nearest-framing", "R5": "reply-to-gist (+R4 for fragments)",
            "R6": "kernel in_force (present only in some batch-3 inputs)"}
    for r in rules:
        table("R%s — %s" % (r[1], desc[r]),
              measure(gold, lambda key, r=r: all_preds.get(key, {}).get(r), kinds))

    def comb(key):
        p = all_preds.get(key, {})
        return combined(p, key[1])
    table("Combined — %s" % COMBINED, measure(gold, comb, kinds))

    print("\n## Ten instructive combined-rule misses")
    print("(one per distinct predicted-kind/gold-kind confusion first, then fill)")
    quotes = {}
    for batch, rec, inp in data:
        for a in rec.get("acts", []):
            quotes.setdefault((rec.get("record"), a["act"]), a.get("quote", ""))
    by_conf = collections.defaultdict(list)
    for key, truth in sorted(gold.items()):
        p = comb(key)
        if p == truth:
            continue
        by_conf[(kinds.get(p, "?"), kinds.get(truth, "?"))].append((key, p, truth))
    picks = []
    for conf in sorted(by_conf, key=lambda c: -len(by_conf[c])):
        picks.append(by_conf[conf][0])
        if len(picks) >= 10:
            break
    for (key, p, truth) in picks:
        batch, recid, aid = key
        print("- %s [%s] predicted %s (%s), gold %s (%s) — quote: %r"
              % (aid, batch, p, kinds.get(p, "?"), truth, kinds.get(truth, "?"),
                 quotes.get((recid.split("#")[0], aid), "")[:100]))
    return 0


def self_test():
    rec1 = {"acts": [
        {"act": "x-f-s1-0", "kind": "propose", "move": "propose/other", "quote": "let's build the thing",
         "answers": [], "scope": {"opens": "the-plan", "closes": None, "within": None}, "confidence": "high"},
        {"act": "x-f-s2-0", "kind": "report", "move": "report/status", "quote": "it is half built",
         "answers": [], "scope": {"opens": None, "closes": None, "within": "x-f-s1-0"}, "confidence": "high"},
        {"act": "x-r-0", "kind": "reply-mark", "move": "report/done", "quote": "done with the first half",
         "answers": [], "scope": {"opens": None, "closes": None, "within": "x-f-s1-0"}, "confidence": "high"},
        {"act": "x-r-1", "kind": "report", "move": "report/status", "quote": "and here is the detail",
         "answers": [], "scope": {"opens": None, "closes": None, "within": "x-r-0"}, "confidence": "high"}]}
    inp1 = {"fragments": [{"id": "x-f-s1-0", "intent": "propose", "text": "let's build the thing"},
                          {"id": "x-f-s2-0", "intent": "report", "text": "it is half built"}],
            "reply_acts": [{"id": "x-r-0", "kind": "reply-mark", "text": "done with the first half"},
                           {"id": "x-r-1", "kind": "report", "text": "and here is the detail"}],
            "previous_reply_acts": [], "open_ports": []}
    preds, kinds, _own = rules_for(rec1, inp1)
    # R1: every other act -> first fragment x-f-s1-0 (3 predictions, 2 correct)
    r1 = [preds[a]["R1"] for a in ("x-f-s2-0", "x-r-0", "x-r-1")]
    assert r1 == ["x-f-s1-0"] * 3, r1
    # R5: reply i>0 -> r-0; fragments fall back to R4 (nearest framing = s1-0)
    r5 = [preds[a]["R5"] for a in ("x-f-s1-0", "x-f-s2-0", "x-r-0", "x-r-1")]
    assert r5 == [None, "x-f-s1-0", "x-f-s1-0", "x-r-0"], r5  # r-0 itself falls back to R4
    print("self-test OK (R1 and R5 assertions pass)")
    return 0


if __name__ == "__main__":
    sys.exit(self_test() if "--self-test" in sys.argv else main())
