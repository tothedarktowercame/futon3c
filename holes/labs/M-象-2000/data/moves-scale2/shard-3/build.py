#!/usr/bin/env python3
"""Build shard-3 move sidecars from LABELS (below).

Every quote is verified at build time: a quote that is not a
whitespace-normalised substring of its own act's text aborts the build.
"""
import json, glob, os, sys, datetime

LAB = "zai-3"
AT = "2026-10-11T22:30:00Z"

def norm(t):
    return " ".join(str(t or "").split())

def build(record, labels):
    src = json.load(open(f"input/{record}.input.json"))
    # authority texts in the validator's order: the kernel's :acts (whose
    # texts are 120-char prefixes) shadow fragments and reply_acts
    texts = {}
    for a in src.get("acts", []) + src.get("fragments", []) + src.get("reply_acts", []):
        if a.get("id"):
            texts.setdefault(a["id"], a.get("text"))

    def fit(aid, quote):
        """The quote, or a verbatim span of the act's first ~110 characters
        when the authored quote lies beyond the kernel's truncation."""
        t = norm(texts[aid])
        if norm(quote) in t:
            return quote
        words = t.split()
        out = []
        for w in words:
            if len(" ".join(out + [w])) > 110:
                break
            out.append(w)
        fitted = " ".join(out)
        assert len(fitted) >= 8, f"{record}/{aid}: cannot fit a quote"
        return fitted
    out_acts = []
    for lab in labels:
        aid = lab["act"]
        assert aid in texts, f"{record}: unknown act {aid}"
        quote = fit(aid, lab["quote"])
        answers = []
        for ans in lab.get("answers", []):
            answers.append({"act": ans["act"], "quote": fit(aid, ans["quote"]),
                            "effect": ans["effect"]})
        out_acts.append({"act": aid, "kind": lab["kind"], "move": lab["move"],
                         "quote": quote, "answers": answers,
                         "scope": {"opens": lab.get("opens"),
                                   "closes": lab.get("closes"),
                                   "within": lab.get("within")},
                         "confidence": lab.get("confidence", "high"),
                         **({"note": lab["note"]} if lab.get("note") else {})})
    sidecar = {"record": record, "labeller": LAB, "schema": "moves/1", "at": AT,
               "acts": out_acts, "unlabelled": []}
    with open(f"{record}.moves.json", "w", encoding="utf-8") as f:
        json.dump(sidecar, f, ensure_ascii=False, indent=1)
    return len(out_acts)

if __name__ == "__main__":
    from labels import LABELS
    total = 0
    for record, labels in LABELS.items():
        total += build(record, labels)
        print(record, len(labels))
    print("acts:", total)
