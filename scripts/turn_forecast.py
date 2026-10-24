#!/usr/bin/env python3
"""Turn-level forecasts over the 象 turn store (read-only).

Two questions over operator-turn chains (one session, ordered by created_at):

(A) Intent forecast — which intents does turn n+1 carry (multi-label)?
    Predictors: 1 base rate; 2 intents of n (per-intent logistic);
    3 = 2 + intents of n-1; 4 = 3 + reply marks of n (marked replies only,
    with 3 re-run on the same subset).  Brier per intent and mean; recall
    at a fixed threshold for the correction intents.

(B) Uptake forecast — which paragraph of reply n does turn n+1 address?
    Clean labels: a >=4-consecutive-word quote of exactly one paragraph of
    reply n, or a :reply_to mark pointer resolving to one paragraph.
    Noisy labels: best embedding similarity (the Minus40 embedder,
    sentence-transformers/all-MiniLM-L6-v2 in the futon3a venv) between the
    paragraph's bracket target (or text) and n+1's operator about/text.
    Rankers: first paragraph (Gist), last, 🈸-else-Gist, and an embedding
    ranker (paragraph bracket vs turn n's about) evaluated on clean only.

Usage (the embedder lives only in the futon3a venv, so run with that python):
  /home/joe/code/futon3a/.venv/bin/python -I futon3c/scripts/turn_forecast.py \
      --store ~/.emacs-graph/session-turn-analysis \
      --split-date 2026-10-17 \
      --out /tmp/turn_forecast_results.json
"""

from __future__ import annotations

import argparse
import glob
import json
import math
import os
import re
import sys

SINCE = "2026-09-28"
TEST_START = "2026-10-18"          # anything after --split-date and through 10-24
CORRECTION_INTENTS = ["disagree", "redirect", "retract", "withdraw"]
RECALL_THRESHOLD = 0.25
MARK_KEY = {  # ~/code/CLAUDE.md, reply proforma table
    "㊥": "gist", "㊭": "propose", "㊣": "approve", "🈚": "disagree",
    "㊟": "qualify", "🈖": "explain", "🈯": "clarify", "㊢": "report",
    "㊩": "report-problem", "㊬": "verify", "🈹": "retract", "🈳": "unresolved",
    "🈲": "constrain", "🈸": "ask-action", "㊯": "delegate", "🈝": "defer",
    "㊮": "collect", "🈕": "extend", "🈰": "continue", "🈘": "redirect",
}
MARK_RE = re.compile(r"^([㊥㊭㊣🈚㊟🈖🈯㊢㊩㊬🈹🈳🈲🈸㊯🈝㊮🈕🈰🈘])(:?)\s*(.*)$")
BRACKET_RE = re.compile(r"^(?:\"([^\"]+)\"|\(([^)]*)\)|（([^）]*)）)\s*")


# ------------------------------------------------------------------ the store

def load_records(store):
    """Operator-origin turn records since SINCE, id -> record."""
    out = {}
    for path in glob.glob(os.path.join(store, "turn-*.json")):
        name = os.path.basename(path)
        if ".analysis." in name or ".draft." in name:
            continue
        try:
            with open(path) as h:
                rec = json.load(h)
        except Exception:
            continue
        if not isinstance(rec, dict):
            continue
        if rec.get("origin") != "operator":
            continue
        at = str(rec.get("created_at") or "")
        if not at or at < SINCE:
            continue
        rec_id = name[:-len(".json")]
        out[rec_id] = rec
    return out


def operator_intents(rec):
    """The set of 象 kinds of this record's operator-authored acts."""
    return sorted({a.get("kind") for a in (rec.get("acts") or [])
                   if a.get("author") == "operator" and a.get("kind")})


def operator_about(rec):
    """The operator acts' about fields, joined (turn n's 'about' text)."""
    parts = [str(a.get("about")) for a in (rec.get("acts") or [])
             if a.get("author") == "operator" and a.get("about")]
    return " ; ".join(parts)


def reply_paragraphs(rec):
    """Reply n's marked paragraphs, in order:
    [{mark, intent, bracket, text}] — brackets and marks parsed here."""
    text = str(rec.get("reply_text") or "")
    paras = []
    for block in re.split(r"\n\s*\n", text):
        block = block.strip()
        if not block:
            continue
        first = block.split("\n", 1)[0].strip()
        m = MARK_RE.match(first)
        if not m or m.group(1) not in MARK_KEY:
            continue
        rest = m.group(3)
        bm = BRACKET_RE.match(rest)
        bracket = ""
        if bm:
            bracket = next(g for g in bm.groups() if g is not None)
            rest = rest[bm.end():]
        paras.append({"mark": m.group(1), "answering": bool(m.group(2)),
                      "intent": MARK_KEY[m.group(1)],
                      "bracket": bracket.strip(),
                      "text": block})
    return paras


# ------------------------------------------------------------------- the edges

def build_edges(records):
    """Per session, ordered by created_at: edge n -> n+1 with context."""
    by_session = {}
    for rid, rec in records.items():
        sid = rec.get("session_id")
        if sid:
            by_session.setdefault(sid, []).append((rid, rec))
    edges = []
    for sid, turns in by_session.items():
        turns.sort(key=lambda t: str(t[1].get("created_at")))
        for i in range(1, len(turns)):
            prev = turns[i - 1] if i >= 1 else None
            prev2 = turns[i - 2] if i >= 2 else None
            edges.append({
                "session": sid,
                "n1": (prev2[0] if prev2 else None),
                "n": prev[0], "n_rec": prev[1],
                "target": turns[i][0], "target_rec": turns[i][1],
                "at": str(turns[i][1].get("created_at")),
                "in_n": operator_intents(prev[1]),
                "in_n1": (operator_intents(prev2[1]) if prev2 else []),
                "y": operator_intents(turns[i][1]),
                "paras": reply_paragraphs(prev[1]),
            })
    edges.sort(key=lambda e: e["at"])
    return edges


def split_edges(edges, split_date):
    train = [e for e in edges if e["at"][:10] <= split_date]
    test = [e for e in edges if TEST_START <= e["at"][:10] <= "2026-10-24"]
    return train, test


# ------------------------------------------------------ (A) per-intent models

def vocabulary(train):
    kinds = sorted({k for e in train for k in e["y"]} |
                   {k for e in train for k in e["in_n"]} |
                   {k for e in train for k in e["in_n1"]})
    return kinds


def feat_row(edge, kinds, use_n1, mark_vocab, use_marks):
    def bin_(values, vocab):
        s = set(values)
        return [1.0 if v in s else 0.0 for v in vocab]
    row = bin_(edge["in_n"], kinds)
    if use_n1:
        row += bin_(edge["in_n1"], kinds)
    if use_marks:
        row += bin_([p["intent"] for p in edge["paras"]], mark_vocab)
    return row


class Logistic:
    """One weight vector per intent, batch gradient descent with L2."""

    def __init__(self, kinds, iters=600, lr=0.5, l2=1e-3):
        self.kinds, self.iters, self.lr, self.l2 = kinds, iters, lr, l2
        self.W = None
        self.b = None

    def fit(self, rows, ys):
        import numpy as np
        X = np.array(rows)
        Y = np.array([[1.0 if k in y else 0.0 for k in self.kinds] for y in ys])
        n, d = X.shape
        k = len(self.kinds)
        self.W = np.zeros((d, k))
        self.b = np.zeros(k)
        for _ in range(self.iters):
            P = 1.0 / (1.0 + np.exp(-(X @ self.W + self.b)))
            G = (P - Y) / max(n, 1)
            self.W -= self.lr * (X.T @ G + self.l2 * self.W)
            self.b -= self.lr * G.sum(axis=0)
        return self

    def predict(self, rows):
        import numpy as np
        X = np.array(rows)
        P = 1.0 / (1.0 + np.exp(-(X @ self.W + self.b)))
        return [{self.kinds[j]: float(P[i, j]) for j in range(len(self.kinds))}
                for i in range(X.shape[0])]


def base_rate(train, kinds):
    import numpy as np
    ysum = np.array([sum(1 for e in train if k in e["y"]) for k in kinds],
                    dtype=float)
    p = ysum / max(len(train), 1)
    return {k: float(p[i]) for i, k in enumerate(kinds)}


def evaluate(preds, test, kinds):
    """Brier per intent, mean Brier, recall@threshold for correction intents."""
    per = {}
    for k in kinds:
        err = sum((pe[k] - (1.0 if k in e["y"] else 0.0)) ** 2
                  for pe, e in zip(preds, test)) / max(len(test), 1)
        per[k] = err
    recalls = {}
    for k in CORRECTION_INTENTS:
        pos = [i for i, e in enumerate(test) if k in e["y"]]
        hit = sum(1 for i in pos if preds[i][k] >= RECALL_THRESHOLD)
        recalls[k] = {"n": len(pos), "recall": (hit / len(pos)) if pos else None}
    return {"brier_per_intent": per,
            "mean_brier": sum(per.values()) / len(per),
            "recall_at_%.2f" % RECALL_THRESHOLD: recalls,
            "n_test": len(test)}


def run_a(train, test, mark_vocab):
    kinds = vocabulary(train)
    out = {"vocab": kinds, "n_train": len(train)}
    # 1 base rate
    br = base_rate(train, kinds)
    out["p1_base_rate"] = evaluate([br] * len(test), test, kinds)
    out["p1_probs"] = br
    # 2 intents of n
    m2 = Logistic(kinds).fit([feat_row(e, kinds, False, mark_vocab, False)
                              for e in train], [e["y"] for e in train])
    out["p2_n"] = evaluate(m2.predict(
        [feat_row(e, kinds, False, mark_vocab, False) for e in test]), test, kinds)
    # 3 + n-1
    m3 = Logistic(kinds).fit([feat_row(e, kinds, True, mark_vocab, False)
                              for e in train], [e["y"] for e in train])
    p3 = m3.predict([feat_row(e, kinds, True, mark_vocab, False) for e in test])
    out["p3_n_n1"] = evaluate(p3, test, kinds)
    # 4 + reply marks of n, on marked-reply edges only; p3 rerun on same subset
    sub_tr = [e for e in train if e["paras"]]
    sub_te = [e for e in test if e["paras"]]
    m3s = Logistic(kinds).fit([feat_row(e, kinds, True, mark_vocab, False)
                               for e in sub_tr], [e["y"] for e in sub_tr])
    m4 = Logistic(kinds).fit([feat_row(e, kinds, True, mark_vocab, True)
                              for e in sub_tr], [e["y"] for e in sub_tr])
    out["marked_subset_n"] = len(sub_te)
    out["p3_marked_subset"] = evaluate(m3s.predict(
        [feat_row(e, kinds, True, mark_vocab, False) for e in sub_te]), sub_te, kinds)
    out["p4_plus_marks"] = evaluate(m4.predict(
        [feat_row(e, kinds, True, mark_vocab, True) for e in sub_te]), sub_te, kinds)
    # 15 largest errors (p3, full test set): per-edge summed squared error
    errs = sorted(
        ({"error": sum((pe[k] - (1.0 if k in e["y"] else 0.0)) ** 2 for k in kinds),
          "n1": e["n1"], "n": e["n"], "target": e["target"], "at": e["at"],
          "actual": e["y"],
          "predicted": sorted([k for k in kinds if pe[k] >= 0.5], key=lambda k: -pe[k])
                      or sorted(kinds, key=lambda k: -pe[k])[:3],
          "probs_top": sorted(((round(pe[k], 3), k) for k in
                               set(e["y"]) | set(sorted(kinds, key=lambda k: -pe[k])[:3])),
                              reverse=True)[:6],
          "words12": " ".join(str(e["target_rec"].get("source_text") or
                                  "").split()[:12])}
         for pe, e in zip(p3, test)),
        key=lambda d: -d["error"])[:15]
    out["largest_errors_p3"] = errs
    return out


# ---------------------------------------------------------------- (B) uptake

def ngrams(words, n=4):
    return {" ".join(words[i:i + n]) for i in range(len(words) - n + 1)}


def clean_label(edge):
    """Paragraph index addressed by turn n+1, or None.
    Quote: >=4 consecutive words shared with exactly one paragraph (a quote
    spanning several paragraphs is ambiguous, no clean label).
    Pointer: :reply_to marks on record n+1 that name one paragraph of n."""
    if not edge["paras"]:
        return None
    target_words = re.findall(r"\S+", str(edge["target_rec"].get("source_text") or ""))
    if len(target_words) >= 4:
        tg = ngrams(target_words)
        hits = [i for i, p in enumerate(edge["paras"])
                if tg & ngrams(re.findall(r"\S+", p["text"]))]
        if len(hits) == 1:
            return {"para": hits[0], "how": "quote", "hit": hits[0]}
    rt = str(edge["target_rec"].get("reply_to") or "")
    if rt:
        cands = [i for i, p in enumerate(edge["paras"]) if p["mark"] in rt]
        if len(cands) == 1:
            return {"para": cands[0], "how": "reply_to", "hit": cands[0]}
    return None


_embedder = None


def embed(texts):
    """The Minus40 embedder path: sentence-transformers in the futon3a venv
    (minus40/scripts/embed_facts.py), all-MiniLM-L6-v2, L2-normalised."""
    global _embedder
    if _embedder is None:
        from sentence_transformers import SentenceTransformer
        transformer = SentenceTransformer("sentence-transformers/all-MiniLM-L6-v2")

        def make(texts):
            return transformer.encode(texts, normalize_embeddings=True)
        _embedder = make
    return _embedder(list(texts))


def para_repr(p):
    """Bracket target when present, else the paragraph text."""
    return p["bracket"] if p["bracket"] else " ".join(p["text"].split())[:200]


def noisy_label(edge, cache):
    """Best-similarity paragraph vs n+1's operator about/text; with margin."""
    if not edge["paras"] or len(edge["paras"]) < 2:
        return None
    q = operator_about(edge["target_rec"]) or " ".join(
        str(edge["target_rec"].get("source_text") or "").split()[:60])
    if not q.strip():
        return None
    keys = [(edge["n"], i) for i in range(len(edge["paras"]))]
    vecs = []
    for k, p in zip(keys, edge["paras"]):
        if k not in cache:
            cache[k] = embed([para_repr(p)])[0]
        vecs.append(cache[k])
    if q not in cache:
        cache[q] = embed([q])[0]
    qv = cache[q]
    sims = sorted(((float(sum(a * b for a, b in zip(v, qv))), i)
                   for i, v in enumerate(vecs)), reverse=True)
    return {"para": sims[0][1], "sim": sims[0][0],
            "margin": sims[0][0] - sims[1][0]}


def rankers(edge):
    """Each ranker's full ranking of reply n's paragraph indices."""
    paras = edge["paras"]
    if not paras:
        return None
    idx = list(range(len(paras)))
    ask = next((i for i, p in enumerate(paras) if p["mark"] == "🈸"), 0)
    by_dist = lambda g: sorted(idx, key=lambda i: (abs(i - g), i))
    return {"first": idx, "last": list(reversed(idx)),
            "ask_or_gist": by_dist(ask)}


def run_b(train_edges, test_edges):
    import numpy as np
    cache = {}
    out = {}
    for split_name, edges in (("test", test_edges), ("train", train_edges)):
        clean = [(e, clean_label(e)) for e in edges]
        clean = [(e, lab) for e, lab in clean if lab is not None]
        noisy = [(e, noisy_label(e, cache)) for e in edges]
        noisy = [(e, lab) for e, lab in noisy if lab is not None]

        def score(rank_fn, labels):
            """top-1 accuracy and MRR over a full paragraph ranking."""
            top1 = rr = n = 0
            for e, lab in labels:
                ranking = rank_fn(e)
                if not ranking or lab["para"] not in ranking:
                    continue
                n += 1
                if ranking[0] == lab["para"]:
                    top1 += 1
                rr += 1.0 / (1 + ranking.index(lab["para"]))
            return {"n": n, "top1": (top1 / n) if n else None,
                    "mrr": (rr / n) if n else None}

        # the embedding ranker ranks all paragraphs: bracket vs turn n's about
        def emb_rank(e):
            if not e["paras"]:
                return None
            q = operator_about(e["n_rec"]) or " ".join(
                str(e["n_rec"].get("source_text") or "").split()[:60])
            if not q.strip():
                return None
            keys = [(e["n"], i) for i in range(len(e["paras"]))]
            sims = []
            for k, p in zip(keys, e["paras"]):
                if k not in cache:
                    cache[k] = embed([para_repr(p)])[0]
                v = cache[k]
                if q not in cache:
                    cache[q] = embed([q])[0]
                sims.append(float(np.dot(v, cache[q])))
            return sorted(range(len(sims)), key=lambda i: -sims[i])

        res = {"clean_n": len(clean),
               "clean_by_how": {"quote": sum(1 for _, l in clean if l["how"] == "quote"),
                                "reply_to": sum(1 for _, l in clean if l["how"] == "reply_to")},
               "noisy_n": len(noisy)}
        for name in ("first", "last", "ask_or_gist"):
            res[name] = score(lambda e, n=name: (rankers(e) or {}).get(n), clean)
            res[name + "_noisy"] = score(lambda e, n=name: (rankers(e) or {}).get(n), noisy)
        res["embedding_clean"] = score(emb_rank, clean)
        margins = [l["margin"] for _, l in noisy if l["margin"] is not None]
        if margins:
            res["noisy_margin_mean"] = sum(margins) / len(margins)
        # agreement between the noisy label and the clean label, edge-matched
        nl = {id(e): l for e, l in noisy}
        pairs = [(l, nl[id(e)]) for e, l in clean if id(e) in nl]
        agree = sum(1 for c, n in pairs if c["para"] == n["para"])
        res["noisy_agrees_clean"] = (agree, len(pairs))
        out[split_name] = res
    return out


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--store", default=os.path.expanduser(
        "~/.emacs-graph/session-turn-analysis"))
    ap.add_argument("--split-date", default="2026-10-17")
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    records = load_records(args.store)
    edges = build_edges(records)
    triples = sum(1 for e in edges if e["n1"])
    train, test = split_edges(edges, args.split_date)
    mark_vocab = sorted({p["intent"] for e in train for p in e["paras"]})

    result = {
        "store": args.store, "since": SINCE,
        "split_date": args.split_date,
        "test_window": [TEST_START, "2026-10-24"],
        "operator_records": len(records),
        "sessions": len({e["session"] for e in edges}),
        "edges": len(edges),
        "triples_with_n1": triples,
        "train_edges": len(train), "test_edges": len(test),
        "marked_reply_edges_test": sum(1 for e in test if e["paras"]),
        "mark_vocab": mark_vocab,
        "A": run_a(train, test, mark_vocab),
        "B": run_b(train, test),
    }
    with open(args.out, "w") as h:
        json.dump(result, h, indent=1, default=str)
    print("wrote", args.out)
    print("sessions", result["sessions"], "edges", result["edges"],
          "triples", triples, "train", len(train), "test", len(test))


if __name__ == "__main__":
    main()
