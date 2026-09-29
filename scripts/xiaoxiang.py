#!/usr/bin/env python3
"""小象 (xiaoxiang) v0.1 -- a classical intent classifier for operator turns.

象 labels Joe's real turns in futon1b's world: each fragment of a turn gets one
of ~26 intents (propose, explain, approve, report-problem, ...).  小象 learns to
predict those labels from the fragment's words alone, with no model service and
no store, so it can run on anyone's logs.  v0.1 is multinomial naive Bayes over
word unigrams and bigrams (CJK as character bigrams).  It is expected to do
poorly; the point of v0.1 is to measure where.

  xiaoxiang.py eval  [--dir DIR]            grouped held-out evaluation
  xiaoxiang.py export [--dir DIR] OUT.json  model with a filtered vocabulary
  xiaoxiang.py classify MODEL.json "text"   top intents for one fragment
  xiaoxiang.py page OUT.html                web page: live classifier + results

Evaluation holds out whole turns, so no fragment is scored by a model that saw
another fragment of the same turn.  Labels are 象's and are not human-approved;
"collisions" (the same short text under different intents) mix real ambiguity
with labeller disagreement, and v0.1 does not separate the two.

The exported vocabulary keeps only tokens seen in at least MIN_TURNS distinct
turns, and drops id-like tokens and every word from a span secret_scan flags in
the original text, so the model file does not carry rare strings or secrets
out of the logs.  Common first names can still appear; filter before publishing.

Standard library only.
"""
from __future__ import annotations

import argparse
from collections import Counter, defaultdict
import glob
import hashlib
import json
import math
import os
import re
import sys

DEFAULT_DIR = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
MIN_TURNS = 3
CJK = "一-鿿㐀-䶿"
WORD = re.compile(r"[a-z][a-z']*|[%s]+" % CJK)
IDLIKE = re.compile(r"\d|^[a-f0-9]{8,}$")


def tokens(text: str) -> list[str]:
    """Words, word bigrams, and CJK character bigrams; digits are ignored."""
    out: list[str] = []
    words: list[str] = []
    for piece in WORD.findall(text.lower()):
        if re.match("[%s]" % CJK, piece):
            chars = list(piece)
            out.extend(chars)
            out.extend(a + b for a, b in zip(chars, chars[1:]))
        else:
            words.append(piece)
    out.extend(words)
    out.extend(f"{a} {b}" for a, b in zip(words, words[1:]))
    return out


def load(directory: str) -> list[dict]:
    """One row per labelled fragment: turn id, labeller, text, intent."""
    rows = []
    for path in sorted(glob.glob(os.path.join(directory, "*.analysis.json"))):
        try:
            with open(path, encoding="utf-8") as fh:
                doc = json.load(fh)
        except (OSError, ValueError):
            continue
        turn = os.path.basename(path).split(".")[0]
        for sentence in doc.get("sentences") or []:
            for frag in sentence.get("fragments") or []:
                intent, text = frag.get("intent"), frag.get("text")
                if isinstance(intent, str) and isinstance(text, str) and text.strip():
                    rows.append({"turn": turn, "labeller": str(doc.get("labeller")),
                                 "text": text, "intent": intent})
    return rows


class NaiveBayes:
    def __init__(self, alpha: float = 0.5):
        self.alpha = alpha
        self.prior: Counter = Counter()
        self.counts: dict[str, Counter] = defaultdict(Counter)
        self.totals: Counter = Counter()
        self.vocab: set[str] = set()

    def fit(self, rows, keep=None):
        for r in rows:
            self.prior[r["intent"]] += 1
            for t in tokens(r["text"]):
                if keep is None or t in keep:
                    self.counts[r["intent"]][t] += 1
                    self.totals[r["intent"]] += 1
                    self.vocab.add(t)
        return self

    def scores(self, text: str) -> dict[str, float]:
        n = sum(self.prior.values())
        v = len(self.vocab) or 1
        toks = [t for t in tokens(text) if t in self.vocab]
        out = {}
        for c in self.prior:
            denom = self.totals[c] + self.alpha * v
            s = math.log(self.prior[c] / n)
            for t in toks:
                s += math.log((self.counts[c][t] + self.alpha) / denom)
            out[c] = s
        return out

    def predict(self, text: str) -> str:
        s = self.scores(text)
        return max(s, key=s.get)


def fold_of(turn: str, k: int) -> int:
    return int(hashlib.sha256(turn.encode()).hexdigest(), 16) % k


def evaluate(rows, k: int = 5) -> dict:
    """k-fold cross-validation grouped by turn."""
    gold, pred = [], []
    for i in range(k):
        train = [r for r in rows if fold_of(r["turn"], k) != i]
        test = [r for r in rows if fold_of(r["turn"], k) == i]
        model = NaiveBayes().fit(train)
        majority = model.prior.most_common(1)[0][0]
        for r in test:
            gold.append(r["intent"])
            pred.append(model.predict(r["text"]) if model.vocab else majority)
    labels = sorted(set(gold))
    acc = sum(g == p for g, p in zip(gold, pred)) / len(gold)
    per = {}
    for c in labels:
        tp = sum(g == c and p == c for g, p in zip(gold, pred))
        fp = sum(g != c and p == c for g, p in zip(gold, pred))
        fn = sum(g == c and p != c for g, p in zip(gold, pred))
        prec = tp / (tp + fp) if tp + fp else 0.0
        rec = tp / (tp + fn) if tp + fn else 0.0
        per[c] = {"support": tp + fn, "precision": prec, "recall": rec,
                  "f1": 2 * prec * rec / (prec + rec) if prec + rec else 0.0}
    majority_label, majority_n = Counter(gold).most_common(1)[0]
    confusions = Counter((g, p) for g, p in zip(gold, pred) if g != p)
    return {
        "fragments": len(gold), "turns": len({r["turn"] for r in rows}),
        "intents": len(labels), "folds": k,
        "accuracy": acc,
        "macro_f1": sum(v["f1"] for v in per.values()) / len(per),
        "majority_baseline": {"intent": majority_label, "accuracy": majority_n / len(gold)},
        "per_intent": per,
        "top_confusions": [{"gold": g, "predicted": p, "count": n}
                           for (g, p), n in confusions.most_common(15)],
    }


def _scanner():
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    try:
        import secret_scan  # noqa: PLC0415
        return secret_scan.scan
    except ImportError:
        return None


def tainted_words(text: str, scan=None) -> set[str]:
    """Words that must not leave the logs: anything inside a span secret_scan
    flags in the ORIGINAL text (before lowercasing strips its structure), any
    raw word mixing letters with digits, and any word over 20 letters."""
    spans = [(f.start, f.end) for f in scan(text)] if scan else []
    bad: set[str] = set()
    for m in re.finditer(r"\S+", text):
        raw = m.group()
        flagged = any(a < m.end() and m.start() < b for a, b in spans)
        mixed = bool(re.search(r"[A-Za-z]", raw) and re.search(r"\d", raw))
        for w in WORD.findall(raw.lower()):
            if flagged or mixed or len(w) > 20:
                bad.add(w)
    return bad


def safe_vocab(rows) -> set[str]:
    """Tokens seen in >= MIN_TURNS distinct turns, not id-like, not tainted."""
    scan = _scanner()
    turns_by_tok: dict[str, set] = defaultdict(set)
    tainted: set[str] = set()
    for r in rows:
        tainted |= tainted_words(r["text"], scan)
        for t in set(tokens(r["text"])):
            turns_by_tok[t].add(r["turn"])
    keep = set()
    for t, turns in turns_by_tok.items():
        if len(turns) < MIN_TURNS:
            continue
        parts = t.split(" ")
        if any(p in tainted for p in parts):
            continue
        if any(IDLIKE.search(w) for w in parts if not re.match("[%s]" % CJK, w)):
            continue
        keep.add(t)
    return keep


def collisions(rows, keep: set[str], max_words: int = 3) -> list[dict]:
    """Short texts labelled with more than one intent, restricted to safe words."""
    by_text: dict[str, Counter] = defaultdict(Counter)
    for r in rows:
        norm = " ".join(WORD.findall(r["text"].lower()))
        words = norm.split()
        if 0 < len(words) <= max_words and all(w in keep for w in words):
            by_text[norm][r["intent"]] += 1
    out = [{"text": t, "intents": dict(c.most_common())}
           for t, c in by_text.items() if len(c) > 1]
    out.sort(key=lambda d: -sum(d["intents"].values()))
    return out


def export(rows, keep: set[str]) -> dict:
    model = NaiveBayes().fit(rows, keep=keep)
    return {
        "name": "xiaoxiang", "version": "0.1", "alpha": model.alpha,
        "prior": dict(model.prior), "totals": dict(model.totals),
        "vocab_size": len(model.vocab),
        "counts": {c: dict(v) for c, v in model.counts.items()},
    }


def classify(model: dict, text: str, n: int = 3) -> list[tuple[str, float]]:
    prior, totals, counts = model["prior"], model["totals"], model["counts"]
    v, alpha, total = model["vocab_size"] or 1, model["alpha"], sum(prior.values())
    vocab = {t for c in counts.values() for t in c}
    toks = [t for t in tokens(text) if t in vocab]
    scores = {}
    for c in prior:
        denom = totals.get(c, 0) + alpha * v
        scores[c] = math.log(prior[c] / total) + sum(
            math.log((counts.get(c, {}).get(t, 0) + alpha) / denom) for t in toks)
    m = max(scores.values())
    z = sum(math.exp(s - m) for s in scores.values())
    ranked = sorted(((c, math.exp(s - m) / z) for c, s in scores.items()), key=lambda x: -x[1])
    return ranked[:n]


PAGE = """<!doctype html>
<html lang="en"><head><meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>小象 v0.1: reading intent from a turn's words</title>
<link rel="stylesheet" href="tufte.css">
<style>
 #box{width:100%;max-width:38rem;font:1rem/1.4 sans-serif;padding:.5rem;box-sizing:border-box}
 #out{font:0.95rem/1.6 sans-serif;margin-top:.5rem;min-height:4.5rem}
 .bar{display:inline-block;height:.7rem;background:#555;margin-left:.5rem;vertical-align:middle}
 table{border-collapse:collapse;font:0.85rem sans-serif}
 td,th{padding:.15rem .6rem;text-align:left;border-bottom:1px solid #ddd}
 td.n{text-align:right}
</style></head><body><article>
<h1>小象 v0.1</h1>
<p class="subtitle">Reading intent from a turn's words alone</p>
<section>
<p>象 (<em>xiàng</em>, elephant) reads the turns I type to software agents and labels each fragment with what it is doing: proposing, explaining, approving, reporting a problem, and so on. 小象 (<em>little elephant</em>) is a classical model that tries to recover those labels from the words alone, with no language model and no database, so that it can run on anyone's logs.</p>
<p>Version 0.1 is deliberately simple: naive Bayes over words and word pairs. It is expected to do poorly, and the point is to measure where. Try a sentence:</p>
<textarea id="box" rows="3" placeholder="e.g. No, you have missed my point again."></textarea>
<div id="out"></div>
</section>
<section>
<h2>How well it does</h2>
<p>Trained and tested on __FRAGMENTS__ labelled fragments from __TURNS__ of my turns, with __INTENTS__ intents. Each test holds out whole turns (__FOLDS__-fold cross-validation), so no fragment is scored by a model that saw its neighbours.</p>
<ul>
<li>Accuracy: <b>__ACC__%</b>, against __MAJ__% for always guessing <em>__MAJLABEL__</em>.</li>
<li>Macro-averaged F1 over intents: <b>__F1__</b>. Rare intents are mostly never predicted.</li>
</ul>
<p>The labels are 象's own readings, and none has yet been confirmed by me. So some errors below are the model's, and some are disagreement between labellers.</p>
<h3>Per intent</h3>
<table><tr><th>intent</th><th>fragments</th><th>recall</th><th>precision</th></tr>__PERINTENT__</table>
<h3>Most common mistakes</h3>
<table><tr><th>labelled</th><th>predicted</th><th>count</th></tr>__CONFUSIONS__</table>
<h3>Intent collisions</h3>
<p>The same short text labelled with different intents. No classifier that reads only the words can get all of these right; they need context.</p>
<table><tr><th>text</th><th>labels</th></tr>__COLLISIONS__</table>
</section>
<section><p>Model: __VOCAB__ tokens, each seen in at least __MINTURNS__ separate turns, with identifiers and anything a secret scanner flags removed. Built __BUILT__.</p></section>
</article>
<script>
const M = __MODEL__;
const CJK = /[\\u4e00-\\u9fff\\u3400-\\u4dbf]/;
const WORD = /[a-z][a-z']*|[\\u4e00-\\u9fff\\u3400-\\u4dbf]+/g;
const VOCAB = new Set(); for (const c in M.counts) for (const t in M.counts[c]) VOCAB.add(t);
function tokens(text){
  const out=[], words=[];
  for (const p of (text.toLowerCase().match(WORD)||[])) {
    if (CJK.test(p[0])) { const ch=[...p]; out.push(...ch); for(let i=0;i+1<ch.length;i++) out.push(ch[i]+ch[i+1]); }
    else words.push(p);
  }
  out.push(...words); for(let i=0;i+1<words.length;i++) out.push(words[i]+' '+words[i+1]);
  return out;
}
function classify(text){
  const toks=tokens(text).filter(t=>VOCAB.has(t)), total=Object.values(M.prior).reduce((a,b)=>a+b,0);
  const s={}; for (const c in M.prior){ const d=(M.totals[c]||0)+M.alpha*M.vocab_size; let v=Math.log(M.prior[c]/total);
    for (const t of toks) v+=Math.log((((M.counts[c]||{})[t])||0)+M.alpha)-Math.log(d); s[c]=v; }
  const m=Math.max(...Object.values(s)); let z=0; for(const c in s) z+=Math.exp(s[c]-m);
  return {known:toks.length, ranked:Object.keys(s).map(c=>[c,Math.exp(s[c]-m)/z]).sort((a,b)=>b[1]-a[1]).slice(0,3)};
}
const box=document.getElementById('box'), out=document.getElementById('out');
box.addEventListener('input',()=>{ const t=box.value.trim(); if(!t){out.innerHTML='';return;}
  const r=classify(t);
  out.innerHTML = r.ranked.map(([c,p])=>`<div>${(100*p).toFixed(0)}% <b>${c}</b><span class="bar" style="width:${Math.round(200*p)}px"></span></div>`).join('')
    + (r.known ? '' : '<div><em>None of these words are in the model; this is just the prior.</em></div>');
});
</script></body></html>
"""


def _esc(s: str) -> str:
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def page(rows) -> str:
    import datetime  # noqa: PLC0415
    keep = safe_vocab(rows)
    ev = evaluate(rows)
    model = export(rows, keep)
    per = "".join(
        f"<tr><td>{_esc(c)}</td><td class=n>{v['support']}</td>"
        f"<td class=n>{v['recall']:.2f}</td><td class=n>{v['precision']:.2f}</td></tr>"
        for c, v in sorted(ev["per_intent"].items(), key=lambda kv: -kv[1]["support"]))
    conf = "".join(f"<tr><td>{_esc(c['gold'])}</td><td>{_esc(c['predicted'])}</td>"
                   f"<td class=n>{c['count']}</td></tr>" for c in ev["top_confusions"][:10])
    coll = "".join(
        f"<tr><td>{_esc(c['text'])}</td><td>"
        + ", ".join(f"{_esc(i)} {n}" for i, n in c["intents"].items()) + "</td></tr>"
        for c in collisions(rows, keep)[:12])
    subs = {
        "__FRAGMENTS__": str(ev["fragments"]), "__TURNS__": str(ev["turns"]),
        "__INTENTS__": str(ev["intents"]), "__FOLDS__": str(ev["folds"]),
        "__ACC__": f"{100 * ev['accuracy']:.0f}",
        "__MAJ__": f"{100 * ev['majority_baseline']['accuracy']:.0f}",
        "__MAJLABEL__": _esc(ev["majority_baseline"]["intent"]),
        "__F1__": f"{ev['macro_f1']:.2f}", "__PERINTENT__": per,
        "__CONFUSIONS__": conf, "__COLLISIONS__": coll,
        "__VOCAB__": str(model["vocab_size"]), "__MINTURNS__": str(MIN_TURNS),
        "__BUILT__": datetime.date.today().isoformat(),
        "__MODEL__": json.dumps(model, ensure_ascii=False).replace("</", "<\\/"),
    }
    html = PAGE
    for k, v in subs.items():
        html = html.replace(k, v)
    return html


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    e = sub.add_parser("eval"); e.add_argument("--dir", default=DEFAULT_DIR)
    x = sub.add_parser("export"); x.add_argument("--dir", default=DEFAULT_DIR); x.add_argument("out")
    c = sub.add_parser("classify"); c.add_argument("model"); c.add_argument("text")
    w = sub.add_parser("page"); w.add_argument("--dir", default=DEFAULT_DIR); w.add_argument("out")
    a = ap.parse_args(argv)
    if a.cmd == "eval":
        rows = load(a.dir)
        result = evaluate(rows)
        result["collisions"] = collisions(rows, safe_vocab(rows))[:20]
        json.dump(result, sys.stdout, indent=1, ensure_ascii=False); print()
    elif a.cmd == "export":
        rows = load(a.dir)
        with open(a.out, "w", encoding="utf-8") as fh:
            json.dump(export(rows, safe_vocab(rows)), fh, ensure_ascii=False)
    elif a.cmd == "page":
        with open(a.out, "w", encoding="utf-8") as fh:
            fh.write(page(load(a.dir)))
    else:
        with open(a.model, encoding="utf-8") as fh:
            model = json.load(fh)
        for intent, p in classify(model, a.text):
            print(f"{p:.2f}  {intent}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
