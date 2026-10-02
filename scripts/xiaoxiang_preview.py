#!/usr/bin/env python3
"""小象 fast mode: mark a draft turn before it is sent, and record corrections.

  xiaoxiang_preview.py preview [--rebuild] TEXT|-     JSON, one entry per fragment
  xiaoxiang_preview.py correct --text T --fragment F --predicted P --intent I
  xiaoxiang_preview.py build                           rebuild the model now

The model is naive Bayes over every published 象 reading plus Joe's
corrections, rebuilt when older than a day.  Its probabilities are not
calibrated (cross-validated, p >= 0.9 is right 44% of the time), so
confidence comes from elsewhere: each intent's cross-validated precision
when predicted.  A fragment is labelled outright only when its predicted
intent is right at least half the time (approve 0.84, ask-action 0.52);
otherwise it shows "?" and the two likeliest intents (right 52% of the
time between them).

Corrections go to storage/operator-turns/xiaoxiang-corrections.jsonl and
are trained on at the next rebuild.  They are not used in the edition
scores (xiaoxiang_editions.py), which stay comparable across editions.
"""
import argparse, collections, datetime, json, os, sys, time

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import xiaoxiang as xx  # noqa: E402
import xiaoxiang_editions as ed  # noqa: E402

MODEL = os.path.expanduser("~/.local/share/xiaoxiang/preview-model.json")
CORRECTIONS = "/home/joe/code/storage/operator-turns/xiaoxiang-corrections.jsonl"
MAX_AGE = 24 * 3600
SURE = 0.5


def corrections():
    rows = []
    if os.path.exists(CORRECTIONS):
        for line in open(CORRECTIONS, encoding="utf-8"):
            c = json.loads(line)
            rows.append({"turn": "correction/" + c["at"], "labeller": "joe",
                         "text": c["fragment"], "intent": c["intent"]})
    return rows


def build():
    rows, _ = ed.rows_now()
    rows += corrections()
    hits = collections.defaultdict(lambda: [0, 0])
    for i in range(5):
        m = xx.NaiveBayes().fit([r for r in rows if xx.fold_of(r["turn"], 5) != i])
        for r in rows:
            if xx.fold_of(r["turn"], 5) == i:
                p = m.predict(r["text"])
                hits[p][0] += 1
                hits[p][1] += p == r["intent"]
    model = xx.export(rows, keep=None)
    model["precision"] = {k: round(c / n, 3) for k, (n, c) in hits.items()}
    model["built_at"] = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    model["rows"] = len(rows)
    os.makedirs(os.path.dirname(MODEL), exist_ok=True)
    with open(MODEL + ".partial", "w", encoding="utf-8") as fh:
        json.dump(model, fh, ensure_ascii=False)
    os.replace(MODEL + ".partial", MODEL)
    return model


def load_model(rebuild=False):
    if rebuild or not os.path.exists(MODEL) or time.time() - os.path.getmtime(MODEL) > MAX_AGE:
        return build()
    with open(MODEL, encoding="utf-8") as fh:
        return json.load(fh)


def _declared_marks(text):
    """(start, end, mark, intent) of each proforma-marked paragraph of TEXT,
    found with the reply parser in xiaoxiang.py (single mark table:
    REPLY_KEY via _paragraphs/_parse_mark)."""
    out = []
    for a, b in xx._paragraphs(text):
        par = text[a:b]
        pa = a + (len(par) - len(par.lstrip()))
        pb = b - (len(par) - len(par.rstrip()))
        parsed = xx._parse_mark(text[pa:pb])
        if parsed:
            mark, intent, _target = parsed
            out.append((pa, pb, mark, intent))
    return out


def preview(text, model):
    declared = _declared_marks(text)
    out = []
    for frag in xx.segment(text):
        ranked = xx.classify(model, frag["text"], n=2)
        top = ranked[0][0]
        sure = bool(xx.evidence(model, frag["text"])) and model["precision"].get(top, 0) >= SURE
        entry = {"start": frag["start"], "end": frag["end"], "text": frag["text"],
                 "intent": top if sure else None,
                 "guesses": [c for c, _ in ranked],
                 "precision": model["precision"].get(top)}
        mark = next((m for m in declared if m[0] <= frag["start"] < m[1]), None)
        if mark:
            entry.update({"intent": mark[3], "precision": 1.0,
                          "basis": "declared", "mark": mark[2]})
        else:
            entry["basis"] = "model"
        out.append(entry)
    return out


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("preview")
    p.add_argument("text")
    p.add_argument("--rebuild", action="store_true")
    c = sub.add_parser("correct")
    for f in ("--text", "--fragment", "--predicted", "--intent"):
        c.add_argument(f, required=f != "--predicted", default="")
    sub.add_parser("build")
    a = ap.parse_args(argv)
    if a.cmd == "build":
        m = build()
        print(f"built from {m['rows']} fragments at {m['built_at']}")
    elif a.cmd == "preview":
        text = sys.stdin.read() if a.text == "-" else a.text
        json.dump(preview(text, load_model(a.rebuild)), sys.stdout, ensure_ascii=False)
        print()
    else:
        if a.intent not in ed.CLOSED:
            sys.exit(f"not an intent on the list: {a.intent}")
        row = {"at": datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%S.%fZ"),
               "text": a.text, "fragment": a.fragment, "predicted": a.predicted or None,
               "intent": a.intent}
        with open(CORRECTIONS, "a", encoding="utf-8") as fh:
            fh.write(json.dumps(row, ensure_ascii=False) + "\n")
        print("recorded")
    return 0


if __name__ == "__main__":
    sys.exit(main())
