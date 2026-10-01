#!/usr/bin/env python3
"""Score every published 象 citation for how likely it is a word-match, in bulk.

Joe (2026-10-01): review the backfill's citations with frequency measures
instead of reading each one.  Each citation (fragment -> pattern) gets:

  sim       tf-idf cosine between the cited sentence and the pattern's
            prose (title, conclusion, context, IF, THEN; not keywords)
  anchors   content words the sentence shares with that prose
  kw_only   the sentence shares words with the pattern's @keywords line
            but none with its prose: the read-only-first-then-extend case
            ("follow mode" vs the keyword "follow")
  spread    distinct readers who ever cite this pattern
  intent_fit share of this pattern's citations that carry this intent
  quote     the rationale repeats 4+ consecutive words of the pattern

and a suspicion score combining them.  Writes one JSON line per citation;
prints the per-reader summary and the most suspicious citations.

  citation_audit.py [--out FILE] [--top N] [--batches DIR]
"""
import argparse, collections, glob, json, math, os, re

LIB = "/home/joe/code/futon3/library"
BATCHES = "/home/joe/code/storage/operator-turns/batches"
STOP = set("""a an the and or but if then else of to in on at for from by with without as is are
was were be been being it its this that these those there here i you we he she they me my our your
their them us do does did done not no yes so than too very can could should would will shall may
might must just also only about into over under again more most some any all each every other such
own same what which who whom whose when where why how let lets let's ok okay please now up down out
off one two use using get got make made go going see like want need think know really well still even
because while through one's it's that's i'm we're don't can't isn't""".split())
WORD = re.compile(r"[a-z][a-z0-9'-]+")


def words(text):
    return [w.strip("'-") for w in WORD.findall(text.lower()) if w.strip("'-") not in STOP and len(w) > 2]


def stem(w):
    for suf in ("ings", "ing", "ies", "es", "ed", "ly", "s"):
        if w.endswith(suf) and len(w) - len(suf) >= 4:
            return w[: -len(suf)]
    return w


def load_patterns():
    pats = {}
    for f in glob.glob(os.path.join(LIB, "**", "*.flexiarg"), recursive=True):
        pid = os.path.relpath(f, LIB)[: -len(".flexiarg")]
        text = open(f, encoding="utf-8", errors="replace").read()
        kw = " ".join(re.findall(r"^@keywords (.*)$", text, re.M))
        title = " ".join(re.findall(r"^@title (.*)$", text, re.M))
        prose = "\n".join(l for l in text.splitlines() if not l.startswith("@"))
        pats[pid] = {"prose": title + "\n" + prose, "kw": kw}
    return pats


def load_citations(batches):
    rows = []
    for f in glob.glob(os.path.join(batches, "*", "*.analysis.json")):
        try:
            a = json.load(open(f, encoding="utf-8"))
        except ValueError:
            continue
        src = a.get("source_text") or ""
        for s in a.get("sentences") or []:
            sent = src[s["start"]:s["end"]] if "start" in s else ""
            for fr in s.get("fragments") or []:
                for ref in fr.get("pattern_refs") or []:
                    rows.append({"file": f, "turn": os.path.basename(f).split(".json")[0],
                                 "block": os.path.basename(os.path.dirname(f)),
                                 "reader": a.get("labeller", "?"), "sentence": sent or fr.get("text", ""),
                                 "fragment": fr.get("text", ""), "intent": fr.get("intent"),
                                 "pattern": ref.get("id"), "rationale": ref.get("rationale", "")})
    return rows


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--out", default="/home/joe/code/storage/operator-turns/citation-audit.jsonl")
    ap.add_argument("--top", type=int, default=25)
    ap.add_argument("--batches", default=BATCHES)
    a = ap.parse_args()
    pats = load_patterns()
    rows = load_citations(a.batches)

    # idf over pattern prose: a word shared with many patterns is weak evidence.
    docs = {p: collections.Counter(stem(w) for w in words(v["prose"])) for p, v in pats.items()}
    df = collections.Counter(w for c in docs.values() for w in c)
    n = len(docs)
    idf = {w: math.log((n + 1) / (d + 1)) + 1 for w, d in df.items()}

    def vec(counter):
        v = {w: (1 + math.log(c)) * idf.get(w, math.log(n + 1) + 1) for w, c in counter.items()}
        norm = math.sqrt(sum(x * x for x in v.values())) or 1
        return {w: x / norm for w, x in v.items()}

    pvec = {p: vec(c) for p, c in docs.items()}
    # A pattern's core terms: its 15 highest tf-idf stems, title words counted
    # three times.  A citation whose shared words miss all of them shares only
    # incidental vocabulary ("follow", "metadata", "line").
    core = {}
    for pid, v in pats.items():
        c = collections.Counter(docs[pid])
        for w in words(v["prose"].split("\n", 1)[0]):
            c[stem(w)] += 3
        core[pid] = {w for w, _ in sorted(c.items(), key=lambda kv: -(1 + math.log(kv[1])) * idf.get(kv[0], 1))[:15]}
    # Session-topic citing: one reader cites the same pattern on 4+ sentences
    # of one agent's thread in a block (kimi-16: maturity-evidence-audit on
    # every sentence of one conversation, "Isn't actually defined." included).
    thread = collections.Counter((r["reader"], r["block"], re.sub(r"-turn-.*", "", r["turn"]),
                                  r["pattern"]) for r in rows)
    for r in rows:
        r["repeat"] = thread[(r["reader"], r["block"], re.sub(r"-turn-.*", "", r["turn"]), r["pattern"])]
    by_pattern = collections.defaultdict(list)
    for r in rows:
        by_pattern[r["pattern"]].append(r)

    for r in rows:
        p = pats.get(r["pattern"])
        if not p:
            r.update({"missing_pattern": True, "suspicion": 1.0})
            continue
        sw = collections.Counter(stem(w) for w in words(r["sentence"]))
        sv = vec(sw)
        r["sim"] = round(sum(x * pvec[r["pattern"]].get(w, 0) for w, x in sv.items()), 3)
        anchors = sorted(set(sw) & set(docs[r["pattern"]]), key=lambda w: -idf.get(w, 0))
        r["anchors"] = anchors[:6]
        r["core_hit"] = sorted(set(anchors) & core[r["pattern"]])
        r["kind"] = ("grounded" if r["core_hit"] else
                     "word-match" if anchors else "conceptual")
        kw = {stem(w) for w in words(p["kw"])}
        r["kw_only"] = bool(set(sw) & kw) and not anchors
        cites = by_pattern[r["pattern"]]
        r["spread"] = len({c["reader"] for c in cites})
        r["intent_fit"] = round(sum(c["intent"] == r["intent"] for c in cites) / len(cites), 2)
        pw = WORD.findall(p["prose"].lower())
        grams = {" ".join(pw[i:i + 4]) for i in range(len(pw) - 3)}
        rw = WORD.findall(r["rationale"].lower())
        r["quote"] = any(" ".join(rw[i:i + 4]) in grams for i in range(len(rw) - 3))
        # Suspicion: low similarity, one or no anchor, keyword-only overlap,
        # a pattern only one reader ever uses, an unusual intent for it.
        s = 0.0
        s += 0.35 * max(0.0, 1 - r["sim"] / 0.15)
        s += 0.2 if len(anchors) <= 1 else 0.0
        s += 0.2 if r["kw_only"] else 0.0
        s += 0.1 if r["spread"] == 1 and len(cites) == 1 else 0.0
        s += 0.15 * (1 - r["intent_fit"]) if len(cites) >= 3 else 0.0
        r["suspicion"] = round(s, 3)

    with open(a.out, "w", encoding="utf-8") as fh:
        for r in rows:
            fh.write(json.dumps(r, ensure_ascii=False) + "\n")

    print(f"{len(rows)} citations, {len(by_pattern)} patterns, written to {a.out}")
    per = collections.defaultdict(list)
    for r in rows:
        per[r["reader"]].append(r)
    print(f"{'reader':10} {'cites':>6} {'repeat>=4':>10} {'grounded':>9} {'word-match':>11} {'conceptual':>11}")
    for k, v in sorted(per.items(), key=lambda kv: -len(kv[1])):
        if len(v) < 40:
            continue
        kc = collections.Counter(x.get("kind") for x in v)
        print(f"{k:10} {len(v):6} {sum(x['repeat'] >= 4 for x in v)/len(v):10.0%} {kc['grounded']/len(v):9.0%} {kc['word-match']/len(v):11.0%} {kc['conceptual']/len(v):11.0%}")
    # Patterns one reader leans on far more than everyone else does.
    tot = collections.Counter(r["pattern"] for r in rows)
    print("\nreader habits (one reader, >=6 citations, >=60% of that pattern's uses):")
    pr = collections.Counter((r["reader"], r["pattern"]) for r in rows)
    for (rd, pt), c in pr.most_common():
        if c >= 6 and c / tot[pt] >= .6:
            print(f"  {rd:9} {pt} {c}/{tot[pt]}")
    hub = collections.Counter(r["pattern"] for r in rows).most_common(10)
    print("most cited patterns:", ", ".join(f"{p} {c}" for p, c in hub))
    print(f"\nFirst {a.top} word-match citations:")
    for r in [r for r in rows if r.get("kind") == "word-match"][: a.top]:
        print(f"{r['suspicion']:.2f} sim={r.get('sim')} {r['reader']:8} {r['pattern']}"
              f"  anchors={r.get('anchors')}  | {r['sentence'][:90]!r}")


if __name__ == "__main__":
    main()
