#!/usr/bin/env python3
"""habit_prior.py — a candidate E (habit prior over cascades) for PROOF-2a,
computed on the M-futon-seams candidates. claude-10, 2026-09-24.

E(pi) = sum over the cascade's patterns of ln p(pattern), with p a
Dirichlet(alpha)-smoothed use frequency over the whole library, from a named
corpus of past pattern use. Two corpora, reported separately:
  missions: futon6/data/mission-triples/*.edn pattern citations (one per mission)
  turns:    operator-turn analyses (live + historical block 0), pattern_refs
Reports E per candidate, the difference a-b, and how much of it comes from
the smoothing floor (patterns the corpus never saw). Deterministic.
"""
import glob, json, math, os, re, subprocess
LIB = "/home/joe/code/futon3/library"
library = sorted(os.path.relpath(p, LIB)[:-9] for p in glob.glob(LIB + "/**/*.flexiarg", recursive=True))
def mission_counts():
    c = {}
    for f in sorted(glob.glob("/home/joe/code/futon6/data/mission-triples/*.edn")):
        for p in set(re.findall(r':ref "futon3/library/([^"]+)\.flexiarg"', open(f).read())):
            c[p] = c.get(p, 0) + 1
    return c
def turn_counts():
    c = {}
    files = sorted(glob.glob(os.path.expanduser("~/.emacs-graph/session-turn-analysis/*.analysis.json")) +
                   glob.glob("/home/joe/code/storage/operator-turns/batches/*/*.analysis.json"))
    for f in files:
        d = json.load(open(f))
        for s in d.get("sentences", []):
            for fr in s.get("fragments", []):
                for r in fr.get("pattern_refs", []):
                    c[r["id"]] = c.get(r["id"], 0) + 1
    return c
def edn_patterns(cid):
    out = subprocess.check_output(["bb", "-e",
        '(let [c (get-in (clojure.edn/read-string (slurp "/home/joe/code/futon3c/holes/labs/M-futon-seams/exemplar/click-001.edn")) '
        '[:decision :selection-certificate :candidate-derivations %s])] (doseq [k (sort-by str (keys (:interpretations c)))] (println (subs (str k) 1))))' % cid], text=True)
    return out.split()
def E(pats, counts, alpha=1.0):
    total = sum(counts.values()) + alpha * len(library)
    terms = {p: math.log((counts.get(p, 0) + alpha) / total) for p in pats}
    return sum(terms.values()), sum(1 for p in pats if counts.get(p, 0) == 0), terms
cands = {"a": edn_patterns(":cand/a-registry-first"), "b": edn_patterns(":cand/b-observe-first")}
res = {}
for name, counts in [("missions", mission_counts()), ("turns", turn_counts())]:
    ea, fa, ta = E(cands["a"], counts); eb, fb, tb = E(cands["b"], counts)
    floor = math.log(1.0 / (sum(counts.values()) + len(library)))
    res[name] = {"corpus_uses": sum(counts.values()), "distinct_patterns_used": len(counts),
                 "E_a": round(ea, 4), "E_b": round(eb, 4), "E_a_minus_E_b": round(ea - eb, 4),
                 "unseen_a": fa, "unseen_b": fb, "floor_ln_p": round(floor, 4),
                 "prior_ratio_a_over_b": round(math.exp(ea - eb), 4),
                 "seen": {p: counts[p] for p in sorted(set(cands["a"]) | set(cands["b"])) if p in counts}}
res["library_size"] = len(library)
res["candidates"] = cands
json.dump(res, open(os.path.join(os.path.dirname(__file__), "habit-prior.json"), "w"), indent=1, sort_keys=True)
print(json.dumps({k: v for k, v in res.items() if k != "candidates"}, indent=1, sort_keys=True))
