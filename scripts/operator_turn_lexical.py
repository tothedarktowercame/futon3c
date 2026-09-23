#!/usr/bin/env python3
"""operator_turn_lexical.py — classical pass over the operator-turn corpus.

Counts, not models: token and function-word profiles, and n-grams (1-5) scored
as candidate rewrite-rule cues.  A cue is a short phrase that recurs across
many turns and tends to sit at the start of a sentence, which is where the
directive force of a turn is usually carried.

Reads the harvest written by harvest_operator_turns.py; writes TSV/JSON under
the same storage directory.  Run data, so nothing lands in a futon repo.

  python3 operator_turn_lexical.py [CORPUS.jsonl] [OUTDIR]
"""
import collections, json, math, os, re, sys

CORPUS = sys.argv[1] if len(sys.argv) > 1 else \
    "/home/joe/code/storage/operator-turns/operator-turns.jsonl"
OUT = sys.argv[2] if len(sys.argv) > 2 else \
    "/home/joe/code/storage/operator-turns/analysis"

# The harness appends its own blocks to some turns; they are not Joe's words
# and would dominate any frequency count, so they come out before anything else.
MACHINE = [
    re.compile(r"\[Session-mode structural analysis request.*?\[End structural analysis request\]", re.S),
    re.compile(r"<system-reminder>.*?</system-reminder>", re.S),
    re.compile(r"<pasted_content[^>]*>.*?</pasted_content>", re.S),
]
CODEISH = re.compile(r"```.*?```", re.S)
# The park/wake machinery appends the woken agent's own report to the operator
# turn that resumed it.  That text is authored by an agent, not by Joe, and it
# runs to thousands of words, so it is cut at the marker.
RESUMED = re.compile(r"\n?-{3} resumed:.*\Z", re.S)
SENT = re.compile(r"[.!?]+[\s]+|\n+")
TOKEN = re.compile(r"[a-z0-9']+")
MAXN = 5
MIN_DF = 8          # a cue must recur across turns, not just within one
TOP = 4000


def clean(t):
    for rx in MACHINE:
        t = rx.sub(" ", t)
    t = RESUMED.sub(" ", t)
    return CODEISH.sub(" ", t)


def main():
    rows = [json.loads(l) for l in open(CORPUS)]
    stats = {"turns": len(rows), "machine_blocks_stripped": 0}

    df = collections.Counter()          # n-gram -> turns containing it
    tf = collections.Counter()          # n-gram -> total occurrences
    initial = collections.Counter()     # n-gram -> sentence-initial occurrences
    unigrams = collections.Counter()
    lengths = []
    sent_count = 0

    for r in rows:
        raw = r.get("text") or ""
        t = clean(raw)
        if len(t) != len(raw):
            stats["machine_blocks_stripped"] += 1
        seen = set()
        toks_all = TOKEN.findall(t.lower())
        lengths.append(len(toks_all))
        unigrams.update(toks_all)
        for s in SENT.split(t):
            toks = TOKEN.findall(s.lower())
            if not toks:
                continue
            sent_count += 1
            for n in range(2, MAXN + 1):
                for i in range(len(toks) - n + 1):
                    g = " ".join(toks[i:i + n])
                    tf[g] += 1
                    seen.add(g)
                    if i == 0:
                        initial[g] += 1
        for g in seen:
            df[g] += 1

    N = len(rows)
    stats.update({
        "tokens": sum(lengths), "sentences": sent_count,
        "vocab": len(unigrams), "ngram_types": len(tf),
        "median_turn_tokens": sorted(lengths)[len(lengths) // 2],
        "span": [min(r["at"] for r in rows if r.get("at")),
                 max(r["at"] for r in rows if r.get("at"))],
        "transports": collections.Counter(r.get("transport") for r in rows).most_common(),
    })

    os.makedirs(OUT, exist_ok=True)
    json.dump(stats, open(f"{OUT}/corpus-stats.json", "w"), indent=2)

    with open(f"{OUT}/unigrams.tsv", "w") as f:
        f.write("token\tcount\trate_per_1k\n")
        for w, c in unigrams.most_common(TOP):
            f.write(f"{w}\t{c}\t{1000 * c / stats['tokens']:.3f}\n")

    # Cue score: spread across turns (log df) times the tendency to open a
    # sentence.  Both matter -- a frequent phrase buried mid-sentence is
    # usually subject matter, while an opener that appears in one turn is noise.
    scored = []
    for g, c in tf.items():
        d = df[g]
        if d < MIN_DF:
            continue
        init_rate = initial[g] / c
        scored.append((math.log(d) * (0.25 + init_rate), g, c, d, init_rate,
                       len(g.split())))
    scored.sort(reverse=True)
    with open(f"{OUT}/ngram-cues.tsv", "w") as f:
        f.write("score\tngram\tcount\tdoc_freq\tsentence_initial_rate\tn\n")
        for s, g, c, d, ir, n in scored[:TOP]:
            f.write(f"{s:.3f}\t{g}\t{c}\t{d}\t{ir:.3f}\t{n}\n")

    print(json.dumps(stats, indent=2))
    print(f"\n{len(scored)} n-grams with df>={MIN_DF}; top cues -> {OUT}/ngram-cues.tsv")
    for s, g, c, d, ir, n in scored[:25]:
        print(f"  {s:6.2f}  df={d:5d}  init={ir:.2f}  {g}")


if __name__ == "__main__":
    main()
