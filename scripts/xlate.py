#!/usr/bin/env python3
"""xlate.py — classical tooling for translating operator turns into cascades.

Built from what the first translation (zai-2, turn-hVCUAL) actually cost:
two hand-rolled regex alternations over 1,404 pattern files to go from a
concept to candidate ids, title reads to disambiguate them, and a one-off
python script to recover character offsets.  No model is involved here --
BM25 over the library's own text, and exact string work on the turn.

  xlate.py find "defer a decision, sort it out later" [-n 8]
  xlate.py offsets TURN.json "exact span text" ["another span"]
  xlate.py lint CASCADE.md --turn TURN.json

`find` indexes @title, @keywords, the ! conclusion and + context of every
flexiarg, and caches to ~/.cache/xlate-index.json keyed on the library's
newest mtime.  CJK is tokenised by character bigrams, since the 象/ family
has no whitespace to split on.
"""
import json, math, os, re, sys, time
from collections import Counter, defaultdict

LIB = "/home/joe/code/futon3/library"
CACHE = os.path.expanduser("~/.cache/xlate-index.json")
CJK = r"一-鿿㐀-䶿"
WORD = re.compile(r"[a-z0-9']+|[%s]" % CJK)
FIELDS = (re.compile(r"^@title (.+)$", re.M), re.compile(r"^@keywords (.+)$", re.M),
          re.compile(r"^! (?:conclusion|summary): (.+)$", re.M),
          re.compile(r"^\s*\+ context: (.+)$", re.M))


def tokens(text):
    """Words for alphabetic script; character bigrams for CJK runs."""
    out, toks = [], WORD.findall(text.lower())
    for i, t in enumerate(toks):
        out.append(t)
        if re.match("[%s]" % CJK, t) and i + 1 < len(toks) \
           and re.match("[%s]" % CJK, toks[i + 1]):
            out.append(t + toks[i + 1])          # bigram
    return out


def walk():
    for root, _, files in os.walk(LIB):
        for f in files:
            if f.endswith(".flexiarg"):
                yield os.path.join(root, f)


def build_index():
    docs = {}
    for path in walk():
        pid = os.path.relpath(path, LIB)[:-len(".flexiarg")]
        text = open(path, encoding="utf-8", errors="replace").read()
        parts, title = [], ""
        for i, rx in enumerate(FIELDS):
            for m in rx.findall(text):
                parts.append(m)
                if i == 0 and not title:
                    title = m
        # title and keywords count twice: they are what a searcher recalls
        weighted = parts + parts[:2]
        docs[pid] = {"title": title, "toks": tokens(" ".join(weighted))}
    return docs


def load_index():
    newest = max(os.path.getmtime(p) for p in walk())
    if os.path.exists(CACHE):
        c = json.load(open(CACHE))
        if c.get("newest", 0) >= newest:
            return c["docs"]
    docs = build_index()
    os.makedirs(os.path.dirname(CACHE), exist_ok=True)
    json.dump({"newest": newest, "docs": docs}, open(CACHE, "w"))
    return docs


def bm25(query, docs, n=8, k1=1.5, b=0.75):
    N = len(docs)
    df = Counter()
    for d in docs.values():
        df.update(set(d["toks"]))
    avgdl = sum(len(d["toks"]) for d in docs.values()) / N
    q = tokens(query)
    scores = defaultdict(float)
    for pid, d in docs.items():
        tf, dl = Counter(d["toks"]), len(d["toks"])
        for t in q:
            if t not in tf:
                continue
            idf = math.log(1 + (N - df[t] + 0.5) / (df[t] + 0.5))
            scores[pid] += idf * tf[t] * (k1 + 1) / (tf[t] + k1 * (1 - b + b * dl / avgdl))
    return sorted(scores.items(), key=lambda kv: -kv[1])[:n]


def cmd_find(args):
    n = 8
    if "-n" in args:
        i = args.index("-n"); n = int(args[i + 1]); args = args[:i] + args[i + 2:]
    docs = load_index()
    for pid, s in bm25(" ".join(args), docs, n):
        print(f"{s:7.2f}  {pid}\n         {docs[pid]['title'][:100]}")


def cmd_offsets(args):
    turn, spans = args[0], args[1:]
    src = json.load(open(turn))["source_text"]
    for s in spans:
        i = src.find(s)
        if i < 0:
            print(f"MISS   {s!r}  -- not a literal substring of the turn")
            continue
        dup = " (NOT UNIQUE)" if src.find(s, i + 1) >= 0 else ""
        print(f"[{i},{i + len(s)}]{dup}  {s!r}")


SPAN = re.compile(r"\[(\d+)\s*,\s*(\d+)\]")
PID = re.compile(r"(?<![\w/])([a-z0-9-]+|[%s]+)/([a-z0-9'-]+|[%s'-]+)" % (CJK, CJK))
HOLE = re.compile(r"(HOLE-?\d+)\b(.{0,400}?)(?=HOLE-?\d+\b|\n\n|\Z)", re.S)


def cmd_lint(args):
    """Check a cascade against the contract: ids resolve, spans are real,
    holes are typed.  Candidate ids -- a hole's proposed filler -- are
    expected not to resolve, so they are reported separately rather than as
    errors.  A slash inside prose is not an id: the family has to be a real
    directory under library/, which is what tells `regress/vacuity` apart
    from `cascades/declared-skeleton`."""
    path, turn = args[0], None
    if "--turn" in args:
        turn = args[args.index("--turn") + 1]
    text = open(path, encoding="utf-8").read()
    bad = 0

    families = {d for d in os.listdir(LIB) if os.path.isdir(f"{LIB}/{d}")}
    candidates = {m for m in re.findall(r":candidate\s+([^\s)]+)", text)}
    cited = {f"{a}/{b}" for a, b in PID.findall(text)
             if a in families} - candidates
    for pid in sorted(cited):
        if not os.path.exists(f"{LIB}/{pid}.flexiarg"):
            print(f"UNRESOLVED id: {pid}"); bad += 1
    for pid in sorted(candidates):
        mark = "already in library" if os.path.exists(f"{LIB}/{pid}.flexiarg") else "new"
        print(f"candidate: {pid} ({mark})")

    if turn:
        src = json.load(open(turn))["source_text"]
        covered = set()
        for a, b in SPAN.findall(text):
            a, b = int(a), int(b)
            if b > len(src) or a >= b:
                print(f"BAD span [{a},{b}] (source is {len(src)} chars)"); bad += 1
                continue
            covered |= {i for i in range(a, b) if not src[i].isspace()}
        total = sum(not c.isspace() for c in src)
        # Reported, not failed: the sparsity rule governs the operative marks
        # shown to the operator, and a cascade file carries fragment spans too.
        print(f"spans cover {len(covered)}/{total} non-space chars "
              f"({100 * len(covered) / total:.0f}%)")

    holes = {}
    for name, body in HOLE.findall(text):
        holes.setdefault(name, "")
        if len(body) > len(holes[name]):
            holes[name] = body
    for name, body in sorted(holes.items()):
        if ":wanted" in body and not any(k in body for k in
                                         (":discharge", ":candidate", ":reason")):
            print(f"UNTYPED hole {name}: no :discharge/:candidate/:reason"); bad += 1
    print(f"{len(cited)} cited ids, {len(candidates)} candidates, "
          f"{len(holes)} holes, {bad} problem(s)")
    return 1 if bad else 0


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    cmd, rest = sys.argv[1], sys.argv[2:]
    sys.exit({"find": cmd_find, "offsets": cmd_offsets, "lint": cmd_lint}[cmd](rest) or 0)
