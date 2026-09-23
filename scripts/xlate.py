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
# 轻 tentative ("we could", "maybe"), 平 plain, 强 insistent ("I'm telling
# you", repetition, rebuke). Latin spellings accepted so a cascade can be
# written without a CJK keyboard.
FORCE_VALUES = {"轻", "平", "强", "light", "plain", "strong"}
HOLE = re.compile(r"(HOLE-?\d+)\b(.{0,400}?)(?=HOLE-?\d+\b|\n\n|\Z)", re.S)


def lint_text(text, src=None):
    """Check a cascade. Returns (problems, stats) and prints nothing.

    Candidate ids -- a hole's proposed filler -- are expected not to resolve,
    so they are reported separately rather than as errors. A slash inside
    prose is not an id: the family has to be a real directory under library/,
    which is what tells `regress/vacuity` from `cascades/declared-skeleton`.
    """
    problems = []
    families = {d for d in os.listdir(LIB) if os.path.isdir(f"{LIB}/{d}")}
    candidates = set(re.findall(r":candidate\s+([^\s)]+)", text))
    cited = {f"{a}/{b}" for a, b in PID.findall(text) if a in families} - candidates
    for pid in sorted(cited):
        if not os.path.exists(f"{LIB}/{pid}.flexiarg"):
            problems.append(f"unresolved id: {pid}")

    if text.count("(") != text.count(")"):
        problems.append(f"unbalanced parens: {text.count('(')} open, "
                        f"{text.count(')')} close")

    coverage = None
    if src is not None:
        covered = set()
        for a, b in SPAN.findall(text):
            a, b = int(a), int(b)
            if b > len(src) or a >= b:
                problems.append(f"bad span [{a},{b}] (source is {len(src)} chars)")
                continue
            covered |= {i for i in range(a, b) if not src[i].isspace()}
        total = sum(not c.isspace() for c in src)
        coverage = (len(covered), total)

    # Force is the illocutionary strength of an act -- 象/言即行's third
    # envelope field. Joe's ruling, 2026-09-23: this project uses "force" in
    # the speech-act sense only; the Alexandrian sense stays in + HOWEVER:.
    # A declared force must be carried by words in the turn, not by the
    # translator's impression of the tone, so it owes a span.
    for m in re.finditer(r":force\s+(\S+)", text):
        value = m.group(1).strip("\"'()[]{},;")
        if value not in FORCE_VALUES:
            problems.append(f"unknown force {value!r}: "
                            f"expected one of {' '.join(sorted(FORCE_VALUES))}")
    if ":force " in text and ":force-span" not in text:
        problems.append("declared :force with no :force-span -- "
                        "force is a span of the turn, not a reading of its tone")

    holes = {}
    for name, body in HOLE.findall(text):
        if len(body) > len(holes.get(name, "")):
            holes[name] = body
    for name, body in sorted(holes.items()):
        if ":wanted" in body and not any(k in body for k in
                                         (":discharge", ":candidate", ":reason")):
            problems.append(f"untyped hole {name}: no :discharge/:candidate/:reason")

    return problems, {"cited": sorted(cited), "candidates": sorted(candidates),
                      "holes": sorted(holes), "coverage": coverage}


def cmd_lint(args):
    path, turn = args[0], None
    if "--turn" in args:
        turn = args[args.index("--turn") + 1]
    src = json.load(open(turn))["source_text"] if turn else None
    problems, stats = lint_text(open(path, encoding="utf-8").read(), src)
    for p in problems:
        print(p.upper() if p.startswith(("unresolved", "bad", "untyped")) else p)
    for c in stats["candidates"]:
        mark = "already in library" if os.path.exists(f"{LIB}/{c}.flexiarg") else "new"
        print(f"candidate: {c} ({mark})")
    if stats["coverage"]:
        cov, total = stats["coverage"]
        print(f"spans cover {cov}/{total} non-space chars ({100 * cov / total:.0f}%)")
    print(f"{len(stats['cited'])} cited ids, {len(stats['candidates'])} candidates, "
          f"{len(stats['holes'])} holes, {len(problems)} problem(s)")
    return 1 if problems else 0


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    cmd, rest = sys.argv[1], sys.argv[2:]
    sys.exit({"find": cmd_find, "offsets": cmd_offsets, "lint": cmd_lint}[cmd](rest) or 0)
