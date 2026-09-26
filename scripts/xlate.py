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
# Proposed patterns that no one has admitted to the library. They are indexed
# alongside it so a translator meets an existing proposal before minting a
# second one for the same move -- which is how a vocabulary doubles without
# anybody deciding to grow it.
CANDIDATES = "/home/joe/code/storage/operator-turns/candidates"
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


def walk(roots=(LIB,)):
    for base in roots:
        if not os.path.isdir(base):
            continue
        for root, _, files in os.walk(base):
            for f in files:
                if f.endswith(".flexiarg"):
                    yield os.path.join(root, f)


def build_index(roots=(LIB,), mark=""):
    docs = {}
    for path in walk(roots):
        base = LIB if path.startswith(LIB) else CANDIDATES
        pid = mark + os.path.relpath(path, base)[:-len(".flexiarg")]
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


def load_index(with_candidates=False):
    roots = (LIB, CANDIDATES) if with_candidates else (LIB,)
    newest = max(os.path.getmtime(p) for p in walk(roots))
    cache = CACHE + (".cand" if with_candidates else "")
    if os.path.exists(cache):
        c = json.load(open(cache))
        if c.get("newest", 0) >= newest:
            return c["docs"]
    docs = build_index((LIB,))
    if with_candidates:
        # "?" marks a proposal in every listing: it is a name someone wanted,
        # not a name the library has agreed to.
        docs.update(build_index((CANDIDATES,), mark="?"))
    os.makedirs(os.path.dirname(cache), exist_ok=True)
    json.dump({"newest": newest, "docs": docs}, open(cache, "w"))
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
    n, cands = 8, False
    if "--with-candidates" in args:
        cands = True; args = [a for a in args if a != "--with-candidates"]
    if "-n" in args:
        i = args.index("-n"); n = int(args[i + 1]); args = args[:i] + args[i + 2:]
    docs = load_index(cands)
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


def cmd_census(args):
    """What the whole corpus of interpreted turns has used, and what it wanted.

    The global cascade is the union of every turn's, and the question it has
    to answer is which names are load-bearing, which are proposals, and which
    proposals have recurred often enough to be admitted. Counting is the only
    honest way to know -- a proposal that has come up once is a guess, and the
    same proposal from three independent turns is a pattern the library is
    missing.
    """
    import collections, glob as _glob
    records = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
    cited, proposed, turns = collections.Counter(), collections.Counter(), 0
    holes = 0
    for path in _glob.glob(f"{records}/*.analysis.json"):
        data = json.load(open(path, encoding="utf-8"))
        turns += 1
        for sentence in data.get("sentences", []):
            for frag in sentence.get("fragments", []):
                refs = frag.get("pattern_refs", [])
                if refs:
                    for r in refs:
                        cited[r["id"]] += 1
                else:
                    holes += 1
        # Two spellings exist in the wild: turn-X.json.candidates.json, which
        # this derived, and turn-X.candidates.json, which is what 16 of the 17
        # files are actually called. Only one was ever found, so the census
        # reported one unminted proposal where there were several -- and a
        # proposal that cannot be counted cannot ripen. Accept both.
        stem = path[:-len(".analysis.json")]
        for side in (stem + ".candidates.json",
                     stem[:-len(".json")] + ".candidates.json"
                     if stem.endswith(".json") else None):
            if side and os.path.exists(side):
                for c in json.load(open(side, encoding="utf-8")).get("candidates", []):
                    proposed[c["id"]] += 1
                break
    # An analyst that meets an already-proposed move again does the right
    # thing by CITING the proposal rather than minting a second name for it --
    # and until 2026-09-24 that made the occurrence invisible here, because
    # only candidates files were counted. The correct behaviour suppressed the
    # very signal that ripens a proposal: kimi-1 reported three alignments for
    # orchestration/lightweight-ack-advance while this census saw one.
    #
    # A mention in a rationale can only INCREMENT an id some candidates file
    # already proposed. It can never introduce one: a first pass at this
    # scanned rationales with a bare id-shaped regex and duly proposed
    # "usr/local", "texlive/2026" and "19/20", because prose contains paths.
    known = set(proposed)
    for path in _glob.glob(f"{records}/*.analysis.json"):
        data = json.load(open(path, encoding="utf-8"))
        seen_here = set()
        for sentence in data.get("sentences", []):
            for frag in sentence.get("fragments", []):
                blob = str(frag.get("rationale", ""))
                for pid in known:
                    if pid in blob and pid not in seen_here:
                        seen_here.add(pid)
                        proposed[pid] += 1
    del known

    # descendants: patterns whose @why rests on this one
    children = collections.Counter()
    for path in walk():
        text = open(path, encoding="utf-8", errors="replace").read()
        m = re.search(r"^@why (.+)$", text, re.M)
        if m:
            for token in m.group(1).split():
                children[token.strip("[](),;")] += 1

    print(f"{turns} interpreted turns; {sum(cited.values())} citations of "
          f"{len(cited)} patterns; {holes} fragments with no pattern")
    print("\nCited patterns (times cited / descendants in library):")
    for pid, n in cited.most_common(20):
        mark = "" if os.path.exists(f"{LIB}/{pid}.flexiarg") else "  UNRESOLVED"
        print(f"  {n:3d} / {children.get(pid, 0):<3d}  {pid}{mark}")
    if proposed:
        print("\nProposed but unminted (times proposed; 3 admits under "
              "cascade-construction/lift-when-three-align):")
        for pid, n in proposed.most_common():
            ripe = "  RIPE" if n >= 3 else ""
            here = f"{CANDIDATES}/{pid}.flexiarg"
            print(f"  {n:3d}  {pid}{ripe}"
                  f"{'' if os.path.exists(here) else '  (no draft written)'}")
    else:
        print("\nNo candidates proposed yet.")
    return 0


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit(__doc__)
    cmd, rest = sys.argv[1], sys.argv[2:]
    sys.exit({"find": cmd_find, "offsets": cmd_offsets, "lint": cmd_lint,
              "census": cmd_census}[cmd](rest) or 0)
