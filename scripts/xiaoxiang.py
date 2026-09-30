#!/usr/bin/env python3
"""小象 (xiaoxiang) v0.2 -- a classical intent classifier for operator turns.

象 labels Joe's real turns in futon1b's world: each fragment of a turn gets one
of ~26 intents (propose, explain, approve, report-problem, ...).  小象 learns to
predict those labels from the fragment's words alone, with no model service and
no store, so it can run on anyone's logs.  v0.1 is multinomial naive Bayes over
word unigrams and bigrams (CJK as character bigrams), plus a small hand-written
table of general English speech-act cues (SEED).  It is expected to do poorly;
the point is to measure where.

  xiaoxiang.py eval  [--dir DIR]            grouped held-out evaluation
  xiaoxiang.py export [--dir DIR] OUT.json  model with a filtered vocabulary
  xiaoxiang.py classify MODEL.json "text"   top intents for one fragment
  xiaoxiang.py page OUT.html                web page: live classifier + results
  xiaoxiang.py bundle OUT.py                one standalone file for other people

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
# ---------------------------------------------------------------------------
# Classical segmenter: cut one operator turn into fragments with stable ids
# and exact offsets, so a reply can name the fragment it answers.  Sentence
# ids match turn_batch.sentences_of (the recorders' rule); clause fragments
# are s<i>.<k>.  Standard library only, deterministic, no model involved.

# The sentence-split rule of scripts/turn_batch.py (SPLIT there); the ids
# must agree with its sentences_of for the same text.
SENTENCE_SPLIT = re.compile(r"(?<=[.!?])\s+")

# Clause-boundary rules.  One entry, one comment; keep the list explicit.
CLAUSE_BOUNDARIES = (
    # a semicolon ends a clause -- the classic list separator in Joe's prose
    ("semicolon", ";"),
    # an em/en dash or spaced hyphen used as a break (" -- like this")
    ("spaced-dash", None),
    # a blank line, or a line opening with a list marker ("- ", "* ", "1. "),
    # starts a new piece; the marker stays with its item
    ("line-structure", None),
    # clause-opening connectives -- cut BEFORE the connective, and only when
    # a comma or a dash sits right before it (mid-sentence "so" after a
    # comma IS a cut; sentence-initial "So," is not, the sentence boundary
    # already did that work)
    ("connective", ("but", "however", "so", "because", "although", "though",
                    "whereas", "while", "and then", "i.e.", "e.g.")),
)

# Spans a boundary must not fall inside.
_PROTECTED = (
    re.compile(r"```.*?```", re.S),                  # fenced code blocks
    re.compile(r"(?:https?://|www\.)\S*[^\s,.;:!?)]"),  # URLs, minus trailing punctuation
    re.compile(r"`[^`]*`"),                          # backticked spans
    re.compile(r"\([^()]*\)|\[[^\[\]]*\]"),          # (parenthesised) [spans]
    re.compile(r'"[^"]*"'),                          # quoted spans
    re.compile(r"\d+(?:\.\d+)+"),                    # numbers with dots (v0.2, 3.14)
    re.compile(r"(?:[\w.-]+/)+[\w.-]+"),             # file/dir paths a/b/c.py
    re.compile(r"\b[\w-]+\.[A-Za-z]{1,4}\b"),        # file names with dots
)


_LINE_STRUCTURE = re.compile(r"\n[ \t]*\n\s*|\n[ \t]*(?=(?:[-*•]|\d+[.)])[ \t])")


def _protected_mask(text: str) -> list[bool]:
    mask = [False] * len(text)
    for pattern in _PROTECTED:
        for m in pattern.finditer(text):
            for i in range(m.start(), m.end()):
                mask[i] = True
    return mask


def _cut_points(sentence: str) -> list[int]:
    """Offsets where the sentence is cut into clauses (start of next piece)."""
    mask = _protected_mask(sentence)
    cuts = []
    for m in _LINE_STRUCTURE.finditer(sentence):
        if m.end() < len(sentence) and not mask[m.start()]:
            cuts.append(m.end())
    i = 0
    while i < len(sentence):
        ch = sentence[i]
        if ch == ";" and not mask[i]:
            j = i + 1
            while j < len(sentence) and sentence[j].isspace():
                j += 1
            if j < len(sentence):
                cuts.append(j)
            i = j
            continue
        if (ch in "—–-" and not mask[i]
                and i > 0 and sentence[i - 1].isspace()
                and sentence[:i].rstrip(" \t")[-1:] not in ("", "\n")
                and i + 1 < len(sentence) and sentence[i + 1].isspace()):
            j = i + 1
            while j < len(sentence) and sentence[j].isspace():
                j += 1
            if j < len(sentence):
                cuts.append(j)
            i = j
            continue
        if ch in ",—–-" and not mask[i]:
            j = i + 1
            while j < len(sentence) and sentence[j].isspace():
                j += 1
            if j < len(sentence):
                for word in CLAUSE_BOUNDARIES[3][1]:
                    if sentence[j:j + len(word)].lower() == word:
                        after = j + len(word)
                        if after == len(sentence) or not sentence[after].isalnum():
                            cuts.append(j)
                            i = after
                            break
                else:
                    i = j
                continue
        i += 1
    return sorted(set(cuts))


def segment(text: str) -> list[dict]:
    """Fragments of one operator turn: {"id": "s2.1", "start", "end", "text"}.

    Sentence ids match turn_batch.sentences_of; clause fragments are
    s<i>.<k>.  Offsets are unicode codepoints, zero-based, end-exclusive,
    text[start:end] == text exactly; fragments are in order, do not
    overlap, and a fragment shorter than 3 words is merged into its
    neighbour.  Deterministic.
    """
    sentences = []
    at = 0
    for i, piece in enumerate(SENTENCE_SPLIT.split(text), start=1):
        if not piece:
            continue
        start = text.index(piece, at)
        end = start + len(piece)
        at = end
        sentences.append((f"s{i}", start, end))
    if not sentences and text.strip():
        sentences = [("s1", 0, len(text))]
    merged = []
    for sid, sstart, send in sentences:
        body = text[sstart:send]
        cuts = _cut_points(body)
        pieces = []
        prev = 0
        for cut in cuts:
            pieces.append((prev, cut))
            prev = cut
        pieces.append((prev, len(body)))
        frags = []
        for a, b in pieces:
            span = body[a:b]
            stripped = span.strip()
            if not stripped:
                continue
            fa = a + (len(span) - len(span.lstrip()))
            fb = b - (len(span) - len(span.rstrip()))
            frags.append({"id": None, "start": sstart + fa, "end": sstart + fb,
                          "text": body[fa:fb]})
        # a fragment shorter than 3 words merges into its neighbour
        result = []
        for f in frags:
            if result and len(f["text"].split()) < 3:
                prev_f = result[-1]
                prev_f["end"] = f["end"]
                prev_f["text"] = text[prev_f["start"]:prev_f["end"]]
            else:
                result.append(f)
        if len(result) > 1 and len(result[0]["text"].split()) < 3:
            first, second = result[0], result[1]
            second["start"] = first["start"]
            second["text"] = text[second["start"]:second["end"]]
            result = result[1:]
        for k, f in enumerate(result):
            f["id"] = f"{sid}.{k}"
            merged.append(f)
    return merged


def seg_eval(directory: str) -> dict:
    """Compare segment() with 象's fragments over the published analyses.

    A boundary matches when a 小象 fragment start is within 2 characters
    of a 象 fragment start.  A measurement, not a gate.
    """
    turns = x_fragments = mine_fragments = matched_mine = matched_x = 0
    for path in sorted(glob.glob(os.path.join(directory, "*.analysis.json"))):
        try:
            with open(path, encoding="utf-8") as fh:
                doc = json.load(fh)
        except (OSError, ValueError):
            continue
        source = doc.get("source_text")
        if not isinstance(source, str) or not source.strip():
            continue
        theirs = [f for sentence in doc.get("sentences") or []
                  for f in sentence.get("fragments") or []
                  if isinstance(f.get("start"), int)]
        if not theirs:
            continue
        turns += 1
        x_fragments += len(theirs)
        mine = segment(source)
        mine_fragments += len(mine)
        their_starts = [f["start"] for f in theirs]
        my_starts = [f["start"] for f in mine]
        matched_mine += sum(1 for m in my_starts
                            if any(abs(m - t) <= 2 for t in their_starts))
        matched_x += sum(1 for t in their_starts
                         if any(abs(m - t) <= 2 for m in my_starts))
    precision = matched_mine / mine_fragments if mine_fragments else 0.0
    recall = matched_x / x_fragments if x_fragments else 0.0
    f1 = (2 * precision * recall / (precision + recall)
          if precision + recall else 0.0)
    return {"turns": turns, "xiang_fragments": x_fragments,
            "xiaoxiang_fragments": mine_fragments,
            "boundary_precision": round(precision, 4),
            "boundary_recall": round(recall, 4),
            "boundary_f1": round(f1, 4)}


CJK = "一-鿿㐀-䶿"
WORD = re.compile(r"[a-z][a-z']*|[%s]+" % CJK)
IDLIKE = re.compile(r"\d|^[a-f0-9]{8,}$")
EXCLUDE_FILE = os.path.expanduser("~/.config/xiaoxiang/exclude.txt")
# A token in at least this share of fragments ("i", "the", "your") is common:
# it shifts the scores but is not evidence for an intent on its own.
COMMON_SHARE = 0.04

# Hand-written, general English speech-act cues: prior knowledge, not learned
# from anyone's logs.  Each token of each phrase gets SEED_WEIGHT pseudo-counts
# for its intent.  Kept small; every entry is a word or phrase whose force is
# the same in any conversation.
SEED_WEIGHT = 5
SEED = {
    "disagree": ["i reject", "reject", "i disagree", "disagree", "that's wrong",
                 "wrong", "incorrect", "i object", "not right"],
    "approve": ["i agree", "agree", "sounds good", "looks good", "that's right",
                "approved", "go ahead", "great"],
    "report-problem": ["doesn't work", "not working", "broken", "failed", "fails", "bug"],
    "ask-action": ["could you", "can you", "would you"],
}


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

    def fit(self, rows, keep=None, seed=True):
        df: Counter = Counter()
        for r in rows:
            self.prior[r["intent"]] += 1
            toks = tokens(r["text"])
            df.update(set(toks))
            for t in toks:
                if keep is None or t in keep:
                    self.counts[r["intent"]][t] += 1
                    self.totals[r["intent"]] += 1
                    self.vocab.add(t)
        if seed:
            for intent, phrases in SEED.items():
                for phrase in phrases:
                    for t in tokens(phrase):
                        self.counts[intent][t] += SEED_WEIGHT
                        self.totals[intent] += SEED_WEIGHT
                        self.vocab.add(t)
        n = max(1, len(rows))
        self.common = {t for t, c in df.items() if c / n >= COMMON_SHARE and t in self.vocab}
        return self

    def evidence(self, text: str) -> list[str]:
        """Known tokens that are not common: what the prediction rests on."""
        return [t for t in tokens(text) if t in self.vocab and t not in self.common]

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


def evaluate(rows, k: int = 5, seed: bool = True) -> dict:
    """k-fold cross-validation grouped by turn.  Accuracy counts every
    fragment; `answered` is the share with any uncommon known token, where the
    page gives a prediction instead of saying it has too little to go on."""
    gold, pred, answered = [], [], []
    for i in range(k):
        train = [r for r in rows if fold_of(r["turn"], k) != i]
        test = [r for r in rows if fold_of(r["turn"], k) == i]
        model = NaiveBayes().fit(train, seed=seed)
        majority = model.prior.most_common(1)[0][0]
        for r in test:
            gold.append(r["intent"])
            pred.append(model.predict(r["text"]) if model.vocab else majority)
            answered.append(bool(model.evidence(r["text"])))
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
        "accuracy": acc, "seed": seed,
        "answered": sum(answered) / len(gold),
        "accuracy_answered": (sum(g == p for g, p, a in zip(gold, pred, answered) if a)
                              / max(1, sum(answered))),
        "macro_f1": sum(v["f1"] for v in per.values()) / len(per),
        "majority_baseline": {"intent": majority_label, "accuracy": majority_n / len(gold)},
        "per_intent": per,
        "top_confusions": [{"gold": g, "predicted": p, "count": n}
                           for (g, p), n in confusions.most_common(15)],
    }


def _scanner():
    """secret_scan.scan from the file beside this one.  Fails closed: without
    the scanner no vocabulary is built, so a copy of this file on its own
    cannot export or publish unchecked text."""
    sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
    try:
        import secret_scan  # noqa: PLC0415
    except ImportError as e:
        raise RuntimeError("secret_scan.py must sit beside xiaoxiang.py; "
                           "refusing to build a vocabulary without it") from e
    return secret_scan.scan


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


def proper_nouns(rows, min_count: int = 3, share: float = 0.7) -> set[str]:
    """Words capitalised mid-sentence in at least SHARE of their uses: names of
    people and places, mostly.  First-person forms and 1-2 letter words are
    exempt.  Learned from the logs, so the code carries no list of names."""
    cap: Counter = Counter()
    low: Counter = Counter()
    for r in rows:
        for sentence in re.split(r"(?<=[.!?])\s+", r["text"]):
            for w in re.findall(r"[A-Za-z][A-Za-z']*", sentence)[1:]:
                (cap if w[0].isupper() else low)[w.lower()] += 1
    return {w for w, c in cap.items()
            if c >= min_count and c / (c + low[w]) >= share
            and len(w) > 2 and not re.match(r"i'", w)}


def excluded_words(path: str = EXCLUDE_FILE) -> set[str]:
    """One word per line from a local file kept outside the repository, for
    names the capitalisation rule misses."""
    try:
        with open(path, encoding="utf-8") as fh:
            return {line.strip().lower() for line in fh if line.strip()}
    except OSError:
        return set()


def safe_vocab(rows, exclude: set[str] | None = None) -> set[str]:
    """Tokens seen in >= MIN_TURNS distinct turns, not id-like, not tainted,
    not a proper noun or an excluded word (nor containing one, possessives
    included)."""
    scan = _scanner()
    turns_by_tok: dict[str, set] = defaultdict(set)
    tainted: set[str] = proper_nouns(rows) | (excluded_words() if exclude is None else exclude)
    tainted |= {w + "'s" for w in tainted}
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
        "name": "xiaoxiang", "version": "0.2", "alpha": model.alpha,
        "prior": dict(model.prior), "totals": dict(model.totals),
        "vocab_size": len(model.vocab),
        "common": sorted(model.common),
        "counts": {c: dict(v) for c, v in model.counts.items()},
    }


def evidence(model: dict, text: str) -> list[str]:
    vocab = {t for c in model["counts"].values() for t in c}
    common = set(model["common"])
    return [t for t in tokens(text) if t in vocab and t not in common]


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
<title>小象 v0.2: reading intent from a turn's words</title>
<link rel="stylesheet" href="tufte.css">
<style>
 #box{width:100%;max-width:38rem;font:1rem/1.4 sans-serif;padding:.5rem;box-sizing:border-box}
 #out{font:0.95rem/1.6 sans-serif;margin-top:.5rem;min-height:4.5rem}
 .bar{display:inline-block;height:.7rem;background:#555;margin-left:.5rem;vertical-align:middle}
 table{border-collapse:collapse;font:0.85rem sans-serif}
 td,th{padding:.15rem .6rem;text-align:left;border-bottom:1px solid #ddd}
 td.n{text-align:right}
 figure{margin:1.2rem 0 1.6rem;max-width:100%} figcaption{font:0.85rem/1.5 sans-serif;color:#444;margin-bottom:.3rem}
 figure svg{border-bottom:1px solid #eee}
</style></head><body><article>
<h1>小象 v0.2</h1>
<p class="subtitle">Reading intent from a turn's words alone</p>
<section>
<p>象 (<em>xiàng</em>, elephant) reads the turns I type to software agents and labels each fragment with what it is doing: proposing, explaining, approving, reporting a problem, and so on. 小象 (<em>little elephant</em>) is a classical model that tries to recover those labels from the words alone, with no language model and no database, so that it can run on anyone's logs.</p>
<p>It is deliberately simple: naive Bayes over words and word pairs, plus a short hand-written list of general English speech-act cues (below). It is expected to do poorly, and the point is to measure where. Try a sentence:</p>
<textarea id="box" rows="3" placeholder="e.g. No, you have missed my point again."></textarea>
<div id="out"></div>
</section>
__FIGURES__
<section>
<h2>Running it on your own logs</h2>
<p>The download on the <a href="index.html">home page</a> is one Python file, <code>xiaoxiang-local.py</code>. It carries this model, the secret scanner and a log reader. It uses only the standard library and makes no network connections, so you can read it before you run it:</p>
<pre>python3 xiaoxiang-local.py --days 7</pre>
<p>It reads Claude Code sessions in <code>~/.claude/projects</code> and Codex sessions in <code>~/.codex/sessions</code>, then prints a summary and writes <code>xiaoxiang-report.html</code>, a page that stays on your machine. The report has three parts.</p>
<h3>1. What kinds of request you make</h3>
<p>Each turn you typed is classified with the model on this page. Only your own turns count. Tool results, subagent traffic, text the tools add by themselves (environment context, <code>AGENTS.md</code>, slash-command wrappers, compaction summaries) and messages passed between agents are skipped. Before a turn is classified, anything the secret scanner flags in it is removed.</p>
<h3>2. Credentials sitting in the logs</h3>
<p>The scanner looks for private keys, cloud and API keys (AWS, GitHub, Anthropic, OpenAI, Slack, Google), JWTs, bearer tokens, passwords in URLs, <code>password = &hellip;</code> style assignments, and long random-looking strings. The report gives counts by kind, both as distinct values and as total appearances, because logs repeat themselves. It lists the files that hold them, and it never prints a value. It is a heuristic: some findings will be test fixtures or false alarms, and it will miss some real secrets.</p>
<h3>3. Work that ran while you weren't typing</h3>
<p>This part draws the chart on the home page from your own logs. Each bar is a stretch with no turn typed by you: its width is how long it lasted, and its height the tokens agents logged during it (input, cached input and output). Claude replies written over several log lines, and Codex's repeated token events, are counted once.</p>
<p>By default a gap must last at least 6 hours, which finds nights and long trips. <code>--gap-hours</code> changes that: <code>--gap-hours 1</code> also finds an hour out at the shops, and <code>--gap-hours 0</code> keeps every stretch between two typed turns, which lays all of your agents' tokens out over time. A gap in typing does not show that you were away, since you may have been reading.</p>
<h3>Options</h3>
<table>
<tr><td><code>--days N</code></td><td>only the last N days (log files and the turns and tokens inside them)</td></tr>
<tr><td><code>--gap-hours H</code></td><td>shortest gap to draw (default 6)</td></tr>
<tr><td><code>--html FILE</code></td><td>where to write the report page (default <code>xiaoxiang-report.html</code>; <code>--html ''</code> for none)</td></tr>
<tr><td><code>--json</code></td><td>print the report as JSON</td></tr>
<tr><td><code>--claude DIR</code>, <code>--codex DIR</code></td><td>read logs from somewhere else</td></tr>
</table>
<p>It is not fast: it reads every line of every log, at roughly five minutes per gigabyte on my machine. <code>--days</code> keeps it short.</p>
</section>
<section>
<h2>How well it does</h2>
<p>Trained and tested on __FRAGMENTS__ labelled fragments from __TURNS__ of my turns, with __INTENTS__ intents. Each test holds out whole turns (__FOLDS__-fold cross-validation), so no fragment is scored by a model that saw its neighbours.</p>
<ul>
<li>Accuracy: <b>__ACC__%</b>, against __MAJ__% for always guessing <em>__MAJLABEL__</em>, and __ACC0__% without the hand-written cues.</li>
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
<section>
<h3>Hand-written cues</h3>
<p>Words whose force is the same in any conversation, added as pseudo-counts (__SEEDW__ each) rather than learned. Version 0.1 read &ldquo;I reject your claim&rdquo; as approval: &ldquo;reject&rdquo; occurs in only four of my fragments, three of them about papers or ideas being rejected, so it never entered the model, and the decision rested on &ldquo;I&rdquo; and &ldquo;your&rdquo;.</p>
<table><tr><th>intent</th><th>cues</th></tr>__SEEDLIST__</table>
<p>When a sentence contains only very common words, the page says so instead of guessing.</p>
</section>
<section><p>Model: __VOCAB__ tokens, each seen in at least __MINTURNS__ separate turns, with identifiers, names of people and places, and anything a secret scanner flags removed. Built __BUILT__.</p></section>
</article>
<script>
const M = __MODEL__;
const CJK = /[\\u4e00-\\u9fff\\u3400-\\u4dbf]/;
const WORD = /[a-z][a-z']*|[\\u4e00-\\u9fff\\u3400-\\u4dbf]+/g;
const VOCAB = new Set(); for (const c in M.counts) for (const t in M.counts[c]) VOCAB.add(t);
const COMMON = new Set(M.common);
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
  return {known:toks.filter(t=>!COMMON.has(t)).length, ranked:Object.keys(s).map(c=>[c,Math.exp(s[c]-m)/z]).sort((a,b)=>b[1]-a[1]).slice(0,3)};
}
const box=document.getElementById('box'), out=document.getElementById('out');
box.addEventListener('input',()=>{ const t=box.value.trim(); if(!t){out.innerHTML='';return;}
  const r=classify(t);
  out.innerHTML = r.ranked.map(([c,p])=>`<div>${(100*p).toFixed(0)}% <b>${c}</b><span class="bar" style="width:${Math.round(200*p)}px"></span></div>`).join('')
    ;
  if (!r.known) out.innerHTML = '<div><em>Too little to go on: only very common words are known.</em></div>' + out.innerHTML.replace(/<div>/g,'<div style="opacity:.35">');
});
</script></body></html>
"""


def _esc(s: str) -> str:
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


SAMPLE_REPORT = os.path.expanduser("~/.local/share/xiaoxiang/sample-report.json")


def figures(report: dict | None) -> str:
    """The downloadable file's charts, drawn from one run of it over my logs.
    Only dates, hours, token counts and intent counts are used: no turn text,
    no file paths, no credential findings."""
    if not report:
        return ""
    import xiaoxiang_reader as rd  # noqa: PLC0415
    days = (report["last_turn"] - report["first_turn"]) / 86400
    views = report.get("gap_views") or {str(report["gap_hours"]): report["gaps"]}
    blurbs = {6: "nights and long trips", 1: "an hour or more away from the keyboard",
              0: "every stretch between two typed turns: all agent tokens laid out over time"}
    charts = []
    for h, gs in sorted(views.items(), key=lambda kv: -float(kv[0])):
        h = float(h)
        view = {**report, "gap_hours": h, "gaps": gs}
        total = sum(g["tokens"] for g in gs)
        share = 100 * total / max(1, report["agent_tokens"])
        label = f"--gap-hours {h:g}"
        charts.append(
            f"<figure><figcaption><code>{_esc(label)}</code>: {_esc(blurbs.get(int(h), ''))}. "
            f"{len(gs)} bar{'s' * (len(gs) != 1)}, holding {share:.0f}% of the tokens "
            f"agents logged.</figcaption>{rd.gap_svg(view)}</figure>")
    classified = sum(report["intents"].values()) or 1
    top = max(report["intents"].values(), default=1)
    bars = "".join(
        f"<tr><td>{_esc(k)}</td><td class=n>{n}</td><td class=n>{100 * n / classified:.0f}%</td>"
        f"<td><span class=bar style='width:{240 * n / top:.0f}px'></span></td></tr>"
        for k, n in report["intents"].items())
    return f"""<section>
<h2>What the download shows, on my own logs</h2>
<p>These charts come from one run of the downloadable file over my last {days:.0f} days of Claude Code and Codex logs ({report['turns']} turns typed by me, {report['agent_tokens'] / 1e9:.1f} billion tokens logged by agents). Your report draws the same charts from your logs, on your machine.</p>
<h3>Work that ran while I wasn't typing</h3>
<p>Each bar is a stretch with no turn typed by me: its width is how long it lasted, and its height the tokens agents logged during it. Bars are scaled within each chart. Hover a bar for its dates and values.</p>
{''.join(charts)}
<h3>What kinds of request I make</h3>
<p>Each typed turn, as 小象 reads it: with the accuracy shown below, so often wrong. {report['too_little_to_go_on']} turns had too little to go on and are left out.</p>
<table class=intents>{bars}</table>
</section>"""


def page(rows, report: dict | None = None) -> str:
    import datetime  # noqa: PLC0415
    keep = safe_vocab(rows)
    ev = evaluate(rows)
    ev0 = evaluate(rows, seed=False)
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
        "__ACC__": f"{100 * ev['accuracy']:.1f}",
        "__ACC0__": f"{100 * ev0['accuracy']:.1f}", "__SEEDW__": str(SEED_WEIGHT),
        "__SEEDLIST__": "".join(f"<tr><td>{_esc(i)}</td><td>{_esc(', '.join(ps))}</td></tr>"
                                for i, ps in SEED.items()),
        "__MAJ__": f"{100 * ev['majority_baseline']['accuracy']:.0f}",
        "__MAJLABEL__": _esc(ev["majority_baseline"]["intent"]),
        "__F1__": f"{ev['macro_f1']:.2f}", "__PERINTENT__": per,
        "__CONFUSIONS__": conf, "__COLLISIONS__": coll,
        "__VOCAB__": str(model["vocab_size"]), "__MINTURNS__": str(MIN_TURNS),
        "__BUILT__": datetime.date.today().isoformat(),
        "__FIGURES__": figures(report),
        "__MODEL__": json.dumps(model, ensure_ascii=False).replace("</", "<\\/"),
    }
    html = PAGE
    for k, v in subs.items():
        html = html.replace(k, v)
    return html


def _body(source: str, stop: str | None = None) -> list[str]:
    """Source lines after the module docstring, without the shebang or the
    __future__ import, cut at the line starting with STOP."""
    import ast  # noqa: PLC0415
    tree = ast.parse(source)
    first = tree.body[0]
    skip = first.end_lineno if isinstance(first, ast.Expr) and isinstance(
        getattr(first, "value", None), ast.Constant) else 0
    lines = source.splitlines()[skip:]
    if stop:
        lines = lines[:next(i for i, l in enumerate(lines) if l.startswith(stop))]
    return [l for l in lines if not l.startswith(("#!", "from __future__"))]


def bundle(rows) -> str:
    """One standalone file: secret_scan (verbatim up to its CLI), the parts of
    this module the classifier needs, the log reader, and the exported model."""
    import ast  # noqa: PLC0415
    here = os.path.dirname(os.path.abspath(__file__))
    _scanner()  # fail closed before building anything
    model = export(rows, safe_vocab(rows))

    def read(name):
        with open(os.path.join(here, name), encoding="utf-8") as fh:
            return fh.read()

    mine = read("xiaoxiang.py")
    wanted = {"CJK", "WORD", "tokens", "classify", "evidence"}
    parts = []
    for node in ast.parse(mine).body:
        names = ({node.name} if isinstance(node, ast.FunctionDef) else
                 {t.id for t in getattr(node, "targets", []) if isinstance(t, ast.Name)})
        if names & wanted:
            parts.append(ast.get_source_segment(mine, node))
    reader = read("xiaoxiang_reader.py")
    doc = ast.get_docstring(ast.parse(reader))
    body = _body(reader)
    a = body.index("# --- dev imports (removed in the bundle) ---")
    b = body.index("# --- end dev imports ---")
    body[a:b + 1] = ["MODEL = " + repr(model)]
    return "\n".join([
        "#!/usr/bin/env python3",
        '"""小象 (xiaoxiang) v' + model["version"] + ": " + doc + "\n\nRun: python3 xiaoxiang-local.py [--days N] [--json]",
        "Standard library only; no network access.  Built " + __import__("datetime").date.today().isoformat() + '."""',
        "from __future__ import annotations",
        "",
        "# ---- secret_scan: classical secret detector ----",
        *_body(read("secret_scan.py"), stop="def _read_inputs"),
        "# ---- 小象 classifier ----",
        "import math",
        "import re",
        "",
        "\n\n".join(parts),
        "",
        "# ---- log reader ----",
        *body,
        "",
    ])


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    e = sub.add_parser("eval"); e.add_argument("--dir", default=DEFAULT_DIR)
    x = sub.add_parser("export"); x.add_argument("--dir", default=DEFAULT_DIR); x.add_argument("out")
    c = sub.add_parser("classify"); c.add_argument("model"); c.add_argument("text")
    b = sub.add_parser("bundle"); b.add_argument("--dir", default=DEFAULT_DIR); b.add_argument("out")
    g = sub.add_parser("segment"); g.add_argument("text")
    v = sub.add_parser("seg-eval"); v.add_argument("--dir", default=DEFAULT_DIR)
    w = sub.add_parser("page"); w.add_argument("--dir", default=DEFAULT_DIR); w.add_argument("out")
    w.add_argument("--report", default=SAMPLE_REPORT,
                   help="JSON from `xiaoxiang-local.py --json`, drawn as figures (skipped if absent)")
    a = ap.parse_args(argv)
    if a.cmd == "eval":
        rows = load(a.dir)
        result = evaluate(rows)
        result["collisions"] = collisions(rows, safe_vocab(rows))[:20]
        json.dump(result, sys.stdout, indent=1, ensure_ascii=False); print()
    elif a.cmd == "export":
        rows = load(a.dir)
        model = export(rows, safe_vocab(rows))
        with open(a.out, "w", encoding="utf-8") as fh:
            json.dump(model, fh, ensure_ascii=False)
    elif a.cmd == "segment":
        json.dump(segment(a.text), sys.stdout, indent=1, ensure_ascii=False); print()
    elif a.cmd == "seg-eval":
        json.dump(seg_eval(a.dir), sys.stdout, indent=1, ensure_ascii=False); print()
    elif a.cmd == "bundle":
        code = bundle(load(a.dir))
        with open(a.out, "w", encoding="utf-8") as fh:
            fh.write(code)
        os.chmod(a.out, 0o755)
    elif a.cmd == "page":
        report = None
        if a.report and os.path.exists(a.report):
            with open(a.report, encoding="utf-8") as fh:
                report = json.load(fh)
        html = page(load(a.dir), report)
        with open(a.out, "w", encoding="utf-8") as fh:
            fh.write(html)
    else:
        with open(a.model, encoding="utf-8") as fh:
            model = json.load(fh)
        if not evidence(model, a.text):
            print("too little to go on: only common words are known")
        for intent, p in classify(model, a.text):
            print(f"{p:.2f}  {intent}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
