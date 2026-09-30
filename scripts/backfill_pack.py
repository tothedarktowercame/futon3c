#!/usr/bin/env python3
"""backfill_pack.py -- batch everything a backfill reader fetches.

The 象 backfill readers annotate Joe's historical turns under
/home/joe/code/storage/operator-turns/batches/<block>/ at about 67 tool
calls per turn, almost all of it fetching: running the pattern search and
opening pattern files one at a time.  The judgement stays with the reader;
the fetching does not need to.

  backfill_pack.py pack DIR TURN-ID [TURN-ID ...] --out PACK.md
      One markdown file per batch of turns: each turn with its id,
      session, date, full text, sentence ids/offsets, the previous two
      turns of its session as context, MOVE and SUBJECT pattern hits per
      sentence (BM25 over the futon3 library, in-process via xlate), the
      whole family added when the turn names one, and one appendix entry
      per distinct pattern (@title, conclusion, context, IF, HOWEVER,
      THEN; missing parts marked absent) so the reader never opens a
      file.  Instructions and the closed intent list are at the top.

  backfill_pack.py publish DIR ANSWER.json
      Validates and publishes each element of the reader's answer through
      session_turn_analysis.py complete (the validator stays the
      authority); never overwrites an existing analysis; one bad element
      does not stop the others.  Prints a JSON summary.

Standard library only.  No network, no store, no model.
"""
from __future__ import annotations

import argparse
import glob
import json
import os
import re
import subprocess
import sys
import tempfile

HERE = os.path.dirname(os.path.abspath(__file__))
LIB = "/home/joe/code/futon3/library"
STA = os.path.join(HERE, "session_turn_analysis.py")

# The SUBJECT query is the sentence's own content words: tokens of the
# sentence minus this stopword list (plus pure punctuation).  It is a
# deliberately crude noun-phrase proxy -- the nouns and noun phrases are
# what the sentence is ABOUT, and dropping grammar words is enough to make
# BM25 retrieve the family the subject belongs to.
STOPWORDS = {
    "a", "an", "the", "and", "or", "but", "so", "if", "then", "of", "to",
    "in", "on", "for", "with", "at", "by", "from", "as", "is", "are", "was",
    "were", "be", "been", "being", "it", "its", "this", "that", "these",
    "those", "i", "we", "you", "he", "she", "they", "them", "me", "us",
    "my", "our", "your", "do", "does", "did", "have", "has", "had", "will",
    "would", "can", "could", "should", "shall", "may", "might", "not",
    "no", "yes", "ok", "just", "also", "very", "there", "here", "what",
    "which", "who", "when", "where", "how", "about", "into", "over",
    "let's", "don't", "i'm", "it's", "that's",
}

INSTRUCTIONS = """## Intents: closed list

Every fragment's `intent` is exactly one of:
  report-problem, explain, report, clarify, qualify, approve, disagree, collect, constrain,
  extend, propose, prioritize, redirect, defer, delegate, ask-action, continue, verify,
  explore, retract, withdraw, unresolved

Meanings: `legend-rows` in /home/joe/code/futon3/src-cljs/futon3/turnfeed/core.cljs (read each
`:aif` gloss). Two gaps are known; use these and say so:
- a question to the agent (asking it to explain, recommend, decide or forecast): intent `clarify`,
  rationale starting "Vocabulary gap: question —";
- a statement of feeling (exasperation, strain, confusion): intent `report`, rationale starting
  "Vocabulary gap: affect —".
Any other misfit: nearest intent plus "Vocabulary gap: <what is missing>". Never invent labels.

## How to work — one record at a time, with NO tool calls

For each turn, in order:
1. Read the turn, its sentences, and the previous turns of its session printed as context.
2. Decide the fragments by hand. Dictated turns break one thought across several "sentences":
   annotate the move ONCE, on the sentence where it lands, and give each pure continuation
   sentence an unresolved_reason such as "continues s3". No fragment per dictation shard.
3. Every sentence below already carries MOVE hits (query: the sentence itself) and SUBJECT hits
   (query: the sentence's content words). Read the pattern text ONCE in the appendix instead of
   running searches. Cite a pattern only if the move is the move it describes; otherwise record
   the best hit you read in `pattern_rejections` with the query named ("MOVE" or "SUBJECT") and
   why it does not fit. A fragment with neither a citation nor a recorded rejection fails the
   check. If nothing in the pack fits, name up to 3 searches per turn in a "more_searches"
   field (plain strings); the packer will not have run them.
4. Every uncited fragment needs a candidate (with `fragment` set to its sentence id and a real
   `parent`). If the same move genuinely recurs, reuse the earlier candidate id, and say why.
5. Choose display_cues by hand, within the budget; do not truncate mechanically.

## Answer format

One JSON array, one element per turn, in pack order. Each element is EXACTLY the analysis JSON
that `python3 scripts/session_turn_analysis.py template DIR/<id>.json` returns once filled, plus
an optional "candidates" key holding what would go in <id>.json.candidates.json, and the optional
"more_searches" list. The template of the first turn is included below as the example. Do not
write any file yourself: return the array; the publisher runs the validator.
"""


def load_docs(lib=LIB):
    """The BM25 index, in-process via xlate (imported cleanly)."""
    sys.path.insert(0, HERE)
    import xlate
    return xlate.load_index(), xlate


def find_hits(query, docs, xlate, n=8):
    """Top-n pattern ids for one query."""
    return [pid for pid, _score in xlate.bm25(query, docs, n)]


def subject_query(sentence_text):
    """The sentence's content words: tokens minus STOPWORDS (see the
    constant's comment)."""
    toks = re.findall(r"[A-Za-z0-9']+", sentence_text.lower())
    return " ".join(t for t in toks if t not in STOPWORDS) or sentence_text


def family_hits(turn_text, lib=LIB):
    """Every pattern of a family the turn names: a whole word (hyphen or
    space allowed) matching a directory under the library."""
    hits = []
    for entry in sorted(glob.glob(os.path.join(lib, "*"))):
        if not os.path.isdir(entry):
            continue
        family = os.path.basename(entry)
        if re.search(r"(?<![A-Za-z0-9])" + re.escape(family).replace(r"\-", r"[ -]")
                     + r"(?![A-Za-z0-9])", turn_text, re.I):
            hits += sorted(
                os.path.relpath(p, lib)[:-len(".flexiarg")]
                for p in glob.glob(os.path.join(entry, "*.flexiarg")))
    return hits


_FIELD_PATTERNS = {
    "title": re.compile(r"^@title (.+)$", re.M),
    "conclusion": re.compile(r"^! (?:conclusion|summary): (.+)$", re.M),
    "context": re.compile(r"^\s*\+ context: (.+)$", re.M),
    "IF": re.compile(r"^\s*IF: (.+)$", re.M | re.I),
    "HOWEVER": re.compile(r"^\s*HOWEVER: (.+)$", re.M | re.I),
    "THEN": re.compile(r"^\s*THEN: (.+)$", re.M | re.I),
}


def read_pattern(pid, lib=LIB):
    """One appendix entry: id, @title and the named parts; absent marked."""
    path = os.path.join(lib, pid + ".flexiarg")
    if not os.path.isfile(path):
        return {"id": pid, "absent": "no such flexiarg"}
    text = open(path, encoding="utf-8", errors="replace").read()
    entry = {"id": pid}
    for name, pattern in _FIELD_PATTERNS.items():
        m = pattern.search(text)
        entry[name] = m.group(1).strip() if m else "(absent)"
    return entry


def previous_turns(directory, record, count=2):
    """The previous turns of the same session in the same block, text only."""
    session = record.get("session_id")
    out = []
    for path in sorted(glob.glob(os.path.join(directory, "*.json"))):
        if path.endswith((".analysis.json", ".candidates.json")):
            continue
        try:
            with open(path, encoding="utf-8") as fh:
                other = json.load(fh)
        except (OSError, ValueError):
            continue
        if other.get("session_id") != session or other is record:
            continue
        out.append(other)
    out.sort(key=lambda r: str(r.get("created_at")))
    mine = [r for r in out if str(r.get("created_at")) <= str(record.get("created_at"))
            and r.get("turn_id") != record.get("turn_id")]
    return mine[-count:]


def build_pack(directory, turn_ids, out_path, docs=None, xlate=None, lib=LIB):
    """Write PACK.md for TURN_IDS; returns stats."""
    if docs is None or xlate is None:
        docs, xlate = load_docs(lib)
    lines = [INSTRUCTIONS]
    appendix_ids = []
    sys.path.insert(0, HERE)
    import session_turn_analysis as sta
    for index, tid in enumerate(turn_ids):
        path = os.path.join(directory, tid + ".json")
        record = json.load(open(path, encoding="utf-8"))
        text = record.get("source_text", "")
        lines.append(f"\n---\n\n## Turn {tid}\n\n"
                     f"- session: `{record.get('session_id')}`\n"
                     f"- created_at: `{record.get('created_at')}`\n")
        context = previous_turns(directory, record)
        for ctx in context:
            lines.append(f"\n### context: previous turn `{ctx.get('turn_id')}` "
                         f"(`{ctx.get('created_at')}`)\n\n> "
                         + "\n> ".join((ctx.get("original_text") or "").splitlines()))
        lines.append(f"\n### source_text\n\n```\n{text}\n```\n")
        lines.append("\n### sentences and hits\n")
        fam = family_hits(text, lib)
        turn_ids_hit = []
        for sentence in record.get("sentences") or []:
            stext = sentence.get("text", "")
            move = find_hits(stext, docs, xlate)
            subj = find_hits(subject_query(stext), docs, xlate)
            turn_ids_hit += move + subj
            lines.append(f"\n**{sentence['id']}** ({sentence['start']}..{sentence['end']}) "
                         f"`{stext}`\n")
            lines.append("- MOVE: " + (", ".join(move) or "(no hits)"))
            lines.append("- SUBJECT: " + (", ".join(subj) or "(no hits)"))
        if fam:
            lines.append("\n### family named by this turn (all of it read)\n")
            lines.append(", ".join(fam))
        turn_ids_hit += fam
        appendix_ids += turn_ids_hit
        if index == 0:
            lines.append("\n### example: the filled template of THIS turn\n\n```json\n"
                         + json.dumps(sta.template(record), ensure_ascii=False, indent=1)
                         + "\n```")
    distinct = sorted(set(appendix_ids))
    lines.append("\n---\n\n## Appendix: pattern texts (one entry each)\n")
    for pid in distinct:
        entry = read_pattern(pid, lib)
        lines.append(f"\n### {pid}\n")
        for key in ("title", "conclusion", "context", "IF", "HOWEVER", "THEN"):
            if key in entry:
                lines.append(f"- {key}: {entry[key]}")
        if "absent" in entry:
            lines.append(f"- {entry['absent']}")
    body = "\n".join(lines) + "\n"
    with open(out_path, "w", encoding="utf-8") as fh:
        fh.write(body)
    return {"turns": len(turn_ids), "patterns": len(distinct),
            "bytes": len(body.encode()), "words": len(body.split())}


def publish(directory, answer_path, order=()):
    """Publish each element through the validator; never overwrite.

    A element's turn is its "turn_id" key when present, else the Nth
    positional id in ORDER (pack order).  One bad element does not stop
    the others."""
    elements = json.load(open(answer_path, encoding="utf-8"))
    results = []
    for index, element in enumerate(elements):
        tid = (element.get("turn_id")
               or (order[index] if index < len(order) else None)
               or element.get("id"))
        if not tid:
            results.append({"turn": None, "published": False,
                            "reason": "no turn id (element has no turn_id key "
                                      "and no positional id for this position)"})
            continue
        request = os.path.join(directory, tid + ".json")
        analysis_path = request + ".analysis.json"
        entry = {"turn": tid}
        if os.path.exists(analysis_path):
            entry.update({"published": False, "reason": "analysis already exists"})
            results.append(entry)
            continue
        payload = {k: v for k, v in element.items()
                   if k not in ("turn_id", "id", "candidates", "more_searches")}
        with tempfile.NamedTemporaryFile("w", suffix=".json", delete=False,
                                         encoding="utf-8") as tmp:
            json.dump(payload, tmp, ensure_ascii=False, indent=1)
            tmp_path = tmp.name
        proc = subprocess.run([sys.executable, STA, "complete", request, tmp_path],
                              capture_output=True, text=True)
        os.unlink(tmp_path)
        if proc.returncode == 0:
            entry["published"] = True
            candidates = element.get("candidates")
            cand_path = request + ".candidates.json"
            if candidates and not os.path.exists(cand_path):
                with open(cand_path, "w", encoding="utf-8") as fh:
                    json.dump(candidates, fh, ensure_ascii=False, indent=1)
                entry["candidates"] = "written"
        else:
            entry.update({"published": False,
                          "reason": (proc.stderr or proc.stdout).strip()[-400:]})
        results.append(entry)
    summary = {"asked": len(elements),
               "published": sum(1 for r in results if r.get("published")),
               "refused": sum(1 for r in results if not r.get("published")),
               "turns": results}
    print(json.dumps(summary, ensure_ascii=False, indent=1))
    return 0


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    p = sub.add_parser("pack")
    p.add_argument("directory")
    p.add_argument("turn_ids", nargs="+")
    p.add_argument("--out", required=True)
    b = sub.add_parser("publish")
    b.add_argument("directory")
    b.add_argument("answer")
    b.add_argument("turn_ids", nargs="*",
                   help="turn ids in pack order, when elements carry no turn_id key")
    args = ap.parse_args(argv)
    if args.cmd == "pack":
        stats = build_pack(args.directory, args.turn_ids, args.out)
        print(json.dumps(stats, ensure_ascii=False))
    else:
        return publish(args.directory, args.answer, args.turn_ids)
    return 0


if __name__ == "__main__":
    sys.exit(main())
