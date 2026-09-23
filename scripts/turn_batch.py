#!/usr/bin/env python3
"""turn_batch.py — build analysis requests for historical operator turns.

The live loop records a turn as it is sent and asks a delegate to interpret it.
This does the same for turns that were sent weeks ago: it reads the harvested
corpus, writes one request record per turn in the shape
session_turn_analysis.py already validates, and stops at a block boundary so
the work stays inspectable between blocks rather than running away.

  turn_batch.py plan   --from 2026-08-22 --to 2026-09-21
  turn_batch.py build  --from 2026-08-22 --to 2026-09-21 --block 100 --index 0
  turn_batch.py status

Records go under storage/operator-turns/batches/<block>/, never into the live
~/.emacs-graph directory: a historical reading and a live one must not be
confusable in the corpus they both feed.
"""
import argparse, json, os, re, sys

CORPUS = "/home/joe/code/storage/operator-turns/operator-turns-filtered.jsonl"
ORIGINS = "/home/joe/code/storage/operator-turns/turn-origins.jsonl"
OUT = "/home/joe/code/storage/operator-turns/batches"

# Sentence splitting mirrors what the live recorder does: break on terminal
# punctuation followed by whitespace, and keep the offsets exact, because every
# fragment the interpreter writes is checked against them.
SPLIT = re.compile(r"(?<=[.!?])\s+")


def sentences_of(text):
    out, at = [], 0
    for i, piece in enumerate(SPLIT.split(text), start=1):
        if not piece:
            continue
        start = text.index(piece, at)
        end = start + len(piece)
        at = end
        out.append({"id": f"s{i}", "start": start, "end": end, "text": piece,
                    "status": "unresolved", "cues": []})
    if not out and text.strip():
        out = [{"id": "s1", "start": 0, "end": len(text), "text": text,
                "status": "unresolved", "cues": []}]
    return out


def origins():
    """turn_id -> operator | agent | harness, from the invoke-start join."""
    out = {}
    if os.path.exists(ORIGINS):
        for line in open(ORIGINS, encoding="utf-8"):
            r = json.loads(line)
            out[r.get("turn_id") or r.get("id")] = r.get("origin")
    return out


def load(a):
    rows = [json.loads(l) for l in open(CORPUS, encoding="utf-8")]
    rows = [r for r in rows if a.frm <= (r.get("at") or "")[:10] <= a.to]
    rows = [r for r in rows if (r.get("text") or "").strip()]
    # A turn recorded under Joe's name is not always Joe: agent- and
    # harness-injected turns wear the same speaker through the Agency wiring.
    # They are few (56 of 3,415 in the audit window) and they are exactly the
    # ones that would teach the replay an agent's dispatch/review rhythm as if
    # it were the operator's. Unmatched turns are KEPT -- 30% of the window has
    # no invoke-start record to join against, and discarding them would throw
    # away real operator turns to avoid a handful of impostors.
    org = origins()
    for r in rows:
        r["_origin"] = org.get(r.get("turn_id")) or "unmatched"
    if not a.all_origins:
        rows = [r for r in rows if r["_origin"] in ("operator", "unmatched")]
    rows.sort(key=lambda r: r["at"])
    return rows


def cmd_plan(a):
    import collections
    rows = load(a)
    print(f"{len(rows)} turns in {a.frm}..{a.to} "
          f"({dict(collections.Counter(r['_origin'] for r in rows))})")
    print(f"{(len(rows) + a.block - 1) // a.block} blocks of {a.block}")
    words = sum(len((r.get('text') or '').split()) for r in rows)
    print(f"{words} words, median {sorted(len((r.get('text') or '').split()) for r in rows)[len(rows)//2]} per turn")


def cmd_build(a):
    rows = load(a)
    block = rows[a.index * a.block:(a.index + 1) * a.block]
    if not block:
        sys.exit(f"turn_batch: block {a.index} is empty ({len(rows)} turns total)")
    directory = os.path.join(OUT, f"{a.frm}_{a.to}_block{a.index:03d}")
    os.makedirs(directory, exist_ok=True)
    written = []
    for row in block:
        text = row["text"]
        record = {"version": 1, "source_text": text,
                  "offset_unit": "unicode-codepoints-zero-based-end-exclusive",
                  "sentences": sentences_of(text), "unmatched": [],
                  "created_at": row["at"], "tagging_failed": False,
                  "original_text": text,
                  # some harvested rows have no turn_id; the record still needs
                  # an id, so fall back to the transport id it was stored under
                  "agent_id": (row.get("turn_id") or "").rsplit("-turn-", 1)[0] or "unknown",
                  "session_id": row.get("session"), "turn_id": row.get("turn_id"),
                  "surface": "historical", "origin": row["_origin"],
                  "analysis_status": "requested"}
        path = os.path.join(directory, f"{(row.get('turn_id') or row['id'])}.json")
        with open(path, "w", encoding="utf-8") as fh:
            json.dump(record, fh)
        written.append(path)
    manifest = os.path.join(directory, "MANIFEST.json")
    json.dump({"from": a.frm, "to": a.to, "block": a.index, "size": len(written),
               "records": [os.path.basename(p) for p in written]},
              open(manifest, "w"), indent=1)
    print(f"{len(written)} records in {directory}")
    print(manifest)


def cmd_status(a):
    if not os.path.isdir(OUT):
        sys.exit("no batches yet")
    for name in sorted(os.listdir(OUT)):
        d = os.path.join(OUT, name)
        recs = [f for f in os.listdir(d) if f.endswith(".json") and f != "MANIFEST.json"]
        done = [f for f in recs if f.endswith(".analysis.json")]
        pending = [f for f in recs if not f.endswith(".analysis.json")
                   and f + ".analysis.json" not in recs]
        print(f"{name}: {len(pending)} pending, {len(done)} analysed")


if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("cmd", choices=["plan", "build", "status"])
    ap.add_argument("--from", dest="frm", default="2026-08-22")
    ap.add_argument("--to", default="2026-09-21")
    ap.add_argument("--block", type=int, default=100)
    ap.add_argument("--index", type=int, default=0)
    ap.add_argument("--all-origins", action="store_true",
                    help="include agent- and harness-injected turns too")
    a = ap.parse_args()
    {"plan": cmd_plan, "build": cmd_build, "status": cmd_status}[a.cmd](a)
