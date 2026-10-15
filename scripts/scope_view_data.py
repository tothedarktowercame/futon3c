#!/usr/bin/env python3
"""Build the scope-view data for one operator turn and its reply.

Output is the shape in futon3c/holes/excursions/E-scope-view.md:
{segments, layers, spans, edges, legend}.  Offsets are Unicode code points
into one segment's text, zero based, end exclusive (the turn records'
offset_unit).  Four sources feed it:

  acts     GET /api/alpha/xiang/scopes?session&turn   (象 acts, within/opens)
  text     GET /api/alpha/xiang/turns/<record-id>     (source_text, reply_text)
  patterns futon1b context-retrieval evidence for the turn: the patterns
           futon3a retrieved to support the agent's turn, joined on the
           recorded query, which is the operator text's first 100 characters
  memory   minus40 facts nearest each reply paragraph (the embeddings
           sidecar m40_near uses), each with its said/unsaid dates, so the
           edge can say whether the memory was in force when the turn was
           written

Run with the futon3a venv (sentence-transformers lives there):

  /home/joe/code/futon3a/.venv/bin/python -I scripts/scope_view_data.py \
      --session <id> --turn claude-16-turn-162 --out web/scopes/fixtures/x.json

--no-memory skips the embedding step and runs under plain python3.
"""
import argparse
import json
import os
import re
import sys
import urllib.parse
import urllib.request

AGENCY = os.environ.get("AGENCY_BASE", "http://localhost:7070")
FUTON1B = os.environ.get("FUTON1B_BASE", "http://localhost:7073")
LIBRARY = os.path.expanduser("~/code/futon3/library")
MINUS40_SCRIPTS = os.path.expanduser("~/code/minus40/scripts")

# The reply proforma key (CLAUDE.md) and 象's glosses, by kind.
MARKS = {
    "gist": "㊥", "propose": "㊭", "approve": "㊣", "disagree": "🈚",
    "qualify": "㊟", "explain": "🈖", "clarify": "🈯", "report": "㊢",
    "report-problem": "㊩", "verify": "㊬", "retract": "🈹", "unresolved": "🈳",
    "constrain": "🈲", "ask-action": "🈸", "delegate": "㊯", "prioritize": "㊝",
    "collect": "㊮", "extend": "🈕", "continue": "🈰", "defer": "🈝",
    "redirect": "🈘", "explore": "㊫", "withdraw": "🈡",
}
KIND_GLOSS = {
    "gist": "the one-paragraph summary that stands alone",
    "propose": "puts forward a course of action",
    "approve": "agreement the speaker actually holds",
    "disagree": "states that something said is wrong",
    "qualify": "limits or conditions an earlier point",
    "explain": "says how or why something is so",
    "clarify": "resolves an ambiguity, or asks a question",
    "report": "says what was done or found",
    "report-problem": "says that something produced does not fit the world",
    "verify": "something checked this turn",
    "retract": "takes back something said",
    "unresolved": "names an open question",
    "constrain": "sets a limit on what may be done",
    "ask-action": "asks the other party to do something",
    "delegate": "hands work to someone",
    "prioritize": "orders work by importance",
    "collect": "gathers items into one place",
    "extend": "adds to an earlier point",
    "continue": "takes up an earlier thread",
    "defer": "puts something off",
    "redirect": "turns attention elsewhere",
    "explore": "opens a line of inquiry",
    "withdraw": "ends an earlier act",
    "commit": "a git commit made during the turn",
    "pattern": "a pattern futon3a retrieved for the turn",
    "memory": "a minus40 fact (one statement from a memory file)",
    "query": "the text futon3a was asked with: the operator turn's first 100 characters",
}
RELATION_GLOSS = {
    "within": "the act is governed by the act it sits within",
    "opens": "the act opens a scope later acts sit within",
    "retrieved-for": "the pattern was retrieved by embedding similarity to the query; label is the cosine score",
    "near": "the memory fact is the nearest in meaning to the reply paragraph; label is the cosine score",
    "R3": "a reply paragraph answers inside the operator act it quotes or continues",
    "OPEN": "an act that nothing governs opens its own scope",
    "in-force": "the memory was said before the turn and not withdrawn by then",
    "retracted-since": "the memory was in force at the turn but has since stopped being said",
    "not-yet-said": "the memory was first said after the turn",
    "not-in-force": "the memory had stopped being said before the turn",
}


def get_json(url):
    with urllib.request.urlopen(url, timeout=30) as resp:
        return json.load(resp)


def find_record(session, turn):
    q = urllib.parse.urlencode({"session": session, "limit": 500})
    for t in get_json(f"{AGENCY}/api/alpha/xiang/turns?{q}")["turns"]:
        if t["turn-id"] == turn:
            return t["id"]
    sys.exit(f"no record for turn {turn} in session {session}")


def retrieval_for(agent, session, source_text):
    """The context-retrieval evidence whose query is this turn's prefix."""
    q = urllib.parse.urlencode({"tags": "context-retrieval", "author": agent,
                                "session-id": session, "limit": 50})
    req = urllib.request.Request(f"{FUTON1B}/api/alpha/evidence?{q}",
                                 headers={"Accept": "application/json"})
    with urllib.request.urlopen(req, timeout=30) as resp:
        entries = json.load(resp).get("entries", [])
    prefix = source_text[:100]
    for e in entries:
        body = e.get("evidence/body") or ""
        m = re.search(r'"query" "(.*?)", "results"', body, re.S)
        if not m:
            continue
        query = m.group(1).replace('\\"', '"')
        if query == prefix[:len(query)]:
            results = [{"id": r[0], "title": r[1], "score": float(r[2]), "rank": int(r[3])}
                       for r in re.findall(
                           r'\{:id "([^"]+)", :title "([^"]*)", :score ([0-9.]+), :rank (\d+)', body)]
            return {"evidence-id": e.get("evidence/id"), "at": e.get("evidence/at"),
                    "query": query, "results": results}
    return None


def pattern_text(pid):
    path = os.path.join(LIBRARY, pid + ".flexiarg")
    if not os.path.exists(path):
        return None
    title, conclusion = pid, ""
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            if line.startswith("@title "):
                title = line[7:].strip()
            elif line.startswith("! conclusion:"):
                conclusion = line.split(":", 1)[1].strip()
    return f"{title}. {conclusion}".strip()


def memory_near(paragraphs, k, turn_at):
    """[(para_index, fact, score)] for the k facts nearest each paragraph."""
    sys.path.insert(0, MINUS40_SCRIPTS)
    import numpy as np
    import m40
    from sentence_transformers import SentenceTransformer
    directory = os.path.join(m40.EMBED_ROOT, m40._model_slug(m40.NEAR_MODEL))
    with open(os.path.join(directory, "ids.json")) as fh:
        ids = json.load(fh)
    vectors = np.load(os.path.join(directory, "vectors.npy"))
    by_id = {f["id"]: f for f in m40.facts()}
    model = SentenceTransformer(m40.NEAR_MODEL)
    queries = model.encode(paragraphs, normalize_embeddings=True)
    out = []
    for i, q in enumerate(queries):
        scores = vectors @ q
        ranked = sorted(zip(ids, scores), key=lambda kv: -kv[1])
        shown = 0
        for fid, score in ranked:
            fact = by_id.get(fid)
            if fact is None:
                continue
            out.append((i, fact, float(score)))
            shown += 1
            if shown >= k:
                break
    return out


def memory_status(fact, turn_at):
    """Was the fact in force when the turn was written, by its said /
    unsaid / said-again dates (minus40's history), compared as ISO strings."""
    said = (fact.get("said") or "")[:19]
    unsaid = (fact.get("unsaid") or "")[:19]
    again = (fact.get("again") or "")[:19]
    t = turn_at[:19]
    if not said or said > t:
        return "not-yet-said"
    if unsaid and unsaid <= t:
        return "in-force" if again and unsaid < again <= t else "not-in-force"
    if unsaid and unsaid > t:
        return "in-force" if again and again > unsaid else "retracted-since"
    return "in-force"


def build(session, turn, k, with_memory):
    record_id = find_record(session, turn)
    view = get_json(f"{AGENCY}/api/alpha/xiang/turns/{urllib.parse.quote(record_id)}")
    rec = view["record"]
    agent = rec["agent_id"]
    source, reply = rec["source_text"], rec.get("reply_text") or ""
    q = urllib.parse.urlencode({"session": session, "turn": turn})
    scopes = get_json(f"{AGENCY}/api/alpha/xiang/scopes?{q}")

    op_seg, re_seg = turn, turn + "-reply"
    segments = [
        {"id": op_seg, "role": "operator", "label": f"joe · {rec['created_at']}", "text": source},
        {"id": re_seg, "role": "agent", "label": f"{agent} · {rec.get('reply_at') or ''}", "text": reply},
    ]
    layers = [{"id": "acts", "name": "speech acts (象)", "ink": "c"},
              {"id": "patterns", "name": "patterns retrieved for the turn", "ink": "m"},
              {"id": "memory", "name": "minus40 memory nearest the reply", "ink": "y"}]
    spans, edges, legend = [], [], {}

    # The route returns the settled exchange around the turn, which includes
    # the reply to the previous turn; its offsets are into that reply's text,
    # not this one's, so only this turn's acts are anchored here.
    kept = set()
    for act in scopes["acts"]:
        if (act.get("start") is None or act.get("side") not in ("operator", "agent")
                or not act["id"].startswith(turn + "-")):
            continue
        kept.add(act["id"])
        kind = act["kind"]
        spans.append({"id": act["id"], "seg": op_seg if act["side"] == "operator" else re_seg,
                      "start": act["start"], "end": act["end"], "layer": "acts",
                      "kind": kind, "mark": MARKS.get(kind), "weight": 1.0,
                      "note": KIND_GLOSS.get(kind, kind)})
        legend[kind] = KIND_GLOSS.get(kind, kind)
    for e in scopes["edges"]:
        rel = "within" if "within" in e else "opens" if "opens" in e else None
        if rel is None or e["act"] not in kept:
            continue
        # A parent in an earlier turn stays as a stub (the view draws its id).
        edges.append({"from": e["act"], "to": e[rel], "relation": rel, "label": e.get("rule")})
        legend[rel] = RELATION_GLOSS[rel]
        if e.get("rule"):
            legend[e["rule"]] = RELATION_GLOSS.get(e["rule"], e["rule"])

    retrieval = retrieval_for(agent, session, source)
    if retrieval:
        qid = turn + "-query"
        spans.append({"id": qid, "seg": op_seg, "start": 0, "end": len(retrieval["query"]),
                      "layer": "patterns", "kind": "query", "weight": 0.5,
                      "note": f"query recorded in {retrieval['evidence-id']} at {retrieval['at']}"})
        legend["query"] = KIND_GLOSS["query"]
        for r in retrieval["results"]:
            sid = "pattern:" + r["id"]
            text = pattern_text(r["id"]) or r["title"]
            segments.append({"id": sid, "role": "pattern", "label": r["id"], "text": text})
            spans.append({"id": sid + "#", "seg": sid, "start": 0, "end": len(text),
                          "layer": "patterns", "kind": "pattern", "weight": r["score"],
                          "note": f"rank {r['rank']}, cosine {r['score']:.4f} (futon3a embeddings)"})
            edges.append({"from": sid + "#", "to": qid, "relation": "retrieved-for",
                          "label": f"{r['score']:.4f}"})
        legend["pattern"] = KIND_GLOSS["pattern"]
        legend["retrieved-for"] = RELATION_GLOSS["retrieved-for"]

    if with_memory and reply:
        agent_spans = [s for s in spans if s["seg"] == re_seg and s["layer"] == "acts"]
        paragraphs = [reply[s["start"]:s["end"]] for s in agent_spans]
        turn_at = rec.get("reply_at") or rec["created_at"]
        seen = set()
        for i, fact, score in memory_near(paragraphs, k, turn_at):
            sid = "m40:" + fact["id"]
            if sid not in seen:
                seen.add(sid)
                segments.append({"id": sid, "role": "memory",
                                 "label": f"{fact['memory'].split('/')[-1]} · said {fact['said'][:10]}"
                                          + (f", unsaid {fact['unsaid'][:10]}" if fact.get("unsaid") else ""),
                                 "text": fact["text"]})
                spans.append({"id": sid + "#", "seg": sid, "start": 0, "end": len(fact["text"]),
                              "layer": "memory", "kind": "memory", "weight": round(score, 4),
                              "note": f"minus40 fact {fact['id']} from memory {fact['memory']}"})
            status = memory_status(fact, turn_at)
            edges.append({"from": agent_spans[i]["id"], "to": sid + "#", "relation": "near",
                          "label": f"{score:.4f} · {status}"})
            legend[status] = RELATION_GLOSS.get(status, status)
        legend["memory"] = KIND_GLOSS["memory"]
        legend["near"] = RELATION_GLOSS["near"]

    return {"turn": turn, "record": record_id, "session": session,
            "offset_unit": rec.get("offset_unit"),
            "segments": segments, "layers": layers, "spans": spans, "edges": edges,
            "legend": legend}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--session", required=True)
    ap.add_argument("--turn", required=True)
    ap.add_argument("--k", type=int, default=2, help="memory facts per reply paragraph")
    ap.add_argument("--no-memory", action="store_true")
    ap.add_argument("--out")
    args = ap.parse_args()
    data = build(args.session, args.turn, args.k, not args.no_memory)
    text = json.dumps(data, ensure_ascii=False, indent=1)
    if args.out:
        with open(args.out, "w", encoding="utf-8") as fh:
            fh.write(text + "\n")
        print(f"{args.out}: {len(data['segments'])} segments, {len(data['spans'])} spans, "
              f"{len(data['edges'])} edges, {len(data['legend'])} legend entries")
    else:
        print(text)


if __name__ == "__main__":
    main()
