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
  memory   minus40 memories that REACHED the turn: the agent's recorded
           minus40 tool calls (Zai harness :turn-round entries, each call
           with its full :result-text since futon3c c7408261b), the memory
           names read back from those results, and, where the reply shares
           a run of words or the memory's name with a memory it reached, an
           "echoed" edge from the reply text to the memory. A turn with no
           recorded call gets an empty memory layer: nothing reached it that
           the record can show.

  --near adds the older resemblance layer (facts nearest each reply
  paragraph by embedding). It is similarity, not evidence of use, and the
  layer says so. It needs the futon3a venv (sentence-transformers):

  /home/joe/code/futon3a/.venv/bin/python -I scripts/scope_view_data.py \
      --session <id> --turn claude-16-turn-162 --near --out web/scopes/fixtures/x.json

Without --near plain python3 -I suffices. For a Zai seat, --session is the
Zai session id (zai-...) and --turn the Agency job id (invoke-...).
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
    "memory": "a minus40 memory (its statements now in force) that a recorded tool call returned to the agent",
    "fact": "a minus40 fact (one statement from a memory file) resembling the reply; resemblance only",
    "call": "a minus40 tool call the agent made during the turn, from the harness round record",
    "echo": "a run of the reply's words that also occurs in a memory the agent reached",
    "query": "the text futon3a was asked with: the operator turn's first 100 characters",
}
RELATION_GLOSS = {
    "within": "the act is governed by the act it sits within",
    "opens": "the act opens a scope later acts sit within",
    "retrieved-for": "the pattern was retrieved by embedding similarity to the query; label is the cosine score",
    "near": "the fact is the nearest in meaning to the reply paragraph; label is the cosine score. Resemblance, not evidence the agent saw it",
    "reached": "the memory's name appears in the result of this recorded tool call, so its text entered the agent's context at that round",
    "echoed": "the reply shares the labelled run of words (or the memory's name) with a memory the agent reached",
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
    for q in ({"session": session, "limit": 500}, {"limit": 2000}):
        for t in get_json(f"{AGENCY}/api/alpha/xiang/turns?{urllib.parse.urlencode(q)}")["turns"]:
            if t["turn-id"] == turn:
                return t["id"]
    sys.exit(f"no record for turn {turn} in session {session}")


def evidence(**params):
    q = urllib.parse.urlencode({k.replace("_", "-"): v for k, v in params.items()})
    return get_json(f"{AGENCY}/api/alpha/evidence?{q}").get("entries", [])


MEMORY_TOOLS = ("m40_find", "m40_state", "m40_asof", "m40_history", "m40_near")


def trace_for(agent, session, turn):
    """The minus40 tool calls the agent made in this turn, from the Zai
    harness round records (zai_api.clj persist-round!). The 象 record's
    turn id is the Agency job id; the harness :turn-start entry joins it
    (:dispatch-id) to the harness's own turn id, which the rounds carry."""
    harness_turns = {e["evidence/body"]["turn-id"]
                     for e in evidence(tag="turn-start", author=agent, session_id=session, limit=1000)
                     if e["evidence/body"].get("dispatch-id") == turn} | {turn}
    calls = []
    for e in evidence(tag="turn-round", author=agent, session_id=session, limit=5000):
        body = e["evidence/body"]
        if body.get("turn-id") not in harness_turns:
            continue
        for i, c in enumerate(body.get("calls") or []):
            if c.get("tool") in MEMORY_TOOLS:
                calls.append({"round": int(body.get("round") or 0), "index": i, "tool": c["tool"],
                              "args": c.get("args", ""), "error": c.get("error?", False),
                              "text": result_text(c),
                              "truncated": ("result-text" not in c
                                            or bool(re.search(r"…\[\+\d+ chars\]$", c["result-text"]))),
                              "evidence-id": e["evidence/id"], "at": e["evidence/at"]})
    calls.sort(key=lambda c: (c["round"], c["index"]))
    return calls


def result_text(call):
    raw = call.get("result-text")
    if raw is None:
        raw = (call.get("result") or {}).get("preview", "")
    raw = re.sub(r"…\[\+\d+ chars\]$", "", raw)
    try:
        parsed = json.loads(raw)
    except ValueError:
        # The record was cut inside the JSON string: take what is left of it.
        m = re.match(r'\{"ok":true,"result":"(.*)', raw, re.S)
        if not m:
            return raw
        try:
            return json.loads('"' + m.group(1).rsplit('"', 1)[0] + '"')
        except ValueError:
            return m.group(1).replace("\\n", "\n")
    if isinstance(parsed, dict):
        return str(parsed.get("result") or parsed.get("error") or "")
    return str(parsed)


def memories_named(tool, text, facts_by_text):
    """(memory name, project or None) pairs a minus40 tool result names,
    per the renderers in minus40/scripts/m40_mcp.py and m40.py."""
    out = []
    lines = text.split("\n")
    if tool == "m40_find":
        for ln in lines:
            m = re.match(r"^(\S+)  (\S+)(?:  \(\d+ versions\))?$", ln)
            if m:
                out.append((m.group(1), m.group(2)))
    elif tool == "m40_asof":
        for ln in lines[1:]:
            if ln and not ln.startswith(" ") and not ln.startswith("…"):
                out.append((ln.strip(), None))
    elif tool == "m40_near":
        for ln in lines:
            m = re.match(r"^[0-9.]+  \[[^\]]*\]  (\S+)  ", ln)
            if m:
                out.append((m.group(1), None))
    elif tool == "m40_history":
        for ln in lines[1:]:
            m = re.match(r"^\d{4}-\d\d-\d\d [+\-↺] (\S+)", ln)
            if m and not m.group(1).endswith("…"):
                out.append((m.group(1), None))
    elif tool == "m40_state":
        # Lines are "[date] text (N earlier)": no name, so match the text.
        for ln in lines[1:]:
            m = re.match(r"^\[[^\]]*\] (.*?)(?:  \(\d+ earlier\))?$", ln)
            if m and m.group(1) in facts_by_text:
                f = facts_by_text[m.group(1)]
                out.append((f["memory"].split("/", 1)[-1], f["memory"].split("/", 1)[0]))
    seen, uniq = set(), []
    for pair in out:
        if pair not in seen:
            seen.add(pair)
            uniq.append(pair)
    return uniq


def m40_facts():
    sys.path.insert(0, MINUS40_SCRIPTS)
    import m40
    return m40.facts()


def memory_text(facts):
    """The memory as the agent could have read it: statements in force,
    else (a memory wholly withdrawn) its last statements."""
    current = [f for f in facts if not f.get("unsaid") or f.get("again")]
    return "\n".join(f["text"] for f in (current or facts))


WORD = re.compile(r"\w+", re.U)
LINK = re.compile(r"\[\[[^\]]*\]\]")


def echoes(reply, memory_name, text, n=5):
    """Runs of N words the reply shares with the memory text, and the
    memory's own name if the reply uses it. Returns (start, end, label)
    into the reply. String match, no model."""
    found = []
    for m in re.finditer(re.escape(memory_name), reply):
        found.append((m.start(), m.end(), memory_name))
    reply_words = [(w.group().lower(), w.start(), w.end()) for w in WORD.finditer(reply)]
    # A [[link]] names another memory; it is not this memory's own words, and
    # left in makes a reply that names the linked memory read as echoing this
    # one (proof2-dispatch-pool, zai-1 turn 6f64d143, 2026-10-16). The text
    # shown in the view keeps its links; only the matching drops them.
    mem_words = [w.group().lower() for w in WORD.finditer(LINK.sub(" ", text))]
    grams = {tuple(mem_words[i:i + n]) for i in range(len(mem_words) - n + 1)}
    i = 0
    while i <= len(reply_words) - n:
        if tuple(w[0] for w in reply_words[i:i + n]) in grams:
            j = i + n
            while j < len(reply_words) and tuple(w[0] for w in reply_words[j - n + 1:j + 1]) in grams:
                j += 1
            start, end = reply_words[i][1], reply_words[j - 1][2]
            found.append((start, end, reply[start:end]))
            i = j
        else:
            i += 1
    return found


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


def build(session, turn, k, with_near):
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
              {"id": "memory", "name": "minus40 memory that reached the turn (tool trace)", "ink": "y"}]
    if with_near:
        layers.append({"id": "near", "name": "minus40 facts resembling the reply (resemblance, not use)", "ink": "y"})
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

    turn_at = rec.get("reply_at") or rec["created_at"]
    calls = trace_for(agent, session, turn)
    if calls:
        facts = m40_facts()
        by_name = {}
        for f in facts:
            project, _, name = f["memory"].partition("/")
            by_name.setdefault(name, []).append(f)
        by_text = {f["text"]: f for f in facts}
        placed = {}
        for c in calls:
            cid = f"call:{c['round']}:{c['index']}"
            named = [] if c["error"] else memories_named(c["tool"], c["text"], by_text)
            head = (f"{c['tool']} {c['args']}\n→ "
                    + (f"error: {c['text'][:200]}" if c["error"]
                       else f"{len(named)} memories named"
                            + (" (result cut at the record's cap; the rest is lost)" if c["truncated"] else "")))
            segments.append({"id": cid, "role": "call", "label": f"{c['tool']} · round {c['round']}", "text": head})
            spans.append({"id": cid + "#", "seg": cid, "start": 0, "end": len(head), "layer": "memory",
                          "kind": "call", "weight": 0.5,
                          "note": f"recorded in {c['evidence-id']} at {c['at']}"})
            legend["call"] = KIND_GLOSS["call"]
            for name, project in named:
                cands = by_name.get(name, [])
                if project:
                    cands = [f for f in cands if f["memory"].startswith(project + "/")] or cands
                if not cands:
                    continue
                mem = cands[0]["memory"]
                sid = "m40:" + mem
                if sid not in placed:
                    text = memory_text(cands)
                    newest = max(cands, key=lambda f: f["said"])
                    status = memory_status(newest, turn_at)
                    segments.append({"id": sid, "role": "memory",
                                     "label": f"{name} · {mem.split('/', 1)[0]} · said {newest['said'][:10]}",
                                     "text": text})
                    spans.append({"id": sid + "#", "seg": sid, "start": 0, "end": len(text),
                                  "layer": "memory", "kind": "memory", "weight": 1.0,
                                  "note": f"minus40 memory {mem}: {len(cands)} facts, {status} at the reply"})
                    placed[sid] = (text, status)
                    for k_, (start, end, label) in enumerate(echoes(reply, name, text)):
                        eid = f"{sid}~{k_}"
                        spans.append({"id": eid, "seg": re_seg, "start": start, "end": end,
                                      "layer": "memory", "kind": "echo", "weight": 1.0,
                                      "note": f"shared with {mem}"})
                        edges.append({"from": eid, "to": sid + "#", "relation": "echoed", "label": label})
                        legend["echo"] = KIND_GLOSS["echo"]
                        legend["echoed"] = RELATION_GLOSS["echoed"]
                status = placed[sid][1]
                edges.append({"from": sid + "#", "to": cid + "#", "relation": "reached",
                              "label": f"{c['tool']} round {c['round']} · {status}"})
                legend[status] = RELATION_GLOSS.get(status, status)
        legend["memory"] = KIND_GLOSS["memory"]
        legend["reached"] = RELATION_GLOSS["reached"]
    else:
        layers[2]["name"] += " — none recorded for this turn"

    if with_near and reply:
        agent_spans = [s for s in spans if s["seg"] == re_seg and s["layer"] == "acts"]
        paragraphs = [reply[s["start"]:s["end"]] for s in agent_spans]
        seen = set()
        for i, fact, score in memory_near(paragraphs, k, turn_at):
            sid = "near:" + fact["id"]
            if sid not in seen:
                seen.add(sid)
                segments.append({"id": sid, "role": "memory",
                                 "label": f"{fact['memory'].split('/')[-1]} · said {fact['said'][:10]}"
                                          + (f", unsaid {fact['unsaid'][:10]}" if fact.get("unsaid") else ""),
                                 "text": fact["text"]})
                spans.append({"id": sid + "#", "seg": sid, "start": 0, "end": len(fact["text"]),
                              "layer": "near", "kind": "fact", "weight": round(score, 4),
                              "note": f"minus40 fact {fact['id']} from memory {fact['memory']}; resemblance only"})
            status = memory_status(fact, turn_at)
            edges.append({"from": agent_spans[i]["id"], "to": sid + "#", "relation": "near",
                          "label": f"{score:.4f} · {status}"})
            legend[status] = RELATION_GLOSS.get(status, status)
        legend["fact"] = KIND_GLOSS["fact"]
        legend["near"] = RELATION_GLOSS["near"]

    return {"turn": turn, "record": record_id, "session": session,
            "offset_unit": rec.get("offset_unit"),
            "segments": segments, "layers": layers, "spans": spans, "edges": edges,
            "legend": legend}


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--session", required=True)
    ap.add_argument("--turn", required=True)
    ap.add_argument("--near", action="store_true",
                    help="add the resemblance layer (facts nearest each reply paragraph; futon3a venv)")
    ap.add_argument("--k", type=int, default=2, help="with --near: facts per reply paragraph")
    ap.add_argument("--out")
    args = ap.parse_args()
    data = build(args.session, args.turn, args.k, args.near)
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
