#!/usr/bin/env python3
"""驴 rerank offline experiment (read-only; Joe approved 2026-10-24).

Question: does a Haiku reranker (the future 驴 seat) pick patterns that
actually OCCUR more often than the embedding retrieval does?

Per turn n (operator record with both :proposed_patterns[*].ranked and
:pattern_verdicts):

  E (embedding)  the recorded :ranked top 3.
  L (驴)          Haiku reranks a classical candidate pool and keeps 3:
                 (a) the embedding top 20 for the turn's source_text,
                     re-running the same search the pattern-card provider
                     feeds from (futon3a/scripts/notions_search.py over the
                     minilm pattern embeddings; the original proto-text
                     query is not recorded, so the turn's source_text is
                     used instead — deviation noted in the LAB);
                 (b) giant-graph neighbours (session-cascade graph.json) of
                     patterns with status occurrence/partial in the
                     :pattern_verdicts of EARLIER records of the session,
                     up to 15, ordered by edge kind why, how, co-cited,
                     rejected-beside, next-in-session.
                 Each pick must carry a verbatim >=5-word quote from turn
                 n's operator text that its IF names; a pick failing the
                 string check is dropped and the next taken.

Scoring: one 馬-sonnet whistle per turn maps the UNION of E's and L's picks,
blind to which arm proposed which. The 馬 prompt and the map checks are
ported from futon3c.xiang.ma-occurrence (prompt-for / check-map /
chain-window, window = the record plus up to 7 earlier operator turns) —
ported to this script, NOT run through the namespace, so nothing is written
to any record.

Usage:
  python3 -I futon3c/scripts/lu_rerank_offline.py \\
      --out /tmp/lu_rerank_results.json \\
      [--store ~/.emacs-graph/session-turn-analysis]
"""

from __future__ import annotations

import argparse
import json
import os
import re
import subprocess
import sys
import time
import urllib.request

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import occurrence_square as osq  # noqa: E402

STORE_DEFAULT = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
GRAPH = os.path.expanduser(
    "~/notes/experiments/2026-10-24-session-cascade/graph.json")
FUTON3A = os.path.expanduser("~/code/futon3a")
EMBEDDINGS = FUTON3A + "/resources/notions/minilm_pattern_embeddings.json"
WHISTLE = "http://127.0.0.1:7070/api/alpha/whistle"
ROLE_NAMES = ["I1", "I2", "G", "B"]
WINDOW_N = 8
MIN_QUOTE_WORDS = 5
EDGE_KIND_ORDER = ["why", "how", "co-cited", "rejected-beside", "next-in-session"]


# ------------------------------------------------------------------ the turns

def population(store):
    """(id, record) of operator records with ranked proposals AND verdicts."""
    out = []
    for name in sorted(os.listdir(store)):
        if not (name.startswith("turn-") and name.endswith(".json")):
            continue
        if ".analysis." in name or ".draft." in name:
            continue
        try:
            with open(os.path.join(store, name)) as h:
                rec = json.load(h)
        except Exception:
            continue
        if not isinstance(rec, dict) or rec.get("origin") != "operator":
            continue
        pp = rec.get("proposed_patterns") or []
        if any(p.get("ranked") for p in pp) and rec.get("pattern_verdicts"):
            out.append((name[:-len(".json")], rec))
    out.sort(key=lambda t: str(t[1].get("created_at")))
    return out


def ranked_ids(rec):
    """E's picks: the recorded :ranked top 3, ~ stripped."""
    for p in rec.get("proposed_patterns") or []:
        if p.get("ranked"):
            return [str(r["id"]).lstrip("~") for r in p["ranked"]][:3]
    return []


def session_operator_records(store, session_id):
    """{id: record} of the session's operator records, for the window."""
    out = {}
    for name in os.listdir(store):
        if not (name.startswith("turn-") and name.endswith(".json")):
            continue
        if ".analysis." in name or ".draft." in name:
            continue
        try:
            with open(os.path.join(store, name)) as h:
                rec = json.load(h)
        except Exception:
            continue
        if (isinstance(rec, dict) and rec.get("origin") == "operator"
                and rec.get("session_id") == session_id):
            out[name[:-len(".json")]] = rec
    return out


def chain_window(all_records, record_id):
    """The record plus up to 7 earlier operator records of the session,
    oldest first — ma-occurrence's chain-window, ported."""
    rec = all_records.get(record_id)
    if rec is None:
        return []
    at = str(rec.get("created_at"))
    same = [(rid, r) for rid, r in all_records.items()
            if (str(r.get("created_at")) <= at or rid == record_id)]
    same.sort(key=lambda t: str(t[1].get("created_at")))
    if record_id not in [rid for rid, _ in same]:
        same.append((record_id, rec))
    return same[-WINDOW_N:]


# ------------------------------------------------------------- the L pipeline

def embedding_top20(text):
    """The same search the pattern-card provider feeds from, re-run with
    k=20, read-only (notions_search.py one-shot over the same embeddings)."""
    cmd = [FUTON3A + "/.venv/bin/python3", "-I",
           FUTON3A + "/scripts/notions_search.py",
           "--embeddings", EMBEDDINGS,
           "--query", text[:4000], "--top", "20", "--json"]
    env = dict(os.environ, HF_HUB_OFFLINE="1", TRANSFORMERS_OFFLINE="1")
    res = subprocess.run(cmd, capture_output=True, text=True, timeout=120, env=env)
    if res.returncode != 0:
        raise RuntimeError("notions_search failed: " + res.stderr[-300:])
    rows = json.loads(res.stdout)
    return [str(r["id"]).lstrip("~") for r in rows if r.get("id")]


def graph_neighbours(graph, seeds, cap=15):
    """Neighbours of SEEDS, ordered by edge kind, capped."""
    prio = {k: i for i, k in enumerate(EDGE_KIND_ORDER)}
    found = {}
    for e in graph["edges"]:
        if e["a"] in seeds or e["b"] in seeds:
            other = e["b"] if e["a"] in seeds else e["a"]
            if other not in seeds:
                best = found.get(other)
                rank = prio.get(e["kind"], 99)
                if best is None or rank < best:
                    found[other] = rank
    ordered = sorted(found.items(), key=lambda kv: (kv[1], kv[0]))
    return [pid for pid, _ in ordered[:cap]]


def seed_patterns_earlier(all_records, record_id):
    """Patterns with status occurrence/partial in EARLIER records' verdicts."""
    rec = all_records[record_id]
    at = str(rec.get("created_at"))
    seeds = {}
    for rid, r in all_records.items():
        if rid == record_id or str(r.get("created_at")) >= at:
            continue
        v = r.get("pattern_verdicts")
        if not isinstance(v, dict):
            continue
        for p in v.get("patterns") or []:
            if p.get("status") in ("occurrence", "partial"):
                seeds[str(p.get("pattern")).lstrip("~")] = True
    return seeds


def haiku_picks(candidates, source_text):
    """One Haiku call: pick 3 of the candidates for this operator turn, each
    with a verbatim >=5-word quote from the operator text naming its IF.
    Returns (ordered candidate picks [(pid, quote)], dropped_count)."""
    if not candidates:
        return [], 0
    blocks = []
    readable = []
    for pid in candidates:
        try:
            pat = osq.read_pattern(pid)
        except (FileNotFoundError, OSError):
            continue
        readable.append(pid)
        parts = pat["parts"]
        blocks.append(
            "--- pattern %s\nIF: %s\nHOWEVER: %s\nTHEN: %s\nBECAUSE: %s"
            % (pid,
               re.sub(r"\s+", " ", parts.get("IF") or "MISSING"),
               re.sub(r"\s+", " ", parts.get("HOWEVER") or "MISSING"),
               re.sub(r"\s+", " ", parts.get("THEN") or "MISSING"),
               re.sub(r"\s+", " ", parts.get("BECAUSE") or "MISSING")))
    prompt = (
        "A reranking task. You write one JSON reply; nothing else.\n\n"
        "Below are candidate patterns and one operator turn. Rank the SIX "
        "patterns whose IF best names a situation element this operator turn "
        "actually contains. For each pick, give a verbatim quote of at least "
        "5 words, copied exactly from the operator text, that the pattern's "
        "IF names (it will be checked by string match).\n\n"
        "Operator turn:\n" + source_text + "\n\n"
        + "\n\n".join(blocks) +
        "\n\nReply with strict JSON only, an array of at most 6 objects in "
        "ranked order:\n"
        '[{"pattern": "family/name", "quote": "verbatim >=5 words from the '
        'operator text"}]')
    raw = ask_claude_cli("claude-haiku-5-5", prompt, timeout=240)
    m = re.search(r"\[.*\]", raw, re.S)
    if not m:
        return [], 0
    try:
        rows = json.loads(m.group(0))
    except ValueError:
        return [], 0
    picked, dropped = [], 0
    seen = set()
    for r in rows:
        if not isinstance(r, dict):
            continue
        pid = str(r.get("pattern", "")).lstrip("~")
        quote = re.sub(r"\s+", " ", str(r.get("quote") or "")).strip()
        if pid in seen or pid not in readable:
            continue
        seen.add(pid)
        words = quote.split()
        if len(words) >= MIN_QUOTE_WORDS and quote in re.sub(r"\s+", " ", source_text):
            picked.append((pid, quote))
        else:
            dropped += 1
        if len(picked) >= 3:
            break
    return picked, dropped


def ask_claude_cli(model, prompt, timeout):
    """minus40/scripts/judge.py ask_claude_cli (:69), verbatim helper."""
    res = subprocess.run(["claude", "-p", "--model", model, "--output-format", "text"],
                         input=prompt, capture_output=True, text=True, timeout=timeout)
    if res.returncode != 0:
        raise RuntimeError("claude -p failed: %s" % res.stderr[-500:])
    return res.stdout


# -------------------------------------------------------------- the 馬 scoring

def whistle_ma(prompt, timeout_ms=180000):
    """ma-occurrence's whistle-ask, ported: a bounded synchronous whistle."""
    body = json.dumps({"agent-id": "馬-sonnet", "prompt": prompt,
                       "timeout-ms": timeout_ms, "caller": "lu-rerank-offline"}
                      ).encode()
    req = urllib.request.Request(WHISTLE, data=body,
                                 headers={"Content-Type": "application/json"})
    with urllib.request.urlopen(req, timeout=(timeout_ms / 1000 + 30)) as resp:
        out = json.load(resp)
    return out.get("response") if out.get("ok") else None


def ma_prompt(window, record_id, patterns):
    """ma-occurrence's prompt-for, ported verbatim in structure; BLIND: no
    arm, source or proposer is named for any pattern."""
    turns = "\n\n".join(
        "--- turn %s (%s)\noperator:\n%s\n\nreply:\n%s"
        % (rid, str(r.get("created_at") or ""),
           str(r.get("source_text") or ""), str(r.get("reply_text") or ""))
        for rid, r in window)
    blocks = []
    for p in patterns:
        parts = p["parts"]
        blocks.append(
            "--- pattern %s\nIF: %s\nHOWEVER: %s\nTHEN: %s\nBECAUSE: %s"
            % (p["id"],
               re.sub(r"\s+", " ", parts.get("IF") or "MISSING"),
               re.sub(r"\s+", " ", parts.get("HOWEVER") or "MISSING"),
               re.sub(r"\s+", " ", parts.get("THEN") or "MISSING"),
               re.sub(r"\s+", " ", parts.get("BECAUSE") or "MISSING")))
    return (
        "From lu-rerank-offline to 馬-sonnet. A bounded reading task. You write one JSON reply; nothing else.\n\n"
        "Rule (CASCADE-SPEC-v3 §2): an occurrence of a pattern maps each of its four parts "
        "(IF -> I1, HOWEVER -> I2, THEN -> B, BECAUSE -> G) to one situation element, meaning "
        "something the turns actually contain that the part's text names. A part with no such "
        "element is void. The test is strict: the element must be what the part describes, not "
        "merely something on the same topic.\n\n"
        "The turns below are the chain window, oldest first; the proposal turn is \""
        + record_id + "\".\n\nTurns:\n" + turns +
        "\n\nProposed patterns, with each pattern's four parts:\n" + "\n\n".join(blocks) +
        "\n\nFor every pattern above, produce two readings:\n"
        "- \"turn\": map each part using only the proposal turn (its operator text and its reply).\n"
        "- \"chain\": map each part using any turn in the window, earlier or later.\n\n"
        "For every map that is not void, give:\n"
        "- \"element\": a short name you choose for the situation element;\n"
        "- \"turn\": the turn id where it is found (the proposal turn's id for the turn window);\n"
        "- \"quote\": a verbatim excerpt from that turn's operator or reply text, at least 5 words, "
        "copied exactly (it will be checked by string match);\n"
        "- \"who\": \"operator\" or \"reply\".\n\n"
        "For a void, give \"element\": null and a one-line \"why\".\n\n"
        "Reply with strict JSON only, an array with one object per pattern:\n"
        "[{\"pattern\": \"family/name\",\n"
        "  \"turn\":  {\"I1\": {..}, \"I2\": {..}, \"G\": {..}, \"B\": {..}},\n"
        "  \"chain\": {\"I1\": {..}, \"I2\": {..}, \"G\": {..}, \"B\": {..}}}]")


def check_map(texts, allowed_ids, m):
    """ma-occurrence's check-map, ported."""
    if not isinstance(m, dict):
        return {"element": None, "why": "map-missing", "attempt": m}
    element = m.get("element")
    turn = str(m.get("turn"))
    quote = re.sub(r"\s+", " ", str(m.get("quote") or "")).strip()
    who = m.get("who")
    if element is None or element == "null":
        return {"element": None, "why": "void", "attempt": m}
    if who not in ("operator", "reply"):
        return {"element": None, "why": "who-not-operator-or-reply", "attempt": m}
    if turn not in allowed_ids:
        return {"element": None, "why": "record-outside-window", "attempt": m}
    if len(quote.split()) < MIN_QUOTE_WORDS:
        return {"element": None, "why": "quote-too-short", "attempt": m}
    text = texts.get(turn, {}).get(who)
    if isinstance(text, str) and quote in re.sub(r"\s+", " ", text):
        return {"element": element, "turn": turn, "quote": quote, "who": who}
    return {"element": None, "why": "quote-not-found", "attempt": m}


def score_turn(window, record_id, patterns):
    """Map each pattern (both windows); returns {pid: row} with chain_mapped,
    status, and the maps (for the operator-side share)."""
    texts = {rid: {"operator": re.sub(r"\s+", " ", str(r.get("source_text") or "")),
                   "reply": re.sub(r"\s+", " ", str(r.get("reply_text") or ""))}
             for rid, r in window}
    window_ids = {rid for rid, _ in window}
    reply = whistle_ma(ma_prompt(window, record_id, patterns))
    rows = {}
    for p in patterns:
        rows[p["id"]] = {"pattern": p["id"],
                         "turn": {r: {"element": None, "why": "no reader"}
                                  for r in ROLE_NAMES},
                         "chain": {r: {"element": None, "why": "no reader"}
                                   for r in ROLE_NAMES},
                         "chain_mapped": 0, "status": "no-reader"}
    if not isinstance(reply, str):
        return rows
    lo, hi = reply.find("["), reply.rfind("]")
    if lo < 0 or hi <= lo:
        return rows
    try:
        parsed = json.loads(reply[lo:hi + 1])
    except ValueError:
        return rows
    by_pattern = {}
    for r in parsed:
        if isinstance(r, dict) and r.get("pattern"):
            by_pattern[str(r["pattern"]).lstrip("~")] = r
    for p in patterns:
        row = by_pattern.get(p["id"], {})
        turn_m = {r: check_map(texts, {record_id}, (row.get("turn") or {}).get(r))
                  for r in ROLE_NAMES}
        chain_m = {r: check_map(texts, window_ids, (row.get("chain") or {}).get(r))
                   for r in ROLE_NAMES}
        n = sum(1 for v in chain_m.values() if v.get("element"))
        rows[p["id"]] = {
            "pattern": p["id"], "turn": turn_m, "chain": chain_m,
            "chain_mapped": n,
            "status": "occurrence" if n == 4 else ("partial" if n else "void")}
    return rows


# ---------------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--store", default=STORE_DEFAULT)
    ap.add_argument("--out", required=True)
    args = ap.parse_args()

    turns = population(args.store)
    graph = json.load(open(GRAPH))
    results = {"turns": [], "store": args.store, "n_turns": len(turns)}

    for rid, rec in turns:
        t0 = time.time()
        all_recs = session_operator_records(args.store, rec.get("session_id"))
        window = chain_window(all_recs, rid)
        e_ids = ranked_ids(rec)
        seeds = seed_patterns_earlier(all_recs, rid)
        neighbours = graph_neighbours(graph, set(seeds))
        pool = []
        for pid in embedding_top20(str(rec.get("source_text") or "")) + neighbours:
            if pid not in pool:
                pool.append(pid)
        t_h = time.time()
        picks, dropped = haiku_picks(pool, str(rec.get("source_text") or ""))
        haiku_s = time.time() - t_h
        l_ids = [pid for pid, _ in picks]
        union = []
        for pid in e_ids + l_ids:
            if pid not in union:
                union.append(pid)
        patterns = []
        missing = []
        for pid in union:
            try:
                pat = osq.read_pattern(pid)
            except (FileNotFoundError, OSError):
                missing.append(pid)
                continue
            if not pat.get("parts", {}).get("IF"):
                # the file exists but has no readable IF: keep it, the 馬
                # prompt will show MISSING and its maps void with a why.
                pass
            patterns.append(pat)
        t_m = time.time()
        rows = score_turn(window, rid, patterns)
        ma_s = time.time() - t_m
        results["turns"].append({
            "id": rid, "at": rec.get("created_at"),
            "E": e_ids, "L": l_ids, "L_quotes": dict(picks),
            "L_dropped_by_quote": dropped,
            "pool_n": len(pool), "neighbours": neighbours,
            "seeds": sorted(seeds), "missing_patterns": missing,
            "rows": rows,
            "haiku_s": round(haiku_s, 1), "ma_s": round(ma_s, 1),
            "total_s": round(time.time() - t0, 1)})
        with open(args.out, "w") as h:
            json.dump(results, h, indent=1, ensure_ascii=False)
        print(rid, "E", len(e_ids), "L", len(l_ids), "dropped", dropped,
              "haiku %.0fs ma %.0fs" % (haiku_s, ma_s), flush=True)

    # per-arm summary
    summary = {}
    for arm, key in (("E", "E"), ("L", "L")):
        picks = void = part = occ = maps = 0
        op_maps = 0
        dropped = 0
        for t in results["turns"]:
            dropped += t["L_dropped_by_quote"] if arm == "L" else 0
            for pid in t[key]:
                row = t["rows"].get(pid)
                if row is None:
                    continue
                picks += 1
                void += row["status"] == "void"
                part += row["status"] == "partial"
                occ += row["status"] == "occurrence"
                maps += row["chain_mapped"]
                op_maps += sum(1 for v in row["chain"].values()
                               if v.get("element") and v.get("who") == "operator")
        summary[arm] = {"picks": picks, "void": void, "partial": part,
                        "occurrence": occ,
                        "mean_chain_mapped": (maps / picks) if picks else None,
                        "operator_share": (op_maps / maps) if maps else None,
                        "dropped_by_quote": dropped if arm == "L" else 0,
                        "chain_mapped_ge_2": sum(
                            1 for t in results["turns"] for pid in t[key]
                            if (t["rows"].get(pid) or {}).get("chain_mapped", 0) >= 2)}
    results["summary"] = summary
    results["haiku_calls"] = len(turns)
    results["haiku_mean_s"] = (sum(t["haiku_s"] for t in results["turns"])
                               / max(len(turns), 1))
    with open(args.out, "w") as h:
        json.dump(results, h, indent=1, ensure_ascii=False)
    print(json.dumps(summary, indent=1))


if __name__ == "__main__":
    main()
