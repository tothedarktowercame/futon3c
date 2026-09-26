#!/usr/bin/env python3
"""memory_shape.py — a shape reading of the memory store (Analyst duties B/C/D).

Companion to transfer_checks.bb. Where that script follows ONE memory's journey
(recorded -> reachable -> used -> measured), this one reads the WHOLE store's
structure, per M-apm-demonstration W.67 and analyst-v1 duties B (sharing,
components), C (used x route), D (bespoke-rate, duplicates), and the generality
/ proof-text hygiene introduced by codex-scribe-v1 / zai-scribe-v1.

Usage:  python3 memory_shape.py [--write PATH] [--since YYYY-MM-DD] [--json]

Reads only. Universe is by DECLARED id (the @flexiarg/@arg/@multiarg lines in
futon3/library/math-*), never by directory listing (analyst-v1 caution). Every
count states its denominator. A count that equals its request limit is printed
as a CEILING, not a total.
"""
import argparse, collections, glob, json, os, re, subprocess, sys, urllib.parse, urllib.request

SUBSTRATE = os.environ.get("FUTON1B_BASE", "http://127.0.0.1:7073")
LIBRARY = os.environ.get("FUTON3_LIBRARY", os.path.expanduser("~/code/futon3/library"))
CAMPAIGNS = os.environ.get("APM_CAMPAIGNS", os.path.expanduser("~/code/futon3c/data/apm-campaigns"))
APM_LEAN = os.environ.get("APM_LEAN", os.path.expanduser("~/code/apm-lean"))
PROBLEM_ID = re.compile(r"\b[a-z]\d{2}[A-Z]\d{2}\b")
APM_IDENT = re.compile(r"apm_[a-z]\d{2}[A-Z]\d{2}")
FILE_LINE = re.compile(r"Main\.lean:\d+")
DECL = re.compile(r"@(?:flexi|multi)?arg\s+([A-Za-z0-9_./-]+)")


def get(path, timeout=120):
    req = urllib.request.Request(SUBSTRATE + path, headers={"Accept": "application/json"})
    return json.load(urllib.request.urlopen(req, timeout=timeout))


def declared_universe():
    ids = set()
    for f in glob.glob(os.path.join(LIBRARY, "math-*", "*.flexiarg")):
        for m in DECL.finditer(open(f, encoding="utf-8", errors="replace").read()):
            if m.group(1).startswith("math"):
                ids.add(m.group(1))
    return ids


def fetch_patterns():
    d = get("/api/alpha/entities?type=pattern/library&limit=5000")
    ents = {e["entity/id"]: e for e in d.get("entities", [])}
    return ents, len(ents) >= 5000


def fetch_memories(limit=1000):
    d = get(f"/api/alpha/evidence?type=memory&limit={limit}")
    ents = d.get("entries", [])
    return ents, len(ents) >= limit


def fetch_edges():
    edges, after = [], None
    for _ in range(50):
        q = "/api/alpha/hyperedges?type=memory/assert&limit=1000" + (f"&after={urllib.parse.quote(after)}" if after else "")
        es = get(q).get("hyperedges", [])
        if not es:
            break
        edges += es
        after = es[-1].get("hx/id")
        if len(es) < 1000:
            break
    return edges


def body_of(m):
    return m.get("evidence/body") or {}


def text_of(m):
    b = body_of(m)
    return (str(b.get("name", "")) + "\n" + str(b.get("hook", "")) + "\n" + str(b.get("body", "")))


def components(adj):
    seen, comps = set(), []
    for start in adj:
        if start in seen:
            continue
        stack, comp = [start], set()
        while stack:
            n = stack.pop()
            if n in seen:
                continue
            seen.add(n); comp.add(n); stack.extend(adj.get(n, ()))
        comps.append(comp)
    return comps


def use_receipts():
    """Per memory: how often surfaced/used, which frames used it, and which
    PROBLEMS used it (the frame's :receipt/problem-id). Reuse is judged on
    problems, not frames: a memory used on the problem it was mined from is
    transfer, not reuse."""
    surfaced, used = collections.Counter(), collections.Counter()
    frames, problems = collections.defaultdict(set), collections.defaultdict(set)
    for f in glob.glob(os.path.join(CAMPAIGNS, "*", "*", "live", "student-attempt-*.edn")):
        frame = os.path.basename(os.path.dirname(os.path.dirname(f))).rsplit("-", 1)[-1]
        s = open(f, encoding="utf-8", errors="replace").read()
        mp = re.search(r':receipt/problem-id "([^"]+)"', s)
        problem = mp.group(1) if mp else None
        mu, ms = re.search(r":used-ids \[([^\]]*)\]", s), re.search(r":surfaced-ids \[([^\]]*)\]", s)
        for i in re.findall(r'"(e-[^"]+)"', mu.group(1) if mu else ""):
            used[i] += 1; frames[i].add(frame)
            if problem: problems[i].add(problem)
        for i in re.findall(r'"(e-[^"]+)"', ms.group(1) if ms else ""):
            surfaced[i] += 1
    return surfaced, used, frames, problems


def witness_commit(b):
    """The apm-lean commit a memory cites as its witness, wherever it sits in
    the body tree (codex-pilot memories nest it under :body :witness :commit)."""
    stack = [b]
    while stack:
        node = stack.pop()
        if isinstance(node, dict):
            c = node.get("commit") or node.get(":commit")
            if isinstance(c, str) and re.fullmatch(r"[0-9a-f]{7,40}", c):
                return c
            stack.extend(node.values())
        elif isinstance(node, list):
            stack.extend(node)
    return None


def problem_of_commit(sha, cache={}):
    """Resolve a witness commit to the problem it touched, via apm-lean."""
    if sha not in cache:
        cache[sha] = None
        try:
            out = subprocess.run(["git", "show", "--stat", "--format=", sha],
                                 cwd=APM_LEAN, capture_output=True, text=True,
                                 timeout=20).stdout
            hits = {m.group(1) for m in re.finditer(r"problems/([a-z]\d{2}[A-Z]\d{2})/", out)}
            if len(hits) == 1:
                cache[sha] = hits.pop()
        except Exception:
            pass
    return cache[sha]


def mined_from(m):
    """The problem a memory was mined from: its :evidence/subject when that is a
    problem ref, else the first problem id in its name/hook/body (the f29/f30
    guide memories name it), else the problem touched by the commit it cites as
    its witness, else None.

    The witness fallback matters: memories deposited by the codex pilot carry a
    MISSION subject (M-codex-sorry-loop), not a problem ref, and name no problem
    id anywhere -- exactly the hygiene the scribe cards ask for. Without this
    they resolve to unknown origin, and a use of one cannot be counted as
    cross-problem reuse even when it plainly is (f33 attempt-1 used
    e-codexpilot-force-a-sublinear-... on a94A07; its witness commit 662b9ec is
    a94J08). That is how the campaign reuse count read 0 while standing at 1."""
    subj = m.get("evidence/subject") or m.get(":evidence/subject") or {}
    if isinstance(subj, dict):
        rid = subj.get("ref/id") or subj.get(":ref/id")
        if rid and PROBLEM_ID.fullmatch(str(rid)):
            return str(rid)
    b = body_of(m)
    hit = PROBLEM_ID.search(" ".join(str(b.get(k, "")) for k in ("name", "hook", "body")))
    if hit:
        return hit.group(0)
    sha = witness_commit(b)
    return problem_of_commit(sha) if sha else None


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--write"); ap.add_argument("--since", default="2026-08-10"); ap.add_argument("--json", action="store_true")
    a = ap.parse_args()

    declared = declared_universe()
    patterns, pat_ceiling = fetch_patterns()
    memories, mem_ceiling = fetch_memories()
    edges = fetch_edges()

    math_patterns = {p for p in patterns if p.split("/")[0].startswith("math")}
    R = {"substrate": SUBSTRATE, "since": a.since}
    R["universe"] = {
        "declared-math-patterns": len(declared),
        "substrate-math-patterns": len(math_patterns),
        "declared-but-absent-from-substrate": sorted(declared - math_patterns)[:20],
        "in-substrate-but-undeclared": sorted(math_patterns - declared)[:20],
        "all-substrate-patterns": len(patterns), "pattern-count-is-ceiling": pat_ceiling,
        "memories": len(memories), "memory-count-is-ceiling": mem_ceiling,
        "memory/assert-edges": len(edges),
    }

    # --- attachment map: memory -> {patterns}, status, problems
    mem_pats, mem_status, pat_mems, pat_problems, mem_problem = {}, {}, collections.defaultdict(set), collections.defaultdict(set), {}
    for e in edges:
        p = e.get("hx/props") or {}
        roles = p.get("roles") or {}
        m = roles.get("entry"); ps = roles.get("patterns") or []
        if not m:
            continue
        mem_pats.setdefault(m, set()).update(ps)
        mem_status[m] = str(p.get("attachment-status") or e.get("prop/attachment-status"))
        probs = {x for x in (e.get("hx/endpoints") or []) if PROBLEM_ID.fullmatch(str(x))}
        if probs:
            mem_problem[m] = sorted(probs)[0]
        for x in ps:
            pat_mems[x].add(m); pat_problems[x].update(probs)

    mem_by_id = {m.get("evidence/id"): m for m in memories}
    recent = [m for m in memories if str(m.get("evidence/at", ""))[:10] >= a.since]
    R["attachment"] = {
        "memories-with-edge": len(mem_pats),
        "status": dict(collections.Counter(mem_status.values())),
        "memories-since-with-edge": sum(1 for m in recent if m.get("evidence/id") in mem_pats),
        "memories-since-total": len(recent),
    }

    # --- sharing / bespoke / components (reviewed graph and all-edge graph)
    def graph_stats(status_filter):
        pm = {p: {m for m in ms if (status_filter is None or mem_status.get(m) == status_filter)} for p, ms in pat_mems.items()}
        pm = {p: ms for p, ms in pm.items() if ms}
        mp = collections.defaultdict(set)
        for p, ms in pm.items():
            for m in ms: mp[m].add(p)
        adj = collections.defaultdict(set)
        for p, ms in pm.items():
            for m in ms: adj["P:" + p].add("M:" + m); adj["M:" + m].add("P:" + p)
        comps = components(adj)
        per_pat = sorted((len(ms) for ms in pm.values()), reverse=True)
        cross = {p: sorted(pat_problems[p]) for p in pm if len(pat_problems[p]) >= 2}
        return {
            "patterns-with-memories": len(pm), "memories-attached": len(mp),
            "patterns-with-exactly-1-memory (bespoke)": sum(1 for n in per_pat if n == 1),
            "patterns-with->=2 (sharing count)": sum(1 for n in per_pat if n >= 2),
            "memories-on->=2-patterns (multi-attachment)": sum(1 for ps in mp.values() if len(ps) >= 2),
            "top-patterns": sorted(((len(ms), p) for p, ms in pm.items()), reverse=True)[:6],
            "components": len(comps), "largest-component-nodes": max((len(c) for c in comps), default=0),
            "patterns-spanning->=2-problems": len(cross), "spanning-examples": dict(list(cross.items())[:6]),
        }
    R["graph-all-edges"] = graph_stats(None)
    R["graph-reviewed-only"] = graph_stats("reviewed")

    # --- hygiene: proof-text and generality (codex/zai-scribe-v1 limits)
    def hyg(ms):
        out = collections.Counter()
        for m in ms:
            t = text_of(m); b = json.dumps(body_of(m))
            out["n"] += 1
            if b.count(":= by") > 3: out["proof-text (>3 ':= by')"] += 1
            if len(json.dumps(body_of(m).get("body", ""))) > 4096: out["body>4KB"] += 1
            if PROBLEM_ID.search(t): out["problem-id-in-name/hook/body"] += 1
            if APM_IDENT.search(t): out["apm_-identifier"] += 1
            if FILE_LINE.search(t): out["Main.lean:N"] += 1
            if PROBLEM_ID.match(str(body_of(m).get("name", ""))) or PROBLEM_ID.match(str(body_of(m).get("hook", ""))): out["hook-starts-with-problem-id"] += 1
            if m.get("evidence/id") not in mem_pats: out["no-pattern-attachment"] += 1
        return dict(out)
    R["hygiene-all"] = hyg(memories)
    R["hygiene-since"] = hyg(recent)
    by_author = collections.defaultdict(list)
    for m in recent: by_author[str(m.get("evidence/author"))].append(m)
    R["hygiene-since-by-author"] = {k: hyg(v) for k, v in sorted(by_author.items()) if len(v) >= 3}

    # --- use receipts (duty C: used x route precondition)
    surfaced, used, frames, problems = use_receipts()
    proof_text_used = [i for i in used if i in mem_by_id and json.dumps(body_of(mem_by_id[i])).count(":= by") > 3]
    # Reuse = used on a problem other than the one it was mined from. "Used in
    # >=2 frames" was the wrong metric: two frames on the same problem (or a
    # re-run) would count, and one frame per problem never can.
    origin = {i: (mined_from(mem_by_id[i]) if i in mem_by_id else None) for i in used}
    reused = {i: sorted(ps - {origin[i]}) for i, ps in problems.items() if origin[i] and (ps - {origin[i]})}
    R["use"] = {
        "distinct-surfaced": len(surfaced), "distinct-used": len(used),
        "used-on-problem-!=-mined-from": reused,
        "used-only-on-mined-from-problem": sorted(i for i, ps in problems.items() if origin[i] and ps <= {origin[i]}),
        "used-with-unknown-origin": sorted(i for i in used if not origin[i]),
        "used-in->=2-frames": [i for i, fs in frames.items() if len(fs) >= 2],
        "used-memories-that-are-proof-text": proof_text_used,
        "used-but-unattached": [i for i in used if i not in mem_pats],
    }

    if a.json or not a.write:
        print(json.dumps(R, indent=1, default=list))
    if a.write:
        with open(a.write, "w") as f: json.dump(R, f, indent=1, default=list)
        print("wrote", a.write, file=sys.stderr)


if __name__ == "__main__":
    main()
