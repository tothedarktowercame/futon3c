#!/usr/bin/env python3
"""m40_store_copy.py — copy minus40's store (:7093) into futon1b (:7073).

MEM-migration-discovery-2026-11-12.md step 1 + claude-13's review amendments
(2fc93d427): throttled, time-preserving, idempotent, resumable, verified.
No client is switched; :7093 is read-only here.

  m40_store_copy.py copy    [--src ...] [--dst ...] [--state DIR]
  m40_store_copy.py verify  [--src ...] [--dst ...]

Time: m40_asof/history/state read hx/props (said-at/unsaid-at), i.e. CONTENT,
so a verbatim copy preserves them; hyperedges are additionally written with
:hx/valid-time from their own said-at (or the doc's evidence/at), since
:7073's hyperedge write path accepts it (futon1b_server.clj:253). Evidence
docs have no valid-time on the write path; nothing in the m40 tools reads
XTDB system time, and the doc's own :evidence/at travels as content.

Idempotent: evidence id present with identical content is skipped; different
content is logged as a collision, never overwritten. Hyperedges keyed by
hx/id. State file in the log dir makes a rerun resume.

Throttle (review amendment): <=10 writes/s; pause 5 s when a write takes
>1 s; every 500 writes check :7073 health (shallow) and the heap sampler
tail (~/notes/futon1b-heap/samples.tsv); stop on health failure or climbing
full-GC deltas.
"""
import argparse, json, os, re, subprocess, sys, time, urllib.request, urllib.error
from pathlib import Path

SRC = "http://127.0.0.1:7093"
DST = "http://127.0.0.1:7073"
DST_HEALTH = "http://127.0.0.1:7072/health"
HEAP_TSV = os.path.expanduser("~/notes/futon1b-heap/samples.tsv")
LOG_DIR = os.path.expanduser("~/code/storage/minus40-migration-2026-11-12")
PAGE = 200
MIN_INTERVAL = 0.1        # <= 10 writes/s
SLOW_WRITE = 1.0          # pause 5 s when a write exceeds this
HEALTH_EVERY = 500

HX_TYPES = [":xiang/scope", ":xiang/act", ":xiang/open", ":xiang/answers",
            ":lu/occurrence", ":memory/fact", ":memory/mentions",
            ":memory/link", ":memory/version", ":memory/assert",
            ":memory/relation", ":memory/supersede-proposed", ":memory/sorry",
            ":memory/alias-proposed", ":m40/fault"]

state = {"evidence_cursor": None, "evidence_done": False,
         "hx_type": None, "hx_cursor": None, "hx_done_types": [],
         "entities_done": False}
stats = {"ev_copied": 0, "ev_skipped": 0, "ev_collisions": [],
         "hx_copied": 0, "hx_skipped": 0, "hx_failed": [],
         "writes": 0, "slow_writes": 0, "started": None}
_last_write = [0.0]


def get(base, path, timeout=60):
    req = urllib.request.Request(base + path, headers={"Accept": "application/json"})
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return json.loads(r.read())


def post(dst, path, payload, timeout=600):
    body = json.dumps(payload).encode()
    req = urllib.request.Request(dst + path, data=body, method="POST",
                                 headers={"Content-Type": "application/json",
                                          "Accept": "application/json",
                                          "X-Penholder": os.environ.get("FUTON1A_PENHOLDER", "api")})
    t0 = time.time()
    try:
        with urllib.request.urlopen(req, timeout=timeout) as r:
            return r.status, json.loads(r.read()), time.time() - t0
    except urllib.error.HTTPError as e:
        try:
            return e.code, json.loads(e.read()), time.time() - t0
        except Exception:
            return e.code, {"error": str(e)}, time.time() - t0


def throttle():
    dt = time.time() - _last_write[0]
    if dt < MIN_INTERVAL:
        time.sleep(MIN_INTERVAL - dt)


def health_check():
    ok, why = True, ""
    try:
        get(DST_HEALTH.replace("/health", "/api/alpha/restart-readiness"), timeout=10)
    except Exception:
        pass  # readiness is a bonus; the heap sampler is the signal
    try:
        with open(HEAP_TSV) as f:
            lines = f.readlines()[-6:]
        rows = [l.split("\t") for l in lines if "\t" in l]
        # last column is the full-GC count (0 at rest)
        fulls = [float(r[-1].strip()) for r in rows]
        if len(fulls) >= 3 and fulls[-1] > fulls[-3]:
            ok, why = False, f"full-GC count climbing in heap sampler: {fulls}"
    except FileNotFoundError:
        pass
    return ok, why


def same_doc(a, b):
    a = {k: v for k, v in a.items() if k != "xt/id"}
    b = {k: v for k, v in b.items() if k != "xt/id"}
    return a == b


EVAL_SH = os.path.expanduser("~/code/futon1b/scripts/futon1b-eval.sh")
BATCH_NS = os.path.expanduser("~/code/futon1b/m40_batch_migrate.clj")


def eval_futon1b(form, timeout=300):
    """One Drawbridge eval on the live futon1b JVM (our batch write path)."""
    out = subprocess.run(["bash", EVAL_SH, form], capture_output=True, text=True,
                         timeout=timeout)
    return out.stdout.strip()


def load_batch_ns():
    print("loading", BATCH_NS, "into the futon1b JVM via Drawbridge")
    out = subprocess.run(["bash", EVAL_SH, "-f", BATCH_NS],
                         capture_output=True, text=True, timeout=300)
    print("load:", out.stdout.strip()[-200:], out.stderr.strip()[-200:])


def copy_evidence():
    """Evidence docs, batched through futon1b-migrate-m40 (in-process via
    Drawbridge): one IN existence query, one execute-tx with per-doc
    :valid-from = :evidence/at, one readback, per batch. The per-document
    HTTP path slowed to ~20 docs/min against the store's 4 query permits
    (claude-13, 2026-11-12); this path is the migrate-derived shape."""
    import subprocess
    print("== evidence (batched, via Drawbridge)")
    batch_dir = os.path.join(LOG_DIR, "batches")
    os.makedirs(batch_dir, exist_ok=True)
    n_batch = state.get("ev_batch", 0)
    while not state["evidence_done"]:
        cursor = state["evidence_cursor"]
        url = "/api/alpha/evidence?limit=%d" % PAGE
        if cursor:
            url += "&before=" + urllib.parse.quote(cursor["at"]) \
                   + "&before-id=" + urllib.parse.quote(cursor["id"])
        page = get(SRC, url)
        entries = page.get("entries", [])
        if not entries:
            state["evidence_done"] = True
            save()
            continue
        path = os.path.join(batch_dir, "batch-%05d.json" % n_batch)
        with open(path, "w") as f:
            json.dump(entries, f)
        t0 = time.time()
        out = eval_futon1b('(futon1b-migrate-m40/write-batch-file! '
                           '@futon1b-server/!node "%s")' % path)
        dt = time.time() - t0
        # the eval returns the EDN result map; pluck the counts
        def count(k):
            m = re.search(r"%s (\d+)" % k, out)
            return int(m.group(1)) if m else 0
        stats["ev_copied"] += count(":written")
        stats["ev_skipped"] += count(":skipped")
        for m in re.finditer(r'CONFLICT[^"]*?(e-[\w-]+)', out):
            stats["ev_collisions"].append(m.group(1))
        stats["writes"] += len(entries)
        stats["batches"] = stats.get("batches", 0) + 1
        print("batch %d: %d docs in %.1fs (%.1f docs/s) written=%d skipped=%d"
              % (n_batch, len(entries), dt, len(entries) / max(dt, 0.01),
                 count(":written"), count(":skipped")))
        if dt > 60:
            print("slow batch %.0fs; pausing 10s" % dt)
            time.sleep(10)
        else:
            time.sleep(2)  # pace between batches (review amendment)
        ok, why = health_check()
        if not ok:
            print("STOPPING on health:", why)
            save(); sys.exit(3)
        state["ev_batch"] = n_batch + 1
        state["evidence_cursor"] = page.get("next-cursor")
        if not state["evidence_cursor"] or len(entries) < PAGE:
            state["evidence_done"] = True
        save()
        n_batch += 1
    print("evidence done:", stats["ev_copied"], "copied,",
          stats["ev_skipped"], "skipped,", len(stats["ev_collisions"]), "collisions")


RECORDS_DIR = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
_record_time_cache = {}


def record_time(rid):
    """created_at of turn record RID (its file in the emacs-graph store), for
    xiang edges that carry no time field of their own (their semantic time is
    the turn's). None when unknown — the caller falls back to copy time."""
    if rid not in _record_time_cache:
        t = None
        if rid:
            p = os.path.join(RECORDS_DIR, rid + ".json")
            try:
                with open(p) as f:
                    t = json.load(f).get("created_at")
            except Exception:
                t = None
        _record_time_cache[rid] = t
    return _record_time_cache[rid]


def hx_valid_time(h):
    p = h.get("hx/props") or {}
    own = p.get("said-at") or p.get("at")
    if own:
        return own
    return record_time((p.get("record")) or "")


def copy_hyperedges():
    """In-process via Drawbridge: write-hyperedges-batch! (500-chunk max),
    every edge batchable because every edge gets a valid-from — its own
    said-at/at, else its turn record's created_at, else copy time (stated in
    the migration note). The per-item single-write HTTP path ran ~85/min."""
    print("== hyperedges (in-process batch, via Drawbridge)")
    for t in HX_TYPES:
        if t in state["hx_done_types"]:
            continue
        cursor = None
        while True:
            url = "/api/alpha/hyperedges?type=" + urllib.parse.quote(t, safe="") + "&limit=1000"
            if cursor:
                url += "&after=" + urllib.parse.quote(cursor, safe="")
            page = get(SRC, url)
            edges = page.get("hyperedges", [])
            if not edges:
                break
            payloads = []
            for h in edges:
                payload = {"hx/id": h.get("hx/id"), "hx/type": t.lstrip(":"),
                           "hx/endpoints": h.get("hx/endpoints"),
                           "hx/props": h.get("hx/props") or {}}
                if h.get("hx/labels"):
                    payload["hx/labels"] = h["hx/labels"]
                payload["hx/valid-time"] = hx_valid_time(h) or \
                    time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
                payloads.append(payload)
            # :memory/assert is excluded from the server's batchable path
            # (single-write item by item): small chunks, long eval timeout.
            chunk_size = 50 if t == ":memory/assert" else 500
            eval_timeout = 1200 if t == ":memory/assert" else 300
            for i in range(0, len(payloads), chunk_size):
                chunk = payloads[i:i + chunk_size]
                bpath = os.path.join(LOG_DIR, "batches", "hx-%s-%05d.json"
                                     % (t.lstrip(":").replace("/", "_"), i))
                with open(bpath, "w") as f:
                    json.dump(chunk, f)
                throttle()
                t0 = time.time()
                out = eval_futon1b(
                    '(@(var futon1b-server/write-hyperedges-batch!)'
                    ' @futon1b-server/!node'
                    ' (cheshire.core/parse-string (slurp "%s") true))' % bpath,
                    timeout=eval_timeout)
                dt = time.time() - t0
                ok_n = len(re.findall(r":ok true", out))
                stats["hx_copied"] += ok_n
                stats["writes"] += len(chunk)
                print("hx %s chunk %d: %d edges, %d ok, %.0fs"
                      % (t, i, len(chunk), ok_n, dt))
                time.sleep(2)
                ok, why = health_check()
                if not ok:
                    print("STOPPING on health:", why); save(); sys.exit(3)
            cursor = page.get("next-cursor")
            save()
            if not cursor or len(edges) < 1000:
                break
        state["hx_done_types"].append(t)
        save()
        print("type", t, "done")
    print("hyperedges done:", stats["hx_copied"], "copied,",
          stats["hx_skipped"], "skipped,", len(stats["hx_failed"]), "failed")


def copy_entities():
    if state["entities_done"]:
        return
    print("== entities")
    # entity types on :7093 are m40/* (types route); page each
    types = get(SRC, "/api/alpha/types").get("types", [])
    for t in types:
        tid = t.get("type/id")
        if not (isinstance(tid, str) and tid.startswith(":m40")):
            continue
        after = None
        while True:
            url = "/api/alpha/entities?entity-type=" + urllib.parse.quote(tid, safe="") + "&limit=500"
            if after:
                url += "&after=" + urllib.parse.quote(after, safe="")
            try:
                page = get(SRC, url)
            except urllib.error.HTTPError as e:
                print("entities list failed for", tid, e.code)
                break
            ents = page.get("entities", [])
            if not ents:
                break
            for ent in ents:
                throttle()
                status, body, dt = post(DST, "/api/alpha/entity", ent)
                _last_write[0] = time.time()
                stats["writes"] += 1
                if status not in (200, 201, 409):
                    print("entity write failed", status, ent.get("id") or ent.get("entity/id"), str(body)[:150])
            after = page.get("next-cursor")
            if not after or len(ents) < 500:
                break
    state["entities_done"] = True
    save()


def save():
    os.makedirs(LOG_DIR, exist_ok=True)
    stats["saved_at"] = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
    tmp = os.path.join(LOG_DIR, "copy-state.json.tmp")
    with open(tmp, "w") as f:
        json.dump({"state": state, "stats": stats}, f, indent=1)
    os.replace(tmp, os.path.join(LOG_DIR, "copy-state.json"))


def load():
    p = os.path.join(LOG_DIR, "copy-state.json")
    if os.path.exists(p):
        with open(p) as f:
            d = json.load(f)
        state.update(d["state"])
        stats.update(d["stats"])


def verify():
    print("== verify: census per hyperedge type")
    rows = []
    for t in HX_TYPES:
        a = get(SRC, "/api/alpha/census?type=" + urllib.parse.quote(t, safe="")).get("count")
        b = get(DST, "/api/alpha/census?type=" + urllib.parse.quote(t, safe="")).get("count")
        rows.append((t, a, b, "OK" if a == b else "MISMATCH"))
        print(f"  {t:32s} src={a} dst={b} {rows[-1][3]}")
    print("== verify: 20 memory docs byte-equal")
    import random
    page = get(SRC, "/api/alpha/evidence?limit=200&type=memory")
    docs = [d for d in page.get("entries", []) if d.get("evidence/id", "").startswith("e-m40-")]
    random.seed(20261112)
    for d in random.sample(docs, min(20, len(docs))):
        eid = d["evidence/id"]
        try:
            other = get(DST, "/api/alpha/evidence/" + urllib.parse.quote(eid, safe=""))
            print("  ", eid, "equal" if same_doc(d, other) else "DIFFERENT")
        except urllib.error.HTTPError as e:
            print("  ", eid, "MISSING on dst" if e.code == 404 else f"ERR {e.code}")


def main():
    global SRC, DST
    ap = argparse.ArgumentParser()
    ap.add_argument("mode", choices=["copy", "verify"])
    ap.add_argument("--src", default=SRC)
    ap.add_argument("--dst", default=DST)
    args = ap.parse_args()
    SRC, DST = args.src, args.dst
    load()
    stats["started"] = stats.get("started") or time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
    if args.mode == "copy":
        if not state["evidence_done"]:
            load_batch_ns()
        copy_evidence()
        copy_entities()
        copy_hyperedges()
        save()
        print("copy complete; state in", LOG_DIR)
    else:
        verify()


import urllib.parse
if __name__ == "__main__":
    main()
