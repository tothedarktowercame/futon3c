#!/usr/bin/env python3
"""驴 situation sidecar: build + resident near/add for the fast read half.

The write half (futon3c.xiang.lu-memory) stores each scored verdict row as
situation evidence + a :lu/occurrence hyperedge in the minus40 store on
:7093.  This script is the fast half's data plane:

  build      read every :lu/occurrence edge + its situation evidence from
             :7093, embed each situation text with MiniLM, write the sidecar
  --resident start a resident process: model + sidecar loaded once, then
             one JSON line per op on stdin, one JSON reply per line

Sidecar layout (embed_facts.py's, under a lu-situations/ subdirectory so a
second embedder model would be a sibling):

  ~/code/data/minus40/embeddings/<model-slug>/lu-situations/ids.json
                                                        vectors.npy
                                                        situations.json
                                                        meta.json

Run under the futon3a virtualenv (sentence-transformers lives only there):
  /home/joe/code/futon3a/.venv/bin/python -I scripts/lu_situations.py build

selftest (system python3, fake embedder, temp dir; no model, no store):
  python3 -I scripts/lu_situations.py selftest
"""

from __future__ import annotations

import argparse
import datetime
import json
import os
import shutil
import sys
import tempfile
import threading
import urllib.parse
import urllib.request

import numpy as np

EMBED_ROOT = os.path.expanduser("~/code/data/minus40/embeddings")
DEFAULT_MODEL = "sentence-transformers/all-MiniLM-L6-v2"
BASE = os.environ.get("MINUS40_BASE", "http://127.0.0.1:7093")
AUTHOR = "lu-memory"

_write_lock = threading.Lock()


def slug(model: str) -> str:
    return model.replace("/", "_")


def sidecar_dir(model: str) -> str:
    return os.path.join(EMBED_ROOT, slug(model), "lu-situations")


# ---------------------------------------------------------------- store reads

def _get_json(path: str):
    req = urllib.request.Request(BASE + path, headers={"Accept": "application/json"})
    with urllib.request.urlopen(req, timeout=60) as resp:
        return json.load(resp)


def fetch_edges():
    """Every :lu/occurrence hyperedge, following the page cursor."""
    edges, cursor = [], {}
    while True:
        q = {"type": "lu/occurrence", "limit": "1000",
             "include-total": "false", **cursor}
        page = _get_json("/api/alpha/hyperedges?" + urllib.parse.urlencode(q))
        edges += page.get("hyperedges", [])
        c = page.get("next-cursor")
        if not c:
            return edges
        cursor = {"cursor-at": c["at"], "cursor-id": c["id"]}


def fetch_evidence():
    """{evidence-id: entry} for every lu-memory evidence row."""
    out, cursor = {}, {}
    while True:
        q = {"author": AUTHOR, "limit": "1000", **cursor}
        page = _get_json("/api/alpha/evidence?" + urllib.parse.urlencode(q))
        for e in page.get("entries", []):
            out[e.get("evidence/id")] = e
        c = page.get("next-cursor")
        if not c:
            return out
        cursor = {"cursor-at": c["at"], "cursor-id": c["id"]}


# ---------------------------------------------------------------- the embedder

class Model:
    """MiniLM behind one call. The fake selftest embedder hashes instead."""

    def __init__(self, model_name: str, fake: bool = False):
        self.model_name, self.fake, self.dim = model_name, fake, 384
        if not fake:
            from sentence_transformers import SentenceTransformer
            self.model = SentenceTransformer(model_name)

    def encode(self, texts):
        if self.fake:
            rng = np.random.default_rng(abs(hash(tuple(texts))) % (2 ** 32))
            vecs = rng.standard_normal((len(texts), self.dim))
        else:
            vecs = np.asarray(self.model.encode(texts, batch_size=32))
        vecs = vecs.astype(np.float32)
        norms = np.linalg.norm(vecs, axis=1, keepdims=True)
        norms[norms == 0] = 1.0
        return vecs / norms


# ------------------------------------------------------------------- sidecar

def load_sidecar(directory):
    ids_path = os.path.join(directory, "ids.json")
    vec_path = os.path.join(directory, "vectors.npy")
    if not (os.path.exists(ids_path) and os.path.exists(vec_path)):
        return None
    with open(ids_path) as handle:
        ids = json.load(handle)
    with open(os.path.join(directory, "situations.json")) as handle:
        situations = json.load(handle)
    return {"ids": ids, "vectors": np.load(vec_path), "situations": situations}


def _atomic_write(directory, ids, vectors, situations, model):
    os.makedirs(directory, exist_ok=True)
    tmps = [("ids.json", json.dumps(ids)),
            ("vectors.npy", None),
            ("situations.json", json.dumps(situations)),
            ("meta.json", json.dumps({
                "model": model, "dim": int(vectors.shape[1]),
                "built_at": datetime.datetime.now(
                    datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
                "situations": len(ids)}))]
    tmp_paths = []
    for name, payload in tmps:
        tmp = os.path.join(directory, name + ".tmp")
        tmp_paths.append((tmp, os.path.join(directory, name)))
        if payload is None:
            with open(tmp, "wb") as handle:
                np.save(handle, vectors)
        else:
            with open(tmp, "w") as handle:
                handle.write(payload)
    for tmp, final in tmp_paths:
        os.replace(tmp, final)


def build(model_name: str):
    model = Model(model_name)
    directory = sidecar_dir(model_name)
    edges = fetch_edges()
    evidence = fetch_evidence()
    ids, texts, situations = [], [], []
    skipped = 0
    for hx in edges:
        props = hx.get("hx/props", {})
        roles = props.get("roles", {})
        eid = roles.get("situation") or (hx.get("hx/endpoints") or [None])[0]
        entry = evidence.get(eid)
        if entry is None:
            skipped += 1
            continue
        body = entry.get("evidence/body", {})
        if isinstance(body, str):
            try:
                body = json.loads(body)
            except ValueError:
                body = {}
        ids.append(eid)
        texts.append(str(body.get("text", "")))
        situations.append({
            "id": eid,
            "record": body.get("record"),
            "session": body.get("session"),
            "pattern": roles.get("pattern"),
            "status": props.get("status"),
            "chain_mapped": props.get("chain-mapped",
                                      props.get("chain_mapped", 0)),
            "sources": props.get("sources", []),
        })
    vectors = model.encode(texts) if texts else np.zeros((0, model.dim), np.float32)
    _atomic_write(directory, ids, vectors, situations, model_name)
    print(json.dumps({"ok": True, "situations": len(ids),
                      "edges": len(edges), "skipped-no-evidence": skipped}))


# ---------------------------------------------------------------- the resident

def resident(model_name: str):
    directory = sidecar_dir(model_name)
    state = load_sidecar(directory)
    if state is None:
        print(json.dumps({"resident": "error",
                          "error": "no sidecar at %s; run build first" % directory}),
              flush=True)
        sys.exit(2)
    model = Model(model_name)
    print(json.dumps({"resident": "ready", "situations": len(state["ids"])}),
          flush=True)
    for line in sys.stdin:
        line = line.strip()
        if not line:
            continue
        try:
            op = json.loads(line)
        except ValueError as exc:
            print(json.dumps({"ok": False, "error": "bad json: %s" % exc}), flush=True)
            continue
        try:
            if op.get("op") == "near":
                reply = _near(state, model, op)
            elif op.get("op") == "add":
                with _write_lock:
                    reply = _add(state, model, directory, op, model_name)
            elif op.get("op") == "ping":
                reply = {"ok": True, "situations": len(state["ids"])}
            else:
                reply = {"ok": False, "error": "unknown op %r" % op.get("op")}
        except Exception as exc:  # noqa: BLE001 -- one bad op must not kill the loop
            reply = {"ok": False, "error": "%s: %s" % (type(exc).__name__, exc)}
        print(json.dumps(reply), flush=True)


def _near(state, model, op):
    text = str(op.get("text", ""))
    k = int(op.get("k", 10))
    min_cos = float(op.get("min-cos", 0.35))
    exclude = set(op.get("exclude-records", []))
    if state["vectors"].shape[0] == 0:
        return {"ok": True, "neighbours": []}
    q = model.encode([text])[0]
    sims = state["vectors"] @ q
    order = np.argsort(-sims)
    out = []
    for i in order:
        if sims[i] < min_cos or len(out) >= k:
            break
        s = state["situations"][int(i)]
        if s.get("record") in exclude:
            continue
        out.append({"id": s["id"], "cos": round(float(sims[i]), 4),
                    "record": s.get("record"), "session": s.get("session"),
                    "pattern": s.get("pattern"), "status": s.get("status"),
                    "chain_mapped": s.get("chain_mapped", 0)})
    return {"ok": True, "neighbours": out, "indexed": len(state["ids"])}


def _add(state, model, directory, op, model_name):
    sid, text = str(op.get("id")), str(op.get("text"))
    props = op.get("props", {})
    if sid in state["ids"]:
        return {"ok": True, "unchanged": True}
    vec = model.encode([text])
    state["ids"] = state["ids"] + [sid]
    state["vectors"] = np.vstack([state["vectors"], vec[0]])
    state["situations"] = state["situations"] + [{
        "id": sid, "record": props.get("record"), "session": props.get("session"),
        "pattern": props.get("pattern"), "status": props.get("status"),
        "chain_mapped": props.get("chain_mapped", 0), "sources": props.get("sources", [])}]
    _atomic_write(directory, state["ids"], state["vectors"],
                  state["situations"], model_name)
    return {"ok": True, "situations": len(state["ids"])}


# ------------------------------------------------------------------ selftest

def selftest():
    """Fake embedder, temp sidecar: near/exclusion, threshold, add one row."""
    global EMBED_ROOT
    model = Model(DEFAULT_MODEL, fake=True)
    with tempfile.TemporaryDirectory() as tmp:
        os.makedirs(tmp, exist_ok=True)
        situations = [
            {"id": "s1", "record": "r1", "session": "se1", "pattern": "p/a",
             "status": "occurrence", "chain_mapped": 4, "sources": ["embedding"]},
            {"id": "s2", "record": "r2", "session": "se2", "pattern": "p/a",
             "status": "void", "chain_mapped": 0, "sources": []},
            {"id": "s3", "record": "r3", "session": "se3", "pattern": "p/b",
             "status": "partial", "chain_mapped": 2, "sources": ["驴"]},
            {"id": "s4", "record": "r4", "session": "se4", "pattern": "p/c",
             "status": "occurrence", "chain_mapped": 4, "sources": []}]
        texts = ["t1", "t2", "t3", "t4"]
        vectors = model.encode(texts)
        _atomic_write(tmp, [s["id"] for s in situations], vectors,
                      situations, "fake-model")
        state = load_sidecar(tmp)
        assert state is not None and len(state["ids"]) == 4
        # every row is its own query's nearest (the fake embedder is
        # deterministic per text tuple); exclusion removes s1's row
        reply = _near(state, model, {"text": "t1", "k": 10, "min-cos": -1.0,
                                     "exclude-records": ["r1"]})
        assert all(n["record"] != "r1" for n in reply["neighbours"]), reply
        reply_self = _near(state, model, {"text": "t1", "k": 10,
                                          "min-cos": -1.0, "exclude-records": []})
        assert any(n["record"] == "r1" for n in reply_self["neighbours"])
        # a threshold above every similarity returns nothing
        assert _near(state, model, {"text": "t1", "k": 10,
                                    "min-cos": 0.999999})["neighbours"] == []
        # add appends exactly one row and persists it
        before = state["vectors"].shape[0]
        _add(state, model, tmp, {"id": "s5", "text": "t5",
                                 "props": {"record": "r5", "pattern": "p/d",
                                           "status": "partial"}}, "fake-model")
        assert state["vectors"].shape[0] == before + 1
        reloaded = load_sidecar(tmp)
        assert reloaded["ids"][-1] == "s5" and len(reloaded["ids"]) == 5
        assert _add(state, model, tmp, {"id": "s5", "text": "t5"},
                    "fake-model").get("unchanged") is True
    print(json.dumps({"ok": True, "selftest": "passed"}))


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--model", default=DEFAULT_MODEL)
    parser.add_argument("--resident", action="store_true")
    parser.add_argument("--base", default=None)
    parser.add_argument("--selftest", action="store_true")
    parser.add_argument("--build", action="store_true")
    args = parser.parse_args(argv)
    if args.base:
        global BASE
        BASE = args.base
    if args.selftest:
        selftest()
    elif args.build:
        build(args.model)
    elif args.resident:
        resident(args.model)
    else:
        parser.error("one of --build, --selftest or --resident is required")


if __name__ == "__main__":
    main()
