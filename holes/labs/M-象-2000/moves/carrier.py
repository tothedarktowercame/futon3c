#!/usr/bin/env python3
"""Carrier: load labelled move sidecars into a scratch futon1b as hyperedges.

Each ``answers`` entry in a sidecar becomes one ``:xiang/answers`` edge
[end answering-act, opener-act]; each non-null ``scope.within`` / ``scope.closes``
becomes one ``:xiang/scope`` edge [opener, governed/closing act].  Edge ids
are sha1-derived so a relabelling re-puts the same edge.  Dry run by
default; ``--apply`` POSTs each edge to the store's
``/api/alpha/hyperedge`` (200 and 409-duplicate are both success; a failed
POST is logged to carrier-failures.jsonl and never stops the run).

Usage::

    python3 -I carrier.py [--moves-glob G] [--base URL] [--apply] [--limit N]
"""

from __future__ import annotations

import argparse
import glob
import hashlib
import json
import os
import sys
import urllib.error
import urllib.request

LABEL_SET = "moves/1"
DEFAULT_GLOB = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "..", "data", "moves-scale*", "shard-*", "turn-*.moves.json")
DEFAULT_BASE = "http://127.0.0.1:7093"
FAILURES_PATH = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                             "..", "data", "carrier-failures.jsonl")


def _sha20(*parts: str) -> str:
    return hashlib.sha1("|".join(parts).encode("utf-8")).hexdigest()[:20]


def answers_edge(answering: str, opener: str, move: str, confidence: str,
                 record: str, labeller: str, shard: str) -> dict:
    """One :xiang/answers edge body for a single answers[] entry."""
    return {
        "hx/id": "hx-xiang-answers-" + _sha20(answering, opener),
        "hx/type": "xiang/answers",
        "hx/endpoints": [answering, opener],
        "hx/props": {
            "roles": {"answer": answering, "opener": opener},
            "effect": None,  # filled by the caller from the entry
            "quote": None,
            "move": move, "confidence": confidence, "record": record,
            "labeller": labeller, "status": "proposed",
            "label_set": LABEL_SET, "shard": shard,
        },
    }


def build_edges(sidecar: dict, shard: str) -> tuple[list[dict], list[str]]:
    """All edges for one sidecar, plus human-readable problem notes."""
    record = sidecar.get("record", "")
    labeller = sidecar.get("labeller", "")
    edges: list[dict] = []
    notes: list[str] = []
    for act in sidecar.get("acts", []):
        act_id = act.get("act")
        move = act.get("move", "")
        confidence = act.get("confidence")
        if not act_id:
            notes.append(f"{record}: act entry without id (move {move!r})")
            continue
        for entry in act.get("answers", []):
            opener = entry.get("act")
            if not opener:
                notes.append(f"{record}: answers entry on {act_id} without act id")
                continue
            edge = answers_edge(act_id, opener, move, confidence, record,
                                labeller, shard)
            edge["hx/props"]["effect"] = entry.get("effect")
            edge["hx/props"]["quote"] = entry.get("quote")
            edges.append(edge)
        scope = act.get("scope") or {}
        within = scope.get("within")
        if within == act_id:
            notes.append(f"{record}: {act_id} is within itself; no edge")
            within = None
        if within:
            edges.append({
                "hx/id": "hx-xiang-scope-" + _sha20("within", within, act_id),
                "hx/type": "xiang/scope",
                "hx/endpoints": [within, act_id],
                "hx/props": {
                    "roles": {"opener": within, "governed": act_id},
                    "relation": "within", "move": move,
                    "confidence": confidence, "record": record,
                    "labeller": labeller, "status": "proposed",
                    "label_set": LABEL_SET, "shard": shard,
                },
            })
        closes = scope.get("closes")
        if closes == act_id:
            notes.append(f"{record}: {act_id} closes itself; no edge")
            closes = None
        if closes:
            edges.append({
                "hx/id": "hx-xiang-scope-" + _sha20("closes", closes, act_id),
                "hx/type": "xiang/scope",
                "hx/endpoints": [closes, act_id],
                "hx/props": {
                    "roles": {"opener": closes, "governed": act_id},
                    "relation": "closes", "move": move,
                    "confidence": confidence, "record": record,
                    "labeller": labeller, "status": "proposed",
                    "label_set": LABEL_SET, "shard": shard,
                },
            })
    return edges, notes


def load_sidecars(pattern: str, limit: int | None = None
                  ) -> tuple[list[tuple[dict, str]], list[str]]:
    """(sidecar, shard-name) pairs in glob order, skipping unreadable files."""
    loaded: list[tuple[dict, str]] = []
    notes: list[str] = []
    for path in sorted(glob.glob(pattern)):
        try:
            with open(path, encoding="utf-8") as handle:
                loaded.append((json.load(handle), os.path.basename(os.path.dirname(path))))
        except (OSError, ValueError) as exc:
            notes.append(f"{path}: unreadable sidecar ({exc})")
        if limit is not None and len(loaded) >= limit:
            break
    return loaded, notes


def post_edge(base: str, edge: dict) -> tuple[bool, str]:
    """POST one edge; 200 and 409 are success.  Returns (ok, detail)."""
    body = json.dumps(edge).encode("utf-8")
    request = urllib.request.Request(
        base.rstrip("/") + "/api/alpha/hyperedge", data=body,
        headers={"Content-Type": "application/json"}, method="POST")
    try:
        with urllib.request.urlopen(request, timeout=30) as response:
            return 200 <= response.status < 300, response.read().decode("utf-8", "replace")[:300]
    except urllib.error.HTTPError as exc:
        if exc.code == 409:
            return True, "duplicate"
        return False, f"HTTP {exc.code}: {exc.read().decode('utf-8', 'replace')[:300]}"
    except (urllib.error.URLError, OSError) as exc:
        return False, str(exc)


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--moves-glob", default=DEFAULT_GLOB)
    parser.add_argument("--base", default=DEFAULT_BASE)
    parser.add_argument("--apply", action="store_true",
                        help="POST edges (default: dry run)")
    parser.add_argument("--limit", type=int, default=None,
                        help="read at most N sidecars")
    args = parser.parse_args(argv)

    loaded, notes = load_sidecars(args.moves_glob, args.limit)
    answers = scope_within = scope_closes = 0
    act_ids: set[str] = set()
    edges: list[dict] = []
    for sidecar, shard in loaded:
        built, build_notes = build_edges(sidecar, shard)
        notes.extend(build_notes)
        edges.extend(built)
        for act in sidecar.get("acts", []):
            if act.get("act"):
                act_ids.add(act["act"])
        for edge in built:
            if edge["hx/type"] == "xiang/answers":
                answers += 1
            elif edge["hx/props"]["relation"] == "within":
                scope_within += 1
            else:
                scope_closes += 1

    print(f"files read: {len(loaded)}")
    print(f"answers edges: {answers}")
    print(f"scope-within edges: {scope_within}")
    print(f"scope-closes edges: {scope_closes}")
    print(f"distinct act ids: {len(act_ids)}")
    for edge in edges[:3]:
        print("sample:", json.dumps(edge, ensure_ascii=False))
    for note in notes[:20]:
        print("note:", note)

    if not args.apply:
        print("dry run: nothing written")
        return 0

    written = no_op = failed = 0
    failure_lines: list[str] = []
    for edge in edges:
        ok, detail = post_edge(args.base, edge)
        if not ok:
            failed += 1
            failure_lines.append(json.dumps(
                {"hx/id": edge["hx/id"], "detail": detail}, ensure_ascii=False))
        elif detail == "duplicate" or "no-op" in detail:
            no_op += 1
        else:
            written += 1
    print(f"written: {written}  no-op: {no_op}  failed: {failed}")
    if failure_lines:
        with open(FAILURES_PATH, "a", encoding="utf-8") as handle:
            for line in failure_lines:
                handle.write(line + "\n")
        print(f"failures logged to {FAILURES_PATH}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
