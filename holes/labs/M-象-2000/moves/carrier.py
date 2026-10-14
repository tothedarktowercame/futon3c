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
import re
import sys
import urllib.error
import urllib.request

LABEL_SET = "moves/1"
DEFAULT_GLOB = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            "..", "data", "moves-scale*", "shard-*", "turn-*.moves.json")
DEFAULT_BASE = "http://127.0.0.1:7093"
PENHOLDER = os.environ.get("MINUS40_PENHOLDER", "api")
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


ACT_ID_RE = re.compile(r"^[\w@.-]+-(?:f-s\d+-\d+|r-\d+|b-\d+)$")


def is_act_id(target: str) -> bool:
    """A scope target that is an act id (fragment, reply, bell), not a name."""
    return bool(ACT_ID_RE.match(target))


def build_opens_index(loaded: list[tuple[dict, str]]) -> dict[str, list[tuple[str, str, str]]]:
    """opens-name -> [(record, act id, shard)] over every loaded sidecar."""
    index: dict[str, list[tuple[str, str, str]]] = {}
    for sidecar, shard in loaded:
        record = sidecar.get("record", "")
        for act in sidecar.get("acts", []):
            opens = (act.get("scope") or {}).get("opens")
            if opens and act.get("act"):
                index.setdefault(opens, []).append((record, act["act"], shard))
    return index


def _session_of(act_id: str) -> str:
    return act_id.split("-turn-")[0]


def resolve_target(target: str, record: str, act_id: str,
                   opens_index: dict[str, list[tuple[str, str, str]]]
                   ) -> tuple[str | None, str | None]:
    """Resolve a scope target to an opener act id.

    Returns (opener act id, resolution) where resolution is None for a
    plain act id, or "same-record" | "same-session" | "unique" for a name;
    (None, None) when a name cannot be resolved.
    """
    if is_act_id(target):
        return target, None
    openers = opens_index.get(target, [])
    same_record = [oid for rec, oid, _ in openers if rec == record]
    if len(same_record) == 1:
        return same_record[0], "same-record"
    if same_record:
        return None, "ambiguous-same-record"
    same_session = [oid for rec, oid, _ in openers
                    if _session_of(oid) == _session_of(act_id)]
    if len(same_session) == 1:
        return same_session[0], "same-session"
    if same_session:
        return None, "ambiguous-same-session"
    if len(openers) == 1:
        return openers[0][1], "unique"
    return None, None


def build_edges(sidecar: dict, shard: str,
                opens_index: dict[str, list[tuple[str, str, str]]] | None = None,
                resolve_stats: dict[str, int] | None = None
                ) -> tuple[list[dict], list[str], list[dict]]:
    """Edges, notes and (old-behaviour) name-endpoint retract bodies."""
    record = sidecar.get("record", "")
    labeller = sidecar.get("labeller", "")
    edges: list[dict] = []
    notes: list[str] = []
    retracts: list[dict] = []
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
        for relation in ("within", "closes"):
            target = scope.get(relation)
            if target == act_id:
                notes.append(f"{record}: {act_id} {relation} itself; no edge")
                target = None
            if not target:
                continue
            opener_id = target
            resolution = None
            if not is_act_id(target):
                retracts.append({
                    "hx/id": "hx-xiang-scope-" + _sha20(relation, target, act_id),
                    "hx/type": "xiang/scope",
                    "hx/endpoints": [target, act_id],
                    "hx/props": {"hx/op": "retract"},
                })
                if opens_index is None:
                    notes.append(
                        f"{record}: {relation} {target!r} is a name and no "
                        f"opens index was given; no edge")
                    continue
                opener_id, resolution = resolve_target(
                    target, record, act_id, opens_index)
                if opener_id is None:
                    count = len(opens_index.get(target, []))
                    notes.append(
                        f"{record}: {relation} {target!r} resolves to "
                        + ("no opener" if count == 0 else f"{count} openers"))
                    if resolve_stats is not None:
                        resolve_stats["unresolved"] = resolve_stats.get("unresolved", 0) + 1
                    continue
                if resolve_stats is not None:
                    resolve_stats[resolution] = resolve_stats.get(resolution, 0) + 1
            edges.append({
                "hx/id": "hx-xiang-scope-" + _sha20(relation, opener_id, act_id),
                "hx/type": "xiang/scope",
                "hx/endpoints": [opener_id, act_id],
                "hx/props": {
                    "roles": {"opener": opener_id, "governed": act_id},
                    "relation": relation, "move": move,
                    "confidence": confidence, "record": record,
                    "labeller": labeller, "status": "proposed",
                    "label_set": LABEL_SET, "shard": shard,
                    **({"opener_name": target, "resolved": resolution}
                       if resolution else {}),
                },
            })
    return edges, notes, retracts


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
        # The store's layer-3 gate wants a penholder; minus40's own writers
        # send "joe" (ingest_hook.py). "api" is the other allowed value.
        headers={"Content-Type": "application/json", "Accept": "application/json",
                 "x-penholder": PENHOLDER}, method="POST")
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
    parser.add_argument("--retract-named", action="store_true",
                        help="also POST retract bodies for the old "
                             "name-endpoint edges (implies nothing without --apply)")
    args = parser.parse_args(argv)

    loaded, notes = load_sidecars(args.moves_glob, args.limit)
    opens_index = build_opens_index(loaded)
    resolve_stats: dict[str, int] = {}
    answers = scope_within = scope_closes = 0
    act_ids: set[str] = set()
    edges: list[dict] = []
    retracts: list[dict] = []
    for sidecar, shard in loaded:
        built, build_notes, built_retracts = build_edges(
            sidecar, shard, opens_index, resolve_stats)
        notes.extend(build_notes)
        edges.extend(built)
        retracts.extend(built_retracts)
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

    resolved = (resolve_stats.get("same-record", 0)
                + resolve_stats.get("same-session", 0)
                + resolve_stats.get("unique", 0))
    unresolved = resolve_stats.get("unresolved", 0)
    print(f"files read: {len(loaded)}")
    print(f"answers edges: {answers}")
    print(f"scope-within edges: {scope_within}")
    print(f"scope-closes edges: {scope_closes}")
    print(f"distinct act ids: {len(act_ids)}")
    print(f"named openers resolved: {resolved} (same-record "
          f"{resolve_stats.get('same-record', 0)}, same-session "
          f"{resolve_stats.get('same-session', 0)}, unique "
          f"{resolve_stats.get('unique', 0)})")
    print(f"named openers unresolved: {unresolved}")
    print(f"name-endpoint retracts available: {len(retracts)}")
    for edge in edges[:3]:
        print("sample:", json.dumps(edge, ensure_ascii=False))
    for note in notes:
        print("note:", note)

    if not args.apply:
        print("dry run: nothing written")
        return 0

    written = no_op = failed = 0
    failure_lines: list[str] = []
    to_post = list(edges)
    if args.retract_named:
        to_post.extend(retracts)
    for edge in to_post:
        ok, detail = post_edge(args.base, edge)
        if not ok:
            failed += 1
            failure_lines.append(json.dumps(
                {"hx/id": edge["hx/id"], "detail": detail}, ensure_ascii=False))
        elif detail == "duplicate" or "no-op" in detail:
            no_op += 1
        else:
            written += 1
    # The scratch store's compat route answers 200 {"ok":true,...} for a put
    # whether or not the doc changed, so "posted" is what we can count;
    # no-op only when the server says so (futon1b proper: {:no-op? true}).
    print(f"posted: {written}  no-op: {no_op}  failed: {failed}")
    if failure_lines:
        with open(FAILURES_PATH, "a", encoding="utf-8") as handle:
            for line in failure_lines:
                handle.write(line + "\n")
        print(f"failures logged to {FAILURES_PATH}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
