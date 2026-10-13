#!/usr/bin/env python3
"""Validate a proof-dialogue scribe checkpoint against the public trace."""
import argparse
import json
from pathlib import Path

from proof_dialogue_trace import read_trace, verify

SCHEMA = "futon3c.proof-dialogue-scribe-checkpoint/v1"
NODE_STATES = {"proposed", "contested", "repaired", "accepted", "open"}
EDGE_KINDS = {"supports", "challenges", "repairs", "depends-on", "discharges"}
PATTERN_STATES = {"noticed", "recurred", "candidate", "validated"}


def validate(checkpoint, trace_path):
    verify(read_trace(trace_path))
    moves = {r["move_id"]: r for r in read_trace(trace_path) if r["record_type"] == "turn"}
    errors = []
    if checkpoint.get("schema") != SCHEMA:
        errors.append("wrong schema")
    if checkpoint.get("through_move_id") not in moves:
        errors.append("through_move_id is not in trace")
    for view in ("conversation", "proof"):
        graph = checkpoint.get(view)
        if not isinstance(graph, dict):
            errors.append(f"{view} must be an object")
            continue
        nodes = graph.get("nodes", [])
        node_ids = {n.get("id") for n in nodes if isinstance(n, dict)}
        for node in nodes:
            if node.get("state") not in NODE_STATES:
                errors.append(f"{view} node has invalid state")
            refs = node.get("move_ids", [])
            if not refs or any(ref not in moves for ref in refs):
                errors.append(f"{view} node has invalid move_ids")
        for edge in graph.get("edges", []):
            if edge.get("kind") not in EDGE_KINDS:
                errors.append(f"{view} edge has invalid kind")
            if edge.get("from") not in node_ids or edge.get("to") not in node_ids:
                errors.append(f"{view} edge has missing endpoint")
    for pattern in checkpoint.get("patterns", []):
        if pattern.get("status") not in PATTERN_STATES:
            errors.append("pattern has invalid status")
        if not all(isinstance(pattern.get(k), str) and pattern[k].strip()
                   for k in ("id", "context", "problem", "response", "consequence")):
            errors.append("pattern is not a complete flexiarg")
        for evidence in pattern.get("evidence", []):
            move = moves.get(evidence.get("move_id"))
            start, end = evidence.get("start"), evidence.get("end")
            if (not move or not isinstance(start, int) or not isinstance(end, int)
                    or not 0 <= start < end <= len(move["body"])
                    or move["body"][start:end] != evidence.get("text")):
                errors.append("pattern evidence does not match trace bytes")
    if not isinstance(checkpoint.get("open_obligations"), list):
        errors.append("open_obligations must be an array")
    if errors:
        raise ValueError("; ".join(errors))
    return {"ok": True, "moves": len(moves),
            "conversation_nodes": len(checkpoint["conversation"]["nodes"]),
            "proof_nodes": len(checkpoint["proof"]["nodes"]),
            "patterns": len(checkpoint["patterns"])}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("checkpoint", type=Path)
    parser.add_argument("--trace", type=Path, required=True)
    args = parser.parse_args()
    print(json.dumps(validate(json.loads(args.checkpoint.read_text()), args.trace), sort_keys=True))


if __name__ == "__main__":
    main()
