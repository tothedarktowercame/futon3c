#!/usr/bin/env python3
"""Build a blinded scope-labelling packet from a canonical dialogue trace."""
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

from proof_dialogue_trace import read_trace, verify


def canonical(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True,
                      separators=(",", ":")).encode()


def build(trace_path: Path) -> dict:
    records = read_trace(trace_path)
    trace_summary = verify(records)
    turns = [record for record in records if record["record_type"] == "turn"]
    roles = {role: f"participant-{index + 1}"
             for index, role in enumerate(sorted({turn["role"] for turn in turns}))}
    messages = []
    for turn in turns:
        body = turn["body"]
        messages.append({
            "message_id": turn["move_id"],
            "parent_message_id": turn["parent_move_id"],
            "speaker": roles[turn["role"]],
            "text": body,
            "text_sha256": hashlib.sha256(body.encode()).hexdigest(),
            "offset_unit": "unicode-codepoint",
        })
    packet = {
        "schema": "futon3c.proof-dialogue-scope-packet/v1",
        "task": "label conversational scopes and their within/keep/close relations",
        "blinding": {
            "excluded": ["Lean source", "formal captures", "goal features",
                         "authored move kinds", "role names", "Matrix event IDs",
                         "source references"],
            "speaker_aliases_stable_within_packet": True,
        },
        "trace_head_sha256": trace_summary["head"],
        "messages": messages,
    }
    packet["packet_sha256"] = hashlib.sha256(canonical(packet)).hexdigest()
    return packet


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("trace", type=Path)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    args.output.write_text(json.dumps(build(args.trace), ensure_ascii=False,
                                      sort_keys=True, indent=2) + "\n")


if __name__ == "__main__":
    main()
