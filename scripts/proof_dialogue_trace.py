#!/usr/bin/env python3
"""Append-only canonical trace for the APM proof-dialogue pilot.

Matrix is a delivery surface.  This JSONL ledger is the experiment record: it
hashes exact turn bytes, records parent/source pointers, and appends delivery
receipts without editing the originating turn.
"""
import argparse
import hashlib
import json
from pathlib import Path

ROLES = {"teacher", "student", "critic", "scribe"}
MOVE_KINDS = {
    "conjecture", "question", "objection", "counterexample", "repair",
    "lemma", "proof", "discharge", "summary", "standing",
}


def canonical(value):
    return json.dumps(value, ensure_ascii=False, sort_keys=True,
                      separators=(",", ":")).encode()


def digest(value):
    return hashlib.sha256(canonical(value)).hexdigest()


def read_trace(path):
    if not path.exists():
        return []
    records = []
    for number, line in enumerate(path.read_text().splitlines(), 1):
        try:
            records.append(json.loads(line))
        except json.JSONDecodeError as exc:
            raise ValueError(f"invalid JSON at line {number}: {exc.msg}") from None
    return records


def verify(records):
    previous = None
    moves = set()
    deliveries = set()
    for sequence, record in enumerate(records, 1):
        if record.get("sequence") != sequence:
            raise ValueError(f"sequence mismatch at line {sequence}")
        claimed = record.get("record_sha256")
        unhashed = {k: v for k, v in record.items() if k != "record_sha256"}
        if claimed != digest(unhashed):
            raise ValueError(f"record hash mismatch at line {sequence}")
        if record.get("previous_record_sha256") != previous:
            raise ValueError(f"chain mismatch at line {sequence}")
        kind = record.get("record_type")
        if kind == "turn":
            move_id = record.get("move_id")
            if not move_id or move_id in moves:
                raise ValueError(f"duplicate or missing move_id at line {sequence}")
            if record.get("role") not in ROLES:
                raise ValueError(f"invalid role at line {sequence}")
            if record.get("move_kind") not in MOVE_KINDS:
                raise ValueError(f"invalid move_kind at line {sequence}")
            parent = record.get("parent_move_id")
            if parent is not None and parent not in moves:
                raise ValueError(f"unresolved parent at line {sequence}")
            body = record.get("body")
            if not isinstance(body, str) or not body.strip():
                raise ValueError(f"empty body at line {sequence}")
            if record.get("body_sha256") != hashlib.sha256(body.encode()).hexdigest():
                raise ValueError(f"body hash mismatch at line {sequence}")
            moves.add(move_id)
        elif kind == "delivery":
            move_id = record.get("move_id")
            key = (move_id, record.get("surface"), record.get("event_id"))
            if move_id not in moves:
                raise ValueError(f"delivery precedes turn at line {sequence}")
            if not key[1] or not key[2] or key in deliveries:
                raise ValueError(f"invalid or duplicate delivery at line {sequence}")
            deliveries.add(key)
        else:
            raise ValueError(f"invalid record_type at line {sequence}")
        previous = claimed
    return {"records": len(records), "moves": len(moves),
            "deliveries": len(deliveries), "head": previous}


def append(path, record):
    records = read_trace(path)
    summary = verify(records)
    record = dict(record)
    record["sequence"] = len(records) + 1
    record["previous_record_sha256"] = summary["head"]
    record["record_sha256"] = digest(record)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as stream:
        stream.write(canonical(record).decode() + "\n")
        stream.flush()
    verify(read_trace(path))
    return record


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--trace", type=Path, required=True)
    commands = parser.add_subparsers(dest="command", required=True)
    turn = commands.add_parser("turn")
    turn.add_argument("--run-id", required=True)
    turn.add_argument("--move-id", required=True)
    turn.add_argument("--role", choices=sorted(ROLES), required=True)
    turn.add_argument("--move-kind", choices=sorted(MOVE_KINDS), required=True)
    turn.add_argument("--parent-move-id")
    turn.add_argument("--source-ref", action="append", default=[])
    turn.add_argument("--body-file", type=Path, required=True)
    delivery = commands.add_parser("delivery")
    delivery.add_argument("--move-id", required=True)
    delivery.add_argument("--surface", required=True)
    delivery.add_argument("--event-id", required=True)
    commands.add_parser("verify")
    args = parser.parse_args()
    if args.command == "turn":
        body = args.body_file.read_text()
        result = append(args.trace, {
            "record_type": "turn", "run_id": args.run_id,
            "move_id": args.move_id, "role": args.role,
            "move_kind": args.move_kind, "parent_move_id": args.parent_move_id,
            "source_refs": args.source_ref, "body": body,
            "body_sha256": hashlib.sha256(body.encode()).hexdigest(),
        })
    elif args.command == "delivery":
        result = append(args.trace, {
            "record_type": "delivery", "move_id": args.move_id,
            "surface": args.surface, "event_id": args.event_id,
        })
    else:
        result = verify(read_trace(args.trace))
    print(json.dumps(result, ensure_ascii=False, sort_keys=True))


if __name__ == "__main__":
    main()
