#!/usr/bin/env python3
"""Validate moves/1 sidecars against their sampled input authority."""
import argparse
import json
import re
import sys
from pathlib import Path

MOVE = re.compile(r"^[a-z-]+/[a-z-]+$")
TOP = {"record", "labeller", "schema", "at", "acts", "unlabelled"}
ACT = {"act", "kind", "move", "quote", "answers", "scope", "confidence"}
ANSWER = {"act", "quote", "effect"}
SCOPE = {"opens", "closes", "within"}


def norm(text):
    return " ".join(str(text or "").split())


def input_path(path, record):
    candidates = [path.with_name(f"{record}.input.json"),
                  path.parent / "input" / f"{record}.input.json"]
    for candidate in candidates:
        if candidate.exists():
            return candidate
    raise ValueError(f"input not found for {record}")


def validate(path):
    errors = []
    try:
        sidecar = json.loads(path.read_text(encoding="utf-8"))
        missing = TOP - sidecar.keys()
        if missing:
            errors.append(f"missing top-level keys: {sorted(missing)}")
        if sidecar.get("schema") != "moves/1":
            errors.append("schema must be moves/1")
        source = json.loads(input_path(path, sidecar.get("record")).read_text(encoding="utf-8"))
    except (OSError, ValueError, TypeError, json.JSONDecodeError) as exc:
        return 0, 0, [str(exc)], []
    current = {}
    # Authority: the reply-paragraph acts as the kernel numbers them
    # (sample.py:reply_marks), then the reading's fragments, then the record's
    # stored acts.  reply_acts and fragments carry full text; the stored :acts
    # carry a 120-character prefix, so they come last (batch 3 shard 2 found
    # 42 of 200 fragments cut by the prefix when :acts came second).
    for act in source.get("reply_acts", []) + source.get("fragments", []) + source.get("acts", []):
        if act.get("id"):
            current.setdefault(act["id"], act)
    previous = {act["id"]: act for act in source.get("previous_reply_acts", [])}
    # Open ports (sample.py:open_ports) are earlier turns' openers the kernel
    # still holds open at this record; a reply may answer or sit within them.
    for port in source.get("open_ports", []) + source.get("in_force", []):
        if port.get("act"):
            previous.setdefault(port["act"], port)
    operator = {aid for aid, act in current.items()
                if act.get("author", "operator") == "operator"}
    valid = void = 0
    for act_id in sidecar.get("unlabelled", []):
        if act_id not in current:
            errors.append(f"unlabelled: unknown act {act_id!r}")
    # Coverage: every input fragment and reply act is either labelled or
    # declared unlabelled. Silent omission is reported, not failed: shard 13
    # of batch 3 omitted 27 acts in 5 records while declaring none, and six
    # earlier shards omitted 1-9 each (2026-10-14 census).
    covered = {act.get("act") for act in sidecar.get("acts", [])} | set(sidecar.get("unlabelled", []))
    uncovered = [act["id"] for act in source.get("fragments", []) + source.get("reply_acts", [])
                 if act.get("id") and act["id"] not in covered]
    for index, act in enumerate(sidecar.get("acts", [])):
        where = f"acts[{index}]"
        missing = ACT - act.keys()
        if missing:
            errors.append(f"{where}: missing {sorted(missing)}")
            continue
        authority = current.get(act.get("act"))
        if authority is None:
            errors.append(f"{where}: unknown act {act.get('act')!r}")
        if not MOVE.fullmatch(str(act.get("move", ""))):
            errors.append(f"{where}: invalid move label")
        if not isinstance(act.get("scope"), dict) or SCOPE - act.get("scope", {}).keys():
            errors.append(f"{where}: scope requires opens, closes, within")
        if act.get("confidence") not in {"high", "medium", "low"}:
            errors.append(f"{where}: invalid confidence")
        quote = norm(act.get("quote"))
        quote_ok = bool(quote) and authority is not None and quote in norm(authority.get("text"))
        if quote_ok:
            valid += 1
        else:
            void += 1
        for aindex, answer in enumerate(act.get("answers", [])):
            if ANSWER - answer.keys():
                errors.append(f"{where}.answers[{aindex}]: missing required keys")
            target = answer.get("act")
            if target not in operator and target not in previous:
                errors.append(f"{where}.answers[{aindex}]: invalid answer target {target!r}")
            if answer.get("effect") not in {"closes", "keeps", "unclear"}:
                errors.append(f"{where}.answers[{aindex}]: invalid effect")
            answer_quote = norm(answer.get("quote"))
            if (not answer_quote
                    or answer_quote not in norm(authority.get("text") if authority else "")):
                errors.append(f"{where}.answers[{aindex}]: quote is not in answering act")
    return valid, void, errors, uncovered


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("files", nargs="+", type=Path)
    args = parser.parse_args()
    failed = False
    for path in args.files:
        valid, void, errors, uncovered = validate(path)
        print(f"{path}: valid={valid} void={void} uncovered={len(uncovered)}")
        for act_id in uncovered:
            print(f"  UNCOVERED: {act_id} (in the input, in neither acts nor unlabelled)", file=sys.stderr)
        for error in errors:
            print(f"  ERROR: {error}", file=sys.stderr)
        failed |= bool(errors)
    raise SystemExit(1 if failed else 0)


if __name__ == "__main__":
    main()
