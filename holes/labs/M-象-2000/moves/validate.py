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
        return 0, 0, [str(exc)]
    current = {}
    for act in source.get("acts", []) + source.get("fragments", []):
        if act.get("id"):
            current[act["id"]] = act
    previous = {act["id"]: act for act in source.get("previous_reply_acts", [])}
    operator = {aid for aid, act in current.items()
                if act.get("author", "operator") == "operator"}
    valid = void = 0
    for act_id in sidecar.get("unlabelled", []):
        if act_id not in current:
            errors.append(f"unlabelled: unknown act {act_id!r}")
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
    return valid, void, errors


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("files", nargs="+", type=Path)
    args = parser.parse_args()
    failed = False
    for path in args.files:
        valid, void, errors = validate(path)
        print(f"{path}: valid={valid} void={void}")
        for error in errors:
            print(f"  ERROR: {error}", file=sys.stderr)
        failed |= bool(errors)
    raise SystemExit(1 if failed else 0)


if __name__ == "__main__":
    main()
