#!/usr/bin/env python3
"""Report the marginal cost basis of the next turn in one local session."""

import argparse
import collections
from datetime import datetime, timezone
import glob
import json
import os
from pathlib import Path
import statistics
import sys

# Opus 5 base rates, kept equal to claude-spend.py ($/MTok).
IN, OUT = 5.0, 25.0
RD, W5, W1H = IN * 0.1, IN * 1.25, IN * 2.0
TAIL_BYTES = 256 * 1024

# Operational parameters relative to the Opus 5 base rates above. These are
# explicitly not assertions about vendor list prices.
MODEL_MULTIPLIERS = {
    "opus": 1.0,
    "fable": 2.0,
    "sonnet": 0.2,
    "haiku": 0.04,
}


def scan(path):
    """The usage aggregation from claude-spend.py, plus per-turn outputs."""
    totals, count, contexts, outputs = collections.Counter(), 0, [], []
    seen = set()
    with open(path, errors="replace") as stream:
        for line in stream:
            try:
                record = json.loads(line)
            except ValueError:
                continue
            usage = (record.get("message") or {}).get("usage")
            if not usage or not first_record_for_message(record, seen):
                continue
            count += 1
            creation = usage.get("cache_creation") or {}
            created = usage.get("cache_creation_input_tokens", 0) or 0
            totals["in"] += usage.get("input_tokens", 0) or 0
            totals["cr"] += usage.get("cache_read_input_tokens", 0) or 0
            totals["out"] += usage.get("output_tokens", 0) or 0
            if creation:
                totals["cc5"] += creation.get("ephemeral_5m_input_tokens", 0) or 0
                totals["cc1h"] += creation.get("ephemeral_1h_input_tokens", 0) or 0
            else:
                totals["cc5"] += created
            contexts.append(
                (usage.get("input_tokens", 0) or 0)
                + (usage.get("cache_read_input_tokens", 0) or 0)
                + created
            )
            outputs.append(usage.get("output_tokens", 0) or 0)
    return totals, count, contexts, outputs


def cost(totals):
    """The weighted cost calculation from claude-spend.py."""
    return (
        totals["in"] * IN
        + totals["cr"] * RD
        + totals["cc5"] * W5
        + totals["cc1h"] * W1H
        + totals["out"] * OUT
    ) / 1e6


def tail_records(path, size=TAIL_BYTES):
    """Parse valid JSON records from the final SIZE bytes, in file order."""
    with open(path, "rb") as stream:
        stream.seek(0, os.SEEK_END)
        end = stream.tell()
        start = max(0, end - size)
        stream.seek(start)
        data = stream.read()
    if start:
        _, _, data = data.partition(b"\n")
    records = []
    for raw in data.splitlines():
        try:
            records.append(json.loads(raw.decode("utf-8", errors="replace")))
        except (ValueError, UnicodeDecodeError):
            continue
    return records


def claude_usage_records(records):
    """Assistant records with real usage; skips the CLI's zero-usage
    ``<synthetic>`` placeholders (e.g. the compaction boundary)."""
    out = []
    for r in records:
        message = r.get("message") or {}
        if message.get("usage") and message.get("model") != "<synthetic>":
            out.append(r)
    return out


def message_key(record):
    """One API response is written as one record per content block (thinking,
    text, tool_use), each repeating the same usage. Key on the message id so a
    response is priced once. claude-spend.py does NOT do this and overcounts."""
    message = record.get("message") or {}
    return message.get("id") or record.get("uuid") or id(record)


def first_record_for_message(record, seen):
    key = message_key(record)
    if key in seen:
        return False
    seen.add(key)
    return True


def dedupe_usage_records(records):
    seen = set()
    return [r for r in records if first_record_for_message(r, seen)]


def is_typed_prompt(record):
    """A user record whose content is a plain string is something the operator
    typed; tool results are user records with a list content."""
    if record.get("type") != "user":
        return False
    message = record.get("message") or {}
    return message.get("role") == "user" and isinstance(message.get("content"), str)


def last_turn_records(path, records):
    """Records from the last typed prompt onward. If the tail window holds no
    prompt boundary, fall back to the whole file."""
    idx = next((i for i in range(len(records) - 1, -1, -1)
                if is_typed_prompt(records[i])), None)
    if idx is None and os.path.getsize(path) > TAIL_BYTES:
        records = tail_records(path, os.path.getsize(path))
        idx = next((i for i in range(len(records) - 1, -1, -1)
                    if is_typed_prompt(records[i])), None)
    if idx is None:
        return None, []
    return records[idx], records[idx + 1:]


def last_turn_report(path, records, multiplier):
    """Price the turn that just completed: every unique API response since the
    operator's last typed prompt."""
    prompt, rest = last_turn_records(path, records)
    calls = dedupe_usage_records(claude_usage_records(rest))
    if prompt is None or not calls:
        return {}
    totals = collections.Counter()
    for record in calls:
        usage = record["message"]["usage"]
        creation = usage.get("cache_creation") or {}
        totals["in"] += usage.get("input_tokens", 0) or 0
        totals["cr"] += usage.get("cache_read_input_tokens", 0) or 0
        totals["out"] += usage.get("output_tokens", 0) or 0
        if creation:
            totals["cc5"] += creation.get("ephemeral_5m_input_tokens", 0) or 0
            totals["cc1h"] += creation.get("ephemeral_1h_input_tokens", 0) or 0
        else:
            totals["cc5"] += usage.get("cache_creation_input_tokens", 0) or 0
    report = {
        "last_turn_usd": round(cost(totals) * multiplier, 6),
        "last_turn_calls": len(calls),
        "last_turn_out": totals["out"],
        "last_turn_in": totals["in"] + totals["cr"] + totals["cc5"] + totals["cc1h"],
        "last_turn_cache_write": totals["cc5"] + totals["cc1h"],
    }
    if prompt.get("timestamp"):
        report["last_turn_started_at"] = iso_z(parse_time(prompt["timestamp"]))
    return report


def model_multiplier(model):
    lowered = (model or "").lower()
    for fragment, multiplier in MODEL_MULTIPLIERS.items():
        if fragment in lowered:
            return multiplier, True, fragment
    return 1.0, False, model or "unknown"


def parse_time(value):
    return datetime.fromisoformat(value.replace("Z", "+00:00"))


def iso_z(value):
    return value.astimezone(timezone.utc).isoformat(timespec="seconds").replace(
        "+00:00", "Z"
    )


def median_output(outputs):
    return statistics.median(outputs) if len(outputs) >= 5 else 1000


def claude_report(path, session_id, fast=False, now=None):
    tail = tail_records(path)
    recent = dedupe_usage_records(claude_usage_records(tail))
    if not recent:
        raise ValueError("session has no assistant usage records")
    last = recent[-1]
    usage = last["message"]["usage"]
    model = last["message"].get("model") or "unknown"
    multiplier, known, _label = model_multiplier(model)
    created = usage.get("cache_creation_input_tokens", 0) or 0
    context = (
        (usage.get("input_tokens", 0) or 0)
        + (usage.get("cache_read_input_tokens", 0) or 0)
        + created
    )
    timestamp = parse_time(last["timestamp"])
    current = now or datetime.now(timezone.utc)
    warm_seconds = max(0, int((current - timestamp).total_seconds()))
    creation = usage.get("cache_creation") or {}

    totals = count = contexts = outputs = None
    if not fast:
        totals, count, contexts, outputs = scan(path)
        one_hour = totals["cc1h"] > totals["cc5"]
    else:
        cc1h = sum(
            ((r["message"]["usage"].get("cache_creation") or {}).get(
                "ephemeral_1h_input_tokens", 0
            ) or 0)
            for r in recent
        )
        cc5 = sum(
            ((r["message"]["usage"].get("cache_creation") or {}).get(
                "ephemeral_5m_input_tokens", 0
            ) or 0)
            for r in recent
        )
        one_hour = cc1h > cc5
        outputs = [r["message"]["usage"].get("output_tokens", 0) or 0 for r in recent]

    # A record without a cache_creation breakdown follows claude-spend.py and
    # is treated as a 5m write. A present breakdown contributes to the session
    # predominance calculation above.
    if creation and not any(creation.values()):
        one_hour = False
    ttl = 3600 if one_hour else 300
    output_basis = median_output(outputs)
    warm_cost = (context * RD + output_basis * OUT) * multiplier / 1e6
    write_rate = W1H if one_hour else W5
    cold_cost = (context * write_rate + output_basis * OUT) * multiplier / 1e6
    report = {
        "vendor": "claude",
        "session": session_id,
        "model": model,
        "mult": multiplier,
        "ctx": context,
        "last_turn_at": iso_z(timestamp),
        "warm_s": warm_seconds,
        "cold": warm_seconds > ttl,
        "ttl_s": ttl,
        "per_turn_usd": round(warm_cost, 6),
        "per_turn_cold_usd": round(cold_cost, 6),
    }
    if not known:
        report["mult_known"] = False
    report.update(last_turn_report(path, tail, multiplier))
    if not fast:
        report.update(
            {
                "turns": count,
                "session_usd": round(cost(totals) * multiplier, 6),
                "session_out_tokens": totals["out"],
                "session_in_tokens": totals["in"] + totals["cr"]
                + totals["cc5"] + totals["cc1h"],
            }
        )
    return report


def codex_token_record(record):
    payload = record.get("payload") or {}
    if payload.get("type") != "token_count":
        return None
    return (payload.get("info") or {}).get("total_token_usage")


def codex_report(path, session_id, fast=False, now=None):
    recent = [(r, codex_token_record(r)) for r in tail_records(path)]
    recent = [(r, usage) for r, usage in recent if usage]
    if not recent:
        raise ValueError("session has no token_count records")
    last_record, cumulative = recent[-1]
    payload = last_record.get("payload") or {}
    last_usage = (payload.get("info") or {}).get("last_token_usage") or {}
    timestamp = parse_time(last_record["timestamp"])
    current = now or datetime.now(timezone.utc)
    report = {
        "vendor": "codex",
        "session": session_id,
        "model": "codex",
        "ctx": last_usage.get("input_tokens", 0) or 0,
        "last_turn_at": iso_z(timestamp),
        "warm_s": max(0, int((current - timestamp).total_seconds())),
        "session_in_tokens": cumulative.get("input_tokens", 0) or 0,
        "session_out_tokens": cumulative.get("output_tokens", 0) or 0,
        "session_total_tokens": cumulative.get("total_tokens", 0) or 0,
    }
    if not fast:
        turns = 0
        with open(path, errors="replace") as stream:
            for line in stream:
                try:
                    turns += codex_token_record(json.loads(line)) is not None
                except ValueError:
                    continue
        report["turns"] = turns
    return report


def find_claude_session(session_id):
    matches = glob.glob(
        os.path.expanduser(f"~/.claude/projects/*/{session_id}.jsonl")
    )
    return matches[0] if len(matches) == 1 else None


def find_codex_session(session_id):
    root = os.path.expanduser("~/.codex/sessions")
    candidates = glob.glob(os.path.join(root, "**", "*.jsonl"), recursive=True)
    filename_matches = [p for p in candidates if session_id in os.path.basename(p)]
    if len(filename_matches) == 1:
        return filename_matches[0]
    header_matches = []
    for path in candidates:
        try:
            with open(path, errors="replace") as stream:
                for _ in range(5):
                    record = json.loads(next(stream))
                    payload = record.get("payload") or {}
                    if session_id in {payload.get("id"), payload.get("session_id")}:
                        header_matches.append(path)
                        break
        except (OSError, StopIteration, ValueError):
            continue
    return header_matches[0] if len(header_matches) == 1 else None


def human(report):
    if report["vendor"] == "codex":
        turns = f" / {report['turns']}t" if "turns" in report else ""
        return (
            f"codex | ctx {report['ctx'] / 1000:.0f}k | "
            f"in {report['session_in_tokens']:,} | out {report['session_out_tokens']:,}"
            f"{turns}"
        )
    warm = report["warm_s"]
    age = f"{warm // 3600}h" if warm >= 3600 else f"{warm // 60}m"
    session = (
        f" | session ${report['session_usd']:.2f} / {report['turns']}t"
        if "session_usd" in report
        else ""
    )
    label = model_multiplier(report["model"])[2]
    last = (
        f" | last turn ${report['last_turn_usd']:.2f} ({report['last_turn_calls']} calls)"
        if "last_turn_usd" in report
        else ""
    )
    return (
        f"{label} x{report['mult']:g} | ctx {report['ctx']/1000:.0f}k"
        f" | warm {age} | ~${report['per_turn_usd']:.2f}/call "
        f"(cold ${report['per_turn_cold_usd']:.2f}){last}{session}"
    )


def main(argv=None):
    parser = argparse.ArgumentParser()
    source = parser.add_mutually_exclusive_group(required=True)
    source.add_argument("--session")
    source.add_argument("--session-file")
    parser.add_argument("--vendor", choices=["claude", "codex"], default="claude")
    parser.add_argument("--json", action="store_true")
    parser.add_argument("--fast", action="store_true")
    args = parser.parse_args(argv)

    path = args.session_file
    session_id = args.session
    if path:
        path = os.path.expanduser(path)
        session_id = session_id or Path(path).stem
    elif args.vendor == "claude":
        path = find_claude_session(session_id)
    else:
        path = find_codex_session(session_id)
    if not path or not os.path.isfile(path):
        print(f"session file not found: {session_id or args.session_file}", file=sys.stderr)
        return 2
    try:
        report = (
            claude_report(path, session_id, args.fast)
            if args.vendor == "claude"
            else codex_report(path, session_id, args.fast)
        )
    except (OSError, ValueError) as error:
        print(str(error), file=sys.stderr)
        return 2
    print(json.dumps(report, separators=(",", ":")) if args.json else human(report))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
