#!/usr/bin/env python3
"""Replay frozen 馬/驴 Claude Code prompts through the Claude Platform.

The source JSONL is read-only.  Results are appended to a separate JSONL
ledger.  The API key is read from a mode-0600 file and is never written to
the ledger.  A conservative pre-request cost bound prevents the run from
crossing --max-usd merely because the final request was large.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import sys
import time
import urllib.error
import urllib.request
from pathlib import Path

API_URL = "https://api.anthropic.com/v1/messages"
API_VERSION = "2023-06-01"
MODEL_PRICES = {
    "claude-haiku-5-5": {
        "short": (0.10, 0.50),
        "long": (0.50, 2.50),
        "threshold": 100_000,
    },
    "claude-sonnet-5-5": {"short": (2.00, 10.00), "long": (2.00, 10.00)},
}
TASK_MARKERS = {
    "ma": "馬 alignment reading:",
    "lu": "A reranking task.",
}


def text_content(message: dict) -> str:
    content = message.get("content", "")
    if isinstance(content, str):
        return content
    if isinstance(content, list):
        return "".join(
            str(block.get("text", ""))
            for block in content
            if isinstance(block, dict) and block.get("type") == "text"
        )
    return ""


def load_frozen_cases(path: Path, task: str) -> list[dict]:
    """Return user prompts and their directly linked Claude Code replies."""
    rows = []
    with path.open(encoding="utf-8") as handle:
        for line_no, line in enumerate(handle, 1):
            try:
                row = json.loads(line)
            except json.JSONDecodeError as exc:
                raise ValueError(f"invalid JSONL at {path}:{line_no}: {exc}") from exc
            rows.append(row)
    assistants: dict[str, list[dict]] = {}
    for row in rows:
        if row.get("type") == "assistant" and row.get("parentUuid"):
            assistants.setdefault(str(row["parentUuid"]), []).append(row)
    marker = TASK_MARKERS[task]
    cases = []
    for row in rows:
        if row.get("type") != "user":
            continue
        prompt = text_content(row.get("message") or {})
        if marker not in prompt:
            continue
        replies = assistants.get(str(row.get("uuid")), [])
        reply = "".join(text_content(r.get("message") or {}) for r in replies)
        if not reply:
            continue
        cases.append({
            "source_uuid": row.get("uuid"),
            "source_timestamp": row.get("timestamp"),
            "prompt": prompt,
            "baseline_reply": reply,
        })
    return cases


def outer_array(text: str):
    lo, hi = text.find("["), text.rfind("]")
    if lo < 0 or hi <= lo:
        return None
    try:
        value = json.loads(text[lo : hi + 1])
    except json.JSONDecodeError:
        return None
    return value if isinstance(value, list) else None


def normalized_rows(text: str, task: str):
    rows = outer_array(text)
    if rows is None or not all(isinstance(row, dict) for row in rows):
        return None
    if task == "ma":
        return {
            str(row.get("act")): (row.get("verdict"), row.get("by"), row.get("quote"))
            for row in rows
            if row.get("act") is not None
        }
    return [str(row.get("pattern", "")).lstrip("~") for row in rows]


def comparison(baseline: str, replay: str, task: str) -> dict:
    left, right = normalized_rows(baseline, task), normalized_rows(replay, task)
    result = {"baseline_json": left is not None, "replay_json": right is not None}
    if left is None or right is None:
        return result
    if task == "ma":
        common = sorted(set(left) & set(right))
        result.update({
            "common": len(common),
            "verdict_agreement": (
                sum(left[k][0] == right[k][0] for k in common) / len(common)
                if common else None
            ),
            "exact_agreement": (
                sum(left[k] == right[k] for k in common) / len(common)
                if common else None
            ),
        })
    else:
        l3, r3 = left[:3], right[:3]
        result.update({
            "top3_overlap": len(set(l3) & set(r3)),
            "top1_equal": bool(l3 and r3 and l3[0] == r3[0]),
        })
    return result


def grounding(prompt: str, replay: str, task: str) -> dict:
    """Apply the task's existing verbatim-quote condition to replay output."""
    rows = outer_array(replay)
    if rows is None or not all(isinstance(row, dict) for row in rows):
        return {"objects": 0, "grounded": 0, "fully_grounded": False}
    if task == "lu":
        try:
            operator = prompt.split("Operator turn:\n", 1)[1].split("\n\n--- pattern ", 1)[0]
        except IndexError:
            return {"objects": len(rows), "grounded": 0, "fully_grounded": False}
        operator = re.sub(r"\s+", " ", operator)
        checks = []
        for row in rows:
            quote = row.get("quote")
            checks.append(
                isinstance(quote, str)
                and len(quote.split()) >= 5
                and re.sub(r"\s+", " ", quote).strip() in operator
            )
    else:
        try:
            paragraph_json = prompt.split("Reply paragraphs (id, mark, text):\n", 1)[1]
            paragraphs, _ = json.JSONDecoder().raw_decode(paragraph_json)
            paragraph_text = {row["id"]: row["text"] for row in paragraphs}
        except (IndexError, KeyError, TypeError, ValueError, json.JSONDecodeError):
            return {"objects": len(rows), "grounded": 0, "fully_grounded": False}
        checks = []
        for row in rows:
            quote = row.get("quote")
            checks.append(
                row.get("verdict") == "not-answered"
                or (
                    isinstance(quote, str)
                    and len(quote.split()) >= 3
                    and quote in paragraph_text.get(row.get("by"), "")
                )
            )
    return {
        "objects": len(checks),
        "grounded": sum(checks),
        "fully_grounded": bool(checks) and all(checks),
    }


def price(model: str, usage: dict) -> float:
    rates = MODEL_PRICES[model]
    input_tokens = int(usage.get("input_tokens", 0))
    output_tokens = int(usage.get("output_tokens", 0))
    tier = "long" if input_tokens > rates.get("threshold", sys.maxsize) else "short"
    input_rate, output_rate = rates[tier]
    return (input_tokens * input_rate + output_tokens * output_rate) / 1_000_000


def conservative_bound(model: str, prompt: str, max_tokens: int) -> float:
    """Bytes bound input tokens; max_tokens bounds the charged output."""
    rates = MODEL_PRICES[model]
    input_bound = len(prompt.encode("utf-8"))
    tier = "long" if input_bound > rates.get("threshold", sys.maxsize) else "short"
    input_rate, output_rate = rates[tier]
    return (input_bound * input_rate + max_tokens * output_rate) / 1_000_000


def platform_call(api_key: str, model: str, prompt: str, max_tokens: int) -> dict:
    body = json.dumps({
        "model": model,
        "max_tokens": max_tokens,
        "messages": [{"role": "user", "content": prompt}],
    }).encode("utf-8")
    request = urllib.request.Request(API_URL, data=body, method="POST", headers={
        "content-type": "application/json",
        "x-api-key": api_key,
        "anthropic-version": API_VERSION,
    })
    started = time.monotonic()
    try:
        with urllib.request.urlopen(request, timeout=180) as response:
            payload = json.loads(response.read())
            request_id = response.headers.get("request-id")
    except urllib.error.HTTPError as exc:
        detail = exc.read().decode("utf-8", "replace")[:2000]
        raise RuntimeError(f"Claude Platform HTTP {exc.code}: {detail}") from exc
    text = "".join(
        str(block.get("text", ""))
        for block in payload.get("content", [])
        if block.get("type") == "text"
    )
    if not text:
        raise RuntimeError("Claude Platform response contained no text block")
    return {
        "text": text,
        "usage": payload.get("usage") or {},
        "response_model": payload.get("model"),
        "request_id": request_id,
        "elapsed_ms": round((time.monotonic() - started) * 1000),
    }


def read_key(path: Path) -> str:
    mode = path.stat().st_mode & 0o777
    if mode & 0o077:
        raise ValueError(f"credential file must not be group/world accessible (mode {mode:o})")
    key = path.read_text(encoding="utf-8").strip()
    if not key:
        raise ValueError("credential file is empty")
    return key


def main(argv=None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--task", choices=sorted(TASK_MARKERS), required=True)
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--key-file", type=Path, default=Path("~/.joe-claude-api-key").expanduser())
    parser.add_argument("--models", default="claude-haiku-5-5,claude-sonnet-5-5")
    parser.add_argument("--limit", type=int, default=20)
    parser.add_argument("--max-tokens", type=int, default=2048)
    parser.add_argument("--max-usd", type=float, default=2.0)
    parser.add_argument("--output", type=Path, required=True)
    parser.add_argument("--dry-run", action="store_true")
    args = parser.parse_args(argv)
    models = [m.strip() for m in args.models.split(",") if m.strip()]
    unknown = set(models) - set(MODEL_PRICES)
    if unknown:
        parser.error(f"no price table for models: {sorted(unknown)}")
    if args.limit < 1 or args.max_tokens < 1 or args.max_usd <= 0:
        parser.error("limit, max-tokens and max-usd must be positive")
    cases = load_frozen_cases(args.source, args.task)[-args.limit :]
    if not cases:
        parser.error("source contains no linked frozen cases for that task")
    plan_bound = sum(
        conservative_bound(model, case["prompt"], args.max_tokens)
        for case in cases for model in models
    )
    print(json.dumps({
        "task": args.task, "cases": len(cases), "models": models,
        "requests": len(cases) * len(models), "conservative_bound_usd": round(plan_bound, 6),
        "max_usd": args.max_usd,
    }))
    if args.dry_run:
        return 0
    key = read_key(args.key_file)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    spent = 0.0
    completed = 0
    with args.output.open("x", encoding="utf-8") as ledger:
        for case in cases:
            for model in models:
                bound = conservative_bound(model, case["prompt"], args.max_tokens)
                if spent + bound > args.max_usd:
                    print(json.dumps({"stopped": "pre-request-cost-bound", "spent_usd": spent}))
                    return 0
                result = platform_call(key, model, case["prompt"], args.max_tokens)
                cost = price(model, result["usage"])
                spent += cost
                completed += 1
                row = {
                    "schema": "xiang-platform-replay.v1",
                    "task": args.task,
                    "source_file": str(args.source),
                    "source_uuid": case["source_uuid"],
                    "source_timestamp": case["source_timestamp"],
                    "model": model,
                    "usage": result["usage"],
                    "cost_usd": cost,
                    "elapsed_ms": result["elapsed_ms"],
                    "request_id": result["request_id"],
                    "response_model": result["response_model"],
                    "comparison": comparison(case["baseline_reply"], result["text"], args.task),
                    "grounding": grounding(case["prompt"], result["text"], args.task),
                    "baseline_reply": case["baseline_reply"],
                    "replay_reply": result["text"],
                }
                ledger.write(json.dumps(row, ensure_ascii=False) + "\n")
                ledger.flush()
                print(json.dumps({"completed": completed, "model": model,
                                  "cost_usd": cost, "spent_usd": spent}))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
