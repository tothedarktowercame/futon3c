#!/usr/bin/env python3
"""Replay a frozen 象 brief with a fresh Platform-funded Claude Code process.

The live request and its input sidecars are copied to an isolated run
directory.  The prompt is rewritten to those copies; the already-published
analysis is deliberately not copied until after the model exits.  Claude Code
runs restricted, without write tools or session persistence.  The proposed
analysis is then passed to the existing session_turn_analysis.py `complete`
validator against the copied request, so publication can affect only the run
directory.
"""

from __future__ import annotations

import argparse
import json
import os
import re
import shutil
import subprocess
from pathlib import Path

from xiang_platform_replay import platform_call, price, read_key, text_content

TURN_PATH = re.compile(r"/home/joe/\.emacs-graph/session-turn-analysis/(turn-[A-Za-z0-9_-]+\.json)")


def frozen_briefs(source: Path) -> list[dict]:
    cases = []
    with source.open(encoding="utf-8") as handle:
        for line in handle:
            row = json.loads(line)
            if row.get("type") != "user":
                continue
            prompt = text_content(row.get("message") or {})
            match = TURN_PATH.search(prompt)
            if match and "interpret operator turn" in prompt:
                cases.append({"uuid": row.get("uuid"), "timestamp": row.get("timestamp"),
                              "prompt": prompt, "record_name": match.group(1)})
    return cases


def isolate_case(case: dict, run_dir: Path, live_store: Path) -> tuple[str, Path, Path | None]:
    run_dir.mkdir(parents=True, exist_ok=False)
    live_record = live_store / case["record_name"]
    if not live_record.is_file():
        raise FileNotFoundError(live_record)
    copied_record = run_dir / live_record.name
    shutil.copy2(live_record, copied_record)
    for suffix in (".draft.json", ".patterns.json", ".quotes.json"):
        sidecar = Path(str(live_record) + suffix)
        if sidecar.is_file():
            shutil.copy2(sidecar, Path(str(copied_record) + suffix))
    live_analysis = Path(str(live_record) + ".analysis.json")
    rewritten = case["prompt"].replace(str(live_record), str(copied_record))
    override = (
        "\n\nREPLAY OVERRIDE — this is an isolated, read-only experiment. "
        "Do not call `complete`; do not write or edit any file; do not bell anyone. "
        "The historical published analysis is intentionally unavailable and must not be sought. "
        "Use the supplied copied record, draft, candidates and read-only pattern searches. "
        "Return only the complete proposed analysis JSON object that would have been passed to "
        "`complete`, with no Markdown fence or commentary. Set `labeller` to "
        "`platform-sonnet-replay`.\n"
    )
    return rewritten + override, copied_record, live_analysis if live_analysis.is_file() else None


def extract_result(envelope: dict) -> dict:
    result = envelope.get("result")
    if not isinstance(result, str):
        raise ValueError("Claude Code envelope has no string result")
    return extract_object(result)


def extract_object(text: str) -> dict:
    candidate = text.strip()
    if candidate.startswith("```") and candidate.endswith("```"):
        candidate = re.sub(r"^```(?:json)?\s*", "", candidate)
        candidate = re.sub(r"\s*```$", "", candidate)
    parsed = json.loads(candidate)
    if not isinstance(parsed, dict):
        raise ValueError("response is not a JSON object")
    return parsed


def main(argv=None) -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--run-dir", type=Path, required=True)
    parser.add_argument("--key-file", type=Path, default=Path("~/.joe-claude-api-key").expanduser())
    parser.add_argument("--live-store", type=Path,
                        default=Path("~/.emacs-graph/session-turn-analysis").expanduser())
    parser.add_argument("--model", default="claude-sonnet-5-5")
    parser.add_argument("--max-usd", type=float, default=1.0)
    args = parser.parse_args(argv)
    cases = frozen_briefs(args.source)
    if not cases:
        parser.error("no intact 象 analysis brief found")
    case = cases[-1]
    prompt, copied_record, live_analysis = isolate_case(case, args.run_dir, args.live_store)
    template = subprocess.run(
        ["python3", "scripts/session_turn_analysis.py", "template", str(copied_record)],
        cwd="/home/joe/code/futon3c", text=True, capture_output=True, timeout=120,
    )
    if template.returncode != 0:
        raise RuntimeError(f"template failed: {template.stderr}")
    prompt += ("\nThe exact validator template for this copied request follows. Preserve its "
               "top-level and per-sentence shape, replacing placeholders with the analysis:\n"
               + template.stdout + "\n")
    api_key = read_key(args.key_file)
    env = os.environ.copy()
    env["ANTHROPIC_API_KEY"] = api_key
    command = [
        "claude", "--print", "--bare", "--restricted", "--no-session-persistence",
        "--model", args.model, "--effort", "medium", "--max-budget-usd", str(args.max_usd),
        "--permission-mode", "dontAsk", "--permission-prompts", "none",
        "--allowedTools", "Read", "Glob", "Grep",
        "Bash(python3 /home/joe/code/futon3c/scripts/xlate.py find *)",
        "--add-dir", str(args.run_dir), "/home/joe/code",
        "--output-format", "json",
    ]
    proc = subprocess.run(command, input=prompt, text=True, capture_output=True, env=env,
                          cwd="/home/joe/code/futon3c", timeout=600)
    (args.run_dir / "claude.stderr").write_text(proc.stderr, encoding="utf-8")
    (args.run_dir / "claude-envelope.json").write_text(proc.stdout, encoding="utf-8")
    if proc.returncode != 0:
        raise RuntimeError(f"Claude Code exited {proc.returncode}; see claude.stderr")
    envelope = json.loads(proc.stdout)
    analysis = extract_result(envelope)
    analysis_path = args.run_dir / "proposed-analysis.json"
    analysis_path.write_text(json.dumps(analysis, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    validation = subprocess.run(
        ["python3", "scripts/session_turn_analysis.py", "complete",
         str(copied_record), str(analysis_path)],
        cwd="/home/joe/code/futon3c", text=True, capture_output=True, timeout=120,
    )
    repair = None
    if validation.returncode != 0:
        repair_prompt = (
            "A validator rejected the proposed analysis below. Correct only the structural "
            "or validation defects it names; preserve the semantic judgments unless the error "
            "requires otherwise. Return only the complete corrected analysis JSON object.\n\n"
            "Validator diagnostic:\n" + validation.stderr + "\n\n"
            "Exact required template:\n" + template.stdout + "\n\n"
            "Rejected analysis:\n" + json.dumps(analysis, ensure_ascii=False)
        )
        repair = platform_call(api_key, args.model, repair_prompt, 4096)
        analysis = extract_object(repair["text"])
        repair_path = args.run_dir / "proposed-analysis-repair-1.json"
        repair_path.write_text(json.dumps(analysis, ensure_ascii=False, indent=2) + "\n",
                               encoding="utf-8")
        validation = subprocess.run(
            ["python3", "scripts/session_turn_analysis.py", "complete",
             str(copied_record), str(repair_path)],
            cwd="/home/joe/code/futon3c", text=True, capture_output=True, timeout=120,
        )
        (args.run_dir / "repair-response.txt").write_text(repair["text"], encoding="utf-8")
    receipt = {
        "schema": "xiang-platform-agent-replay.v1",
        "source_uuid": case["uuid"], "source_timestamp": case["timestamp"],
        "record": copied_record.name, "model": args.model,
        "claude_exit": proc.returncode, "validation_exit": validation.returncode,
        "validation_stdout": validation.stdout, "validation_stderr": validation.stderr,
        "cost_usd": ((envelope.get("total_cost_usd") or 0)
                     + (price(args.model, repair["usage"]) if repair else 0)),
        "initial_cost_usd": envelope.get("total_cost_usd"), "usage": envelope.get("usage"),
        "repair": ({"usage": repair["usage"], "request_id": repair["request_id"],
                    "cost_usd": price(args.model, repair["usage"])} if repair else None),
        "duration_ms": envelope.get("duration_ms"), "num_turns": envelope.get("num_turns"),
    }
    if live_analysis:
        shutil.copy2(live_analysis, args.run_dir / "historical-analysis.json")
    (args.run_dir / "receipt.json").write_text(
        json.dumps(receipt, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")
    print(json.dumps(receipt, ensure_ascii=False))
    return 0 if validation.returncode == 0 else 2


if __name__ == "__main__":
    raise SystemExit(main())
