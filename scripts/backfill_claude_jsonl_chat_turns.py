#!/usr/bin/env python3
"""
Backfill missing Claude REPL chat-turn evidence from a Claude Code JSONL session.

Default mode is dry-run. Use --apply to append missing entries through
Drawbridge inside the live futon3c process, preserving original timestamps.
"""

from __future__ import annotations

import argparse
import base64
import collections
import hashlib
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from pathlib import Path
from typing import Iterable


CURRENT_TURN_RE = re.compile(
    r"^--- CURRENT TURN ---\nSurface: ([^\n]+)\n"
    r"(?:(?:From|To|Origin): [^\n]*\n)*"
    r"Caller: ([^\n]+)\n---\n",
    re.S,
)
USER_MESSAGE_RE = re.compile(r"\nUser message:\n(.*)\Z", re.S)
AGENT_LINE_RE = re.compile(r"\nAgent: ([^\n]+)")
SURFACE_AGENT_RE = re.compile(r"Current surface: emacs-[^\n]* \(agent ([^)]+)\)")


@dataclass
class Turn:
    index: int
    timestamp: str
    role: str
    author: str
    text: str
    agent_id: str | None
    surface: str


@dataclass
class ParseResult:
    turns: list[Turn]
    operator_turns: int
    parsed_operator_turns: int
    unparsed_operator_turns: int
    excluded_tool_results: int
    excluded_harness_records: int


def load_lines(path: Path) -> list[dict]:
    rows = []
    with path.open() as handle:
        for line in handle:
            try:
                rows.append(json.loads(line))
            except json.JSONDecodeError:
                continue
    return rows


def extract_assistant_text(content) -> str:
    if isinstance(content, str):
        return content.strip()
    if isinstance(content, list):
        parts = []
        for block in content:
            if isinstance(block, dict) and block.get("type") == "text":
                text = block.get("text", "")
                if text:
                    parts.append(text)
        return "\n".join(parts).strip()
    return ""


def has_tool_result(row: dict) -> bool:
    if row.get("toolUseResult"):
        return True
    content = row.get("message", {}).get("content")
    return isinstance(content, list) and any(
        isinstance(block, dict) and block.get("type") == "tool_result"
        for block in content
    )


def is_harness_record(text: str) -> bool:
    stripped = text.lstrip()
    return stripped.startswith(
        (
            "This session is being continued from a previous conversation",
            "<local-command-caveat>",
            "<command-name>",
            "<local-command-stdout>",
        )
    )


def infer_agent_id(message: str) -> str | None:
    match = AGENT_LINE_RE.search(message)
    if match:
        return match.group(1).strip()
    match = SURFACE_AGENT_RE.search(message)
    if match:
        return match.group(1).strip()
    return None


def parse_repl_turns(rows: Iterable[dict]) -> ParseResult:
    turns: list[Turn] = []
    current_context: dict | None = None
    turn_index = 0
    operator_turns = 0
    parsed_operator_turns = 0
    unparsed_operator_turns = 0
    excluded_tool_results = 0
    excluded_harness_records = 0

    for row in rows:
        row_type = row.get("type")
        if row_type == "user":
            content = row.get("message", {}).get("content", "")
            if has_tool_result(row):
                excluded_tool_results += 1
                continue
            content = extract_assistant_text(content)
            if is_harness_record(content):
                excluded_harness_records += 1
                current_context = None
                continue
            operator_turns += 1
            match = CURRENT_TURN_RE.search(content)
            if not match:
                unparsed_operator_turns += 1
                current_context = None
                continue
            surface = match.group(1).strip()
            caller = match.group(2).strip()
            agent_id = infer_agent_id(content)
            user_message = USER_MESSAGE_RE.search(content)
            if user_message:
                text = user_message.group(1).strip()
                turns.append(
                    Turn(
                        index=turn_index,
                        timestamp=row.get("timestamp", ""),
                        role="user",
                        author=caller,
                        text=text,
                        agent_id=agent_id,
                        surface=surface,
                    )
                )
                turn_index += 1
                parsed_operator_turns += 1
                current_context = {
                    "surface": surface,
                    "caller": caller,
                    "agent_id": agent_id,
                    "backfillable": True,
                }
            else:
                current_context = {
                    "surface": surface,
                    "caller": caller,
                    "agent_id": agent_id,
                        "backfillable": False,
                }
        elif row_type == "assistant":
            text = extract_assistant_text(row.get("message", {}).get("content"))
            if text and current_context and current_context.get("backfillable"):
                turns.append(
                    Turn(
                        index=turn_index,
                        timestamp=row.get("timestamp", ""),
                        role="assistant",
                        author=current_context.get("agent_id") or "claude-2",
                        text=text,
                        agent_id=current_context.get("agent_id"),
                        surface=current_context.get("surface") or "emacs-repl",
                    )
                )
                turn_index += 1
    return ParseResult(
        turns=turns,
        operator_turns=operator_turns,
        parsed_operator_turns=parsed_operator_turns,
        unparsed_operator_turns=unparsed_operator_turns,
        excluded_tool_results=excluded_tool_results,
        excluded_harness_records=excluded_harness_records,
    )


def http_json(url: str, timeout: float = 180.0, *, headers: dict[str, str] | None = None) -> dict:
    request = urllib.request.Request(url, headers=headers or {})
    with urllib.request.urlopen(request, timeout=timeout) as response:
        return json.loads(response.read().decode("utf-8"))


def fetch_session_entries(base_url: str, session_id: str) -> list[dict]:
    query_sid = urllib.parse.quote(session_id, safe="")
    count_payload = http_json(f"{base_url}/api/alpha/evidence/count?session-id={query_sid}")
    count = int(count_payload.get("count", 0))
    if count <= 0:
        return []
    payload = http_json(f"{base_url}/api/alpha/evidence?session-id={query_sid}&limit={count}")
    return payload.get("entries", [])


def parse_evidence_body(body) -> dict | None:
    if isinstance(body, dict):
        return body
    if isinstance(body, str):
        try:
            parsed = json.loads(body)
        except json.JSONDecodeError:
            return None
        return parsed if isinstance(parsed, dict) else None
    return None


def evidence_body_shapes(entries: Iterable[dict]) -> collections.Counter:
    shapes: collections.Counter = collections.Counter()
    for entry in entries:
        body = entry.get("evidence/body")
        if isinstance(body, dict):
            shapes["map"] += 1
        elif isinstance(body, str):
            try:
                parsed = json.loads(body)
            except json.JSONDecodeError:
                shapes["unparsed-string"] += 1
            else:
                shapes["json-string-map" if isinstance(parsed, dict) else "json-string-other"] += 1
        else:
            shapes[type(body).__name__] += 1
    return shapes


def build_existing_chat_queues(entries: Iterable[dict]) -> dict[tuple[str, str], collections.deque]:
    chat_entries = []
    for entry in entries:
        body = parse_evidence_body(entry.get("evidence/body"))
        if body and body.get("event") == "chat-turn":
            chat_entries.append((entry, body))
    chat_entries.sort(key=lambda pair: pair[0].get("evidence/at", ""))
    queues: dict[tuple[str, str], collections.deque] = collections.defaultdict(collections.deque)
    for entry, body in chat_entries:
        key = ((body.get("role") or "").strip(), (body.get("text") or "").strip())
        queues[key].append(entry)
    return queues


def stable_backfill_id(session_id: str, turn: Turn) -> str:
    digest = hashlib.sha1(
        f"{session_id}|{turn.index}|{turn.role}|{turn.timestamp}|{turn.text}".encode("utf-8")
    ).hexdigest()[:16]
    return f"e-backfill-chat-{session_id[:8]}-{turn.index:04d}-{digest}"


def claim_type_for(turn: Turn) -> str:
    if turn.role == "user":
        return "question"
    if turn.text.startswith("[Error"):
        return "correction"
    return "observation"


def build_missing_entries(session_id: str, turns: list[Turn], entries: list[dict]) -> list[dict]:
    queues = build_existing_chat_queues(entries)
    missing: list[dict] = []

    for turn in turns:
        key = (turn.role, turn.text)
        matched = None
        if queues[key]:
            matched = queues[key].popleft()
        if matched:
            continue

        evidence_id = stable_backfill_id(session_id, turn)
        entry = {
            "evidence/id": evidence_id,
            "evidence/subject": {"ref/type": "session", "ref/id": session_id},
            "evidence/type": "coordination",
            "evidence/claim-type": claim_type_for(turn),
            "evidence/author": turn.author,
            "evidence/at": turn.timestamp,
            "evidence/body": {
                "event": "chat-turn",
                "transport": "emacs-claude-repl",
                "role": turn.role,
                "text": turn.text,
            },
            "evidence/tags": ["claude", "chat", "turn", turn.role, "backfill"],
            "evidence/session-id": session_id,
        }
        missing.append(entry)

    return missing


def parse_drawbridge_response(payload: str) -> dict:
    text = payload.strip()
    if not text:
        return {"ok": False, "error": "empty-response"}
    if text.startswith("{"):
        try:
            return json.loads(text)
        except json.JSONDecodeError:
            pass
    if text.startswith("{:") or text.startswith("("):
        return {"ok": True, "value": text}
    return {"ok": False, "error": text}


def apply_backfill(drawbridge_url: str, admin_token: str, entries: list[dict]) -> dict:
    entries_json = json.dumps(entries, separators=(",", ":"), ensure_ascii=False)
    encoded = base64.b64encode(entries_json.encode("utf-8")).decode("ascii")
    form = f"""
(do
  (require '[cheshire.core :as json]
           '[futon3c.evidence.boundary :as boundary]
           '[futon3c.evidence.store :as store]
           '[futon3c.dev :as dev])
  (let [payload-str (String. (.decode (java.util.Base64/getDecoder) "{encoded}") "UTF-8")
        entries (json/parse-string payload-str true)
        evidence-store (or @dev/!evidence-store store/!store)
        results (mapv #(boundary/append! evidence-store %) entries)]
    {{:attempted (count entries)
     :ok-count (count (filter :ok results))
     :error-count (count (remove :ok results))
     :results results}}))
""".strip()
    data = form.encode("utf-8")
    request = urllib.request.Request(
        drawbridge_url,
        data=data,
        headers={
            "x-admin-token": admin_token,
            "Content-Type": "text/plain",
        },
        method="POST",
    )
    with urllib.request.urlopen(request, timeout=60) as response:
        return parse_drawbridge_response(response.read().decode("utf-8"))


def repo_root_from_script() -> Path:
    return Path(__file__).resolve().parents[1]


def load_admin_token(explicit: str | None) -> str:
    if explicit:
        return explicit
    token_path = repo_root_from_script() / ".admintoken"
    return token_path.read_text().strip()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("jsonl_path", type=Path)
    parser.add_argument("--session-id")
    parser.add_argument("--base-url", default="http://127.0.0.1:7070")
    parser.add_argument("--drawbridge-url", default="http://127.0.0.1:6768/eval")
    parser.add_argument("--admin-token")
    parser.add_argument("--apply", action="store_true")
    parser.add_argument("--show", type=int, default=10)
    args = parser.parse_args()

    session_id = args.session_id or args.jsonl_path.stem
    rows = load_lines(args.jsonl_path)
    parsed = parse_repl_turns(rows)
    if parsed.parsed_operator_turns == 0:
        print(
            "ERROR: parsed zero operator turns; refusing to report a clean empty backfill",
            file=sys.stderr,
        )
        return 2
    entries = fetch_session_entries(args.base_url, session_id)
    operator_chat_turns = [turn for turn in parsed.turns if turn.role == "user"]
    missing = build_missing_entries(session_id, operator_chat_turns, entries)
    shapes = evidence_body_shapes(entries)

    print(
        json.dumps(
            {
                "session_id": session_id,
                "operator_turns": parsed.operator_turns,
                "parsed_operator_turns": parsed.parsed_operator_turns,
                "unparsed_operator_turns": parsed.unparsed_operator_turns,
                "excluded_tool_results": parsed.excluded_tool_results,
                "excluded_harness_records": parsed.excluded_harness_records,
                "jsonl_chat_turns_both_roles": len(parsed.turns),
                "candidate_scope": "operator-user-turns-only",
                "existing_session_entries": len(entries),
                "evidence_body_shapes": dict(sorted(shapes.items())),
                "missing_chat_turns": len(missing),
            },
            indent=2,
        )
    )
    for entry in missing[: args.show]:
        body = entry["evidence/body"]
        preview = body["text"].replace("\n", " ")
        print(
            f"- {entry['evidence/at']} {body['role']} {entry['evidence/author']}: "
            f"{preview[:120]}"
        )

    if not args.apply:
        return 0

    if not missing:
        print("No missing chat-turn entries to backfill.")
        return 0

    token = load_admin_token(args.admin_token)
    result = apply_backfill(args.drawbridge_url, token, missing)
    print(json.dumps(result, indent=2, sort_keys=True))
    return 0


if __name__ == "__main__":
    sys.exit(main())
