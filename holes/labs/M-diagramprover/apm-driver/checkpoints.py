#!/usr/bin/env python3
"""Claude review checkpoints and the nonblocking promotion approval queue."""

from __future__ import annotations

import datetime as dt
import json
import os
import re
from pathlib import Path
from typing import Any, Callable, Mapping

import agency
import driver


CHECKPOINT_KINDS = frozenset({"fidelity", "anomaly"})
BASE_DIR = Path(__file__).resolve().parent
DEFAULT_VERDICTS_DIR = BASE_DIR / "verdicts"
DEFAULT_PROMOTION_QUEUE = BASE_DIR / "promotion-queue.jsonl"
REVIEW_TIMEOUT_SECONDS = 12 * 60 * 60
SAFE_COMPONENT_RE = re.compile(r"^[A-Za-z0-9._-]+$")


class CheckpointError(ValueError):
    """Malformed checkpoint input or verdict file."""


def _iso(value: dt.datetime) -> str:
    if value.tzinfo is None:
        raise CheckpointError("checkpoint timestamp must be timezone-aware")
    return value.astimezone(dt.timezone.utc).isoformat().replace("+00:00", "Z")


def _parse_iso(value: str) -> dt.datetime:
    try:
        parsed = dt.datetime.fromisoformat(value.replace("Z", "+00:00"))
    except (TypeError, ValueError) as exc:
        raise CheckpointError("verdict.at must be an ISO-8601 timestamp") from exc
    if parsed.tzinfo is None:
        raise CheckpointError("verdict.at must include a timezone")
    return parsed.astimezone(dt.timezone.utc)


def verdict_path(
    chain_id: str,
    checkpoint: str,
    *,
    verdicts_dir: Path = DEFAULT_VERDICTS_DIR,
) -> Path:
    """Return the canonical, traversal-safe verdict path."""

    if not SAFE_COMPONENT_RE.fullmatch(chain_id):
        raise CheckpointError(f"unsafe chain id: {chain_id!r}")
    if checkpoint not in CHECKPOINT_KINDS:
        raise CheckpointError(f"unknown checkpoint kind: {checkpoint!r}")
    return verdicts_dir / f"{chain_id}-{checkpoint}.edn"


def compose_bell_body(
    *,
    chain_id: str,
    problem_id: str,
    checkpoint: str,
    lean_statement: str,
    informal_paths: list[str],
    gate_results: Mapping[str, Any],
    verdict_file: Path,
    resume_state: str | None = None,
) -> str:
    """Compose a self-contained checkpoint request for claude-10."""

    if checkpoint not in CHECKPOINT_KINDS:
        raise CheckpointError(f"unknown checkpoint kind: {checkpoint!r}")
    allowed = "approve | reject" if checkpoint == "fidelity" else "resume | abandon"
    path_lines = "\n".join(f"- {path}" for path in informal_paths)
    resume_line = (
        f"\nPredetermined resume state on `resume`: `{resume_state}`."
        if checkpoint == "anomaly"
        else ""
    )
    return f"""CLAUDE CHECKPOINT REQUEST

Chain ID: {chain_id}
Problem ID: {problem_id}
Checkpoint kind: {checkpoint}{resume_line}

Lean statement (verbatim):
```lean
{lean_statement}
```

Informal source paths:
{path_lines}

Mechanical gate results:
```json
{json.dumps(dict(gate_results), indent=2, sort_keys=True)}
```

Verdict-file instructions:
Write exactly one JSON or EDN map to:
`{verdict_file}`

Required schema:
`{{verdict, reviewer, at, notes}}`

- `verdict`: {allowed}
- `reviewer`: non-empty string
- `at`: timezone-qualified ISO-8601 timestamp
- `notes`: string

JSON and EDN are both accepted. Do not add or omit fields. A malformed file
becomes an explicit checkpoint error and is never interpreted as a verdict.
"""


def request_checkpoint(
    *,
    chain_id: str,
    problem_id: str,
    checkpoint: str,
    lean_statement: str,
    informal_paths: list[str],
    gate_results: Mapping[str, Any],
    ledger_path: Path,
    verdicts_dir: Path = DEFAULT_VERDICTS_DIR,
    dispatch: Callable[..., Mapping[str, Any]] = agency.dispatch_fn,
    resume_state: str | None = None,
    now: dt.datetime | None = None,
) -> dict[str, Any]:
    """Bell claude-10 and append the corresponding review-request transition."""

    if checkpoint == "anomaly" and not resume_state:
        raise CheckpointError("anomaly checkpoint requires a predetermined resume_state")
    if checkpoint == "fidelity" and resume_state is not None:
        raise CheckpointError("fidelity checkpoint must not carry resume_state")
    requested_at = _iso(now or dt.datetime.now(dt.timezone.utc))
    path = verdict_path(chain_id, checkpoint, verdicts_dir=verdicts_dir)
    body = compose_bell_body(
        chain_id=chain_id,
        problem_id=problem_id,
        checkpoint=checkpoint,
        lean_statement=lean_statement,
        informal_paths=informal_paths,
        gate_results=gate_results,
        verdict_file=path,
        resume_state=resume_state,
    )
    dispatch_result = dict(dispatch("claude-10", body))
    job_id = dispatch_result.get("job-id")
    if not job_id:
        raise CheckpointError(f"checkpoint dispatch returned no job-id: {dispatch_result!r}")
    payload = {
        "checkpoint": checkpoint,
        "job-id": str(job_id),
        "verdict-file": str(path),
        "requested-at": requested_at,
        "bell-request": dispatch_result.get("request"),
    }
    if resume_state is not None:
        payload["resume-state"] = resume_state
    record = driver.make_record(
        chain_id,
        problem_id,
        "review-request",
        payload,
        at=requested_at,
    )
    driver.append_transition(ledger_path, record)
    return {
        "job-id": str(job_id),
        "bell-body": body,
        "verdict-file": str(path),
        "ledger-record": record,
    }


class _EdnParser:
    """Parser for the deliberately tiny flat-map verdict EDN schema."""

    def __init__(self, text: str):
        self.text = text
        self.index = 0

    def _space(self) -> None:
        while self.index < len(self.text) and (
            self.text[self.index].isspace() or self.text[self.index] == ","
        ):
            self.index += 1

    def _token(self) -> str:
        self._space()
        if self.index >= len(self.text):
            raise CheckpointError("unexpected end of EDN verdict")
        if self.text[self.index] == '"':
            start = self.index
            self.index += 1
            escaped = False
            while self.index < len(self.text):
                char = self.text[self.index]
                self.index += 1
                if escaped:
                    escaped = False
                elif char == "\\":
                    escaped = True
                elif char == '"':
                    try:
                        return json.loads(self.text[start : self.index])
                    except json.JSONDecodeError as exc:
                        raise CheckpointError("invalid EDN string escape") from exc
            raise CheckpointError("unterminated EDN string")
        start = self.index
        while self.index < len(self.text) and not (
            self.text[self.index].isspace() or self.text[self.index] in ",{}"
        ):
            self.index += 1
        token = self.text[start : self.index]
        if not token:
            raise CheckpointError("empty EDN token")
        return token[1:] if token.startswith(":") else token

    def parse(self) -> dict[str, Any]:
        self._space()
        if self.index >= len(self.text) or self.text[self.index] != "{":
            raise CheckpointError("EDN verdict must be a map")
        self.index += 1
        result = {}
        while True:
            self._space()
            if self.index < len(self.text) and self.text[self.index] == "}":
                self.index += 1
                break
            key = self._token()
            value = self._token()
            if key in result:
                raise CheckpointError(f"duplicate EDN verdict key: {key}")
            result[key] = value
        self._space()
        if self.index != len(self.text):
            raise CheckpointError("trailing content after EDN verdict")
        return result


def parse_verdict_file(path: Path, checkpoint: str) -> dict[str, str]:
    """Parse and validate one JSON or EDN verdict file defensively."""

    try:
        text = path.read_text(encoding="utf-8")
    except OSError as exc:
        raise CheckpointError(f"could not read verdict file: {exc}") from exc
    try:
        value = json.loads(text)
    except json.JSONDecodeError:
        value = _EdnParser(text).parse()
    if not isinstance(value, dict):
        raise CheckpointError("verdict file must contain a map/object")
    expected = {"verdict", "reviewer", "at", "notes"}
    if set(value) != expected:
        raise CheckpointError(
            f"verdict keys must be exactly {sorted(expected)}; got {sorted(value)}"
        )
    verdict = value["verdict"]
    if isinstance(verdict, str):
        verdict = verdict.removeprefix(":")
    allowed = {"approve", "reject"} if checkpoint == "fidelity" else {"resume", "abandon"}
    if verdict not in allowed:
        raise CheckpointError(
            f"verdict {verdict!r} is invalid for {checkpoint}; expected {sorted(allowed)}"
        )
    if not isinstance(value["reviewer"], str) or not value["reviewer"].strip():
        raise CheckpointError("verdict.reviewer must be a non-empty string")
    if not isinstance(value["at"], str):
        raise CheckpointError("verdict.at must be a string")
    _parse_iso(value["at"])
    if not isinstance(value["notes"], str):
        raise CheckpointError("verdict.notes must be a string")
    return {
        "verdict": verdict,
        "reviewer": value["reviewer"],
        "at": value["at"],
        "notes": value["notes"],
    }


def watch_verdict(
    *,
    chain_id: str,
    checkpoint: str,
    ledger_path: Path,
    verdicts_dir: Path = DEFAULT_VERDICTS_DIR,
    now: dt.datetime | None = None,
) -> dict[str, Any]:
    """Apply a valid verdict, or report awaiting/stale/error without fabrication."""

    current = now or dt.datetime.now(dt.timezone.utc)
    chains = driver.fold_ledger(driver.read_ledger(ledger_path))
    chain = chains.get(chain_id)
    if chain is None:
        raise CheckpointError(f"unknown chain: {chain_id}")
    if chain["state"] != "AWAITING_REVIEW" or chain["review-checkpoint"] != checkpoint:
        raise CheckpointError(
            f"chain {chain_id} is not awaiting {checkpoint} review"
        )
    path = verdict_path(chain_id, checkpoint, verdicts_dir=verdicts_dir)
    requested_at = driver._parse_time(chain["updated-at"])
    age_seconds = max(0, int((current.astimezone(dt.timezone.utc) - requested_at).total_seconds()))
    if not path.exists():
        return {
            "status": "awaiting-review",
            "stale": age_seconds > REVIEW_TIMEOUT_SECONDS,
            "age-seconds": age_seconds,
            "verdict-file": str(path),
        }
    try:
        verdict = parse_verdict_file(path, checkpoint)
    except CheckpointError as exc:
        return {
            "status": "error",
            "error": str(exc),
            "verdict-file": str(path),
        }
    payload: dict[str, Any] = dict(verdict)
    payload["verdict-file"] = str(path)
    if checkpoint == "anomaly" and verdict["verdict"] == "resume":
        payload["resume-state"] = chain["review-resume-state"]
    record = driver.make_record(
        chain_id,
        chain["problem-id"],
        "verdict",
        payload,
        at=_iso(current),
    )
    driver.append_transition(ledger_path, record)
    resulting = driver.fold_ledger(driver.read_ledger(ledger_path))[chain_id]
    return {
        "status": "applied",
        "verdict": verdict,
        "state": resulting["state"],
        "outcome": resulting["outcome"],
        "ledger-record": record,
    }


def queue_promotion_approval(
    *,
    chain_id: str,
    memory_ids: list[str],
    approvals_file: str,
    queue_path: Path = DEFAULT_PROMOTION_QUEUE,
    now: dt.datetime | None = None,
) -> dict[str, Any]:
    """Append one nonblocking promotion approval item outside chain state."""

    if not chain_id or not memory_ids or not all(isinstance(item, str) and item for item in memory_ids):
        raise CheckpointError("promotion queue requires chain-id and non-empty memory-ids")
    if not approvals_file:
        raise CheckpointError("promotion queue requires approvals-file")
    record = {
        "chain-id": chain_id,
        "memory-ids": list(memory_ids),
        "approvals-file": approvals_file,
        "queued-at": _iso(now or dt.datetime.now(dt.timezone.utc)),
    }
    encoded = (json.dumps(record, sort_keys=True, separators=(",", ":")) + "\n").encode("utf-8")
    queue_path.parent.mkdir(parents=True, exist_ok=True)
    descriptor = os.open(queue_path, os.O_APPEND | os.O_CREAT | os.O_WRONLY, 0o644)
    try:
        written = os.write(descriptor, encoded)
        if written != len(encoded):
            raise OSError(f"short promotion queue append: {written}/{len(encoded)}")
        os.fsync(descriptor)
    finally:
        os.close(descriptor)
    return record
