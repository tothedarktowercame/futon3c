#!/usr/bin/env python3
"""Append-only APM workflow ledger and dry-run state machine.

H1 deliberately contains no transport implementation.  Later handoffs inject
the dispatch, poll, and gate functions declared at the end of this module.
"""

from __future__ import annotations

import argparse
import copy
import datetime as dt
import json
import os
from pathlib import Path
from typing import Any, Iterable, Mapping


TRANSITIONS = frozenset(
    {
        "select",
        "dispatch-a",
        "dispatch-b",
        "poll",
        "gate",
        "review-request",
        "verdict",
        "closer-hop",
        "scribe",
        "promotion-queued",
        "capability-update",
        "chain-close",
    }
)
TERMINAL_POLL_STATUSES = frozenset({"done", "failed", "cancelled", "timed-out"})
POLLABLE_STATES = frozenset({"DISPATCH_A", "DISPATCH_B", "CLOSER_HOP", "SCRIBE"})
TERMINAL_STATES = frozenset({"DONE"})
MAX_CLOSER_HOPS = 3
REVIEW_STALE_SECONDS = 12 * 60 * 60
DEFAULT_LEDGER = Path(__file__).with_name("ledger.jsonl")


class LedgerError(ValueError):
    """Raised when a record would violate the ledger or workflow contract."""


def utc_now() -> str:
    """Return a UTC RFC 3339 timestamp suitable for a ledger record."""

    return dt.datetime.now(dt.timezone.utc).isoformat().replace("+00:00", "Z")


def make_record(
    chain_id: str,
    problem_id: str,
    transition: str,
    payload: Mapping[str, Any] | None = None,
    *,
    at: str | None = None,
) -> dict[str, Any]:
    """Construct, but do not append, one canonical transition record."""

    return {
        "chain-id": chain_id,
        "problem-id": problem_id,
        "transition": transition,
        "at": at or utc_now(),
        "payload": dict(payload or {}),
    }


def _require(payload: Mapping[str, Any], field: str, transition: str) -> Any:
    value = payload.get(field)
    if value is None or value == "":
        raise LedgerError(f"{transition} requires payload.{field}")
    return value


def _validate_record_shape(record: Mapping[str, Any]) -> None:
    required = {"chain-id", "problem-id", "transition", "at", "payload"}
    if set(record) != required:
        raise LedgerError(
            f"record keys must be exactly {sorted(required)}; got {sorted(record)}"
        )
    for field in ("chain-id", "problem-id", "transition", "at"):
        if not isinstance(record[field], str) or not record[field]:
            raise LedgerError(f"record.{field} must be a non-empty string")
    if record["transition"] not in TRANSITIONS:
        raise LedgerError(f"unknown transition: {record['transition']}")
    if not isinstance(record["payload"], dict):
        raise LedgerError("record.payload must be an object")
    try:
        _parse_time(record["at"])
    except ValueError as exc:
        raise LedgerError(f"record.at is not an RFC 3339 timestamp: {record['at']}") from exc


def _new_chain(record: Mapping[str, Any]) -> dict[str, Any]:
    if record["transition"] != "select":
        raise LedgerError(
            f"chain {record['chain-id']} must begin with select, not "
            f"{record['transition']}"
        )
    return {
        "chain-id": record["chain-id"],
        "problem-id": record["problem-id"],
        "state": "SELECT",
        "hops": 0,
        "statement-hash": None,
        "waiting-on": "dispatch-a",
        "started-at": record["at"],
        "updated-at": record["at"],
        "last-transition": "select",
        "job-id": None,
        "last-poll-status": None,
        "review-origin": None,
        "review-checkpoint": None,
        "review-resume-state": None,
        "fidelity-approved": False,
        "outcome": None,
    }


def _require_state(chain: Mapping[str, Any], transition: str, *states: str) -> None:
    if chain["state"] not in states:
        expected = ", ".join(states)
        raise LedgerError(
            f"illegal {transition} from {chain['state']} "
            f"(expected one of: {expected})"
        )


def _require_completed_poll(chain: Mapping[str, Any], transition: str) -> None:
    if chain["last-poll-status"] != "done":
        raise LedgerError(
            f"{transition} from {chain['state']} requires a done poll; "
            f"last status is {chain['last-poll-status']!r}"
        )


def _apply_transition(chain: Mapping[str, Any], record: Mapping[str, Any]) -> dict[str, Any]:
    result = copy.deepcopy(dict(chain))
    transition = record["transition"]
    payload = record["payload"]

    if record["problem-id"] != result["problem-id"]:
        raise LedgerError(
            f"chain {result['chain-id']} problem changed from "
            f"{result['problem-id']} to {record['problem-id']}"
        )
    if _parse_time(record["at"]) < _parse_time(result["updated-at"]):
        raise LedgerError(
            f"chain {result['chain-id']} timestamp moved backwards at {transition}"
        )
    if transition == "select":
        raise LedgerError(f"chain {result['chain-id']} may only be selected once")

    if transition == "dispatch-a":
        _require_state(result, transition, "SELECT")
        result.update(
            {
                "state": "DISPATCH_A",
                "job-id": _require(payload, "job-id", transition),
                "last-poll-status": None,
                "waiting-on": "phase-a-job",
            }
        )

    elif transition == "dispatch-b":
        _require_state(result, transition, "DISPATCH_A")
        _require_completed_poll(result, transition)
        result.update(
            {
                "state": "DISPATCH_B",
                "job-id": _require(payload, "job-id", transition),
                "last-poll-status": None,
                "waiting-on": "phase-b-job",
            }
        )

    elif transition == "poll":
        _require_state(result, transition, *sorted(POLLABLE_STATES))
        job_id = _require(payload, "job-id", transition)
        if job_id != result["job-id"]:
            raise LedgerError(
                f"poll job-id {job_id!r} does not match active job {result['job-id']!r}"
            )
        status = _require(payload, "status", transition)
        result["last-poll-status"] = status
        result["waiting-on"] = (
            "mechanical-gate"
            if status in TERMINAL_POLL_STATUSES and result["state"] != "SCRIBE"
            else "promotion-queue"
            if status == "done" and result["state"] == "SCRIBE"
            else result["waiting-on"]
        )

    elif transition == "gate":
        _require_state(result, transition, "DISPATCH_B", "CLOSER_HOP", "GATE")
        if result["state"] != "GATE" and result["last-poll-status"] not in TERMINAL_POLL_STATUSES:
            raise LedgerError("gate requires a terminal solver poll")
        outcome = _require(payload, "outcome", transition)
        if outcome not in {"closed", "partial", "defective"}:
            raise LedgerError(f"unsupported gate outcome: {outcome!r}")
        statement_hash = _require(payload, "statement-hash", transition)
        gate_results = _require(payload, "gate-results", transition)
        if not isinstance(gate_results, dict):
            raise LedgerError("gate payload.gate-results must be an object")
        if outcome == "partial" and gate_results.get("boundary-conforming") is not True:
            raise LedgerError("partial gate outcome requires a conforming boundary")
        frozen = result["statement-hash"]
        if frozen is None:
            result["statement-hash"] = statement_hash
        mismatch = frozen is not None and frozen != statement_hash
        if mismatch:
            result.update(
                {
                    "state": "VOID",
                    "outcome": "statement-altered",
                    "waiting-on": "review-request",
                    "job-id": None,
                    "last-poll-status": None,
                }
            )
        elif outcome == "closed":
            result.update(
                {
                    "state": "CLOSED",
                    "outcome": "closed",
                    "waiting-on": "review-request",
                    "job-id": None,
                    "last-poll-status": None,
                }
            )
        elif outcome == "defective":
            result.update(
                {
                    "state": "DEFECTIVE",
                    "outcome": "defective",
                    "waiting-on": "review-request",
                    "job-id": None,
                    "last-poll-status": None,
                }
            )
        elif result["hops"] >= MAX_CLOSER_HOPS:
            result.update(
                {
                    "state": "OPEN_HOLE",
                    "outcome": "open-hole",
                    "waiting-on": "capability-update",
                    "job-id": None,
                    "last-poll-status": None,
                }
            )
        else:
            result.update(
                {
                    "state": "PARTIAL",
                    "outcome": "partial-conforming",
                    "waiting-on": "closer-hop",
                    "job-id": None,
                    "last-poll-status": None,
                }
            )

    elif transition == "review-request":
        _require_state(result, transition, "CLOSED", "DEFECTIVE", "VOID")
        checkpoint = _require(payload, "checkpoint", transition)
        origin = result["state"]
        if origin == "CLOSED":
            if checkpoint != "fidelity":
                raise LedgerError("closed chain requires fidelity checkpoint")
            resume_state = None
        else:
            if checkpoint != "anomaly":
                raise LedgerError("defective/void chain requires anomaly checkpoint")
            resume_state = _require(payload, "resume-state", transition).upper().replace("-", "_")
            if resume_state not in {"GATE", "PARTIAL", "CLOSED"}:
                raise LedgerError(f"invalid anomaly resume-state: {resume_state!r}")
        result.update(
            {
                "state": "AWAITING_REVIEW",
                "review-origin": origin,
                "review-checkpoint": checkpoint,
                "review-resume-state": resume_state,
                "waiting-on": f"review:{checkpoint}",
            }
        )

    elif transition == "verdict":
        _require_state(result, transition, "AWAITING_REVIEW")
        verdict = _require(payload, "verdict", transition)
        origin = result["review-origin"]
        checkpoint = result["review-checkpoint"]
        if checkpoint == "fidelity" and origin == "CLOSED":
            if verdict == "approve":
                result.update(
                    {
                        "state": "CLOSED",
                        "fidelity-approved": True,
                        "waiting-on": "scribe",
                        "review-origin": None,
                        "review-checkpoint": None,
                        "review-resume-state": None,
                    }
                )
            elif verdict == "reject":
                result.update(
                    {
                        "state": "DONE",
                        "outcome": "fidelity-rejected",
                        "waiting-on": None,
                        "review-origin": None,
                        "review-checkpoint": None,
                        "review-resume-state": None,
                    }
                )
            else:
                raise LedgerError(f"verdict {verdict!r} is invalid for fidelity review")
        elif checkpoint == "anomaly" and origin in {"DEFECTIVE", "VOID"} and verdict == "resume":
            resume_state = _require(payload, "resume-state", transition).upper().replace("-", "_")
            if resume_state not in {"GATE", "PARTIAL", "CLOSED"}:
                raise LedgerError(f"invalid resume-state: {resume_state!r}")
            if resume_state != result["review-resume-state"]:
                raise LedgerError(
                    f"resume-state {resume_state!r} does not match requested "
                    f"state {result['review-resume-state']!r}"
                )
            result.update(
                {
                    "state": resume_state,
                    "outcome": None,
                    "waiting-on": {
                    "GATE": "mechanical-gate",
                    "PARTIAL": "closer-hop",
                    "CLOSED": "review-request",
                }[resume_state],
                    "review-origin": None,
                    "review-checkpoint": None,
                    "review-resume-state": None,
                }
            )
        elif checkpoint == "anomaly" and origin in {"DEFECTIVE", "VOID"} and verdict == "abandon":
            result.update(
                {
                    "state": "DONE",
                    "outcome": "abandoned",
                    "waiting-on": None,
                    "review-origin": None,
                    "review-checkpoint": None,
                    "review-resume-state": None,
                }
            )
        else:
            raise LedgerError(f"verdict {verdict!r} is invalid for review origin {origin}")

    elif transition == "closer-hop":
        _require_state(result, transition, "PARTIAL")
        if result["hops"] >= MAX_CLOSER_HOPS:
            raise LedgerError("closer-hop limit exhausted")
        expected_hop = result["hops"] + 1
        supplied_hop = _require(payload, "hop", transition)
        if supplied_hop != expected_hop:
            raise LedgerError(f"expected closer hop {expected_hop}, got {supplied_hop}")
        supplied_hash = _require(payload, "statement-hash", transition)
        if supplied_hash != result["statement-hash"]:
            raise LedgerError("closer-hop must carry the frozen statement hash")
        result.update(
            {
                "state": "CLOSER_HOP",
                "hops": expected_hop,
                "job-id": _require(payload, "job-id", transition),
                "last-poll-status": None,
                "waiting-on": f"closer-job:{expected_hop}",
            }
        )

    elif transition == "scribe":
        _require_state(result, transition, "CLOSED")
        if not result["fidelity-approved"]:
            raise LedgerError("scribe requires an approved fidelity review")
        result.update(
            {
                "state": "SCRIBE",
                "job-id": _require(payload, "job-id", transition),
                "last-poll-status": None,
                "waiting-on": "scribe-job",
            }
        )

    elif transition == "promotion-queued":
        _require_state(result, transition, "SCRIBE")
        _require_completed_poll(result, transition)
        result.update(
            {
                "state": "PROMOTION_QUEUE",
                "waiting-on": "capability-update",
                "job-id": None,
                "last-poll-status": None,
            }
        )

    elif transition == "capability-update":
        _require_state(result, transition, "PROMOTION_QUEUE", "OPEN_HOLE")
        result.update({"state": "UPDATE", "waiting-on": "chain-close"})

    elif transition == "chain-close":
        _require_state(result, transition, "UPDATE")
        outcome = _require(payload, "outcome", transition)
        if result["outcome"] is not None and outcome != result["outcome"]:
            raise LedgerError(
                f"chain-close outcome {outcome!r} does not match derived "
                f"outcome {result['outcome']!r}"
            )
        result.update(
            {
                "state": "DONE",
                "outcome": outcome,
                "waiting-on": None,
            }
        )

    else:  # pragma: no cover - shape validation has already rejected this.
        raise LedgerError(f"unhandled transition: {transition}")

    result["updated-at"] = record["at"]
    result["last-transition"] = transition
    return result


def fold_ledger(records: Iterable[Mapping[str, Any]]) -> dict[str, dict[str, Any]]:
    """Purely derive every chain state from ordered ledger records."""

    chains: dict[str, dict[str, Any]] = {}
    for input_record in records:
        record = copy.deepcopy(dict(input_record))
        _validate_record_shape(record)
        chain_id = record["chain-id"]
        if chain_id not in chains:
            chains[chain_id] = _new_chain(record)
        else:
            chains[chain_id] = _apply_transition(chains[chain_id], record)
    return chains


def read_ledger(path: os.PathLike[str] | str = DEFAULT_LEDGER) -> list[dict[str, Any]]:
    """Read JSONL records without modifying the ledger."""

    ledger_path = Path(path)
    if not ledger_path.exists():
        return []
    records = []
    with ledger_path.open("r", encoding="utf-8") as stream:
        for line_number, line in enumerate(stream, 1):
            if not line.strip():
                continue
            try:
                record = json.loads(line)
            except json.JSONDecodeError as exc:
                raise LedgerError(f"invalid JSON at {ledger_path}:{line_number}") from exc
            records.append(record)
    return records


def append_transition(
    path: os.PathLike[str] | str,
    record: Mapping[str, Any],
) -> dict[str, Any]:
    """Validate against folded history, then append exactly one JSONL record.

    The file is opened with ``O_APPEND`` and is never truncated or rewritten.
    """

    canonical = copy.deepcopy(dict(record))
    records = read_ledger(path)
    fold_ledger([*records, canonical])  # Validate before opening for append.
    encoded = (
        json.dumps(canonical, sort_keys=True, separators=(",", ":")) + "\n"
    ).encode("utf-8")
    ledger_path = Path(path)
    ledger_path.parent.mkdir(parents=True, exist_ok=True)
    descriptor = os.open(ledger_path, os.O_APPEND | os.O_CREAT | os.O_WRONLY, 0o644)
    try:
        written = os.write(descriptor, encoded)
        if written != len(encoded):
            raise OSError(f"short append: wrote {written} of {len(encoded)} bytes")
        os.fsync(descriptor)
    finally:
        os.close(descriptor)
    return canonical


def _parse_time(value: str) -> dt.datetime:
    parsed = dt.datetime.fromisoformat(value.replace("Z", "+00:00"))
    if parsed.tzinfo is None:
        raise ValueError("timestamp has no timezone")
    return parsed.astimezone(dt.timezone.utc)


def _format_age(seconds: int) -> str:
    hours, remainder = divmod(seconds, 3600)
    minutes = remainder // 60
    return f"{hours}h {minutes}m"


def active_status(
    chains: Mapping[str, Mapping[str, Any]],
    *,
    now: dt.datetime | None = None,
) -> list[dict[str, Any]]:
    """Return deterministic status rows for nonterminal chains."""

    current = now or dt.datetime.now(dt.timezone.utc)
    rows = []
    for chain_id in sorted(chains):
        chain = chains[chain_id]
        if chain["state"] in TERMINAL_STATES:
            continue
        started = _parse_time(chain["started-at"])
        updated = _parse_time(chain["updated-at"])
        state_age = max(0, int((current - updated).total_seconds()))
        stale_review = chain["state"] == "AWAITING_REVIEW" and state_age > REVIEW_STALE_SECONDS
        review_status = (
            f"STALE-REVIEW ({_format_age(state_age)})"
            if stale_review
            else chain["state"]
        )
        rows.append(
            {
                "chain-id": chain_id,
                "problem-id": chain["problem-id"],
                "state": chain["state"],
                "status": review_status,
                "review-stale": stale_review,
                "hops": chain["hops"],
                "waiting-on": chain["waiting-on"],
                "age-seconds": max(0, int((current - started).total_seconds())),
                "state-age-seconds": state_age,
                "statement-hash": chain["statement-hash"],
            }
        )
    return rows


def render_status(rows: Iterable[Mapping[str, Any]]) -> str:
    """Render active-chain status as a compact fixed-column table."""

    materialized = list(rows)
    if not materialized:
        return "No active chains."
    headers = ("CHAIN", "PROBLEM", "STATE", "HOPS", "WAITING-ON", "AGE", "STATE-AGE")
    values = [
        (
            row["chain-id"],
            row["problem-id"],
            row.get("status", row["state"]),
            str(row["hops"]),
            row["waiting-on"] or "-",
            f"{row['age-seconds']}s",
            f"{row['state-age-seconds']}s",
        )
        for row in materialized
    ]
    widths = [max(len(headers[i]), *(len(row[i]) for row in values)) for i in range(len(headers))]
    format_row = lambda row: "  ".join(value.ljust(widths[i]) for i, value in enumerate(row))
    return "\n".join([format_row(headers), format_row(tuple("-" * width for width in widths)), *(format_row(row) for row in values)])


def dispatch_fn(*_args: Any, **_kwargs: Any) -> Mapping[str, Any]:
    """H2 injection point: dispatch a packet and return its job metadata."""

    raise NotImplementedError("H1 is dry-run only; inject dispatch_fn in H2")


def poll_fn(*_args: Any, **_kwargs: Any) -> Mapping[str, Any]:
    """H2 injection point: poll an existing job without changing state itself."""

    raise NotImplementedError("H1 is dry-run only; inject poll_fn in H2")


def gate_fn(*_args: Any, **_kwargs: Any) -> Mapping[str, Any]:
    """H3 injection point: run mechanical gates and return a gate payload."""

    raise NotImplementedError("H1 is dry-run only; inject gate_fn in H3")


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--ledger", type=Path, default=DEFAULT_LEDGER)
    subparsers = parser.add_subparsers(dest="command", required=True)
    status_parser = subparsers.add_parser("status", help="render active chains")
    status_parser.add_argument("--json", action="store_true", dest="as_json")
    args = parser.parse_args(argv)

    chains = fold_ledger(read_ledger(args.ledger))
    rows = active_status(chains)
    if args.as_json:
        print(json.dumps(rows, indent=2, sort_keys=True))
    else:
        print(render_status(rows))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
