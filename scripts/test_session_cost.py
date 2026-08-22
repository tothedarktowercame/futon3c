import importlib.util
import json
from datetime import datetime, timezone
from pathlib import Path


SCRIPT = Path(__file__).with_name("session-cost.py")
SPEC = importlib.util.spec_from_file_location("session_cost", SCRIPT)
session_cost = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(session_cost)


def record(timestamp, model="claude-opus-5", out=100, read=900, cc5=0, cc1h=0):
    return {
        "timestamp": timestamp,
        "message": {
            "model": model,
            "usage": {
                "input_tokens": 100,
                "cache_read_input_tokens": read,
                "cache_creation_input_tokens": cc5 + cc1h,
                "output_tokens": out,
                "cache_creation": {
                    "ephemeral_5m_input_tokens": cc5,
                    "ephemeral_1h_input_tokens": cc1h,
                },
            },
        },
    }


def write_jsonl(path, records):
    path.write_text("".join(json.dumps(r) + "\n" for r in records))


def test_tail_read_finds_last_usage_across_large_prefix(tmp_path):
    path = tmp_path / "s.jsonl"
    records = [{"noise": "x" * 2000} for _ in range(150)]
    records += [record("2026-08-22T14:00:00Z"), record("2026-08-22T14:01:00Z", read=1900)]
    write_jsonl(path, records)
    usages = session_cost.claude_usage_records(session_cost.tail_records(path))
    assert usages[-1]["message"]["usage"]["cache_read_input_tokens"] == 1900


def test_cold_and_warm_flags(tmp_path):
    path = tmp_path / "s.jsonl"
    write_jsonl(path, [record("2026-08-22T14:00:00Z", cc1h=1000)] * 5)
    warm = session_cost.claude_report(
        path, "s", fast=True,
        now=datetime(2026, 8, 22, 14, 30, tzinfo=timezone.utc),
    )
    cold = session_cost.claude_report(
        path, "s", fast=True,
        now=datetime(2026, 8, 22, 15, 1, tzinfo=timezone.utc),
    )
    assert warm["cold"] is False
    assert cold["cold"] is True


def test_model_multiplier_known_and_unknown():
    assert session_cost.model_multiplier("claude-fable-5")[:2] == (2.0, True)
    assert session_cost.model_multiplier("claude-sonnet-5")[:2] == (0.2, True)
    assert session_cost.model_multiplier("new-model")[:2] == (1.0, False)


def test_ttl_uses_predominant_write_class(tmp_path):
    path = tmp_path / "s.jsonl"
    five = [record("2026-08-22T14:00:00Z", cc5=1000) for _ in range(5)]
    write_jsonl(path, five)
    assert session_cost.claude_report(path, "s", fast=True)["ttl_s"] == 300
    one_hour = [record("2026-08-22T14:00:00Z", cc1h=1000) for _ in range(5)]
    write_jsonl(path, one_hour)
    assert session_cost.claude_report(path, "s", fast=True)["ttl_s"] == 3600
