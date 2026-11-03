#!/usr/bin/env python3
"""Fixture tests for the intents-are-tracked way-of-working probe."""
import datetime
import importlib.util
import pathlib
import sys
import unittest

SCRIPTS = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPTS))
SPEC = importlib.util.spec_from_file_location("ways_of_working_probe", SCRIPTS / "ways-of-working-probe.py")
PROBE = importlib.util.module_from_spec(SPEC); SPEC.loader.exec_module(PROBE)
UTC = datetime.timezone.utc
NOW = datetime.datetime(2026, 11, 3, 12, 0, tzinfo=UTC)

def record(session="s", turn="t", age_days=8, open_=True, closed=False, act="ask"):
    at = NOW - datetime.timedelta(days=age_days)
    opener = {"id": act, "author": "operator", "kind": "ask-action",
              "text": "please do the bounded work", "at": at.isoformat()}
    inferred = ([{"opener": act, "answer": "reply", "effect": "closes"}] if closed else [])
    return {"session_id": session, "turn_id": turn, "agent_id": "claude-5",
            "created_at": at.isoformat(), "acts": [opener],
            "ports": {"still_open": [opener] if open_ else [],
                      "closed_this_turn": [{"act": act}] if closed else [],
                      "inferred": inferred}}

def classify(records, latest=None, sidecars=(), jobs=None):
    latest = latest or {r["session_id"]: r for r in records}
    return PROBE.classify_intents(records, latest, list(sidecars), NOW,
                                  lambda job: (jobs or {}).get(job))

class IntentClassifierTest(unittest.TestCase):
    def test_kernel_closed_ask_is_not_open(self):
        row = record(open_=False, closed=True)
        result = classify([row])
        self.assertEqual(0, result["violation-count"])
        self.assertEqual(1, result["status-counts"]["answered"])

    def test_failed_bell_job_is_answered_not_met(self):
        aid = "invoke-1-2-acde-b-0"
        row = record(act=aid)
        result = classify([row], jobs={"invoke-1-2-acde": {"state": "failed"}})
        self.assertEqual(1, result["status-counts"]["answered"])
        self.assertNotIn("met", result["status-counts"])

    def test_old_is_violation_but_two_hours_is_pending(self):
        old = record(session="old", act="old")
        recent = record(session="new", act="new", age_days=2 / 24)
        result = classify([old, recent])
        self.assertEqual(["old"], [x["act-id"] for x in result["violations"]])
        self.assertEqual(["new"], [x["act-id"] for x in result["pending"]])

    def test_only_latest_session_record_controls_open_state(self):
        old = record(turn="old", open_=True)
        new = record(turn="new", age_days=7, open_=False, closed=True)
        result = classify([old, new], latest={"s": new})
        self.assertEqual(0, result["violation-count"])
        self.assertEqual(1, result["status-counts"]["answered"])

    def test_act_dropped_by_latest_record_is_not_counted(self):
        # The earlier record left "x" open; the session's latest record no
        # longer carries it and has no closure witness for it.  Reading all
        # records instead of the latest would count "x" as a stale open act.
        old = record(turn="old", act="x", age_days=9)
        new = record(turn="new", act="y", age_days=8, open_=False, closed=True)
        result = classify([old, new], latest={"s": new})
        self.assertEqual(0, result["violation-count"])
        self.assertNotIn("open", result["status-counts"])
        self.assertEqual(1, result["status-counts"]["answered"])

    def test_duplicate_act_counts_once(self):
        a = record(session="a", act="same")
        b = record(session="b", act="same")
        result = classify([a, b], latest={"a": a, "b": b})
        self.assertEqual(1, result["status-counts"]["open"])
        self.assertEqual(1, result["violation-count"])

if __name__ == "__main__": unittest.main()
