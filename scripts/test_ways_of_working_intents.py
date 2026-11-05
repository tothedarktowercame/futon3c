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

    def test_act_listed_as_act_and_still_open_counts_once(self):
        row = record(act="same")  # opener in acts and in still_open
        result = classify([row])
        self.assertEqual(1, result["status-counts"]["open"])
        self.assertEqual(1, result["violation-count"])

    def test_same_act_id_in_two_sessions_is_two_asks(self):
        # Act ids restart with each session of a seat, so zai-7's turn 1 on
        # 10-16 and on 10-31 share an id but are different asks.
        a = record(session="a", act="zai-7-turn-1-f-s1-0", age_days=18)
        b = record(session="b", act="zai-7-turn-1-f-s1-0", age_days=3)
        result = classify([a, b], latest={"a": a, "b": b})
        self.assertEqual(2, result["status-counts"]["open"])
        self.assertEqual(["a"], [x["session"] for x in result["violations"]])

    def test_sidecar_answer_closes_only_its_own_session(self):
        a = record(session="a", act="x"); a["_record"] = "turn-A"
        b = record(session="b", act="x"); b["_record"] = "turn-B"
        sidecar = {"record": "turn-A",
                   "acts": [{"act": "r", "kind": "report",
                             "answers": [{"act": "x", "effect": "closes"}]}]}
        result = classify([a, b], latest={"a": a, "b": b}, sidecars=[sidecar])
        self.assertEqual(1, result["status-counts"]["met"])
        self.assertEqual(["b"], [x["session"] for x in result["violations"]])

class StandingAndCensusTest(unittest.TestCase):
    def test_standing_false_act_is_not_stale(self):
        # Joe pasted "claude is thinking..." terminal output; 象 typed it as a
        # clarify act but the act's own about marks it as pasted status text.
        # Pasted text is not an open intent.  (standing? false alone is NOT
        # the marker: 象 stamps it on real asks — codex-2's 10-21 pair — so
        # a standing?-only filter would erase real violations.)
        row = record()
        row["acts"][0]["kind"] = "clarify"
        row["acts"][0]["text"] = "claude is thinking..."
        row["acts"][0]["standing?"] = False
        row["acts"][0]["about"] = "the pasted 'claude is thinking' status line"
        row["ports"]["still_open"] = [row["acts"][0]]
        result = classify([row])
        self.assertEqual(0, result["violation-count"])
        self.assertNotIn("open", result["status-counts"])

    def test_standing_false_real_ask_is_still_an_intent(self):
        # Guard against over-filtering: codex-2-turn-1-f-s1-0 carries
        # standing? false but is a real operator ask (no paste marker).
        row = record(act="codex-2-turn-1-f-s1-0")
        row["acts"][0]["standing?"] = False
        row["acts"][0]["about"] = "claude-2's current activity"
        result = classify([row])
        self.assertEqual(1, result["status-counts"]["open"])
        self.assertEqual(1, result["violation-count"])

    def test_about_mentioning_the_past_is_still_an_intent(self):
        # "past" is not "pasted": an ask about the past week must stay open.
        row = record(act="codex-2-turn-2-f-s1-0")
        row["acts"][0]["about"] = "the past week's unfinished missions"
        result = classify([row])
        self.assertEqual(1, result["status-counts"]["open"])
        self.assertEqual(1, result["violation-count"])

    def test_draft_artefact_not_counted_invalid(self):
        # 象 sibling artefacts (.draft/.analysis/.candidates) are dicts
        # without session_id/ports by design; they must not be read as turn
        # records at all.
        import tempfile
        with tempfile.TemporaryDirectory() as tmp:
            tmp = pathlib.Path(tmp)
            (tmp / "turn-real.json").write_text(__import__("json").dumps(
                record(session="s", act="a", age_days=0)))
            for name in ("turn-x.draft.json", "turn-y.analysis.json",
                         "turn-z.candidates.json"):
                (tmp / name).write_text('{"stage": "draft"}')
            valid, latest, invalid, unanalysed = PROBE.read_intent_records(tmp)
            self.assertEqual(0, invalid)
            self.assertEqual(1, len(valid))
            self.assertEqual({"total": 0, "operator": 0}, unanalysed)

    def test_unanalysed_operator_turn_is_typed_violation(self):
        # A real turn record (session_id, created_at) with ports: null was
        # never analysed: the intent was lost upstream.  It must surface as
        # its own typed evidence line and a named violation, not as one
        # silently skipped count.
        import tempfile
        with tempfile.TemporaryDirectory() as tmp:
            tmp = pathlib.Path(tmp)
            (tmp / "turn-un.json").write_text(__import__("json").dumps(
                {"session_id": "u", "created_at": NOW.isoformat(),
                 "origin": "operator", "ports": None}))
            (tmp / "turn-empty.json").write_text(__import__("json").dumps(
                {"session_id": "v", "created_at": NOW.isoformat(),
                 "origin": "agent", "ports": None}))
            valid, latest, invalid, unanalysed = PROBE.read_intent_records(tmp)
            self.assertEqual({"total": 2, "operator": 1}, unanalysed)
            result = PROBE.classify_intents(valid, latest, [], NOW,
                                            lambda job: None, invalid, unanalysed)
            self.assertEqual(1, result["violation-count"])
            self.assertIn("operator-turns-never-analysed",
                          [v["act-id"] for v in result["violations"]])
            self.assertTrue(any("operator turns never analysed: 1 of 2" in e
                                for e in result["evidence"]))

    def test_unwitnessable_act_not_counted_stale(self):
        # Finding 6: Zai seats write no assistant rows, so no probe can
        # witness closure for their acts.  Stale-looking Zai acts are
        # unwitnessable by construction — kept visible, not violations.
        row = record(act="zai-5-turn-3-f-s1-0")
        row["agent_id"] = "zai-5"
        result = classify([row])
        self.assertEqual(0, result["violation-count"])
        self.assertEqual(1, result["unwitnessable-count"])
        self.assertEqual(["zai-5-turn-3-f-s1-0"],
                         [x["act-id"] for x in result["unwitnessable"]])


if __name__ == "__main__": unittest.main()
