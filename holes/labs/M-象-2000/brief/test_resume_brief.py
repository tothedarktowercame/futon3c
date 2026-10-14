#!/usr/bin/env python3
import datetime as dt
import importlib.util
import json
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).parent
SPEC = importlib.util.spec_from_file_location("resume_brief", HERE / "resume_brief.py")
brief = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(brief)

class ResumeBriefTest(unittest.TestCase):
    def write(self, root, name, value):
        (root / name).write_text(json.dumps(value))

    def test_two_sessions_dedupe_sections_and_measurement(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            common = {"mission_id": "M-test", "agent_id": "claude-1",
                      "created_at": "2026-10-11T00:00:00Z",
                      "ports": {"still_open": [], "closed_this_turn": []},
                      "standings": {"in_force": [], "adopted_not_applied": []},
                      "obligations": []}
            first = dict(common, turn_id="turn-1", evidence_id="record-1",
                         session_id="session-one")
            first["acts"] = [
                {"id": "agent-open", "author": "claude-1", "kind": "promise",
                 "text": "I will return the tested patch", "at": "2026-10-10T00:00:00Z"},
                {"id": "operator-open", "author": "operator", "kind": "ask-action",
                 "text": "Please check the boundary", "at": "2026-10-10T01:00:00Z"},
                {"id": "standing", "author": "operator", "kind": "constrain",
                 "text": "Keep the statement frozen", "at": "2026-10-10T02:00:00Z"}]
            first["ports"] = {"still_open": [
                {"act": "agent-open", "since": "2026-10-10T00:00:00Z"},
                {"act": "operator-open", "since": "2026-10-10T01:00:00Z"}],
                "closed_this_turn": [{"act": "closed", "kind": "delegate",
                                      "text": "old task", "at": "2026-10-11T03:00:00Z"}],
                "inferred": [{"opener": "closed", "answer": "answer",
                              "rule": "sole-port", "effect": "closes"}]}
            first["standings"] = {"in_force": ["standing"], "adopted_not_applied": []}
            second = dict(common, turn_id="turn-2", evidence_id="record-2",
                          session_id="session-two")
            second["acts"] = list(first["acts"])
            second["ports"] = {"still_open": [
                {"act": "agent-open", "since": "2026-10-10T00:00:00Z"}],
                "closed_this_turn": []}
            third = dict(first, turn_id="turn-3", evidence_id="record-3",
                         created_at="2026-10-12T00:00:00Z")
            third["ports"] = dict(first["ports"], closed_this_turn=[], inferred=[])
            self.write(root, "turn-a.json", first)
            self.write(root, "turn-b.json", second)
            self.write(root, "turn-c.json", third)
            records, join = brief.load_records(root, "M-test", set())
            now = dt.datetime(2026, 10, 12, tzinfo=dt.timezone.utc)
            data = brief.prepare(records, now)
            text = brief.render("M-test", join, [], data, now)
            self.assertEqual(1, text.count("I will return the tested patch"))
            self.assertIn("Awaiting you: agent proposals and asks (1)", text)
            self.assertIn("Asked by the operator, no answer linked (1)", text)
            self.assertIn("Keep the statement frozen", text)
            self.assertIn("inferred: sole-port", text)
            path = brief.write_measurement(root, "M-test", "claude-1", now, data)
            payload = json.loads(path.read_text())
            self.assertEqual(["agent-open"], payload["sections"]["awaiting_you"])
            self.assertEqual(["turn-a", "turn-b", "turn-c"], payload["record_ids"])

    def test_seed_block_and_inline_quotes(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "mission.md"
            path.write_text(
                '**Operator-voice anchor (Joe, 2026-10-12, verbatim):** "new inline"\n'
                '**Operator-voice anchor** (Joe, 2026-10-11, verbatim):\n\n'
                '> older block\n'
            )
            self.assertEqual(["new inline", "> older block"], brief.seeds(path))

    def test_mission_or_excursion_join_and_header(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            base = {"agent_id": "claude-1", "created_at": "2026-10-12T00:00:00Z",
                    "ports": {"still_open": [], "closed_this_turn": []},
                    "standings": {}, "obligations": [], "acts": []}
            self.write(root, "turn-mission.json",
                       dict(base, turn_id="m", session_id="sm", mission_id="M-test"))
            self.write(root, "turn-excursion.json",
                       dict(base, turn_id="e", session_id="se", excursion_id="E-listed"))
            self.write(root, "turn-other.json",
                       dict(base, turn_id="x", session_id="sx", excursion_id="E-other"))
            records, join = brief.load_records(
                root, "M-test", set(), join="auto", excursions={"E-listed"})
            self.assertEqual(["e", "m"], sorted(r["turn_id"] for r in records))
            self.assertEqual("mission-excursion", join)
            now = dt.datetime(2026, 10, 12, tzinfo=dt.timezone.utc)
            text = brief.render("M-test", join, [], brief.prepare(records, now),
                                now, ["E-listed"])
            self.assertIn("_Join: mission_id M-test + excursions E-listed._", text)
            strict, strict_join = brief.load_records(
                root, "M-test", set(), join="mission", excursions={"E-listed"})
            self.assertEqual(["m"], [r["turn_id"] for r in strict])
            self.assertEqual("mission", strict_join)

    def test_mission_facts_true_now_matching_newest_first(self):
        now = dt.datetime(2026, 10, 14, 18, tzinfo=dt.timezone.utc)
        rows = [
            {"id": "a", "memory": "cc:p/one", "text": "M-test needs a frozen statement",
             "said": "2026-10-10T00:00:00Z", "unsaid": None},
            {"id": "b", "memory": "cc:p/two", "text": "M-test ruling withdrawn",
             "said": "2026-10-11T00:00:00Z", "unsaid": "2026-10-12T00:00:00Z"},
            {"id": "c", "memory": "cc:p/three", "text": "unrelated lesson",
             "said": "2026-10-13T00:00:00Z", "unsaid": None},
            {"id": "d", "memory": "cc:p/M-test-notes", "text": "said in the id only",
             "said": "2026-10-12T00:00:00Z", "unsaid": None},
            {"id": "e", "memory": "cc:p/four", "text": "M-test future fact",
             "said": "2026-10-15T00:00:00Z", "unsaid": None},
        ]
        picked = brief.mission_facts(rows, ["M-test"], now)
        self.assertEqual(["d", "a"], [f["id"] for f in picked])
        self.assertEqual(["d"], [f["id"] for f in brief.mission_facts(rows, ["M-test"], now, since="2026-10-11")])
        self.assertEqual(["d"], [f["id"] for f in brief.mission_facts(rows, ["M-test"], now, cap=1)])
        self.assertTrue(brief.fact_line(picked[0]).startswith("- 2026-10-12 M-test-notes: "))
        self.assertEqual("- 2026-10-10 one: M-test needs a frozen statement", brief.fact_line(picked[1]))
        now2 = dt.datetime(2026, 10, 14, tzinfo=dt.timezone.utc)
        text = brief.render("M-test", "mission", [], dict(
            awaiting_you=[], awaiting_others=[], asked=[], in_force=[], adopted=[],
            obligations=[], closed=[], newest=None, record_count=0, session_count=0,
            record_ids=[], facts=picked, facts_note="store not answering; cache of x"), now2)
        self.assertIn("## Facts (2)", text)
        self.assertIn("_facts: store not answering; cache of x_", text)

    def test_bell_ask_closed_when_its_job_ended(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            rec = {"mission_id": "M-test", "agent_id": "zai-1", "session_id": "zai-s",
                   "turn_id": "bell-1", "created_at": "2026-10-13T00:00:00Z",
                   "acts": [{"id": "invoke-1791000000000-1-abcdef01-b-0", "author": "zai-9a",
                             "kind": "ask-action", "text": "Requisition: shard 3",
                             "at": "2026-10-13T00:00:00Z"},
                            {"id": "invoke-1791000000000-2-abcdef02-b-0", "author": "zai-9a",
                             "kind": "ask-action", "text": "Requisition: shard 4",
                             "at": "2026-10-13T00:00:00Z"}],
                   "ports": {"still_open": [
                       {"act": "invoke-1791000000000-1-abcdef01-b-0", "since": "2026-10-13T00:00:00Z"},
                       {"act": "invoke-1791000000000-2-abcdef02-b-0", "since": "2026-10-13T00:00:00Z"}],
                       "closed_this_turn": []},
                   "standings": {"in_force": [], "adopted_not_applied": []}, "obligations": []}
            self.write(root, "turn-z.json", rec)
            records, join = brief.load_records(root, "M-test", set())
            now = dt.datetime(2026, 10, 14, tzinfo=dt.timezone.utc)
            states = {"invoke-1791000000000-1-abcdef01": ("done", "2026-10-13T01:30:00.000Z"),
                      "invoke-1791000000000-2-abcdef02": ("running", None)}
            data = brief.prepare(records, now, lambda j: states.get(j, (None, None)))
            self.assertEqual(["invoke-1791000000000-2-abcdef02-b-0"],
                             [brief.aid(x) for x in data["awaiting_others"]])
            self.assertEqual(1, len(data["closed"]))
            self.assertEqual("job done 2026-10-13T01:30Z", data["closed"][0]["inferred"])
            text = brief.render("M-test", join, [], data, now)
            self.assertIn("inferred: job done 2026-10-13T01:30Z", text)
            self.assertEqual("invoke-1791000000000-1-abcdef01", brief.job_of("invoke-1791000000000-1-abcdef01-b-3"))
            self.assertIsNone(brief.job_of("agent-open"))

    def test_fact_joined_to_the_turn_that_wrote_it(self):
        fact = {"id": "f", "memory": "cc:p/lesson", "text": "M-test lesson",
                "said": "2026-10-14T17:35:10.469Z", "unsaid": None}
        versions = [("cc:p/lesson", "2026-10-14T17:35:10.469Z", "sess-A"),
                    ("cc:p/other", "2026-10-14T17:36:00.000Z", "sess-B"),
                    ("cc:p/lesson", "2026-10-14T18:00:00.000Z", "sess-C")]
        index = {"sess-A": [("2026-10-14T17:20:00Z", "claude-14-turn-7", "claude-14", "earlier ask", "turn-x"),
                            ("2026-10-14T17:31:34Z", "claude-14-turn-8", "claude-14", "73 problems still seemed like a lot", "turn-y"),
                            ("2026-10-14T17:40:00Z", "claude-14-turn-9", "claude-14", "later", "turn-z")]}
        turn = brief.writing_turn(fact, versions, index)
        self.assertEqual("claude-14-turn-8", turn["turn_id"])
        self.assertEqual("turn-y", turn["record_id"])
        fact["_turn"] = turn
        self.assertIn("↳ written during claude-14-turn-8 (claude-14), after the operator said: “73 problems",
                      brief.fact_line(fact))
        self.assertIsNone(brief.writing_turn(dict(fact, memory="cc:p/none"), versions, index))

    def test_default_excursions_from_mission_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "mission.md"
            path.write_text("# M\n\n**Excursions:** E-one, E-two, E-one\n\nMentions E-three in passing; ignore M-three.")
            self.assertEqual(["E-one", "E-two"], brief.mission_excursions(path))
            path.write_text("Mentions E-three only in passing, no declaration.")
            self.assertEqual([], brief.mission_excursions(path))

if __name__ == "__main__":
    unittest.main()
