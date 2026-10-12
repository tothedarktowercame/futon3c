#!/usr/bin/env python3
import importlib.util
import json
import pathlib
import tempfile
import unittest

SCRIPT = pathlib.Path(__file__).with_name("backfill_mission_id.py")
SPEC = importlib.util.spec_from_file_location("backfill_mission_id", SCRIPT)
BACKFILL = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(BACKFILL)


class BackfillMissionIdTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.directory = pathlib.Path(self.temp.name)
        self.write("turn-mission.json", self.record("seat-1", "session-1"))
        self.write("turn-excursion.json", self.record("seat-2", "session-2"))
        self.write("turn-existing.json",
                   dict(self.record("seat-1", "session-1"), mission_id="old"))
        self.write("turn-absent.json", self.record("gone", "session-x"))
        self.write("turn-wrong-session.json", self.record("seat-1", "session-other"))
        self.write("turn-blank.json", self.record("blank", "session-blank"))
        self.write("turn-ignore.json.analysis.json", self.record("seat-1", "session-1"))
        self.clock_file = self.directory / "clocks.json"
        self.clock_file.write_text(json.dumps({"rows": [
            {"agent": "seat-1", "session": "session-1", "mission_id": " M-one ",
             "excursion_id": None, "decided_at": "2026-10-01T00:00:00Z"},
            {"agent": "seat-2", "session": "session-2", "mission_id": None,
             "excursion_id": " E-two ", "decided_at": "2026-10-02T00:00:00Z"},
            {"agent": "blank", "session": "session-blank", "mission_id": " ",
             "excursion_id": None}],}), encoding="utf-8")

    def tearDown(self):
        self.temp.cleanup()

    @staticmethod
    def record(agent, session):
        return {"agent_id": agent, "session_id": session, "value": [1, 2]}

    def write(self, name, value):
        path = self.directory / name
        path.write_text(json.dumps(value, ensure_ascii=False, indent=2,
                                   separators=(",", " : ")) + "\n", encoding="utf-8")
        return path

    def result(self):
        return BACKFILL.plan(self.directory, BACKFILL.load_clocks(self.clock_file))

    def test_plan_joins_session_and_includes_excursion_only(self):
        result = self.result()
        planned = {(r["record"], r["key"], r["value"]) for r in result["rows"]}
        self.assertEqual({("turn-mission", "mission_id", "M-one"),
                          ("turn-excursion", "excursion_id", "E-two")}, planned)
        self.assertEqual(2, sum(result["no_clock"].values()))
        self.assertEqual(1, sum(result["no_target"].values()))
        self.assertEqual({"mission_id": 1}, dict(result["existing"]))
        self.assertEqual(6, result["total"])

    def test_apply_preserves_existing_and_adds_only_selected_key(self):
        existing = self.directory / "turn-existing.json"
        existing_before = existing.read_bytes()
        mission_before = json.loads((self.directory / "turn-mission.json").read_text())
        excursion_before = json.loads((self.directory / "turn-excursion.json").read_text())
        ledger = self.directory / "ledger.tsv"
        self.assertEqual(2, BACKFILL.apply_plan(self.result(), ledger))
        self.assertEqual(existing_before, existing.read_bytes())
        self.assertEqual(dict(mission_before, mission_id="M-one"),
                         json.loads((self.directory / "turn-mission.json").read_text()))
        self.assertEqual(dict(excursion_before, excursion_id="E-two"),
                         json.loads((self.directory / "turn-excursion.json").read_text()))
        self.assertIn(BACKFILL.LEDGER_NOTE, ledger.read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
