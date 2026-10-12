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
        self.write("turn-stamp.json", {"agent_id": "seat-1", "value": [1, 2]})
        self.write("turn-existing.json", {"agent_id": "seat-1", "mission_id": "old"})
        self.write("turn-absent.json", {"agent_id": "gone"})
        self.write("turn-blank.json", {"agent_id": "blank"})
        self.write("turn-ignore.json.analysis.json", {"agent_id": "seat-1"})
        self.write("turn-ignore.candidates.json", {"not": "a record"})
        self.roster = {"seat-1": {"mission-id": " M-one "},
                       "blank": {"mission-id": "  "}}

    def tearDown(self):
        self.temp.cleanup()

    def write(self, name, value):
        path = self.directory / name
        path.write_text(json.dumps(value, ensure_ascii=False, indent=2,
                                   separators=(",", " : ")) + "\n", encoding="utf-8")
        return path

    def test_plan_stamps_and_counts_skips(self):
        result = BACKFILL.plan(self.directory, self.roster)
        self.assertEqual(["turn-stamp"], [row["record"] for row in result["rows"]])
        self.assertEqual({"gone": 1}, dict(result["off_roster"]))
        self.assertEqual({"blank": 1}, dict(result["no_mission"]))
        self.assertEqual(1, result["existing"])
        self.assertEqual(4, result["total"])

    def test_apply_preserves_existing_bytes_and_adds_only_key(self):
        existing = self.directory / "turn-existing.json"
        existing_before = existing.read_bytes()
        stamp = self.directory / "turn-stamp.json"
        stamp_before = json.loads(stamp.read_text(encoding="utf-8"))
        result = BACKFILL.plan(self.directory, self.roster)
        ledger = self.directory / "ledger.tsv"
        self.assertEqual(1, BACKFILL.apply_plan(result, ledger))
        self.assertEqual(existing_before, existing.read_bytes())
        expected = dict(stamp_before, mission_id="M-one")
        self.assertEqual(expected, json.loads(stamp.read_text(encoding="utf-8")))
        self.assertTrue(stamp.read_text(encoding="utf-8").endswith("\n"))
        self.assertIn(BACKFILL.PROXY_WARNING, ledger.read_text(encoding="utf-8"))


if __name__ == "__main__":
    unittest.main()
