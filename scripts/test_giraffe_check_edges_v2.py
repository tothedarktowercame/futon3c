#!/usr/bin/env python3
"""Bad-case tests for the combined Giraffe reading checker."""
import copy
import hashlib
import json
import subprocess
import tempfile
import unittest
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
CHECKER = ROOT / "resources" / "giraffe" / "check_edges-v2.py"
VOCAB = ROOT / "resources" / "giraffe" / "placement-vocabularies.json"
WAYS = ROOT / "holes" / "excursions" / "ways-of-working.json"


class CombinedCheckerTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        for source in (CHECKER, VOCAB):
            (self.root / source.name).write_bytes(source.read_bytes())
        (self.root / "ways-of-working.json").write_bytes(WAYS.read_bytes())
        self.source = self.root / "M-example.md"
        self.source.write_text(
            "# M-example — a checked example\n"
            "Status: OPEN\n"
            "This work keeps the backlog understood for the operator.\n"
            "Finish: every open item has a checked placement record.\n")
        digest = hashlib.sha256(self.source.read_bytes()).hexdigest()
        (self.root / "members.json").write_text(json.dumps([
            {"id": "M-example", "path": str(self.source), "sha256": digest}
        ]))
        evidence = {"path": str(self.source), "sha256": digest, "line": 3,
                    "quote": "This work keeps the backlog understood for the operator."}
        finish_evidence = {"path": str(self.source), "sha256": digest, "line": 4,
                           "quote": "Finish: every open item has a checked placement record."}
        self.record = {
            "id": "M-example", "source": {"path": str(self.source), "sha256": digest},
            "status_as_read": "open", "purpose": "Test the combined checker.",
            "edges": [], "unresolved": [],
            "placement": {
                "what": "A test of combined link and placement reading.",
                "status": "open", "ways": ["backlog-understood"], "wing": "B",
                "action": "work", "finish": "Every open item has a checked placement.",
                "beneficiary": "self", "confidence": "high",
                "evidence": [evidence, finish_evidence]
            }
        }

    def tearDown(self):
        self.temp.cleanup()

    def check(self, record):
        batch = self.root / "batch-test.jsonl"
        batch.write_text(json.dumps(record) + "\n")
        return subprocess.run(
            ["python3", "-I", str(self.root / "check_edges-v2.py"), str(batch)],
            text=True, capture_output=True, check=False)

    def test_valid_combined_record_passes(self):
        result = self.check(self.record)
        self.assertEqual(0, result.returncode, result.stdout + result.stderr)
        self.assertIn("0 problems", result.stdout)

    def test_unknown_way_prints_problem(self):
        record = copy.deepcopy(self.record)
        record["placement"]["ways"] = ["invented-way"]
        result = self.check(record)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("way 'invented-way' not allowed", result.stdout)

    def test_missing_placement_prints_problem(self):
        record = copy.deepcopy(self.record)
        del record["placement"]
        result = self.check(record)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("placement missing or not an object", result.stdout)

    def test_placement_quote_outside_window_prints_problem(self):
        record = copy.deepcopy(self.record)
        record["placement"]["evidence"][0]["line"] = 1
        record["placement"]["evidence"][0]["quote"] = "not present in the six line window"
        result = self.check(record)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("quote not found within lines", result.stdout)

    def test_destination_required_when_need_passes_elsewhere(self):
        record = copy.deepcopy(self.record)
        record["placement"]["action"] = "close"
        result = self.check(record)
        self.assertNotEqual(0, result.returncode)
        self.assertIn("need_goes_to is required for action 'close'", result.stdout)


if __name__ == "__main__":
    unittest.main()
