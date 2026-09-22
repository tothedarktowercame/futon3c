"""Exact source spans and canonical-library validation (no model calls)."""
import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    "analysis", Path(__file__).resolve().parents[1] / "scripts/session_turn_analysis.py")
analysis = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(analysis)


class ValidationTest(unittest.TestCase):
    def setUp(self):
        self.request = {"source_text": "A 🐈 needs evidence.", "offset_unit": "unicode-codepoints-zero-based-end-exclusive",
                        "sentences": [{"id": "s1", "start": 0, "end": 19}]}
        self.fragment = {"start": 4, "end": 18, "text": "needs evidence", "intent": "verify",
                         "target": "claim", "rationale": "requires evidence", "relations": ["condition"],
                         "pattern_refs": []}
        self.data = {"labeller": "test-agent", "sentences": [
            {"id": "s1", "fragments": [self.fragment], "unresolved_reason": ""}]}

    def test_exact_unicode_span_and_real_pattern(self):
        self.fragment["pattern_refs"] = [{"id": "agent/provisional-claims-ledger", "rationale": "explicit evidence requirement"}]
        result = analysis.validate(self.request, self.data)
        self.assertFalse(result["human_approved"])
        self.assertEqual(result["sentences"][0]["fragments"][0]["text"], "needs evidence")

    def test_reject_wrong_offsets_and_invented_pattern(self):
        self.fragment["start"] = 5
        with self.assertRaisesRegex(ValueError, "offsets/text"):
            analysis.validate(self.request, self.data)
        self.fragment["start"] = 4
        self.fragment["pattern_refs"] = [{"id": "nonexistent/pattern-codex-test", "rationale": "invented"}]
        with self.assertRaisesRegex(ValueError, "unknown canonical pattern"):
            analysis.validate(self.request, self.data)

    def test_cannot_silently_omit_unknown_sentence(self):
        self.data["sentences"] = []
        with self.assertRaisesRegex(ValueError, "one analysis per source"):
            analysis.validate(self.request, self.data)
        self.data["sentences"] = [{"id": "s1", "fragments": [], "unresolved_reason": "unclear target"}]
        self.assertEqual(analysis.validate(self.request, self.data)["sentences"][0]["unresolved_reason"], "unclear target")

    def test_publication_preserves_request_and_refuses_overwrite(self):
        with tempfile.TemporaryDirectory() as directory:
            request = Path(directory) / "turn.json"
            request.write_text(json.dumps(self.request))
            before = request.read_bytes()
            output = analysis.complete(request, self.data)
            with self.assertRaises(FileExistsError):
                analysis.complete(request, self.data)
            self.assertEqual(request.read_bytes(), before)
            self.assertEqual(json.loads(output.read_text())["status"], "analyzed")


if __name__ == "__main__":
    unittest.main()
