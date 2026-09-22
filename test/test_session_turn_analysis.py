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
                         "pattern_refs": [], "display_cues": [{"start": 4, "end": 18, "text": "needs evidence"}]}
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

    def test_whole_sentence_display_is_rejected_but_full_analysis_is_allowed(self):
        source = "Even if the whole turn is processed I would want keyword based analysis."
        request = {"source_text": source, "offset_unit": "unicode-codepoints-zero-based-end-exclusive",
                   "sentences": [{"id": "s1", "start": 0, "end": len(source)}]}
        self.fragment.update(start=0, end=len(source), text=source,
                             display_cues=[{"start": 0, "end": len(source), "text": source}])
        with self.assertRaisesRegex(ValueError, "short keyword phrases"):
            analysis.validate(request, self.data)
        start = source.index("I would want")
        self.fragment["display_cues"] = [{"start": start, "end": start+12, "text": "I would want"}]
        result = analysis.validate(request, self.data)
        self.assertEqual(result["sentences"][0]["fragments"][0]["text"], source)

    def test_reusable_cue_requires_source_and_reuse_reason(self):
        self.data["reusable_cues"] = [{"start": 4, "end": 18, "text": "needs evidence",
                                      "intent": "verify", "rationale": "Explicit evidence request"}]
        result = analysis.validate(self.request, self.data)
        self.assertEqual(result["reusable_cues"][0]["intent"], "verify")
        self.data["reusable_cues"][0]["text"] = "not actually said"
        with self.assertRaisesRegex(ValueError, "exact source span"):
            analysis.validate(self.request, self.data)

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
