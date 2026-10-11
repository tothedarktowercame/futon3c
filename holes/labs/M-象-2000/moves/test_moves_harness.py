import importlib.util
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent


def module(name):
    spec = importlib.util.spec_from_file_location(name, HERE / f"{name}.py")
    loaded = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(loaded)
    return loaded


sample = module("sample")
validate = module("validate")


class MovesHarnessTest(unittest.TestCase):
    def row(self, name, session, intents):
        record = {"turn_id": name, "session_id": session, "created_at": name,
                  "origin": "operator", "analysis_status": "analyzed",
                  "reply_text": "㊥ (done) reply", "source_text": "source"}
        fragments = [{"id": f"{name}-f-s1-{i}", "intent": intent,
                      "text": f"text {intent} {i}"}
                     for i, intent in enumerate(intents)]
        return (Path(name + ".json"), record, fragments)

    def test_sampler_stratifies_and_caps_sessions(self):
        rows = []
        for i in range(8):
            rows.append(self.row(f"multi{i}", f"s{i // 2}", ["propose", "clarify"]))
            rows.append(self.row(f"single{i}", f"t{i // 2}", ["ask-action"]))
        chosen = sample.choose(rows, 8, 1)
        self.assertEqual(8, len(chosen))
        self.assertEqual(4, sum(sum(f["intent"] in sample.OPENERS for f in row[2]) >= 2
                                for row in chosen))
        counts = {}
        for row in chosen:
            counts[row[1]["session_id"]] = counts.get(row[1]["session_id"], 0) + 1
        self.assertLessEqual(max(counts.values()), 3)

    def test_include_labelled_keeps_frozen_29_even_when_n_is_smaller(self):
        rows = [self.row(f"label{i}", sample.LABELLED_SESSION, ["ask-action"])
                for i in range(31)]
        chosen = sample.choose(rows, 5, 1, include_labelled=True)
        self.assertEqual(29, len(chosen))

    def test_validator_reports_void_without_failing(self):
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            source = {"record": "turn-a", "fragments": [
                {"id": "op", "intent": "ask-action", "text": "please do the work"},
                {"id": "reply", "intent": "report", "text": "I completed the work"}],
                "previous_reply_acts": []}
            (root / "turn-a.input.json").write_text(json.dumps(source))
            sidecar = {"record": "turn-a", "labeller": "zai-1", "schema": "moves/1",
                       "at": "now", "unlabelled": [], "acts": [
                           {"act": "reply", "kind": "report", "move": "report/done",
                            "quote": "not present", "answers": [
                                {"act": "op", "quote": "completed the work", "effect": "closes"}],
                            "scope": {"opens": None, "closes": None, "within": None},
                            "confidence": "high"}]}
            path = root / "turn-a.moves.json"
            path.write_text(json.dumps(sidecar))
            valid, void, errors = validate.validate(path)
            self.assertEqual((0, 1, []), (valid, void, errors))

    def test_validator_catches_unknown_answer_and_bad_move(self):
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw)
            (root / "turn-a.input.json").write_text(json.dumps({
                "record": "turn-a", "fragments": [{"id": "reply", "text": "answer text"}],
                "previous_reply_acts": []}))
            sidecar = {"record": "turn-a", "labeller": "z", "schema": "moves/1",
                       "at": "now", "unlabelled": [], "acts": [{
                           "act": "reply", "kind": "report", "move": "Bad label",
                           "quote": "answer text", "answers": [{"act": "missing", "quote": "answer", "effect": "closes"}],
                           "scope": {}, "confidence": "low"}]}
            path = root / "turn-a.moves.json"; path.write_text(json.dumps(sidecar))
            self.assertGreaterEqual(len(validate.validate(path)[2]), 2)

    def test_scorer_counts_incidence_and_precision(self):
        with tempfile.TemporaryDirectory() as raw:
            root = Path(raw); moves = root / "moves"; moves.mkdir()
            labels = [{"row": 1, "record": "turn-a", "act": "a", "label": "answered"},
                      {"row": 2, "record": "turn-a", "act": "b", "label": "open"},
                      {"row": 3, "record": None, "act": None, "label": "done"}]
            label_path = root / "labels.json"; label_path.write_text(json.dumps(labels))
            (moves / "turn-a.moves.json").write_text(json.dumps({"record": "turn-a", "acts": [
                {"answers": [{"act": "a"}, {"act": "b"}]}]}))
            result = subprocess.run([sys.executable, "-I", str(HERE / "score.py"),
                                     "--moves", str(moves), "--labels", str(label_path)],
                                    text=True, capture_output=True, check=True)
            report = json.loads(result.stdout)
            self.assertEqual(2, report["incidence"])
            self.assertEqual(0.5, report["precision_answered_vs_open"])

class ReplyMarksFollowKernelNumbering(unittest.TestCase):
    def test_marked_only_zero_based_body_text(self):
        record = {"turn_id": "a-turn-1", "agent_id": "a",
                  "reply_text": "plain intro paragraph\n\n㊢ (what) did it\n\nunmarked middle\n\n🈸: yes or no?"}
        acts = sample.reply_marks(record)
        self.assertEqual([a["id"] for a in acts], ["a-turn-1-r-0", "a-turn-1-r-1"])
        self.assertEqual(acts[0]["text"], "(what) did it")
        self.assertEqual(acts[1]["text"], "yes or no?")
        self.assertEqual(acts[1]["mark"], "🈸")


if __name__ == "__main__":
    unittest.main()
