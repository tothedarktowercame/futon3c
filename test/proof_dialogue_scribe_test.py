import hashlib
import tempfile
import unittest
from pathlib import Path

from proof_dialogue_trace import append
from proof_dialogue_scribe import validate


class ScribeCheckpointTest(unittest.TestCase):
    def test_accepts_graphs_and_exact_flexiarg_evidence(self):
        with tempfile.TemporaryDirectory() as directory:
            trace = Path(directory) / "trace.jsonl"
            body = "Control the finite head, then bound the tail."
            append(trace, {"record_type": "turn", "run_id": "r", "move_id": "m1",
                "role": "student", "move_kind": "lemma", "parent_move_id": None,
                "source_refs": [], "body": body,
                "body_sha256": hashlib.sha256(body.encode()).hexdigest()})
            checkpoint = {"schema": "futon3c.proof-dialogue-scribe-checkpoint/v1",
                "through_move_id": "m1", "conversation": {"nodes": [
                    {"id": "c1", "state": "proposed", "move_ids": ["m1"]}], "edges": []},
                "proof": {"nodes": [{"id": "p1", "state": "open", "move_ids": ["m1"]}], "edges": []},
                "patterns": [{"id": "finite-head-infinite-tail", "status": "noticed",
                    "context": "weighted series", "problem": "control an infinite bound",
                    "response": "split head and tail", "consequence": "two finite obligations",
                    "evidence": [{"move_id": "m1", "start": 0, "end": 23,
                                  "text": body[0:23]}]}], "open_obligations": ["bound tail"]}
            self.assertEqual(validate(checkpoint, trace)["patterns"], 1)
            checkpoint["patterns"][0]["evidence"][0]["text"] = "wrong"
            with self.assertRaisesRegex(ValueError, "trace bytes"):
                validate(checkpoint, trace)


if __name__ == "__main__":
    unittest.main()
