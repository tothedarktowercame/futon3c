import importlib.util
import json
from pathlib import Path
import tempfile
import unittest

SPEC = importlib.util.spec_from_file_location(
    "proof_dialogue_trace", Path(__file__).parents[1] / "scripts/proof_dialogue_trace.py")
TRACE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(TRACE)


class ProofDialogueTraceTest(unittest.TestCase):
    def test_turn_delivery_chain_and_tamper_rejection(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "trace.jsonl"
            TRACE.append(path, {
                "record_type": "turn", "run_id": "pilot", "move_id": "m1",
                "role": "teacher", "move_kind": "conjecture",
                "parent_move_id": None, "source_refs": ["problem.md"],
                "body": "State the topology claim.",
                "body_sha256": __import__("hashlib").sha256(
                    b"State the topology claim.").hexdigest(),
            })
            TRACE.append(path, {
                "record_type": "delivery", "move_id": "m1",
                "surface": "matrix", "event_id": "$event",
            })
            self.assertEqual({"records": 2, "moves": 1, "deliveries": 1},
                {k: v for k, v in TRACE.verify(TRACE.read_trace(path)).items() if k != "head"})
            records = TRACE.read_trace(path)
            records[0]["body"] = "tampered"
            path.write_text("\n".join(json.dumps(r) for r in records) + "\n")
            with self.assertRaisesRegex(ValueError, "record hash mismatch"):
                TRACE.verify(TRACE.read_trace(path))

    def test_parent_must_precede_child(self):
        record = {
            "sequence": 1, "previous_record_sha256": None,
            "record_type": "turn", "run_id": "pilot", "move_id": "m2",
            "role": "student", "move_kind": "question",
            "parent_move_id": "missing", "source_refs": [], "body": "Why?",
            "body_sha256": __import__("hashlib").sha256(b"Why?").hexdigest(),
        }
        record["record_sha256"] = TRACE.digest(record)
        with self.assertRaisesRegex(ValueError, "unresolved parent"):
            TRACE.verify([record])


if __name__ == "__main__":
    unittest.main()
