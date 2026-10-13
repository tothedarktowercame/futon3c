import hashlib
import importlib.util
import json
from pathlib import Path
import sys
import tempfile
import unittest

SCRIPTS = Path(__file__).parents[1] / "scripts"
sys.path.insert(0, str(SCRIPTS))
SPEC = importlib.util.spec_from_file_location("packet", SCRIPTS / "proof_dialogue_label_packet.py")
PACKET = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(PACKET)
TRACE_SPEC = importlib.util.spec_from_file_location("trace", SCRIPTS / "proof_dialogue_trace.py")
TRACE = importlib.util.module_from_spec(TRACE_SPEC)
TRACE_SPEC.loader.exec_module(TRACE)


class LabelPacketTest(unittest.TestCase):
    def test_packet_preserves_bytes_and_excludes_authored_labels(self):
        with tempfile.TemporaryDirectory() as directory:
            path = Path(directory) / "trace.jsonl"
            body = "Prove \\(x+y=y+x\\).\n"
            TRACE.append(path, {"record_type": "turn", "run_id": "r", "move_id": "m1",
                "role": "teacher", "move_kind": "conjecture", "parent_move_id": None,
                "source_refs": ["secret-proof.lean"], "body": body,
                "body_sha256": hashlib.sha256(body.encode()).hexdigest()})
            packet = PACKET.build(path)
            encoded = json.dumps(packet)
            self.assertEqual(packet["messages"][0]["text"], body)
            self.assertEqual(packet["messages"][0]["speaker"], "participant-1")
            for forbidden in ("move_kind", "teacher", "secret-proof.lean", "event_id"):
                self.assertNotIn(forbidden, encoded)


if __name__ == "__main__":
    unittest.main()
