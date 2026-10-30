import json
import tempfile
import unittest
from pathlib import Path

import sys
sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "scripts"))
import xiang_platform_agent_replay as replay


class XiangPlatformAgentReplayTest(unittest.TestCase):
    def test_isolation_rewrites_record_and_does_not_copy_baseline(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            store, run = root / "store", root / "run"
            store.mkdir()
            record = store / "turn-Ab12.json"
            record.write_text('{"source_text":"x"}\n')
            Path(str(record) + ".draft.json").write_text("{}\n")
            Path(str(record) + ".analysis.json").write_text('{"secret":"baseline"}\n')
            case = {"prompt": f"read {record}", "record_name": record.name}
            prompt, copied, baseline = replay.isolate_case(case, run, store)
            self.assertIn(str(copied), prompt)
            self.assertNotIn(str(record), prompt)
            self.assertTrue(Path(str(copied) + ".draft.json").is_file())
            self.assertFalse(Path(str(copied) + ".analysis.json").exists())
            self.assertEqual(Path(str(record) + ".analysis.json"), baseline)

    def test_extract_result_rejects_non_json(self):
        with self.assertRaises((ValueError, json.JSONDecodeError)):
            replay.extract_result({"result": "I completed it"})

    def test_extract_object_rejects_array(self):
        with self.assertRaises(ValueError):
            replay.extract_object('[{"sentences": []}]')


if __name__ == "__main__":
    unittest.main()
