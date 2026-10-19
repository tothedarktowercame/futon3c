import importlib.util
import os
import subprocess
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[2]
SCRIPT = ROOT / "scripts" / "mined_pattern_graph.py"


def load_script():
    spec = importlib.util.spec_from_file_location("mined_pattern_graph", SCRIPT)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class PublishGraphTest(unittest.TestCase):
    def test_relative_output_is_resolved_before_futon2_subprocess(self):
        script = load_script()
        with tempfile.TemporaryDirectory() as caller, tempfile.TemporaryDirectory() as futon2:
            old = os.getcwd()
            os.chdir(caller)
            try:
                with mock.patch.object(script.subprocess, "run",
                                       return_value=subprocess.CompletedProcess([], 0, "{:status :published}", "")) as run:
                    script.publish_graph({"records": 0}, "relative.json", futon2)
            finally:
                os.chdir(old)
        self.assertEqual(str(Path(caller) / "relative.json"), run.call_args.args[0][-1])
        self.assertEqual(futon2, run.call_args.kwargs["cwd"])

    def test_publication_failure_keeps_typed_stdout(self):
        script = load_script()
        with tempfile.TemporaryDirectory() as root, \
             mock.patch.object(script.subprocess, "run",
                               return_value=subprocess.CompletedProcess([], 1, "{:status :refused :kind :bad-pin}", "")):
            with self.assertRaisesRegex(RuntimeError, "bad-pin"):
                script.publish_graph({"records": 0}, str(Path(root) / "graph.json"), root)


if __name__ == "__main__":
    unittest.main()
