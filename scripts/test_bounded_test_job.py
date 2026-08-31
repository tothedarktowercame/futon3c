import importlib.util
import contextlib
import io
import json
import pathlib
import subprocess
import sys
import tempfile
import unittest


SCRIPT = pathlib.Path(__file__).with_name("bounded_test_job.py")
SPEC = importlib.util.spec_from_file_location("bounded_test_job", SCRIPT)
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)
BG_SCRIPT = pathlib.Path(__file__).with_name("bg.py")
BG_SPEC = importlib.util.spec_from_file_location("bg", BG_SCRIPT)
BG_MODULE = importlib.util.module_from_spec(BG_SPEC)
BG_SPEC.loader.exec_module(BG_MODULE)


class CertificateResourceTest(unittest.TestCase):
    def receipt(self, **changes):
        base = {"outer-exit": 0, "inner-exit": 0, "reason": None,
                "pids-events-max-delta": 0, "native-thread-markers": [],
                "pids-peak": 7, "receipt-path": "/tmp/measured.json"}
        return dict(base, **changes)

    def test_clean_measurement_emits_certificate_edn(self):
        with tempfile.TemporaryDirectory() as root:
            path = str(pathlib.Path(root) / "resource.edn")
            MODULE.write_certificate_resource(path, self.receipt())
            text = pathlib.Path(path).read_text()
            self.assertIn(":source-schema :futon-bounded-test-v1", text)
            self.assertIn(":status :clean", text)
            self.assertIn(":command-exit 0", text)
            self.assertIn(":native-thread-exhaustion false", text)

    def test_dirty_measurement_cannot_look_clean(self):
        with tempfile.TemporaryDirectory() as root:
            path = str(pathlib.Path(root) / "resource.edn")
            MODULE.write_certificate_resource(
                path, self.receipt(**{"outer-exit": 125, "inner-exit": 7,
                                     "reason": "test-failure"}))
            text = pathlib.Path(path).read_text()
            self.assertIn(":status :dirty", text)
            self.assertIn(":reason :test-failure", text)
            self.assertIn(":command-exit 7", text)

    def test_failed_test_is_correlated_with_clean_run_resources(self):
        receipt = self.receipt(**{"outer-exit": 125, "inner-exit": 7,
                                  "reason": "test-failure"})
        correlation = MODULE.failure_resource_correlation(receipt)
        self.assertEqual("clean", correlation["resource-status"])
        self.assertEqual("whole-run-not-per-test", correlation["scope"])
        self.assertIn("cannot identify which test", correlation["limitation"])
        self.assertIn("resource-status=:clean",
                      MODULE.correlation_line(correlation))

    def test_tiny_budget_resource_failure_is_correlated_as_dirty(self):
        # C91's tiny-budget fixture shape: green inner suite, cgroup max events.
        receipt = self.receipt(**{"outer-exit": 125, "inner-exit": 0,
                                  "reason": "resource-limit-failure",
                                  "pids-events-max-delta": 2})
        correlation = MODULE.failure_resource_correlation(receipt)
        self.assertEqual("dirty", correlation["resource-status"])
        self.assertEqual(2, correlation["pids-events-max-delta"])
        self.assertIn("resource-status=:dirty",
                      MODULE.correlation_line(correlation))

    def test_unreadable_repository_basis_fails_top_level_certification(self):
        with tempfile.TemporaryDirectory() as root:
            receipt = pathlib.Path(root) / "receipt.json"
            output = pathlib.Path(root) / "output.log"
            proc = subprocess.run(
                [sys.executable, str(SCRIPT), "--receipt", str(receipt),
                 "--output", str(output), "--cwd", root, "true"],
                capture_output=True, text=True)
            self.assertEqual(125, proc.returncode)
            data = json.loads(receipt.read_text())
            self.assertEqual("fail", data["verdict"])
            self.assertEqual(125, data["outer-exit"])
            self.assertEqual("repository-basis-unavailable", data["reason"])
            self.assertFalse(data["repository-basis-start"]["readable"])

    def test_launch_without_repository_basis_is_refused_before_submission(self):
        output = io.StringIO()
        with contextlib.redirect_stdout(output):
            exit_code = BG_MODULE.main(["launch-test", "true"])
        self.assertEqual(1, exit_code)
        result = json.loads(output.getvalue())
        self.assertFalse(result["ok"])
        self.assertEqual("refused", result["state"])
        self.assertEqual("repository-basis-required", result["reason"])


if __name__ == "__main__":
    unittest.main()
