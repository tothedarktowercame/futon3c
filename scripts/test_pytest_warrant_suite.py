#!/usr/bin/env python3
"""Bad-case test for pytest_warrant_suite --pattern (M-ways-of-working finding 7).

The bad case this check is named for: futon3c test files are named test_*.py,
while the suite's built-in default glob is *_test.py (the mfuton convention).
With the old code the glob was hardcoded, so a futon3c sweep selected zero
files and named subsets were rejected as 'not test files'. This test
constructs that exact situation against the real script module: the default
pattern finds nothing under scripts/, the pattern 'test_ways_of_working_*.py'
finds the real probe tests.
"""
import importlib.util
import unittest
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
SCRIPT = REPO / "scripts" / "pytest_warrant_suite.py"


def load():
    spec = importlib.util.spec_from_file_location("pytest_warrant_suite", SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


class PatternTest(unittest.TestCase):
    def setUp(self):
        self.mod = load()

    def test_default_pattern_misses_futon3c_names(self):
        # the bad case: hardcoded *_test.py saw none of the probe tests
        found = self.mod.test_files(REPO, "scripts", "*_test.py")
        self.assertEqual([f for f in found if "ways_of_working" in f], [])

    def test_custom_pattern_finds_the_probe_tests(self):
        found = self.mod.test_files(REPO, "scripts", "test_ways_of_working_*.py")
        self.assertIn("scripts/test_ways_of_working_feedback.py", found)
        self.assertIn("scripts/test_ways_of_working_status.py", found)

    def test_named_subset_matches_known_set(self):
        # main() rejects named files not in test_files(); with the old
        # hardcoded glob a named subset was always rejected. A single-file
        # pattern must yield exactly that file, so named subsets resolve.
        files = self.mod.test_files(REPO, "scripts", "test_ways_of_working_feedback.py")
        self.assertEqual(files, ["scripts/test_ways_of_working_feedback.py"])


if __name__ == "__main__":
    unittest.main()
