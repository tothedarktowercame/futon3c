#!/usr/bin/env python3
"""Acceptance tests for probe_cascade_standards in ways-of-working-probe.py.

Bad cases from the zai-4 audit (2026-11-04) and Joe's 2026-11-05 decision:
an unchecked standard must not pass, and a summary whose standards key is
missing (shape drift) must read as broken, not all-pass.
"""
import importlib.util
import pathlib
import unittest

HERE = pathlib.Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("wow_probe", HERE / "ways-of-working-probe.py")
wow = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(wow)


def unchecked_std():
    return {"s1-regenerates": {"state": "unchecked",
                               "reason": "regeneration is not distinguished from answering"},
            "s2-evidence": {"state": "unchecked", "reason": "no check exists"},
            "s3-reconstitution": {"state": "unchecked", "reason": "no exercise exists"},
            "s4-honest-holes": {"state": "unchecked",
                                "reason": "no producer writes cascade/hole-target edges"},
            "s5-composed": True}


class CascadeStandardsTest(unittest.TestCase):
    def test_false_standard_is_a_violation(self):
        std = unchecked_std()
        std["s5-composed"] = False
        live = wow.probe_cascade_standards({"standards": std, "holes": {}})
        self.assertEqual("failing", live["invariant"])
        self.assertIn("s5-composed", live["violations"])
        self.assertIn("s5-composed: False", live["evidence"])

    def test_unchecked_standard_does_not_pass(self):
        live = wow.probe_cascade_standards({"standards": unchecked_std(), "holes": {}})
        self.assertEqual("on", live["monitor"])
        self.assertEqual("failing", live["invariant"])
        for k in ("s1-regenerates", "s2-evidence", "s3-reconstitution", "s4-honest-holes"):
            self.assertIn(k, live["violations"], f"{k} unchecked must be a violation")
        self.assertTrue(any("no producer writes cascade/hole-target" in e for e in live["evidence"]),
                        "the unchecked reason must reach the evidence")

    def test_string_unchecked_also_fails(self):
        std = unchecked_std()
        std["s2-evidence"] = "unchecked"
        live = wow.probe_cascade_standards({"standards": std, "holes": {}})
        self.assertIn("s2-evidence", live["violations"])

    def test_missing_standards_key_is_broken(self):
        live = wow.probe_cascade_standards({"holes": {}})
        self.assertEqual("broken", live["monitor"])
        self.assertEqual("unknown", live["invariant"])
        self.assertEqual([], live["violations"])
        self.assertTrue(any("standards absent" in e for e in live["evidence"]))

    def test_absent_summary_is_broken(self):
        live = wow.probe_cascade_standards(None)
        self.assertEqual("broken", live["monitor"])
        self.assertTrue(any("summary.json absent" in e for e in live["evidence"]))

    def test_s6_readable_stays_a_violation(self):
        live = wow.probe_cascade_standards({"standards": unchecked_std(), "holes": {}})
        self.assertIn("s6-readable", live["violations"])


if __name__ == "__main__":
    unittest.main()
