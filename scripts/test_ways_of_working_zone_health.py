#!/usr/bin/env python3
"""Bad-case tests for the zone-health way-of-working probe.

The probe shells out to futon0/scripts/zone-health.bb; the runner is injectable
so these tests feed synthetic zone-health JSON lines instead. Bad cases:
- one failing check row -> invariant failing, naming the check;
- a check in ALIVE_CHECKS missing from zone-health's output -> the reading is
  broken, not holding (audit 2026-11-04, zai-7: missing checks were silently
  dropped, shrinking the denominator).
"""

import importlib.util
import json
import pathlib
import sys
import unittest


SCRIPTS = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPTS))
SPEC = importlib.util.spec_from_file_location(
    "ways_of_working_probe", SCRIPTS / "ways-of-working-probe.py")
PROBE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(PROBE)


def fake_runner(rows):
    """A runner standing in for the bb subprocess: one JSON line per row."""
    out = "\n".join(json.dumps(r) for r in rows)
    def run(cmd, cwd=None, timeout=60):  # noqa: A001
        return 0, out, ""
    return run


def row(check_id, status="pass", summary="fine"):
    return {"id": check_id, "what": check_id, "status": status, "summary": summary}


def all_rows(drop=(), override=None):
    override = override or {}
    return [row(c, **override.get(c, {})) for c in PROBE.ALIVE_CHECKS if c not in drop]


class ZoneHealthProbe(unittest.TestCase):

    def test_all_pass_reads_holding(self):
        result = PROBE.probe_zone_health(runner=fake_runner(all_rows()))
        self.assertEqual(result["monitor"], "on")
        self.assertEqual(result["invariant"], "holding")
        self.assertEqual(result["violation-count"], 0)

    def test_one_fail_row_gives_failing_naming_the_check(self):
        rows = all_rows(override={"evidence-backup":
                                   {"status": "fail", "summary": "no off-box evidence copy"}})
        result = PROBE.probe_zone_health(runner=fake_runner(rows))
        self.assertEqual(result["monitor"], "on")
        self.assertEqual(result["invariant"], "failing")
        self.assertEqual([v["check"] for v in result["violations"]], ["evidence-backup"])
        self.assertIn("no off-box evidence copy", result["violations"][0]["summary"])

    def test_missing_check_is_broken_not_holding(self):
        # zone-health crashed on (or renamed) one check: it is absent from the
        # output entirely. The reading must not be holding.
        rows = all_rows(drop={"write-rate"})
        result = PROBE.probe_zone_health(runner=fake_runner(rows))
        self.assertEqual(result["monitor"], "broken")
        self.assertNotEqual(result["invariant"], "holding")
        named = [v["check"] for v in result["violations"]]
        self.assertIn("write-rate", named)
        self.assertTrue(any("did not report" in v["summary"] for v in result["violations"]))
        self.assertIn("1 of 14 checks missing", result["denominator"])

    def test_missing_check_alongside_a_real_fail_names_both(self):
        rows = all_rows(drop={"federation"},
                        override={"parked-timers":
                                   {"status": "fail", "summary": "11 of 12 parked"}})
        result = PROBE.probe_zone_health(runner=fake_runner(rows))
        self.assertEqual(result["monitor"], "broken")
        named = sorted(v["check"] for v in result["violations"])
        self.assertEqual(named, ["federation", "parked-timers"])
        self.assertEqual(result["violation-count"], 2)


if __name__ == "__main__":
    unittest.main()
