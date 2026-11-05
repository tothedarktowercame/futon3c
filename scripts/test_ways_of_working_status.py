#!/usr/bin/env python3
"""Fixture tests for the status-known way-of-working probe (way 0).

A way's status counts as known only if (a) fresh within 30 min, (b) readable,
(c) its probe has a bad-case test file, (d) it declares a probe. The probe
never reads its own result; real-users is excluded by design.
"""

import datetime
import importlib.util
import json
import pathlib
import sys
import tempfile
import unittest


SCRIPTS = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPTS))
SPEC = importlib.util.spec_from_file_location(
    "ways_of_working_probe", SCRIPTS / "ways-of-working-probe.py")
PROBE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(PROBE)
UTC = datetime.timezone.utc
NOW = datetime.datetime(2026, 11, 5, 12, 0, tzinfo=UTC)


def iso(dt):
    return dt.replace(microsecond=0).isoformat().replace("+00:00", "Z")


def way(wid, probe):
    return {"id": wid, "probe": probe}


def result(name, observed_at, monitor="on", invariant="holding"):
    return {"probe": name, "observed-at": observed_at, "live":
            {"monitor": monitor, "invariant": invariant,
             "observed-at": observed_at, "violations": [],
             "violation-count": 0, "evidence": [f"{name} fine"]}}


def environment(ways, results):
    root = pathlib.Path(tempfile.mkdtemp(prefix="status-fixture-"))
    scripts = root / "scripts"; scripts.mkdir()
    results_dir = root / "results"; results_dir.mkdir()
    (scripts / "ways-of-working-probe.py").write_text("")  # placeholder only
    for name, payload in results.items():
        (results_dir / f"{name}.json").write_text(json.dumps(payload))
    for suffix in ("feedback", "autoclock"):  # default: probe name == suffix
        (scripts / f"test_ways_of_working_{suffix}.py").write_text("")
    decl = root / "decl.json"
    decl.write_text(json.dumps({"schema": "ways-of-working-v1", "ways": ways}))
    return decl, results_dir, scripts


def probes_with(ways, results):
    decl, results_dir, scripts = environment(ways, results)
    return PROBE.probe_status_known(now=NOW, decl=decl,
                                    results_dir=results_dir, scripts_dir=scripts)


def fails_for(live, way_id):
    return next((v["fails"] for v in live["violations"] if v["way"] == way_id), None)


class StatusKnownProbeTest(unittest.TestCase):
    def test_all_good_is_holding(self):
        live = probes_with(
            [way("w-good", "feedback"), way("status-is-known", "status-known"),
             way("real-users", "none")],
            {"feedback": result("feedback", iso(NOW - datetime.timedelta(minutes=5)))})
        self.assertEqual("on", live["monitor"])
        self.assertEqual("holding", live["invariant"])
        self.assertEqual(0, live["violation-count"])

    def test_stale_result_fails_a(self):
        live = probes_with(
            [way("w-stale", "feedback")],
            {"feedback": result("feedback", iso(NOW - datetime.timedelta(hours=2)))})
        self.assertEqual("failing", live["invariant"])
        self.assertIn("a-fresh", fails_for(live, "w-stale"))

    def test_broken_result_fails_b(self):
        live = probes_with(
            [way("w-broken", "feedback")],
            {"feedback": result("feedback", iso(NOW), monitor="broken", invariant="unknown")})
        self.assertEqual("failing", live["invariant"])
        self.assertIn("b-readable", fails_for(live, "w-broken"))

    def test_probe_without_test_file_fails_c(self):
        # the fixture scripts dir has test files only for feedback and
        # autoclock, so a declared git-clean probe has no test file
        decl, results_dir, scripts = environment(
            [way("w-untested", "git-clean")],
            {"git-clean": result("git-clean", iso(NOW))})
        live = PROBE.probe_status_known(now=NOW, decl=decl, results_dir=results_dir,
                                        scripts_dir=scripts)
        self.assertIn("c-can-fail", fails_for(live, "w-untested"))

    def test_none_probe_fails_d(self):
        live = probes_with([way("w-prose", "none")], {})
        self.assertEqual(["d-declared"], fails_for(live, "w-prose"))

    def test_real_users_excluded_and_self_never_read(self):
        live = probes_with(
            [way("status-is-known", "status-known"), way("real-users", "none")],
            {})
        self.assertEqual(0, live["violation-count"])
        self.assertTrue(any("real-users" in e for e in live["evidence"]))


if __name__ == "__main__":
    unittest.main()
