#!/usr/bin/env python3
"""Acceptance tests for per-probe --run/--assemble in ways-of-working-probe.py.

Stub probes are injected by patching build_probe_registry; every run uses a
temporary results dir. The default results dir is never written.
"""
import datetime
import importlib.util
import io
import json
import pathlib
import tempfile
import unittest
from contextlib import redirect_stdout

HERE = pathlib.Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("wow_probe", HERE / "ways-of-working-probe.py")
wow = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(wow)

DECL = {
    "as-of": "2026-11-04", "states": {"on/holding": "good"},
    "provenance": "test", "ways": [
        {"name": "alpha", "probe": "alpha"},
        {"name": "beta", "probe": "beta", "scope-note": "beta only"},
    ]}

def stub_registry(probes):
    def build(_summary=None, _graph=None):
        return probes
    return build

class AssembleTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = pathlib.Path(self.tmp.name)
        self.decl = root / "decl.json"
        self.decl.write_text(json.dumps(DECL))
        self.results = root / "results"
        self.out_a, self.out_b = root / "full.json", root / "assembled.json"

    def tearDown(self):
        self.tmp.cleanup()

    def run_main(self, *args):
        with redirect_stdout(io.StringIO()):
            wow.main(list(args))

    def strip_times(self, doc):
        doc = json.loads(json.dumps(doc))
        doc["probed-at"] = None
        for w in doc["ways"]:
            w["live"]["observed-at"] = None
            w["live"]["last-observation"] = None
        return doc

    def test_full_run_equals_assembly(self):  # A
        wow.build_probe_registry = stub_registry({
            "alpha": lambda: {"monitor": "on", "invariant": "holding",
                              "observed-at": wow.iso(wow.NOW), "violations": [],
                              "violation-count": 0, "evidence": ["alpha fine"]},
            "beta": lambda: {"monitor": "on", "invariant": "failing",
                             "observed-at": wow.iso(wow.NOW), "violations": [{"x": 1}],
                             "violation-count": 1, "evidence": ["beta bad"]}})
        self.run_main(str(self.decl), str(self.out_a),
                      "--results-dir", str(self.results))
        for name in ("alpha", "beta"):
            self.run_main(str(self.decl), "--run", name, "--results-dir", str(self.results))
        self.run_main(str(self.decl), str(self.out_b), "--assemble",
                      "--results-dir", str(self.results))
        self.assertEqual(self.strip_times(json.loads(self.out_a.read_text())),
                         self.strip_times(json.loads(self.out_b.read_text())))

    def test_missing_result_is_broken_with_no_result_yet(self):  # B
        ok = {"monitor": "on", "invariant": "holding", "observed-at": wow.iso(wow.NOW),
              "violations": [], "violation-count": 0, "evidence": ["ok"]}
        wow.build_probe_registry = stub_registry({"alpha": lambda: ok, "beta": lambda: ok})
        self.run_main(str(self.decl), "--run", "alpha", "--results-dir", str(self.results))
        self.run_main(str(self.decl), str(self.out_b), "--assemble",
                      "--results-dir", str(self.results))
        ways = {w["name"]: w["live"] for w in json.loads(self.out_b.read_text())["ways"]}
        self.assertEqual("broken", ways["beta"]["monitor"])
        self.assertEqual("unknown", ways["beta"]["invariant"])
        self.assertEqual("no result yet for probe beta", ways["beta"]["evidence"][0])
        self.assertEqual("scope: beta only", ways["beta"]["evidence"][1])

    def test_raising_probe_writes_own_broken_file_only(self):  # C
        good = {"monitor": "on", "invariant": "holding", "observed-at": wow.iso(wow.NOW),
                "violations": [], "violation-count": 0, "evidence": ["alpha fine"]}
        def boom():
            raise RuntimeError("store down")
        wow.build_probe_registry = stub_registry({"alpha": lambda: dict(good), "beta": boom})
        self.run_main(str(self.decl), "--run", "alpha", "--results-dir", str(self.results))
        before = (self.results / "alpha.json").read_bytes()
        self.run_main(str(self.decl), "--run", "beta", "--results-dir", str(self.results))
        self.assertEqual(before, (self.results / "alpha.json").read_bytes())
        live = json.loads((self.results / "beta.json").read_text())["live"]
        self.assertEqual("broken", live["monitor"])
        self.assertEqual(["probe error: store down"], live["evidence"])

    def test_probed_at_is_oldest_observed_at(self):  # D
        self.results.mkdir(parents=True)
        for name, when in (("alpha", "2026-11-04T10:00:00+00:00"),
                           ("beta", "2026-11-04T09:00:00+00:00")):
            (self.results / f"{name}.json").write_text(json.dumps(
                {"probe": name, "observed-at": when,
                 "live": {"monitor": "on", "invariant": "holding", "observed-at": when,
                          "violations": [], "violation-count": 0, "evidence": ["ok"]}}))
        self.run_main(str(self.decl), str(self.out_b), "--assemble",
                      "--results-dir", str(self.results))
        self.assertEqual("2026-11-04T09:00:00+00:00",
                         json.loads(self.out_b.read_text())["probed-at"])

if __name__ == "__main__":
    unittest.main()
