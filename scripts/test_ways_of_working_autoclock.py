#!/usr/bin/env python3
"""Fixture tests for the autoclock-gate way-of-working probe (E-autoclock-wow).

The probe reports the gate's state from facts: whether `admit-request` sits
at the `create-invoke-job!` seam, and how many `E-kimi-task-*` excursions
were minted in the last 24 h (the workaround the gate should replace).
With no gate the reading must be on/failing, never holding.
"""

import datetime
import importlib.util
import pathlib
import subprocess
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

SEAM_WITHOUT_GATE = """(ns fake.http)
(defn- create-invoke-job!
  [request]
  (let [job-id (str request)]
    job-id))
(defn other-fn [] (admit-request))  ; marker elsewhere must not count
"""

SEAM_WITH_GATE = """(ns fake.http)
(defn- create-invoke-job!
  [request]
  (let [admitted (admit-request request)]
    admitted))
(defn other-fn [] nil)
"""


def git(repo, *args, env=None):
    subprocess.run(["git", "-C", str(repo), *args], check=True,
                   capture_output=True, text=True, env=env)


def mint_repo(fresh=True, stale=True):
    """A git repo with E-kimi-task files committed at chosen times."""
    repo = pathlib.Path(tempfile.mkdtemp(prefix="autoclock-fixture-"))
    (repo / "holes" / "excursions").mkdir(parents=True)
    git(repo, "init", "-q")
    git(repo, "config", "user.email", "t@t")
    git(repo, "config", "user.name", "t")
    def commit(name, when, content):
        path = repo / "holes" / "excursions" / name
        path.write_text(f"# {name}\n\n{content}\n")
        env = {"GIT_AUTHOR_DATE": when, "GIT_COMMITTER_DATE": when,
               "PATH": "/usr/bin:/bin", "HOME": str(repo),
               "GIT_CONFIG_NOSYSTEM": "1"}
        git(repo, "add", str(path))
        git(repo, "commit", "-q", "-m", name, env=env)
    if stale:
        commit("E-kimi-task-1.md",
               "2026-11-02T12:00:00+00:00", "old workaround file")
    if fresh:
        commit("E-kimi-task-2.md",
               "2026-11-05T06:00:00+00:00", "fresh workaround file")
    return repo


def seam_file(text):
    d = pathlib.Path(tempfile.mkdtemp(prefix="autoclock-seam-"))
    p = d / "http.clj"
    p.write_text(text)
    return p


class AutoclockGateProbeTest(unittest.TestCase):
    def test_no_gate_and_fresh_mints_is_failing(self):
        live = PROBE.probe_autoclock_gate(
            now=NOW, repo=mint_repo(fresh=True, stale=True),
            seam=seam_file(SEAM_WITHOUT_GATE))
        self.assertEqual("on", live["monitor"])
        self.assertEqual("failing", live["invariant"])
        checks = {v["check"]: v for v in live["violations"]}
        self.assertIn("gate-not-built", checks)
        self.assertIn("minted-excursion-workaround", checks)
        self.assertEqual(1, len(checks["minted-excursion-workaround"]["files"]))
        self.assertEqual(["holes/excursions/E-kimi-task-2.md"],
                         checks["minted-excursion-workaround"]["files"])

    def test_gate_present_no_fresh_mints_is_holding(self):
        live = PROBE.probe_autoclock_gate(
            now=NOW, repo=mint_repo(fresh=False, stale=True),
            seam=seam_file(SEAM_WITH_GATE))
        self.assertEqual("on", live["monitor"])
        self.assertEqual("holding", live["invariant"])
        self.assertEqual(0, live["violation-count"])

    def test_no_gate_with_no_fresh_mints_is_still_failing(self):
        # never holding without the gate: nothing refuses anything yet
        live = PROBE.probe_autoclock_gate(
            now=NOW, repo=mint_repo(fresh=False, stale=True),
            seam=seam_file(SEAM_WITHOUT_GATE))
        self.assertEqual("failing", live["invariant"])
        self.assertEqual(1, live["violation-count"])
        self.assertEqual("gate-not-built", live["violations"][0]["check"])

    def test_marker_outside_seam_body_does_not_count_as_gate(self):
        self.assertFalse(PROBE.gate_present_at_seam(seam=seam_file(SEAM_WITHOUT_GATE)))
        self.assertTrue(PROBE.gate_present_at_seam(seam=seam_file(SEAM_WITH_GATE)))

    def test_marker_in_a_comment_inside_the_seam_is_not_a_gate(self):
        text = ("(defn create-invoke-job!\n  [req]\n"
                "  ;; TODO: call admit-request here\n  (enqueue! req))\n"
                "(defn other [] nil)\n")
        self.assertFalse(PROBE.gate_present_at_seam(seam=seam_file(text)))

    def test_unreadable_repo_is_broken_not_holding(self):
        live = PROBE.probe_autoclock_gate(
            now=NOW, repo=pathlib.Path("/nonexistent-repo"),
            seam=seam_file(SEAM_WITHOUT_GATE))
        self.assertEqual("broken", live["monitor"])
        self.assertEqual("unknown", live["invariant"])


if __name__ == "__main__":
    unittest.main()
