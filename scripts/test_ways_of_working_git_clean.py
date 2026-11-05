#!/usr/bin/env python3
"""Bad-case test for the git-clean (inbox-zero) way-of-working probe.

Constructs the population defects the 2026-11-04 audit found (zai-6 report):
a leaked .repair-limbs-fixture-* dir (excluded, counted), a repo with no
remote at all holding commits (violation), and a worktree on an unpushed
branch with no @{u} (violation). Master's probe misses the last two and
counts the fixture as "no upstream".
"""
import importlib.util
import pathlib
import subprocess
import sys
import tempfile
import unittest

SCRIPTS = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPTS))
SPEC = importlib.util.spec_from_file_location("ways_of_working_probe", SCRIPTS / "ways-of-working-probe.py")
PROBE = importlib.util.module_from_spec(SPEC); SPEC.loader.exec_module(PROBE)

def git(cwd, *cmd):
    subprocess.run(("git",) + tuple(cmd), cwd=str(cwd), check=True,
                   capture_output=True, text=True)

class GitCleanPopulationTest(unittest.TestCase):
    def setUp(self):
        self.outer = tempfile.TemporaryDirectory()   # remotes live outside CODE
        self.inner = tempfile.TemporaryDirectory()   # this is CODE
        code = pathlib.Path(self.inner.name)
        origin = pathlib.Path(self.outer.name) / "origin.git"
        subprocess.run(["git", "init", "-q", "--bare", "-b", "master", str(origin)], check=True)
        # clean, fully pushed repo "a"
        a = code / "a"
        subprocess.run(["git", "clone", "-q", str(origin), str(a)], check=True)
        (a / "f.txt").write_text("one\n")
        git(a, "add", "."); git(a, "commit", "-qm", "one")
        git(a, "push", "-q", "-u", "origin", "master")
        # worktree of "a" on an unpushed branch: no @{u}, remote exists
        wt = code / "wt"
        subprocess.run(["git", "-C", str(a), "worktree", "add", "-q", "-b", "topic", str(wt)], check=True)
        (wt / "g.txt").write_text("two\n")
        git(wt, "add", "."); git(wt, "commit", "-qm", "two")
        # repo with no remote at all and one commit (minus40 shape)
        loner = code / "loner"
        subprocess.run(["git", "init", "-q", "-b", "master", str(loner)], check=True)
        (loner / "h.txt").write_text("solo\n")
        git(loner, "add", "."); git(loner, "commit", "-qm", "solo")
        # leaked test fixture, even dirty: excluded, not part of the population
        fx = code / ".repair-limbs-fixture-123"
        subprocess.run(["git", "init", "-q", "-b", "master", str(fx)], check=True)
        (fx / "junk.txt").write_text("junk\n")
        git(fx, "add", "."); git(fx, "commit", "-qm", "junk")
        (fx / "dirty.txt").write_text("dirty\n")
        self.code = code
        self._old_code = PROBE.CODE
        PROBE.CODE = code

    def tearDown(self):
        PROBE.CODE = self._old_code
        self.inner.cleanup(); self.outer.cleanup()

    def test_population(self):
        result = PROBE.probe_git_clean()
        repos = {r["repo"]: r for r in result["violations"]}
        self.assertIn("loner", repos, "single-copy repo with a commit must be a violation")
        self.assertGreaterEqual(repos["loner"]["ahead"], 1)
        self.assertIn("wt", repos, "worktree branch with no upstream and unpushed commits must be a violation")
        self.assertGreaterEqual(repos["wt"]["ahead"], 1)
        self.assertNotIn("a", repos, "clean pushed repo must not be a violation")
        self.assertNotIn(".repair-limbs-fixture-123", repos)
        ev = " | ".join(result["evidence"])
        self.assertIn(".repair-limbs-fixture", ev)
        self.assertIn("1", next(e for e in result["evidence"] if "fixture" in e))
        self.assertIn("5 min", ev)  # probe cadence stated apart from the parked timer

if __name__ == "__main__":
    unittest.main()
