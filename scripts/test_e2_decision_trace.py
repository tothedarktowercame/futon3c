#!/usr/bin/env python3
from __future__ import annotations

import importlib.util
import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

SCRIPT = Path(__file__).with_name("e2_decision_trace.py")
SPEC = importlib.util.spec_from_file_location("e2_decision_trace_tested", SCRIPT)
trace = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
sys.modules[SPEC.name] = trace
SPEC.loader.exec_module(trace)


def run(repo: Path, *args: str) -> str:
    result = subprocess.run(args, cwd=repo, text=True, check=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    return result.stdout.strip()


class History:
    def __init__(self, root: Path):
        self.root = root
        run(root, "git", "init", "-q")
        run(root, "git", "config", "user.name", "E2 fixture")
        run(root, "git", "config", "user.email", "e2@example.invalid")

    def commit(self, source: str, message: str) -> str:
        (self.root / "Main.lean").write_text(source, encoding="utf-8")
        run(self.root, "git", "add", "Main.lean")
        run(self.root, "git", "commit", "-q", "-m", message)
        return run(self.root, "git", "rev-parse", "HEAD")


BASE = """import Mathlib

theorem alpha (n : Nat) : n = n := by
  rfl

theorem beta : True := by
  trivial
"""


class DecisionTraceTest(unittest.TestCase):
    def make_history(self, root: Path, bodies: list[str]) -> tuple[History, str]:
        history = History(root)
        base = history.commit(BASE, "baseline")
        for index, body in enumerate(bodies):
            history.commit(
                f"""import Mathlib

theorem alpha (n : Nat) : n = n := by
{body}

theorem beta : True := by
  trivial
""",
                f"attempt {index + 1}",
            )
        return history, base

    @staticmethod
    def outcome(name: str):
        return lambda _commit: name

    def test_determinism_is_byte_identical(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary)
            _, base = self.make_history(repo, ["  exact Eq.refl n"])
            first = trace.extract_trace(repo, base, outcome_provider=self.outcome("success"))
            second = trace.extract_trace(repo, base, outcome_provider=self.outcome("success"))
            self.assertEqual(first, second)
            self.assertEqual(
                json.dumps(first, sort_keys=True, separators=(",", ":")),
                json.dumps(second, sort_keys=True, separators=(",", ":")),
            )

    def test_tactic_text_insensitivity_is_load_bearing(self) -> None:
        """Different proofs of the same declaration produce the same identity."""
        with tempfile.TemporaryDirectory() as left_tmp, tempfile.TemporaryDirectory() as right_tmp:
            left = Path(left_tmp)
            right = Path(right_tmp)
            _, left_base = self.make_history(left, ["  exact Eq.refl n"])
            _, right_base = self.make_history(right, ["  simpa using rfl"])
            left_trace = trace.extract_trace(left, left_base,
                                             outcome_provider=self.outcome("success"))
            right_trace = trace.extract_trace(right, right_base,
                                              outcome_provider=self.outcome("success"))
            self.assertEqual([["alpha", "modify-body", "success"]],
                             left_trace["sequence"])
            self.assertEqual(left_trace["sequence"], right_trace["sequence"])
            self.assertEqual(left_trace["sha256"], right_trace["sha256"])

    def test_different_declarations_and_order_are_structurally_sensitive(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary)
            history = History(repo)
            base = history.commit(BASE, "baseline")
            alpha_commit = history.commit(BASE.replace("  rfl", "  exact Eq.refl n"), "alpha")
            history.commit(BASE.replace("  trivial", "  exact True.intro"), "beta")
            alpha_then_beta = trace.extract_trace(
                repo, base, outcome_provider=self.outcome("success"))
            alpha_only = trace.extract_trace(
                repo, base, alpha_commit, outcome_provider=self.outcome("success"))
            self.assertNotEqual(alpha_only["sha256"], alpha_then_beta["sha256"])

        with tempfile.TemporaryDirectory() as reverse_tmp:
            reverse = Path(reverse_tmp)
            history = History(reverse)
            reverse_base = history.commit(BASE, "baseline")
            history.commit(BASE.replace("  trivial", "  exact True.intro"), "beta")
            history.commit(BASE.replace("  rfl", "  exact Eq.refl n"), "alpha")
            beta_then_alpha = trace.extract_trace(
                reverse, reverse_base, outcome_provider=self.outcome("success"))
            self.assertNotEqual(alpha_then_beta["sha256"], beta_then_alpha["sha256"])

    def test_build_outcome_is_structurally_sensitive(self) -> None:
        with tempfile.TemporaryDirectory() as temporary:
            repo = Path(temporary)
            _, base = self.make_history(repo, ["  exact Eq.refl n"])
            hashes = {
                outcome: trace.extract_trace(
                    repo, base, outcome_provider=self.outcome(outcome))["sha256"]
                for outcome in ("success", "error", "sorry-present")
            }
            self.assertEqual(3, len(set(hashes.values())))

    def test_sorry_transitions_take_the_registered_specific_categories(self) -> None:
        before = "theorem alpha : True := by\n  sorry\n"
        after = "theorem alpha : True := by\n  trivial\n"
        self.assertEqual("sorry-removed", trace.structural_edits("Main.lean", before, after)[0].kind)
        self.assertEqual("sorry-introduced", trace.structural_edits("Main.lean", after, before)[0].kind)

    def test_import_removal_fails_closed(self) -> None:
        with self.assertRaisesRegex(trace.TraceError, "no remove-import category"):
            trace.structural_edits("Main.lean", "import Mathlib\n", "")


if __name__ == "__main__":
    unittest.main()
