import subprocess
import tempfile
import unittest
from pathlib import Path

import gates


APM_REPO = Path("/home/joe/code/apm-lean")
GOLDENS = {
    "a96J04": {
        "revision": "33575db",
        "outcome": "closed",
        "theorem": "ac_monotone_maps_null_to_null",
        "sorries": 0,
        "boundary": True,
        "sorry-axiom": False,
        "statement-hash": "sha256:3621ac37a02bb85a3a779bfacd2984b728b5aece2555e4c895ff5a20426754fe",
    },
    "a96J08": {
        "revision": "37192e1",
        "outcome": "partial",
        "theorem": "apm_a96J08",
        "sorries": 1,
        "boundary": True,
        "sorry-axiom": True,
        "statement-hash": "sha256:df0a2dedcc57f5d7dd3e9b2888d2417fbb86a98c5dfa6f5632a5581563e17ae4",
    },
    "a96J07": {
        "revision": "462b48a",
        "outcome": "closed",
        "theorem": "apm_a96J07",
        "sorries": 0,
        "boundary": True,
        "sorry-axiom": False,
        "statement-hash": "sha256:83e07f98c3c9e0e7516c7682e4cb437f00cedc5217952b4793e9a66dbfc234f6",
    },
}


class StaticGateTests(unittest.TestCase):
    def test_comment_stripped_sorry_count(self):
        source = """theorem demo : True := by
  -- sorry
  /- outer sorry /- nested sorry -/ still comment -/
  have text : String := "not-a-hole -- /-"
  sorry
"""
        self.assertEqual(1, gates.count_sorries(source))
        self.assertEqual([5], gates.sorry_sites(source))

    def test_statement_hash_is_stable_under_whitespace_reformatting(self):
        compact = "theorem demo (n : Nat) : n = n := by rfl\n"
        reformatted = """theorem   demo
    (n : Nat) :
    n = n    := by
  rfl
"""
        self.assertEqual(gates.statement_hash(compact)[2], gates.statement_hash(reformatted)[2])

    def test_first_theorem_not_first_lemma_is_main(self):
        source = "lemma helper : True := by trivial\n\ntheorem main_result : True := by trivial\n"
        name, normalized = gates.extract_main_statement(source)
        self.assertEqual("main_result", name)
        self.assertEqual("theorem main_result : True :=", normalized)

    def test_theorem_name_is_qualified_after_namespace_closes(self):
        source = """namespace Outer.Inner
theorem result : True := by trivial
end Outer.Inner
"""
        self.assertEqual("Outer.Inner.result", gates.qualified_theorem_name(source, "result"))

    def test_conforming_boundary(self):
        source = """theorem demo : True := by
  -- Searched Mathlib for `missing_bridge`.
  -- Tried the direct route first.
  -- The blocker is the absent conversion lemma.
  -- It requires a local finite-sum bridge.
  -- The remaining route is induction on the cover.
  sorry
"""
        result = gates.boundary_conformance(source)
        self.assertTrue(result["conforming"])
        self.assertEqual(5, result["sites"][0]["comment-lines"])

    def test_thin_boundary_is_nonconforming(self):
        source = """theorem demo : True := by
  -- blocker: `missing_bridge`
  -- searched Mathlib
  sorry
"""
        self.assertFalse(gates.boundary_conformance(source)["conforming"])

    def test_unterminated_comment_is_rejected(self):
        with self.assertRaises(gates.GateError):
            gates.strip_comments("theorem demo : True := by /- sorry")

    def test_sorry_axiom_contradiction_is_defective(self):
        build = {"exit-code": 0}
        boundary = {"conforming": True}
        with self.subTest("zero-sorries-with-sorryAx"):
            outcome, reasons = gates._classify(
                build,
                0,
                boundary,
                {"exit-code": 0, "line": "'demo' depends on axioms: [sorryAx]"},
            )
            self.assertEqual("defective", outcome)
            self.assertEqual(["sorry-count-axiom-contradiction"], reasons)
        with self.subTest("sorries-without-sorryAx"):
            outcome, reasons = gates._classify(
                build,
                1,
                boundary,
                {"exit-code": 0, "line": "'demo' depends on axioms: [propext]"},
            )
            self.assertEqual("defective", outcome)
            self.assertEqual(["sorry-count-axiom-contradiction"], reasons)


class HistoricalIntegrationTests(unittest.TestCase):
    maxDiff = None

    def run_golden(self, problem_id):
        expected = GOLDENS[problem_id]
        repository_path = f"problems/{problem_id}/lean/Main.lean"
        shown = subprocess.run(
            ["git", "show", f"{expected['revision']}:{repository_path}"],
            cwd=APM_REPO,
            capture_output=True,
            text=True,
            timeout=30,
            check=True,
        )
        with tempfile.TemporaryDirectory(prefix=f"apm-driver-{problem_id}-") as directory:
            lean_file = Path(directory) / "Main.lean"
            lean_file.write_text(shown.stdout, encoding="utf-8")
            result = gates.gate_path(lean_file, repo_root=APM_REPO, timeout_seconds=900)
        gate_results = result["gate-results"]
        actual = {
            "outcome": result["outcome"],
            "theorem": gate_results["theorem-name"],
            "sorries": gate_results["sorries"],
            "boundary": gate_results["boundary-conforming"],
            "sorry-axiom": "sorryAx" in (gate_results["axioms"]["line"] or ""),
            "statement-hash": result["statement-hash"],
        }
        self.assertEqual(
            {key: value for key, value in expected.items() if key != "revision"},
            actual,
        )
        self.assertEqual(0, gate_results["build"]["exit-code"])
        self.assertEqual(0, gate_results["axioms"]["exit-code"])
        self.assertEqual([], gate_results["reasons"])
        return result

    def test_a96j04_closed_at_33575db(self):
        self.run_golden("a96J04")

    def test_a96j08_partial_at_37192e1(self):
        self.run_golden("a96J08")

    def test_a96j07_closed_at_462b48a(self):
        self.run_golden("a96J07")


if __name__ == "__main__":
    unittest.main()
