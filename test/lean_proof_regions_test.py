import tempfile
import unittest
from pathlib import Path

from lean_proof_regions import extract


class ProofRegionTest(unittest.TestCase):
    def test_extracts_declarations_dependencies_and_nested_branches(self):
        directory = tempfile.TemporaryDirectory()
        self.addCleanup(directory.cleanup)
        source = Path(directory.name) / "Main.lean"
        source.write_text("""lemma helper : True := by exact True.intro
theorem result : True := by
  constructor
  · exact helper
  · refine ?_
    · exact helper
""")
        result = extract(source, "fixture/Main.lean")
        self.assertEqual(result["source"], "fixture/Main.lean")
        self.assertEqual(result["declarations"][1]["declared_uses"], ["helper"])
        branches = [r for r in result["regions"] if r["kind"] == "tactic-branch"]
        self.assertEqual([r["parent"] for r in branches],
                         ["decl:result", "decl:result", "decl:result:branch:2"])
        self.assertEqual(len(result["source_sha256"]), 64)


if __name__ == "__main__":
    unittest.main()
