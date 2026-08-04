import unittest

import render


PHASE_A = {"problem_id": "a96J09", "bundle_path": "/x/problems/a96J09/"}
PHASE_B = {"problem_id": "a96J09", "bundle_path": "/x/problems/a96J09/",
           "main_lean_path": "problems/a96J09/lean/Main.lean"}
CLOSER = {"problem_id": "a96J09", "hop_n": "1",
          "main_lean_path": "problems/a96J09/lean/Main.lean",
          "base_commit": "abc1234", "sorry_count": "1",
          "boundary_excerpt": "-- searched Foo.bar; bridge = baz",
          "statement_hash": "sha256:deadbeef"}
SCRIBE = {"problem_id": "a96J09", "session_jobs": "invoke-1, invoke-2",
          "commit_sha": "abc1234", "output_dir": "holes/x/a96J09-scribe"}


class RenderTest(unittest.TestCase):
    def test_all_templates_render_with_full_params(self):
        for name, params in [("phase-a", PHASE_A), ("phase-b", PHASE_B),
                             ("closer", CLOSER), ("scribe", SCRIBE)]:
            out = render.render(name, params)
            self.assertNotIn("{{", out, name)
            self.assertIn("a96J09", out, name)

    def test_missing_param_raises(self):
        with self.assertRaises(render.RenderError):
            render.render("phase-a", {"problem_id": "a96J09"})

    def test_unused_param_raises(self):
        with self.assertRaises(render.RenderError):
            render.render("phase-a", {**PHASE_A, "extra": "x"})

    def test_unknown_template_raises(self):
        with self.assertRaises(render.RenderError):
            render.render("nonexistent", {})

    def test_leakage_check_catches_whole_word(self):
        with self.assertRaises(render.RenderError) as ctx:
            render.render("phase-a", PHASE_A,
                          forbidden_terms=["reconnaissance"])
        self.assertIn("leakage", str(ctx.exception))

    def test_leakage_check_is_word_bounded(self):
        # 'recon' appears only inside 'reconnaissance'; word-bounded
        # matching must NOT flag it.
        out = render.render("phase-a", PHASE_A, forbidden_terms=["recon"])
        self.assertIn("reconnaissance", out)

    def test_closer_carries_frozen_hash_and_excerpt(self):
        out = render.render("closer", CLOSER)
        self.assertIn("sha256:deadbeef", out)
        self.assertIn("searched Foo.bar", out)


if __name__ == "__main__":
    unittest.main()
