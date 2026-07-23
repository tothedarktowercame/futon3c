#!/usr/bin/env python3

import importlib.util
import tempfile
import unittest
from pathlib import Path
from unittest import mock


SCRIPT = Path(__file__).with_name("apm_formal_zai_cron.py")
SPEC = importlib.util.spec_from_file_location("apm_formal_zai_cron", SCRIPT)
cron = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(cron)


class QuotaTests(unittest.TestCase):
    def test_normalizes_both_token_windows_and_ignores_mcp_limit(self):
        limits = cron.quota_snapshot(
            {
                "success": True,
                "data": {
                    "limits": [
                        {"type": "TOKENS_LIMIT", "unit": 3, "number": 5,
                         "percentage": 10, "nextResetTime": 100},
                        {"type": "TOKENS_LIMIT", "unit": 6, "number": 1,
                         "percentage": 34, "nextResetTime": 200},
                        {"type": "TIME_LIMIT", "percentage": 99},
                    ]
                },
            }
        )
        self.assertEqual([90.0, 66.0], [item["available"] for item in limits])

    def test_exactly_half_available_is_closed_because_threshold_is_strict(self):
        with self.assertRaises(cron.GateClosed):
            cron.enforce_quota(
                [{"unit": 3, "number": 5, "used": 50.0,
                  "available": 50.0, "next_reset_ms": 0}]
            )

    def test_missing_token_limits_fails_closed(self):
        with self.assertRaises(cron.GateClosed):
            cron.quota_snapshot({"success": True, "data": {"limits": []}})


class AgencyTests(unittest.TestCase):
    def agent(self, status="restored", ready=True, route="local"):
        return {"type": "zai", "status": status, "invoke-ready?": ready,
                "invoke-route": route, "metadata": {}}

    def test_allows_one_other_invoking_and_picks_numeric_first_available(self):
        agent, invoking = cron.choose_agent(
            {"zai-10": self.agent(), "zai-2": self.agent(),
             "zai-1": self.agent(status="invoking")}
        )
        self.assertEqual(("zai-2", 1), (agent, invoking))

    def test_rejects_two_other_invoking(self):
        with self.assertRaises(cron.GateClosed):
            cron.choose_agent(
                {"zai-1": self.agent(status="invoking"),
                 "zai-2": self.agent(status="invoking"),
                 "zai-3": self.agent()}
            )


class CandidateTests(unittest.TestCase):
    def test_candidate_requires_informal_and_no_attempt_or_claim(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            informal = root / "informal"
            apm = root / "apm"
            state = root / "state"
            informal.mkdir()
            for pid in ("a00J01", "a00J02", "a00J03"):
                (informal / f"apm-{pid}.md").write_text("proof\n" * 30)
                bundle = apm / "problems" / pid
                bundle.mkdir(parents=True)
                for name in ("problem.tex", "problem.md", "informal-solution.md",
                             "proof-outline.md"):
                    (bundle / name).write_text("present\n")
                (bundle / "status.json").write_text(
                    '{"classification":"informal-only","lean":{"main":null}}\n'
                )
            (apm / "problems/a00J02/lean").mkdir(parents=True)
            (apm / "problems/a00J02/lean/Main.lean").write_text(
                "example : True := by trivial\n"
            )
            (state / "claims").mkdir(parents=True)
            (state / "claims/a00J03.json").write_text("{}\n")
            with (
                mock.patch.object(cron, "INFORMAL_DIR", informal),
                mock.patch.object(cron, "APM_LEAN_DIR", apm),
                mock.patch.object(cron, "STATE_DIR", state),
                mock.patch.object(cron, "OVERRIDES_PATH", root / "missing.tsv"),
            ):
                self.assertEqual(["a00J01"], cron.candidate_problem_ids())


class ProgressTests(unittest.TestCase):
    def test_progress_snapshot_counts_clean_partial_and_unattempted_bundles(self):
        with tempfile.TemporaryDirectory() as tmp:
            apm = Path(tmp)
            source = apm / "apm"
            source.mkdir()
            for pid in ("a00J01", "a00J02", "a00J03"):
                (source / f"{pid}.tex").write_text("problem\n")
                (apm / "problems" / pid).mkdir(parents=True)
            (apm / "problems/a00J01/informal-solution.md").write_text("proof\n" * 30)
            (apm / "problems/a00J01/lean").mkdir()
            (apm / "problems/a00J01/lean/Main.lean").write_text(
                "example : True := by trivial\n"
            )
            (apm / "problems/a00J02/lean").mkdir()
            (apm / "problems/a00J02/lean/Main.lean").write_text(
                "-- sorry in a comment\n"
                "/- outer sorry /- nested sorry -/ comment -/\n"
                "def documentation := \"sorry in a string\"\n"
                "def «sorry» := True\n"
                "example : True := by\n  sorry\n"
            )
            (apm / "problems/a00J03/candidates/old/lean").mkdir(parents=True)
            (apm / "problems/a00J03/candidates/old/lean/Main.lean").write_text(
                "example : True := by\n  sorry\n"
            )
            with mock.patch.object(cron, "APM_LEAN_DIR", apm):
                snapshot = cron.proof_progress_snapshot()
            self.assertEqual("apm-formal-progress.v2", snapshot["schema"])
            self.assertEqual("current-lean-code.v1", snapshot["metric_version"])
            self.assertEqual(3, snapshot["total"])
            self.assertEqual(1, snapshot["informal"])
            self.assertEqual(2, snapshot["lean_total"])
            self.assertEqual(1, snapshot["lean_with_sorry"])
            self.assertEqual(1, snapshot["lean_clean"])
            self.assertEqual(1, snapshot["sorries"])
            self.assertEqual(2, snapshot["remaining"])

    def test_append_progress_snapshot_writes_jsonl(self):
        with tempfile.TemporaryDirectory() as tmp:
            progress_path = Path(tmp) / "formal-progress.jsonl"
            snapshot = {"schema": "apm-formal-progress.v2", "lean_clean": 7}
            with (
                mock.patch.object(cron, "PROGRESS_PATH", progress_path),
                mock.patch.object(cron, "proof_progress_snapshot", return_value=snapshot),
            ):
                self.assertIs(snapshot, cron.append_progress_snapshot())
            self.assertEqual(snapshot, cron.json.loads(progress_path.read_text()))


if __name__ == "__main__":
    unittest.main()
