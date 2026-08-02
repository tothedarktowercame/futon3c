#!/usr/bin/env python3
import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path

from edn_format import Keyword as K, dumps


SCRIPT = Path(__file__).with_name("runner_gate.py")
SPEC = importlib.util.spec_from_file_location("runner_gate_tested", SCRIPT)
gate = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
sys.modules[SPEC.name] = gate
SPEC.loader.exec_module(gate)


class RunnerGateTests(unittest.TestCase):
    def test_compliant_and_incomplete_with_named_ids(self):
        with tempfile.TemporaryDirectory() as tmp:
            state = Path(tmp)
            ok = gate.adjudicate(
                gate.Run("codex-1", "job-ok", "USED e-alpha00: carried route\n", ["e-alpha00"]),
                [gate.UseAttributionGate()], state_dir=state,
            )
            bad = gate.adjudicate(
                gate.Run("codex-1", "job-bad", "Memory usage: vague\n", ["e-alpha00"]),
                [gate.UseAttributionGate()], state_dir=state,
            )
        self.assertTrue(ok["counts_toward_endpoints"])
        self.assertEqual("attribution-incomplete", bad["run_status"])
        self.assertEqual(["e-alpha00"], bad["missing_ids"])

    def test_ledger_and_deposit_are_idempotent_by_job(self):
        deposits = []
        with tempfile.TemporaryDirectory() as tmp:
            state = Path(tmp)
            run = gate.Run("codex-2", "same-job", "n/a", ["e-alpha00"])
            deposit = lambda _v, _m: deposits.append("called") or {"memory_id": "e-fix0000"}
            first = gate.adjudicate(run, [gate.UseAttributionGate()], state_dir=state, deposit=deposit)
            second = gate.adjudicate(run, [gate.UseAttributionGate()], state_dir=state, deposit=deposit)
            lines = (state / "agents/codex-2/violations.jsonl").read_text().splitlines()
        self.assertEqual(1, len(lines))
        self.assertEqual(["called"], deposits)
        self.assertEqual("reject-push-back", first["verdict"])
        self.assertTrue(second["idempotent_replay"])

    def test_threshold_sets_stop_and_meta_record(self):
        with tempfile.TemporaryDirectory() as tmp:
            state = Path(tmp)
            with self.subTest("threshold"):
                old = gate.STOP_THE_LINE_THRESHOLD
                gate.STOP_THE_LINE_THRESHOLD = 2
                try:
                    for index in range(2):
                        result = gate.adjudicate(
                            gate.Run("codex-3", f"job-{index}", "n/a", ["e-alpha00"]),
                            [gate.UseAttributionGate()], state_dir=state,
                        )
                finally:
                    gate.STOP_THE_LINE_THRESHOLD = old
            self.assertEqual("stop-the-line", result["verdict"])
            self.assertTrue(gate.is_stopped("codex-3", state_dir=state))
            self.assertTrue((state / "meta-learning.jsonl").exists())

    def test_reviewed_correction_is_supplied_once_on_next_dispatch(self):
        with tempfile.TemporaryDirectory() as tmp:
            state = Path(tmp)
            gate.adjudicate(
                gate.Run("codex-5", "bad-job", "n/a", ["e-alpha00"]),
                [gate.UseAttributionGate()], state_dir=state,
                deposit=lambda _v, _m: {
                    "memory_id": "e-correction00", "attachment_status": ":reviewed"
                },
            )
            packet = gate.correction_packet("codex-5", state_dir=state)
            gate.mark_corrections_delivered("codex-5", "next-job", state_dir=state)
            after = gate.correction_packet("codex-5", state_dir=state)
        self.assertIn("e-correction00", packet)
        self.assertEqual("", after)

    def test_gate_error_fails_safe_without_crashing(self):
        class Broken:
            norm = "use-attribution"

            def check(self, _run):
                raise ValueError("boom")

        with tempfile.TemporaryDirectory() as tmp:
            result = gate.adjudicate(
                gate.Run("codex-4", "broken", "report", ["e-alpha00"]),
                [Broken()], state_dir=Path(tmp),
            )
        self.assertEqual("review-required", result["verdict"])
        self.assertFalse(result["counts_toward_endpoints"])

    def test_receipts_audit_counts_use_and_coverage_separately(self):
        fixture = {
            K("entries"): [
                {K("evidence/body"): {K("memory-use"): {
                    K("memory-use/surfaced-ids"): ["e-one000", "e-two000"],
                    K("memory-use/used-ids"): [], K("memory-use/rejected-ids"): [],
                }}},
                {K("evidence/body"): {K("memory-use"): {
                    K("memory-use/surfaced-ids"): ["e-three00"],
                    K("memory-use/used-ids"): ["e-three00"],
                    K("memory-use/rejected-ids"): [],
                }}},
            ]
        }
        with tempfile.TemporaryDirectory() as tmp:
            artifact = Path(tmp) / "receipts.edn"
            artifact.write_text(dumps(fixture), encoding="utf-8")
            result = gate.audit_receipts_export(artifact)
        self.assertEqual(2, result["applicable_receipt_rows"])
        self.assertEqual(1, result["rows_with_no_recorded_use"])
        self.assertEqual(1, result["rows_incomplete_under_used_or_rejected_coverage"])


if __name__ == "__main__":
    unittest.main()
