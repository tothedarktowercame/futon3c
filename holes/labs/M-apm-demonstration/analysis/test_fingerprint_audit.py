#!/usr/bin/env python3
import importlib.util
import pathlib
import tempfile
import unittest
from unittest import mock


HERE = pathlib.Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location(
    "fingerprint_audit", HERE / "fingerprint_audit.py")
AUDIT = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(AUDIT)


class RecordedUseKindsTest(unittest.TestCase):
    def test_compact_map_and_inclusion_reason_are_accepted(self):
        receipt = '''
          :memory-use/kinds {"e-api" :substitutive}
          :memory-use/inclusion-reasons
          [{:memory-id "e-process" :reason "reviewed"
            :memory-use/kind :regulative}]
        '''
        self.assertEqual(
            {"e-api": "substitutive", "e-process": "regulative"},
            AUDIT.recorded_use_kinds(receipt))

    def test_missing_kind_is_not_guessed(self):
        self.assertEqual({}, AUDIT.recorded_use_kinds(
            ':used-ids ["e-historical"] :reason "rewrite rule"'))


class ArtifactVerdictTest(unittest.TestCase):
    def test_regulative_use_is_not_mislabeled_unwitnessed(self):
        self.assertEqual(
            "not-adjudicable-by-token",
            AUDIT.artifact_verdict("regulative", 0, [], [], []))

    def test_regulative_gate_does_not_turn_process_text_into_token_evidence(self):
        self.assertEqual(
            "not-adjudicable-by-token",
            AUDIT.artifact_verdict("regulative", 0, ["Some.lemma"],
                                   ["Some.lemma"], ["Some.lemma"]))

    def test_substitutive_and_historical_rows_keep_existing_semantics(self):
        self.assertEqual("fingerprinted", AUDIT.artifact_verdict(
            "substitutive", 0, ["Some.lemma"], ["Some.lemma"], ["Some.lemma"]))
        self.assertEqual("unwitnessed", AUDIT.artifact_verdict(
            None, 0, [], [], []))
        self.assertEqual("paste", AUDIT.artifact_verdict(
            None, 3, [], [], []))


class VoidedFramePopulationTest(unittest.TestCase):
    def write_attempt(self, frame_dir, used_ids):
        live = frame_dir / "live"
        live.mkdir(parents=True)
        ids = " ".join(f'"{mid}"' for mid in used_ids)
        (live / "student-attempt-1.edn").write_text(
            f'{{:receipt {{:receipt/frame-id "{frame_dir.name.rsplit("-", 1)[-1]}" '
            f':problem-id "p" :used-ids [{ids}]}}}}', encoding="utf-8")

    def test_void_receipts_remain_diagnostic_but_leave_experiment_tallies(self):
        with tempfile.TemporaryDirectory() as td:
            campaign = pathlib.Path(td) / "campaign"
            voided = campaign / "campaign-f59"
            included = campaign / "campaign-f60"
            self.write_attempt(voided, ["e-void-use"])
            self.write_attempt(included, [])
            (voided / "ledger.edn").write_text(
                '{:event/type :frame/stopped '
                ':event/body {:certificate {:certificate/type :frame-void '
                ':classification :apparatus-invalidated}}}\n', encoding="utf-8")

            with mock.patch.object(AUDIT, "master_problem_count", return_value=100):
                result = AUDIT.audit_campaign(str(campaign), "campaign")

            self.assertEqual(1, result["summary"]["attempts"])
            self.assertEqual(0, result["summary"]["use-events"])
            self.assertEqual(1, result["summary"]["excluded-void-frames"])
            self.assertEqual(1, result["summary"]["excluded-attempts"])
            self.assertEqual(1, result["summary"]["excluded-use-events"])
            self.assertEqual(
                [{"frame": "f59", "classification": "apparatus-invalidated",
                  "reason": "void-frame"}], result["excluded-frames"])
            self.assertEqual("excluded-void-frame", result["rows"][0]["verdict"])
            self.assertFalse(result["rows"][0]["experimental-evidence"])


if __name__ == "__main__":
    unittest.main()
