#!/usr/bin/env python3
import importlib.util
import pathlib
import unittest


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


if __name__ == "__main__":
    unittest.main()
