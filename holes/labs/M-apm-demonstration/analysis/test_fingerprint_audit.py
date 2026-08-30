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


class TransferReportTest(unittest.TestCase):
    def test_transfer_distance_comes_from_evidence_provenance(self):
        within = {"origin-author": "f53-scribe", "origin-problem": "a99J03"}
        same = {"origin-author": "f50-guide", "origin-problem": "a99J03"}
        cross = {"origin-author": "f34-guide", "origin-problem": "a95J03"}
        self.assertEqual(("within-frame", "f53"),
                         AUDIT.transfer_stratum("f53", "a99J03", within))
        self.assertEqual(("prior-frame-same-problem", "f50"),
                         AUDIT.transfer_stratum("f53", "a99J03", same))
        self.assertEqual(("cross-problem", "f34"),
                         AUDIT.transfer_stratum("f58", "aunk04", cross))
        self.assertEqual(("unknown", None),
                         AUDIT.transfer_stratum("f58", "aunk04", {}))

    def test_routes_require_id_bearing_receipt_evidence(self):
        receipt = '''
          :accessible-memory-ids ["e-shelf" "e-both"]
          :receipt/memory-cascade {:used-via-cascade ["e-cascade" "e-both"]}
          :queries ["prose says e-search but carries no result receipt"]
        '''
        self.assertEqual("shelf", AUDIT.durable_delivery_route(receipt, "e-shelf"))
        self.assertEqual("cascade", AUDIT.durable_delivery_route(receipt, "e-cascade"))
        self.assertEqual("cascade", AUDIT.durable_delivery_route(receipt, "e-both"))
        self.assertEqual("unknown", AUDIT.durable_delivery_route(receipt, "e-search"))

    def test_f42_f50_f53_f58_receipts_pin_expected_strata_inputs(self):
        root = pathlib.Path(AUDIT.CAMPAIGNS)
        cases = [
            (root / "jit-all-open-nontopology-v1" /
             "jit-all-open-nontopology-v1-f42/live/student-attempt-1.edn",
             "e-f72e5ece-2a26-48aa-a47c-2b6b310caf69", "shelf"),
            (root / "jit-all-open-v2/jit-all-open-v2-f50/live/student-attempt-1.edn",
             "e-63b7c7c1-1906-412c-ae18-b4644762fbea", "shelf"),
            (root / "jit-all-open-v2/jit-all-open-v2-f53/live/student-attempt-2.edn",
             "e-apm-promotion-de30b5ae3706422a549cc710ce4e7841", "shelf"),
            (root / "jit-all-open-v2/jit-all-open-v2-f58/live/student-attempt-1.edn",
             "e-63b7c7c1-1906-412c-ae18-b4644762fbea", "shelf"),
        ]
        for path, memory_id, route in cases:
            with self.subTest(path=path):
                receipt = path.read_text(encoding="utf-8", errors="replace")
                self.assertIn(memory_id, AUDIT.ID_STR.findall(
                    AUDIT.USED_IDS.search(receipt).group(1)))
                self.assertEqual(route,
                                 AUDIT.durable_delivery_route(receipt, memory_id))

    def test_counts_preserve_excluded_rows_without_polluting_experiment(self):
        rows = [
            {"transfer-stratum": "cross-problem", "experimental-evidence": True},
            {"transfer-stratum": "cross-problem", "experimental-evidence": False},
            {"transfer-stratum": "within-frame", "experimental-evidence": True},
        ]
        self.assertEqual(
            {"experimental": {"cross-problem": 1, "within-frame": 1},
             "diagnostic-all-rows": {"cross-problem": 2, "within-frame": 1}},
            AUDIT.stratified_counts(rows, "transfer-stratum"))

    def test_memory_metadata_does_not_infer_missing_fields(self):
        raw = '''{:evidence/body {:kind :memory :body "x"}
                  :evidence/subject {:ref/type :problem :ref/id "a99J03"}
                  :evidence/author "f53-scribe"}'''
        self.assertEqual(
            {"origin-author": "f53-scribe", "origin-problem": "a99J03",
             "memory-kind": "memory", "memory-use-kind": None},
            AUDIT.memory_metadata(raw))
        self.assertEqual(
            {"origin-author": None, "origin-problem": None, "memory-kind": None,
             "memory-use-kind": None},
            AUDIT.memory_metadata(""))

    def test_review_edge_is_authoritative_for_regulative_verdict(self):
        raw = '''{:evidence/body {:kind :memory :body "process guidance"}
                  :hx/props {:review {:reviewer "f60-promotion-proctor"
                                      :verdict :approve
                                      :memory-use/kind :regulative}}}'''
        metadata = AUDIT.memory_metadata(raw)
        self.assertEqual("regulative", metadata["memory-use-kind"])
        self.assertEqual(
            "not-adjudicable-by-token",
            AUDIT.artifact_verdict(metadata["memory-use-kind"], 0, [], [], []))


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


class ReviewedEdgeKindTest(unittest.TestCase):
    EDGES = [{"hx/type": "memory/assert",
              "hx/props": {"state": "current", "attachment-status": "reviewed",
                           "review": {"evidence-id": "e-apm-promotion-review-1",
                                      "verdict": "approve",
                                      "memory-use/kind": "regulative"}}},
             {"hx/type": "memory/assert",
              "hx/props": {"state": "superseded",
                           "review": {"memory-use/kind": "substitutive"}}}]

    def test_current_assert_edge_review_kind_is_authoritative(self):
        self.assertEqual("regulative", AUDIT.review_kind_from_edges(self.EDGES))
        self.assertEqual("regulative", AUDIT.memory_metadata(
            "{:evidence/author \"f60-scribe\"}", "regulative")["memory-use-kind"])

    def test_falls_back_to_review_evidence_body(self):
        edges = [{"hx/type": "memory/assert",
                  "hx/props": {"state": "current",
                               "review": {"evidence-id": "e-apm-promotion-review-2"}}}]
        fetched = []
        def fetch(eid):
            fetched.append(eid)
            return "{:evidence/body {:review/verdict :approve :memory-use/kind :substitutive}}"
        self.assertEqual("substitutive", AUDIT.review_kind_from_edges(edges, fetch))
        self.assertEqual(["e-apm-promotion-review-2"], fetched)

    def test_pre_typed_review_edge_stays_unknown(self):
        # f53-era edge: review map with no kind anywhere, evidence body without it.
        edges = [{"hx/type": "memory/assert",
                  "hx/props": {"state": "current",
                               "review": {"evidence-id": "e-apm-promotion-review-3",
                                          "verdict": "approve"}}}]
        self.assertIsNone(AUDIT.review_kind_from_edges(
            edges, lambda eid: "{:evidence/body {:review/verdict :approve}}"))
        self.assertIsNone(AUDIT.memory_metadata("{:evidence/author \"f53-guide\"}", None)["memory-use-kind"])
        self.assertEqual("unwitnessed", AUDIT.artifact_verdict(None, 0, [], [], []))


if __name__ == "__main__":
    unittest.main()
