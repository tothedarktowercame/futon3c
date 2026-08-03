#!/usr/bin/env python3

import importlib.util
import tempfile
import unittest
from pathlib import Path
from unittest import mock

from edn_format import Keyword as K, dumps


SCRIPT = Path(__file__).with_name("memory_outcome_sweeper.py")
SPEC = importlib.util.spec_from_file_location("memory_outcome_sweeper", SCRIPT)
sweeper = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(sweeper)


class ExtractionTests(unittest.TestCase):
    def test_used_and_ignored_lines_are_distinguished(self):
        outcome = sweeper.extract_outcome(
            "invoke-1",
            {
                K("state"): "done",
                K("session-id"): "session-1",
                K("finished-at"): "2026-07-31T10:00:00Z",
                K("result"): """
[dispatch-recall-outcome=completed-with-memories]
- `e-memory-used`: used directly in the proof.
- `e-memory-ignored`: ignored; unrelated setting.
""",
            },
        )
        self.assertTrue(outcome["recoverable"])
        self.assertEqual(["e-memory-used"], outcome["used_ids"])
        self.assertEqual(
            ["e-memory-ignored", "e-memory-used"], outcome["surfaced_ids"]
        )

    def test_unclassified_memory_line_fails_closed(self):
        outcome = sweeper.extract_outcome(
            "invoke-2",
            {
                K("state"): "done",
                K("result"): """
[dispatch-recall-outcome=completed-with-memories]
- `e-memory-ambiguous`: mentioned without attribution.
""",
            },
        )
        self.assertFalse(outcome["recoverable"])
        self.assertEqual("unclassified-memory-lines", outcome["reason"])

    def test_completed_empty_recovers_empty_used_set(self):
        outcome = sweeper.extract_outcome(
            "invoke-3",
            {
                K("state"): "done",
                K("result"): "[dispatch-recall-outcome=completed-empty]",
            },
        )
        self.assertTrue(outcome["recoverable"])
        self.assertEqual([], outcome["used_ids"])

    def test_legacy_final_memory_usage_is_recoverable_without_outcome_marker(self):
        outcome = sweeper.extract_outcome(
            "invoke-legacy",
            {
                K("state"): "done",
                K("result"): """
Earlier discussion mentioned e-memory-not-an-attribution.

Memory usage:
- `e-memory-used`: used to choose the proof route.
- `e-memory-ignored`: ignored as unrelated.
""",
            },
        )
        self.assertTrue(outcome["recoverable"])
        self.assertEqual("legacy-unknown", outcome["recall_outcome"])
        self.assertEqual(["e-memory-used"], outcome["used_ids"])
        self.assertNotIn("e-memory-not-an-attribution", outcome["surfaced_ids"])


class SweepTests(unittest.TestCase):
    @staticmethod
    def endpoint_job(overrides=None):
        job = {
            K("state"): "done",
            K("started-at"): "2026-08-03T10:00:00Z",
            K("finished-at"): "2026-08-03T10:30:00Z",
            K("endpoint/sha-pre"): "1111111",
            K("endpoint/repository"): "/tmp/fixture-repo",
            K("endpoint/target-set"): [
                {K("module"): "problems/a96J01/lean/Main.lean",
                 K("declarations"): ["target_theorem"]},
            ],
            K("events"): [{K("type"): "prompt", K("text"): "frozen packet"}],
        }
        job.update(overrides or {})
        return job

    def test_stale_assignment_is_excluded_from_denominator(self):
        executed = {
            "sha_pre": "1" * 40,
            "sha_post": "2" * 40,
            "targets": [{"module": "problems/a96J01/lean/Main.lean",
                         "declarations": ["target_theorem"]}],
            "pre_open": {"problems/a96J01/lean/Main.lean#target_theorem": False},
            "lake_exit": 0,
            "sorry_counts": {"problems/a96J01/lean/Main.lean": 0},
            "axiom_verdicts": {"target_theorem": {"clean?": True}},
        }
        witness = sweeper.capture_mechanical_witness(
            "invoke-stale", self.endpoint_job(), "2026-08-03T11:00:00Z",
            executor=lambda _packet, _job: executed,
        )
        self.assertEqual(K("stale-assignment"), witness[K("endpoint/class")])
        self.assertFalse(witness[K("endpoint/counts-toward-denominator?")])

    def test_executed_witness_overrides_runner_report(self):
        actual = {
            "sha_pre": "a" * 40,
            "sha_post": "b" * 40,
            "targets": [{"module": "Main.lean", "declarations": ["closed"]}],
            "pre_open": {"Main.lean#closed": True},
            "lake_exit": 1,
            "sorry_counts": {"Main.lean": 3},
            "axiom_verdicts": {"closed": {"exit": 1, "clean?": False,
                                             "verdict": "not-clean"}},
        }
        runner_claim = "lake exit 0; zero sorries; axiom-clean; commit deadbeef"
        job = self.endpoint_job({
            K("result"): runner_claim,
            K("artifact-ref"): "deadbeef",
        })
        witness = sweeper.capture_mechanical_witness(
            "invoke-disagree", job, "2026-08-03T11:00:00Z",
            executor=lambda _packet, _job: actual,
        )
        self.assertEqual(1, witness[K("endpoint/lake-exit")])
        self.assertEqual(3, witness[K("endpoint/sorry-counts")]["Main.lean"])
        self.assertEqual("b" * 40, witness[K("endpoint/sha-post")])
        self.assertFalse(
            witness[K("endpoint/axiom-verdicts")]["closed"]["clean?"]
        )
        self.assertNotIn("deadbeef", repr(witness))

    def test_lexical_sorry_count_strips_nested_comments_and_literals(self):
        source = '''
theorem open_target : True := by sorry
-- sorry
/- outer sorry /- nested sorry -/ still comment -/
def prose := "sorry"
'''
        self.assertEqual(1, sweeper.lexical_sorry_count(source))

    def test_pull_offer_union_licenses_used_memory(self):
        entries = {
            K("entries"): [
                {K("evidence/body"): {
                    K("phase"): K("offered"), K("job-id"): "invoke-pull",
                    K("memory-use"): {K("memory-use/surfaced-ids"): ["e-push-memory"]},
                }},
                {K("evidence/body"): {
                    K("event"): K("memory-pull-offer"),
                    K("dispatch-id"): "invoke-pull", K("round"): 2,
                    K("pull-surfaced-ids"): ["e-pull-used", "e-overlap-memory"],
                }},
                {K("evidence/body"): {
                    K("event"): K("memory-pull-offer"),
                    K("dispatch-id"): "invoke-pull", K("round"): 4,
                    K("pull-surfaced-ids"): ["e-overlap-memory", "e-late-memory"],
                }},
            ]
        }
        response = mock.MagicMock()
        response.__enter__.return_value.read.return_value = dumps(entries).encode("utf-8")
        response.__exit__.return_value = False
        job = {
            K("state"): "done", K("agent-id"): "zai-1",
            K("finished-at"): "2026-08-03T14:00:00Z",
            K("result"): """[dispatch-recall-outcome=completed-with-memories]
Memory usage:
- USED `e-pull-used`: directly after memory_search.
- IGNORED `e-push-memory`: unrelated to the route.
- IGNORED `e-overlap-memory`: duplicate background material.
- IGNORED `e-late-memory`: arrived after the needed step.
""",
        }
        original_adjudicate = sweeper.runner_gate.adjudicate
        with tempfile.TemporaryDirectory() as tmp:
            def isolated_adjudicate(run, gates, **kwargs):
                return original_adjudicate(run, gates, state_dir=Path(tmp), **kwargs)

            with (
                mock.patch.object(sweeper.urllib.request, "urlopen", return_value=response),
                mock.patch.object(sweeper.runner_gate, "adjudicate",
                                  side_effect=isolated_adjudicate),
            ):
                self.assertEqual(
                    ["e-late-memory", "e-overlap-memory", "e-pull-used", "e-push-memory"],
                    sweeper.offered_surfaced_ids("invoke-pull", "http://store"),
                )
                result = sweeper.adjudicate_job("invoke-pull", job, base="http://store")
        self.assertEqual("accept", result["verdict"])
        self.assertTrue(result["counts_toward_endpoints"])

    def test_second_sweep_writes_zero_for_same_job_id(self):
        ledger = {
            K("job-order"): ["invoke-1"],
            K("jobs"): {
                "invoke-1": {
                    K("state"): "done",
                    K("session-id"): "session-1",
                    K("finished-at"): "2026-07-31T10:00:00Z",
                    K("result"): """
[dispatch-recall-outcome=completed-with-memories]
- `e-memory-used`: used directly.
""",
                }
            },
        }
        with tempfile.TemporaryDirectory() as tmp:
            path = Path(tmp) / "ledger.edn"
            path.write_text(dumps(ledger), encoding="utf-8")
            existing: set[str] = set()

            def fetch(_job_ids, _base):
                return set(existing)

            def post(entry, _base):
                existing.add(
                    entry[K("evidence/body")][K("job-id")]
                )

            with (
                mock.patch.object(sweeper, "fetch_existing_outcome_jobs", fetch),
                mock.patch.object(sweeper, "post_entry", post),
            ):
                first = sweeper.sweep(base="http://store", ledger_path=path)
                second = sweeper.sweep(base="http://store", ledger_path=path)
        self.assertEqual(1, first["written"])
        self.assertEqual(0, second["written"])
        self.assertEqual(1, second["skipped_existing"])

    def test_completed_run_is_gated_before_clean_outcome(self):
        job = {
            K("state"): "done",
            K("agent-id"): "codex-2",
            K("finished-at"): "2026-08-02T14:00:00Z",
            K("result"): "[dispatch-recall-outcome=completed-with-memories]\nMemory usage: vague",
        }
        adjudication = {
            "verdict": "reject-push-back", "run_status": "attribution-incomplete",
            "counts_toward_endpoints": False, "agent": "codex-2", "run_id": "invoke-1",
            "missing_ids": ["e-memory-one"],
        }
        with (
            mock.patch.object(sweeper, "offered_surfaced_ids", return_value=["e-memory-one"]),
            mock.patch.object(sweeper.runner_gate, "adjudicate", return_value=adjudication) as gate,
        ):
            result = sweeper.adjudicate_job("invoke-1", job, base="http://store")
        self.assertEqual("attribution-incomplete", result["run_status"])
        self.assertFalse(result["counts_toward_endpoints"])
        self.assertEqual(["e-memory-one"], result["missing_ids"])
        gate.assert_called_once()


if __name__ == "__main__":
    unittest.main()
