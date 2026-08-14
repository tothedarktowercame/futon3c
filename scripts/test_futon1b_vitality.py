import importlib.util
import pathlib
import unittest


SCRIPT_PATH = pathlib.Path(__file__).parent / "systemd" / "futon1b-vitality.py"
SPEC = importlib.util.spec_from_file_location("futon1b_vitality", SCRIPT_PATH)
VITALITY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(VITALITY)


class Futon1bVitalityTest(unittest.TestCase):
    def test_summarizes_accepted_evidence_and_hyperedge_writes(self):
        records = [
            {
                "MESSAGE": "end method=POST uri=/api/alpha/evidence outcome=ok",
                "__REALTIME_TIMESTAMP": "1720000000000000",
            },
            {
                "MESSAGE": "end method=POST uri=/api/alpha/hyperedge outcome=error",
                "__REALTIME_TIMESTAMP": "1720000001000000",
            },
            {
                "MESSAGE": "end method=POST uri=/api/alpha/hyperedge outcome=ok",
                "__REALTIME_TIMESTAMP": "1720000002000000",
            },
        ]

        result = VITALITY.summarize_evidence_writes(records)

        self.assertEqual(2, result["count"])
        self.assertEqual("2024-07-03T09:46:42+00:00", result["last_accepted_at"])

    def test_dual_write_disabled_when_unset_or_same_after_normalization(self):
        self.assertEqual(
            "secondary-unset",
            VITALITY.dual_write_status({"FUTON_SUBSTRATE_URL": "http://127.0.0.1:7073"})["reason"],
        )
        result = VITALITY.dual_write_status(
            {
                "FUTON_SUBSTRATE_URL": "http://localhost:7073/",
                "FUTON1B_URL": "http://127.0.0.1:7073",
            }
        )
        self.assertTrue(result["disabled"])
        self.assertEqual("same-target", result["reason"])

    def test_dual_write_enabled_for_distinct_targets(self):
        result = VITALITY.dual_write_status(
            {
                "FUTON_SUBSTRATE_URL": "http://127.0.0.1:7073",
                "FUTON1B_URL": "http://127.0.0.1:17073",
            }
        )
        self.assertFalse(result["disabled"])
        self.assertIsNone(result["reason"])

    def test_summarizes_only_completed_evidence_errors(self):
        journal = "\n".join(
            [
                "[futon1b-request] start method=POST uri=/api/alpha/evidence",
                (
                    "[futon1b-request] end method=POST uri=/api/alpha/evidence "
                    'outcome=error status=500 message="Invalid token: :"'
                ),
                (
                    "[futon1b-request] end method=POST uri=/api/alpha/evidence "
                    "outcome=ok"
                ),
                (
                    "[futon1b-request] end method=GET uri=/api/alpha/evidence "
                    "outcome=error status=503"
                ),
            ]
        )

        self.assertEqual(
            {
                "count": 1,
                "invalid_edn_count": 1,
                "statuses": ["500"],
            },
            VITALITY.summarize_evidence_append_errors(journal),
        )

    def test_concise_summary_distinguishes_store_and_evidence_health(self):
        record = {
            "active_state": "active",
            "health": {"status": 200, "elapsed_ms": 12.0},
            "independent_liveness": {"status": 200, "elapsed_ms": 0.8},
            "memory": {"ratio_to_high": 0.4},
            "evidence_append_errors": {"count": 2},
            "alerts": ["evidence-append-rejected"],
        }

        summary = VITALITY.concise_summary(record)

        self.assertIn("futon1b DEGRADED", summary)
        self.assertIn("main=200/12.0ms", summary)
        self.assertIn("recent-evidence-errors=2", summary)


if __name__ == "__main__":
    unittest.main()
