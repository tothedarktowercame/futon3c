import importlib.util
import pathlib
import unittest


SCRIPT_PATH = pathlib.Path(__file__).parent / "systemd" / "futon1b-vitality.py"
SPEC = importlib.util.spec_from_file_location("futon1b_vitality", SCRIPT_PATH)
VITALITY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(VITALITY)


class Futon1bVitalityTest(unittest.TestCase):
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
