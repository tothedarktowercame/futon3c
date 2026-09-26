import importlib.util
import pathlib
import unittest
from unittest import mock


SCRIPT_PATH = pathlib.Path(__file__).parent / "systemd" / "futon1b-vitality.py"
SPEC = importlib.util.spec_from_file_location("futon1b_vitality", SCRIPT_PATH)
VITALITY = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(VITALITY)


class Futon1bVitalityTest(unittest.TestCase):
    def test_four_day_wedge_fires_cheap_health_alert_with_diagnosis(self):
        now = 1_800_000_000.0
        previous = {
            "sampled_at_epoch": now - 301,
            "substrate_rejected_rising_since_epoch": now - 301,
            "substrate_g1_busy_since_epoch": now - 301,
            "latest_record": {
                "substrate_health": {
                    "stats": {"rejected": 100},
                    "gc": {"G1 Concurrent GC": {"time-ms": 1_000}},
                }
            },
        }
        wedge = {
            "permits/total": 2,
            "permits/available": 0,
            "holders": [{"age-ms": 400_000}, {"age-ms": 399_000}],
            "oldest-holder-ms": 400_000,
            "stats": {"rejected": 130, "timed-out": 8},
            "heap": {"used-mb": 3970, "max-mb": 4096},
            "gc": {"G1 Concurrent GC": {"time-ms": 302_000}},
        }

        result = VITALITY.evaluate_substrate_health(wedge, previous, now)

        self.assertEqual("hung-jdbc", result["diagnosis"])
        self.assertIn("substrate-oldest-holder-over-60s", result["alerts"])
        self.assertIn("substrate-all-permits-held-over-60s", result["alerts"])
        self.assertIn("substrate-rejections-rising-5m", result["alerts"])
        self.assertIn("substrate-post-gc-heap-over-85pct", result["alerts"])
        self.assertIn("substrate-g1-concurrent-cpu-busy-5m", result["alerts"])
        self.assertIn("substrate-failure-mode:hung-jdbc", result["alerts"])

    def test_healthy_payload_does_not_alert(self):
        payload = {
            "permits/total": 2,
            "permits/available": 2,
            "holders": [],
            "oldest-holder-ms": 0,
            "stats": {"rejected": 10, "timed-out": 0},
            "heap": {"used-mb": 1000, "max-mb": 4096},
            "gc": {"G1 Concurrent GC": {"time-ms": 1000}},
        }
        previous = {"sampled_at_epoch": 1000, "latest_record": {"substrate_health": payload}}

        result = VITALITY.evaluate_substrate_health(payload, previous, 1060)

        self.assertEqual([], result["alerts"])
        self.assertIsNone(result["diagnosis"])

    def test_health_probe_requests_json_and_never_deep_health(self):
        response = mock.MagicMock()
        response.status = 200
        response.read.return_value = b'{"ok": true}'
        response.__enter__.return_value = response
        with mock.patch.object(VITALITY.urllib.request, "urlopen", return_value=response) as opened:
            status, _elapsed, error, payload = VITALITY.health_probe(
                VITALITY.MAIN_HEALTH_URL, include_payload=True
            )
        request = opened.call_args.args[0]
        self.assertEqual(200, status)
        self.assertIsNone(error)
        self.assertEqual({"ok": True}, payload)
        self.assertNotIn("deep", request.full_url)
        self.assertEqual("application/json", request.headers["Accept"])
        with self.assertRaises(ValueError):
            VITALITY.health_probe(VITALITY.MAIN_HEALTH_URL + "?deep=true")

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
