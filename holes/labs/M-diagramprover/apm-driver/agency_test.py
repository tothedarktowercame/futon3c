import inspect
import unittest

import agency
import driver


class FakeConnection:
    def __init__(self):
        self.closed = False

    def close(self):
        self.closed = True


class ScriptedFetcher:
    def __init__(self, responses):
        self.responses = list(responses)
        self.calls = []

    def __call__(self, method, url, **kwargs):
        self.calls.append({"method": method, "url": url, **kwargs})
        if not self.responses:
            raise AssertionError(f"unexpected HTTP call: {method} {url}")
        return self.responses.pop(0)


class IdentityTests(unittest.TestCase):
    def test_register_new_identity_and_clean_shutdown(self):
        fetcher = ScriptedFetcher(
            [
                {"status": 200, "body": {"ok": True, "agents": {}, "ws-connected": []}},
                {"status": 201, "body": {"ok": True, "agent-id": "apm-driver"}},
                {
                    "status": 200,
                    "body": {
                        "ok": True,
                        "agents": {"apm-driver": {"type": "peripheral"}},
                        "ws-connected": ["apm-driver"],
                    },
                },
                {"status": 200, "body": {"ok": True, "deregistered": True}},
                {"status": 200, "body": {"ok": True, "agents": {}, "ws-connected": []}},
            ]
        )
        connection = FakeConnection()
        connector_calls = []

        def connector(*args):
            connector_calls.append(args)
            return connection

        identity = agency.AgencyIdentity(
            fetcher=fetcher,
            connection_factory=connector,
            session_id="session-fixed",
        ).start()
        registration = fetcher.calls[1]
        self.assertEqual("POST", registration["method"])
        self.assertEqual(
            {
                "agent-id": "apm-driver",
                "type": "peripheral",
                "ws-bridge": True,
                "capabilities": [],
            },
            registration["body"],
        )
        self.assertEqual(
            (agency.DEFAULT_WS_URL, "apm-driver", "session-fixed", agency.HTTP_TIMEOUT),
            connector_calls[0],
        )
        identity.close()
        self.assertTrue(connection.closed)
        self.assertEqual("DELETE", fetcher.calls[3]["method"])
        self.assertTrue(fetcher.calls[3]["url"].endswith("/api/alpha/agents/apm-driver"))

    def test_stale_self_is_reclaimed_but_other_identity_is_untouched(self):
        fetcher = ScriptedFetcher(
            [
                {
                    "status": 200,
                    "body": {
                        "ok": True,
                        "agents": {"apm-driver": {}, "codex-1": {}},
                        "ws-connected": ["codex-1"],
                    },
                },
                {"status": 200, "body": {"ok": True, "deregistered": True}},
                {"status": 201, "body": {"ok": True}},
                {
                    "status": 200,
                    "body": {
                        "ok": True,
                        "agents": {"apm-driver": {}, "codex-1": {}},
                        "ws-connected": ["apm-driver", "codex-1"],
                    },
                },
            ]
        )
        identity = agency.AgencyIdentity(
            fetcher=fetcher,
            connection_factory=lambda *_args: FakeConnection(),
            session_id="session-fixed",
        ).start()
        self.assertTrue(fetcher.calls[1]["url"].endswith("/api/alpha/agents/apm-driver"))
        self.assertNotIn("codex-1", fetcher.calls[1]["url"])
        self.assertIsNotNone(identity.connection)

    def test_live_self_is_not_claim_jumped(self):
        fetcher = ScriptedFetcher(
            [
                {
                    "status": 200,
                    "body": {
                        "ok": True,
                        "agents": {"apm-driver": {"status": "invoking"}},
                    },
                }
            ]
        )
        with self.assertRaisesRegex(agency.AgencyError, "claim-jump"):
            agency.AgencyIdentity(fetcher=fetcher).start()
        self.assertEqual(1, len(fetcher.calls))

    def test_live_self_with_running_jobs_is_not_claim_jumped(self):
        fetcher = ScriptedFetcher(
            [
                {
                    "status": 200,
                    "body": {
                        "ok": True,
                        "agents": {"apm-driver": {"running-jobs": 1}},
                    },
                }
            ]
        )
        with self.assertRaisesRegex(agency.AgencyError, "claim-jump"):
            agency.AgencyIdentity(fetcher=fetcher).start()


class DispatchTests(unittest.TestCase):
    def test_signature_matches_h1_injection_stub(self):
        self.assertEqual(inspect.signature(driver.dispatch_fn), inspect.signature(agency.dispatch_fn))
        self.assertEqual(inspect.signature(driver.poll_fn), inspect.signature(agency.poll_fn))

    def test_dispatch_payload_is_attributed_and_returned_for_ledger(self):
        fetcher = ScriptedFetcher(
            [{"status": 202, "body": {"ok": True, "job-id": "invoke-sample"}}]
        )
        result = agency.dispatch_fn(
            "zai-1", "PHASE A packet", fetcher=fetcher, base_url="http://agency"
        )
        expected = {
            "from": "apm-driver",
            "to": "zai-1",
            "body": "PHASE A packet",
            "mode": "work",
            "caller": "apm-driver",
            "agent-id": "zai-1",
            "prompt": "PHASE A packet",
            "surface": "bell",
        }
        self.assertEqual({"job-id": "invoke-sample", "request": expected}, result)
        self.assertEqual(expected, fetcher.calls[0]["body"])
        self.assertEqual("http://agency/api/alpha/bell", fetcher.calls[0]["url"])


class PollTests(unittest.TestCase):
    def test_explicit_id_poll_and_state_mapping(self):
        cases = [
            ("queued", "queued"),
            ("running", "running"),
            ("overrun", "running"),
            ("done", "done"),
            ("failed", "error"),
            ("timed-out", "error"),
            ("cancelled", "cancelled"),
        ]
        for source, expected in cases:
            with self.subTest(source=source):
                fetcher = ScriptedFetcher(
                    [
                        {
                            "status": 200,
                            "body": {
                                "ok": True,
                                "job": {"state": source, "result": "answer"},
                            },
                        }
                    ]
                )
                result = agency.poll_fn(
                    "invoke/one", fetcher=fetcher, base_url="http://agency"
                )
                self.assertEqual({"status": expected, "result": "answer"}, result)
                self.assertEqual(
                    "http://agency/api/alpha/invoke/jobs/invoke%2Fone",
                    fetcher.calls[0]["url"],
                )
                self.assertNotIn("?", fetcher.calls[0]["url"])


class QuotaTests(unittest.TestCase):
    RESPONSE = {
        "success": True,
        "data": {
            "limits": [
                {
                    "type": "TOKENS_LIMIT",
                    "unit": 3,
                    "number": 5,
                    "percentage": 10,
                    "nextResetTime": 100,
                },
                {
                    "type": "TOKENS_LIMIT",
                    "unit": 6,
                    "number": 1,
                    "percentage": 34,
                    "nextResetTime": 200,
                },
                {"type": "TIME_LIMIT", "percentage": 99},
            ]
        },
    }

    def test_quota_open_uses_continuity_log_format(self):
        fetcher = ScriptedFetcher([{"status": 200, "body": self.RESPONSE}])
        lines = []
        limits = agency.fetch_and_enforce_quota(
            fetcher=fetcher, logger=lines.append, key="test-key"
        )
        self.assertEqual([90.0, 66.0], [item["available"] for item in limits])
        self.assertEqual(
            [
                "usage-gate-open min-available=50 limits="
                "unit=3/number=5/used=10/available=90,"
                "unit=6/number=1/used=34/available=66"
            ],
            lines,
        )
        self.assertEqual("test-key", fetcher.calls[0]["headers"]["Authorization"])

    def test_exactly_half_available_is_closed_with_same_message(self):
        with self.assertRaisesRegex(
            agency.GateClosed,
            r"usage-gate-closed min-available=50 limits=unit=3/number=5/used=50/available=50",
        ):
            agency.enforce_quota(
                [
                    {
                        "unit": 3,
                        "number": 5,
                        "used": 50.0,
                        "available": 50.0,
                        "next_reset_ms": 0,
                    }
                ],
                logger=lambda _line: self.fail("closed quota must not log open"),
            )

    def test_missing_token_limits_fails_closed(self):
        with self.assertRaisesRegex(agency.GateClosed, "no-token-limits"):
            agency.quota_snapshot({"success": True, "data": {"limits": []}})

    def test_quota_transport_failure_is_fail_closed(self):
        def unavailable(*_args, **_kwargs):
            raise agency.AgencyError("network unavailable")

        with self.assertRaisesRegex(agency.GateClosed, "request-failed.*network unavailable"):
            agency.fetch_and_enforce_quota(fetcher=unavailable, key="test-key")


if __name__ == "__main__":
    unittest.main()
