#!/usr/bin/env python3
import importlib.util
import pathlib
import unittest


PATH = pathlib.Path(__file__).with_name("turn_dispatch_reap.py")
SPEC = importlib.util.spec_from_file_location("turn_dispatch_reap", PATH)
REAP = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(REAP)


class FailureKindTokenTest(unittest.TestCase):
    def test_typed_kind_wins_and_prints_plain_token(self):
        kind, source = REAP.failure_kind(
            {"failure-kind": "invoke/failed"}, "quota HTTP 429")
        self.assertEqual(("invoke/failed", "job"), (kind, source))
        self.assertIn(
            "failure-kind=invoke/failed ",
            REAP.terminal_line("failed", "turn-invented.json", "quota", kind, source))

    def test_legacy_quota_and_store_busy_are_marked(self):
        for reason, expected in [
            ("usage limit reached", "provider/quota"),
            ("Turn not started: futon1b busy", "clock/store-busy"),
        ]:
            kind, source = REAP.failure_kind({}, reason)
            self.assertEqual((expected, "legacy-inferred"), (kind, source))
            self.assertIn(
                f"failure-kind={expected};legacy ",
                REAP.terminal_line("refused", "turn-invented.json", reason,
                                   kind, source))

    def test_other_legacy_failure_is_typed_too(self):
        self.assertEqual(
            ("invoke/failed", "legacy-inferred"),
            REAP.failure_kind({}, "invented failure"))


if __name__ == "__main__":
    unittest.main()
