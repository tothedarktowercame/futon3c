#!/usr/bin/env python3
"""Bad-case tests for the records-warrant probe (records-carry-warrant way).

The bad case this way names: a bellback reply asserts a test result by count
alone — "13 tests, OK" — with no registered run behind it. Nothing is
accepted on say-so, so the probe must flag exactly that reply, while passing
the same claim when it cites a test-registry-… id the reviewer can check.
Tests construct the situation against the real probe module, not a stub.
"""
import datetime
import importlib.util
import json
import pathlib
import unittest

REPO = pathlib.Path(__file__).resolve().parent.parent
SCRIPT = REPO / "scripts" / "ways-of-working-probe.py"


def load():
    spec = importlib.util.spec_from_file_location("wow_probe", SCRIPT)
    mod = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(mod)
    return mod


NOW = datetime.datetime(2026, 11, 6, 18, 0, 0, tzinfo=datetime.timezone.utc)


class ClassifyWarrantClaimsTest(unittest.TestCase):
    def setUp(self):
        self.mod = load()

    def reply(self, text, started="2026-11-06T17:00:00Z"):
        return {"id": "invoke-test-1", "started-at": started, "text": text}

    def test_bad_case_pasted_count_is_flagged(self):
        # the exact bad case the way is named for
        claims, rows = self.mod.classify_warrant_claims(
            [self.reply("`1ccf08a4a` passes 13 tests / 64 assertions.")], now=NOW)
        self.assertEqual(claims, 1)
        self.assertEqual(len(rows), 1)
        self.assertEqual(rows[0]["job"], "invoke-test-1")

    def test_claim_with_warrant_id_is_carried(self):
        claims, rows = self.mod.classify_warrant_claims(
            [self.reply("passed 7 tests, warrant test-registry-f8a23ccc2ec4e26e62875b1a6421740667c454981b5ebcb51b8442966a8e4395")], now=NOW)
        self.assertEqual(claims, 1)
        self.assertEqual(rows, [])

    def test_reply_without_claim_ignored(self):
        claims, rows = self.mod.classify_warrant_claims(
            [self.reply("Merge landed; panel refreshed.")], now=NOW)
        self.assertEqual((claims, rows), (0, []))

    def test_stale_reply_out_of_population(self):
        claims, rows = self.mod.classify_warrant_claims(
            [self.reply("Ran 12 tests, all pass", started="2026-10-01T00:00:00Z")], now=NOW)
        self.assertEqual((claims, rows), (0, []))

    def test_undated_claim_is_not_excused(self):
        claims, rows = self.mod.classify_warrant_claims(
            [self.reply("0 failures across the suite", started=None)], now=NOW)
        self.assertEqual(claims, 1)
        self.assertEqual(len(rows), 1)


class ParseInvokeRepliesTest(unittest.TestCase):
    LEDGER = ('{:jobs {"a" {:bellback-of nil, :job-id "bell-1", :started-at "2026-11-06T17:00:00Z", '
              ':result-summary "please run the tests"}, '
              '"b" {:bellback-of "bell-1", :job-id "reply-1", :started-at "2026-11-06T17:05:00Z", '
              ':result-summary "done: 18 tests, OK \\u2014 canonical carries it"}}}')

    def setUp(self):
        self.mod = load()

    def test_only_replies_parsed_and_text_unescaped(self):
        replies = self.mod.parse_invoke_replies(self.LEDGER)
        self.assertEqual([r["id"] for r in replies], ["reply-1"])
        self.assertIn("18 tests", replies[0]["text"])
        self.assertIn("—", replies[0]["text"])  # \u2014 unescaped to a char

    def test_probe_flags_the_constructed_bad_case(self):
        # end to end against the real ledger shape: one bell, one pasted-count
        # reply -> probe invariant failing with that job named
        import tempfile, os
        with tempfile.NamedTemporaryFile("w", suffix=".edn", delete=False) as fh:
            fh.write(self.LEDGER); path = fh.name
        try:
            live = self.mod.probe_records_warrant(now=NOW, ledger=path)
        finally:
            os.unlink(path)
        self.assertEqual(live["monitor"], "on")
        self.assertEqual(live["invariant"], "failing")
        self.assertEqual(live["violation-count"], 1)
        self.assertEqual(live["violations"][0]["job"], "reply-1")
        self.assertIn("of 1 test-result claims", live["denominator"])

    def test_missing_ledger_reads_broken(self):
        live = self.mod.probe_records_warrant(now=NOW, ledger="/nonexistent/invoke-jobs.edn")
        self.assertEqual(live["monitor"], "broken")
        self.assertEqual(live["invariant"], "unknown")


if __name__ == "__main__":
    unittest.main()
