#!/usr/bin/env python3
"""Fixture tests for the feedback-reaches way-of-working probe."""

import datetime
import importlib.util
import pathlib
import sys
import unittest
from unittest import mock


SCRIPTS = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(SCRIPTS))
SPEC = importlib.util.spec_from_file_location(
    "ways_of_working_probe", SCRIPTS / "ways-of-working-probe.py")
PROBE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(PROBE)
UTC = datetime.timezone.utc
NOW = datetime.datetime(2026, 11, 3, 12, 0, tzinfo=UTC)


def stamp(delta):
    return (NOW - delta).isoformat().replace("+00:00", "Z")


def bell(job_id, caller="claude-1", finished=datetime.timedelta(hours=2), **extra):
    row = {"job-id": job_id, "surface": "bell", "caller": caller,
           "agent-id": "codex-1", "state": "done",
           "created-at": stamp(finished + datetime.timedelta(minutes=1)),
           "finished-at": stamp(finished), "events": []}
    row.update(extra)
    return row


def classify(jobs=(), history=(), replies=()):
    return PROBE.classify_feedback_reaches(list(jobs), list(history), list(replies), NOW)


class FeedbackClassifierTest(unittest.TestCase):
    def test_reply_joins_by_turn_id_past_intervening_tool_row(self):
        operator = {"evidence/id": "operator-1", "evidence/session-id": "session-1",
                    "evidence/at": "2026-11-03T10:00:00Z",
                    "evidence/subject": {"ref/id": "claude-1"},
                    "evidence/body": {"role": "user", "turn-id": "turn-1"}}
        reply = {"evidence/id": "reply-1", "evidence/session-id": "session-1",
                 "evidence/in-reply-to": "tool-1", "evidence/at": "2026-11-03T10:01:00Z",
                 "evidence/body": {"role": "assistant", "turn-id": "turn-1",
                                   "segment-final": True,
                                   "text": "narration\n\n㊥ (gist) Final reply",
                                   "unified-text": "narration\n\n㊥ (gist) Final reply"}}
        found = PROBE.join_operator_replies([operator], [operator, reply])
        self.assertEqual(["turn-1"], [row["job-id"] for row in found])
        self.assertEqual("㊥ (gist) Final reply", found[0]["text"])

    def test_two_turns_in_one_session_are_not_swapped(self):
        operators = [{"evidence/id": f"operator-{n}", "evidence/session-id": "session-1",
                      "evidence/at": f"2026-11-03T10:0{n}:00Z",
                      "evidence/body": {"role": "user", "turn-id": f"turn-{n}"}}
                     for n in (1, 2)]
        replies = [{"evidence/id": f"reply-{n}", "evidence/session-id": "session-1",
                    "evidence/at": f"2026-11-03T10:0{n}:30Z",
                    "evidence/body": {"role": "assistant", "turn-id": f"turn-{n}",
                                      "segment-final": True, "text": f"reply {n}"}}
                   for n in (2, 1)]
        found = PROBE.join_operator_replies(operators, operators + replies)
        self.assertEqual(["reply 1", "reply 2"], [row["text"] for row in found])

    def test_operator_without_assistant_stays_in_coverage_denominator(self):
        operator = {"evidence/id": "operator-1", "evidence/session-id": "session-1",
                    "evidence/at": "2026-11-03T10:00:00Z",
                    "evidence/body": {"role": "user", "turn-id": "turn-1"}}
        replies = PROBE.join_operator_replies([operator], [operator])
        result = PROBE.classify_feedback_reaches([], [], replies, NOW, operator_turns=1)
        self.assertIn("replies found: 0 of 1 operator turns", result["evidence"])

    def test_http_caller_is_grouped_by_emitter(self):
        job = bell("no-path", caller="http-caller")
        job["events"] = [{"type": "prompt", "text": "[sweeper/consent-gate-emit] work"}]
        result = classify([job])
        self.assertEqual(1, result["violation-count"])
        self.assertEqual("[sweeper/consent-gate-emit]", result["violations"][0]["emitter"])
        self.assertEqual(1, result["violations"][0]["count"])

    def test_brief_notification_needs_no_return_path(self):
        notice = bell("notice", caller="http-caller", mode="brief")
        work = bell("work", caller="http-caller", mode="work")
        result = classify([notice, work])
        self.assertEqual(1, result["violation-count"])
        self.assertEqual(["work"], result["violations"][0]["job-ids"])

    def test_polling_caller_receipt_is_the_terminal_result(self):
        inbox_only = {"sent?": True, "bell-job-id": "copy-nobody-reads"}
        polled = bell("polled", caller="wm-full-loop", result="answer",
                      **{"auto-bellback": inbox_only})
        no_result = bell("no-result", caller="wm-full-loop",
                         **{"auto-bellback": inbox_only})
        result = classify([polled, no_result])
        self.assertEqual(["no-result"], [v["job-id"] for v in result["violations"]])

    def test_old_unacked_park_fails_and_recent_one_is_pending(self):
        old = bell("old", auto_bellback={"suppressed?": True, "reason": "parked-on",
                                         "park-id": "park-old"})
        old["auto-bellback"] = old.pop("auto_bellback")
        recent = bell("recent", finished=datetime.timedelta(minutes=5),
                      auto_bellback={"suppressed?": True, "reason": "parked-on",
                                     "park-id": "park-recent"})
        recent["auto-bellback"] = recent.pop("auto_bellback")
        history = [{"evidence/type": "promise/ready-enqueued",
                    "evidence/body": {"park-id": park}} for park in ("park-old", "park-recent")]
        result = classify([old, recent], history)
        self.assertEqual(["old"], [v["job-id"] for v in result["violations"]])
        self.assertEqual(["recent"], [p["job-id"] for p in result["pending"]])

    def test_delivered_auto_bellback_is_clean(self):
        original = bell("original", **{"auto-bellback": {"sent?": True,
                                                           "bell-job-id": "return"}})
        child = {"job-id": "return", "surface": "auto-bellback", "caller": "auto-bellback",
                 "created-at": stamp(datetime.timedelta(hours=1)), "state": "done",
                 "delivery": {"status": "delivered"}}
        result = classify([original, child])
        self.assertEqual("holding", result["invariant"])
        self.assertEqual(0, result["violation-count"])

    def test_strict_reply_form(self):
        bad_middle = "㊥ (gist) One.\n\nUnmarked.\n\n㊢ (result) Three."
        good = "㊥ (gist) One.\n\n㊢ (result) Two."
        wrong_first = "㊢ (result) One.\n\n㊥ (gist) Two."
        replies = [{"job-id": name, "agent-id": "codex-1", "text": text}
                   for name, text in (("bad", bad_middle), ("good", good),
                                      ("wrong-first", wrong_first))]
        result = classify(replies=replies)
        self.assertEqual(["bad", "wrong-first"],
                         [v["job-id"] for v in result["violations"]])

    def test_fetch_error_is_broken_not_holding(self):
        with mock.patch.object(PROBE, "fetch_feedback_jobs", side_effect=OSError("offline")):
            result = PROBE.probe_feedback_reaches(lambda: {"operator-evidence": []})
        self.assertEqual("broken", result["monitor"])
        self.assertEqual("unknown", result["invariant"])


if __name__ == "__main__":
    unittest.main()


class FetchOperatorRepliesPaginationTest(unittest.TestCase):
    """A 2-hour bucket can hold more than one page (2026-11-04: one chatty
    session's promise-lifecycle churn filled it). The reader must follow
    next-cursor instead of refusing on a full page."""

    def operator_row(self):
        return {"evidence/id": "operator-1", "evidence/session-id": "session-1",
                "evidence/at": "2026-11-03T10:00:00Z",
                "evidence/body": {"role": "user", "turn-id": "turn-1"}}

    def reply_row(self, evidence_id):
        return {"evidence/id": evidence_id, "evidence/session-id": "session-1",
                "evidence/at": "2026-11-03T10:01:00Z",
                "evidence/body": {"role": "assistant", "turn-id": "turn-1",
                                  "segment-final": True,
                                  "unified-text": "㊥ (gist) Final reply",
                                  "text": "㊥ (gist) Final reply"}}

    def test_full_page_plus_cursor_then_reply_is_read(self):
        page_one = {"entries": [self.reply_row(f"filler-{i}") for i in range(1000)],
                    "count": 1001,
                    "next-cursor": {"at": "2026-11-03T10:00:30Z", "id": "filler-999"}}
        page_two = {"entries": [self.reply_row("reply-1")], "count": 1}
        pages = [page_one, page_two]
        seen_urls = []

        def fetcher(url):
            seen_urls.append(url)
            return pages.pop(0)

        with mock.patch.object(PROBE, "get_json", side_effect=fetcher):
            replies = PROBE.fetch_operator_replies(
                [self.operator_row()], NOW - datetime.timedelta(hours=2),
                now=NOW, base="http://store.test")
        self.assertEqual(["turn-1"], [row["job-id"] for row in replies])
        self.assertEqual("㊥ (gist) Final reply", replies[0]["text"])
        self.assertEqual(2, len(seen_urls))
        self.assertIn("cursor-at=2026-11-03T10%3A00%3A30Z", seen_urls[1])
        self.assertIn("cursor-id=filler-999", seen_urls[1])

    def test_full_page_without_cursor_fails_loudly(self):
        page = {"entries": [self.reply_row(f"filler-{i}") for i in range(1000)],
                "count": 1000}

        with mock.patch.object(PROBE, "get_json", return_value=page):
            with self.assertRaisesRegex(RuntimeError, "full with no next-cursor"):
                PROBE.fetch_operator_replies(
                    [self.operator_row()], NOW - datetime.timedelta(hours=2),
                    now=NOW, base="http://store.test")

    def test_cursor_loop_fails_loudly(self):
        page = {"entries": [self.reply_row(f"filler-{i}") for i in range(1000)],
                "count": 1000,
                "next-cursor": {"at": "2026-11-03T10:00:30Z", "id": "filler-999"}}

        def fetcher(url):
            return page

        with mock.patch.object(PROBE, "get_json", side_effect=fetcher):
            with self.assertRaisesRegex(RuntimeError, "exceeded bounded pagination"):
                PROBE.fetch_operator_replies(
                    [self.operator_row()], NOW - datetime.timedelta(hours=2),
                    now=NOW, base="http://store.test")
