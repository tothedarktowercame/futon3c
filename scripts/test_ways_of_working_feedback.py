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
            result = PROBE.probe_feedback_reaches()
        self.assertEqual("broken", result["monitor"])
        self.assertEqual("unknown", result["invariant"])


if __name__ == "__main__":
    unittest.main()
