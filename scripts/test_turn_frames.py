#!/usr/bin/env python3
"""Unit tests for turn_frames.py — fixture data only, no live calls."""

import json
import os
import shutil
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import turn_frames as tf

SID = "test-session-0000-0000-000000000000"

T1 = "2026-09-29T10:00:00Z"
T2 = "2026-09-29T11:00:00Z"


def op_turn(eid, at, turn_id, text):
    return {
        "evidence/id": eid,
        "evidence/type": "coordination",
        "evidence/at": at,
        "evidence/origin": {"kind": "operator", "actor": "joe",
                            "writer": "agent-chat/turn"},
        "evidence/body": {"event": "chat-turn", "role": "user",
                          "turn-id": turn_id, "text": text},
    }


def harness_user_turn(at):
    """A parked-resume replay: role user but origin kind harness."""
    return {
        "evidence/id": "emacs-harness1",
        "evidence/type": "coordination",
        "evidence/at": at,
        "evidence/origin": {"kind": "harness", "actor": "parked-resume",
                            "writer": "agent-chat/turn"},
        "evidence/body": {"event": "chat-turn", "role": "user",
                          "turn-id": "agent-turn-99", "text": "replayed"},
    }


def fixture_rows():
    return [
        op_turn("emacs-t1", T1, "agent-turn-1", "do the thing please"),
        op_turn("emacs-t2", T2, "agent-turn-2", "now stop"),
        harness_user_turn("2026-09-29T10:30:00Z"),
        {  # turn-commits inside window 1
            "evidence/id": "e-commits",
            "evidence/type": "coordination",
            "evidence/at": "2026-09-29T10:15:00Z",
            "evidence/origin": {"kind": "agent", "actor": "agent-1",
                                "writer": "agent-chat/turn-commits"},
            "evidence/body": {"event": "turn-commits", "turn-id": "agent-turn-1",
                              "commits": [{"repo": "futon3c",
                                           "sha": "abc123",
                                           "committed-at": "2026-09-29T10:14:00Z",
                                           "author": "Someone Else",
                                           "subject": "fix a thing"}]},
        },
        {  # promise row inside window 1
            "evidence/id": "e-park",
            "evidence/type": "promise/park-made",
            "evidence/at": "2026-09-29T10:20:00Z",
            "evidence/origin": {"kind": "harness"},
            "evidence/body": {"park-id": "p-1", "reason": "waiting"},
        },
        {  # exactly AT the next turn's time -> belongs to frame 2
            "evidence/id": "e-boundary",
            "evidence/type": "promise/woken",
            "evidence/at": T2,
            "evidence/origin": {"kind": "harness"},
            "evidence/body": {"park-id": "p-1"},
        },
        {  # negation row pointing at turn 1, disagreeing intent
            "evidence/id": "interpretation-negation:x1",
            "evidence/type": "interpretation/negation",
            "evidence/at": "2026-09-29T10:25:00Z",
            "evidence/origin": {"kind": "harness", "actor": "negation-interpretation"},
            "evidence/subject": {"ref/type": "evidence", "ref/id": "emacs-t1"},
            "evidence/body": {"intent": "withdraw",
                              "fragment-id": "s1:0",
                              "fragment-text": "do the thing",
                              "target": "act:xyz"},
        },
    ]


ANALYSIS_1 = {
    "version": 2, "status": "analyzed", "labeller": "claude-15",
    "sentences": [
        {"id": "s1", "fragments": [
            {"text": "do the thing", "intent": "ask-action",
             "display_cues": [{"text": "please"}],
             "pattern_refs": ["discourse/ask-plainly"],
             "pattern_rejections": [{"id": "p/x", "reason": "nope"}]},
        ]},
    ],
}


class TurnFramesTest(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp()
        # record + analysis for turn 1; record only for turn 2 (no analysis)
        rec1 = {"session_id": SID, "turn_id": "agent-turn-1",
                "evidence_id": "emacs-t1", "source_text": "do the thing please"}
        rec2 = {"session_id": SID, "turn_id": "agent-turn-2",
                "source_text": "now stop"}
        self._write("turn-AAAAAA.json", rec1)
        self._write("turn-AAAAAA.json.analysis.json", ANALYSIS_1)
        self._write("turn-AAAAAA.json.candidates.json",  # spelling 1
                    {"candidates": [{"id": "p/a", "title": "A",
                                     "fragment": "s1", "parent": "p/parent"},
                                    {"id": "p/b", "title": "B",
                                     "fragment": "s1", "parent": None}]})
        self._write("turn-BBBBBB.json", rec2)
        self._write("turn-BBBBBB.candidates.json",  # spelling 2
                    {"candidates": [{"id": "p/c", "title": "C",
                                     "fragment": "s1", "parent": "p/q"}]})

    def _write(self, name, obj):
        with open(os.path.join(self.dir, name), "w") as f:
            json.dump(obj, f)

    def tearDown(self):
        shutil.rmtree(self.dir)

    def build(self, limit=None):
        analyses = tf.load_analyses(SID, analysis_dir=self.dir)
        return tf.build_frames(fixture_rows(), analyses, session_id=SID,
                               limit=limit)

    def test_frame_count_and_operator_filter(self):
        frames = self.build()
        # harness-origin user turn must NOT become a frame
        self.assertEqual(len(frames), 2)
        self.assertEqual(frames[0]["turn"]["evidence_id"], "emacs-t1")
        self.assertEqual(frames[1]["turn"]["text"], "now stop")

    def test_time_window_assignment(self):
        frames = self.build()
        w1 = [h["evidence_id"] if "evidence_id" in h else h["at"] for h in ()]
        f1_ids = {h["at"] for h in frames[0]["happened"]}
        f2_ids = {h["at"] for h in frames[1]["happened"]}
        self.assertIn("2026-09-29T10:15:00Z", f1_ids)  # commits in window 1
        self.assertIn("2026-09-29T10:20:00Z", f1_ids)  # park in window 1
        self.assertNotIn(T2, f1_ids)
        # a row exactly at the next turn's time belongs to the next frame
        self.assertIn(T2, f2_ids)
        types1 = [h["type"] for h in frames[0]["happened"]]
        self.assertIn("coordination", types1)       # harness replay + commits
        self.assertIn("promise/park-made", types1)
        self.assertIn("interpretation/negation", types1)

    def test_turn_commits_summary_keeps_repo_author_subject(self):
        frames = self.build()
        c = [h for h in frames[0]["happened"]
             if isinstance(h["summary"], dict)
             and h["summary"].get("event") == "turn-commits"][0]
        commit = c["summary"]["commits"][0]
        self.assertEqual(commit["repo"], "futon3c")
        self.assertEqual(commit["author"], "Someone Else")
        self.assertEqual(commit["subject"], "fix a thing")

    def test_analysis_join_and_labels(self):
        frames = self.build()
        p0 = frames[0]["parse"]
        self.assertEqual(p0["status"], "analyzed")
        frag = p0["fragments"][0]
        self.assertEqual(frag["sentence"], "s1")
        sources = {l["source"]: l["intent"] for l in frag["labels"]}
        self.assertEqual(sources["象/claude-15"], "ask-action")
        self.assertEqual(sources["negation"], "withdraw")
        # two sources disagree -> flagged, combined null
        self.assertTrue(frag["disagree"])
        self.assertIsNone(frag["combined"])

    def test_missing_analysis(self):
        frames = self.build()
        self.assertEqual(frames[1]["parse"]["status"], "missing")
        self.assertEqual(frames[1]["parse"]["fragments"], [])

    def test_both_candidates_spellings(self):
        frames = self.build()
        p1 = frames[0]["patterns"]["proposed_by_parent"]
        self.assertEqual([c["id"] for c in p1["p/parent"]], ["p/a"])
        self.assertEqual([c["id"] for c in p1["(none)"]], ["p/b"])
        p2 = frames[1]["patterns"]["proposed_by_parent"]
        self.assertEqual([c["id"] for c in p2["p/q"]], ["p/c"])

    def test_patterns_matched_rejected(self):
        frames = self.build()
        self.assertEqual(frames[0]["patterns"]["matched"],
                         ["discourse/ask-plainly"])
        self.assertEqual(frames[0]["patterns"]["rejected"][0]["id"], "p/x")

    def test_limit(self):
        self.assertEqual(len(self.build(limit=1)), 1)

    def test_fallback_join_by_turn_id(self):
        # record without evidence_id joins via session_id + turn_id
        rec = self._write("turn-CCCCCC.json",
                          {"session_id": SID, "turn_id": "agent-turn-2",
                           "source_text": "now stop"})
        self._write("turn-CCCCCC.json.analysis.json",
                    {"labeller": "claude-9", "sentences": [
                        {"id": "s1", "fragments": [
                            {"text": "stop", "intent": "ask-action",
                             "pattern_refs": [], "pattern_rejections": []}]}]})
        # remove the analysis-less rec2 so the turn_id key is unambiguous
        os.remove(os.path.join(self.dir, "turn-BBBBBB.json"))
        os.remove(os.path.join(self.dir, "turn-BBBBBB.candidates.json"))
        frames = self.build()
        self.assertEqual(frames[1]["parse"]["status"], "analyzed")
        frag = frames[1]["parse"]["fragments"][0]
        self.assertFalse(frag["disagree"])
        self.assertEqual(frag["combined"], "ask-action")


class ReviewCases(unittest.TestCase):
    """Planted in review (claude-17)."""

    @staticmethod
    def _turn(i, at):
        return {"evidence/type": "coordination", "evidence/id": f"t{i}", "evidence/at": at,
                "evidence/origin": {"kind": "operator"},
                "evidence/body": {"event": "chat-turn", "role": "user", "text": f"turn {i}"}}

    @staticmethod
    def _row(at):
        return {"evidence/type": "coordination", "evidence/id": "x" + at, "evidence/at": at,
                "evidence/body": {"event": "chat-turn", "role": "assistant", "text": "r"}}

    def test_limit_keeps_the_window_ending_at_the_next_turn(self):
        rows = [self._turn(1, "2026-09-29T10:00:00Z"), self._row("2026-09-29T10:05:00Z"),
                self._turn(2, "2026-09-29T11:00:00Z"), self._row("2026-09-29T11:05:00Z")]
        frames = tf.build_frames(rows, {}, "s", limit=1)
        self.assertEqual(["2026-09-29T10:05:00Z"], [h["at"] for h in frames[0]["happened"]])

    def test_mixed_fraction_precision_orders_by_instant(self):
        # 3-digit and 9-digit fractions both occur in the live store.
        rows = [self._turn(1, "2026-09-29T10:00:00.840Z"),
                self._row("2026-09-29T10:00:00.840500000Z"),
                self._turn(2, "2026-09-29T10:00:00.841Z")]
        self.assertEqual([1, 0], [len(f["happened"]) for f in tf.build_frames(rows, {}, "s")])


if __name__ == "__main__":
    unittest.main()
