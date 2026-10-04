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


class RowNames(unittest.TestCase):
    def test_rows_without_an_event_are_named_by_tag_or_edn(self):
        self.assertEqual("invoke-start", tf.summarize_row(
            {"evidence/type": "coordination", "evidence/tags": ["invoke", "dev", "claude-17", "invoke-start"],
             "evidence/body": '{"prompt-preview" "x"}'})["event"])
        self.assertEqual("context-retrieval", tf.summarize_row(
            {"evidence/type": "coordination", "evidence/tags": ["invoke", "dev", "context-retrieval", "futon3a"],
             "evidence/body": '{"event" "context-retrieval"}'})["event"])


class Operators(unittest.TestCase):
    rules = tf.load_operators()

    def test_inflections_and_longest_phrase(self):
        text = "we stopped and checked whether it runs"
        got = [(h["text"], h["ibol"]) for h in tf.operator_hits(text, self.rules, [])]
        # "checked whether" is LOOK, not Q-RAY "checked" + stray "whether"
        self.assertEqual([("stopped", "WALL"), ("checked whether", "LOOK"), ("runs", "GO")], got)

    def test_no_match_inside_a_word(self):
        self.assertEqual([], tf.operator_hits("the testament of a bellwether", self.rules, []))

    def test_intersection_with_a_cue(self):
        text = "I refuse to wait"
        hits = tf.operator_hits(text, self.rules, [(0, len(text), "constrain")])
        self.assertEqual([("refuse", "constrain", True), ("wait", "constrain", False)],
                         [(h["text"], h["cue_intent"], h["agree"]) for h in hits])

    def test_cue_without_an_operator_is_listed(self):
        entry = {"analysis": {"source_text": "is that so? look for it",
                              "sentences": [{"fragments": [
                                  {"intent": "clarify", "display_cues": [{"start": 0, "end": 7, "text": "is that"}]},
                                  {"intent": "clarify", "display_cues": [{"start": 12, "end": 20, "text": "look for"}]}]}]}}
        ops = tf._operators_for(entry, None, self.rules)
        self.assertEqual([{"text": "is that", "intent": "clarify"}], ops["cues_without_operator"])
        self.assertTrue(ops["hits"][0]["agree"])


# ---------------------------------------------------------------- M-象-2000 step 3

RECORD = {"session_id": SID, "turn_id": "agent-turn-1",
          "evidence_id": "emacs-t1", "created_at": T1,
          "source_text": "do the thing please",
          "original_text": "do the thing please",
          "analysis_status": "analyzed"}

ANALYSIS = {"labeller": "象-1", "source_text": "do the thing please",
            "sentences": [{"id": "s1", "fragments": [
                {"start": 0, "end": 16, "text": "do the thing",
                 "intent": "continue", "display_cues": []}]}]}


def write_turn_files(analysis_dir, record=RECORD, analysis=ANALYSIS):
    path = os.path.join(analysis_dir, "turn-abc123.json")
    with open(path, "w") as f:
        json.dump(record, f)
    if analysis is not None:
        with open(path + ".analysis.json", "w") as f:
            json.dump(analysis, f)
    return path


def frames_for(rows, analyses):
    frames = tf.build_frames(rows, analyses, session_id=SID, operator_rules=[])
    for f in frames:
        f.pop("_join", None)
    return frames


class XiangReadings(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp(prefix="turn-frames-step3-")
        self.addCleanup(shutil.rmtree, self.dir)
        self.rows = [op_turn("emacs-t1", T1, "agent-turn-1", "do the thing please"),
                     op_turn("emacs-t2", T2, "agent-turn-2", "now stop")]

    def test_reading_from_entry_equals_reading_from_file(self):
        """The same turn's reading, taken from its e-xiang-turn- entry or
        from its .analysis.json file, builds the identical frame."""
        write_turn_files(self.dir)
        from_files = frames_for(self.rows, tf.load_analyses(SID, self.dir))

        empty = tempfile.mkdtemp(prefix="turn-frames-empty-")
        self.addCleanup(shutil.rmtree, empty)
        analyses = tf.load_analyses(SID, empty)
        tf.merge_xiang_readings(analyses, {"turn-abc123": {
            "record": RECORD, "reading": ANALYSIS, "settled": "analyzed"}}, SID)
        from_evidence = frames_for(self.rows, analyses)

        self.assertEqual(from_files, from_evidence)
        self.assertEqual("analyzed", from_evidence[0]["parse"]["status"])
        self.assertEqual([{"source": "象/象-1", "intent": "continue"}],
                         from_evidence[0]["parse"]["fragments"][0]["labels"])

    def test_older_shape_entry_falls_back_to_file(self):
        """An e-xiang-turn- entry whose body is a bare record (the four
        step-2 turns) is ignored by fetch_xiang_readings; the file's
        .analysis.json is used instead."""
        body = dict(RECORD)   # old shape: the record IS the body
        row = {"evidence/id": "e-xiang-turn-turn-abc123",
               "evidence/session-id": SID, "evidence/body": body}
        server = StubEvidence(self, pages={None: {"entries": [row]}})
        readings = tf.fetch_xiang_readings(SID, base=server.url)
        self.assertEqual({}, readings)
        write_turn_files(self.dir)
        analyses = tf.load_analyses(SID, self.dir)
        tf.merge_xiang_readings(analyses, readings, SID)
        frames = frames_for(self.rows, analyses)
        self.assertEqual("analyzed", frames[0]["parse"]["status"])


class StubEvidence:
    """A real local HTTP server answering /api/alpha/evidence.

    PAGES maps a cursor (None for the first page) to the page body; STATUS
    makes every answer that HTTP status (the futon1b-busy case)."""

    def __init__(self, test, pages=None, status=200):
        import http.server
        import threading
        pages = pages or {None: {"entries": []}}

        class Handler(http.server.BaseHTTPRequestHandler):
            def do_GET(self):
                self.send_response(status)
                self.send_header("Content-Type", "application/json")
                self.end_headers()
                body = pages.get(None, {"entries": []}) if status == 200 else {}
                self.wfile.write(json.dumps(body).encode())

            def log_message(self, *args):
                pass

        self.httpd = http.server.HTTPServer(("127.0.0.1", 0), Handler)
        self.url = "http://127.0.0.1:%d" % self.httpd.server_address[1]
        self.thread = threading.Thread(target=self.httpd.serve_forever, daemon=True)
        self.thread.start()
        test.addCleanup(self.httpd.shutdown)
        test.addCleanup(self.httpd.server_close)


class EvidenceBusy(unittest.TestCase):
    def setUp(self):
        self.dir = tempfile.mkdtemp(prefix="turn-frames-busy-")
        self.addCleanup(shutil.rmtree, self.dir)

    def test_504_builds_frames_from_files_exit_0_one_warning(self):
        """futon1b answering 504: frames still build from the files alone,
        main exits 0, and one stderr line names the fallback."""
        write_turn_files(self.dir)
        server = StubEvidence(self, status=504)
        import contextlib
        import io
        out, err = io.StringIO(), io.StringIO()
        with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
            rc = tf.main([SID, "--base", server.url, "--analysis-dir", self.dir])
        self.assertEqual(0, rc)
        frames = json.loads(out.getvalue())
        self.assertEqual(1, len(frames))
        self.assertEqual("do the thing please", frames[0]["turn"]["text"])
        self.assertEqual("analyzed", frames[0]["parse"]["status"])
        warnings = [ln for ln in err.getvalue().splitlines() if ln.strip()]
        self.assertEqual(1, len(warnings))
        self.assertIn("files alone", warnings[0])


if __name__ == "__main__":
    unittest.main()
