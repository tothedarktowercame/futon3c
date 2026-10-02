"""Tests for xiaoxiang_reader.py and the standalone bundle.
Run: python3 -m unittest scripts/test_xiaoxiang_reader.py"""
import json
import os
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import xiaoxiang as xx  # noqa: E402
import xiaoxiang_reader as rd  # noqa: E402

SECRET = "Sup3rS3cretValue9"


def claude(content, **extra):
    return {"type": "user", "message": {"role": "user", "content": content}, **extra}


def codex(text, role="user"):
    return {"type": "response_item",
            "payload": {"type": "message", "role": role,
                        "content": [{"type": "input_text", "text": text}]}}


class Turns(unittest.TestCase):
    def test_claude_keeps_typed_text_only(self):
        self.assertEqual(["please fix it"], rd.claude_turns(claude("please fix it")))
        self.assertEqual(["go ahead"], rd.claude_turns(claude([{"type": "text", "text": "go ahead"}])))
        for skipped in [claude([{"type": "tool_result", "content": "x"}]),
                        claude("hi", isSidechain=True), claude("hi", isMeta=True),
                        claude("<command-name>/compact</command-name>"),
                        claude("[compacted: to 3 turns]"),
                        {"type": "assistant", "message": {"content": "hello"}}]:
            self.assertEqual([], rd.claude_turns(skipped))

    def test_agency_header_is_stripped_and_agent_turns_skipped(self):
        text = "--- CURRENT TURN ---\nFrom: joe\nOrigin: operator\n---\n\nUser message:\nYes please"
        self.assertEqual(["Yes please"], rd.claude_turns(claude(text)))
        bell = "--- CURRENT TURN ---\nFrom: claude-17\nOrigin: agent\n---\n\nUser message:\nBuild it"
        self.assertEqual([], rd.codex_turns(codex(bell)))

    def test_codex_keeps_user_input_only(self):
        self.assertEqual(["add a test"], rd.codex_turns(codex("add a test")))
        for skipped in [codex("<environment_context>cwd</environment_context>"),
                        codex("# AGENTS.md instructions for /x"), codex("done", role="assistant")]:
            self.assertEqual([], rd.codex_turns(skipped))


H = 3600


class Gaps(unittest.TestCase):
    def test_only_long_gaps_with_agent_work_are_kept(self):
        turns = [0, 1 * H, 10 * H, 12 * H, 30 * H]
        events = [(0.5 * H, 7), (5 * H, 100), (6 * H, 50), (11 * H, 999), (20 * H, 0)]
        found = rd.gaps(turns, events)
        self.assertEqual([(1 * H, 10 * H, 150)], [(g["start"], g["end"], g["tokens"]) for g in found])

    def test_gap_hours_is_a_parameter(self):
        turns = [0, 2 * H, 2.9 * H]
        events = [(1 * H, 10), (2.5 * H, 5)]
        self.assertEqual([], rd.gaps(turns, events))
        self.assertEqual([10], [g["tokens"] for g in rd.gaps(turns, events, 1)])
        self.assertEqual([10, 5], [g["tokens"] for g in rd.gaps(turns, events, 0)])
        report = {"gap_hours": 0, "gaps": rd.gaps(turns, events, 0), "first_turn": 0,
                  "last_turn": 3 * H}
        self.assertIn("stretch between turns you typed", rd.gap_svg(report) + rd._gap_phrase(report))

    def test_events_at_a_turn_belong_to_neither_side(self):
        found = rd.gaps([0, 8 * H], [(0, 5), (8 * H, 5), (4 * H, 1)])
        self.assertEqual([1], [g["tokens"] for g in found])

    def test_claude_reply_split_over_lines_counts_once(self):
        rec = {"type": "assistant", "timestamp": "2026-09-01T03:00:00Z", "requestId": "r1",
               "message": {"id": "m1", "usage": {"input_tokens": 2, "cache_read_input_tokens": 90,
                                                 "cache_creation_input_tokens": 5, "output_tokens": 3}}}
        self.assertEqual(("m1/r1", 100), rd.claude_tokens(rec))
        self.assertIsNone(rd.claude_tokens(claude("hi")))

    def test_codex_repeated_running_total_has_the_same_key(self):
        def ev(total, last):
            return {"type": "event_msg", "payload": {"type": "token_count", "info": {
                "total_token_usage": {"total_tokens": total},
                "last_token_usage": {"input_tokens": last, "cached_input_tokens": last // 2,
                                     "output_tokens": 1}}}}
        a, b, c = rd.codex_tokens(ev(10, 9)), rd.codex_tokens(ev(10, 9)), rd.codex_tokens(ev(30, 19))
        self.assertEqual(a, b)
        self.assertNotEqual(a[0], c[0])
        self.assertEqual(20, c[1])  # cached input is inside input_tokens, not added again


def fake_home(root):
    """A made-up ~/.claude and ~/.codex (never real logs) with a planted secret."""
    cl = os.path.join(root, "claude", "proj")
    cx = os.path.join(root, "codex", "2026", "09", "29")
    os.makedirs(cl)
    os.makedirs(cx)
    with open(os.path.join(cl, "s1.jsonl"), "w") as fh:
        for r in [claude(f"password: {SECRET}"), claude("I reject your claim"),
                  claude([{"type": "tool_result", "content": f"password={SECRET}"}]),
                  {"type": "assistant", "message": {"content": "ok"}}]:
            fh.write(json.dumps({**r, "timestamp": "2026-09-01T00:00:00Z"}) + "\n")
        reply = {"type": "assistant", "timestamp": "2026-09-01T05:00:00Z", "requestId": "r",
                 "message": {"id": "m", "usage": {"input_tokens": 400, "output_tokens": 100}}}
        fh.write(json.dumps(reply) + "\n")
        fh.write(json.dumps(reply) + "\n")  # the same reply, second content block
    with open(os.path.join(cx, "rollout-1.jsonl"), "w") as fh:
        fh.write(json.dumps({**codex("sounds good, go ahead"), "timestamp": "2026-09-01T09:00:00Z"}) + "\n")
        fh.write("not json\n")
    return os.path.join(root, "claude"), os.path.join(root, "codex")


def stub_model():
    """A model from a handful of labelled phrases: enough to classify the
    fixture's turns without any real readings on the machine."""
    rows = [{"turn": f"t{i}", "labeller": "x", "text": t, "intent": k} for i, (t, k) in enumerate([
        ("I reject your claim", "disagree"), ("I disagree with that", "disagree"),
        ("sounds good, go ahead", "approve"), ("looks good to me", "approve"),
        ("please change the plan", "redirect")])]
    return json.loads(json.dumps(xx.export(rows, None)))


class Parallel(unittest.TestCase):
    def files_in(self, d):
        cl, cx = fake_home(d)
        return ([("claude", p) for p in rd.log_files(cl, "*/*.jsonl", None)]
                + [("codex", p) for p in rd.log_files(cx, "*/*/*/rollout-*.jsonl", None)])

    def test_parallel_and_serial_reports_agree(self):
        model = stub_model()
        with tempfile.TemporaryDirectory() as d:
            files = self.files_in(d)
            serial = rd.read(files, model, jobs=1)
            parallel = rd.read(files, model, jobs=2)
        self.assertEqual(serial, parallel)
        self.assertEqual((2, 3, 2, 1), (serial["files"], serial["turns"], serial["secrets"],
                                        serial["distinct_secrets"]))
        self.assertEqual([500], [g["tokens"] for g in serial["gaps"]])

    def test_progress_covers_every_file_in_either_mode(self):
        model = stub_model()
        for jobs in (1, 3):
            seen = []
            with tempfile.TemporaryDirectory() as d:
                rd.read(self.files_in(d), model, progress=lambda n, t, s: seen.append((n, t, round(s, 3))),
                        jobs=jobs)
            self.assertEqual([(1, 2), (2, 2)], [(n, t) for n, t, _ in seen], jobs)
            self.assertEqual(1.0, seen[-1][2])

    def test_a_copied_transcript_is_counted_once(self):
        """A resumed Claude session writes the earlier transcript again into a
        new file; its replies carry the same message ids, so the second copy
        adds no turns and no tokens.  Secret occurrences are still counted
        per line, since each copy is a place the value sits."""
        model = stub_model()
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            src = os.path.join(cl, "proj", "s1.jsonl")
            with open(src) as a, open(os.path.join(cl, "proj", "s2.jsonl"), "w") as b:
                b.write(a.read())
            files = ([("claude", p) for p in rd.log_files(cl, "*/*.jsonl", None)]
                     + [("codex", p) for p in rd.log_files(cx, "*/*/*/rollout-*.jsonl", None)])
            report = rd.read(files, model, jobs=1)
        self.assertEqual(3, report["files"])
        self.assertEqual(3, report["turns"])
        self.assertEqual(500, report["agent_tokens"])
        self.assertEqual(1, report["distinct_secrets"])
        self.assertEqual(4, report["secrets"])


class Report(unittest.TestCase):
    def setUp(self):
        rows = xx.load(xx.DEFAULT_DIR)
        if not rows:
            self.skipTest("no labelled turns on this machine")
        self.model = json.loads(json.dumps(xx.export(rows, xx.safe_vocab(rows))))

    def test_counts_turns_and_secrets_without_printing_them(self):
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            files = ([("claude", p) for p in rd.log_files(cl, "*/*.jsonl", None)]
                     + [("codex", p) for p in rd.log_files(cx, "*/*/*/rollout-*.jsonl", None)])
            report = rd.read(files, self.model)
        self.assertEqual(2, report["files"])
        self.assertEqual(3, report["turns"])
        self.assertEqual(2, report["secrets"])
        self.assertEqual(1, report["distinct_secrets"])  # the same value, twice
        self.assertEqual([500], [g["tokens"] for g in report["gaps"]])
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            files = [("claude", p) for p in rd.log_files(cl, "*/*.jsonl", None)]
            late = rd.read(files, self.model, since=rd._epoch("2026-09-01T04:00:00Z"))
        self.assertEqual([], late["gaps"])  # the turns before the window are not read
        page = rd.render_html(report)
        self.assertIn("<rect", page)
        self.assertNotIn(SECRET, page)
        self.assertNotIn("reject your claim", page)
        self.assertIn("disagree", report["intents"])
        self.assertIn("approve", report["intents"])
        text = rd.render(report) + json.dumps(report)
        self.assertNotIn(SECRET, text)
        self.assertNotIn("reject your claim", text)


class Bundle(unittest.TestCase):
    def test_one_file_runs_alone_and_leaks_nothing(self):
        rows = xx.load(xx.DEFAULT_DIR)
        if not rows:
            self.skipTest("no labelled turns on this machine")
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "xiaoxiang-local.py")
            with open(path, "w") as fh:
                fh.write(xx.bundle(rows))
            cl, cx = fake_home(d)
            out = subprocess.run([sys.executable, path, "--claude", cl, "--codex", cx, "--json"],
                                 cwd=d, capture_output=True, text=True, timeout=120,
                                 env={"PATH": os.environ.get("PATH", ""), "HOME": d})
        self.assertEqual(0, out.returncode, out.stderr)
        report = json.loads(out.stdout)
        self.assertEqual((2, 3, 2), (report["files"], report["turns"], report["secrets"]))
        self.assertNotIn(SECRET, out.stdout + out.stderr)


if __name__ == "__main__":
    unittest.main()
