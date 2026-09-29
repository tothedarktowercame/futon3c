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
            fh.write(json.dumps(r) + "\n")
    with open(os.path.join(cx, "rollout-1.jsonl"), "w") as fh:
        fh.write(json.dumps(codex("sounds good, go ahead")) + "\n")
        fh.write("not json\n")
    return os.path.join(root, "claude"), os.path.join(root, "codex")


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
