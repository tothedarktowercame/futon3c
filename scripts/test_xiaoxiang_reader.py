"""Tests for xiaoxiang_reader.py and the standalone bundle.
Run: python3 -m unittest scripts/test_xiaoxiang_reader.py"""
import json
import os
from pathlib import Path
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

    def test_the_stretch_after_the_last_turn_counts(self):
        """Asked, then went to bed: no later turn closes the gap, so the last
        agent event does, and the gap is marked."""
        found = rd.gaps([0, 2 * H], [(1 * H, 10), (9 * H, 100), (12 * H, 1)])
        self.assertEqual([(2 * H, 12 * H, 101, "after")],
                         [(g["start"], g["end"], g["tokens"], g.get("edge")) for g in found])
        self.assertEqual([], rd.gaps([0, 2 * H], [(1 * H, 10), (9 * H, 100)], edges=False))
        before = rd.gaps([10 * H], [(1 * H, 50), (11 * H, 5)])
        self.assertEqual([(1 * H, 10 * H, 50, "before")],
                         [(g["start"], g["end"], g["tokens"], g.get("edge")) for g in before])
        page = rd.gap_svg({"gaps": found, "first_turn": 0, "last_turn": 2 * H, "gap_hours": 6})
        self.assertIn("<rect", page)
        self.assertIn("after your last turn", rd._edge_note(found[0]))

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
    model = json.loads(json.dumps(xx.export(rows, None)))
    model["common"] = []  # five rows make every word "common"; keep them as evidence
    return model


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


class Window(unittest.TestCase):
    def test_days_drops_old_turns_and_tokens_but_not_old_credentials(self):
        """Rob (2026-10-02): --days did not filter the session file.  It chose
        files by mtime and dropped old token events, but every turn in a chosen
        file was counted and classified whatever its date."""
        model = stub_model()
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            files = ([("claude", p) for p in rd.log_files(cl, "*/*.jsonl", None)]
                     + [("codex", p) for p in rd.log_files(cx, "*/*/*/rollout-*.jsonl", None)])
            whole = rd.read(files, model, jobs=1)
            late = rd.read(files, model, since=rd._epoch("2026-09-01T04:00:00Z"), jobs=1)
        self.assertEqual(3, whole["turns"])
        self.assertEqual(1, late["turns"], "only the codex turn at 09:00 is inside the window")
        self.assertEqual({"approve": 1}, late["intents"])
        self.assertEqual(0, late["agent_tokens"] - 500, "the 05:00 reply is inside the window")
        self.assertEqual(whole["secrets"], late["secrets"], "credentials are counted wherever they sit")


class Provenance(unittest.TestCase):
    def test_where_credentials_sit(self):
        model = stub_model()
        key = "AKIAABCDEFGHIJKLMNOP"
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            with open(os.path.join(cl, "proj", "s3.jsonl"), "w") as fh:
                write = {"type": "assistant", "timestamp": "2026-09-02T00:00:00Z",
                         "message": {"content": [{"type": "tool_use", "name": "Write",
                                                  "input": {"file_path": "tests/keys_test.py",
                                                            "content": f"KEY = '{key}'"}}]}}
                prose = {"type": "assistant", "timestamp": "2026-09-02T00:00:01Z",
                         "message": {"content": [{"type": "text", "text": f"use AKIAIOSFODNN7EXAMPLE or sk-ant-{'q' * 24}"}]}}
                for r in (write, prose):
                    fh.write(json.dumps(r) + "\n")
            files = [("claude", p) for p in rd.log_files(cl, "*/*.jsonl", None)]
            report = rd.read(files, model, jobs=1)
        where = report["secret_where"]
        self.assertEqual(1, where["typed"]["occurrences"])
        self.assertEqual(1, where["tool-output"]["occurrences"])
        self.assertEqual(2, where["fixture"]["occurrences"], "the test-path write and the documented example key")
        self.assertEqual(1, where["agent-said"]["occurrences"])
        text = rd.render(report)
        self.assertIn("in what you typed", text)
        self.assertIn("in test fixtures or documented example keys", text)
        self.assertNotIn(key, text)
        self.assertIn("where they sit", rd.render_html(report))

    def test_where_in_for_codex_records(self):
        self.assertEqual(("typed", False), rd.where_in("codex", codex("hi")))
        self.assertEqual(("agent-said", False), rd.where_in("codex", codex("hi", role="assistant")))
        call = {"type": "response_item", "payload": {"type": "function_call", "name": "shell",
                                                     "arguments": "{\"cmd\": \"cat tests/fixture.py\"}"}}
        self.assertEqual(("agent-wrote", True), rd.where_in("codex", call))
        self.assertEqual(("elsewhere", False), rd.where_in("codex", None))


class Precision(unittest.TestCase):
    def test_export_carries_cross_validated_precision(self):
        model = stub_model()
        self.assertIn("precision", model)
        self.assertTrue(set(model["precision"]) <= {"disagree", "approve", "redirect"})

    def test_unsure_intents_are_not_reported_as_labels(self):
        model = dict(stub_model(), precision={"disagree": 0.9, "approve": 0.3})
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            files = ([("claude", p) for p in rd.log_files(cl, "*/*.jsonl", None)]
                     + [("codex", p) for p in rd.log_files(cx, "*/*/*/rollout-*.jsonl", None)])
            report = rd.read(files, model, jobs=1)
        self.assertEqual({"disagree": 1}, report["intents"])
        self.assertEqual(1, report["not_sure"])
        self.assertEqual({"disagree": 0.9}, report["intent_precision"])
        text = rd.render(report)
        self.assertIn(" 90%", text)
        self.assertIn("right on less than half the time", text)
        self.assertNotIn("often wrong", text)
        old = dict(stub_model())
        old.pop("precision", None)
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            report = rd.read([("claude", p) for p in rd.log_files(cl, "*/*.jsonl", None)], old, jobs=1)
        self.assertIn("often wrong", rd.render(report))


class UnderAnAgent(unittest.TestCase):
    """Rob's report (2026-10-02): his agent ran the scan, was told which files
    held credentials, and read them.  The report is not a map."""

    def report(self):
        model = stub_model()
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            files = [("claude", p) for p in rd.log_files(cl, "*/*.jsonl", None)]
            return rd.read(files, model, jobs=1)

    def test_detection(self):
        self.assertTrue(rd.run_by_agent({"CLAUDECODE": "1"}))
        self.assertTrue(rd.run_by_agent({"CLAUDE_CODE_ENTRYPOINT": "cli"}))
        self.assertTrue(rd.run_by_agent({"CODEX_THREAD_ID": "x"}))
        self.assertTrue(rd.run_by_agent({"CODEX_SESSION_ID": "x"}))
        self.assertTrue(rd.run_by_agent({"AI_AGENT": "1"}))
        self.assertFalse(rd.run_by_agent({"HOME": "/home/rob", "CLAUDE_MD": "x"}))
        self.assertFalse(rd.run_by_agent({"CLAUDE_CODE_DISABLE_TERMINAL_TITLE": "1"}))

    def test_paths_are_withheld_by_default_and_named_only_on_request(self):
        report = self.report()
        path = next(iter(report["files_with_secrets"]))
        text = rd.render(report)
        self.assertNotIn(path, text)
        self.assertIn("They sit in 1 of the files read; --list-files names them.", text)
        self.assertIn(path, rd.render(report, list_files=True))

    def test_an_agent_gets_the_notice_and_never_the_paths(self):
        report = dict(self.report(), run_by_agent=True)
        path = next(iter(report["files_with_secrets"]))
        text = rd.render(report, list_files=True)
        self.assertNotIn(path, text)
        self.assertIn("do not open the log files", text)
        self.assertNotIn("--list-files names them", text)

    def test_main_refuses_list_files_under_an_agent_and_strips_paths_from_json(self):
        model = stub_model()
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            mp = os.path.join(d, "model.json")
            with open(mp, "w") as fh:
                json.dump(model, fh)
            base = ["--claude", cl, "--codex", cx, "--model", mp, "--html", "", "--jobs", "1", "--yes"]
            env_agent = {"PATH": os.environ.get("PATH", ""), "HOME": d, "CLAUDECODE": "1"}
            env_human = {"PATH": os.environ.get("PATH", ""), "HOME": d}
            here = os.path.dirname(os.path.abspath(rd.__file__))
            refused = subprocess.run([sys.executable, rd.__file__, *base, "--list-files"], cwd=here,
                                     capture_output=True, text=True, env=env_agent)
            self.assertEqual(2, refused.returncode)
            self.assertIn("refused", refused.stderr)
            self.assertEqual("", refused.stdout)
            verbose_refused = subprocess.run([sys.executable, rd.__file__, *base, "--verbose"],
                                             cwd=here, capture_output=True, text=True,
                                             env=env_agent)
            self.assertEqual(2, verbose_refused.returncode)
            self.assertIn("refused", verbose_refused.stderr)
            self.assertEqual("", verbose_refused.stdout)
            agent = subprocess.run([sys.executable, rd.__file__, *base, "--json"], cwd=here,
                                   capture_output=True, text=True, env=env_agent)
            self.assertEqual(0, agent.returncode, agent.stderr)
            out = json.loads(agent.stdout)
            self.assertEqual(1, out["files_with_secrets"], "a count, not the paths")
            self.assertIn("do not open the log files", out["notice"])
            self.assertNotIn(cl, agent.stdout)
            human = subprocess.run([sys.executable, rd.__file__, *base, "--list-files"], cwd=here,
                                   capture_output=True, text=True, env=env_human)
            self.assertEqual(0, human.returncode, human.stderr)
            self.assertIn("Files holding them", human.stdout)
            self.assertIn("s1.jsonl", human.stdout)
            self.assertNotIn(SECRET, human.stdout + agent.stdout)


class VerboseTurns(unittest.TestCase):
    def test_only_structured_matches_in_typed_turns_are_shown(self):
        github = "ghp_" + "A" * 24
        random = "Z" * 40
        model = stub_model()
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "turns.jsonl")
            with open(path, "w") as fh:
                fh.write(json.dumps({**claude(f"please inspect {github}"),
                                     "timestamp": "2026-10-02T12:00:00Z"}) + "\n")
                fh.write(json.dumps({**claude(f"ignore this random id {random}"),
                                     "timestamp": "2026-10-02T12:01:00Z"}) + "\n")
            report = rd.read([("claude", Path(path))], model, jobs=1, verbose=True)
        text = rd.render(report)
        self.assertIn(github, text)
        self.assertIn("github-token", text)
        self.assertNotIn(random, text)
        self.assertNotIn("--- high-entropy", text)
        self.assertNotIn("--- keyword-assignment", text)

    def test_agent_render_never_includes_verbose_turns(self):
        report = {"turns": 0, "files": 1, "gaps": [], "distinct_secrets": 0,
                  "secrets": 0, "secret_kinds": {}, "files_with_secrets": {},
                  "verbose_turns": [{"kinds": ["github-token"], "text": "SECRET",
                                     "path": "/private/log", "timestamp": None}],
                  "run_by_agent": True}
        text = rd.render(report)
        self.assertNotIn("SECRET", text)
        self.assertNotIn("/private/log", text)

    def test_preamble_discloses_verbose_output(self):
        text = rd.preamble("claude", "codex", 7, verbose=True)
        self.assertIn("complete text of your turns", text)
        self.assertIn("high-entropy and keyword-assignment matches stay excluded", text)
        self.assertNotIn("Never:   the credential values", text)

    def test_cli_allows_verbose_with_explicit_consent_without_a_tty(self):
        here = os.path.dirname(os.path.abspath(rd.__file__))
        with tempfile.TemporaryDirectory() as d:
            model = os.path.join(d, "model.json")
            with open(model, "w") as fh:
                json.dump(stub_model(), fh)
            env = {"PATH": os.environ.get("PATH", ""), "HOME": d}
            run = subprocess.run([sys.executable, rd.__file__, "--model", model,
                                  "--verbose", "--yes", "--claude", d,
                                  "--codex", d, "--html", ""], cwd=here,
                                 capture_output=True, text=True, env=env,
                                 stdin=subprocess.DEVNULL)
        self.assertEqual(1, run.returncode)
        self.assertIn("No Claude Code or Codex logs found", run.stderr)


class Consent(unittest.TestCase):
    def test_without_a_terminal_nothing_is_read_unless_yes(self):
        model = stub_model()
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            mp = os.path.join(d, "model.json")
            with open(mp, "w") as fh:
                json.dump(model, fh)
            here = os.path.dirname(os.path.abspath(rd.__file__))
            env = {"PATH": os.environ.get("PATH", ""), "HOME": d}
            base = [sys.executable, rd.__file__, "--claude", cl, "--codex", cx, "--model", mp, "--html", "", "--json"]
            declined = subprocess.run(base, cwd=here, capture_output=True, text=True, env=env, stdin=subprocess.DEVNULL)
            self.assertEqual(3, declined.returncode)
            self.assertEqual("", declined.stdout)
            self.assertIn("about to read your agent logs", declined.stderr)
            self.assertIn("pass --yes to proceed", declined.stderr)
            self.assertIn("Nothing was read.", declined.stderr)
            agreed = subprocess.run(base + ["--yes"], cwd=here, capture_output=True, text=True, env=env, stdin=subprocess.DEVNULL)
            self.assertEqual(0, agreed.returncode, agreed.stderr)
            self.assertIn("about to read your agent logs", agreed.stderr)
            self.assertEqual(3, json.loads(agreed.stdout)["turns"])

    def test_preamble_says_what_it_never_prints_and_what_an_agent_learns(self):
        text = rd.preamble("~/.claude/projects", "~/.codex/sessions", 7)
        self.assertIn("the last 7 days of ~/.claude/projects", text)
        self.assertIn("Never:   the credential values", text)
        self.assertIn("If an agent runs this, it learns the counts and kinds above and nothing more", text)
        self.assertIn("all of ~/.claude/projects", rd.preamble("~/.claude/projects", "~/.codex/sessions", None))

    def test_report_tells_the_person_what_to_do_per_kind(self):
        model = stub_model()
        with tempfile.TemporaryDirectory() as d:
            cl, cx = fake_home(d)
            report = rd.read([("claude", p) for p in rd.log_files(cl, "*/*.jsonl", None)], model, jobs=1)
        text = rd.render(report)
        self.assertIn("What to do: rotate first", text)
        self.assertIn("keyword-assignment   look at which setting it was", text)


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
            out = subprocess.run([sys.executable, path, "--claude", cl, "--codex", cx, "--json", "--yes"],
                                 cwd=d, capture_output=True, text=True, timeout=120,
                                 env={"PATH": os.environ.get("PATH", ""), "HOME": d})
        self.assertEqual(0, out.returncode, out.stderr)
        report = json.loads(out.stdout)
        self.assertEqual((2, 3, 2), (report["files"], report["turns"], report["secrets"]))
        self.assertNotIn(SECRET, out.stdout + out.stderr)


if __name__ == "__main__":
    unittest.main()
