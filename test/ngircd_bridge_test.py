import importlib.util
import pathlib
import unittest
from unittest import mock


def _load_bridge_module():
    root = pathlib.Path(__file__).resolve().parents[1]
    path = root / "scripts" / "ngircd_bridge.py"
    spec = importlib.util.spec_from_file_location("ngircd_bridge", path)
    module = importlib.util.module_from_spec(spec)
    assert spec.loader is not None
    spec.loader.exec_module(module)
    return module


bridge = _load_bridge_module()


class NgircdBridgeSanitizeTest(unittest.TestCase):
    def test_sanitize_for_irc_uses_ircbot_helper(self):
        text = "/home/joe/code/futon3c/scripts/ngircd_bridge.py"
        sanitized = bridge.IRCBot._sanitize_for_irc(text)
        self.assertEqual("~/code/futon3c/scripts/ngircd_bridge.py", sanitized)


class NgircdBridgeAnnounceTest(unittest.TestCase):
    def test_announce_invoke_uses_server_canonical_job_id(self):
        bot = bridge.IRCBot("codex", "codex-1", "#math", "localhost", 6667, "pw")
        with mock.patch.object(bridge, "api_post", return_value={
            "ok": True,
            "job-id": "codex-announce-7",
            "queued-jobs": 3,
            "status-url": "/api/alpha/invoke/jobs/codex-announce-7",
        }) as api_post:
            result = bot._announce_invoke("joe", "prompt text", "FM-001", reply_channel="#math")
        self.assertTrue(result["ok"])
        self.assertEqual("codex-announce-7", result["job_id"])
        self.assertEqual(3, result["queued_jobs"])
        api_post.assert_called_once()
        url, payload = api_post.call_args.args[:2]
        self.assertEqual(bridge.INVOKE_ANNOUNCE_URL, url)
        self.assertEqual("codex-1", payload["agent-id"])
        self.assertEqual("irc:joe", payload["caller"])
        self.assertEqual("irc (#math)", payload["surface"])
        self.assertEqual("FM-001", payload["mission-id"])


class NgircdBridgeCodexFormattingTest(unittest.TestCase):
    def test_codex_mention_does_not_emit_accepted_ack(self):
        bot = bridge.IRCBot("codex", "codex-1", "#math", "localhost", 6667, "pw")
        with mock.patch.object(bot, "_announce_invoke", return_value={
            "ok": True,
            "job_id": "codex-job-1",
            "queued_jobs": 1,
        }), mock.patch.object(bot, "_enqueue_invoke", return_value="codex-job-1"), mock.patch.object(bot, "_say") as say:
            bot._handle_mention("joe", "@codex check the solver", channel="#math")
        say.assert_not_called()

    def test_codex_clean_summary_omits_done_prefix_and_session_suffix(self):
        bot = bridge.IRCBot("codex", "codex-1", "#math", "localhost", 6667, "pw")
        with mock.patch.object(bot, "_say") as say:
            bot._emit_success_reply({
            "ok": True,
            "result": "Re-running `kissat --time=3600 FM001-n6.cnf` now.",
            "session_id": "019ccdc0abcdef",
            "invoke_meta": {"execution": {"executed?": True, "tool-events": 1, "command-events": 1}},
        }, "#math", "codex-job-2", multi_message=False)
        say.assert_called_once()
        text = say.call_args.args[0]
        self.assertNotIn("[done", text)
        self.assertNotIn("(session ", text)
        self.assertNotIn("artifact refs", text)
        self.assertTrue(text.startswith("Re-running `kissat --time=3600 FM001-n6.cnf` now."))

    def test_fallback_summary_omits_no_artifact_and_execution_suffixes(self):
        bot = bridge.IRCBot("helper", "codex-1", "#math", "localhost", 6667, "pw")
        with mock.patch.object(bot, "_uses_clean_irc_output", return_value=False), \
             mock.patch.object(bot, "_say") as say:
            bot._emit_success_reply({
                "ok": True,
                "result": "@joe yep, loud and clear.",
                "session_id": "019ccdc0abcdef",
                "invoke_meta": {"execution": {"executed?": False, "tool-events": 0, "command-events": 0}},
            }, "#math", "codex-job-3", multi_message=False)
        say.assert_called_once()
        text = say.call_args.args[0]
        self.assertNotIn("no artifact refs", text)
        self.assertNotIn("no execution evidence", text)


if __name__ == "__main__":
    unittest.main()
