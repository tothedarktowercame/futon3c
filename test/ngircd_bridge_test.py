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

    def test_frontiermath_math_room_control_mentions_bypass_generic_bridge_queue(self):
        bot = bridge.IRCBot("tickle", "tickle-1", "#math", "localhost", 6667, "pw")
        self.assertTrue(
            bot._is_frontiermath_room_control_message(
                "@tickle BELL SPEC_VERIFIED", channel="#math"
            )
        )
        self.assertTrue(
            bot._is_frontiermath_room_control_message(
                "@tickle I'll take T3-general", channel="#math"
            )
        )
        self.assertFalse(
            bot._is_frontiermath_room_control_message(
                "@tickle BELL SPEC_VERIFIED", channel="#futon"
            )
        )
        self.assertFalse(
            bot._is_frontiermath_room_control_message(
                "@codex check the solver", channel="#math"
            )
        )

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


class NgircdBridgeSurfaceContractTest(unittest.TestCase):
    def test_task_surface_context_uses_commit_algorithm_and_tracked_work_item(self):
        bot = bridge.IRCBot("codex", "codex-1", "#math", "localhost", 6667, "pw")
        text = bot._surface_context("joe", "", brief=False, multi_message=False, channel="#math")
        self.assertIn("(commit/tracked work item/file path)", text)
        self.assertIn("Run the commit algorithm for gh before referencing commit artifacts", text)
        self.assertIn("use tracked work-item refs for blocker/work-item continuity", text)
        self.assertNotIn("(commit/PR/issue/file path)", text)
        self.assertNotIn("Push artifacts to git before referencing them", text)
        self.assertNotIn("mfuton gitlab issue", text)

    def test_say_truncation_points_to_tracked_work_item(self):
        bot = bridge.IRCBot("codex", "codex-1", "#math", "localhost", 6667, "pw")
        with mock.patch.object(bot, "_send") as send, mock.patch.object(bridge.time, "sleep"):
            bot._say("line1\nline2\nline3", max_lines=2, channel="#math")
        self.assertEqual(3, send.call_count)
        self.assertIn("post details to tracked work item instead", send.call_args_list[2].args[0])
        self.assertNotIn("post details to GitHub instead", send.call_args_list[2].args[0])

    def test_extract_artifact_refs_promotes_frontiermath_local_paths(self):
        text = (
            "See http://192.168.165.188/mfuton/-/issues/2 and "
            "mfuton/data/frontiermath-local/FM-001/runs/"
            "2026-03-12-live-runtime-proof-root-cycle-230802Z/ for the receipt bundle. "
            "Writable root is mfuton/data/frontiermath-local/FM-001/active."
        )
        refs = bridge.IRCBot._extract_artifact_refs(text)
        self.assertEqual(
            "mfuton/data/frontiermath-local/FM-001/runs/2026-03-12-live-runtime-proof-root-cycle-230802Z/",
            refs[0],
        )
        self.assertIn("mfuton/data/frontiermath-local/FM-001/active", refs)


class NgircdBridgePendingTimeoutTest(unittest.TestCase):
    def test_response_is_pending_timeout(self):
        self.assertTrue(bridge.IRCBot._response_is_pending_timeout({"pending": True}))
        self.assertFalse(bridge.IRCBot._response_is_pending_timeout({"pending": False}))

    def test_job_timeout_is_provisional_while_agent_invoking(self):
        bot = bridge.IRCBot("codex", "codex-1", "#math", "localhost", 6667, "pw")
        job = {"state": "timeout", "terminal-code": "timeout"}
        status = {"status": "invoking"}
        self.assertTrue(bot._job_timeout_while_agent_still_invoking(job, status))
        self.assertFalse(bot._job_timeout_while_agent_still_invoking(job, {"status": "idle"}))

    def test_pending_invoke_waits_past_timeout_until_agent_is_idle(self):
        bot = bridge.IRCBot("codex", "codex-1", "#math", "localhost", 6667, "pw")
        with mock.patch.object(bot, "_agent_status", side_effect=[
            {"status": "invoking"},
            {"status": "idle"},
        ]), mock.patch.object(bot, "_fetch_job", side_effect=[
            {"job-id": "codex-job-9", "state": "timeout", "terminal-code": "timeout"},
            {"job-id": "codex-job-9", "state": "timeout", "terminal-code": "timeout"},
        ]), mock.patch.object(bridge.time, "sleep"):
            result = bot._wait_for_pending_invoke_terminal("codex-job-9")
        self.assertEqual("job", result["kind"])
        self.assertEqual("timeout", result["job"]["state"])

    def test_pending_invoke_emits_still_running_then_terminal_reply(self):
        bot = bridge.IRCBot("codex", "codex-1", "#math", "localhost", 6667, "pw")
        response = {
            "ok": False,
            "pending": True,
            "job_id": "codex-job-11",
            "error": "invoke timeout: codex-1 is already invoking",
        }
        awaited = {
            "kind": "job",
            "job": {
                "job-id": "codex-job-11",
                "state": "done",
                "result-summary": "SAT check finished; artifact refs: abc123",
                "trace-id": "invoke-123",
            },
        }
        with mock.patch.object(bot, "_say") as say, \
             mock.patch.object(bot, "_wait_for_pending_invoke_terminal", return_value=awaited), \
             mock.patch.object(bot, "_record_job_delivery_receipt") as record:
            bot._handle_pending_invoke(response, "#math", "codex-job-11")
        self.assertEqual(2, say.call_count)
        self.assertIn("[still running codex-job-11]", say.call_args_list[0].args[0])
        self.assertIn("SAT check finished; artifact refs: abc123", say.call_args_list[1].args[0])
        record.assert_called_once()

    def test_pending_invoke_terminal_reply_appends_canonical_local_artifact_ref(self):
        bot = bridge.IRCBot("codex", "codex-1", "#math", "localhost", 6667, "pw")
        response = {
            "ok": False,
            "pending": True,
            "job_id": "codex-job-12",
            "error": "invoke timeout: codex-1 is already invoking",
        }
        awaited = {
            "kind": "job",
            "job": {
                "job-id": "codex-job-12",
                "state": "done",
                "result-summary": "Bounded FM local run bundle recorded",
                "artifact-ref": "mfuton/data/frontiermath-local/FM-001/runs/2026-03-12-live-runtime-proof-root-cycle-230802Z/",
                "trace-id": "invoke-124",
            },
        }
        with mock.patch.object(bot, "_say") as say, \
             mock.patch.object(bot, "_wait_for_pending_invoke_terminal", return_value=awaited), \
             mock.patch.object(bot, "_record_job_delivery_receipt") as record:
            bot._handle_pending_invoke(response, "#math", "codex-job-12")
        self.assertEqual(2, say.call_count)
        self.assertIn("Bounded FM local run bundle recorded", say.call_args_list[1].args[0])
        self.assertIn(
            "mfuton/data/frontiermath-local/FM-001/runs/2026-03-12-live-runtime-proof-root-cycle-230802Z/",
            say.call_args_list[1].args[0],
        )
        record.assert_called_once()


if __name__ == "__main__":
    unittest.main()
