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


if __name__ == "__main__":
    unittest.main()
