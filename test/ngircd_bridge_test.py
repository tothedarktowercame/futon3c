import importlib.util
import pathlib
import unittest


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


if __name__ == "__main__":
    unittest.main()
