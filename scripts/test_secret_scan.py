import importlib.util
import json
from pathlib import Path
import subprocess
import sys
import unittest


SPEC = importlib.util.spec_from_file_location(
    "secret_scan", Path(__file__).with_name("secret_scan.py"))
scan_module = importlib.util.module_from_spec(SPEC)
sys.modules[SPEC.name] = scan_module
SPEC.loader.exec_module(scan_module)


class SecretScanTest(unittest.TestCase):
    def assert_detected(self, kind, text, secret):
        findings = scan_module.scan(text)
        self.assertIn(kind, {finding.kind for finding in findings})
        redacted, _ = scan_module.redact(text)
        for index in range(max(0, len(secret) - 7)):
            self.assertNotIn(secret[index:index + 8], redacted)

    def test_every_kind(self):
        cases = {
            "private-key": (
                "-----BEGIN PRIVATE KEY-----\nRmFrZVByaXZhdGVLZXlCeXRlc09ubHk=\n-----END PRIVATE KEY-----",
                "-----BEGIN PRIVATE KEY-----\nRmFrZVByaXZhdGVLZXlCeXRlc09ubHk=\n-----END PRIVATE KEY-----",
            ),
            "aws-access-key": ("AKIAFAKE1234567890XY", "AKIAFAKE1234567890XY"),
            "github-token": ("ghp_FakeToken0123456789ABCDEFGH", "ghp_FakeToken0123456789ABCDEFGH"),
            "anthropic-key": ("sk-ant-FakeClaudeKey0123456789ABCDEFG", "sk-ant-FakeClaudeKey0123456789ABCDEFG"),
            "openai-key": ("sk-proj-FakeOpenAIKey0123456789ABCDEFG", "sk-proj-FakeOpenAIKey0123456789ABCDEFG"),
            "slack-token": ("xoxb-FakeSlack0123456789-ABCDEFG", "xoxb-FakeSlack0123456789-ABCDEFG"),
            "google-api-key": ("AIzaFakeGoogleApiKey0123456789ABCDEFG", "AIzaFakeGoogleApiKey0123456789ABCDEFG"),
            "jwt": ("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJmYWtlIn0.FakeSignature123", "eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJmYWtlIn0.FakeSignature123"),
            "bearer": ("Authorization: Bearer FakeBearerToken0123456789", "FakeBearerToken0123456789"),
            "url-credentials": ("https://fake-user:FakePassword987@example.invalid/a", "FakePassword987"),
            "keyword-assignment": ('{"password": "FakeAssignedPassword987"}', "FakeAssignedPassword987"),
            "high-entropy": ("Az9/By8_Cx7+Dw6=Ev5/Fu4_Gt3+Hs2=Jk1", "Az9/By8_Cx7+Dw6=Ev5/Fu4_Gt3+Hs2=Jk1"),
        }
        for kind, (text, secret) in cases.items():
            with self.subTest(kind=kind):
                self.assert_detected(kind, text, secret)

    def test_keyword_assignment_forms(self):
        for text, value in [
            ("password: FakePass1234", "FakePass1234"),
            ("API_KEY='FakeApiValue1234'", "FakeApiValue1234"),
            ("SERVICE_CLIENT_SECRET=FakeClientSecret123", "FakeClientSecret123"),
            ("FOO_PASSWORD=FakeEnvPassword123", "FakeEnvPassword123"),
        ]:
            with self.subTest(text=text):
                self.assert_detected("keyword-assignment", text, value)

    def test_false_positives(self):
        clean = [
            "commit 7e57a91 and 0123456789abcdef0123456789abcdef01234567",
            "sha256 " + "a" * 64,
            "id 123e4567-e89b-42d3-a456-426614174000",
            "/home/joe/code/futon3c/scripts/agency_send.py",
            "invoke-1790652170192-27459-5b8573de",
            "the password check can be the first classical tool",
            "token counts and a secret test are ordinary prose",
            "password:",
            "password=<redacted> password=*** password=xxx password=changeme?",
            "data:image/png;base64,AbCdEf0123456789",
        ]
        for text in clean:
            with self.subTest(text=text):
                self.assertEqual([], scan_module.scan(text))

    def test_mixed_paragraph_has_exactly_one_finding(self):
        text = (
            "The password check is documented at /home/joe/code/tool.py. "
            "Commit 0123456789abcdef0123456789abcdef01234567 and UUID "
            "123e4567-e89b-42d3-a456-426614174000 are identifiers. "
            "Credential: AKIAFAKE1234567890XY"
        )
        findings = scan_module.scan(text)
        self.assertEqual(1, len(findings))
        self.assertEqual("aws-access-key", findings[0].kind)

    def test_overlap_never_leaks_part_of_secret(self):
        secret = "sk-proj-FakeOverlapToken0123456789ABCDE"
        redacted, findings = scan_module.redact("Bearer " + secret)
        self.assertEqual(1, len(findings))
        self.assertNotIn(secret, redacted)
        self.assertNotIn(secret[:8], redacted)

    def test_agent_log_regressions(self):
        # Found in review (claude-3, 2026-09-29): shapes that occur in agent
        # .jsonl logs and Markdown chat, where the first version leaked.
        for kind, text, secret in [
            ("keyword-assignment", '{"password": "correct horse battery staple"}',
             "horse battery staple"),
            ("keyword-assignment", "**Password:** Hunter2Fake99", "Hunter2Fake99"),
            ("keyword-assignment", "**Password**: Hunter2Fake99", "Hunter2Fake99"),
            ("keyword-assignment", "`api_key`=FakeApiValue1234", "FakeApiValue1234"),
            ("keyword-assignment", r'{"content":"{\"password\": \"FakeEsc4pedPw\"}"}',
             "FakeEsc4pedPw"),
            ("github-token", r'"output":"line1\nghp_FakeToken0123456789ABCDEFGH\n"',
             "ghp_FakeToken0123456789ABCDEFGH"),
            ("anthropic-key", r'"x\tsk-ant-FakeClaudeKey0123456789ABCDEFG"',
             "sk-ant-FakeClaudeKey0123456789ABCDEFG"),
        ]:
            with self.subTest(text=text):
                self.assert_detected(kind, text, secret)

    def test_agent_log_false_positives(self):
        import base64
        blob = base64.b64encode(bytes(range(256)) * 3).decode()
        for text in [
            "PWD=/home/joe/code/futon3c\nOLDPWD=~/code",
            "max_token=4096 and token: 12",
            "the bearer instrument clause",
            '"data":"' + blob + '"',
            "password=*** password=xxx",
        ]:
            with self.subTest(text=text[:40]):
                self.assertEqual([], scan_module.scan(text))

class SecretScanCliTest(unittest.TestCase):
    script = Path(__file__).with_name("secret_scan.py")

    def run_cli(self, *args, input_text=""):
        return subprocess.run(
            [sys.executable, str(self.script), *args],
            input=input_text,
            text=True,
            capture_output=True,
            check=False,
        )

    def test_clean_exit_zero(self):
        result = self.run_cli(input_text="ordinary token counts\n")
        self.assertEqual(0, result.returncode)
        self.assertEqual("ordinary token counts\n", result.stdout)

    def test_check_exit_one_without_secret_output(self):
        secret = "AKIAFAKE1234567890XY"
        result = self.run_cli("--check", input_text=secret)
        self.assertEqual(1, result.returncode)
        self.assertEqual("", result.stdout)
        self.assertIn("aws-access-key", result.stderr)
        self.assertNotIn(secret[:8], result.stderr)

    def test_json_shape(self):
        result = self.run_cli("--json", input_text="pwd=FakePassword123")
        self.assertEqual(1, result.returncode)
        value = json.loads(result.stdout)
        self.assertEqual(["end", "kind", "start"], sorted(value[0]))
        self.assertEqual("keyword-assignment", value[0]["kind"])
        self.assertNotIn("FakePassword", result.stdout)


if __name__ == "__main__":
    unittest.main()
