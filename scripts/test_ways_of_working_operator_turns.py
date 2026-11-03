#!/usr/bin/env python3
import datetime
import importlib.util
import io
import json
import pathlib
import tempfile
import unittest

HERE = pathlib.Path(__file__).resolve().parent
SPEC = importlib.util.spec_from_file_location("wow_probe", HERE / "ways-of-working-probe.py")
wow = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(wow)
UTC = datetime.timezone.utc

def turn(at, message_id, vendor="claude", text="same words"):
    return {"vendor": vendor, "id": message_id, "at": at, "agent": "claude-1",
            "surface": "emacs-repl", "text": wow.normalize_turn_text(text)}

def evidence(at, evidence_id="emacs-one", text="same words", transport="emacs-claude-repl"):
    return {"evidence/id": evidence_id, "evidence/at": wow.iso(at),
            "evidence/origin": {"kind": "operator"},
            "evidence/body": {"event": "chat-turn", "role": "user",
                              "transport": transport, "text": text}}

class Response(io.BytesIO):
    def __enter__(self): return self
    def __exit__(self, *_): self.close()

class OperatorTurnProbeTest(unittest.TestCase):
    def test_one_evidence_cannot_cover_two_nearby_turns(self):
        now = datetime.datetime(2026, 11, 3, 12, tzinfo=UTC)
        turns = [turn(now - datetime.timedelta(minutes=10), "a"),
                 turn(now - datetime.timedelta(minutes=9, seconds=40), "b")]
        rows = [evidence(now - datetime.timedelta(minutes=10))]
        # The rejected window-only algorithm would call both turns matched.
        naive_window_matches = sum(any(abs((wow.parse_time(e["evidence/at"]) - t["at"]).total_seconds()) <= 60
                                       for e in rows) for t in turns)
        self.assertEqual(2, naive_window_matches)
        actual = wow.match_operator_turns(turns, rows, now)
        self.assertEqual(1, actual["matched"])
        self.assertEqual(1, len(actual["violations"]))

    def test_mature_missing_is_violation_but_young_missing_is_pending(self):
        now = datetime.datetime(2026, 11, 3, 12, tzinfo=UTC)
        mature = wow.match_operator_turns([turn(now - datetime.timedelta(minutes=10), "old")], [], now)
        young = wow.match_operator_turns([turn(now - datetime.timedelta(seconds=30), "new")], [], now)
        self.assertEqual(1, len(mature["violations"]))
        self.assertEqual(0, len(mature["pending"]))
        self.assertEqual(0, len(young["violations"]))
        self.assertEqual(1, len(young["pending"]))

    def test_quoted_operator_header_inside_agent_turn_is_not_counted(self):
        text = ("--- CURRENT TURN ---\nSurface: bell\nOrigin: agent\n---\n\n"
                "quoted: --- CURRENT TURN ---\nOrigin: operator\n---\n")
        self.assertIsNone(wow.parse_operator_header(text))

    def test_vendor_duplicates_count_once(self):
        now = datetime.datetime(2026, 11, 3, 12, tzinfo=UTC)
        header = "--- CURRENT TURN ---\nSurface: emacs-repl\nTo: {agent}\nOrigin: operator\n---\n\nhello"
        with tempfile.TemporaryDirectory() as root:
            root = pathlib.Path(root); claude = root / "claude"; codex = root / "codex"
            claude.mkdir(); codex.mkdir()
            row = {"type": "user", "timestamp": wow.iso(now), "uuid": "c-one",
                   "message": {"content": [{"type": "text", "text": header.format(agent="claude-1")} ]}}
            (claude / "a.jsonl").write_text(json.dumps(row) + "\n" + json.dumps(row) + "\n")
            crow = {"type": "response_item", "timestamp": wow.iso(now),
                    "payload": {"type": "message", "role": "user", "id": "x-one",
                                "content": [{"type": "input_text", "text": header.format(agent="codex-1")} ]}}
            (codex / "rollout-a.jsonl").write_text(json.dumps(crow) + "\n" + json.dumps(crow) + "\n")
            turns = wow.read_operator_turns(now + datetime.timedelta(seconds=1), claude, codex)
        self.assertEqual([("claude", "c-one"), ("codex", "x-one")],
                         [(x["vendor"], x["id"]) for x in turns])

    def test_truncated_window_breaks_monitor(self):
        now = datetime.datetime(2026, 11, 3, 12, tzinfo=UTC)
        payload = json.dumps({"entries": [{"evidence/id": str(i)} for i in range(2)],
                              "count": 2}).encode()
        with self.assertRaisesRegex(RuntimeError, "truncated"):
            wow.fetch_operator_evidence([turn(now, "a")],
                                        opener=lambda *_args, **_kw: Response(payload),
                                        pause=lambda _seconds: None, limit=2)
        with tempfile.TemporaryDirectory() as root:
            root = pathlib.Path(root); (root / "c").mkdir(); (root / "x").mkdir()
            result = wow.probe_operator_turns(now, root / "c", root / "x",
                                              fetcher=lambda _turns: (_ for _ in ()).throw(RuntimeError("truncated")))
        self.assertEqual("broken", result["monitor"])
        self.assertEqual("unknown", result["invariant"])

if __name__ == "__main__":
    unittest.main()
