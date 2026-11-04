#!/usr/bin/env python3
import contextlib
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

class BusyStoreTest(unittest.TestCase):
    def busy(self):
        import urllib.error
        return urllib.error.HTTPError("u", 503, "Service Unavailable", {}, None)

    def test_busy_store_is_retried_then_read(self):
        import io, json as _json
        calls, waits = [], []
        def opener(request, timeout):
            calls.append(1)
            if len(calls) < 3: raise self.busy()
            return io.BytesIO(_json.dumps({"ok": 1}).encode())
        req = wow.urllib.request.Request("http://x")
        self.assertEqual({"ok": 1}, wow.open_json(req, 1, opener, waits.append))
        self.assertEqual([2, 4], waits)

    def test_store_busy_past_backoff_still_raises(self):
        def opener(request, timeout): raise self.busy()
        req = wow.urllib.request.Request("http://x")
        with self.assertRaises(wow.urllib.error.HTTPError):
            wow.open_json(req, 1, opener, lambda s: None)

class CensusCacheTest(unittest.TestCase):
    NOW = datetime.datetime(2026, 11, 3, 12, tzinfo=UTC)

    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        root = pathlib.Path(self.tmp.name)
        self.claude, self.codex = root / "claude", root / "codex"
        self.claude.mkdir(); self.codex.mkdir()
        self.cache = root / "cache" / "operator-census.json"

    def tearDown(self):
        self.tmp.cleanup()

    def write_turns(self, turns):
        by_file = {}
        for t in turns:
            header = (f"--- CURRENT TURN ---\nSurface: emacs-repl\nTo: {t['agent']}\n"
                      f"Origin: operator\n---\n\n{t['text']}")
            by_file.setdefault(t["vendor"], []).append(
                {"type": "user", "timestamp": wow.iso(t["at"]), "uuid": t["id"],
                 "message": {"content": [{"type": "text", "text": header}]}} if t["vendor"] == "claude" else
                {"type": "response_item", "timestamp": wow.iso(t["at"]),
                 "payload": {"type": "message", "role": "user", "id": t["id"],
                             "content": [{"type": "input_text", "text": header}]}})
        for vendor, rows in by_file.items():
            name = "a.jsonl" if vendor == "claude" else "rollout-a.jsonl"
            (self.claude if vendor == "claude" else self.codex).joinpath(name).write_text(
                "".join(json.dumps(r) + "\n" for r in rows))

    def census(self, fetch_rows, now=None):
        calls = []
        def fetcher(turns):
            calls.append(list(turns))
            return fetch_rows
        result = wow.read_operator_census(now or self.NOW, self.claude, self.codex,
                                          fetcher, cache_path=self.cache)
        return result, calls

    def test_second_run_with_no_new_turns_fetches_nothing_and_is_identical(self):
        t = turn(self.NOW - datetime.timedelta(minutes=30), "a")
        self.write_turns([t])
        first, calls1 = self.census([evidence(t["at"])])
        self.assertEqual(1, len(calls1))
        second, calls2 = self.census([evidence(t["at"])])
        self.assertEqual([], calls2)  # A: fetcher called zero times
        for key in ("matched", "pending", "violations", "operator-evidence"):
            self.assertEqual(first[key], second[key], key)

    def test_second_run_with_one_new_turn_fetches_exactly_that_turn(self):
        old = turn(self.NOW - datetime.timedelta(hours=2), "old")
        self.write_turns([old])
        _, calls1 = self.census([evidence(old["at"])])
        self.assertEqual([old], calls1[0])
        new = turn(self.NOW - datetime.timedelta(minutes=5), "new")
        self.write_turns([old, new])
        _, calls2 = self.census([evidence(old["at"]), evidence(new["at"], evidence_id="e-new")])
        self.assertEqual(1, len(calls2))
        self.assertEqual([new], calls2[0])  # B: exactly the new turn

    def test_turn_unmatched_then_matched_is_matched_on_second_run(self):
        t = turn(self.NOW - datetime.timedelta(minutes=30), "a")
        self.write_turns([t])
        first, _ = self.census([])          # no evidence yet
        self.assertEqual(0, first["matched"])
        second, calls2 = self.census([evidence(t["at"])])  # C: unmatched retried
        self.assertEqual(1, len(calls2))
        self.assertEqual(1, second["matched"])
        self.assertEqual([], second["violations"])

    def test_corrupt_cache_still_gives_correct_result_with_full_fetch(self):
        t = turn(self.NOW - datetime.timedelta(minutes=30), "a")
        self.write_turns([t])
        self.cache.parent.mkdir(parents=True, exist_ok=True)
        self.cache.write_text("{not json")
        with contextlib.redirect_stderr(io.StringIO()) as err:
            result, calls = self.census([evidence(t["at"])])
        self.assertEqual(1, len(calls))     # D: full fetch
        self.assertEqual(1, result["matched"])
        self.assertIn("unreadable", err.getvalue())

    def test_entries_outside_window_are_pruned(self):
        old = turn(self.NOW - datetime.timedelta(hours=20), "old")
        self.write_turns([old])
        self.census([evidence(old["at"])])
        fresh = turn(self.NOW - datetime.timedelta(minutes=5), "fresh")
        self.write_turns([fresh])
        later = self.NOW + datetime.timedelta(hours=5)
        self.census([evidence(fresh["at"])], now=later)  # old is now outside 24 h
        self.assertEqual({"claude/fresh"}, set(json.loads(self.cache.read_text())))  # E

    def test_new_turn_cannot_reuse_evidence_held_by_a_cached_turn(self):
        # Joe sends the same words twice; only the first reached the store.
        first = turn(self.NOW - datetime.timedelta(minutes=8), "first")
        self.write_turns([first])
        self.census([evidence(first["at"])])
        second = turn(first["at"] + datetime.timedelta(seconds=30), "second")
        self.write_turns([first, second])
        result, _ = self.census([evidence(first["at"])])  # same row, nothing new
        self.assertEqual(1, result["matched"])
        self.assertEqual(["second"], [v["vendor-id"] for v in result["violations"]])


if __name__ == "__main__":
    unittest.main()
