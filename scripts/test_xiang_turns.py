import os

import xiang_turns


class FakePost:
    def __init__(self, responses):
        self.responses = list(responses)
        self.calls = []

    def __call__(self, url, payload, timeout=None):
        self.calls.append((url, payload))
        return self.responses.pop(0) if self.responses else (500, {"ok": False})


def test_record_turn_posts_the_operator_turn_and_returns_the_id(monkeypatch):
    fake = FakePost([(201, {"ok": True, "id": "turn-abc"})])
    monkeypatch.setattr(xiang_turns, "POST", fake)
    rid = xiang_turns.record_turn("http://h:7070/", "hello 象", "claude-17", "sess", "matrix:job-1",
                                  operator_id="@joe:example.org", surface="matrix (!room:example.org)",
                                  evidence_id="$matrix-event")
    assert rid == "turn-abc"
    url, payload = fake.calls[0]
    assert url == "http://h:7070/api/alpha/xiang/turns"
    assert payload == {"text": "hello 象", "agent-id": "claude-17", "session-id": "sess",
                       "turn-id": "matrix:job-1", "origin": "operator", "dispatch": "later",
                       "operator-id": "@joe:example.org", "surface": "matrix (!room:example.org)",
                       "evidence-id": "$matrix-event"}


def test_a_refused_or_unreachable_record_is_none_and_logged(monkeypatch):
    logged = []
    fake = FakePost([(400, {"ok": False, "reason": "missing-field"}), (0, {"ok": False, "error": "URLError: x"})])
    monkeypatch.setattr(xiang_turns, "POST", fake)
    assert xiang_turns.record_turn("http://h", "t", "a", "s", "id", log=logged.append) is None
    assert xiang_turns.record_turn("http://h", "t", "a", "s", "id", log=logged.append) is None
    assert "http 400: missing-field" in logged[0]
    assert "http 0" in logged[1]


def test_after_reply_dispatches_the_turn_then_records_the_reply_as_an_agent_turn(monkeypatch):
    fake = FakePost([(200, {"ok": True, "dispatched": True, "job-id": "job-9"}),
                     (201, {"ok": True, "id": "turn-reply"})])
    monkeypatch.setattr(xiang_turns, "POST", fake)
    out = xiang_turns.after_reply("http://h", "turn-abc", "㊥ Done.\n\n🈸 Shall I go on?", "claude-17", "sess",
                                  "matrix:job-1", surface="matrix (!r)", reply_evidence_id="$reply")
    assert out["happened"]["dispatched"] is True
    assert out["reply_record"] == "turn-reply"
    assert fake.calls[0][0] == "http://h/api/alpha/xiang/turns/turn-abc/happened"
    assert fake.calls[0][1] == {"reply": "㊥ Done.\n\n🈸 Shall I go on?", "commits": []}
    reply_payload = fake.calls[1][1]
    assert reply_payload["origin"] == "agent"
    assert reply_payload["dispatch"] == "now"
    assert reply_payload["turn-id"] == "matrix:job-1:reply"
    assert reply_payload["evidence-id"] == "$reply"
    assert "operator-id" not in reply_payload


def test_after_reply_without_a_record_still_records_the_reply(monkeypatch):
    fake = FakePost([(201, {"ok": True, "id": "turn-reply"})])
    monkeypatch.setattr(xiang_turns, "POST", fake)
    out = xiang_turns.after_reply("http://h", None, "reply", "a", "s", "irc:job-2")
    assert out == {"happened": None, "reply_record": "turn-reply"}
    assert len(fake.calls) == 1


def test_kill_switch(monkeypatch):
    fake = FakePost([])
    monkeypatch.setattr(xiang_turns, "POST", fake)
    monkeypatch.setenv("FUTON3C_XIANG_BRIDGE", "0")
    assert xiang_turns.record_turn("http://h", "t", "a", "s", "id") is None
    assert xiang_turns.after_reply("http://h", "turn-x", "r", "a", "s", "id") == {"happened": None, "reply_record": None}
    assert fake.calls == []


def test_blank_text_is_not_recorded(monkeypatch):
    fake = FakePost([])
    monkeypatch.setattr(xiang_turns, "POST", fake)
    assert xiang_turns.record_turn("http://h", "   ", "a", "s", "id") is None
    assert fake.calls == []
