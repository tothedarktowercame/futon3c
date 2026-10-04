import json

import xlate


DOCS = {
    "social/defer-decision": {"title": "Defer a decision until it is cheap", "toks": xlate.tokens("defer decision later sort it out cheap")},
    "social/ask-the-seat": {"title": "Ask the seat that holds it", "toks": xlate.tokens("ask seat delegate question agent")},
    "象/言即行": {"title": "言即行", "toks": xlate.tokens("言即行 言 即 行 speech act envelope")},
}


def test_find_many_returns_hits_per_query_with_excerpts(monkeypatch):
    monkeypatch.setattr(xlate, "excerpt", lambda pid: {"context": f"ctx of {pid}", "conclusion": "do it"})
    out = xlate.find_many(["defer a decision, sort it out later", "ask 象-2 next", "zzz nothing"], n=2, docs=DOCS)
    assert list(out) == ["defer a decision, sort it out later", "ask 象-2 next", "zzz nothing"]
    first = out["defer a decision, sort it out later"]
    assert first[0]["id"] == "social/defer-decision"
    assert first[0]["title"].startswith("Defer a decision")
    assert first[0]["context"] == "ctx of social/defer-decision"
    assert first[0]["conclusion"] == "do it"
    assert isinstance(first[0]["score"], float)
    assert out["ask 象-2 next"][0]["id"] == "social/ask-the-seat"
    assert out["zzz nothing"] == []
    assert all(len(hits) <= 2 for hits in out.values())


def test_cjk_queries_find_cjk_patterns(monkeypatch):
    monkeypatch.setattr(xlate, "excerpt", lambda pid: {})
    out = xlate.find_many(["言即行"], n=1, docs=DOCS)
    assert out["言即行"][0]["id"] == "象/言即行"
    assert "context" not in out["言即行"][0]


def test_find_many_command_reads_stdin_and_writes_json(monkeypatch, capsys):
    monkeypatch.setattr(xlate, "load_index", lambda cands=False: DOCS)
    monkeypatch.setattr(xlate, "excerpt", lambda pid: {})
    import io, sys
    monkeypatch.setattr(sys, "stdin", io.StringIO(json.dumps(["ask the seat"])))
    xlate.cmd_find_many(["-n", "1"])
    out = json.loads(capsys.readouterr().out)
    assert out == {"ask the seat": [{"id": "social/ask-the-seat", "score": out["ask the seat"][0]["score"], "title": "Ask the seat that holds it"}]}


def test_opts_parsing():
    assert xlate._opts(["-n", "3", "--json", "--with-candidates", "x", "y"]) == (3, True, True, ["x", "y"])
    assert xlate._opts(["plain"]) == (8, False, False, ["plain"])
