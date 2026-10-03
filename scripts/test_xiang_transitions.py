import json
import os
import tempfile

import xiang_transitions as xt


def write_turn(d, name, session, at, intents, turn_id=None, reply_to=None, origin="operator"):
    rec = {"session_id": session, "created_at": at, "turn_id": turn_id or name, "origin": origin}
    if reply_to:
        rec["in_reply_to"] = reply_to
    json.dump(rec, open(os.path.join(d, f"{name}.json"), "w"))
    ana = {"status": "analyzed", "sentences": [{"id": "s1", "fragments": [{"intent": i} for i in intents]}]}
    json.dump(ana, open(os.path.join(d, f"{name}.json.analysis.json"), "w"))


def test_counts_consecutive_within_a_session_and_explicit_by_reply():
    with tempfile.TemporaryDirectory() as d:
        write_turn(d, "turn-a1", "s1", "2026-10-03T09:00:00Z", ["propose"], "t1")
        write_turn(d, "turn-a2", "s1", "2026-10-03T09:05:00Z", ["approve", "ask-action"], "t2", reply_to="t1")
        write_turn(d, "turn-a3", "s1", "2026-10-03T09:10:00Z", ["accept"], "t3", reply_to="t2")
        write_turn(d, "turn-b1", "s2", "2026-10-03T09:00:00Z", ["delegate"], "u1")
        write_turn(d, "turn-b2", "s2", "2026-10-03T09:01:00Z", ["promise"], "u2")
        json.dump({"nope": 1}, open(os.path.join(d, "turn-zz.json"), "w"))  # no analysis: skipped
        turns = xt.load_turns(d)
        assert len(turns) == 5
        consecutive, explicit = xt.count(turns)
    assert consecutive[("propose", "approve")] == 1
    assert consecutive[("propose", "ask-action")] == 1
    assert consecutive[("approve", "accept")] == 1 and consecutive[("ask-action", "accept")] == 1
    assert consecutive[("delegate", "promise")] == 1
    assert consecutive[("propose", "delegate")] == 0, "sessions do not leak into each other"
    assert explicit[("propose", "approve")] == 1
    assert explicit[("ask-action", "accept")] == 1
    assert explicit[("delegate", "promise")] == 0, "no reply link, no explicit pair"


def test_posterior_keeps_the_prior_and_rows_sum_to_one():
    post = xt.posterior({("propose", "approve"): 7, ("propose", "frown"): 1})
    row = post["propose"]
    assert abs(sum(row.values()) - 1) < 1e-9
    assert row["approve"] > row["disagree"] > 0, "data lifts approve; disagree keeps its prior"
    assert row["frown"] > 0, "a pair the table lacks appears with its count"
    assert list(row)[0] == "approve", "rows are sorted by probability"
    assert set(xt.posterior({})) == set(xt.ADJACENCY)


def test_report_names_surprises_and_missing_rows(capsys):
    consecutive, explicit = xt.count([
        {"session": "s", "at": "1", "turn_id": "a", "intents": ["propose"], "reply_to": None},
        {"session": "s", "at": "2", "turn_id": "b", "intents": ["collect"], "reply_to": "a"},
    ])
    text = xt.report(consecutive, explicit, xt.posterior(explicit))
    assert "Explicit answers the table lacks" in text
    assert "propose -> collect" in text
    assert "Rows the table has that no reading shows" in text


def test_main_writes_the_matrix(tmp_path):
    d = tmp_path / "store"
    d.mkdir()
    write_turn(str(d), "turn-a1", "s1", "2026-10-03T09:00:00Z", ["delegate"], "t1")
    write_turn(str(d), "turn-a2", "s1", "2026-10-03T09:05:00Z", ["promise"], "t2", reply_to="t1")
    out = tmp_path / "m.json"
    assert xt.main(["--dir", str(d), "--json", str(out)]) == 0
    m = json.load(open(out))
    assert m["delegate"]["promise"] > m["delegate"]["retract"]
    assert xt.main(["--dir", str(tmp_path / "empty")]) == 1
