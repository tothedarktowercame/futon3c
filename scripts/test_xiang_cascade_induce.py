import sys
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import xiang_cascade_induce as induce


def facts(reply=True, commit=False, made=False, released=False):
    return {"agent_reply_present": reply,
            "commits": ([{"repo": "r", "sha": "s", "resolved": True}] if commit else []),
            "commit_resolved": int(commit), "commit_unresolved": 0,
            "promise_park_rows": ([{"event": "promise/park-made"}] if made else [])
                                 + ([{"event": "promise/released"}] if released else []),
            "parks_made": int(made), "parks_released": int(released)}


def row(index, post=None, n=12):
    return {"family": "f", "turn_id": f"turn-{index}", "session_id": "s",
            "frame_evidence_id": f"family-{index}", "pre": facts(),
            "post": post or facts(reply=True)}


def wm_frame(eid, made=False, released=False):
    happened = []
    if made:
        happened.append({"summary": {"event": "promise/park-made"}})
    if released:
        happened.append({"summary": {"event": "promise/released"}})
    return {"turn": {"evidence_id": eid}, "happened": happened}


class InduceTest(unittest.TestCase):
    def test_ubiquitous_fact_is_not_better_than_baseline(self):
        rows = [row(i, facts(made=True)) for i in range(10)]
        frames = {"s": [wm_frame(f"family-{i}", made=True) for i in range(10)]
                        + [wm_frame(f"other-{i}", made=True) for i in range(5)]}
        rule = induce.induce_family("f", rows, frames)
        self.assertEqual("parks-made", rule["produces"])
        self.assertEqual({"hit": 10, "miss": 0, "guard-not-met": 0}, rule["held-out"])
        self.assertEqual({"count": 5, "denominator": 5, "fact": "parks-made"},
                         rule["baseline"])
        self.assertEqual("not-better", rule["verdict"])

    def test_held_out_only_fact_is_a_miss(self):
        rows = [row(0, facts(commit=True)), row(1), row(2)]
        measured = induce.candidate_loo(rows, "commit-resolved")
        self.assertEqual(0, measured["hit"])
        self.assertEqual(3, measured["miss"])

    def test_fewer_than_ten_is_too_few_even_at_one_hundred_percent(self):
        rows = [row(i, facts(released=True)) for i in range(7)]
        frames = {"s": [wm_frame(f"family-{i}", released=True) for i in range(7)]
                        + [wm_frame("other", released=False)]}
        rule = induce.induce_family("f", rows, frames)
        self.assertEqual({"hit": 7, "miss": 0, "guard-not-met": 0}, rule["held-out"])
        self.assertEqual("too-few-to-judge", rule["verdict"])


if __name__ == "__main__":
    unittest.main()
