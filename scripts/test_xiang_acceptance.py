"""Planted bad cases: each McCarthy check must catch the case it is named for."""
import os
import subprocess
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import xiang_acceptance as xa  # noqa: E402

HEAD = subprocess.run(["git", "-C", os.path.join(xa.CODE, "futon3c"), "rev-parse", "HEAD"],
                      capture_output=True, text=True).stdout.strip()


def row(at, event=None, type_="coordination", **kw):
    s = {"event": event} if event else {}
    s.update(kw)
    return {"at": at, "type": type_, "summary": s}


def good():
    return [
        {"turn": {"at": "T1"}, "happened": [
            row("T1a", "chat-turn", text="ok"),
            row("T1b", "promise/park-made", "promise/park-made", **{"park-id": "park-1"}),
            row("T1c", "turn-commits", commits=[{"repo": "futon3c", "sha": HEAD}],
                **{"start-heads": {"futon3c": HEAD}})]},
        {"turn": {"at": "T2"}, "happened": [
            row("T2a", "chat-turn", text="ok"),
            row("T2b", "promise/released", "promise/released", **{"park-id": "park-1"})]},
    ]


class Checks(unittest.TestCase):
    def test_good_session_passes(self):
        self.assertEqual({"PASS"}, {r["status"] for r in xa.run(good())})
        self.assertEqual({}, xa.open_promises(good()))

    def test_unanswered_turn(self):
        f = good(); f[0]["happened"].pop(0)
        self.assertTrue(xa.check_recording_is_acting(f))

    def test_row_outside_its_window(self):
        f = good(); f[0]["happened"].append(row("T3", "chat-turn"))
        self.assertTrue(xa.check_past_is_ordered(f))

    def test_bare_coordination_row(self):
        f = good(); f[1]["happened"].append(row("T2c"))
        self.assertTrue(xa.check_outputs_are_acts(f))

    def test_invented_commit(self):
        f = good(); f[0]["happened"][2]["summary"]["commits"][0]["sha"] = "0" * 40
        self.assertTrue(xa.check_records_are_true(f))

    def test_unreleased_park_is_listed_open(self):
        f = good(); f[1]["happened"].pop()
        self.assertEqual({"park-1": "T1"}, xa.open_promises(f))

    def test_park_without_an_id(self):
        f = good(); del f[0]["happened"][1]["summary"]["park-id"]
        self.assertTrue(xa.check_promises_accounted(f))

    def test_missing_pin_is_pending_not_fail(self):
        f = good(); del f[0]["happened"][2]["summary"]["start-heads"]
        r = {x["check"]: x for x in xa.run(f)}
        rw = r["R19 each turn can be rewound to its start"]
        self.assertEqual("PENDING", rw["status"])
        self.assertIn("start-heads", rw["re_arm"])


if __name__ == "__main__":
    unittest.main()
