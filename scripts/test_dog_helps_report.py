"""Tests for dog_helps_report.py (E-dog-helps packet A).

Fixtures are COPIES of real 象 operator turn records (ids cited in each test)
plus one clearly-marked SYNTHETIC withheld record, because no withheld records
exist yet (packet B not landed).

The echo rule is a Python REUSE of dog-provider/use-check's exact definition
(see dog_helps_report.py docstring); no JVM is loaded. Agreement is pinned on
3 real records: the two whose dog_use was written by the live Clojure
(turn-WHNTlI, turn-JrUTT2) and turn-vsC2tD, whose reply dog_provider.clj's
own docstring documents as sharing 3 content bigrams with
-home-joe-code-voxterm/kimi-unique-requisition.
"""
import json
import os
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import dog_helps_report as dhr

FIX = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                   "..", "test", "fixtures", "dog-helps")


def load(name):
    d = json.load(open(os.path.join(FIX, name)))
    d["_id"] = name[:-5]
    return d


def bodies(fix=FIX):
    """memory handle -> body text from the snapshot under fixtures/.../memories."""
    out = {}
    mdir = os.path.join(fix, "memories")
    for f in os.listdir(mdir):
        if f.endswith(".md"):
            handle = f[:-3].replace("__", "/", 1)
            out[handle] = open(os.path.join(mdir, f), encoding="utf-8",
                               errors="replace").read()
    return out


class EchoRuleParityTest(unittest.TestCase):
    """The Python reuse agrees with the Clojure use-check outcomes."""

    def test_real_record_whntli(self):
        # turn-WHNTlI (claude-5, 2026-11-11T21:52Z): dog_use recorded
        # kinesis-vdrive-editing as echoed, but under the THRESHOLD-1 rule —
        # dog_provider.clj's docstring documents raising echo-min-shared to 2
        # because of this very turn (one shared bigram, "mode line", in a
        # reply about a music player's mode line). The CURRENT rule — the one
        # this reuse copies — calls it unused, and the file is unchanged
        # (snapshot == live, verified). Pin: current rule = unused;
        # server-remote-access-setup = unused as recorded.
        r = load("turn-WHNTlI.json")
        b = bodies()
        self.assertEqual(
            dhr.used_kind("-home-joe/kinesis-vdrive-editing", r["reply_text"],
                          b["-home-joe/kinesis-vdrive-editing"]), "unused")
        self.assertEqual(len(dhr.content_bigrams(r["reply_text"])
                            & dhr.content_bigrams(
                                b["-home-joe/kinesis-vdrive-editing"])), 1)
        self.assertEqual(
            dhr.used_kind("-home-joe/server-remote-access-setup", r["reply_text"],
                          b["-home-joe/server-remote-access-setup"]), "unused")

    def test_real_record_jrutt2(self):
        # turn-JrUTT2 (claude-5, 2026-11-11T21:49Z): dog_use recorded
        # futon3c-slice-memory-high-throttle as unused.
        r = load("turn-JrUTT2.json")
        b = bodies()
        self.assertEqual(
            dhr.used_kind("-home-joe-code/futon3c-slice-memory-high-throttle",
                          r["reply_text"],
                          b["-home-joe-code/futon3c-slice-memory-high-throttle"]),
            "unused")

    def test_real_record_vsc2td_docstring_case(self):
        # turn-vsC2tD (claude-13, 2026-11-11T20:16Z): dog_provider.clj's
        # docstring says this reply shares 3 distinctive content bigrams with
        # kimi-unique-requisition (slug mentions removed), i.e. echoed at
        # threshold 2.
        r = load("turn-vsC2tD.json")
        b = bodies()
        handle = "-home-joe-code-voxterm/kimi-unique-requisition"
        reply = r["reply_text"].replace("kimi-unique-requisition", "")
        shared = dhr.content_bigrams(reply) & dhr.content_bigrams(b[handle])
        self.assertGreaterEqual(len(shared), 2)
        self.assertEqual(dhr.used_kind(handle, reply, b[handle]), "echoed")

    def test_named_uses_slug(self):
        self.assertEqual(dhr.used_kind("proj/some-slug",
                                       "as some-slug says", "body text here"),
                         "named")
        self.assertEqual(dhr.used_kind("proj/some-slug", "no mention", None),
                         "unused")


class ReportTest(unittest.TestCase):
    def setUp(self):
        self.records = [load(n) for n in
                        ("turn-vsC2tD.json", "turn-JrUTT2.json", "turn-WHNTlI.json")]
        # SYNTHETIC withheld record (no withheld records exist yet): a marked
        # copy of turn-JrUTT2 with arm "withheld" and a different id/date/
        # session (see the fixture file's "synthetic" note).
        self.synthetic = load("turn-SYNTH-withheld.json")
        self.reports = self.records + [self.synthetic]

    def test_pre_arm_not_mixed_with_arms(self):
        items, turns = dhr.collect(self.reports)
        self.assertIn("pre-arm", {r["arm"] for r in items})
        self.assertIn("withheld", {r["arm"] for r in items})
        self.assertNotIn("shown", {r["arm"] for r in items})

    def test_withheld_items_are_the_synthetic_only(self):
        items, _ = dhr.collect(self.reports)
        withheld = [r for r in items if r["arm"] == "withheld"]
        self.assertTrue(withheld)
        self.assertTrue(all(r["record"] == "turn-SYNTH-withheld" for r in withheld))

    def test_d1_numbers(self):
        items, _ = dhr.collect(self.reports)
        used = dhr.by_arm(items, "used")
        # real items with dog_use: WHNTlI 2 (1 used), JrUTT2 1 (0 used);
        # vsC2tD has items but its dog_use is a typed absence (no items).
        self.assertEqual(used["pre-arm"][1], 3)
        self.assertEqual(used["pre-arm"][0], 1)
        # the synthetic withheld record copied JrUTT2's dog_use (0 of 1 used)
        self.assertEqual(used["withheld"][1], 1)
        self.assertEqual(used["withheld"][0], 0)

    def test_d4_next_turn_correction_join(self):
        # vsC2tD's session has no later operator turn in the fixture set, so
        # corrected_next must be False (absent next turn), never True.
        _, turns = dhr.collect(self.reports)
        by_id = {t["id"]: t for t in turns}
        self.assertFalse(by_id["turn-vsC2tD"]["corrected_next"])

    def test_d5_needs_echo_between_use_and_correction(self):
        items, _ = dhr.collect(self.reports)
        self.assertTrue(all(not r["misleading"] for r in items))

    def test_render_mentions_typed_absence_and_pre_arm(self):
        items, turns = dhr.collect(self.reports)
        text = dhr.render(items, turns, self.reports, arm_live=None)
        self.assertIn("typed absence", text)
        self.assertIn("pre-arm", text)
        self.assertIn("Progress", text)


class HandCheckTest(unittest.TestCase):
    def test_sheet_lists_act_and_reply_paragraph(self):
        rec = load("turn-vsC2tD.json")
        rec["alignment"] = {"acts": [{"act": "t-f", "kind": "redirect",
                                      "text": "do it differently",
                                      "quote": "packet"}]}
        rec["reply_text"] = "first para\n\nthe packet line"
        out = os.path.join(FIX, "tmp-hand-check.md")
        try:
            n = dhr.hand_check([rec], 5, out)
            self.assertEqual(n, 1)
            text = open(out).read()
            self.assertIn("turn-vsC2tD", text)
            self.assertIn("do it differently", text)
            self.assertIn("the packet line", text)  # quote-matching paragraph
        finally:
            if os.path.exists(out):
                os.remove(out)


if __name__ == "__main__":
    unittest.main()
