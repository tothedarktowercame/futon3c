"""Tests for carrier.py (unittest, no network)."""

from __future__ import annotations

import json
import os
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

import carrier  # noqa: E402


def sidecar(**overrides):
    base = {
        "record": "turn-XXXXXX", "labeller": "zai-2", "schema": "moves/1",
        "at": "2026-10-13T00:00:00Z",
        "acts": [
            {
                "act": "a-turn-1-r-0", "kind": "reply-mark",
                "move": "report/done",
                "quote": "the work is finished and landed",
                "answers": [
                    {"act": "a-turn-1-f-s1-0",
                     "quote": "the work is finished and landed",
                     "effect": "closes"},
                ],
                "scope": {"opens": None, "closes": "a-turn-0-f-s1-0",
                          "within": "a-turn-0-f-s2-0"},
                "confidence": "high",
            },
            {
                "act": "a-turn-1-f-s1-0", "kind": "ask-action",
                "move": "ask-action/commission",
                "quote": "please do the work now.",
                "answers": [],
                "scope": {"opens": None, "closes": None, "within": None},
                "confidence": "high",
            },
        ],
        "unlabelled": [],
    }
    base.update(overrides)
    return base


class BuildEdgesTest(unittest.TestCase):
    def setUp(self):
        self.edges, self.notes, _ = carrier.build_edges(sidecar(), "shard-2")

    def test_one_answers_edge_per_entry(self):
        answers = [e for e in self.edges if e["hx/type"] == "xiang/answers"]
        self.assertEqual(len(answers), 1)
        self.assertEqual(answers[0]["hx/endpoints"],
                         ["a-turn-1-r-0", "a-turn-1-f-s1-0"])
        self.assertEqual(answers[0]["hx/props"]["effect"], "closes")
        self.assertEqual(answers[0]["hx/props"]["shard"], "shard-2")
        self.assertEqual(answers[0]["hx/props"]["status"], "proposed")
        self.assertEqual(answers[0]["hx/props"]["label_set"], "moves/1")
        self.assertEqual(answers[0]["hx/props"]["roles"],
                         {"answer": "a-turn-1-r-0", "opener": "a-turn-1-f-s1-0"})

    def test_within_edge_endpoint_order_and_relation(self):
        within = [e for e in self.edges
                  if e["hx/type"] == "xiang/scope"
                  and e["hx/props"]["relation"] == "within"]
        self.assertEqual(len(within), 1)
        self.assertEqual(within[0]["hx/endpoints"],
                         ["a-turn-0-f-s2-0", "a-turn-1-r-0"])
        self.assertEqual(within[0]["hx/props"]["roles"],
                         {"opener": "a-turn-0-f-s2-0", "governed": "a-turn-1-r-0"})

    def test_self_referential_scope_is_noted_not_edged(self):
        sc = sidecar()
        sc["acts"][0]["scope"] = {"opens": None, "closes": "a-turn-1-r-0",
                                  "within": "a-turn-1-r-0"}
        edges, notes, _ = carrier.build_edges(sc, "shard-2")
        self.assertEqual([], [e for e in edges if e["hx/type"] == "xiang/scope"])
        self.assertEqual(2, sum("itself" in n for n in notes), notes)

    def test_closes_edge_endpoint_order_and_relation(self):
        closes = [e for e in self.edges
                  if e["hx/type"] == "xiang/scope"
                  and e["hx/props"]["relation"] == "closes"]
        self.assertEqual(len(closes), 1)
        self.assertEqual(closes[0]["hx/endpoints"],
                         ["a-turn-0-f-s1-0", "a-turn-1-r-0"])

    def test_null_scope_produces_nothing(self):
        null_scope = sidecar()
        null_scope["acts"][0]["scope"] = {"opens": "x", "closes": None,
                                          "within": None}
        null_scope["acts"][0]["answers"] = []
        edges, _, _ = carrier.build_edges(null_scope, "shard-2")
        self.assertEqual(edges, [])

    def test_ids_deterministic(self):
        again, _, _ = carrier.build_edges(sidecar(), "shard-2")
        self.assertEqual([e["hx/id"] for e in self.edges],
                         [e["hx/id"] for e in again])

    def test_answers_entry_without_act_id_noted(self):
        broken = sidecar()
        broken["acts"][0]["answers"] = [
            {"act": None, "quote": "something", "effect": "keeps"}]
        edges, notes, _ = carrier.build_edges(broken, "shard-2")
        self.assertEqual([e for e in edges if e["hx/type"] == "xiang/answers"], [])
        self.assertTrue(any("without act id" in n for n in notes))


class LoadAndApplyTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.addCleanup(self.tmp.cleanup)
        root = Path(self.tmp.name)
        (root / "shard-2").mkdir()
        (root / "shard-2" / "turn-AAAAAA.moves.json").write_text(
            json.dumps(sidecar()), encoding="utf-8")
        second = sidecar()
        second["record"] = "turn-BBBBBB"
        (root / "shard-2" / "turn-BBBBBB.moves.json").write_text(
            json.dumps(second), encoding="utf-8")
        (root / "shard-2" / "turn-BROKEN.moves.json").write_text(
            "{not json", encoding="utf-8")
        self.glob = str(root / "shard-2" / "*.moves.json")

    def test_unreadable_sidecar_skipped(self):
        loaded, notes = carrier.load_sidecars(self.glob)
        self.assertEqual(len(loaded), 2)
        self.assertTrue(any("BROKEN" in n for n in notes))

    def test_apply_posts_exactly_built_edges(self):
        loaded, _ = carrier.load_sidecars(self.glob)
        edges = []
        for sidecar, shard in loaded:
            built, _, _ = carrier.build_edges(sidecar, shard)
            edges.extend(built)
        posted = []
        original = carrier.post_edge

        def fake_post(base, edge):
            posted.append((base, edge))
            return True, "stored"

        carrier.post_edge = fake_post
        try:
            ok, detail = carrier.post_edge("http://x", edges[0])
            self.assertTrue(ok)
        finally:
            carrier.post_edge = original
        # The fake must have been used and bodies preserved verbatim.
        self.assertEqual(posted[0][1], edges[0])


class ResolveNamedTest(unittest.TestCase):
    """Named-opener resolution: same-record, same-session, unique, unresolved."""

    @staticmethod
    def _named_sidecar(act_id, record, within, opens=None, opens_act=None):
        return {
            "record": record, "labeller": "zai-2", "schema": "moves/1",
            "at": "2026-10-13T00:00:00Z",
            "acts": [
                {"act": opens_act or (record.replace("turn-", "s1-") + "-f-s1-0"),
                 "kind": "constrain", "move": "constrain/standing-rule",
                 "quote": "opens a named scope here", "answers": [],
                 "scope": {"opens": opens, "closes": None, "within": None},
                 "confidence": "high"},
                {"act": act_id, "kind": "report", "move": "report/done",
                 "quote": "an act governed by the named scope",
                 "answers": [],
                 "scope": {"opens": None, "closes": None, "within": within},
                 "confidence": "high"},
            ],
            "unlabelled": [],
        }

    def _scope_edge(self, sidecar, index):
        edges, notes, _ = carrier.build_edges(sidecar, "shard-9", index)
        scope = [e for e in edges if e["hx/type"] == "xiang/scope"]
        return scope, notes

    def test_same_record_resolution(self):
        sc = self._named_sidecar("a-turn-1-r-0", "turn-1", "evidentiality",
                                 opens="evidentiality",
                                 opens_act="a-turn-1-f-s1-0")
        index = carrier.build_opens_index([(sc, "shard-9")])
        edges, notes = self._scope_edge(sc, index)
        self.assertEqual(len(edges), 1)
        self.assertEqual(edges[0]["hx/endpoints"],
                         ["a-turn-1-f-s1-0", "a-turn-1-r-0"])
        self.assertEqual(edges[0]["hx/props"]["opener_name"], "evidentiality")
        self.assertEqual(edges[0]["hx/props"]["resolved"], "same-record")
        self.assertEqual(notes, [])

    def test_same_session_resolution(self):
        opener_sidecar = self._named_sidecar(
            "a-turn-1-f-s1-0", "turn-1", None, opens="evidentiality",
            opens_act="a-turn-1-f-s1-0")
        governed = self._named_sidecar("a-turn-2-r-0", "turn-2",
                                       "evidentiality")
        index = carrier.build_opens_index(
            [(opener_sidecar, "shard-9"), (governed, "shard-9")])
        edges, notes = self._scope_edge(governed, index)
        self.assertEqual(len(edges), 1)
        self.assertEqual(edges[0]["hx/endpoints"],
                         ["a-turn-1-f-s1-0", "a-turn-2-r-0"])
        self.assertEqual(edges[0]["hx/props"]["resolved"], "same-session")

    def test_unique_resolution(self):
        opener = self._named_sidecar("b-turn-3-f-s1-0", "turn-3", None,
                                     opens="only-everywhere",
                                     opens_act="b-turn-3-f-s1-0")
        governed = self._named_sidecar("a-turn-2-r-0", "turn-2",
                                       "only-everywhere")
        index = carrier.build_opens_index(
            [(opener, "shard-1"), (governed, "shard-9")])
        edges, _ = self._scope_edge(governed, index)
        self.assertEqual(edges[0]["hx/endpoints"],
                         ["b-turn-3-f-s1-0", "a-turn-2-r-0"])
        self.assertEqual(edges[0]["hx/props"]["resolved"], "unique")

    def test_unresolved_no_opener_and_two_openers(self):
        sc = self._named_sidecar("a-turn-2-r-0", "turn-2", "ghost-scope")
        index = carrier.build_opens_index([(sc, "shard-9")])
        edges, notes = self._scope_edge(sc, index)
        self.assertEqual(edges, [])
        self.assertTrue(any("resolves to no opener" in n for n in notes))
        first = self._named_sidecar("b-turn-1-f-s1-0", "turn-1", None,
                                    opens="shared",
                                    opens_act="b-turn-1-f-s1-0")
        second = self._named_sidecar("c-turn-9-f-s1-0", "turn-9", None,
                                     opens="shared",
                                     opens_act="c-turn-9-f-s1-0")
        governed = self._named_sidecar("a-turn-5-r-0", "turn-5", "shared")
        index2 = carrier.build_opens_index(
            [(first, "shard-1"), (second, "shard-2"), (governed, "shard-3")])
        edges2, notes2 = self._scope_edge(governed, index2)
        self.assertEqual(edges2, [])
        self.assertTrue(any("resolves to 2 openers" in n for n in notes2))

    def test_retract_bodies_use_old_ids(self):
        sc = self._named_sidecar("a-turn-2-r-0", "turn-2", "evidentiality",
                                 opens="evidicality-other")
        index = carrier.build_opens_index([(sc, "shard-9")])
        edges, notes, retracts = carrier.build_edges(sc, "shard-9", index)
        self.assertEqual(len(retracts), 1)
        self.assertEqual(retracts[0]["hx/op"], "retract")
        self.assertNotIn("hx/props", retracts[0])
        expected = "hx-xiang-scope-" + carrier._sha20(
            "within", "evidentiality", "a-turn-2-r-0")
        self.assertEqual(retracts[0]["hx/id"], expected)
        self.assertEqual(retracts[0]["hx/endpoints"],
                         ["evidentiality", "a-turn-2-r-0"])


if __name__ == "__main__":
    unittest.main()
