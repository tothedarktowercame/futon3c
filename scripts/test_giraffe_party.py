#!/usr/bin/env python3
"""test_giraffe_party.py — invented-fixture tests for giraffe_party.py.

Every fixture below is INVENTED for the test (marked FIXTURE); no real
pending item, lexicon entity or Giraffe run is used.

Run: python3 scripts/test_giraffe_party.py
Exit 0 when all checks pass, 1 otherwise.
"""

import os
import json
import tempfile
import unittest
from unittest.mock import patch
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import giraffe_party as gp

RESULTS = []


def check(label, ok):
    RESULTS.append((label, bool(ok)))
    print(label, "PASS" if ok else "FAIL")


# ---- FIXTURE: a four-entity lexicon -------------------------------------
# alpha (partly) links beta (partly); gamma (exists) links beta;
# delta (hypothesised) is alone. Two-hop: alpha -> {beta, gamma's view...}.
LEXICON = [  # FIXTURE
    {"entity": "alpha", "status": "partly", "relations": "uses beta"},
    {"entity": "beta", "status": "partly", "relations": "part-of gamma"},
    {"entity": "gamma", "status": "exists", "relations": ""},
    {"entity": "delta", "status": "hypothesised", "relations": ""},
]
SCORES = gp.convergence_scores(LEXICON)
SLUGS = {"all": {"alpha", "beta", "gamma", "delta"},
         "unfinished": {"alpha", "beta", "delta"}}

# ---- FIXTURE: a pending index -------------------------------------------
PENDING = {  # FIXTURE
    "M-alpha": {"id": "M-alpha", "pending": 2,
                "pending_label": "specified, multi-day work", "harm": "",
                "entities": ["beta"], "one_line": "the alpha mission"},
    "E-quick": {"id": "E-quick", "pending": 0,
                "pending_label": "ready (one small handoff)", "harm": "",
                "entities": ["gamma"], "one_line": "a quick excursion"},
    "T-harm": {"id": "T-harm", "pending": 1,
               "pending_label": "waiting on a short answer", "harm": "H2",
               "entities": [], "one_line": "a harm ticket"},
    "E-dead": {"id": "E-dead", "pending": 6,
               "pending_label": "may be no longer wanted", "harm": "",
               "entities": ["alpha"], "one_line": "superseded"},
    "M-nofeature": {"id": "M-nofeature", "pending": 3,
                    "pending_label": "specified, multi-day work", "harm": "",
                    "entities": [], "one_line": "no lexicon feature"},
}

# ---- FIXTURE: a Giraffe pass order ---------------------------------------
PASS_ORDER = [  # FIXTURE
    {"id": "M-alpha", "unblocks": 3, "cone": 5, "rank": 1},
    {"id": "T-harm", "unblocks": 1, "cone": 2, "rank": 2},
    {"id": "E-quick", "unblocks": 0, "cone": 0, "rank": 3},
]
PASS_RANK = {e["id"]: e["rank"] for e in PASS_ORDER}

# convergence: alpha sees beta within one hop and (via beta) nothing new
# within two except itself excluded; beta sees alpha and gamma within one.
check("convergence two-hop scores (FIXTURE lexicon)",
      SCORES == {"alpha": 1, "beta": 1, "gamma": 2, "delta": 0})

thor = gp.thor_order(PASS_ORDER, 10)
check("thor: Giraffe's own order, unblocks then cone",
      [t["item"] for t in thor] == ["M-alpha", "T-harm", "E-quick"]
      and "unblocks 3" in thor[0]["reason"] and "cone of 5" in thor[0]["reason"])
unranked = gp.thor_order([{"id": "M-unranked", "unblocks": 1, "cone": 1}], 10)
check("thor: a missing pass rank is said, never printed as rank 0",
      unranked[0]["reason"].endswith("no Giraffe pass rank")
      and "rank 0" not in unranked[0]["reason"]
      and thor[0]["reason"].endswith("Giraffe pass rank 1"))

merlin, m_abs = gp.merlin_order(PENDING, SCORES, SLUGS, 10)
# M-alpha: is alpha (1) + half of beta (1) = 1.5; E-dead dropped;
# M-nofeature is an absence, not rank 0.
check("merlin: convergence of the feature the item is + half the strongest "
      "unfinished feature it names",
      [m["item"] for m in merlin] == ["M-alpha"]
      and merlin[0]["score"] == 1.5)
check("merlin: 'may be no longer wanted' dropped",
      all(m["item"] != "E-dead" for m in merlin))
check("merlin: an item with no feature is a typed absence, not rank 0",
      any(a["what"] == "merlin score for M-nofeature" for a in m_abs)
      and all(m["score"] != 0 for m in merlin))

questor, q_abs = gp.questor_order(PENDING, SCORES, SLUGS, 10)
check("questor: only 'ready (one small handoff)', strongest feature first",
      [q["item"] for q in questor] == ["E-quick"]
      and "gamma" in questor[0]["reason"] and "2" in questor[0]["reason"])

thyra, _ = gp.thyra_order(PENDING, PASS_RANK, 10)
check("thyra: harm-line items only, most ready first",
      [t["item"] for t in thyra] == ["T-harm"]
      and "H2" in thyra[0]["reason"] and "pass rank 2" in thyra[0]["reason"])

# ---- party vote -----------------------------------------------------------
orderings = {"thor": [{"item": "A", "reason": "r"},
                      {"item": "B", "reason": "r"}],
             "merlin": [{"item": "A", "reason": "r"},  # dup of thor's
                        {"item": "C", "reason": "r"}],
             "questor": [{"item": "D", "reason": "r"}],
             "thyra": [{"item": "E", "reason": "r"}]}
party = gp.party_vote({k: list(v) for k, v in orderings.items()}, 12)
check("party: round robin skips items already chosen",
      [p["item"] for p in party] == ["A", "C", "D", "E", "B"]
      and [p["chosen_by"] for p in party] ==
      ["thor", "merlin", "questor", "thyra", "thor"])

# Plant: break the skip — the dedup check above must fail on the broken vote.
broken = gp.party_vote({k: list(v) for k, v in orderings.items()}, 12,
                       skip_seen=False)
check("plant: with the skip broken, 'A' is chosen twice",
      [p["item"] for p in broken].count("A") == 2
      and [p["item"] for p in broken] != ["A", "C", "D", "E", "B"])

# ---- FIXTURE: absent counts are not measured zeros ------------------------
COUNT_PASS = [
    {"id": "M-missing", "rank": 1},
    {"id": "T-null", "rank": 2, "unblocks": None, "cone": None},
    {"id": "E-zero", "rank": 3, "unblocks": 0, "cone": 0},
    {"id": "M-outside", "rank": 4},
]
count_rows = gp.thor_order(COUNT_PASS, 3)
check("thor: absent and null counts are explicitly missing, score stays null",
      all(r["reason"] == "unblocks 0 item(s) (count missing); "
          "cone of 0 behind it (count missing); Giraffe pass rank %d" % (j + 1)
          and r["score"] is None for j, r in enumerate(count_rows[:2])))
check("thor: measured zeros have no missing qualifier",
      count_rows[2]["reason"] ==
      "unblocks 0 item(s); cone of 0 behind it; Giraffe pass rank 3"
      and count_rows[2]["score"] == 0)


def fixture_party(entries, top):
    """Exercise main's actual party.json writer using only invented inputs."""
    with tempfile.TemporaryDirectory() as root:
        with patch.object(gp, "load_pass", return_value={"order": entries}), \
             patch.object(gp, "load_input", return_value={"items": [], "edges": []}), \
             patch.object(gp, "load_pending", return_value={}), \
             patch.object(gp, "load_lexicon", return_value=[]), \
             patch.object(gp.gcc_preview, "load_corpus", return_value=([], [], [])):
            gp.main(["--run", os.path.join(root, "fixture-run"),
                     "--pending", os.path.join(root, "pending.json"),
                     "--lexicon", os.path.join(root, "lexicon.json"),
                     "--corpus", os.path.join(root, "corpus"),
                     "--out", root, "--top", str(top)])
        with open(os.path.join(root, "party.json"), encoding="utf-8") as stream:
            return json.load(stream)


count_party = fixture_party(COUNT_PASS, 3)
expected_absences = [
    {"what": "%s for %s" % (field, iid),
     "reason": "Giraffe pass entry has no %s count (reading contract violation); shown as 0" % field}
    for iid in ("M-missing", "T-null") for field in ("unblocks", "cone")]
check("party.json: one typed violation per missing count, only Thor top entries",
      count_party["absences"] == expected_absences)
check("party.json: Thor displays the qualified missing counts",
      count_party["characters"][0]["top"][0]["reason"] ==
      "unblocks 0 item(s) (count missing); cone of 0 behind it (count missing); Giraffe pass rank 1")
check("party.json: one missing cone yields exactly one violation",
      fixture_party([{"id": "T-cone", "rank": 1, "unblocks": 2}], 1)["absences"] == [
          {"what": "cone for T-cone", "reason":
           "Giraffe pass entry has no cone count (reading contract violation); shown as 0"}])
check("party.json: complete pass and empty top have no count violations",
      fixture_party(PASS_ORDER, 3)["absences"] == []
      and fixture_party(COUNT_PASS, 0)["absences"] == [])

failed = [l for l, ok in RESULTS if not ok]
print("%d/%d PASS" % (len(RESULTS) - len(failed), len(RESULTS)))
class ExistingPartyChecks(unittest.TestCase):
    def test_existing_checks(self):
        self.assertEqual([], failed)


class PartyNeedsTests(unittest.TestCase):
    def test_pinned_direction_and_stable_linearisation(self):
        rows = [{"item": x, "rank": i + 1, "reason": "unchanged"}
                for i, x in enumerate(["A", "B", "C", "D"])]
        inputs = {"items": [{"id": "X", "state-group": "completed"}], "edges": [
            # Raw C unblocks A has ALREADY been reversed by readings/convert-edge.
            {"from": "A", "to": "C", "reading-kind": "unblocks"},
            {"from": "A", "to": "X"}, {"from": "A", "to": "X"},
            {"from": "D", "to": "Z", "kind": "external"},
            {"from": "B", "to": "A", "class": "alternative"}]}
        result = gp.party_dependencies(rows, inputs)
        self.assertEqual(["B", "C", "A", "D"], result["linearised"])
        self.assertEqual([{"item": "C", "shown_rank": 3, "state": None},
                          {"item": "X", "shown_rank": None, "state": "completed"}],
                         result["top"][0]["needs"])
        self.assertEqual([], result["top"][1]["needs"])
        self.assertEqual([], result["top"][2]["needs"])
        self.assertEqual([{"item": "X", "state": "completed"},
                          {"item": "Z", "state": None}], result["unshown_needs"])
        self.assertEqual(rows, [{k: v for k, v in row.items() if k != "needs"}
                                for row in result["top"]])

    def test_cycle_and_self_cycle(self):
        rows = [{"item": x, "rank": i + 1} for i, x in enumerate(["A", "B", "C"])]
        edges = [{"from": "A", "to": "B"}, {"from": "B", "to": "C"},
                 {"from": "C", "to": "B"}]
        result = gp.party_dependencies(rows, {"items": [], "edges": edges})
        self.assertIsNone(result["linearised"])
        self.assertEqual(["B", "C"], result["cycle"])
        result = gp.party_dependencies(rows[:1], {"items": [], "edges": [{"from": "A", "to": "A"}]})
        self.assertEqual(["A"], result["cycle"])

    def test_writer_and_empty_party(self):
        party = fixture_party(PASS_ORDER, 3)["party"]
        self.assertTrue(all(row["needs"] == [] for row in party["top"]))
        self.assertEqual([row["item"] for row in party["top"]], party["linearised"])
        self.assertEqual({"top": [], "linearised": [], "cycle": [], "unshown_needs": []},
                         gp.party_dependencies([], {"items": [], "edges": []}))


if __name__ == "__main__":
    unittest.main()
