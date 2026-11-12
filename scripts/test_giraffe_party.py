#!/usr/bin/env python3
"""test_giraffe_party.py — invented-fixture tests for giraffe_party.py.

Every fixture below is INVENTED for the test (marked FIXTURE); no real
pending item, lexicon entity or Giraffe run is used.

Run: python3 scripts/test_giraffe_party.py
Exit 0 when all checks pass, 1 otherwise.
"""

import os
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

failed = [l for l, ok in RESULTS if not ok]
print("%d/%d PASS" % (len(RESULTS) - len(failed), len(RESULTS)))
sys.exit(1 if failed else 0)
