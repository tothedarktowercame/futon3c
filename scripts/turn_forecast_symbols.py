#!/usr/bin/env python3
"""小象 send-time guards → mined predictions, scored on a held-out week.

Sibling of turn_forecast.py (reuses its loaders read-only by import). A guard
may use only what is observable BEFORE the agent replies:

  - the draft's fragments (turn-<id>.json.draft.json): guesses, precision,
    sure, basis ("declared" = Joe typed a mark);
  - Joe's <mark>: pointers in turn n's source text;
  - the previous reply's paragraph marks and brackets (parsed there);
  - the chain so far (earlier records of the session).

No 象 reading of turn n itself: that arrives only at reply end.

Guards: a declared mark; a top-1 guess with sure=true (per intent); the
previous reply's closing-paragraph mark; and conjunctions of at most two of
these. Kept only if they fire on >=15 training edges (n+1 <= split date).

Predictions: (B) uptake of the closing paragraph / of the last paragraph when
it carries an ask/next mark (🈸 🈯 🈰 ㊭), scored on clean labels only;
(A) intent:<I> for the 5 most frequent intents of n+1 in train.

A pair survives the held-out week (>=5 test fires) if its test hit rate beats
the matching baseline (last-paragraph ranker for uptake, base rate for
intents). Every pair, surviving or not, is reported.

Named candidates (counts only, single-day data, no held-out split yet):
  (i)  iching/* family patterns with 0/4 parts mapped;
  (ii) mapped parts pointing into the reply (who=reply) vs the operator turn.

Usage:
  python3 -I futon3c/scripts/turn_forecast_symbols.py \
      --store ~/.emacs-graph/session-turn-analysis \
      --split-date 2026-10-17 \
      --out futon3c/data/xiaoxiang-symbols/candidates-2026-10-24.json \
      --results-json /tmp/turn_forecast_symbols_results.json \
      --occurrences ~/notes/experiments/2026-10-24-chain-occurrences/occurrences.json
"""

from __future__ import annotations

import argparse
import itertools
import json
import os
import random
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import turn_forecast as tf  # noqa: E402  (read-only reuse of the loaders)

MIN_TRAIN_FIRES = 15
MIN_TEST_FIRES = 5
ASK_NEXT_MARKS = {"🈸", "🈯", "🈰", "㊭"}
TOP_N_INTENTS = 5


# ------------------------------------------------------------------ send time

def load_draft(store, rec_id):
    path = os.path.join(store, "turn-%s.json.draft.json" % rec_id.replace("turn-", ""))
    try:
        with open(path) as h:
            d = json.load(h)
        return d if isinstance(d, dict) else None
    except Exception:
        return None


def declared_marks(draft, rec):
    """Marks observable at send time: fragments with basis 'declared' (or a
    mark key), plus Joe's '<mark>:' pointers at line starts in the source."""
    out = set()
    for fr in (draft or {}).get("fragments") or []:
        if fr.get("basis") == "declared" or fr.get("mark"):
            m = fr.get("mark")
            if m in tf.MARK_KEY:
                out.add(m)
            elif fr.get("intent"):
                out.add(fr["intent"])
    for line in str((rec or {}).get("source_text") or "").split("\n"):
        m = re.match(r"^([㊥㊭㊣🈚㊟🈖🈯㊢㊩㊬🈹🈳🈲🈸㊯🈝㊮🈕🈰🈘]):", line.strip())
        if m:
            out.add(m.group(1))
    return out


def sure_top1(draft):
    """Intents that some fragment guesses top-1 with sure=true."""
    out = set()
    for fr in (draft or {}).get("fragments") or []:
        if fr.get("sure") and (fr.get("guesses") or []):
            out.add(fr["guesses"][0])
    return out


def guard_atoms(edge, draft):
    """The atomic guards firing on this edge, as (kind, value) strings."""
    atoms = set()
    for m in declared_marks(draft, edge["n_rec"]):
        atoms.add("declared:" + m)
    for i in sure_top1(draft):
        atoms.add("sure:" + i)
    if edge["paras"]:
        close = edge["paras"][-1]
        atoms.add("close:" + close["intent"])
    return atoms


def guard_fires(guard, atoms):
    g = guard.split(" & ")
    return all(a in atoms for a in g)


# --------------------------------------------------------------- predictions

def uptake_predictions(edge, label):
    """(prediction-id, hit|miss) pairs defined on this edge, or [].
    B1: n+1 takes up the closing (last) paragraph.
    B2: n+1 takes up the last paragraph, which carries an ask/next mark."""
    if label is None or not edge["paras"]:
        return []
    last = len(edge["paras"]) - 1
    hit = label["para"] == last
    out = [("uptake:closing", hit)]
    if edge["paras"][-1]["mark"] in ASK_NEXT_MARKS:
        out.append(("uptake:closing-askmark", hit))
    return out


def intent_predictions(train):
    """The TOP_N most frequent intents of n+1 over training edges."""
    counts = {}
    for e in train:
        for k in e["y"]:
            counts[k] = counts.get(k, 0) + 1
    return [k for k, _ in sorted(counts.items(), key=lambda kv: -kv[1])[:TOP_N_INTENTS]]


# ------------------------------------------------------------------ scoring

def score_pair(guard, prediction, edges, draft_cache):
    """(fires, labeled, hits, hit_rate) of `prediction hits` over edges where
    the guard fires (population per prediction kind)."""
    fires = labeled = hits = 0
    for e in edges:
        draft = draft_cache.get(e["n"])
        atoms = guard_atoms(e, draft)
        if not guard_fires(guard, atoms):
            continue
        fires += 1
        if prediction.startswith("uptake:"):
            for pid, hit in uptake_predictions(e, tf.clean_label(e)):
                if pid == prediction:
                    labeled += 1
                    hits += 1 if hit else 0
        else:  # intent:<I>
            labeled += 1
            hits += 1 if prediction[len("intent:"):] in e["y"] else 0
    return {"fires": fires, "labeled": labeled, "hits": hits,
            "hit_rate": (hits / labeled) if labeled else None}


def baselines(test, intents):
    """Last-paragraph ranker hit rate (on clean-labeled edges, and on the
    ask/next-marked subset) and the per-intent base rate over the test week."""
    b = {}
    lab = [(e, tf.clean_label(e)) for e in test]
    lab = [(e, l) for e, l in lab if l is not None and e["paras"]]
    if lab:
        last = len(lab[0][0]["paras"]) - 1  # per-edge below
        b["uptake:closing"] = sum(
            1 for e, l in lab if l["para"] == len(e["paras"]) - 1) / len(lab)
        sub = [(e, l) for e, l in lab if e["paras"][-1]["mark"] in ASK_NEXT_MARKS]
        b["uptake:closing-askmark"] = (
            sum(1 for e, l in sub if l["para"] == len(e["paras"]) - 1) / len(sub)
            if sub else None)
    for i in intents:
        b["intent:" + i] = sum(1 for e in test if i in e["y"]) / max(len(test), 1)
    return b


def negative_control(guard, prediction, test, draft_cache, perms=200, seed=7):
    """Shuffle the guard's firing across test edges, keeping its firing
    count; report the mean shuffled hit rate (the advantage should vanish)."""
    rng = random.Random(seed)
    idxs = list(range(len(test)))
    atoms = [guard_atoms(e, draft_cache.get(e["n"])) for e in test]
    real = [i for i, a in enumerate(atoms) if guard_fires(guard, a)]
    if not real:
        return None
    rates = []
    for _ in range(perms):
        chosen = set(rng.sample(idxs, len(real)))
        hits = labeled = 0
        for i in chosen:
            e = test[i]
            if prediction.startswith("uptake:"):
                for pid, hit in uptake_predictions(e, tf.clean_label(e)):
                    if pid == prediction:
                        labeled += 1
                        hits += 1 if hit else 0
            else:
                labeled += 1
                hits += 1 if prediction[len("intent:"):] in e["y"] else 0
        if labeled:
            rates.append(hits / labeled)
    return {"perms": len(rates),
            "mean_hit_rate": (sum(rates) / len(rates)) if rates else None}


# ------------------------------------------------------- the named candidates

def named_candidates(store, records, occurrences_path):
    """Counts only: all :pattern_verdicts data is from 2026-10-24, and the
    hand experiment is the same day, so there is no held-out split."""
    rows = []
    for rid, rec in records.items():
        v = rec.get("pattern_verdicts")
        if isinstance(v, dict):
            rows.append({"turn": rid, "at": str(rec.get("created_at")), "v": v})
    if occurrences_path and os.path.isfile(occurrences_path):
        with open(occurrences_path) as h:
            for row in json.load(h):
                rows.append({"turn": row.get("proposal_turn"),
                             "at": "2026-10-24 (hand experiment)",
                             "v": {"patterns": [row]}})
    iching = [(r["turn"], p) for r in rows for p in r["v"].get("patterns", [])
              if str(p.get("pattern", "")).startswith("iching/")]
    iching_zero = [(t, p.get("pattern")) for t, p in iching
                   if not p.get("chain_mapped")]
    who = {"reply": 0, "operator": 0}
    for r in rows:
        for p in r["v"].get("patterns", []):
            for win in ("turn", "chain"):
                for part in (p.get(win) or {}).values():
                    if isinstance(part, dict) and part.get("element"):
                        who[part.get("who")] = who.get(part.get("who"), 0) + 1
    return {
        "rows": len(rows),
        "iching_proposals": len(iching),
        "iching_zero_of_4": len(iching_zero),
        "iching_zero_examples": [p for _, p in iching_zero[:10]],
        "mapped_parts_who": who,
        "held_out_split": "none: all data is 2026-10-24 (one day)",
    }


# ---------------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--store", default=os.path.expanduser(
        "~/.emacs-graph/session-turn-analysis"))
    ap.add_argument("--split-date", default="2026-10-17")
    ap.add_argument("--out", required=True, help="symbol records JSON")
    ap.add_argument("--results-json", required=True)
    ap.add_argument("--occurrences", default=os.path.expanduser(
        "~/notes/experiments/2026-10-24-chain-occurrences/occurrences.json"))
    args = ap.parse_args()

    records = tf.load_records(args.store)
    edges = tf.build_edges(records)
    train, test = tf.split_edges(edges, args.split_date)
    intents = intent_predictions(train)

    draft_cache = {}
    def draft_of(rid):
        if rid not in draft_cache:
            draft_cache[rid] = load_draft(args.store, rid)
        return draft_cache[rid]

    with_draft = [e for e in edges if draft_of(e["n"])]
    tr = [e for e in train if draft_of(e["n"])]
    te = [e for e in test if draft_of(e["n"])]

    # 1. candidate guards on train, keep >= MIN_TRAIN_FIRES
    atomics = {}
    for e in tr:
        for a in guard_atoms(e, draft_of(e["n"])):
            atomics[a] = atomics.get(a, 0) + 1
    kept = sorted(a for a, n in atomics.items() if n >= MIN_TRAIN_FIRES)
    guards = list(kept)
    for a, b in itertools.combinations(kept, 2):
        if a.split(":", 1)[0] != "close" or b.split(":", 1)[0] != "close":
            g = a + " & " + b
            fires = sum(1 for e in tr
                        if guard_fires(g, guard_atoms(e, draft_of(e["n"]))))
            if fires >= MIN_TRAIN_FIRES:
                guards.append(g)

    # 2. predictions and train hit rates
    preds = ["uptake:closing", "uptake:closing-askmark"] + \
        ["intent:" + i for i in intents]
    pairs = []
    for g in guards:
        for p in preds:
            s = score_pair(g, p, tr, draft_cache)
            if s["labeled"]:
                pairs.append({"guard": g, "prediction": p, "train": s})

    # 3. held-out week, no re-fitting
    base = baselines(te, intents)
    for pair in pairs:
        t = score_pair(pair["guard"], pair["prediction"], te, draft_cache)
        pair["test"] = t
        b = base.get(pair["prediction"])
        pair["baseline_test"] = b
        pair["survives"] = bool(
            t["fires"] >= MIN_TEST_FIRES and t["labeled"] >= MIN_TEST_FIRES
            and t["hit_rate"] is not None and b is not None
            and t["hit_rate"] > b)

    # 4. negative control on survivors
    for pair in pairs:
        if pair["survives"]:
            pair["shuffle_control"] = negative_control(
                pair["guard"], pair["prediction"], te, draft_cache)

    # 5. symbol records
    os.makedirs(os.path.dirname(args.out), exist_ok=True)
    symbols = []
    for pair in pairs:
        if not pair["survives"]:
            continue
        ev = [e["target"] for e in te
              if guard_fires(pair["guard"], guard_atoms(e, draft_of(e["n"])))][:20]
        symbols.append({
            "symbol": "sym-" + pair["guard"].replace(" & ", "+").replace(":", "-")
                      + "--" + pair["prediction"].replace(":", "-"),
            "gloss": "when %s, predict %s" % (pair["guard"], pair["prediction"]),
            "guard": pair["guard"], "prediction": pair["prediction"],
            "train": {"fired": pair["train"]["labeled"], "hit": pair["train"]["hits"]},
            "test": {"fired": pair["test"]["labeled"], "hit": pair["test"]["hits"]},
            "baseline_test": pair["baseline_test"],
            "evidence": ev, "status": "candidate"})
    with open(args.out, "w") as h:
        json.dump(symbols, h, indent=1, ensure_ascii=False)

    named = named_candidates(args.store, records, args.occurrences)
    with open(args.results_json, "w") as h:
        json.dump({
            "store": args.store, "split_date": args.split_date,
            "edges_with_draft": len(with_draft),
            "train_edges_with_draft": len(tr),
            "test_edges_with_draft": len(te),
            "guards_considered": len(atomics), "guards_kept": guards,
            "intent_predictions": intents, "baselines_test": base,
            "pairs": pairs, "survivors": len(symbols),
            "named_candidates": named,
        }, h, indent=1, default=str)
    print("edges with draft:", len(with_draft),
          "(train", len(tr), "test", len(te), ")")
    print("guards kept:", len(guards), "pairs scored:", len(pairs),
          "survivors:", len(symbols))
    print("named:", json.dumps(named)[:400])


if __name__ == "__main__":
    main()
