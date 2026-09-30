#!/usr/bin/env python3
"""Induce C2 or guard-matched C5 rules for M-象-cascade."""

from __future__ import annotations

import argparse
import json
import math
import sys
from collections import Counter
from pathlib import Path

import xiang_cascade_corpus as c1


HERE = Path(__file__).resolve().parent
C2_CORPUS = Path("/home/joe/code/futon3c/holes/labs/M-象-cascade/corpus-c1.jsonl")
C2_CACHE = Path("/tmp/xiang-cascade-c1-frames-live")
C5_CORPUS = Path("/home/joe/code/futon3c/holes/labs/M-象-cascade/corpus-c4.jsonl")
C5_CACHE = Path("/tmp/xiang-cascade-c4-frames-live")
C2_OUTPUTS = {
    "go-ahead": Path("/home/joe/code/futon3c/holes/labs/M-象-cascade/rule-go-ahead-c2.edn"),
    "observed-running-correction": Path("/home/joe/code/futon3c/holes/labs/M-象-cascade/rule-observed-running-correction-c2.edn"),
}
C5_OUTPUTS = {
    "go-ahead": Path("/home/joe/code/futon3c/holes/labs/M-象-cascade/rule-go-ahead-c5.edn"),
    "live-gap": Path("/home/joe/code/futon3c/holes/labs/M-象-cascade/rule-live-gap-c5.edn"),
}
POST_FACTS = ("commit-resolved", "parks-made", "parks-released")
PRE_FACTS = ("agent-reply-present", "commit-resolved", "parks-made", "parks-released")
MIN_JUDGEMENT = 10
ALPHA = 0.05 / len(POST_FACTS)


def read_jsonl(path):
    return [json.loads(line) for line in path.read_text(encoding="utf-8").splitlines() if line.strip()]


def pre_holds(row, fact):
    pre = row["pre"]
    return {"agent-reply-present": bool(pre["agent_reply_present"]),
            "commit-resolved": pre["commit_resolved"] >= 1,
            "parks-made": pre["parks_made"] >= 1,
            "parks-released": pre["parks_released"] >= 1}[fact]


def post_holds(row, fact):
    post = row["post"]
    return {"commit-resolved": post["commit_resolved"] >= 1,
            "parks-made": post["parks_made"] >= 1,
            "parks-released": post["parks_released"] >= 1}[fact]


def frame_post_holds(frame, fact):
    facts = c1.frame_facts(frame)
    return {"commit-resolved": facts["commit_resolved"] >= 1,
            "parks-made": facts["parks_made"] >= 1,
            "parks-released": facts["parks_released"] >= 1}[fact]


def induce_guard(training):
    """Pre facts present in at least 2/3 of TRAINING (integer exact)."""
    n = len(training)
    if not n:
        return []
    return [fact for fact in PRE_FACTS
            if 3 * sum(pre_holds(row, fact) for row in training) >= 2 * n]


def guard_holds(row, guard):
    return all(pre_holds(row, fact) for fact in guard)


def candidate_loo(rows, fact):
    """Held-out results for FACT.

    FACT is predicted in a fold only after it occurred in that fold's
    training rows. This is the check that prevents a held-out-only fact from
    teaching its own prediction.
    """
    counts = Counter(hit=0, miss=0, guard_not_met=0)
    for index, held in enumerate(rows):
        training = rows[:index] + rows[index + 1:]
        guard = induce_guard(training)
        if not guard_holds(held, guard):
            counts["guard_not_met"] += 1
        elif not any(post_holds(row, fact) for row in training):
            counts["miss"] += 1
        elif post_holds(held, fact):
            counts["hit"] += 1
        else:
            counts["miss"] += 1
    return dict(counts)


def rate(counts):
    denominator = counts["hit"] + counts["miss"]
    return counts["hit"] / denominator if denominator else 0.0


def select_produces(rows):
    scored = [(rate(candidate_loo(rows, fact)),
               candidate_loo(rows, fact)["hit"],
               -POST_FACTS.index(fact), fact, candidate_loo(rows, fact))
              for fact in POST_FACTS]
    return max(scored)[3], max(scored)[4]


def load_cached_frames(sessions, cache_dir, legacy=False):
    out = {}
    for session in sorted(sessions):
        legacy_texts = (c1.unique_operator_texts(c1.DEFAULT_RECORDS, session)
                        if legacy else None)
        result = c1.load_session_frames(session, cache_dir, legacy_texts=legacy_texts)
        out[session] = result.get("frames", [])
    return out


def baseline_counts(family_rows, frames_by_session, fact):
    sessions = {row["session_id"] for row in family_rows}
    excluded = {row.get("frame_evidence_id") for row in family_rows
                if row.get("frame_evidence_id")}
    population = []
    for session in sorted(sessions):
        for frame in frames_by_session.get(session, []):
            if frame.get("turn", {}).get("evidence_id") not in excluded:
                population.append(frame)
    count = sum(frame_post_holds(frame, fact) for frame in population)
    return {"count": count, "denominator": len(population)}


def verdict(full_count, loo, baseline):
    if full_count < MIN_JUDGEMENT:
        return "too-few-to-judge"
    family_den = loo["hit"] + loo["miss"]
    base_den = baseline["denominator"]
    family_rate = loo["hit"] / family_den if family_den else 0
    base_rate = baseline["count"] / base_den if base_den else 0
    return "better-than-baseline" if family_rate > base_rate else "not-better"


def induce_family(family, rows, frames_by_session):
    full = [row for row in rows if "missing" not in row]
    produces, loo = select_produces(full)
    baselines = {fact: baseline_counts(rows, frames_by_session, fact)
                 for fact in POST_FACTS}
    selected_baseline = baselines[produces]
    return {
        "schema": "xiang-cascade/induced-rule-v1",
        "family": family,
        "turns": [row["turn_id"] for row in rows],
        "full-triples": len(full),
        "guard": induce_guard(full),
        "produces": produces,
        "held-out": {"hit": loo["hit"], "miss": loo["miss"],
                     "guard-not-met": loo["guard_not_met"]},
        "baseline": {**selected_baseline, "fact": produces},
        "candidate-baselines": baselines,
        "proxy": {"move"},
        "verdict": verdict(len(full), loo, selected_baseline),
    }


def nonfamily_rows(family_rows, frames_by_session):
    """Return operator rows outside FAMILY_ROWS with their record-backed pre/post.

    Frames already embody C4's legacy admission.  A candidate needs a preceding
    frame because the guard describes the work before that operator turn.
    """
    excluded = {row.get("frame_evidence_id") for row in family_rows
                if row.get("frame_evidence_id")}
    out = []
    for session in sorted({row["session_id"] for row in family_rows}):
        frames = frames_by_session.get(session, [])
        for index in range(1, len(frames)):
            frame = frames[index]
            evidence_id = frame.get("turn", {}).get("evidence_id")
            if evidence_id in excluded:
                continue
            out.append({"session_id": session,
                        "frame_evidence_id": evidence_id,
                        "pre": c1.frame_facts(frames[index - 1]),
                        "post": c1.frame_facts(frame)})
    return out


def training_choice(training, guard):
    """Choose produces on guard-matched training rows only."""
    matched = [row for row in training if guard_holds(row, guard)]
    scored = []
    for fact in POST_FACTS:
        count = sum(post_holds(row, fact) for row in matched)
        denominator = len(matched)
        value = count / denominator if denominator else 0.0
        scored.append((value, count, -POST_FACTS.index(fact), fact))
    return max(scored)[3]


def matched_baseline(rows, guard, fact):
    matched = [row for row in rows if guard_holds(row, guard)]
    return {"count": sum(post_holds(row, fact) for row in matched),
            "denominator": len(matched)}


def c5_folds(rows, baseline_rows):
    """Score each held-out row using only its fold's induced rule."""
    folds = []
    for index, held in enumerate(rows):
        training = rows[:index] + rows[index + 1:]
        guard = induce_guard(training)
        produces = training_choice(training, guard)
        if not guard_holds(held, guard):
            result = "guard-not-met"
        else:
            result = "hit" if post_holds(held, produces) else "miss"
        folds.append({"turn-id": held["turn_id"], "guard": guard,
                      "produces": produces, "result": result,
                      "matched-baseline": matched_baseline(
                          baseline_rows, guard, produces)})
    return folds


def binomial_tail(hits, trials, probability):
    """Exact P[X >= hits] for X ~ Binomial(trials, probability)."""
    if not trials:
        return 1.0
    if probability <= 0:
        return 1.0 if hits == 0 else 0.0
    if probability >= 1:
        return 1.0
    return sum(math.comb(trials, k) * probability ** k
               * (1 - probability) ** (trials - k)
               for k in range(hits, trials + 1))


def c5_verdict(folds):
    scored = [fold for fold in folds if fold["result"] != "guard-not-met"]
    hits = sum(fold["result"] == "hit" for fold in scored)
    # One matched population per distinct fold rule.  Repeating the same
    # (guard, produces) population for every held-out turn would inflate the
    # reported row counts without adding comparison observations.
    populations = {}
    for fold in scored:
        signature = (tuple(fold.get("guard", [])), fold.get("produces"))
        populations.setdefault(signature, fold["matched-baseline"])
    base_count = sum(population["count"] for population in populations.values())
    base_den = sum(population["denominator"] for population in populations.values())
    probability = base_count / base_den if base_den else 0.0
    p = binomial_tail(hits, len(scored), probability)
    if len(scored) < MIN_JUDGEMENT:
        status = "too-few-to-judge"
    else:
        status = "better-than-baseline" if p < ALPHA else "not-better"
    return {"status": status, "p": p, "alpha": ALPHA,
            "family": {"count": hits, "denominator": len(scored)},
            "matched-baseline": {"count": base_count, "denominator": base_den}}


def induce_family_c5(family, rows, baseline_rows):
    full = [row for row in rows if "missing" not in row]
    folds = c5_folds(full, baseline_rows)
    held = Counter(fold["result"] for fold in folds)
    choices = Counter(fold["produces"] for fold in folds)
    produces = max(POST_FACTS, key=lambda fact: (choices[fact], -POST_FACTS.index(fact)))
    return {
        "schema": "xiang-cascade/induced-rule-v2",
        "family": family,
        "turns": [row["turn_id"] for row in rows],
        "full-triples": len(full),
        "guard": induce_guard(full),
        "produces": produces,
        "fold-produces": dict(choices),
        "held-out": {"hit": held["hit"], "miss": held["miss"],
                     "guard-not-met": held["guard-not-met"]},
        "matched-baseline": c5_verdict(folds)["matched-baseline"],
        "proxy": {"move"},
        "verdict": c5_verdict(folds),
        "folds": folds,
    }


def edn(value):
    if isinstance(value, dict):
        return "{" + " ".join(f":{key} {edn(item)}" for key, item in value.items()) + "}"
    if isinstance(value, (list, tuple)):
        return "[" + " ".join(edn(item) for item in value) + "]"
    if isinstance(value, set):
        return "#{" + " ".join(f":{item}" for item in sorted(value)) + "}"
    if isinstance(value, str):
        if value in ("xiang-cascade/induced-rule-v1", "xiang-cascade/induced-rule-v2",
                     "too-few-to-judge",
                     "better-than-baseline", "not-better") or value in PRE_FACTS + POST_FACTS:
            return f":{value}"
        return json.dumps(value, ensure_ascii=False)
    if isinstance(value, bool):
        return "true" if value else "false"
    if value is None:
        return "nil"
    return str(value)


def write_rule(rule, path):
    path.write_text(edn(rule) + "\n", encoding="utf-8")


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--mode", choices=("c2", "c5"), default="c5")
    parser.add_argument("--corpus", type=Path, default=None)
    parser.add_argument("--cache", type=Path, default=None)
    parser.add_argument("--output-dir", type=Path, default=None)
    args = parser.parse_args()
    corpus = args.corpus or (C2_CORPUS if args.mode == "c2" else C5_CORPUS)
    cache = args.cache or (C2_CACHE if args.mode == "c2" else C5_CACHE)
    outputs = C2_OUTPUTS if args.mode == "c2" else C5_OUTPUTS
    rows = read_jsonl(corpus)
    sessions = {row["session_id"] for row in rows}
    frames = load_cached_frames(sessions, cache, legacy=args.mode == "c5")
    rules = {}
    for family in sorted({row["family"] for row in rows}):
        family_rows = [row for row in rows if row["family"] == family]
        rules[family] = (induce_family(family, family_rows, frames)
                         if args.mode == "c2" else
                         induce_family_c5(family, family_rows,
                                          nonfamily_rows(family_rows, frames)))
        path = ((args.output_dir / outputs[family].name) if args.output_dir
                else outputs[family])
        path.parent.mkdir(parents=True, exist_ok=True)
        write_rule(rules[family], path)
    keys = (("full-triples", "guard", "produces", "held-out", "baseline", "verdict")
            if args.mode == "c2" else
            ("full-triples", "guard", "produces", "fold-produces", "held-out",
             "matched-baseline", "verdict"))
    report = {family: {key: rule[key] for key in keys}
              for family, rule in rules.items()}
    print(json.dumps(report, indent=2, sort_keys=True))


if __name__ == "__main__":
    sys.exit(main())
