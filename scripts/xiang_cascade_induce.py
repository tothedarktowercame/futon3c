#!/usr/bin/env python3
"""Induce and measure C2 rules from the M-象-cascade C1 corpus."""

from __future__ import annotations

import argparse
import json
import sys
from collections import Counter
from pathlib import Path

import xiang_cascade_corpus as c1


HERE = Path(__file__).resolve().parent
DEFAULT_CORPUS = Path("/home/joe/code/futon3c/holes/labs/M-象-cascade/corpus-c1.jsonl")
DEFAULT_CACHE = Path("/tmp/xiang-cascade-c1-frames-live")
OUTPUTS = {
    "go-ahead": Path("/home/joe/code/futon3c/holes/labs/M-象-cascade/rule-go-ahead-c2.edn"),
    "observed-running-correction": Path("/home/joe/code/futon3c/holes/labs/M-象-cascade/rule-observed-running-correction-c2.edn"),
}
POST_FACTS = ("commit-resolved", "parks-made", "parks-released")
PRE_FACTS = ("agent-reply-present", "commit-resolved", "parks-made", "parks-released")
MIN_JUDGEMENT = 10


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


def load_cached_frames(sessions, cache_dir):
    out = {}
    for session in sorted(sessions):
        result = c1.load_session_frames(session, cache_dir)
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


def edn(value):
    if isinstance(value, dict):
        return "{" + " ".join(f":{key} {edn(item)}" for key, item in value.items()) + "}"
    if isinstance(value, (list, tuple)):
        return "[" + " ".join(edn(item) for item in value) + "]"
    if isinstance(value, set):
        return "#{" + " ".join(f":{item}" for item in sorted(value)) + "}"
    if isinstance(value, str):
        if value in ("xiang-cascade/induced-rule-v1", "too-few-to-judge",
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
    parser.add_argument("--corpus", type=Path, default=DEFAULT_CORPUS)
    parser.add_argument("--cache", type=Path, default=DEFAULT_CACHE)
    parser.add_argument("--output-dir", type=Path, default=None)
    args = parser.parse_args()
    rows = read_jsonl(args.corpus)
    sessions = {row["session_id"] for row in rows}
    frames = load_cached_frames(sessions, args.cache)
    rules = {}
    for family in sorted({row["family"] for row in rows}):
        family_rows = [row for row in rows if row["family"] == family]
        rules[family] = induce_family(family, family_rows, frames)
        path = ((args.output_dir / OUTPUTS[family].name) if args.output_dir
                else OUTPUTS[family])
        path.parent.mkdir(parents=True, exist_ok=True)
        write_rule(rules[family], path)
    report = {family: {key: rule[key] for key in
                       ("full-triples", "guard", "produces", "held-out", "baseline", "verdict")}
              for family, rule in rules.items()}
    print(json.dumps(report, indent=2, sort_keys=True))


if __name__ == "__main__":
    sys.exit(main())
