#!/usr/bin/env python3
"""Compare batch-1 move incidences with the live Xiang kernel links."""
import argparse
import json
import re
from collections import Counter
from pathlib import Path

RECORDS = Path.home() / ".emacs-graph/session-turn-analysis"
OPENERS = {"propose", "ask-action", "offer", "delegate", "promise",
           "report-problem", "clarify", "verify"}
ADJACENCY = {
    "propose": {"approve": "closes", "disagree": "closes", "defer": "closes",
                "redirect": "closes", "extend": "closes", "commit": "closes",
                "withdraw": "closes", "retract": "closes"},
    "ask-action": {"accept": "closes", "disagree": "closes", "qualify": "keeps",
                   "offer": "keeps", "report": "closes", "explain": "closes"},
    "offer": {"accept": "closes", "disagree": "closes", "defer": "closes",
              "retract": "closes", "withdraw": "closes"},
    "delegate": {"promise": "closes", "report-problem": "keeps",
                 "retract": "closes", "disagree": "closes", "report": "closes"},
    "promise": {"fulfil": "closes", "release": "closes", "lapse": "closes",
                "report-problem": "keeps", "qualify": "keeps"},
    "report-problem": {"verify": "closes", "explain": "closes",
                       "redirect": "closes", "commit": "closes", "collect": "keeps"},
    "clarify": {"explain": "closes", "report": "closes"},
    "verify": {"report": "closes", "approve": "closes", "explain": "closes",
               "report-problem": "closes"},
}
STOP = {"that", "this", "with", "from", "have", "what", "when", "where", "which",
        "would", "could", "should", "about", "there", "their", "they", "them", "then",
        "than", "into", "your", "you", "ours", "also", "just", "some", "more", "does"}


def norm(text):
    return " ".join(str(text or "").split()).lower()


def words(text):
    return {word for word in re.findall(r"[a-z0-9]+", norm(text))
            if len(word) >= 4 and word not in STOP}


def bracket(text):
    match = re.match(r'^\s*[^\w\s]\s*\(["“](.*?)["”]\)', str(text or ""), re.S)
    return match.group(1) if match else None


def effect(opener, answer):
    return ADJACENCY.get(opener, {}).get(answer, "closes")


def resolve_answer(side_act, agent_acts):
    direct = next((act for act in agent_acts if act.get("id") == side_act.get("act")), None)
    if direct:
        return direct
    quote = norm(side_act.get("quote"))
    matches = [act for act in agent_acts
               if quote and (quote in norm(act.get("text")) or norm(act.get("text")) in quote)]
    return matches[0] if len(matches) == 1 else None


def analyse(moves_root, records_root=RECORDS):
    classes = Counter()
    kernel_only = []
    labeller_only_closes = []
    totals = Counter()
    unmapped_answers = 0
    unmapped_incidences = 0
    for side_path in sorted(moves_root.glob("shard-*/turn-*.moves.json")):
        side = json.loads(side_path.read_text(encoding="utf-8"))
        record_id = side["record"]
        record = json.loads((records_root / f"{record_id}.json").read_text(encoding="utf-8"))
        acts = record.get("acts") or []
        by_id = {act.get("id"): act for act in acts if act.get("id")}
        operator = [act for act in acts if act.get("author") == "operator"]
        agent = [act for act in acts if act.get("author") != "operator"]
        opener_by_id = {act["id"]: act for act in operator if act.get("kind") in OPENERS}
        kernel = {}
        for answer in agent:
            target = answer.get("target")
            if target:
                opener = by_id.get(target, {})
                kernel[(answer["id"], target)] = effect(opener.get("kind"), answer.get("kind"))
        current_ids = set(by_id)
        for inferred in (record.get("ports") or {}).get("inferred") or []:
            answer_id, opener_id = inferred.get("answer"), inferred.get("opener")
            if answer_id not in current_ids:
                continue
            answer, opener = by_id.get(answer_id, {}), by_id.get(opener_id, {})
            kernel[(answer_id, opener_id)] = (inferred.get("effect")
                                              or effect(opener.get("kind"), answer.get("kind")))
        labelled = {}
        answer_details = {}
        for side_act in side.get("acts", []):
            if not side_act.get("answers"):
                continue
            totals["raw_labelled"] += len(side_act["answers"])
            totals["raw_labelled_closes"] += sum(
                incidence.get("effect") == "closes" for incidence in side_act["answers"])
            answer = resolve_answer(side_act, agent)
            if not answer:
                unmapped_answers += 1
                unmapped_incidences += len(side_act["answers"])
                answer = {"id": side_act.get("act"), "kind": side_act.get("kind"),
                          "text": side_act.get("quote", "")}
            answer_details[answer["id"]] = answer
            for incidence in side_act["answers"]:
                labelled[(answer["id"], incidence.get("act"))] = incidence.get("effect")
        totals["kernel"] += len(kernel)
        totals["labelled"] += len(labelled)
        totals["labelled_closes"] += sum(v == "closes" for v in labelled.values())
        for pair in sorted(set(kernel) | set(labelled)):
            kval, lval = kernel.get(pair), labelled.get(pair)
            if kval is not None and lval is not None:
                if kval == lval:
                    classes["agreement"] += 1
                else:
                    classes["effect-disagreement"] += 1
            elif kval is not None:
                classes["kernel-only"] += 1
                answer, opener = by_id.get(pair[0], {}), by_id.get(pair[1], {})
                kernel_only.append({"record": record_id, "answer": pair[0], "opener": pair[1],
                                    "effect": kval, "answer_quote": answer.get("text", ""),
                                    "opener_quote": opener.get("text", "")})
            else:
                classes["labeller-only"] += 1
                if lval == "closes":
                    answer, opener = answer_details.get(pair[0], {}), opener_by_id.get(pair[1], {})
                    same_kind = [act for act in opener_by_id.values()
                                 if act.get("kind") == opener.get("kind")]
                    label = bracket(answer.get("text"))
                    if label and len(words(label) & words(opener.get("text"))) >= 2:
                        rule = "A: bracket overlap"
                    elif len(same_kind) == 1:
                        rule = "B: sole opener of kind"
                    else:
                        closable = [act for act in opener_by_id.values()
                                    if ADJACENCY.get(act.get("kind"), {}).get(answer.get("kind")) == "closes"]
                        rule = ("C: unique adjacency closer" if len(closable) == 1 and closable[0].get("id") == pair[1]
                                else "D: none")
                    labeller_only_closes.append(rule)
        totals["labelled_closes_reached"] += sum(
            lval == "closes" and pair in kernel and kernel[pair] == "closes"
            for pair, lval in labelled.items())
    return (classes, totals, kernel_only, Counter(labeller_only_closes),
            unmapped_answers, unmapped_incidences)


def pct(n, d):
    return 100.0 * n / d if d else 0.0


def render(result):
    classes, totals, kernel_only, rules, unmapped, unmapped_incidences = result
    overlap = classes["agreement"] + classes["effect-disagreement"]
    lines = ["# Kernel answer links against batch-1 move incidences",
             "", "Read-only classical comparison generated by `moves/kernel_links_vs_labels.py`.",
             "", "## Headline", "",
             f"Batch 1 contains **{totals['raw_labelled']} labelled incidences** "
             f"(**{totals['labelled']} distinct resolved pairs**; two sidecar acts collapse onto one live pair). "
             "The kernel exposes "
             f"**{totals['kernel']} links** on those records. {unmapped} labelled answering acts "
             f"({unmapped_incidences} incidences) could not be mapped uniquely to a live reply act; they remain "
             "labeller-only pairs because the kernel exposes no corresponding answering act.", "",
             "| class | pairs |", "|---|---:|",
             f"| agreement (same pair and effect) | {classes['agreement']} |",
             f"| effect disagreement | {classes['effect-disagreement']} |",
             f"| kernel-only | {classes['kernel-only']} |",
             f"| labeller-only | {classes['labeller-only']} |", "",
             f"Kernel pair precision (human links the same pair): **{overlap}/{totals['kernel']} "
             f"({pct(overlap, totals['kernel']):.1f}%)**. Exact-effect precision: "
             f"**{classes['agreement']}/{totals['kernel']} ({pct(classes['agreement'], totals['kernel']):.1f}%)**.",
             f"Distinct-pair labelled-close recall: **{totals['labelled_closes_reached']}/{totals['labelled_closes']} "
             f"({pct(totals['labelled_closes_reached'], totals['labelled_closes']):.1f}%)** "
             f"({totals['raw_labelled_closes']} raw close incidences).", "",
             "## Rules that could reach labeller-only closes", "",
             "Rules are exclusive in A → B → C → none order.", "", "| rule | closes |", "|---|---:|"]
    for rule in ["A: bracket overlap", "B: sole opener of kind", "C: unique adjacency closer", "D: none"]:
        lines.append(f"| {rule} | {rules[rule]} |")
    lines += ["", "## Kernel-only links", "",
              "| record | answering act | opener | effect | answering quote | opener quote |",
              "|---|---|---|---|---|---|"]
    esc = lambda s: norm(s).replace("|", "\\|")[:240]
    for row in kernel_only:
        lines.append(f"| `{row['record']}` | `{row['answer']}` | `{row['opener']}` | {row['effect']} | "
                     f"{esc(row['answer_quote'])} | {esc(row['opener_quote'])} |")
    lines += ["", "## Method", "",
              "Explicit kernel links are live agent acts carrying `target`; inferred links are current-record "
              "entries in `ports.inferred`. Missing stored effects are recomputed from the checked-in adjacency "
              "table, with the kernel's explicit-link default of `closes`. Sidecar reply ids that differ from "
              "kernel ids are joined only by a unique whitespace-normalised quote match.", ""]
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--moves", type=Path,
                        default=Path(__file__).resolve().parents[1] / "data/moves-scale")
    parser.add_argument("--records", type=Path, default=RECORDS)
    parser.add_argument("--out", type=Path)
    args = parser.parse_args()
    text = render(analyse(args.moves, args.records))
    if args.out:
        args.out.write_text(text + "\n", encoding="utf-8")
    else:
        print(text)


if __name__ == "__main__":
    main()
