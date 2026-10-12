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


def load_case(side_path, records_root):
    side = json.loads(side_path.read_text(encoding="utf-8"))
    record = json.loads((records_root / f"{side['record']}.json").read_text(encoding="utf-8"))
    acts = record.get("acts") or []
    by_id = {act.get("id"): act for act in acts if act.get("id")}
    operator = [act for act in acts if act.get("author") == "operator"]
    agent = [act for act in acts if act.get("author") != "operator"]
    labelled = {}
    for side_act in side.get("acts", []):
        answer = resolve_answer(side_act, agent)
        if answer:
            for incidence in side_act.get("answers", []):
                labelled[(answer["id"], incidence.get("act"))] = incidence.get("effect")
    return side, record, by_id, operator, agent, labelled


def move_paths(root):
    return sorted(root.glob("shard-*/turn-*.moves.json")) or sorted(root.glob("turn-*.moves.json"))


def canonical_replies(side, agent):
    """Return ordered reply acts plus labels, retaining unmapped sidecar acts."""
    replies, labelled = [], {}
    for side_act in side.get("acts", []):
        if "-r-" not in str(side_act.get("act", "")):
            continue
        live = resolve_answer(side_act, agent)
        side_kind = str(side_act.get("move", "")).split("/", 1)[0]
        answer = live or {"id": side_act.get("act"), "kind": side_kind,
                          "text": side_act.get("quote", "")}
        replies.append(answer)
        for incidence in side_act.get("answers", []):
            labelled[(answer["id"], incidence.get("act"))] = incidence.get("effect")
    return replies, labelled


def analyse(moves_root, records_root=RECORDS):
    classes = Counter()
    kernel_only = []
    labeller_only_closes = []
    totals = Counter()
    unmapped_answers = 0
    unmapped_incidences = 0
    for side_path in move_paths(moves_root):
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


def rule_b_analysis(paths, records_root=RECORDS):
    variants = {"B": Counter(), "B*": Counter()}
    breakdown = {"B": {}, "B*": {}}
    disagreements = Counter()
    disagreement_examples = {"kernel closes / labeller keeps": [],
                             "kernel keeps / labeller closes": []}
    sole = Counter()
    sole_by_kind = {}
    for side_path in paths:
        side, record, by_id, operator, agent, labelled = load_case(side_path, records_root)
        openers = [act for act in operator if act.get("kind") in OPENERS]
        kind_counts = Counter(act.get("kind") for act in openers)
        unique = [act for act in openers if kind_counts[act.get("kind")] == 1]
        # The sidecars are the complete reply-act population.  Canonicalise to
        # a live id where possible, but retain an unmapped sidecar act: absence
        # from the live segmentation must not silently shrink recall's
        # labelled-close denominator.
        replies, labelled_all = canonical_replies(side, agent)
        candidates = {"B": {}, "B*": {}}
        for answer in replies:
            if answer.get("target"):
                continue
            for opener in unique:
                pair = (answer["id"], opener["id"])
                candidates["B"][pair] = effect(opener.get("kind"), answer.get("kind"))
                typed = ADJACENCY.get(opener.get("kind"), {}).get(answer.get("kind"))
                if typed:
                    candidates["B*"][pair] = typed
        labelled_closes = {pair for pair, value in labelled_all.items() if value == "closes"}
        for variant, pairs in candidates.items():
            stats = variants[variant]
            stats["asserted"] += len(pairs)
            stats["confirmed"] += sum(pair in labelled_all for pair in pairs)
            stats["same"] += sum(labelled_all.get(pair) == value for pair, value in pairs.items())
            stats["silent"] += sum(pair not in labelled_all for pair in pairs)
            stats["close_reached"] += sum(pair in labelled_closes and value == "closes"
                                          for pair, value in pairs.items())
            stats["labelled_closes"] += len(labelled_closes)
            for pair, value in pairs.items():
                opener = by_id.get(pair[1], {})
                answer = next((act for act in replies if act.get("id") == pair[0]), {})
                key = (opener.get("kind", "?"), answer.get("kind", "?"))
                row = breakdown[variant].setdefault(key, Counter())
                row["asserted"] += 1
                row["confirmed"] += pair in labelled_all
                row["same"] += labelled_all.get(pair) == value
                row["silent"] += pair not in labelled_all

        # Reconstruct the 98101d97 effect-disagreement population.
        kernel = {}
        for answer in agent:
            if answer.get("target"):
                opener = by_id.get(answer["target"], {})
                kernel[(answer["id"], answer["target"])] = effect(opener.get("kind"), answer.get("kind"))
        for inferred in (record.get("ports") or {}).get("inferred") or []:
            pair = (inferred.get("answer"), inferred.get("opener"))
            if pair[0] in by_id:
                kernel[pair] = (inferred.get("effect") or
                                effect(by_id.get(pair[1], {}).get("kind"),
                                       by_id.get(pair[0], {}).get("kind")))
        for pair in set(kernel) & set(labelled):
            if kernel[pair] == labelled[pair]:
                continue
            opener, answer = by_id.get(pair[1], {}), by_id.get(pair[0], {})
            direction = f"kernel {kernel[pair]} / labeller {labelled[pair]}"
            disagreements[(opener.get("kind", "?"), answer.get("kind", "?"), direction)] += 1
            if len(disagreement_examples.setdefault(direction, [])) < 3:
                side_answer = next((act for act in side.get("acts", [])
                                    if (resolve_answer(act, agent) or {}).get("id") == pair[0]), {})
                disagreement_examples[direction].append(
                    {"record": side["record"], "answer": answer.get("text", ""),
                     "opener": opener.get("text", ""),
                     "opener_kind": opener.get("kind", "?"),
                     "answer_kind": answer.get("kind", "?"),
                     "label_evidence": side_answer.get("note") or side_answer.get("quote", "")})

        inferred = [row for row in (record.get("ports") or {}).get("inferred") or []
                    if row.get("rule") == "sole-port" and row.get("answer") in by_id]
        sole["asserted"] += len(inferred)
        for row in inferred:
            answer = by_id.get(row.get("answer"), {})
            confirmed = (row.get("answer"), row.get("opener")) in labelled
            sole["confirmed"] += confirmed
            sole["silent"] += not confirmed
            kind = answer.get("kind", "?")
            krow = sole_by_kind.setdefault(kind, Counter())
            krow["asserted"] += 1
            krow["confirmed"] += confirmed
            krow["silent"] += not confirmed
    return variants, breakdown, disagreements, disagreement_examples, sole, sole_by_kind


def sole_port_variants(paths, records_root=RECORDS):
    names = ["V0", "V1", "V2", "V3", "V4", "V5"]
    totals = {name: Counter() for name in names}
    false_kinds = {name: Counter() for name in names}
    records = 0
    for side_path in paths:
        side, _, _, operator, agent, _ = load_case(side_path, records_root)
        openers = [act for act in operator if act.get("kind") in OPENERS]
        if not operator:
            input_path = side_path.parent / "input" / f"{side['record']}.input.json"
            if input_path.exists():
                input_record = json.loads(input_path.read_text(encoding="utf-8"))
                operator = [{"id": row.get("id"), "kind": row.get("intent"),
                             "text": row.get("text", ""), "author": "operator"}
                            for row in input_record.get("fragments", [])]
                openers = [act for act in operator if act.get("kind") in OPENERS]
        if len(openers) != 1:
            continue
        records += 1
        opener = openers[0]
        replies, labelled = canonical_replies(side, agent)
        first_ask = next((i for i, act in enumerate(replies)
                          if act.get("kind") in {"ask-action", "offer"}
                          or "🉸" in str(act.get("text", ""))), len(replies))
        untargeted = [act for act in replies if not act.get("target")]
        before_ask = [act for act in replies[:first_ask] if not act.get("target")]
        first_closer = next((act for act in untargeted
                             if ADJACENCY.get(opener.get("kind"), {}).get(act.get("kind")) == "closes"), None)
        typed_before_ask = [act for act in before_ask
                            if ADJACENCY.get(opener.get("kind"), {}).get(act.get("kind"))
                            in {"closes", "keeps"}]
        selected = {
            "V0": untargeted,
            "V1": untargeted[:1],
            "V2": [first_closer] if first_closer else [],
            "V3": before_ask,
            "V4": [first_closer] if first_closer in before_ask else [],
            "V5": typed_before_ask,
        }
        labelled_closes = {pair for pair, value in labelled.items()
                           if value == "closes" and pair[1] == opener["id"]}
        record_has_close = any(value == "closes" for value in labelled.values())
        for name, answers in selected.items():
            row = totals[name]
            row["records"] += 1
            row["labelled_closes"] += len(labelled_closes)
            predicted_close = False
            for answer in answers:
                pair = (answer["id"], opener["id"])
                predicted = effect(opener.get("kind"), answer.get("kind"))
                predicted_close = predicted_close or predicted == "closes"
                row["asserted"] += 1
                row["confirmed"] += pair in labelled
                row["same"] += labelled.get(pair) == predicted
                row["false"] += pair not in labelled
                row["close_reached"] += pair in labelled_closes and predicted == "closes"
                if pair not in labelled:
                    false_kinds[name][answer.get("kind", "?")] += 1
            row["mis_close"] += predicted_close and not record_has_close
    return records, totals, false_kinds


def render_sole_variants(datasets, disagreement_examples):
    lines = ["## Sole-port variants", "",
             "This experiment is restricted to records with exactly one opener total. V3's ask boundary "
             "is the first reply act typed `ask-action` or `offer`, or containing 🉸; the boundary act "
             "itself is excluded. An absent labeller incidence counts as a false positive.", ""]
    for title, paths, result in datasets:
        records, totals, false_kinds = result
        lines += [f"### {title}", "",
                  f"Sidecars: **{len(paths)}**; one-opener records: **{records}**.", "",
                  "| variant | asserted | confirmed | same effect | false positives | pair precision | exact precision | close recall |",
                  "|---|---:|---:|---:|---:|---:|---:|---:|"]
        for name in ["V0", "V1", "V2", "V3", "V4"]:
            row = totals[name]
            lines.append(f"| {name} | {row['asserted']} | {row['confirmed']} | {row['same']} | {row['false']} | "
                         f"{pct(row['confirmed'], row['asserted']):.1f}% | {pct(row['same'], row['asserted']):.1f}% | "
                         f"{row['close_reached']}/{row['labelled_closes']} "
                         f"({pct(row['close_reached'], row['labelled_closes']):.1f}%) |")
        kinds = sorted({kind for rows in false_kinds.values() for kind in rows})
        lines += ["", "False positives by reply kind:", "",
                  "| reply kind | " + " | ".join(["V0", "V1", "V2", "V3", "V4"]) + " |",
                  "|---|" + "---:|" * 5]
        for kind in kinds:
            lines.append(f"| {kind} | " + " | ".join(str(false_kinds[name][kind]) for name in
                                                      ["V0", "V1", "V2", "V3", "V4"]) + " |")
        lines.append("")
    lines += ["### Reading the 18 effect disagreements", "",
              "All 18 point in the same direction: the kernel says `closes`, while the labeller says "
              "`keeps`. No kernel-keeps/labeller-closes case was observed.", "",
              "| record | opener/reply kinds | labeller note or labelled reply span |",
              "|---|---|---|"]
    rows = disagreement_examples.get("kernel closes / labeller keeps", [])[:3]
    for row in rows:
        evidence = norm(row.get("label_evidence")).replace("|", "\\|")[:300]
        lines.append(f"| `{row['record']}` | {row['opener_kind']} / {row['answer_kind']} | {evidence} |")
    lines += ["", "These examples read as partial delivery, qualification, or evidence bearing on an "
              "opener—not as the labeller denying that the reply is related to it.", ""]
    return "\n".join(lines)


def render_batch2_variants(paths, result):
    records, totals, false_kinds = result
    names = ["V0", "V1", "V2", "V3", "V4", "V5"]
    lines = ["## Sole-port variants, batch 2", "",
             f"All **{len(paths)}** batch-2 sidecars are present; **{records}** have exactly one opener.", "",
             "| variant | asserted | confirmed | same effect | false positives | pair precision | exact precision | close recall | mis-closes |",
             "|---|---:|---:|---:|---:|---:|---:|---:|---:|"]
    for name in names:
        row = totals[name]
        lines.append(f"| {name} | {row['asserted']} | {row['confirmed']} | {row['same']} | {row['false']} | "
                     f"{pct(row['confirmed'], row['asserted']):.1f}% | {pct(row['same'], row['asserted']):.1f}% | "
                     f"{row['close_reached']}/{row['labelled_closes']} "
                     f"({pct(row['close_reached'], row['labelled_closes']):.1f}%) | {row['mis_close']} |")
    kinds = sorted({kind for rows in false_kinds.values() for kind in rows})
    lines += ["", "False positives by reply kind:", "",
              "| reply kind | " + " | ".join(names) + " |",
              "|---|" + "---:|" * len(names)]
    for kind in kinds:
        lines.append(f"| {kind} | " + " | ".join(str(false_kinds[name][kind]) for name in names) + " |")
    lines += ["", "### Pick", "",
              "1. I would put **V3 (before the ask)** in the kernel.",
              "2. It preserves almost all of V0's close recall while reducing both asserted pairs and false positives.",
              "3. It also reduces mis-closes; that matters most because a false close hides an owed item.",
              "4. V2, V4, and V5 cut mis-closes further, but each loses more than one third of V0's close recall.",
              "5. V1 loses too much recall without improving mis-closes enough; a visible false open is safer than that trade."]
    return "\n".join(lines)


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


def render_rule_b(batch, pilot):
    variants, breakdown, disagreements, examples, sole, sole_by_kind = batch
    _, _, _, _, pilot_sole, pilot_by_kind = pilot
    lines = ["## Rule B precision; sole-port false positives", "",
             "B links every untargeted reply paragraph to every opener that is the turn's only "
             "opener of its kind. B* retains only adjacency-typed pairs. Labeller-silent is the "
             "requested false-positive definition; an omitted label is treated as disagreement, not unknown.", "",
             "| variant | asserted | labeller-confirmed | same effect | labeller-silent | pair precision | exact-effect precision | labelled-close recall |",
             "|---|---:|---:|---:|---:|---:|---:|---:|"]
    for variant in ["B", "B*"]:
        row = variants[variant]
        lines.append(f"| {variant} | {row['asserted']} | {row['confirmed']} | {row['same']} | {row['silent']} | "
                     f"{pct(row['confirmed'], row['asserted']):.1f}% | {pct(row['same'], row['asserted']):.1f}% | "
                     f"{row['close_reached']}/{row['labelled_closes']} ({pct(row['close_reached'], row['labelled_closes']):.1f}%) |")
    for variant in ["B", "B*"]:
        lines += ["", f"### {variant}: top 12 opener/reply kinds", "",
                  "| opener | reply | asserted | confirmed | same effect | silent | precision |",
                  "|---|---|---:|---:|---:|---:|---:|"]
        ranked = sorted(breakdown[variant].items(), key=lambda item: (-item[1]["asserted"], item[0]))[:12]
        for (opener, answer), row in ranked:
            lines.append(f"| {opener} | {answer} | {row['asserted']} | {row['confirmed']} | "
                         f"{row['same']} | {row['silent']} | {pct(row['confirmed'], row['asserted']):.1f}% |")
    lines += ["", "### Existing effect disagreements", "",
              "| opener | reply | direction | pairs |", "|---|---|---|---:|"]
    for (opener, answer, direction), count in sorted(disagreements.items(), key=lambda item: (-item[1], item[0])):
        lines.append(f"| {opener} | {answer} | {direction} | {count} |")
    lines += ["", "Quoted examples (up to three per observed direction):", ""]
    for direction, rows in examples.items():
        if not rows:
            lines.append(f"**{direction}.** None observed.")
            lines.append("")
            continue
        lines.append(f"**{direction}.**")
        for row in rows[:3]:
            lines.append(f"- `{row['record']}` — answer: “{norm(row['answer'])[:180]}”; opener: “{norm(row['opener'])[:180]}”.")
        lines.append("")

    def sole_table(title, total, by_kind):
        out = [f"### {title}", "",
               f"Current stored sole-port links: **{total['asserted']}**; labeller-confirmed "
               f"**{total['confirmed']}**; labeller-silent **{total['silent']}**. False-positive rate: "
               f"**{pct(total['silent'], total['asserted']):.1f}%**.", "",
               "| reply kind | asserted | confirmed | silent | false-positive rate |",
               "|---|---:|---:|---:|---:|"]
        for kind, row in sorted(by_kind.items(), key=lambda item: (-item[1]["asserted"], item[0])):
            out.append(f"| {kind} | {row['asserted']} | {row['confirmed']} | {row['silent']} | "
                       f"{pct(row['silent'], row['asserted']):.1f}% |")
        return out

    lines += [""] + sole_table("Current sole-port: batch 1", sole, sole_by_kind)
    lines += [""] + sole_table("Current sole-port: 20-record pilot", pilot_sole, pilot_by_kind)
    combined = sole + pilot_sole
    lines += ["", f"Combined (counting the two requested populations independently): **{combined['silent']}/{combined['asserted']} "
              f"({pct(combined['silent'], combined['asserted']):.1f}%)** labeller-silent.", ""]
    return "\n".join(lines)


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--moves", type=Path,
                        default=Path(__file__).resolve().parents[1] / "data/moves-scale")
    parser.add_argument("--records", type=Path, default=RECORDS)
    parser.add_argument("--out", type=Path)
    args = parser.parse_args()
    pilot_root = Path(__file__).resolve().parents[1] / "data/moves-pilot"
    scale2_root = Path(__file__).resolve().parents[1] / "data/moves-scale2"
    batch_paths = move_paths(args.moves)
    pilot_paths = move_paths(pilot_root)
    scale2_paths = move_paths(scale2_root)
    variant_sets = [("Batch 1", batch_paths, sole_port_variants(batch_paths, args.records)),
                    ("20-record pilot", pilot_paths, sole_port_variants(pilot_paths, args.records))]
    batch2_variants = sole_port_variants(scale2_paths, args.records) if scale2_paths else None
    batch_rule_b = rule_b_analysis(batch_paths, args.records)
    text = (render(analyse(args.moves, args.records)) + "\n" +
            render_rule_b(batch_rule_b, rule_b_analysis(pilot_paths, args.records)) + "\n" +
            render_sole_variants(variant_sets, batch_rule_b[3]) +
            (("\n" + render_batch2_variants(scale2_paths, batch2_variants))
             if batch2_variants else ""))
    if args.out:
        args.out.write_text(text + "\n", encoding="utf-8")
    else:
        print(text)


if __name__ == "__main__":
    main()
