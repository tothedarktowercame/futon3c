#!/usr/bin/env python3
"""Adapt one interpreted operator-turn record to scope-view data.

Usage: scope_view_turn_analysis.py RECORD.json [--out FILE]
"""
import argparse
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from turn_margin_html import fragments, load
from scope_view_data import MARKS

OFFSET_UNIT = "unicode-codepoints-zero-based-end-exclusive"
TURNFEED = os.path.expanduser("~/code/futon3/src-cljs/futon3/turnfeed/core.cljs")


def intent_glosses(path=TURNFEED):
    """Read intent -> AIF gloss from turnfeed's authoritative legend rows."""
    text = open(path, encoding="utf-8").read()
    start = text.index("(def legend-rows")
    end = text.index("(def intent-style", start)
    out = {}
    for intents, gloss in re.findall(
            r':intents\s+\[([^]]+)\].*?:aif\s+"((?:[^"\\]|\\.)*)"',
            text[start:end], re.S):
        decoded = bytes(gloss, "utf-8").decode("unicode_escape") if "\\" in gloss else gloss
        for intent in re.findall(r'"([^"]+)"', intents):
            out[intent] = decoded
    return out


def note_for(fragment):
    bits = [fragment.get("intent", "")]
    if fragment.get("target"):
        bits.append(f"target: {fragment['target']}")
    if fragment.get("rationale"):
        bits.append(fragment["rationale"])
    for ref in fragment.get("pattern_refs", []):
        bits.append(ref["id"] + (f": {ref['rationale']}" if ref.get("rationale") else ""))
    return " · ".join(filter(None, bits))


def build(path):
    record, analysis = load(path)
    if analysis is None:
        raise ValueError(f"missing analysis sidecar: {path}.analysis.json")
    if record.get("origin", "operator") != "operator":
        raise ValueError("record is not an operator turn")
    if record.get("offset_unit") != OFFSET_UNIT or analysis.get("offset_unit") != OFFSET_UNIT:
        raise ValueError(f"offset_unit must be {OFFSET_UNIT}")

    name = os.path.basename(path).removesuffix(".json")
    source = record.get("source_text", "")
    frags = fragments(analysis, prefix=f"{name}-")
    glosses = intent_glosses()
    segment_id = "analysis-" + name
    spans, edges, legend = [], [], {}
    first_cue_by_fragment = {}
    candidates = []

    for frag_index, fragment in enumerate(frags):
        if fragment.get("unresolved"):
            sid = f"{segment_id}-unresolved-{frag_index}"
            spans.append({"id": sid, "seg": segment_id,
                          "start": fragment["start"], "end": fragment["end"],
                          "layer": "analysis", "kind": "unresolved",
                          "mark": MARKS.get("unresolved"), "weight": 0.25,
                          "note": fragment.get("rationale", "unresolved")})
            legend["unresolved"] = glosses.get("unresolved", "unresolved")
            continue
        intent = fragment["intent"]
        legend[intent] = glosses.get(intent, intent)
        for cue_index, cue in enumerate(fragment.get("display_cues", [])):
            candidates.append((cue["start"], cue["end"], frag_index, cue_index, fragment, cue))

    # This is mark_prose's rule: source order, and the first overlapping cue wins.
    at = 0
    dropped = 0
    for start, end, frag_index, cue_index, fragment, cue in sorted(candidates):
        if start < at:
            dropped += 1
            continue
        sid = f"{segment_id}-f{frag_index}-c{cue_index}"
        intent = fragment["intent"]
        spans.append({"id": sid, "seg": segment_id, "start": start, "end": end,
                      "layer": "analysis", "kind": intent,
                      "mark": MARKS.get(intent), "weight": 1.0,
                      "note": note_for(fragment)})
        first_cue_by_fragment.setdefault(frag_index, sid)
        at = end

    # Relations are structural roles, not target ids. Preserve the only
    # reconstructible ordering: each later fragment relates to its predecessor.
    ordinary = [frag for frag in frags if not frag.get("unresolved")]
    ordinary_indexes = [i for i, frag in enumerate(frags) if not frag.get("unresolved")]
    for position in range(1, len(ordinary)):
        current_index = ordinary_indexes[position]
        previous_index = ordinary_indexes[position - 1]
        if current_index not in first_cue_by_fragment or previous_index not in first_cue_by_fragment:
            continue
        for relation in ordinary[position].get("relations", []):
            edges.append({"from": first_cue_by_fragment[current_index],
                          "to": first_cue_by_fragment[previous_index],
                          "relation": relation, "label": ""})
            legend[relation] = relation

    unresolved = sum(1 for sentence in analysis.get("sentences", [])
                     if not sentence.get("fragments") and sentence.get("unresolved_reason"))
    data = {
        "turn": record.get("turn_id") or name,
        "record": name,
        "session": record.get("session_id"),
        "offset_unit": OFFSET_UNIT,
        "segments": [{"id": segment_id, "role": "operator",
                      "label": f"{record.get('session_id', '')} · {record.get('created_at', '')}",
                      "text": source}],
        "layers": [{"id": "analysis", "name": "interpretation", "ink": "c"}],
        "spans": spans,
        "edges": edges,
        "legend": legend,
        "spot_check": {"fragments": len(frags), "cues_drawn": len(candidates) - dropped,
                       "cues_dropped_overlap": dropped, "unresolved_sentences": unresolved},
    }
    return data


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("record")
    parser.add_argument("--out")
    args = parser.parse_args()
    data = build(args.record)
    text = json.dumps(data, ensure_ascii=False, indent=1) + "\n"
    if args.out:
        with open(args.out, "w", encoding="utf-8") as output:
            output.write(text)
        counts = data["spot_check"]
        print(f"{args.out}: {counts['fragments']} fragments, {counts['cues_drawn']} cues, "
              f"{counts['cues_dropped_overlap']} overlaps dropped, "
              f"{counts['unresolved_sentences']} unresolved sentences")
    else:
        print(text, end="")


if __name__ == "__main__":
    main()
