#!/usr/bin/env python3
"""Consolidate open-ended move labels into a first-pass vocabulary."""

import argparse
import collections
import glob
import json
import re
from pathlib import Path


STOP = set("""the a an and or of to in on for with that this these those is are was were be been being it its
we you i they he she our your their my me us them not no yes do does did done have has had will would can could
should may might just also then than so as at by from into over about what which who how when where why let lets
please ok okay well right sure thing things something anything some any all more most much very really there here
one two first next now still again back out up down off like want need think know see use used using make made
going go get got take went come came say said work works working way ways check plan step""".split())


def content_words(text):
    words = set()
    for raw in re.findall(r"\S+", (text or "").lower()):
        if any(mark in raw for mark in ("/", "_", "`", "象")) or any(char.isdigit() for char in raw):
            continue
        word = re.sub(r"^[^a-z]+|[^a-z]+$", "", raw)
        if not re.fullmatch(r"[a-z][a-z'\-]{2,}", word):
            continue
        if re.fullmatch(r"(?:claude|codex|zai)-\w+", word) or re.fullmatch(r"futon\w*", word):
            continue
        if word not in STOP and len(word) >= 4:
            words.add(word)
    return words


def _stem(part):
    if len(part) > 4 and part.endswith("ies"):
        return part[:-3] + "y"
    if len(part) > 5 and part.endswith("ing"):
        stem = part[:-3]
        if len(stem) > 2 and stem[-1] == stem[-2]:
            stem = stem[:-1]
        return stem
    if len(part) > 4 and part.endswith("ed"):
        stem = part[:-2]
        if len(stem) > 2 and stem[-1] == stem[-2]:
            stem = stem[:-1]
        return stem
    if len(part) > 3 and part.endswith("s") and not part.endswith(("ss", "us", "is")):
        return part[:-1]
    return part


def normalize_label(label):
    """Lower-case and hyphenate a kind/name label, then strip inflections."""
    kind, separator, name = label.strip().lower().partition("/")
    if not separator:
        raise ValueError(f"move label lacks kind prefix: {label!r}")
    kind = re.sub(r"[^a-z0-9]+", "-", kind).strip("-")
    parts = re.sub(r"[^a-z0-9]+", "-", name).strip("-").split("-")
    name = "-".join(_stem(part) for part in parts if part)
    if not kind or not name:
        raise ValueError(f"move label normalises to an empty component: {label!r}")
    return f"{kind}/{name}"


def label_stems(label):
    """Morphological components in the label name, independent of quote STOP."""
    return {_stem(part) for part in label.split("/", 1)[1].split("-") if len(part) >= 3}


def consolidate(acts):
    """Return (vocabulary, label_map, summary, folds) for ACTS."""
    original = collections.defaultdict(list)
    normalized = collections.defaultdict(list)
    for act in acts:
        label = act["move"]
        quote = act.get("quote", "")
        original[label].append(quote)
        normalized[normalize_label(label)].append(quote)

    counts = {label: len(quotes) for label, quotes in normalized.items()}
    words = {
        label: set().union(*(content_words(quote) for quote in quotes))
        for label, quotes in normalized.items()
    }
    destination = {}
    fold_reasons = {}
    by_kind = collections.defaultdict(list)
    for label in normalized:
        by_kind[label.split("/", 1)[0]].append(label)

    for kind, labels in by_kind.items():
        frequent = [label for label in labels if counts[label] >= 3]
        for label in labels:
            if counts[label] >= 3:
                destination[label] = label
                continue
            candidates = []
            for candidate in frequent:
                shared = sorted(label_stems(label) & label_stems(candidate))
                if shared:
                    candidates.append((-counts[candidate], candidate, shared[0]))
            if candidates:
                _, target, shared = min(candidates)
                destination[label] = target
                fold_reasons[label] = f"label:{shared}"
                continue
            candidates = []
            for candidate in frequent:
                overlap = sorted(words[label] & words[candidate])
                if len(overlap) >= 2:
                    candidates.append((-counts[candidate], candidate, overlap))
            if candidates:
                _, target, overlap = min(candidates)
                destination[label] = target
                fold_reasons[label] = "folded:" + ",".join(overlap)
            else:
                destination[label] = f"{kind}/other"

    consolidated_quotes = collections.defaultdict(list)
    for label, quotes in normalized.items():
        consolidated_quotes[destination[label]].extend(quotes)

    mapping = {}
    folds = []
    for label in sorted(original):
        norm = normalize_label(label)
        target = destination[norm]
        if target.endswith("/other"):
            reason = "other"
        elif target != norm:
            reason = fold_reasons[norm]
        elif label != norm:
            reason = "normalised"
        else:
            reason = "kept"
        mapping[label] = {"label": target, "reason": reason}
        if target != norm:
            folds.append({
                "from": label,
                "normalized": norm,
                "to": target,
                "count": len(original[label]),
                "reason": reason,
            })

    kinds = {}
    all_kinds = sorted({label.split("/", 1)[0] for label in consolidated_quotes})
    for kind in all_kinds:
        entries = []
        for label, quotes in consolidated_quotes.items():
            if label.split("/", 1)[0] != kind:
                continue
            entries.append({
                "label": label,
                "count": len(quotes),
                "examples": quotes[:3],
            })
        entries.sort(key=lambda item: (-item["count"], item["label"]))
        kinds[kind] = {
            "act_count": sum(item["count"] for item in entries),
            "labels": entries,
        }

    summary = []
    for kind in sorted(by_kind):
        before = sum(1 for label in original if label.split("/", 1)[0].lower() == kind)
        after = sum(1 for label in consolidated_quotes if label.split("/", 1)[0] == kind)
        other = len(consolidated_quotes.get(f"{kind}/other", []))
        summary.append({"kind": kind, "before": before, "after": after, "other": other})

    vocabulary = {
        "schema": "moves/vocabulary/1",
        "total_acts": len(acts),
        "kinds": kinds,
    }
    label_map = {"schema": "moves/label-map/1", "labels": mapping}
    return vocabulary, label_map, summary, folds


def load_acts(pattern):
    paths = sorted(glob.glob(pattern))
    if not paths:
        raise SystemExit(f"no files matched {pattern!r}")
    acts = []
    for path in paths:
        with open(path, encoding="utf-8") as handle:
            document = json.load(handle)
        acts.extend(document.get("acts", []))
    return paths, acts


def write_json(path, value):
    target = Path(path)
    target.parent.mkdir(parents=True, exist_ok=True)
    target.write_text(json.dumps(value, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def main(argv=None):
    parser = argparse.ArgumentParser()
    parser.add_argument("--moves-glob", required=True)
    parser.add_argument("--out", required=True)
    parser.add_argument("--map", dest="map_path", required=True)
    args = parser.parse_args(argv)
    paths, acts = load_acts(args.moves_glob)
    vocabulary, label_map, summary, _ = consolidate(acts)
    vocabulary["source_files"] = paths
    write_json(args.out, vocabulary)
    write_json(args.map_path, label_map)
    print("kind\tlabels-before\tlabels-after\tacts-under-other")
    for row in summary:
        print(f"{row['kind']}\t{row['before']}\t{row['after']}\t{row['other']}")
    print(f"TOTAL\t{sum(r['before'] for r in summary)}\t"
          f"{sum(r['after'] for r in summary)}\t{sum(r['other'] for r in summary)}")


if __name__ == "__main__":
    main()
