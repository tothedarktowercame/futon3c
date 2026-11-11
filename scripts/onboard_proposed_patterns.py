#!/usr/bin/env python3
"""onboard_proposed_patterns.py — write recurring 象 proposals into the futon3 library.

Joe, 2026-11-11: "'proposal recurs' is enough for me to accept it, and on
that basis we should onboard the 84".  Status ladder: proposed (象's
per-turn proposal) -> accepted (it recurs: proposed for two or more turns)
-> attested (used in a mission or a War Machine cascade; set later, not here).

A proposal recurs when its id appears in the candidates of two or more turn
records under TURN_DIR.  The written text is 象's own: the most common
wording of the five parts among its proposals (ties: the earliest record),
never a rewrite.  An id whose THEN was worded four or more ways is flagged
in its @status line.  Existing library files are never overwritten.

  onboard_proposed_patterns.py [--write] [--min-recur 2]

Without --write it lists what it would write.
"""
import argparse
import collections
import datetime
import glob
import json
import os
import re

TURN_DIR = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
LIBRARY = os.path.expanduser("~/code/futon3/library")
FIELDS = ("title", "if", "however", "then", "because")
ID = re.compile(r"^[a-z0-9][a-z0-9-]*/[a-z0-9][a-z0-9-]*$")


def proposals(turn_dir=TURN_DIR):
    """id -> [(record id, mtime, proposal)], oldest first."""
    by = collections.defaultdict(list)
    # Two namings: turn-X.json.candidates.json (current) and the older
    # turn-X.candidates.json.
    for path in glob.glob(os.path.join(turn_dir, "turn-*.candidates.json")):
        try:
            with open(path, encoding="utf-8") as f:
                data = json.load(f)
        except (OSError, ValueError):
            continue
        items = data if isinstance(data, list) else data.get("candidates", [])
        record = re.sub(r"(\.json)?\.candidates\.json$", "", os.path.basename(path))
        for p in items if isinstance(items, list) else []:
            if isinstance(p, dict) and isinstance(p.get("id"), str):
                by[p["id"]].append((record, os.path.getmtime(path), p))
    for v in by.values():
        v.sort(key=lambda t: t[1])
    return by


def chosen(versions):
    """The most common complete wording; ties go to the earliest."""
    counts = collections.Counter()
    first = {}
    for i, (_, _, p) in enumerate(versions):
        key = tuple((p.get(k) or "").strip() for k in FIELDS)
        if all(key[1:]):
            counts[key] += 1
            first.setdefault(key, i)
    if not counts:
        return None
    key = max(counts, key=lambda k: (counts[k], -first[k]))
    return dict(zip(FIELDS, key)), versions[first[key]][2]


def flexiarg(pid, versions, today):
    pick = chosen(versions)
    if pick is None:
        return None
    text, source = pick
    records = sorted({r for r, _, _ in versions})
    thens = {(p.get("then") or "").strip() for _, _, p in versions}
    varies = "; THEN worded %d ways across its proposals" % len(thens) if len(thens) >= 4 else ""
    # Only directives in futon3's flexiarg-directives.edn: 象's "parent" is
    # the general pattern a proposal rests on, i.e. @why; the turn it was
    # first proposed for goes into @provenance (there is no @context).
    context = re.sub(r"\s+", " ", source.get("context") or "").strip()
    lines = [
        "@flexiarg %s" % pid,
        "@title %s" % (text["title"] or pid.split("/")[1].replace("-", " ")),
        "@status accepted %s: proposed by 象 for %d turns (Joe %s: recurrence suffices); "
        "attested once used in a mission or a War Machine cascade%s" % (today, len(records), today, varies),
        "@provenance 象 proposals, ~/.emacs-graph/session-turn-analysis/<record>[.json].candidates.json; "
        "records: %s%s" % (" ".join(records),
                           "; first proposed for: %s" % context if context else ""),
    ]
    if source.get("parent"):
        lines.append("@why [%s]" % source["parent"])
    lines += ["", "! conclusion: %s" % text["then"], ""]
    for part, key in (("IF", "if"), ("HOWEVER", "however"), ("THEN", "then"), ("BECAUSE", "because")):
        lines += ["  + %s:" % part, "    %s" % re.sub(r"\s+", " ", text[key]).strip(), ""]
    return "\n".join(lines)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--write", action="store_true")
    ap.add_argument("--min-recur", type=int, default=2)
    ap.add_argument("--library", default=LIBRARY)
    a = ap.parse_args(argv)
    today = datetime.date.today().isoformat()
    written, skipped = [], []
    for pid, versions in sorted(proposals().items()):
        if len({r for r, _, _ in versions}) < a.min_recur:
            continue
        path = os.path.join(a.library, pid + ".flexiarg")
        if not ID.match(pid):
            skipped.append((pid, "id not of the form family/name"))
            continue
        if os.path.exists(path):
            skipped.append((pid, "already in the library"))
            continue
        body = flexiarg(pid, versions, today)
        if body is None:
            skipped.append((pid, "no proposal with all four parts"))
            continue
        if a.write:
            os.makedirs(os.path.dirname(path), exist_ok=True)
            with open(path, "w", encoding="utf-8") as f:
                f.write(body)
        written.append(path)
    print("%s %d" % ("wrote" if a.write else "would write", len(written)))
    for pid, why in skipped:
        print("skipped %s: %s" % (pid, why))
    for p in written:
        print(p)


if __name__ == "__main__":
    main()
