#!/usr/bin/env python3
"""reanchor.py — re-point an author's annotations after the mission text moves.

  reanchor.py --author claude-1 [--apply]

An anchor whose quote is no longer at its offsets is FLAGGED by the page
generator, never moved by it. This moves them, deliberately and with a report.

An anchor also records the HEADING it fell under. A quote can be unchanged
and unique and still have moved between sections -- from DERIVE to ARGUE, say
-- and a note silently re-pointed under a different phase misleads whoever
reads it there. So a move that would change the note's heading is reported
and left for a decision, exactly as a changed quote is.

`--author X` moves only X's notes. `--author all` moves anyone's, which is
safe for the case it is meant for: an edit earlier in the mission shifts every
later offset by a constant, and re-pointing an UNCHANGED, UNIQUE quote cannot
change what a note says — it is provably the same text at a new address. What
is never moved is a note whose quote has changed, or occurs more than once;
those need a decision, and are reported instead.
"""
import argparse, hashlib, json, os, re, subprocess, sys

import offset_unit

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from mission_anchors import heading_for

MISSION = "holes/missions/M-futon-seams.md"
ANN = "holes/labs/M-futon-seams/annotations.edn"


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--author", required=True)
    ap.add_argument("--apply", action="store_true")
    a = ap.parse_args()

    text = open(MISSION, encoding="utf-8").read()
    sha = hashlib.sha256(text.encode()).hexdigest()
    src = open(ANN, encoding="utf-8").read()
    notes = json.loads(subprocess.run(
        ["bb", "-e",
         '(require (quote [clojure.edn :as edn]) (quote [cheshire.core :as j]))'
         f'(print (j/generate-string (edn/read-string (slurp "{ANN}"))))'],
        capture_output=True, text=True).stdout)

    # These are character offsets. Refuse to move spans whose unit is
    # unstated: a consumer reading bytes gets the wrong span and no error,
    # and an outside reader already drew the wrong conclusion from them.
    undeclared = [n["id"] for n in notes
                  if offset_unit.complaint(n["anchor"].get("offset-unit"),
                                           n["id"]) is not None]
    if undeclared:
        sys.exit(f"annotations.edn: {len(undeclared)} anchor(s) do not declare "
                 f":offset-unit :{offset_unit.UNIT} — "
                 f"{', '.join(undeclared[:3])}"
                 f"{'…' if len(undeclared) > 3 else ''}")

    moved = skipped = 0
    moves = []
    for n in notes:
        anc = n["anchor"]
        if text[anc["start"]:anc["end"]] == anc["quote"]:
            continue
        if a.author != "all" and n.get("author") != a.author:
            print(f"  LEAVE {n['id']} — {n.get('author')}'s note, not mine to move")
            skipped += 1
            continue
        hits = [m.start() for m in re.finditer(re.escape(anc["quote"]), text)]
        if len(hits) != 1:
            print(f"  LEAVE {n['id']} — quote occurs {len(hits)} times; needs a decision")
            skipped += 1
            continue
        s = hits[0]
        e = s + len(anc["quote"])
        was, now = anc.get("heading"), heading_for(text, s)
        if was is not None and now != was:
            print(f"  LEAVE {n['id']} — heading changed, needs a decision:")
            print(f"           was  {was}")
            print(f"           now  {now}")
            skipped += 1
            continue
        print(f"  MOVE  {n['id']} {anc['start']}..{anc['end']} -> {s}..{e}")
        moves.append((anc["start"], anc["end"], s, e))
        moved += 1

    # Two passes. A uniform shift makes one note's NEW offsets equal another's
    # OLD ones, so a sequence of direct replacements rewrites the wrong entry --
    # it did, silently, and left seven notes stale that the run had just
    # reported as moved. Sentinels first, then the values.
    for i, (a0, b0, _, _) in enumerate(moves):
        src = src.replace(f":start {a0} :end {b0}", f":start \x00{i}\x00 :end \x01{i}\x01", 1)
    for i, (_, _, a1, b1) in enumerate(moves):
        src = src.replace(f":start \x00{i}\x00 :end \x01{i}\x01", f":start {a1} :end {b1}", 1)
    src = re.sub(r':mission-sha "[0-9a-f]{64}"', f':mission-sha "{sha}"', src)
    if a.apply:
        open(ANN, "w", encoding="utf-8").write(src)
        print(f"applied: {moved} moved, {skipped} left")
        # Verify rather than assume. An earlier version reported 27 moved and
        # left four of them stale anyway; a re-read is the only thing that
        # would have caught that, and it costs nothing.
        again = json.loads(subprocess.run(
            ["bb", "-e",
             '(require (quote [clojure.edn :as edn]) (quote [cheshire.core :as j]))'
             f'(print (j/generate-string (edn/read-string (slurp "{ANN}"))))'],
            capture_output=True, text=True).stdout)
        still = [n["id"] for n in again
                 if text[n["anchor"]["start"]:n["anchor"]["end"]] != n["anchor"]["quote"]
                 and (a.author == "all" or n.get("author") == a.author)]
        if still:
            print(f"  NOT FIXED, run again: {', '.join(still)}")
            sys.exit(1)
    else:
        print(f"dry run: {moved} would move, {skipped} left (pass --apply)")


if __name__ == "__main__":
    main()
