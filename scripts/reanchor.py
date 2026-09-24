#!/usr/bin/env python3
"""reanchor.py — re-point an author's annotations after the mission text moves.

  reanchor.py --author claude-1 [--apply]

An anchor whose quote is no longer at its offsets is FLAGGED by the page
generator, never moved by it. This moves them, deliberately and with a
report, and only for the author named: another agent's notes are that
agent's to re-point. A quote that no longer occurs, or occurs more than
once, is left alone and reported — it needs a human decision, not an offset.
"""
import argparse, hashlib, json, re, subprocess, sys

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

    moved = skipped = 0
    for n in notes:
        anc = n["anchor"]
        if text[anc["start"]:anc["end"]] == anc["quote"]:
            continue
        if n.get("author") != a.author:
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
        print(f"  MOVE  {n['id']} {anc['start']}..{anc['end']} -> {s}..{e}")
        src = src.replace(f":start {anc['start']} :end {anc['end']}", f":start {s} :end {e}", 1)
        moved += 1

    src = re.sub(r':mission-sha "[0-9a-f]{64}"', f':mission-sha "{sha}"', src)
    if a.apply:
        open(ANN, "w", encoding="utf-8").write(src)
        print(f"applied: {moved} moved, {skipped} left")
    else:
        print(f"dry run: {moved} would move, {skipped} left (pass --apply)")


if __name__ == "__main__":
    main()
