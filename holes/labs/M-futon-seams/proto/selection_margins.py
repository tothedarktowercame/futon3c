#!/usr/bin/env python3
"""selection_margins.py — regenerate proto/selection-margins.edn.

Asks, for each cascade slot, whether the library could have been searched
into the pattern that was chosen. Each slot is queried twice: once with the
move stated in the MISSION's vocabulary, once in the PATTERN's own. The
difference between those two ranks is the measurement.

  python3 holes/labs/M-futon-seams/proto/selection_margins.py > /tmp/out.edn

Queries live in SLOTS below and are written into the output verbatim, so a
reader can judge whether they were leading rather than taking the result on
trust.
"""
import json, re, subprocess, sys

SLOTS = [
 ("cascade-construction/choose-the-grain-where-state-lives",
  "does the state a dispatch decision changes belong to the provider, the seat, or the role",
  "choose the grain where the cascade's state naturally lives"),
 ("coordination/assignment-binding",
  "make the assignment of a worker to a duty an explicit record rather than an inference",
  "assignment binding: bind the assignment as a record"),
 ("cycle-machine/single-producer",
  "exactly one place may write this state and everything else reads it",
  "single producer: one writer for the state, all others read"),
 ("or3/count-every-card-back",
  "enumerate every site that hardcodes the thing before changing any of them",
  "count every card back before you act on the deck"),
 ("gauntlet/placenta-transfer",
  "move callers from the old way to the new one without a flag day",
  "placenta transfer: carry the caller across to the new source"),
 ("translation/test-by-reproducing-behaviour",
  "test an indirection by rebinding it and confirming behaviour follows",
  "test by reproducing behaviour known from elsewhere"),
 ("realtime/mode-gate",
  "retire the old path once the new one is proven",
  "mode gate: gate the old mode off once the new one holds"),
]


def hits(query):
    out = subprocess.run(["timeout", "120", "python3", "scripts/xlate.py", "find", query],
                         capture_output=True, text=True).stdout
    return [(float(m.group(1)), m.group(2)) for m in
            (re.match(r"\s+([\d.]+)\s+(\S+)\s*$", l) for l in out.splitlines()) if m]


def rank_of(hs, pat):
    for i, (s, p) in enumerate(hs, 1):
        if p == pat:
            return i, s
    return None, None


def main():
    found_p = 0
    for pat, qp, qn in SLOTS:
        hp, hn = hits(qp), hits(qn)
        rp, _ = rank_of(hp, pat)
        rn, _ = rank_of(hn, pat)
        found_p += rp is not None
        print(f"{pat:56s} problem {str(rp):5s} name {rn}", file=sys.stderr)
    print(f"\n{found_p} of {len(SLOTS)} reachable from the problem statement", file=sys.stderr)


if __name__ == "__main__":
    main()
