#!/usr/bin/env python3
"""cascade_sexp.py — one turn's pattern cascade as an s-expression to read in Emacs.

Joe, 2026-11-11: "a textual presentation (not .edn please) that would allow
me to read these things nicely in Emacs ... s-exps with #n= markers would be
good for the joins".

A situation element used by more than one part is written once as
#N=(element NAME "quote") and afterwards as #N#.  Emacs's reader keeps that
sharing: `read' returns one object for every #N#, so a glue is two parts
that hold the same (eq) element.  An element used once gets no label.

  cascade_sexp.py <lab-dir> [--out FILE]

<lab-dir> holds occurrences.json (kimi-2's mapping format: pattern, maps
I1/I2/G/B each {element, source} or {element: null, reason}) and frame.json
(the turn).  Pattern part texts come from the futon3 library, read the way
occurrence_square.py reads them.
"""
import argparse
import json
import os
import re
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import occurrence_square as osq  # noqa: E402

# Pattern order of the parts, with the square role each is mapped from.
PARTS = (("IF", "I1"), ("HOWEVER", "I2"), ("THEN", "B"), ("BECAUSE", "G"))


def q(s):
    """An Emacs Lisp string literal."""
    return '"' + str(s).replace("\\", "\\\\").replace('"', '\\"') + '"'


def sym(s):
    """An Emacs Lisp symbol; characters the reader would misread are escaped."""
    return re.sub(r'([\s()\[\]"\';`,#?\\.])', r"\\\1", str(s)) if s else "nil"


def one_line(s, n=None):
    s = re.sub(r"\s+", " ", s or "").strip()
    return s if n is None or len(s) <= n else s[: n - 1] + "…"


def mapped(occ):
    return [p for p, r in PARTS if (occ["maps"].get(r) or {}).get("element")]


def render(occurrences, frame, part_text=None):
    """The s-expression text for one turn's cascade."""
    part_text = part_text or (lambda pid: osq.read_pattern(pid)["parts"])
    complete = [o for o in occurrences if len(mapped(o)) == 4]
    partial = sorted((o for o in occurrences if 0 < len(mapped(o)) < 4),
                     key=lambda o: -len(mapped(o)))
    void = [o for o in occurrences if not mapped(o)]

    uses = {}
    for o in complete + partial:
        for p, r in PARTS:
            e = (o["maps"].get(r) or {}).get("element")
            if e:
                uses.setdefault(e, []).append((o["pattern"], p))
    labels, seen = {}, set()
    for e, us in uses.items():
        if len(us) > 1:
            labels[e] = len(labels) + 1

    def element(m):
        e = m["element"]
        if e in labels and e in seen:
            return "#%d#" % labels[e]
        seen.add(e)
        body = "(element %s\n%s%s)" % (sym(e), " " * 12, q(one_line(m.get("source"))))
        return ("#%d=" % labels[e] + body) if e in labels else body

    def occurrence(o, head):
        parts = part_text(o["pattern"])
        lines = ["  (%s %s" % (head, sym(o["pattern"]))]
        if head == "partial":
            lines[0] += " %d/4" % len(mapped(o))
        for p, r in PARTS:
            m = o["maps"].get(r) or {}
            lines.append("    (%s %s" % (p, q(one_line(parts.get(p), 160))))
            if m.get("element"):
                lines.append("        " + element(m) + ")")
            else:
                lines.append("        (void %s))" % q(one_line(m.get("reason") or "no map")))
        lines[-1] += ")"
        return "\n".join(lines)

    turn = frame.get("turn") or {}
    out = [";;; -*- mode: lisp-data -*-",
           ";;; One turn read as a pattern cascade (CASCADE-SPEC-v3 §2-3).",
           ";;; #N=(element ...) names a situation element where it first appears;",
           ";;; #N# is the same element again.  Two complete occurrences that hold",
           ";;; the same element are glued there.",
           "(cascade",
           "  (situation (turn %s :at %s)" % (q(turn.get("evidence_id")), q(turn.get("at"))),
           "    %s)" % q(one_line(turn.get("text"), 240)),
           "  (reading :candidates %d :complete %d :partial %d :void %d)"
           % (len(occurrences), len(complete), len(partial), len(void))]
    out += [occurrence(o, "occurrence") for o in complete]
    glues = []
    for e, us in uses.items():
        whole = [u for u in us if u[0] in {o["pattern"] for o in complete}]
        if len(whole) > 1 and e in labels:
            glues.append("    (#%d# %s)" % (labels[e], " ".join(
                "(%s %s)" % (sym(pid), p) for pid, p in whole)))
    out.append("  (glues" + ("\n" + "\n".join(glues) if glues else "") + ")")
    out += [occurrence(o, "partial") for o in partial]
    out.append("  (void %s))" % " ".join(sym(o["pattern"]) for o in void))
    return "\n".join(out) + "\n"


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("lab_dir")
    ap.add_argument("--out")
    a = ap.parse_args(argv)
    with open(os.path.join(a.lab_dir, "occurrences.json"), encoding="utf-8") as f:
        occurrences = json.load(f)
    with open(os.path.join(a.lab_dir, "frame.json"), encoding="utf-8") as f:
        frame = json.load(f)
    frame = frame[0] if isinstance(frame, list) else frame
    text = render(occurrences, frame)
    if a.out:
        with open(a.out, "w", encoding="utf-8") as f:
            f.write(text)
    else:
        sys.stdout.write(text)


if __name__ == "__main__":
    main()
