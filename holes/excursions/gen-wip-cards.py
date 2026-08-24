#!/usr/bin/env python3
"""Generate the WIP / Uxbridge layer for pipeline-pattern-cascade-live.html.

Zero hand-typed rows, matching the discipline the host page already claims: the
cards are derived from the paper's two typed sources of truth (WR-8), and the
draw pile is read from the ticket directory. Nothing here is authored prose.

  wr-overlay.edn    which control-loop nodes have a WR ruling that does NOT hold
                    -> the red rings -> one card each
  cascade-map.edn   which cascade mission would supply that mechanism
                    -> the blue ring -> the card's supplier
  holes/tickets/    the draw pile

The Uxbridge part is the two fields a status board usually omits: every mark
carries the DATE it was established, so a card can be shown as visibly stale
rather than merely shown; and `promotion-test` records what call would move the
card to the next register. A card with promotion-test null is not "not done" --
it is unmeasurable, which is a different and more actionable defect. `watu` is
the matching null on the other side: nobody has reconstructed what happened.
Those two numbers are what this layer exists to surface.

Registers: plan -> built -> ran -> live. Promotion to `live` requires the
artefact to produce its effect AND a matched null control to fail (the m3 rule).

usage: python3 gen-wip-cards.py [--out wip-cards.json]
"""
import json, re, sys, os, subprocess, datetime

P4NG = os.environ.get("P4NG", "/home/joe/code/p4ng")
TICKETS = os.environ.get("TICKETS", "/home/joe/code/futon3c/holes/tickets")
OVERLAY = f"{P4NG}/empirics-futon/wr-overlay.edn"
CASCADE = f"{P4NG}/empirics-futon/cascade-map.edn"


def read(p):
    with open(p) as f:
        return f.read()


def as_of(txt, default=None):
    m = re.search(r':as-of\s+"([^"]+)"', txt)
    return m.group(1) if m else default


def red_rings(ov):
    """Badges whose governing WR ruling does not currently hold."""
    out = []
    for m in re.finditer(
            r'\{:node\s+"(R\d+)"\s+:wr\s+"([^"]+)"\s+:holds\s+(\w+)\s+:note\s+"([^"]*)"', ov):
        if m.group(3) == "false":
            out.append({"node": m.group(1), "wr": m.group(2), "gap": m.group(4)})
    return out


def blue_rings(cm):
    return {m.group(2): {"box": m.group(1), "pattern": m.group(3), "why": m.group(4)}
            for m in re.finditer(
                r'\{:box\s+"(\w+)"\s+:pairs-with\s+"(R\d+)"\s+:pattern\s+"([^"]+)"\s+:why\s+"([^"]*)"', cm)}


def boxes(cm):
    flat = re.sub(r'\s+', ' ', cm)
    lab = dict(re.findall(r':id "(\w+)" :label "([^"]*)"', flat))
    mis = dict(re.findall(r':id "(\w+)" :label "[^"]*" :mission "([^"]*)"', flat))
    return lab, mis


def draw_pile(d):
    """The tickets, with their git-recorded age. Reference, never cache."""
    pile = []
    for fn in sorted(os.listdir(d)):
        if not (fn.startswith("T-") and fn.endswith(".md")):
            continue
        path = os.path.join(d, fn)
        first = read(path).split("\n", 1)[0].lstrip("# ").strip()
        try:
            last = subprocess.run(["git", "log", "-1", "--format=%as", "--", fn],
                                  cwd=d, capture_output=True, text=True,
                                  timeout=10).stdout.strip() or None
        except Exception:
            last = None
        # A repo path, rendered as TEXT rather than a link. This board is read
        # over the web and holes/tickets/ is not published, so any href here --
        # page-relative or repo-relative -- is a dead link. Nineteen dead links
        # are worse than nineteen pointers, because they promise a click that
        # does not work.
        pile.append({"id": fn[:-3], "title": first, "repo_path": f"holes/tickets/{fn}",
                     "last_touched": last})
    return pile


def main():
    out = sys.argv[sys.argv.index("--out") + 1] if "--out" in sys.argv else "wip-cards.json"
    ov, cm = read(OVERLAY), read(CASCADE)
    reds, blues = red_rings(ov), blue_rings(cm)
    lab, mis = boxes(cm)

    # Assert before trusting the extraction: a silently-empty regex here would
    # publish a board that under-reports suppliers, which is worse than no board.
    assert reds, "no red rings extracted from wr-overlay.edn"
    assert blues, "no blue rings extracted from cascade-map.edn"
    for b in blues.values():
        assert b["box"] in mis, f"blue ring {b['box']} has no mission in cascade-map.edn"

    established = as_of(ov)
    cards = []
    for r in reds:
        b = blues.get(r["node"])
        cards.append({
            "id": f"C-{r['node']}",
            "node": r["node"],
            "wr": r["wr"],
            "gap": r["gap"],
            "supplier": None if not b else {
                "box": b["box"], "label": lab.get(b["box"]),
                "mission": mis.get(b["box"]), "why": b["why"]},
            # No red ring has a mechanism behind it by definition, so all four
            # sit at `plan`. Recorded as data rather than assumed by the renderer.
            "register": "plan",
            "established": established,
            # Honest null: no card has a defined promotion test yet. This is the
            # field that makes the colour clock mean something -- without it,
            # "how old is this mark" has no answer, because nothing says what
            # re-establishing it would involve.
            "promotion_test": None,
            # Second honest null, and a different absence from the first.
            # promotion_test says nobody has stated what SHIPPED would look
            # like; watu says nobody has reconstructed what HAPPENED. A card can
            # lack either independently, and a board that reports only open work
            # gives good news less discipline than bad -- which is the very
            # ruling (WR-25) that put C-R5 on this list.
            "watu": None,
            "wip": False,
        })

    doc = {
        "generated_from": {"overlay": OVERLAY, "cascade": CASCADE, "tickets": TICKETS},
        "overlay_as_of": established,
        "registers": ["plan", "built", "ran", "live"],
        "promotion_rule": "produced its effect AND a matched null control fails",
        "cards": cards,
        "draw_pile": draw_pile(TICKETS),
        "counts": {"cards": len(cards),
                   "with_supplier": sum(1 for c in cards if c["supplier"]),
                   "without_promotion_test": sum(1 for c in cards if not c["promotion_test"]),
                   "without_watu": sum(1 for c in cards if not c["watu"]),
                   "draw_pile": None},
    }
    doc["counts"]["draw_pile"] = len(doc["draw_pile"])
    with open(out, "w") as f:
        json.dump(doc, f, indent=2)
    c = doc["counts"]
    print(f"  {out}: {c['cards']} cards ({c['with_supplier']} with a supplier, "
          f"{c['without_promotion_test']} with no promotion test, "
          f"{c['without_watu']} with no replay), "
          f"{c['draw_pile']} in the draw pile; overlay as-of {established}")


if __name__ == "__main__":
    main()
