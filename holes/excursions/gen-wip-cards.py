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

usage: python3 gen-wip-cards.py [--out wip-cards.json] [--offline]
"""
import json, re, sys, os, subprocess, datetime

P4NG = os.environ.get("P4NG", "/home/joe/code/p4ng")
TICKETS = os.environ.get("TICKETS", "/home/joe/code/futon3c/holes/tickets")
LIBRARY = os.environ.get("LIBRARY", "/home/joe/code/futon3/library")


def why_path(wr):
    """The ruling this card compresses, as a path.

    A card carries `wr` as a bare label, which is a compression whose expansion
    -- the IF/HOWEVER/EXAMPLE/BECAUSE and the dated incident -- sits in the
    library and was not reachable from the card. A compression is legitimate
    when its expansion is reachable from where the compression is read
    (Joe, 2026-08-25), so emit the route back.
    """
    n = wr.split("-")[-1]
    d = os.path.join(LIBRARY, "war-room")
    hits = [f for f in sorted(os.listdir(d))
            if f.startswith("wr-" + n + "-") and f.endswith(".flexiarg")] \
        if os.path.isdir(d) else []
    return "futon3/library/war-room/" + hits[0] if hits else None


def how_path(node):
    """The @how pattern that would close this ring, found by its @holds-at."""
    d = os.path.join(LIBRARY, "problems")
    if not os.path.isdir(d):
        return None
    for f in sorted(os.listdir(d)):
        if not f.endswith(".flexiarg"):
            continue
        txt = open(os.path.join(d, f), encoding="utf-8").read()
        if re.search(r"^@holds-at\s+.*\b" + re.escape(node) + r"\b", txt, re.M):
            return "futon3/library/problems/" + f
    return None
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
            r'\{:node\s+"(R\d+)"\s+:wr\s+"([^"]+)"\s+:holds\s+(\w+)\s+:note\s+"([^"]*)"'
            r'(?:\s+:established\s+"([^"]+)")?', ov):
        if m.group(3) == "false":
            # A badge may carry its own :established. The file-level :as-of is
            # when the whole overlay was last swept; a ring revised after that
            # sweep is younger than the sweep, and dating it to the sweep would
            # make this layer -- whose job is the AGE of a mark -- lie about the
            # one field it exists to report (Joe, 2026-08-25).
            out.append({"node": m.group(1), "wr": m.group(2), "gap": m.group(4),
                        "established": m.group(5)})
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


AGENCY = os.environ.get("AGENCY", "http://localhost:7070")


def fetch(path, timeout=45):
    """A live read, or an honest null. Never a zero.

    The board's whole claim is that a count means something. A partial payload
    from the cascade graph -- which on 2026-08-25 returned patterns=0 on five
    of six requests -- would otherwise be written out as "0 tickets", which
    reads as an empty backlog rather than as a failed read. So the column
    carries `available: false` and the reason, and the renderer shows the
    column as unread rather than as empty.
    """
    import urllib.request
    try:
        with urllib.request.urlopen(AGENCY + path, timeout=timeout) as r:
            return json.load(r), None
    except Exception as e:
        return None, f"{type(e).__name__}: {e}"


def col(stage, source, count, items=None, available=True, note=None):
    return {"stage": stage, "source": source, "count": count,
            "items": items or [], "available": available, "note": note}


def age_days(iso, today):
    """Days since a mark was established -- the input to the fall-back rule.

    A card falls to a lower stage when it is neglected, so the stage a card is
    IN is not enough to render it; the board needs to know how long it has been
    there. Computed here rather than in the page so the number is in the
    artefact and can be checked against it.
    """
    if not iso:
        return None
    try:
        return (today - datetime.date.fromisoformat(iso[:10])).days
    except ValueError:
        return None


def build_board(cards, pile, offline=False):
    """The five columns of the control loop, each fed by its own source.

    Not a progress lane per card: the stages are populated from five different
    places, and a thing moves between them by being taken up, not by being
    ticked. PERCEIVE is everything the sweep has seen; BELIEVE is what someone
    curated into a ticket; EVALUATE is what has a ruling against it that does
    not hold; SELECT is what has a stated promotion test; ACT is what a session
    is actually clocked into right now.
    """
    today = datetime.date.today()

    if offline:
        perceive = col("PERCEIVE", "cascade-real/graph tickets.count-total",
                       None, available=False, note="offline: not fetched")
    else:
        g, err = fetch("/api/alpha/cascade-real/graph")
        # Fail closed on :section-status, not on the shape of :counts. The
        # endpoint has a 5s per-page deadline and marks a section :failed when
        # it blows -- HTTP 200, well-formed, honest in the payload. During the
        # 2026-08-25 mission-scope backfill 5 of 6 samples came back that way,
        # and a consumer reading only :counts saw zeros and could not tell them
        # from an empty backlog (futon1b/README-backlog-catchup.md §4).
        bad = sorted(k for k, v in ((g or {}).get("section-status") or {}).items()
                     if v.get("status") != "ok")
        t = (g or {}).get("tickets") or {}
        total = t.get("count-total")
        if err or bad or total is None:
            perceive = col("PERCEIVE", "cascade-real/graph tickets.count-total",
                           None, available=False,
                           note=err or (f"sections not ok: {', '.join(bad)}" if bad
                                        else "payload carried no tickets.count-total"))
        else:
            perceive = col("PERCEIVE", "cascade-real/graph tickets.count-total",
                           total, [i.get("stem") for i in t.get("items", [])[:12]])

    believe = col("BELIEVE", "holes/tickets/T-*.md", len(pile),
                  [t["id"] for t in pile[:12]])
    evaluate = col("EVALUATE", "wr-overlay.edn badges with :holds false",
                   len(cards), [c["id"] for c in cards])
    selected = [c["id"] for c in cards if c["promotion_test"]]
    select = col("SELECT", "card.promotion_test non-null", len(selected), selected,
                 note=None if selected else
                 "empty because no card states what would promote it, not because "
                 "nothing was chosen")

    if offline:
        act = col("ACT", "/api/alpha/agents with mission-id and a turn in flight",
                  None, available=False, note="offline: not fetched")
    else:
        a, err = fetch("/api/alpha/agents", timeout=15)
        if a is None:
            act = col("ACT", "/api/alpha/agents with mission-id and a turn in flight",
                      None, available=False, note=err)
        else:
            rows = list(a["agents"].values()) if isinstance(a.get("agents"), dict) \
                else (a.get("agents") or [])
            flight = [r for r in rows if r.get("invoke-started-at")]
            clocked = [r for r in flight if r.get("mission-id")]
            act = col("ACT", "/api/alpha/agents with mission-id and a turn in flight",
                      len(clocked), [r["id"]["id/value"] for r in clocked],
                      note=(f"{len(flight)} turns in flight, {len(clocked)} clocked into a "
                            f"mission -- the gap IS route A's coverage")
                      if len(flight) != len(clocked) else None)

    return {"stages": ["PERCEIVE", "BELIEVE", "EVALUATE", "SELECT", "ACT"],
            "columns": [perceive, believe, evaluate, select, act],
            "cards_enter_at": "EVALUATE",
            "fall_back_rule": None,
            "as_of": today.isoformat()}


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
            "established": r.get("established") or established,
            # The route from the compression back to what it stands for.
            "why_path": why_path(r["wr"]),
            "how_path": how_path(r["node"]),
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
            # Input to the fall-back rule: a card that sits at EVALUATE
            # untouched is meant to decay to a lower stage, so the board
            # needs the age of the mark and not only its stage.
            "age_days": None,
        })

    today = datetime.date.today()
    for c in cards:
        c["age_days"] = age_days(c["established"], today)

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
    doc["board"] = build_board(cards, doc["draw_pile"],
                               offline="--offline" in sys.argv)
    with open(out, "w") as f:
        json.dump(doc, f, indent=2)
    c = doc["counts"]
    for b in doc["board"]["columns"]:
        n = b["count"] if b["available"] else "--"
        print(f"  {b['stage']:<9}{str(n):>7}   {b['source']}")
        if b["note"]:
            print(f"           {'':>7}   ({b['note']})")
    print(f"  {out}: {c['cards']} cards ({c['with_supplier']} with a supplier, "
          f"{c['without_promotion_test']} with no promotion test, "
          f"{c['without_watu']} with no replay), "
          f"{c['draw_pile']} in the draw pile; overlay as-of {established}")


if __name__ == "__main__":
    main()
