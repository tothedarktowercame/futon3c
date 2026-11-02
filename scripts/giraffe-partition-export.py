#!/usr/bin/env python3
"""Export the Giraffe partition over the review sheet as data for Cascade Live.

Usage: giraffe-partition-export.py SIGNALS_TSV SHEET_MD OUT_JSON

Reads claude-2's signals file (rank, id, status, action, unblocks, phase,
substantive-commits-since-oct, last-substantive-commit, stated-finish, path,
wing, situation) and the review sheet's cone and decision columns, and writes
one row per item with what it still needs before the War Machine's outer loop
could select it. Free-text evidence is not exported. Readings are advisory;
nothing here is a decision.
"""
import json, pathlib, re, sys, datetime

NEEDS = {  # situation -> what the outermost loop still has to do
    "place-gone":           ("retire-or-rehome", "the Place no longer exists (port, cron or client gone); retire, or name the live substrate"),
    "awaiting-ruling":      ("joe-rules", "held on an act of Joe's: spend, verbatims, or a design choice"),
    "no-consumer":          ("retire-or-merge", "nothing downstream benefits; retire or merge"),
    "unread":               ("read", "no reading yet; read it first"),
    "artifact-landed":      ("close-with-witness", "the Product exists; attach the witness and close, carrying the residue"),
    "closeable-unverified": ("verify-then-close", "a close reading with no artifact named yet"),
    "held-other":           ("read", "held for a reason that needs a per-row reading"),
    "merge":                ("merge", "merge into the named successor"),
    "work":                 ("who-acts", "a work reading; decide whether an agent or Joe acts"),
    "unplaced":             ("place", "no placement at all"),
}

def main(sig, sheet, out):
    rows = [l.split("\t") for l in pathlib.Path(sig).read_text().splitlines()]
    hdr, rows = rows[0], rows[1:]
    cone, decision = {}, {}
    for l in pathlib.Path(sheet).read_text().splitlines():
        if not l.startswith("| ") or l.startswith("| rank") : continue
        c = [x.strip() for x in l.strip("|").split("|")]
        if len(c) >= 9 and c[1]: cone[c[1]] = c[3]; decision[c[1]] = c[8]
    items = []
    for r in rows:
        d = dict(zip(hdr, r))
        sit = d.get("situation", "?")
        need, why = NEEDS.get(sit, ("read", "unclassified"))
        selectable = (d["action"] == "work" and bool(d["stated-finish"]) and sit == "work")
        items.append({
            "rank": int(d["rank"]), "id": d["id"], "kind": {"M": "mission", "E": "excursion", "T": "ticket"}.get(d["id"][0], "doc"),
            "repo": (re.match(r"(futon[0-9a-z]*)", d["path"] or "") or [None, None])[1],
            "wing": d.get("wing") or "none", "situation": sit, "action": d["action"],
            "phase": d["phase"], "cone": int(cone.get(d["id"], "0") or 0),
            "unblocks": [u.strip() for u in d["unblocks"].split(",") if u.strip()],
            "commits-since-oct": int(d["substantive-commits-since-oct"] or 0),
            "last-commit": d["last-substantive-commit"] or None,
            "stated-finish": (d["stated-finish"] or "")[:160] or None,
            "decision-question": decision.get(d["id"]),
            "needs": need, "needs-why": why, "selectable-now": selectable,
        })
    items.sort(key=lambda x: x["rank"])
    by_sit, by_wing, by_need = {}, {}, {}
    for it in items:
        by_sit[it["situation"]] = by_sit.get(it["situation"], 0) + 1
        by_wing[it["wing"]] = by_wing.get(it["wing"], 0) + 1
        by_need[it["needs"]] = by_need.get(it["needs"], 0) + 1
    pathlib.Path(out).write_text(json.dumps({
        "schema": "giraffe-partition-v1",
        "source": "Giraffe run 20261102T013012.768Z-26ad19e6; review sheet and signals 2026-11-02 (claude-16 readings, claude-2 signals); advisory, no decision applied",
        "exported-at": datetime.datetime.now(datetime.timezone.utc).replace(microsecond=0).isoformat(),
        "loop": "outermost: prepares what the War Machine's outer loop may select",
        "count": len(items), "selectable-now": sum(1 for i in items if i["selectable-now"]),
        "by-situation": by_sit, "by-wing": by_wing, "by-needs": by_need,
        "items": items}, indent=1, ensure_ascii=False) + "\n")
    print(f"{len(items)} rows; selectable now {sum(1 for i in items if i['selectable-now'])}; needs {by_need}")

if __name__ == "__main__":
    main(*sys.argv[1:4])
