#!/usr/bin/env python3
"""Flatten harvested A1 place rows into one TSV for scoring (claude-16 scores classical vs reader).

Writes futon3c/holes/excursions/ant-reads/A1-placements.tsv: one row per stem; value fields only,
plus seat, job, citation counts. Full rows with citations stay in ant-reads/<stem>.A1.json.
"""
import json, pathlib
HERE = pathlib.Path(__file__).resolve().parent.parent / "holes" / "excursions" / "ant-reads"
COLS = ["id", "what", "status", "wing", "way", "action", "need_goes_to", "finish", "beneficiary", "confidence", "seat", "job", "at", "ok", "unsure", "failed"]
def val(x):
    if not isinstance(x, dict): return str(x or "")
    return str(x.get("value") if "value" in x else ("unsure: " + str(x.get("unsure", ""))))
rows = []
for f in sorted(HERE.glob("*.A1.json")):
    d = json.loads(f.read_text()); r = d.get("row") or {}
    rows.append([d["stem"]] + [val(r.get(k)) for k in COLS[1:10]] + [d["seat"], d["job"], d["at"], str(d["ok"]), str(d["unsure"]), ",".join(d["failed"])])
out = HERE / "A1-placements.tsv"
out.write_text("\t".join(COLS) + "\n" + "".join("\t".join(c.replace("\t", " ").replace("\n", " ") for c in r) + "\n" for r in rows))
print(f"{len(rows)} placements -> {out}")
