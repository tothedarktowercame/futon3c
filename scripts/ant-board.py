#!/usr/bin/env python3
"""Generate ant packets from the partition and the signals file.

Usage: ant-board.py KIND BATCH_SIZE OUT_DIR
Writes OUT_DIR/<KIND>-batch-<n>.md (COMMON + kind prompt + the items with paths) and prints
the batches. KIND in {A2, A5}. Reads futon3c/holes/excursions/giraffe-partition-2026-11-02.json
for the rows and ~/notes/commons-map/review-unblockers-2026-11-02-signals.tsv for paths.
"""
import json, pathlib, sys
HERE = pathlib.Path(__file__).resolve().parent.parent / "holes" / "excursions"
PART = json.loads((HERE / "giraffe-partition-2026-11-02.json").read_text())
SIG = [l.split("\t") for l in (pathlib.Path.home() / "notes/commons-map/review-unblockers-2026-11-02-signals.tsv").read_text().splitlines()]
PATH = {r[1]: r[9] for r in SIG[1:] if len(r) > 9}
SELECT = {
    "A2": lambda it: it["situation"] in ("artifact-landed", "closeable-unverified"),
    "A5": lambda it: (it.get("decision") or {}).get("decision") == "rehome",
}
PROMPT = {"A2": "A2-witness.md", "A5": "A5-rehome.md"}
def main(kind, size, out):
    items = [it for it in PART["items"] if SELECT[kind](it)]
    out = pathlib.Path(out); out.mkdir(parents=True, exist_ok=True)
    common = (HERE / "ant-prompts" / "COMMON.md").read_text(); kp = (HERE / "ant-prompts" / PROMPT[kind]).read_text()
    batches = [items[i:i + size] for i in range(0, len(items), size)]
    for n, b in enumerate(batches, 1):
        lines = [common, "", kp, "", f"## Items for this batch ({len(b)})", ""]
        for it in b:
            lines.append(f"- `{it['id']}` — file `/home/joe/code/{PATH.get(it["id"], "?")}` — stated finish (claude-2's reading, verify it): {it.get('stated-finish') or '(none stated)'}")
        (out / f"{kind}-batch-{n}.md").write_text("\n".join(lines) + "\n")
        print(f"{kind}-batch-{n}: " + ", ".join(it["id"] for it in b))
if __name__ == "__main__":
    main(sys.argv[1], int(sys.argv[2]), sys.argv[3])
