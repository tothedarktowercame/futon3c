#!/usr/bin/env python3
"""Generate ant packets from the partition and the signals file.

Usage: ant-board.py KIND BATCH_SIZE OUT_DIR
Writes OUT_DIR/<KIND>-batch-<n>.md (COMMON + kind prompt + the items with paths) and prints
the batches. KIND in {A1, A2, A3, A4, A5, A6}; A1 reads open-unplaced.tsv, batch from $A1_BATCH (default 1). Reads futon3c/holes/excursions/giraffe-partition-2026-11-02.json
for the rows and ~/notes/commons-map/review-unblockers-2026-11-02-signals.tsv for paths.
"""
import json, os, pathlib, sys
HERE = pathlib.Path(__file__).resolve().parent.parent / "holes" / "excursions"
PART = json.loads((HERE / "giraffe-partition-2026-11-02.json").read_text())
SIG = [l.split("\t") for l in (pathlib.Path.home() / "notes/commons-map/review-unblockers-2026-11-02-signals.tsv").read_text().splitlines()]
PATH = {r[1]: r[9] for r in SIG[1:] if len(r) > 9}
SELECT = {
    "A2": lambda it: it["situation"] in ("artifact-landed", "closeable-unverified"),
    "A5": lambda it: (it.get("decision") or {}).get("decision") == "rehome",
    "A3": lambda it: (it.get("decision") or {}).get("decision") == "audit",
    "A4": lambda it: (it.get("decision") or {}).get("decision") == "close-as-prototype",
    "A6": lambda it: it["id"] == "M-superpod-mark2",
}
OPEN_UNPLACED = pathlib.Path.home() / "notes/pattern-global/auto/open-unplaced.tsv"  # claude-16, 2026-11-02; batch 1 first (Joe)
def open_unplaced(batch):
    rows = [l.split("\t") for l in OPEN_UNPLACED.read_text().splitlines()]
    return [{"id": r[1], "path": r[4], "stated-finish": None, "status": r[5] if len(r) > 5 else ""} for r in rows[1:] if r[0] == str(batch)]
PROMPT = {"A1": "A1-place.md", "A2": "A2-witness.md", "A5": "A5-rehome.md", "A3": "A3-audit-explain.md", "A4": "A4-lesson.md", "A6": "A6-acceptance-check.md"}
def canonical(path):
    """The signals file read futon6-old-copy for two rows; the live tree is futon6."""
    if path.startswith("futon6-old-copy/") and (pathlib.Path("/home/joe/code") / path.replace("futon6-old-copy/", "futon6/", 1)).exists():
        return path.replace("futon6-old-copy/", "futon6/", 1)
    return path
def main(kind, size, out):
    if kind == "A1":
        items = open_unplaced(os.environ.get("A1_BATCH", "1"))
        for it in items: PATH[it["id"]] = it["path"]
    else:
        items = [it for it in PART["items"] if SELECT[kind](it)]
    out = pathlib.Path(out); out.mkdir(parents=True, exist_ok=True)
    common = (HERE / "ant-prompts" / "COMMON.md").read_text(); kp = (HERE / "ant-prompts" / PROMPT[kind]).read_text()
    batches = [items[i:i + size] for i in range(0, len(items), size)]
    for n, b in enumerate(batches, 1):
        lines = [common, "", kp, "", f"## Items for this batch ({len(b)})", ""]
        for it in b:
            if kind == "A1":
                lines.append(f"- `{it['id']}` — file `/home/joe/code/{canonical(PATH.get(it['id'], '?'))}`" + (f" — status line as parsed (verify it): {it['status'][:80]}" if it.get("status") else ""))
            else:
                lines.append(f"- `{it['id']}` — file `/home/joe/code/{canonical(PATH.get(it["id"], "?"))}` — stated finish (claude-2's reading, verify it): {it.get('stated-finish') or '(none stated)'}")
        (out / f"{kind}-batch-{n}.md").write_text("\n".join(lines) + "\n")
        print(f"{kind}-batch-{n}: " + ", ".join(it["id"] for it in b))
if __name__ == "__main__":
    main(sys.argv[1], int(sys.argv[2]), sys.argv[3])
