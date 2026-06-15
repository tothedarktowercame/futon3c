#!/usr/bin/env python3
"""DISCHARGE/proof projection adapter for M-typed-holes D1.

This adapter maps futon2 `sorry` registry entries onto the single runtime
`fill.fill()` operator. It does not perform discharge itself:

- a sorry is a proof-hole; its `TypedHole.hungry_for` is the proposition owed;
- a discharged sorry supplies a proof/term reference as the filler;
- an open sorry calls `fill(..., filler=None)`, recording openness locally
  without an ArSE write, per fill.py's I2 behavior.

Live writes require `--apply`. A default run is a deterministic dry-run so
reviewers do not accidentally mint duplicate ArSE witness pairs.
"""
from __future__ import annotations

import argparse
import importlib.util
import json
import sys
import urllib.request
from dataclasses import asdict
from pathlib import Path
from typing import Any

import edn_format


ROOT = Path("/home/joe/code/futon3c")
SCRIPT_DIR = ROOT / "scripts"
FILL_PATH = SCRIPT_DIR / "fill.py"
DEFAULT_SORRYS = Path("/home/joe/code/futon2/resources/sorrys.edn")
REPORT = SCRIPT_DIR / "adapters" / "discharge-report.md"
MANIFEST = Path("/home/joe/code/storage/arse/manifest.json")
ARSE_BASE = "http://localhost:7070/api/alpha/arse"
AUTHOR = "discharge-adapter"
TAGS = ["m-typed-holes", "projection:discharge", "sorry-registry"]
K = edn_format.Keyword


def load_fill():
    spec = importlib.util.spec_from_file_location("fill", FILL_PATH)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load {FILL_PATH}")
    module = importlib.util.module_from_spec(spec)
    sys.modules["fill"] = module
    spec.loader.exec_module(module)
    return module


def kname(value: Any) -> str:
    text = str(value)
    return text[1:] if text.startswith(":") else text


def edn_text(value: Any) -> str:
    """Normalize EDN values, including `(str ...)` tuples, to compact text."""
    if value is None:
        return ""
    if isinstance(value, str):
        return value
    if isinstance(value, (list, tuple)):
        parts = list(value)
        if parts and str(parts[0]) == "str":
            parts = parts[1:]
        return "".join(edn_text(part) for part in parts)
    if isinstance(value, edn_format.Keyword):
        return kname(value)
    return str(value)


def get(row: Any, name: str, default: Any = None) -> Any:
    return row.get(K(name), default)


def read_manifest_count() -> int | None:
    if not MANIFEST.exists():
        return None
    data = json.loads(MANIFEST.read_text())
    count = data.get("entity_count")
    return int(count) if isinstance(count, int) else None


def http_json(method: str, path: str) -> dict[str, Any]:
    req = urllib.request.Request(f"{ARSE_BASE}{path}", method=method)
    with urllib.request.urlopen(req, timeout=20) as res:
        return json.loads(res.read().decode("utf-8"))


def unanswered_thread_ids() -> set[str]:
    data = http_json("GET", "/unanswered")
    out = set()
    for q in data.get("questions", []):
        if isinstance(q, dict):
            tid = q.get("thread-id") or q.get("thread_id") or q.get("id")
            if isinstance(tid, str):
                out.add(tid)
    return out


def load_sorrys(path: Path) -> tuple[dict[str, Any], list[Any]]:
    doc = edn_format.loads(path.read_text())
    schema = {
        "schema-version": doc.get(K("schema-version")),
        "schema-note": (
            "v2-in-comments over :schema-version 1; optional :kind; "
            ":status distinguishes :open from discharged/addressed states"
        ),
        "path": str(path),
    }
    return schema, list(doc.get(K("sorrys")) or [])


def proof_ref(row: Any) -> str | None:
    """Return a deterministic proof/term reference for discharged sorries."""
    bits: list[str] = []
    for key in [
        "resolution",
        "resolved-by-cg",
        "resolved-by-cg-chain",
        "resolved-by-pilot",
        "addressed-by-excursion",
        "addressed-by-cg-chain",
        "addressed-by-pilot",
        "has-excursion",
        "decomposed-by-cg",
    ]:
        value = get(row, key)
        if value:
            bits.append(f"{key}={edn_text(value)}")
    return " | ".join(bits) if bits else None


def hole_from_sorry(fillmod: Any, row: Any, registry_path: Path):
    sid = kname(get(row, "id"))
    title = edn_text(get(row, "title"))
    status = kname(get(row, "status"))
    kind = kname(get(row, "kind")) if get(row, "kind") else ""
    return fillmod.TypedHole(
        id=sid,
        hungry_for=title,
        projection=fillmod.KIND_DISCHARGE,
        source={
            "registry": str(registry_path),
            "schema": "sorrys.edn v2-in-comments",
            "status": status,
            "kind": kind,
            "raised_at": edn_text(get(row, "raised-at")),
        },
    )


def select_sorries(rows: list[Any], *, n_filled: int, n_open: int) -> list[Any]:
    def sid(row: Any) -> str:
        return kname(get(row, "id"))

    discharged = [
        row for row in rows
        if kname(get(row, "status")) != "open" and proof_ref(row)
    ]
    open_rows = [row for row in rows if kname(get(row, "status")) == "open"]
    return sorted(discharged, key=sid)[:n_filled] + sorted(open_rows, key=sid)[:n_open]


def note_for(row: Any, filler: str | None, registry_path: Path) -> str:
    status = kname(get(row, "status"))
    kind = kname(get(row, "kind")) if get(row, "kind") else "unspecified"
    lines = [
        "DISCHARGE/proof adapter witness.",
        "",
        f"sorry registry: `{registry_path}`",
        "schema used: `:schema-version 1` with v2 comment schema adding optional `:kind`.",
        f"sorry status: `{status}`",
        f"sorry kind: `{kind}`",
        "",
        f"owed proposition: {edn_text(get(row, 'title'))}",
    ]
    rationale = edn_text(get(row, "rationale"))
    if rationale:
        lines += ["", "Registry rationale excerpt:", rationale[:900]]
    if filler:
        lines += ["", "Proof/term reference used as filler:", filler[:1200]]
    else:
        lines += ["", "No proof/term reference is present because this sorry is still open."]
    return "\n".join(lines)


def render_report(rows: list[dict[str, Any]], schema: dict[str, Any], before: int | None, after: int | None,
                  apply: bool) -> str:
    filled_count = sum(1 for row in rows if row["filled"])
    lines = [
        "# DISCHARGE adapter report",
        "",
        "Adapter: `scripts/adapters/discharge_adapter.py`",
        "Single runtime fill: `scripts/fill.py` via `fill.fill(..., kind=fill.KIND_DISCHARGE)`",
        "",
        f"- mode: {'APPLY/live ArSE writes' if apply else 'dry-run'}",
        f"- sorry registry: `{schema['path']}`",
        f"- schema: `:schema-version {schema['schema-version']}`; {schema['schema-note']}",
        f"- selected sorries: {len(rows)}",
        f"- filled/discharged sorries posted: {filled_count}",
        f"- ArSE manifest before entity_count: {before if before is not None else 'unavailable'}",
        f"- ArSE manifest after entity_count: {after if after is not None else 'unavailable'}",
        "",
        "| sorry | status | filler | filled | thread-id | question evidence | answer evidence |",
        "|---|---|---|---:|---|---|---|",
    ]
    for row in rows:
        filler = row["filler"] or "OPEN"
        if len(filler) > 90:
            filler = filler[:87] + "..."
        lines.append(
            f"| `{row['sorry_id']}` | `{row['status']}` | {filler} | "
            f"{str(row['filled']).lower()} | `{row['thread_id'] or ''}` | "
            f"`{row['question_evidence'] or ''}` | `{row['answer_evidence'] or ''}` |"
        )
    lines.extend(
        [
            "",
            "Verification:",
            "",
            "- The adapter imports `fill.py`; it does not issue ArSE ask/answer calls directly.",
            "- Open sorries route through `fill(..., filler=None)` and produce no ArSE witness.",
            "- Live run checks `entity_count` delta equals the number of filled records.",
            "- Live run checks posted threads are not left unanswered.",
            "",
        ]
    )
    return "\n".join(lines)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--sorrys", type=Path, default=DEFAULT_SORRYS)
    ap.add_argument("--filled", type=int, default=3)
    ap.add_argument("--open", type=int, default=2)
    ap.add_argument("--apply", action="store_true", help="perform live ArSE writes for filled sorries")
    args = ap.parse_args()

    if args.filled < 0 or args.open < 0:
        raise SystemExit("--filled and --open must be nonnegative")
    if args.filled * 2 > 10:
        raise SystemExit("hard bound exceeded: filled sorries would perform more than 10 ArSE writes")

    fillmod = load_fill()
    schema, all_sorrys = load_sorrys(args.sorrys)
    chosen = select_sorries(all_sorrys, n_filled=args.filled, n_open=args.open)
    before = read_manifest_count()
    out_rows: list[dict[str, Any]] = []

    for row in chosen:
        sid = kname(get(row, "id"))
        status = kname(get(row, "status"))
        filler = proof_ref(row) if status != "open" else None
        hole = hole_from_sorry(fillmod, row, args.sorrys)
        if args.apply:
            record = fillmod.fill(
                hole,
                filler,
                kind=fillmod.KIND_DISCHARGE,
                filler_type=hole.hungry_for if filler else None,
                enforce_type=bool(filler),
                note=note_for(row, filler, args.sorrys),
                author=AUTHOR,
                tags=TAGS,
            )
            rec = asdict(record)
        else:
            rec = {
                "filled": bool(filler),
                "thread_id": None,
                "question_evidence": None,
                "answer_evidence": None,
            }
        out_rows.append(
            {
                "sorry_id": sid,
                "status": status,
                "filler": filler,
                "filled": bool(rec["filled"]),
                "thread_id": rec.get("thread_id"),
                "question_evidence": rec.get("question_evidence"),
                "answer_evidence": rec.get("answer_evidence"),
            }
        )

    after = read_manifest_count()
    if args.apply:
        filled_count = sum(1 for row in out_rows if row["filled"])
        if before is not None and after is not None and after - before != filled_count:
            raise RuntimeError(
                f"manifest entity_count delta {after - before} != filled records {filled_count}"
            )
        missing = {row["thread_id"] for row in out_rows if row["thread_id"]} & unanswered_thread_ids()
        if missing:
            raise RuntimeError(f"posted threads still unanswered: {sorted(missing)}")

    REPORT.write_text(render_report(out_rows, schema, before, after, args.apply))
    print(f"wrote {REPORT}")
    for row in out_rows:
        state = "FILLED" if row["filled"] else "OPEN"
        print(f"{state} {row['sorry_id']} thread={row['thread_id'] or ''}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
