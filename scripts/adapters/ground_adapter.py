#!/usr/bin/env python3
"""GROUND/symbol projection adapter for M-typed-holes D1.

This adapter does only projection-specific translation:

    substrate-2a :bind edge -> fill.TypedHole(hungry_for="concept")

The discharge itself is routed through scripts/fill.py with KIND_GROUND.  Open
`:concept :?` edges are passed as filler=None so I2 is recorded without an ArSE
write; filled edges create the witnessed ArSE ask->answer pair inside fill.fill.
"""
from __future__ import annotations

import importlib.util
import json
import sys
import urllib.request
from dataclasses import dataclass
from pathlib import Path
from typing import Any


PAPER = "0704.0502"
STORE = Path(f"/home/joe/code/futon6/data/substrate-2a/hx/{PAPER}.edn")
MANIFEST = Path("/home/joe/code/storage/arse/manifest.json")
REPORT = Path("/home/joe/code/futon3c/scripts/adapters/ground-report.md")
ARSE_BASE = "http://localhost:7070/api/alpha/arse"
AUTHOR = "ground-adapter"
TAGS = ("m-typed-holes", "substrate-2a", "ground")


@dataclass(frozen=True)
class GroundingCandidate:
    edge_index: int
    subject: str
    concept: str | None

    @property
    def hole_id(self) -> str:
        return f"ground:{PAPER}:edge-{self.edge_index}:{self.subject}"

    @property
    def filled(self) -> bool:
        return self.concept is not None


def _load_script(name: str, path: Path):
    spec = importlib.util.spec_from_file_location(name, path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def load_scopequery():
    scripts = Path(__file__).resolve().parents[1]
    return _load_script("scope_query_dogfood", scripts / "scope_query_dogfood.py")


def load_fill():
    scripts = Path(__file__).resolve().parents[1]
    return _load_script("fill", scripts / "fill.py")


def read_manifest_count() -> int | None:
    if not MANIFEST.exists():
        return None
    data = json.loads(MANIFEST.read_text())
    count = data.get("entity_count")
    return int(count) if isinstance(count, int) else None


def unanswered_thread_ids() -> set[str]:
    req = urllib.request.Request(f"{ARSE_BASE}/unanswered", method="GET")
    with urllib.request.urlopen(req, timeout=20) as res:
        data = json.loads(res.read().decode("utf-8"))
    out = set()
    for q in data.get("questions", []):
        if isinstance(q, dict):
            tid = q.get("thread-id") or q.get("thread_id") or q.get("id")
            if isinstance(tid, str):
                out.add(tid)
    return out


def pick_candidates(scopequery) -> list[GroundingCandidate]:
    store = scopequery.load_store(STORE)
    grounded: list[GroundingCandidate] = []
    open_: list[GroundingCandidate] = []
    seen_subjects: set[str] = set()

    for edge_index, (kind, ends) in enumerate(store, start=1):
        if kind != "bind":
            continue
        subjects = [node for role, node in ends if role == "subject"]
        concepts = [node for role, node in ends if role == "concept"]
        if not subjects or not concepts:
            continue
        subject, concept = subjects[0], concepts[0]
        if subject in seen_subjects:
            continue
        seen_subjects.add(subject)
        candidate = GroundingCandidate(
            edge_index=edge_index,
            subject=subject,
            concept=None if concept == "?" else concept,
        )
        if candidate.filled and len(grounded) < 4:
            grounded.append(candidate)
        elif not candidate.filled and not open_:
            open_.append(candidate)
        if len(grounded) == 4 and open_:
            return grounded + open_

    raise RuntimeError("could not find 4 grounded candidates and 1 open candidate")


def fill_candidate(fillmod, candidate: GroundingCandidate):
    hole = fillmod.TypedHole(
        id=candidate.hole_id,
        hungry_for="concept",
        projection=fillmod.KIND_GROUND,
        source={
            "paper": PAPER,
            "store": str(STORE),
            "edge-index": candidate.edge_index,
            "subject": candidate.subject,
        },
    )
    note = (
        f"GROUND adapter witness: symbol `{candidate.subject}` in `{PAPER}` "
        f"grounds to concept `{candidate.concept}`."
        if candidate.filled
        else f"GROUND adapter I2 record: symbol `{candidate.subject}` in `{PAPER}` remains open."
    )
    return fillmod.fill(
        hole,
        candidate.concept,
        kind=fillmod.KIND_GROUND,
        filler_type="concept" if candidate.filled else None,
        note=note,
        author=AUTHOR,
        tags=TAGS,
        enforce_type=True,
    )


def render_report(rows: list[dict[str, Any]], before: int | None, after: int | None) -> str:
    lines = [
        "# ground adapter report",
        "",
        "Runner: `scripts/adapters/ground_adapter.py`",
        "",
        f"- paper: `{PAPER}`",
        f"- store: `{STORE}`",
        f"- ArSE manifest before entity_count: {before if before is not None else 'unavailable'}",
        f"- ArSE manifest after entity_count: {after if after is not None else 'unavailable'}",
        f"- filled holes posted: {sum(1 for row in rows if row['filled'])}",
        f"- open holes recorded locally: {sum(1 for row in rows if not row['filled'])}",
        "",
        "| subject | hole-id | filler | filled | thread-id | question evidence | answer evidence |",
        "|---|---|---|---|---|---|---|",
    ]
    for row in rows:
        lines.append(
            f"| `{row['subject']}` | `{row['hole_id']}` | "
            f"{'`' + row['filler'] + '`' if row['filler'] else 'OPEN'} | "
            f"{'yes' if row['filled'] else 'no'} | "
            f"{'`' + row['thread_id'] + '`' if row['thread_id'] else ''} | "
            f"{'`' + row['question_evidence'] + '`' if row['question_evidence'] else ''} | "
            f"{'`' + row['answer_evidence'] + '`' if row['answer_evidence'] else ''} |"
        )
    lines.extend(
        [
            "",
            "Verification:",
            "",
            "- Adapter imports `fill.py` and calls `fill.fill(..., kind=fill.KIND_GROUND)`.",
            "- Open `:concept :?` is passed as `filler=None`; no ArSE write is made for the open hole.",
            "- Every filled thread was absent from `/api/alpha/arse/unanswered` after the run.",
            "",
        ]
    )
    return "\n".join(lines)


def main() -> int:
    scopequery = load_scopequery()
    fillmod = load_fill()
    candidates = pick_candidates(scopequery)
    if len([c for c in candidates if c.filled]) * 2 > 10:
        raise RuntimeError("hard ArSE write bound exceeded")

    before = read_manifest_count()
    rows: list[dict[str, Any]] = []

    for candidate in candidates:
        record = fill_candidate(fillmod, candidate)
        rows.append(
            {
                "subject": candidate.subject,
                "hole_id": record.hole_id,
                "filler": record.filler or "",
                "filled": record.filled,
                "thread_id": record.thread_id or "",
                "question_evidence": record.question_evidence or "",
                "answer_evidence": record.answer_evidence or "",
            }
        )

    unanswered = unanswered_thread_ids()
    posted = {row["thread_id"] for row in rows if row["thread_id"]}
    still_open = posted & unanswered
    if still_open:
        raise RuntimeError(f"filled threads still unanswered: {sorted(still_open)}")

    after = read_manifest_count()
    filled_count = sum(1 for row in rows if row["filled"])
    if before is not None and after is not None and after - before != filled_count:
        raise RuntimeError(
            f"manifest entity_count delta {after - before} != filled holes {filled_count}"
        )

    REPORT.write_text(render_report(rows, before, after))
    print(f"wrote {REPORT}")
    for row in rows:
        print(row["hole_id"], row["filler"] or "OPEN", row["thread_id"] or "OPEN")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
