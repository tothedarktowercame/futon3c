#!/usr/bin/env python3
"""COMPOSE/comb projection adapter for M-typed-holes D1.

This is intentionally only an adapter: it maps BV-comb `:composes` rows into
`fill.TypedHole` + filler pairs, then delegates discharge to the single runtime
`fill.fill(..., kind=fill.KIND_COMPOSE)`.
"""
from __future__ import annotations

import importlib.util
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any


REPO = Path("/home/joe/code/futon3c")
BV_TYPING = Path("/home/joe/code/futon6/holes/bv-comb-typing.edn")
MANIFEST = Path("/home/joe/code/storage/arse/manifest.json")
REPORT = REPO / "scripts" / "adapters" / "compose-report.md"
AUTHOR = "compose-adapter"
TAGS = ["m-typed-holes", "compose-adapter", "bv-combs"]
MAX_ARSE_WRITES = 10


@dataclass(frozen=True)
class ComposeEdge:
    mission: str
    from_node: str
    to_node: str
    verdict: str
    shape: str

    @property
    def hole_id(self) -> str:
        return f"compose:{self.mission}:{self.from_node}->{self.to_node}"

    @property
    def boundary(self) -> str:
        if self.verdict == ":typed-monotone":
            return f"bv/seq-boundary:{self.mission}:{self.from_node}->{self.to_node}"
        return "?"

    @property
    def filler(self) -> str | None:
        if self.verdict != ":typed-monotone":
            return None
        return f"sub-process:{self.mission}:{self.from_node}->{self.to_node}"


def load_fill_module():
    path = REPO / "scripts" / "fill.py"
    spec = importlib.util.spec_from_file_location("fill", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not load {path}")
    module = importlib.util.module_from_spec(spec)
    sys.modules["fill"] = module
    spec.loader.exec_module(module)
    return module


def read_manifest_count() -> int | None:
    if not MANIFEST.exists():
        return None
    data = json.loads(MANIFEST.read_text())
    count = data.get("entity_count")
    return int(count) if isinstance(count, int) else None


def parse_bv_typing(path: Path = BV_TYPING) -> list[ComposeEdge]:
    text = path.read_text()
    missions: list[ComposeEdge] = []
    for block in re.finditer(
        r'\{:mission\s+"(?P<mission>[^"]+)".*?'
        r":shape\s+(?P<shape>:[\w-]+).*?"
        r":composes-typing\s+\[(?P<edges>.*?)\]\s+"
        r":endpoints-comb",
        text,
        re.S,
    ):
        mission = block.group("mission")
        shape = block.group("shape")
        edges = block.group("edges")
        for edge in re.finditer(r"\[(:[\w-]+)\s+(:[\w-]+)\s+(:[\w-]+)\]", edges):
            missions.append(
                ComposeEdge(
                    mission=mission,
                    from_node=edge.group(1),
                    to_node=edge.group(2),
                    verdict=edge.group(3),
                    shape=shape,
                )
            )
    if not missions:
        raise RuntimeError(f"no :composes rows parsed from {path}")
    return missions


def select_edges(edges: list[ComposeEdge]) -> list[ComposeEdge]:
    """Pick a deterministic bounded slice: every typed filler plus one open gap."""
    filled = [edge for edge in edges if edge.filler is not None]
    open_edges = [edge for edge in edges if edge.filler is None]
    selected = filled[:4] + open_edges[:1]
    arse_writes = 2 * len([edge for edge in selected if edge.filler is not None])
    if arse_writes > MAX_ARSE_WRITES:
        raise RuntimeError(f"hard bound exceeded: {arse_writes} ArSE writes")
    if not selected:
        raise RuntimeError("no compose holes selected")
    return selected


def fill_edge(fillmod: Any, edge: ComposeEdge) -> Any:
    hole = fillmod.TypedHole(
        id=edge.hole_id,
        hungry_for=edge.boundary,
        projection=fillmod.KIND_COMPOSE,
        source={
            "bv_typing": str(BV_TYPING),
            "mission": edge.mission,
            "shape": edge.shape,
            "edge": [edge.from_node, edge.to_node, edge.verdict],
        },
    )
    filler = edge.filler
    note = (
        "COMPOSE/comb projection adapter. The BV typing shape is a linear "
        f"`:bv/seq` chain; this row is `{edge.from_node} -> {edge.to_node}` "
        f"with verdict `{edge.verdict}`. A `:typed-monotone` verdict is treated "
        "as the present sub-process filler; a gap verdict is recorded open."
    )
    return fillmod.fill(
        hole,
        filler,
        kind=fillmod.KIND_COMPOSE,
        filler_type=edge.boundary if filler is not None else None,
        enforce_type=filler is not None,
        note=note,
        author=AUTHOR,
        tags=TAGS,
    )


def render_report(rows: list[dict[str, Any]], before: int | None, after: int | None) -> str:
    filled_count = sum(1 for row in rows if row["filled"])
    lines = [
        "# compose adapter report",
        "",
        "Runner: `scripts/adapters/compose_adapter.py`",
        "",
        "Input shape:",
        "",
        f"- BV comb typing path: `{BV_TYPING}`",
        "- Shape used: `:typed-missions` entries with `:shape :linear-chain`, "
        "`:bv-type {:bv/seq [...]}`, and `:composes-typing` rows of "
        "`[:from :to :typed-monotone|:gap-no-boundary-type]`.",
        "- Mapping: each `:composes` row is a COMPOSE hole. `:typed-monotone` "
        "rows get a `sub-process:<mission>:<from>-><to>` filler with matching "
        "`bv/seq-boundary`; gap rows are recorded OPEN with `filler=None`.",
        "",
        f"- ArSE manifest before entity_count: {before if before is not None else 'unavailable'}",
        f"- ArSE manifest after entity_count: {after if after is not None else 'unavailable'}",
        f"- selected compose holes: {len(rows)}",
        f"- filled compose holes posted: {filled_count}",
        f"- total ArSE writes: {filled_count * 2} ({filled_count} asks + {filled_count} answers)",
        "",
        "| hole | verdict | filler | thread-id | question evidence | answer evidence |",
        "|---|---|---|---|---|---|",
    ]
    for row in rows:
        filler = f"`{row['filler']}`" if row["filler"] else "OPEN"
        thread = f"`{row['thread_id']}`" if row["thread_id"] else "-"
        qev = f"`{row['question_evidence']}`" if row["question_evidence"] else "-"
        aev = f"`{row['answer_evidence']}`" if row["answer_evidence"] else "-"
        lines.append(
            f"| `{row['hole_id']}` | `{row['verdict']}` | {filler} | "
            f"{thread} | {qev} | {aev} |"
        )
    lines.extend(
        [
            "",
            "Verification:",
            "",
            "- Imports `scripts/fill.py` and uses `fill.fill` with `kind=fill.KIND_COMPOSE`.",
            "- Open COMPOSE holes return `FillRecord(filled=False)` and do not write ArSE, preserving I2 without a fake witness.",
            "- Filled COMPOSE holes enforce the adapter boundary string as `filler_type` before discharge.",
            "",
        ]
    )
    return "\n".join(lines)


def main() -> int:
    fillmod = load_fill_module()
    edges = parse_bv_typing()
    selected = select_edges(edges)

    before = read_manifest_count()
    rows: list[dict[str, Any]] = []
    for edge in selected:
        record = fill_edge(fillmod, edge)
        rows.append(
            {
                "hole_id": edge.hole_id,
                "verdict": edge.verdict,
                "filler": edge.filler,
                "filled": record.filled,
                "thread_id": record.thread_id,
                "question_evidence": record.question_evidence,
                "answer_evidence": record.answer_evidence,
            }
        )
    after = read_manifest_count()

    filled_count = sum(1 for row in rows if row["filled"])
    if before is not None and after is not None and after - before != filled_count:
        raise RuntimeError(
            f"manifest entity_count delta {after - before} != filled holes {filled_count}"
        )

    REPORT.write_text(render_report(rows, before, after))
    print(f"wrote {REPORT}")
    for row in rows:
        status = row["thread_id"] or "OPEN"
        print(f"{status} {row['hole_id']} {row['verdict']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
