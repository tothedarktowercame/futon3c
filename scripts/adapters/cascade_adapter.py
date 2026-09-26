#!/usr/bin/env python3
"""CASCADE-FEED mining adapter for M-typed-holes D1.

This adapter is deliberately thin: it projects mined mission-triples satiety
nodes into the single runtime `fill.fill()` operator.  It does not implement its
own ArSE discharge path.
"""

from __future__ import annotations

import importlib.util
import json
import re
import sys
from dataclasses import dataclass
from pathlib import Path
from typing import Any


ROOT = Path("/home/joe/code/futon3c")
TRIPLES_DIR = Path("/home/joe/code/futon6/data/mission-triples")
MANIFEST = Path("/home/joe/code/storage/arse/manifest.json")
REPORT = ROOT / "scripts" / "adapters" / "cascade-report.md"
AUTHOR = "cascade-adapter"
TAGS = ["m-typed-holes", "cascade-feed", "mission-triples"]
MAX_FILLED = 3
MAX_OPEN = 2


@dataclass(frozen=True)
class Pattern:
    node_id: str
    form: str
    ref: str


@dataclass(frozen=True)
class Candidate:
    mission: str
    file: Path
    source: str | None
    hole_id: str | None
    node_id: str
    hungry_for: str
    problem_form: str
    patterns: tuple[Pattern, ...]

    @property
    def adapter_hole_id(self) -> str:
        if self.hole_id:
            return f"{self.mission}:{self.hole_id}:cascade-problem"
        return f"{self.mission}:{self.node_id}:cascade-problem"

    @property
    def filled_by_cascade(self) -> bool:
        return bool(self.patterns)


def load_fill_module():
    path = ROOT / "scripts" / "fill.py"
    spec = importlib.util.spec_from_file_location("fill", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"could not import {path}")
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


def _first(pattern: str, text: str) -> str | None:
    match = re.search(pattern, text, flags=re.S)
    return match.group(1) if match else None


def _unescape_edn_string(value: str) -> str:
    # The generated mission-triples strings are simple JSON-compatible escapes.
    return json.loads(f'"{value}"')


def parse_candidate(path: Path) -> Candidate | None:
    text = path.read_text(encoding="utf-8")
    mission = _first(r':mission\s+"([^"]+)"', text) or path.stem
    source = _first(r':source\s+"([^"]+)"', text)
    hole_id = _first(r":hole\s+\{.*?:id\s+\"([^\"]+)\"", text)

    problem_match = re.search(
        r":id\s+:problem\b(?P<body>.*?)(?=\n\s+\{\s*:id\s+:pattern-|\n\s+\]\n|\n\s+:hyperedges)",
        text,
        flags=re.S,
    )
    if not problem_match:
        return None
    body = problem_match.group("body")
    hungry = _first(r":hungry-for\s+:(parse|payoff|canon|bundling|role)", body)
    if not hungry:
        return None
    form_raw = _first(r':form\s+"((?:\\"|[^"])*)"', body) or ""
    problem_form = _unescape_edn_string(form_raw) if form_raw else ""

    cascade_text = text.split("\n :wiring", 1)[0]
    patterns: list[Pattern] = []
    for match in re.finditer(
        r":id\s+:(pattern-\d+)\b(?P<body>.*?)(?=\n\s+\{\s*:id\s+:pattern-|\n\s+\]\n\s+:hyperedges|\n\s+:hyperedges)",
        cascade_text,
        flags=re.S,
    ):
        pbody = match.group("body")
        if not re.search(r":satiety\s+:full\b", pbody):
            continue
        form = _first(r':form\s+"((?:\\"|[^"])*)"', pbody)
        ref = _first(r':ref\s+"((?:\\"|[^"])*)"', pbody)
        if form and ref:
            patterns.append(
                Pattern(
                    node_id=match.group(1),
                    form=_unescape_edn_string(form),
                    ref=_unescape_edn_string(ref),
                )
            )

    return Candidate(
        mission=mission,
        file=path,
        source=source,
        hole_id=hole_id,
        node_id="problem",
        hungry_for=hungry,
        problem_form=problem_form,
        patterns=tuple(patterns),
    )


def load_candidates() -> list[Candidate]:
    candidates = []
    for path in sorted(TRIPLES_DIR.glob("*.edn")):
        candidate = parse_candidate(path)
        if candidate is not None:
            candidates.append(candidate)
    return candidates


def choose_rows(candidates: list[Candidate]) -> list[Candidate]:
    filled = [c for c in candidates if c.filled_by_cascade][:MAX_FILLED]
    open_rows = [c for c in candidates if not c.filled_by_cascade][:MAX_OPEN]
    rows = filled + open_rows
    if len(rows) > 5:
        raise RuntimeError("deterministic selection exceeded 5 nodes")
    if len(filled) * 2 > 10:
        raise RuntimeError("hard bound exceeded: more than 10 ArSE writes")
    return rows


def filler_for(candidate: Candidate) -> dict[str, Any] | None:
    if not candidate.patterns:
        return None
    return {
        "type": candidate.hungry_for,
        "cascade_output": "pattern-chain",
        "terminal": {
            "node": candidate.patterns[-1].node_id,
            "form": candidate.patterns[-1].form,
            "ref": candidate.patterns[-1].ref,
        },
        "patterns": [
            {"node": p.node_id, "form": p.form, "ref": p.ref}
            for p in candidate.patterns
        ],
    }


def note_for(candidate: Candidate, filler: dict[str, Any] | None) -> str:
    lines = [
        "CASCADE-FEED/mining projection adapter.",
        "",
        f"mission: `{candidate.mission}`",
        f"mission-triples: `{candidate.file}`",
        f"source mission file: `{candidate.source or 'unknown'}`",
        f"problem node: `{candidate.node_id}`",
        f"satiety hungry-for: `:{candidate.hungry_for}`",
        f"problem form: `{candidate.problem_form}`",
    ]
    if filler is None:
        lines += [
            "",
            "No cascade pattern-cites were mined for this problem node; recorded as OPEN per I2.",
        ]
    else:
        pattern_lines = "\n".join(
            f"- `{p['node']}` `{p['form']}` -> `{p['ref']}`"
            for p in filler["patterns"]
        )
        lines += [
            "",
            "Cascade filler: ordered pattern chain mined from `:cascade :nodes`.",
            pattern_lines,
        ]
    return "\n".join(lines)


def render_report(rows: list[dict[str, Any]], before: int | None, after: int | None) -> str:
    filled = [r for r in rows if r["filled"]]
    open_rows = [r for r in rows if not r["filled"]]
    delta = None if before is None or after is None else after - before
    lines = [
        "# cascade adapter report",
        "",
        "Runner: `scripts/adapters/cascade_adapter.py`",
        "",
        "Invariant check:",
        "",
        "- Imports and calls `scripts/fill.py`; no local ArSE ask/answer implementation.",
        "- Uses `kind=fill.KIND_CASCADE_FEED` for every call.",
        "- Calls `fill(..., None)` for open mined holes, recording I2 honestly with no ArSE write.",
        "- Deterministic selection: first 3 cascade-fed problem nodes, then first 2 unfed problem nodes, sorted by mission-triples filename.",
        "",
        f"- ArSE manifest before entity_count: {before if before is not None else 'unavailable'}",
        f"- ArSE manifest after entity_count: {after if after is not None else 'unavailable'}",
        f"- observed entity_count delta: {delta if delta is not None else 'unavailable'}",
        f"- filled holes posted: {len(filled)}",
        f"- open holes recorded locally: {len(open_rows)}",
        f"- ArSE writes expected from filled holes: {len(filled) * 2} (ask + answer per `fill.fill()`)",
        "",
        "| status | hole | hungry_for | filler / OPEN | thread-id | question evidence | answer evidence |",
        "|---|---|---|---|---|---|---|",
    ]
    for row in rows:
        if row["filled"]:
            terminal = row["filler"]["terminal"]
            filler_text = f"`{terminal['form']}` (`{terminal['ref']}`)"
        else:
            filler_text = "OPEN"
        lines.append(
            f"| {'FILLED' if row['filled'] else 'OPEN'} | `{row['hole_id']}` | "
            f"`:{row['hungry_for']}` | {filler_text} | "
            f"`{row['thread_id'] or ''}` | `{row['question_evidence'] or ''}` | "
            f"`{row['answer_evidence'] or ''}` |"
        )
    lines.append("")
    return "\n".join(lines)


def main() -> int:
    fillmod = load_fill_module()
    candidates = load_candidates()
    selected = choose_rows(candidates)
    before = read_manifest_count()
    rows: list[dict[str, Any]] = []

    for candidate in selected:
        filler = filler_for(candidate)
        hole = fillmod.TypedHole(
            id=candidate.adapter_hole_id,
            hungry_for=f":{candidate.hungry_for}",
            projection=fillmod.KIND_CASCADE_FEED,
            source={
                "mission": candidate.mission,
                "mission_triples": str(candidate.file),
                "source": candidate.source,
                "node": candidate.node_id,
                "problem_form": candidate.problem_form,
            },
        )
        record = fillmod.fill(
            hole,
            filler,
            kind=fillmod.KIND_CASCADE_FEED,
            filler_type=f":{candidate.hungry_for}" if filler is not None else None,
            note=note_for(candidate, filler),
            author=AUTHOR,
            tags=TAGS,
            enforce_type=filler is not None,
        )
        rows.append(
            {
                "hole_id": record.hole_id,
                "hungry_for": candidate.hungry_for,
                "filled": record.filled,
                "filler": filler,
                "thread_id": record.thread_id,
                "question_evidence": record.question_evidence,
                "answer_evidence": record.answer_evidence,
                "open_note": record.open_note,
            }
        )

    after = read_manifest_count()
    REPORT.write_text(render_report(rows, before, after), encoding="utf-8")
    print(f"wrote {REPORT}")
    print(f"filled={sum(1 for r in rows if r['filled'])} open={sum(1 for r in rows if not r['filled'])}")
    print(f"threads={[r['thread_id'] for r in rows if r['thread_id']]}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
