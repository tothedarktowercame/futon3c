#!/usr/bin/env python3
"""Regenerate LEMMA-INDEX.md — the helper lemmas a closer can reuse.

The index is what the closer packet tells every hop to grep before re-deriving
anything, so a stale index quietly costs hops: it was last built 2026-08-07 and
by the following afternoon the packet's own counts (16 modules, 1095 lemmas)
had both drifted.

Two deliberate choices:

* A problem's OWN statement is excluded. The index advertises reusable
  machinery; listing `apm_t97j01` would invite a closer to "reuse" the very
  theorem it is being asked to prove.
* A lemma counts as proved when its own proof body carries no `sorry`, which is
  per-DECLARATION, not per-file. A partial artifact routinely contains fully
  proved helpers, and those are exactly the ones worth reusing — indexing only
  zero-sorry files would have hidden them.

Both tests are lexical. That matches how the index is consumed (a human or agent
greps it and then reads the source), and the axiom sweep is what actually
certifies anything.
"""
from __future__ import annotations

import re
import subprocess
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import gates

REPO = Path("/home/joe/code/apm-lean")
OUT = REPO / "LEMMA-INDEX.md"

# Importable libraries, in the form `import <name>.<Module>`.
LIB_DIRS = ("ConstructionTargets", "YoungL2")

TOP_RE = re.compile(
    r"^(?:@\[|private\s|protected\s|noncomputable\s|theorem\s|lemma\s|def\s|abbrev\s"
    r"|structure\s|inductive\s|instance\s|class\s|namespace\s|end\b|open\s|section\b"
    r"|variable\s|universe\s|import\s|attribute\s|#)")


def claims(source: str) -> list[tuple[str, str, bool]]:
    """(name, signature, proved) for each theorem/lemma, by lexical scan."""

    stripped = gates.strip_comments(source)
    lines = stripped.splitlines()
    tops = [i for i, line in enumerate(lines) if TOP_RE.match(line)]
    out: list[tuple[str, str, bool]] = []
    for index, line in enumerate(lines):
        match = gates.CLAIM_RE.match(line)
        if not match:
            continue
        end = next((t for t in tops if t > index), len(lines))
        block = "\n".join(lines[index:end])
        signature = " ".join(block.split(":=")[0].split())
        signature = signature[len(match.group(0)):].strip() or "(no arguments)"
        proved = not gates.SORRY_RE.search(block)
        out.append((match.group(1), signature, proved))
    return out


def main() -> int:
    rows: list[tuple[str, str, str]] = []
    for lib in LIB_DIRS:
        for path in sorted((REPO / lib).glob("*.lean")):
            module = f"{lib}.{path.stem}"
            for name, signature, proved in claims(path.read_text(encoding="utf-8")):
                if proved:
                    rows.append((name, f"LIB: {module}", signature))
    for path in sorted(REPO.glob("problems/*/lean/Main.lean")):
        pid = path.parent.parent.name
        # Most files name the statement `apm_<pid>`, but a96J02 names its own
        # `a96J02` — so match the bare id too, or the index advertises the very
        # theorem a closer is being asked to prove.
        own = {f"apm_{pid}".lower(), pid.lower()}
        for name, signature, proved in claims(path.read_text(encoding="utf-8")):
            if proved and name.lower() not in own:
                rows.append((name, pid, signature))

    rows.sort(key=lambda r: (r[0].lower(), r[1]))
    lib_count = sum(1 for r in rows if r[1].startswith("LIB:"))
    modules = len({r[1] for r in rows if r[1].startswith("LIB:")})

    body = [
        "# Helper-lemma index (generated — do not hand-edit)",
        "",
        f"{len(rows)} lemmas already proved in this repo, outside the problems'",
        "own statements. GREP THIS BEFORE RE-DERIVING ANYTHING.",
        "",
        f"`LIB:` rows are importable today ({lib_count} lemmas across {modules} modules,",
        "e.g. `import ConstructionTargets.Rouche`). The rest live inside one problem",
        "file: read the proof there and reuse the argument, or ask for the lemma to be",
        "promoted into ConstructionTargets.",
        "",
        "A lemma is listed when its own proof body has no `sorry`, so a problem that is",
        "still open can still contribute helpers here.",
        "",
        "| lemma | where | signature |",
        "|---|---|---|",
    ]
    for name, where, signature in rows:
        cell = signature.replace("|", "\\|")
        body.append(f"| `{name}` | {where} | `{cell}` |")
    OUT.write_text("\n".join(body) + "\n", encoding="utf-8")
    print(f"wrote {OUT}: {len(rows)} lemmas ({lib_count} importable, {modules} modules)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
