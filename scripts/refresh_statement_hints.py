#!/usr/bin/env python3
"""Refresh :line and :statement-hint for rows whose Lean file has changed.

WHY THIS EXISTS (2026-07-30). `:statement-hint` and `:line` are snapshots taken
when the queue was censused. After the statement repairs of 2026-07-30 they were
STALE: the packet handed runners the pre-repair signature and pre-repair line
numbers. a95A08 was dispatched twice on a stale hint and both times the runner
correctly reported the OLD statement false -- wasting two runner slots and two
ground-control verification cycles on input we had already fixed.

Rows are matched by DECLARATION NAME, not by line number, because line numbers
move whenever anything is inserted. RENAMES maps the two a01A06 declarations
that the repair renamed.

Usage:  refresh_statement_hints.py [--commit] [row-id ...]
Default is a dry run printing a diff of what would change.
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

FUTON3C = Path(__file__).resolve().parent.parent
QUEUE = FUTON3C / "data/codex-sorry-queue.edn"
APM = Path("/home/joe/code/apm-lean")

# Declarations renamed by the 2026-07-30 repairs: old hint name -> current name.
RENAMES = {
    "distribution_exponential_decay": "distribution_polynomial_decay",
    "orlicz_bound_implies_L2": "orlicz_bound_not_implies_L2",
}

# In a hint the declaration follows "Line N: ", so it is NOT at line start.
HINT_DECL_RE = re.compile(r"(?:theorem|lemma)\s+(\w+)")


def decl_names(hint: str) -> list[str]:
    """Declaration names mentioned in an existing statement hint, in order."""
    seen, out = set(), []
    for m in HINT_DECL_RE.finditer(hint):
        if m.group(1) not in seen:
            seen.add(m.group(1))
            out.append(m.group(1))
    return out


def signature_and_sorry(src: str, name: str) -> tuple[str, int] | None:
    """Return (signature text, 1-indexed line of its first sorry) for `name`."""
    m = re.search(
        r"^(?:private\s+)?(?:noncomputable\s+)?(?:theorem|lemma)\s+" + re.escape(name) + r"\b",
        src,
        re.M,
    )
    if not m:
        return None
    tail = src[m.start():]
    # signature runs to the := that opens the proof
    body = re.search(r":=\s*(?:by\b)?", tail)
    if not body:
        return None
    sig = tail[: body.start()].strip()
    sig = re.sub(r"\s*\n\s*", " ", sig)          # one line, as the census stored it
    # Search from an ABSOLUTE position in src. Relative-offset arithmetic across
    # three nested slices was off by one for some declarations and not others.
    sig_end = m.start() + body.end()
    s = re.compile(r"^[ \t]*sorry\b", re.M).search(src, sig_end)
    if not s:
        return None
    return sig, src[: s.start()].count("\n") + 1


def iter_rows(text: str):
    """Yield (start, end) of each top-level {...} row, STRING-AWARE.

    Separator heuristics do not work here: a statement hint can contain Lean
    binder syntax like `{g : ℝ → ℝ} {A : ℝ}`, i.e. a literal "} {" INSIDE a
    string. Depth counting must therefore skip string contents and escapes.
    """
    depth = 0
    start = None
    in_str = False
    esc = False
    for i, ch in enumerate(text):
        if in_str:
            if esc:
                esc = False
            elif ch == "\\":
                esc = True
            elif ch == '"':
                in_str = False
            continue
        if ch == '"':
            in_str = True
        elif ch == "{":
            if depth == 0:
                start = i
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0 and start is not None:
                yield start, i + 1
                start = None


def row_span(text: str, anchor_file: str) -> tuple[int, int]:
    """Span of the row whose :file is anchor_file."""
    needle = ':file "%s"' % anchor_file
    hits = [(lo, hi) for lo, hi in iter_rows(text) if needle in text[lo:hi]]
    if not hits:
        raise KeyError(anchor_file)
    assert len(hits) == 1, "%d rows match %s" % (len(hits), anchor_file)
    lo, hi = hits[0]
    row = text[lo:hi]
    assert row.count(':id "') == 1, "span covers %d ids" % row.count(':id "')
    return lo, hi


def edn_str(s: str) -> str:
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n") + '"'


def main() -> int:
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    commit = "--commit" in sys.argv
    text = QUEUE.read_text(encoding="utf-8")

    from edn_format import loads, Keyword as K

    rows = loads(text)
    targets = [
        r for r in rows
        if (r.get(K("repaired-at")) is not None)
        and (not args or str(r.get(K("id"))) in args)
    ]
    print(f"rows to refresh: {len(targets)}\n")

    changed = 0
    for r in targets:
        rid = str(r.get(K("id")))
        f = str(r.get(K("file")))
        src = (APM / f).read_text(encoding="utf-8")
        old_hint = str(r.get(K("statement-hint")))
        names = [RENAMES.get(n, n) for n in decl_names(old_hint)]
        found = [(n, signature_and_sorry(src, n)) for n in names]
        # A declaration with no sorry has been PROVED since the census, so it
        # must be DROPPED from the hint rather than causing the row to be
        # skipped. Before 2026-07-30 this skipped a01A04 entirely after a
        # partial closed two of its three targets, leaving the packet pointing
        # runners at work already done.
        proved = [n for n, v in found if v is None]
        found = [(n, v) for n, v in found if v is not None]
        if proved:
            print(f"  {rid[14:-15]:10s} dropping {len(proved)} PROVED decl(s) from hint: {proved}")
        if not found:
            print(f"  {rid[14:-15]:10s} SKIP - no sorried declaration remains")
            continue
        parts, lines = [], []
        for n, (sig, line) in found:
            parts.append(f"Line {line}: {sig}")
            lines.append(line)
        new_hint = "\n\n".join(parts)
        if new_hint == old_hint and list(r.get(K("line"))) == lines:
            print(f"  {rid[14:-15]:10s} already current")
            continue
        changed += 1
        print(f"  {rid[14:-15]:10s} line {list(r.get(K('line')))} -> {lines}")
        for n, (sig, _) in found:
            if n not in old_hint:
                print(f"      renamed decl now present: {n}")
        if commit:
            lo, hi = row_span(text, f)
            row = text[lo:hi]
            row = re.sub(r":line \[[^\]]*\]", ":line [%s]" % " ".join(map(str, lines)), row, count=1)
            row = re.sub(r':statement-hint "(?:[^"\\]|\\.)*"',
                         ":statement-hint " + edn_str(new_hint), row, count=1)
            text = text[:lo] + row + text[hi:]

    if commit and changed:
        loads(text)  # parse guard before writing
        QUEUE.write_text(text, encoding="utf-8")
        print(f"\nwrote {changed} refreshed row(s)")
    elif not commit:
        print("\n(dry run - pass --commit to write)")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
