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


def strip_lean_comments(src: str) -> str:
    """Blank out `--` line comments and `/- -/` block comments, preserving offsets.

    Replacing with spaces (and keeping newlines) means every index and line
    number computed on the result is still valid for the original source.
    """
    out = list(src)
    i, n = 0, len(src)
    while i < n:
        if src.startswith("/-", i):
            depth, j = 1, i + 2          # Lean block comments nest
            while j < n and depth:
                if src.startswith("/-", j):
                    depth += 1
                    j += 2
                elif src.startswith("-/", j):
                    depth -= 1
                    j += 2
                else:
                    j += 1
            for k in range(i, min(j, n)):
                if out[k] != "\n":
                    out[k] = " "
            i = j
        elif src.startswith("--", i):
            j = src.find("\n", i)
            j = n if j < 0 else j
            for k in range(i, j):
                out[k] = " "
            i = j
        else:
            i += 1
    return "".join(out)


# A sorry may be INLINE -- `:= by sorry` on the declaration's own line -- as well
# as on a line of its own. `^[ \t]*sorry\b` misses every inline one.
# scripts/count_sorries.sh has warned about exactly this since 2026-07-30 ("grep
# -c '^\s*sorry\b' misses inline `:= by sorry`"), and this file used the bad
# pattern anyway until a97A08 (three inline sorries, lines 72/76/82) exposed it
# on 2026-07-31: `file_is_clean` called that file CLEAN, which would have dropped
# its still-open declarations from the hint as PROVED. Match the TOKEN, with
# comments stripped so prose mentions ("the generated def had a sorry in it",
# a95J03) do not count.
SORRY_TOKEN = re.compile(r"(?<![\w.])sorry(?![\w])")


def sorry_positions(src: str) -> list[int]:
    """Absolute offsets of executable `sorry` tokens, comments excluded."""
    return [m.start() for m in SORRY_TOKEN.finditer(strip_lean_comments(src))]


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
    # Bound the search to THIS declaration's body. Without the bound, a proved
    # declaration picks up the `sorry` of a later one and is reported as still
    # open — observed on a01A06, where distribution_polynomial_decay had been
    # proved but inherited orlicz_bound_not_implies_L2's sorry.
    nxt = re.compile(r"^(?:private\s+)?(?:noncomputable\s+)?(?:theorem|lemma)\s+", re.M).search(src, sig_end)
    stop = nxt.start() if nxt else len(src)
    hits = [p for p in sorry_positions(src) if sig_end <= p < stop]
    if not hits:
        return None
    return sig, src[: hits[0]].count("\n") + 1


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


def row_span(text: str, anchor_id: str) -> tuple[int, int]:
    """Span of the row whose :id is anchor_id.

    Anchored by :id, not :file. Two rows may legitimately share one Lean file
    (a95J07 has two), and anchoring by :file made this assert and abort the whole
    refresh -- which is how a95J07 sat undispatchable with a stale hint on
    2026-07-31. :id is the actual key.
    """
    needle = ':id "%s"' % anchor_id
    hits = [(lo, hi) for lo, hi in iter_rows(text) if needle in text[lo:hi]]
    if not hits:
        raise KeyError(anchor_id)
    assert len(hits) == 1, "%d rows match %s" % (len(hits), anchor_id)
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
    # A row's hint goes stale whenever its Lean file changes -- which happens on
    # a STATEMENT REPAIR (:repaired-at) but equally on a PRIOR ATTEMPT that
    # committed a partial (:last-commit). Only the first was covered until
    # 2026-07-30, so a92J05 was re-dispatched after run 24 carrying a hint that
    # pointed at line 121 when the sorry had moved to 177 -- the same stale-hint
    # waste that cost two dispatches on a95A08 earlier the same day.
    # Rows are auto-selected when their file is KNOWN to have changed (a repair or
    # a prior attempt). But a hint can also go stale for reasons the queue never
    # recorded -- a Mathlib bump, or work done outside this lane -- and such rows
    # then fail the pre-dispatch "hint line actually contains a sorry" assertion
    # and become permanently unreachable. So an EXPLICITLY NAMED row is always
    # refreshed, regardless of whether the queue knows why it went stale.
    # (2026-07-31: three rows were stuck this way overnight.)
    targets = [
        r for r in rows
        if (str(r.get(K("id"))) in args) if args
    ] if args else [
        r for r in rows
        if r.get(K("repaired-at")) is not None or r.get(K("last-commit")) is not None
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
        # "No direct sorry" is NOT the same as proved. A declaration rewritten as
        # a reduction onto another still-sorried theorem carries sorryAx
        # TRANSITIVELY and needs an elaboration to detect -- a01A07 (2026-07-30)
        # had two such declarations dropped from its hint as "PROVED", which
        # would have told the next runner they were done. Only treat a
        # declaration as discharged when the whole FILE is sorry-free; otherwise
        # keep it in the hint and say why.
        no_direct_sorry = [n for n, v in found if v is None]
        file_is_clean = not sorry_positions(src)
        proved = no_direct_sorry if file_is_clean else []
        found = [(n, v) for n, v in found if v is not None]
        if proved:
            print(f"  {rid[14:-15]:10s} dropping {len(proved)} PROVED decl(s) from hint: {proved}")
        elif no_direct_sorry:
            print(f"  {rid[14:-15]:10s} KEEPING {len(no_direct_sorry)} decl(s) with no direct sorry "
                  f"but a still-sorried file (possible transitive sorryAx): {no_direct_sorry}")
            # SIDE EFFECT OF THAT KEEP, found on a95J08 (2026-07-31): if EVERY
            # hinted declaration is transitively-sorried, the refresher finds no
            # direct sorry among them and SKIPs, leaving a stale :line that the
            # pre-dispatch assert then rejects - so the row silently drops out of
            # rotation. Warn loudly; the hint must be repointed at whichever
            # declaration actually carries the sorry.
            if not found:
                print(f"  {rid[14:-15]:10s} *** WARNING: every hinted declaration is transitively "
                      f"sorried, so no line can be refreshed. REPOINT THE HINT BY HAND at the "
                      f"declaration holding the direct sorry, or this row becomes undispatchable.")
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
            lo, hi = row_span(text, rid)
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
