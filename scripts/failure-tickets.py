#!/usr/bin/env python3
"""Write a ticket per failure class, the way *Backtrace* writes a buffer.

An error that nobody records is an error nobody can act on, so every typed
terminal-code gets a file in the draw pile automatically -- one per CLASS, not
per occurrence. The 72nd agent-not-found updates a count; it does not create a
72nd ticket.

FALL-BACK RULE. A class that reaches DEMOTE_AT occurrences while nothing has
been done about it falls back from BELIEVE to PERCEIVE. This is deliberately
the opposite of escalate-on-repeat: repetition without response is evidence
about the OBSERVER, not the fault. If a thing has fired seventy times and no
one has touched its ticket, the honest reading is "I perceive this and no
longer see it as a problem" -- so the board stops presenting it as a decision
and keeps it as a fact. It costs nothing to reverse: edit the ticket and it is
back in BELIEVE on the next run. That is also why nothing is excluded by fiat.
operator-cancelled is not filtered out for being your own decision; if it keeps
happening and nobody cares, it demotes itself, and if it stops it stays small.

The generator owns ONE delimited block and never writes outside it, so a human
can put anything in the rest of the file and the counts stay live underneath.
Text outside that block differing from the template is exactly what "somebody
did something" means here -- there is no status field to keep honest.

  python3 scripts/failure-tickets.py            # dry run: say what would change
  python3 scripts/failure-tickets.py --write
"""
import os, re, sys, importlib.util

HERE = os.path.dirname(os.path.abspath(__file__))
ROOT = os.path.dirname(HERE)
TICKETS = os.environ.get("TICKETS", os.path.join(ROOT, "holes", "tickets"))
DEMOTE_AT = int(os.environ.get("DEMOTE_AT", "10"))

_spec = importlib.util.spec_from_file_location(
    "failure_census", os.path.join(HERE, "failure-census.py"))
census = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(census)

BEGIN = "<!-- census:begin -->"
END = "<!-- census:end -->"

BODY = """
## Why this file exists

Written by `scripts/failure-tickets.py` from the invoke-job ledger, so a typed
failure lands in the queue without anyone noticing it first. Everything above
the `census:end` marker is regenerated on each run; everything below it is
yours and is never touched.

Editing anything in this section is what moves the class back to BELIEVE: there
is no status field, so a file that nobody has written in is a class that nobody
has responded to.

## What would close this

(unstated -- until it is, this class has no promotion test and cannot reach
SELECT)
"""


def block(r, stage, reason):
    tgt = r["targets"][0][0] if r["targets"] else "?"
    clr = r["callers"][0][0] if r["callers"] else "?"
    return "\n".join([
        BEGIN,
        f"- **class**: `{r['terminal_code']}`",
        f"- **multiplicity**: {r['count']}",
        f"- **window**: {r['first']} -> {r['last']}",
        f"- **most often**: `{tgt}` <- `{clr}`",
        f"- **stage**: {stage}{(' -- ' + reason) if reason else ''}",
        "- **source**: `GET /api/alpha/invoke/jobs`, field `terminal-code`",
        END,
    ])


def path_for(code):
    return os.path.join(TICKETS, "T-fail-" + re.sub(r"[^a-z0-9-]", "-", code) + ".md")


def split(text):
    """(head, tail) around the generated block, or (None, None) if absent."""
    if BEGIN not in text or END not in text:
        return None, None
    i, j = text.index(BEGIN), text.index(END) + len(END)
    return text[:i], text[j:]


def current_stages():
    """{terminal-code: stage} as recorded in the written tickets.

    The board reads this rather than re-deciding the fall-back rule for itself.
    A rule evaluated in two places is a rule that will eventually be evaluated
    two ways, and the queue is the one that has to be right -- it is where
    somebody writes the response that reverses the demotion.
    """
    out = {}
    if not os.path.isdir(TICKETS):
        return out
    for fn_ in sorted(os.listdir(TICKETS)):
        if not (fn_.startswith("T-fail-") and fn_.endswith(".md")):
            continue
        txt = open(os.path.join(TICKETS, fn_), encoding="utf-8").read()
        code = re.search(r"^- \*\*class\*\*: `([^`]+)`", txt, re.M)
        stage = re.search(r"^- \*\*stage\*\*: (\w+)", txt, re.M)
        if code and stage:
            out[code.group(1)] = stage.group(1)
    return out


def main():
    write = "--write" in sys.argv
    rows, _ = census.classes(census.fetch_jobs())
    os.makedirs(TICKETS, exist_ok=True)
    changed, demoted = [], []

    for r in rows:
        p = path_for(r["terminal_code"])
        old = open(p, encoding="utf-8").read() if os.path.exists(p) else None
        _, tail = split(old) if old else (None, None)

        # Untouched means the prose is still exactly the template. That is the
        # whole action signal: no status field to forget to update, and no way
        # to look responded-to without having written something.
        # .strip() on BOTH sides, not .rstrip(). Comparing raw text made the
        # separator newline part of the test: each rewrite added one, so the
        # tail stopped matching the template after a single run and every class
        # silently read as "somebody responded to this" -- which would have
        # turned the fall-back rule off on its second run and looked like the
        # rule working.
        untouched = old is None or (tail is not None and tail.strip() == BODY.strip())
        stale = untouched and r["count"] >= DEMOTE_AT
        stage = "PERCEIVE" if stale else "BELIEVE"
        reason = (f"fell back: {r['count']} occurrences at or over {DEMOTE_AT} "
                  f"with nothing written here") if stale else ""
        if stale:
            demoted.append((r["terminal_code"], r["count"]))

        head = (f"# T-fail-{r['terminal_code']} -- {r['terminal_code']} "
                f"({r['count']}x)\n\n")
        # Normalise the seam so rewriting is a fixed point: the block is
        # followed by exactly one blank line whatever the previous run left.
        new = (head + block(r, stage, reason) + "\n\n"
               + (tail if tail is not None else BODY).lstrip("\n"))

        if old != new:
            changed.append((os.path.basename(p), "new" if old is None else "updated"))
            if write:
                with open(p, "w", encoding="utf-8") as f:
                    f.write(new)

    verb = "wrote" if write else "would write"
    print(f"{len(rows)} class(es); {verb} {len(changed)} file(s) in {TICKETS}")
    for name, how in changed:
        print(f"  {how:<8} {name}")
    if demoted:
        print(f"\n  fell back to PERCEIVE (>= {DEMOTE_AT} occurrences, nothing written):")
        for code, n in demoted:
            print(f"    {code}  x{n}")
    if not write:
        print("\n  (dry run -- pass --write)")


if __name__ == "__main__":
    main()
