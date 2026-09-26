#!/usr/bin/env python3
"""mission_anchors.py — what an anchor into the mission is anchored to.

One module, imported by both the re-anchorer and the page generator. Writing
`heading_for` twice would have been the defect M-futon-seams instance 7 is
about, in the tooling that renders it.

An anchor is a quote at an offset. That is enough to know the text has not
changed, and not enough to know the note still means what it meant: move a
paragraph from DERIVE to ARGUE and its quote is unchanged, unique, and now
sits under a different phase. So an anchor also records the heading it fell
under, and a change of heading is reported for a decision rather than
applied.
"""
import re

HEADING = re.compile(r"^(#{1,6})\s+(.*)$", re.M)


def heading_for(text, pos):
    """The nearest heading at or before POS, as it is written in the file.

    Returns None before the first heading -- a mission's title block sits
    there, and 'no heading' is a real answer rather than a missing one."""
    last = None
    for m in HEADING.finditer(text):
        if m.start() > pos:
            break
        last = m.group(0).strip()
    return last


def heading_changed(text, anchor):
    """(was, now) when the anchor's recorded heading no longer holds, else None."""
    was = anchor.get("heading")
    if was is None:
        return None                      # not recorded: nothing to compare
    now = heading_for(text, anchor["start"])
    return None if now == was else (was, now)
