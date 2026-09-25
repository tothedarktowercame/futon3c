#!/usr/bin/env python3
"""offset_unit.py — what the numbers in a span actually count.

Every span in this lab points into holes/missions/M-futon-seams.md as a pair
of integers, and a pair of integers does not say what it counts. They are
character offsets. The mission has multi-byte characters before the first of
them, so read as bytes -- the natural reading for a span into a file, and
what a Clojure, Rust or Go consumer, or a Python reader in binary mode, does
by default -- all 51 of them land in the wrong place.

That is not hypothetical. An outside reader (H-WITNESS-ii, futon2 417bfb4f)
read the lifecycle's anchors as bytes and concluded all eight were stale.
They were exact. The wrong diagnosis was the record's fault for not saying.

Worse are the spans with no quote beside them: mission-C and lifecycle carry
one, so a byte reader at least gets a mismatch, which is how that reader
noticed. The 44 bare :cue spans in proto/ and exemplar/ have nothing to fail
against -- a byte reader gets coherent-looking wrong prose. :cue [4399 4421]
is "se)\\n\\n1. **Declare the " and reads as "ring (use this to prio".

This mission shipped packages/turn-seam, which makes offset_unit a REQUIRED
field and refuses a record whose value is not this one, for exactly this
reason. It taught the lesson outward and never applied it to its own records.
That is the mission's own subject: a seam nobody declared.
"""
UNIT = "unicode-codepoints-zero-based-end-exclusive"


def complaint(declared, where):
    """None if WHERE declares the unit correctly, else why it is refused."""
    if declared is None:
        return (f"{where}: no :offset-unit — its spans are character offsets "
                f"and nothing says so; a consumer reading bytes gets the "
                f"wrong text and no error. Declare :offset-unit :{UNIT}")
    if declared != UNIT:
        return (f"{where}: :offset-unit is :{declared} — this lab's spans are "
                f":{UNIT}")
    return None
