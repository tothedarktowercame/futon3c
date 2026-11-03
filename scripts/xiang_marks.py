"""Shared parsing primitives for the 象 reply proforma."""

import re


MARKS = "㊩🈖㊢🈯㊟㊣🈚㊮🈹🈲🈕㊫㊭㊝🈘🈝㊯🈡🈸🈰㊬㊥🈳"


def paragraphs(text):
    return [p.strip() for p in re.split(r"\n\s*\n", text or "") if p.strip()]


def bracket(after):
    """Return a balanced leading parenthesized target and the remaining text."""
    s = after.lstrip()
    if s.startswith(":"):
        s = s[1:].lstrip()
    if not s.startswith("("):
        return None, after
    depth = 0
    for i, ch in enumerate(s):
        depth += ch == "("
        depth -= ch == ")"
        if depth == 0:
            return s[1:i].strip(), s[i + 1:]
    return None, after


def marked(text):
    out = []
    for p in paragraphs(text):
        if p and p[0] in MARKS:
            target, body = bracket(p[1:])
            out.append({"mark": p[0], "target": target,
                        "body": body.strip(), "para": p})
    return out
