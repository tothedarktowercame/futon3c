"""Packet template rendering for the APM driver.

Templates live in templates/*.md with ``{{placeholder}}`` slots.
Rendering is strict: every placeholder must be supplied, no
placeholder may survive into the output, and an optional
``forbidden_terms`` list enforces the E9 leakage discipline (a
rendered packet must not name the expected memories' vocabulary when
a probe design requires blindness).
"""

from __future__ import annotations

import re
from pathlib import Path

TEMPLATE_DIR = Path(__file__).resolve().parent / "templates"
_PLACEHOLDER = re.compile(r"\{\{([a-z0-9_]+)\}\}")


class RenderError(ValueError):
    """Raised for missing/unused parameters, leftovers, or leakage."""


def render(template_name: str, params: dict[str, str],
           forbidden_terms: list[str] | None = None) -> str:
    path = TEMPLATE_DIR / f"{template_name}.md"
    if not path.exists():
        raise RenderError(f"no such template: {template_name}")
    text = path.read_text(encoding="utf-8")
    slots = set(_PLACEHOLDER.findall(text))
    supplied = set(params)
    if slots - supplied:
        raise RenderError(
            f"{template_name}: missing params {sorted(slots - supplied)}")
    if supplied - slots:
        raise RenderError(
            f"{template_name}: unused params {sorted(supplied - slots)}")
    rendered = _PLACEHOLDER.sub(lambda m: str(params[m.group(1)]), text)
    leftovers = _PLACEHOLDER.findall(rendered)
    if leftovers:
        raise RenderError(f"{template_name}: unsubstituted {leftovers}")
    if forbidden_terms:
        lowered = rendered.lower()
        words = set(re.split(r"[^a-z0-9]+", lowered))
        leaked = sorted(t.lower() for t in forbidden_terms
                        if t.lower() in words)
        if leaked:
            raise RenderError(
                f"{template_name}: leakage check FAILED on {leaked}")
    return rendered
