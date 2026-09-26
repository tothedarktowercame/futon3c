#!/usr/bin/env python3
"""Print enabled/disabled/unknown for a campaign's JIT coordinator.

The watcher needs to tell a stalled regulator from one an operator switched
off. A disabled coordinator has flat ticks and a :running durable status,
which is indistinguishable from a hang unless the registry is consulted.

Entry extraction is brace-matched, not offset-based. Earlier versions bounded
the entry by scanning forward a fixed distance or to the next known key; both
broke, because an entry repeats its own id inside :coordinator/config and that
config grew to embed a whole problem list.
"""
import re
import sys

REGISTRY = "/home/joe/code/futon3c/data/apm-coordinators/registry.edn"


def entry_text(text, campaign):
    """Return the EDN map following the campaign's key, brace-matched."""
    marker = '"jit-queue:%s"' % campaign
    start = text.find(marker)
    if start < 0:
        return None
    open_brace = text.find("{", start + len(marker))
    if open_brace < 0:
        return None
    depth, i, in_string, escaped = 0, open_brace, False, False
    while i < len(text):
        ch = text[i]
        if in_string:
            if escaped:
                escaped = False
            elif ch == "\\":
                escaped = True
            elif ch == '"':
                in_string = False
        elif ch == '"':
            in_string = True
        elif ch == "{":
            depth += 1
        elif ch == "}":
            depth -= 1
            if depth == 0:
                return text[open_brace:i + 1]
        i += 1
    return None


def enabled(campaign, registry=REGISTRY):
    try:
        with open(registry) as handle:
            text = handle.read()
    except OSError:
        return "unknown"
    entry = entry_text(text, campaign)
    if entry is None:
        return "unknown"
    found = re.search(r":coordinator/enabled\?\s+(true|false)", entry)
    if not found:
        return "unknown"
    return "enabled" if found.group(1) == "true" else "disabled"


if __name__ == "__main__":
    print(enabled(sys.argv[1] if len(sys.argv) > 1 else ""))
