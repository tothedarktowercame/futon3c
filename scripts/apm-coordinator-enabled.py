#!/usr/bin/env python3
"""Print enabled/disabled/unknown for a campaign's JIT coordinator.

The watcher needs to tell a stalled regulator from one an operator switched
off. A disabled coordinator has flat ticks and a :running durable status,
which is indistinguishable from a hang unless the registry is consulted.
"""
import re
import sys

REGISTRY = "/home/joe/code/futon3c/data/apm-coordinators/registry.edn"


def enabled(campaign, registry=REGISTRY):
    try:
        with open(registry) as handle:
            text = handle.read()
    except OSError:
        return "unknown"
    marker = '"jit-queue:%s"' % campaign
    start = text.find(marker)
    if start < 0:
        return "unknown"
    # Each entry ends with its digest. Bounding on the next coordinator KEY
    # does not work: the entry repeats its own id inside :coordinator/config,
    # so the search lands mid-entry, before the flag.
    end = text.find(":coordinator/entry-digest", start)
    entry = text[start:end if end > 0 else len(text)]
    found = re.search(r":coordinator/enabled\?\s+(true|false)", entry)
    if not found:
        return "unknown"
    return "enabled" if found.group(1) == "true" else "disabled"


if __name__ == "__main__":
    print(enabled(sys.argv[1] if len(sys.argv) > 1 else ""))
