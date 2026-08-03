#!/usr/bin/env python3
import json
import sys
from pathlib import Path

if len(sys.argv) != 3:
    raise SystemExit("usage: compare_modulo_watermark.py FIRST SECOND")

def normalized(path):
    value = json.loads(Path(path).read_text())
    watermark = value["snapshot-watermark"]
    watermark.pop("read-started-at", None)
    watermark.pop("read-completed-at", None)
    for example in value["worked-join-examples"]:
        example.pop("snapshot-lag-seconds", None)
        example.pop("staleness-bounds-seconds", None)
    return value

if normalized(sys.argv[1]) != normalized(sys.argv[2]):
    raise SystemExit("attachment snapshots differ beyond watermark-derived fields")
print("attachment snapshots are byte-equivalent modulo watermark-derived fields")
