#!/usr/bin/env python3
"""Discharge the E2-pilot e3 instrument gate with control run 1."""
from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path
from typing import Sequence

sys.path.insert(0, str(Path(__file__).resolve().parent))
import e2_pilot_run as pilot  # noqa: E402


def evaluate(record: dict) -> tuple[bool, str]:
    if record.get("isolation-receipt", {}).get("effective-uid") != pilot.EXPECTED_UID:
        return False, "isolation receipt has the wrong effective uid"
    if record.get("trace", {}).get("attempt-count", 0) < 1:
        return False, "control run produced no incremental commit/attempt"
    if not record.get("extractor-stable"):
        return False, "decision-sequence extractor is not byte-stable across two extractions"
    if not record.get("manipulation", {}).get("passed"):
        return False, "control manipulation check failed: registered memories did not surface"
    if not record.get("attribution", {}).get("passed"):
        return False, "use-attribution gate failed"
    return True, ""


def parser() -> argparse.ArgumentParser:
    result = argparse.ArgumentParser(description=__doc__)
    result.add_argument("--problem", choices=["a95J01"], default="a95J01")
    result.add_argument("--base", choices=["51b6bc00"], default="51b6bc00")
    result.add_argument("--output", type=Path, required=True)
    return result


def main(argv: Sequence[str] | None = None) -> int:
    args = parser().parse_args(argv)
    try:
        record = pilot.run_one(args.problem, args.base, "control", args.output)
    except pilot.PilotError as error:
        print(f"e3 apparatus defect: {error}", file=sys.stderr)
        return 2
    passed, defect = evaluate(record)
    if not passed:
        print(f"e3 apparatus defect: {defect}", file=sys.stderr)
        return 1
    print("e3 discharged " + str(record["trace-hash"]))
    print(json.dumps(record, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
