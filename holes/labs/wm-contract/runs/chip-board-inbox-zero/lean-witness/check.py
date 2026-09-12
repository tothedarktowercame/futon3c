#!/usr/bin/env python3
"""Elaborate the Lean witness and compare its projection with retained EDN.
No live service, effect handler or repository mutation is invoked.
"""
import hashlib
import json
from pathlib import Path
import subprocess

f3 = Path(__file__).resolve().parents[6]
mathlib = f3.parent / "mathlib4"
here = Path(__file__).resolve().parent
witness = mathlib / "DarkTower/WarMachine/ChipBoardWitness.lean"
watched = [witness, here / "readback.clj", here.parent / "run-2026-09-12.txt",
           f3 / "src/futon3c/agents/chip_board.clj",
           f3 / "src/futon3c/agents/inbox_zero_board.clj"]


def digests():
    return {str(p): hashlib.sha256(p.read_bytes()).hexdigest() for p in watched}


def run(argv, cwd):
    return subprocess.run(argv, cwd=cwd, check=True, capture_output=True,
                          text=True, timeout=60).stdout


before = digests()
lean = run(["lake", "env", "lean", str(witness.relative_to(mathlib))], mathlib)
assert "sorryAx" not in lean
assert len([x for x in lean.splitlines() if x.startswith("'DarkTower.")]) == 17
prefixes = ("BOARD|", "INPUTS|", "END|", "CHIP|", "ROW|")
expected = [x for x in lean.splitlines() if x.startswith(prefixes)]
actual = run(["bb", "--classpath", "src", str(here / "readback.clj")], f3).splitlines()
assert len(expected) == 17
assert actual == expected, {"lean": expected, "clojure": actual}
assert before == digests(), "Source or retained evidence changed during verification"
print(lean, end="")
print(json.dumps({"state": "validated", "scope": "pinned-projection-and-repaired-base-verb-hazard",
                  "matching_lines": 17, "deltas": [0] * 17,
                  "runtime_certificate_status": "pending",
                  "blockers": ["mutable-registry-not-bound-by-certificate",
                               "arbitrary-validated-board-can-still-throw"],
                  "input_digest": "retained carrier only; original observation packet absent",
                  "hashes": before}, indent=2))
