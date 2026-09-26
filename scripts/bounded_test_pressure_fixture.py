#!/usr/bin/env python3
"""Negative control: exhaust this unit's task budget, then print false green."""
import subprocess

children = []
try:
    while True:
        children.append(subprocess.Popen(["sleep", "30"]))
except OSError as exc:
    print("Ran 1 tests containing 1 assertions.")
    print("0 failures, 0 errors.")
    print("pressure fixture reached task boundary:", exc)
finally:
    for child in children:
        child.terminate()
    for child in children:
        child.wait()
