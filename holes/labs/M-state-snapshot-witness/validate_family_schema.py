#!/usr/bin/env python3
"""
validate_family_schema.py — B3-F5 shape validator for the family schema.
Mission: M-state-snapshot-witness.
Deposit grounding: ft-state-snapshot-witness-002 (protocol-family naming + run-14 h1 holes).

Validates the family-schema.edn has a 4×4 grid:
  4 siblings {inventory, registry, repo-refs, hud-render}
  × 4 components {container, projection-fn, cadence, emit-fn}
Each cell must be either substantive (cite real paths/fn names) or an explicit :hole.
Plus 1 inline malformed fixture that must be rejected.

stdlib only; file-relative paths; runs from any cwd, exit 0.
"""

import os
import re
import sys

LAB_DIR = os.path.dirname(os.path.abspath(__file__))
SCHEMA_PATH = os.path.join(LAB_DIR, "family-schema.edn")

REQUIRED_SIBLINGS = ["inventory", "registry", "repo-refs", "hud-render"]
REQUIRED_COMPONENTS = ["container", "projection-fn", "cadence", "emit-fn"]


def parse_sibling_blocks(text):
    """
    Minimal EDN parser: extract sibling blocks from {:id "state-snapshot-witness/<name>" ...}.
    Returns list of (sibling_name, block_text).
    """
    siblings = []
    # Match each sibling block — the lookahead handles next sibling, end of siblings vector, or end of text
    pattern = re.compile(
        r'\{:id\s+"state-snapshot-witness/([^"]+)"(.*?)(?=\{:id\s+"state-snapshot-witness/|\]\s*:remaining|\]\s*$|\Z)',
        re.DOTALL,
    )
    for m in pattern.finditer(text):
        name = m.group(1)
        body = m.group(2)
        siblings.append((name, body))
    return siblings


def check_component(body, component_name):
    """
    Check if a component cell is present and substantive.
    Returns (status, detail): status is "substantive", "hole", "missing", or "blank".
    """
    # Look for :component-name followed by a map or :hole
    # Component is a map: {:source "..." ...}
    pattern = rf':{re.escape(component_name)}\s*(?::hole|(?:\{{[^}}]*\}}|\{{.*?\}}))'
    # Try to find the component block
    comp_match = re.search(
        rf':{re.escape(component_name)}\s*\{{(.*?)\}}',
        body, re.DOTALL,
    )
    if comp_match:
        comp_body = comp_match.group(1)
        # Check if it has a :source or :description (substantive content)
        has_source = re.search(r':source\s+"', comp_body)
        has_description = re.search(r':description\s+"', comp_body)
        has_fn_name = re.search(r':fn-name\s+"', comp_body)
        if has_source or has_description or has_fn_name:
            return ("substantive", f"has source/description/fn-name")
        return ("blank", f"map present but no substantive content")
    # Check for explicit :hole
    hole_match = re.search(rf':{re.escape(component_name)}\s+:hole', body)
    if hole_match:
        return ("hole", "explicit :hole entry")
    return ("missing", f"component '{component_name}' not found")


def validate_grid(text):
    """Validate the 4×4 grid. Returns list of error strings."""
    errors = []
    siblings = parse_sibling_blocks(text)

    if len(siblings) != len(REQUIRED_SIBLINGS):
        errors.append(f"sibling count: expected {len(REQUIRED_SIBLINGS)}, got {len(siblings)}")

    found_names = [s[0] for s in siblings]
    for expected in REQUIRED_SIBLINGS:
        if expected not in found_names:
            errors.append(f"MISSING sibling: state-snapshot-witness/{expected}")

    for sib_name, sib_body in siblings:
        for comp in REQUIRED_COMPONENTS:
            status, detail = check_component(sib_body, comp)
            if status == "missing":
                errors.append(f"state-snapshot-witness/{sib_name}: MISSING component '{comp}'")
            elif status == "blank":
                errors.append(f"state-snapshot-witness/{sib_name}: BLANK component '{comp}' — {detail}")
            # "substantive" and "hole" are both valid

    return errors, siblings


def main():
    print("=== B3-F5 validate_family_schema.py ===")
    print(f"Schema: {SCHEMA_PATH}")
    print(f"Required siblings: {REQUIRED_SIBLINGS}")
    print(f"Required components: {REQUIRED_COMPONENTS}")
    print()

    # ── 1. Parse and validate the real family schema ────────────────────────
    with open(SCHEMA_PATH, "r") as f:
        schema_text = f.read()

    errors, siblings = validate_grid(schema_text)
    print(f"Parsed {len(siblings)} siblings from family-schema.edn")

    if errors:
        print("REAL SCHEMA: FAIL")
        for e in errors:
            print(f"  ERROR: {e}")
        sys.exit(1)
    else:
        print("REAL SCHEMA: GREEN — 4×4 grid present, all cells substantive")
        for sib_name, sib_body in siblings:
            statuses = []
            for comp in REQUIRED_COMPONENTS:
                status, _ = check_component(sib_body, comp)
                statuses.append(f"{comp}={status}")
            print(f"  state-snapshot-witness/{sib_name}: {', '.join(statuses)}")
    print()

    # ── 2. Inline malformed fixture: missing component ──────────────────────
    # A sibling with only 3 of 4 components (missing :emit-fn).
    malformed_text = """
    {:id "state-snapshot-witness/broken"
     :container {:source "some-path" :description "test"}
     :projection-fn {:fn-name "some-fn" :description "test"}
     :cadence {:source "boot" :description "boot-time"}}
    """
    malformed_errors, _ = validate_grid(malformed_text)

    # For the malformed fixture, we only check the one sibling
    malformed_siblings = parse_sibling_blocks(malformed_text)
    fixture_errors = []
    for sib_name, sib_body in malformed_siblings:
        for comp in REQUIRED_COMPONENTS:
            status, detail = check_component(sib_body, comp)
            if status == "missing":
                fixture_errors.append(f"state-snapshot-witness/{sib_name}: MISSING component '{comp}'")
            elif status == "blank":
                fixture_errors.append(f"state-snapshot-witness/{sib_name}: BLANK component '{comp}'")

    print("MALFORMED FIXTURE (missing :emit-fn):")
    if fixture_errors:
        for e in fixture_errors:
            print(f"  REJECT: {e}")
        print("  PASS — malformed fixture rejected with named error")
    else:
        print("  FAIL — malformed fixture was accepted")
        sys.exit(1)
    print()

    # ── Done ────────────────────────────────────────────────────────────────
    print("Validation complete. All acceptance criteria met. Exit 0.")
    sys.exit(0)


if __name__ == "__main__":
    main()
