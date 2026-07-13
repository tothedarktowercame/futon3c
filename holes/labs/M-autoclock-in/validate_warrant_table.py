#!/usr/bin/env python3
"""
validate_warrant_table.py — B3-F4 shape validator for the warrant table.
Mission: M-autoclock-in.
Deposit grounding: ft-autoclock-in-003 (run-13 finding: surface the hidden forks).

Validates the warrant-table.edn has all 3 rules × 4 fields (non-blank),
plus 1 inline malformed fixture that must be rejected.

stdlib only; file-relative paths; runs from any cwd, exit 0.
"""

import os
import re
import sys

LAB_DIR = os.path.dirname(os.path.abspath(__file__))
TABLE_PATH = os.path.join(LAB_DIR, "warrant-table.edn")

REQUIRED_RULE_IDS = ["explicit-resolved-target", "creation-clock", "edit-activity"]
REQUIRED_FIELDS = ["condition", "warrant", "success-witness", "silent-fork"]


def parse_edn_rule_blocks(text):
    """
    Minimal EDN parser for the warrant-table structure.
    Extracts rule blocks: {:id "..." :condition "..." :warrant "..." ...}
    Returns list of dicts with rule fields.
    """
    rules = []
    # Find each rule block: starts with {:id "..."
    rule_pattern = re.compile(
        r'\{:id\s+"([^"]+)"(.*?)(?=\{:id\s+"|\]\s*$|\}\s*$)',
        re.DOTALL,
    )
    for m in rule_pattern.finditer(text):
        rule_id = m.group(1)
        body = m.group(2)
        rule = {"id": rule_id}
        for field in REQUIRED_FIELDS:
            # Match :field-name "..." (handling multi-line strings with escaped quotes)
            pattern = rf':{field}\s+"((?:[^"\\]|\\.)*)"\s*(?=[:]|\}}|$)'
            fm = re.search(pattern, body, re.DOTALL)
            if fm:
                rule[field] = fm.group(1)
            else:
                rule[field] = None
        rules.append(rule)
    return rules


def validate_rules(rules, expected_ids):
    """Validate the rules list. Returns list of error strings."""
    errors = []

    if len(rules) != len(expected_ids):
        errors.append(f"rule count: expected {len(expected_ids)}, got {len(rules)}")

    found_ids = [r.get("id") for r in rules]
    for eid in expected_ids:
        if eid not in found_ids:
            errors.append(f"MISSING rule: {eid}")

    for rule in rules:
        rid = rule.get("id", "?")
        for field in REQUIRED_FIELDS:
            val = rule.get(field)
            if val is None:
                errors.append(f"rule '{rid}': MISSING field '{field}'")
            elif not isinstance(val, str) or val.strip() == "":
                errors.append(f"rule '{rid}': BLANK field '{field}'")

    return errors


def main():
    print("=== B3-F4 validate_warrant_table.py ===")
    print(f"Table: {TABLE_PATH}")
    print(f"Required rules: {REQUIRED_RULE_IDS}")
    print(f"Required fields: {REQUIRED_FIELDS}")
    print()

    # ── 1. Parse and validate the real warrant table ────────────────────────
    with open(TABLE_PATH, "r") as f:
        table_text = f.read()

    rules = parse_edn_rule_blocks(table_text)
    print(f"Parsed {len(rules)} rules from warrant-table.edn")

    errors = validate_rules(rules, REQUIRED_RULE_IDS)
    if errors:
        print("REAL TABLE: FAIL")
        for e in errors:
            print(f"  ERROR: {e}")
        sys.exit(1)
    else:
        print("REAL TABLE: GREEN — all 3 rules × 4 fields present and non-blank")
        for rule in rules:
            print(f"  {rule['id']}: {sum(1 for f in REQUIRED_FIELDS if rule.get(f))}/{len(REQUIRED_FIELDS)} fields")
    print()

    # ── 2. Inline malformed fixture: missing field ──────────────────────────
    # A rule with a blank :silent-fork — the field is present but empty.
    malformed_text = """
    {:id "bad-rule"
     :condition "some condition"
     :warrant "some warrant"
     :success-witness "some witness"
     :silent-fork ""}
    """
    malformed_rules = parse_edn_rule_blocks(malformed_text)
    malformed_errors = validate_rules(malformed_rules, ["bad-rule"])

    print("MALFORMED FIXTURE (blank :silent-fork):")
    if malformed_errors:
        for e in malformed_errors:
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
