#!/usr/bin/env python3
"""
choice_point_validator.py — ChipWitz Corps choice-point shape validator (G5-style).

Implements b6's task-shape-validation requirement (ft-chipwitz-corps-014):
choice-point records must pass shape validation BEFORE entering the
warrant-finder. Early rejection is cheaper than searching for a warrant for an
incoherent fork.

This validator checks the three record types that live in pxr-log.edn:

  1. PSR RECORDS (:psr/*) — the logged choice-point with its warrant search.
     Required fields:
       :psr/id              — non-empty string (record identity)
       :psr/at              — ISO-8601 timestamp
       :psr/choice-point    — non-empty string (the fork description; b3's typed
                              choice-point gate — you cannot find a warrant for
                              an unspecified fork)
       :psr/pattern-chosen  — non-empty string (the selected warrant pattern id)
       :psr/score           — float in [0.0, 1.0] (cosine similarity)
       :psr/threshold       — float in [0.0, 1.0]
       :psr/decision        — one of {"proceed-with-PSR", "escalate-to-operator"}
       :psr/candidates-considered — list of {:pattern-id str :score float} maps
       :psr/rationale       — non-empty string

  2. PUR FIELDS (:pur/*) — the post-proceed audit, embedded within a PSR record.
     Required fields:
       :pur/at              — ISO-8601 timestamp
       :pur/actions         — non-empty string (what was done)
       :pur/outcome         — non-empty string (what happened)
       :pur/prediction-error — non-empty string (self-assessment; b8's binding
                               intent-handshake: without the PUR, the arrow's
                               codomain proof is missing)

  3. DETERMINATION RECORDS (:determination/*) — the b3 reverse-morphogenesis
     classification (appended by B2-F1's determination_test.py).
     Required fields:
       :determination/id           — non-empty string
       :determination/at           — ISO-8601 timestamp
       :determination/source-psr   — non-empty string (back-ref to :psr/id)
       :determination/choice-point — non-empty string
       :determination/classification — one of {"DETERMINED",
                                  "CONSISTENT-NOT-DETERMINED", "UNDETERMINED"}
       :determination/heuristic    — non-empty string (must document calibration)

G5-style rejection: each error is returned as a structured tuple
(gate/id, error/key, detail) so the caller can route it, not just print it.

Usage:
  python3 choice_point_validator.py                    # validate real pxr-log.edn
  python3 choice_point_validator.py --log <path>       # validate a specific log
  python3 choice_point_validator.py --smoke            # validate + inline fixtures
  python3 choice_point_validator.py --json             # machine-readable output

All file paths are resolved relative to THIS SCRIPT's location (Flight 1
lesson: cwd-robust, file-relative).
"""

import sys
import os
import re
import json
from datetime import datetime

# --- file-relative path resolution ---
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
LOG_DEFAULT = os.path.join(SCRIPT_DIR, "pxr-log.edn")

# --- valid enum values ---
VALID_DECISIONS = {"proceed-with-PSR", "escalate-to-operator"}
VALID_CLASSIFICATIONS = {"DETERMINED", "CONSISTENT-NOT-DETERMINED", "UNDETERMINED"}

# --- required field specs ---
PSR_REQUIRED = [
    "psr/id",
    "psr/at",
    "psr/choice-point",
    "psr/pattern-chosen",
    "psr/score",
    "psr/threshold",
    "psr/decision",
    "psr/candidates-considered",
    "psr/rationale",
]

PUR_REQUIRED = [
    "pur/at",
    "pur/actions",
    "pur/outcome",
    "pur/prediction-error",
]

DETERMINATION_REQUIRED = [
    "determination/id",
    "determination/at",
    "determination/source-psr",
    "determination/choice-point",
    "determination/classification",
    "determination/heuristic",
]

ISO_RE = re.compile(
    r"^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}Z$"
)


# === EDN parsing (minimal, tuned to pxr-log.edn structure) ===

def parse_log(log_path):
    """Parse pxr-log.edn into a list of record dicts.

    Returns (psr_records, determination_records).
    Each PSR record includes any embedded PUR fields.
    """
    with open(log_path, "r", encoding="utf-8") as f:
        content = f.read()

    psr_records = []
    det_records = []

    # Split into blocks by {:psr/id or {:determination/id
    # We use regex to find record starts
    psr_starts = [(m.start(), "psr") for m in re.finditer(r"\{:psr/id\s+", content)]
    det_starts = [(m.start(), "det") for m in re.finditer(r"\{:determination/id\s+", content)]

    all_starts = sorted(psr_starts + det_starts)

    for idx, (start, rtype) in enumerate(all_starts):
        end = all_starts[idx + 1][0] if idx + 1 < len(all_starts) else len(content)
        block = content[start:end]

        if rtype == "psr":
            rec = _parse_psr_block(block)
            if rec:
                psr_records.append(rec)
        else:
            rec = _parse_det_block(block)
            if rec:
                det_records.append(rec)

    return psr_records, det_records


def _extract_string(block, key, dotall=True):
    """Extract a string value for a keyword key from an EDN block."""
    flags = re.DOTALL if dotall else 0
    pat = re.compile(r":" + re.escape(key) + r'\s+"(.*?)"', flags)
    m = pat.search(block)
    return m.group(1) if m else None


def _extract_float(block, key):
    """Extract a float value for a keyword key."""
    pat = re.compile(r":" + re.escape(key) + r"\s+([\d.]+)")
    m = pat.search(block)
    return float(m.group(1)) if m else None


def _extract_candidates(block):
    """Extract the :psr/candidates-considered list."""
    # Find the candidates vector
    pat = re.compile(r":psr/candidates-considered\s*\n?\s*\[(.*?)\]", re.DOTALL)
    m = pat.search(block)
    if not m:
        return None
    vec = m.group(1)
    # Extract each {:pattern-id "..." :score ...}
    entries = re.findall(r'\{:pattern-id\s+"([^"]+)"\s+:score\s+([\d.]+)\}', vec)
    return [{"pattern-id": pid, "score": float(s)} for pid, s in entries] if entries else []


def _parse_psr_block(block):
    """Parse a PSR record block into a dict."""
    rec = {}
    for key in PSR_REQUIRED:
        if key in ("psr/score", "psr/threshold"):
            rec[key] = _extract_float(block, key)
        elif key == "psr/candidates-considered":
            rec[key] = _extract_candidates(block)
        else:
            rec[key] = _extract_string(block, key)

    # PUR fields (optional but validated if present)
    for key in PUR_REQUIRED:
        rec[key] = _extract_string(block, key)

    return rec


def _parse_det_block(block):
    """Parse a determination record block into a dict."""
    rec = {}
    for key in DETERMINATION_REQUIRED:
        if key in ("determination/match-score",):
            rec[key] = _extract_float(block, key)
        else:
            rec[key] = _extract_string(block, key)
    return rec


# === Validation ===

class ValidationError:
    """A structured validation error (G5-style)."""
    def __init__(self, gate_id, error_key, detail, record_id=None):
        self.gate_id = gate_id        # e.g. "psr/shape"
        self.error_key = error_key    # e.g. "missing-field"
        self.detail = detail          # human-readable explanation
        self.record_id = record_id

    def __repr__(self):
        rid = f" [{self.record_id}]" if self.record_id else ""
        return f"REJECT{rid}: {self.gate_id} / {self.error_key} — {self.detail}"

    def to_dict(self):
        return {
            "gate/id": self.gate_id,
            "error/key": self.error_key,
            "detail": self.detail,
            "record/id": self.record_id,
        }


def validate_iso(ts):
    if ts is None:
        return False
    if not ISO_RE.match(ts):
        return False
    try:
        datetime.strptime(ts, "%Y-%m-%dT%H:%M:%SZ")
        return True
    except ValueError:
        return False


def validate_psr(rec):
    """Validate a PSR record. Returns list of ValidationErrors (empty = valid)."""
    errors = []
    rid = rec.get("psr/id", "???")

    # Check required fields exist and are non-empty
    for field in PSR_REQUIRED:
        val = rec.get(field)
        if val is None:
            errors.append(ValidationError(
                "psr/shape", "missing-field",
                f"Required field :{field} is absent", rid))
        elif isinstance(val, str) and val.strip() == "":
            errors.append(ValidationError(
                "psr/shape", "empty-field",
                f"Required field :{field} is empty", rid))
        elif isinstance(val, list) and len(val) == 0:
            errors.append(ValidationError(
                "psr/shape", "empty-list",
                f"Required field :{field} is an empty list", rid))

    # Type checks
    score = rec.get("psr/score")
    if score is not None and not (0.0 <= score <= 1.0):
        errors.append(ValidationError(
            "psr/shape", "score-out-of-range",
            f":psr/score {score} not in [0.0, 1.0]", rid))

    threshold = rec.get("psr/threshold")
    if threshold is not None and not (0.0 <= threshold <= 1.0):
        errors.append(ValidationError(
            "psr/shape", "threshold-out-of-range",
            f":psr/threshold {threshold} not in [0.0, 1.0]", rid))

    decision = rec.get("psr/decision")
    if decision is not None and decision not in VALID_DECISIONS:
        errors.append(ValidationError(
            "psr/shape", "invalid-decision",
            f":psr/decision '{decision}' not in {sorted(VALID_DECISIONS)}", rid))

    # ISO timestamp check
    at = rec.get("psr/at")
    if at is not None and not validate_iso(at):
        errors.append(ValidationError(
            "psr/shape", "invalid-timestamp",
            f":psr/at '{at}' is not ISO-8601", rid))

    # Candidates structure
    candidates = rec.get("psr/candidates-considered")
    if candidates is not None:
        for i, c in enumerate(candidates):
            if "pattern-id" not in c or "score" not in c:
                errors.append(ValidationError(
                    "psr/shape", "malformed-candidate",
                    f"Candidate {i} missing :pattern-id or :score", rid))
            elif not isinstance(c.get("score"), (int, float)):
                errors.append(ValidationError(
                    "psr/shape", "malformed-candidate",
                    f"Candidate {i} :score is not numeric", rid))

    # PUR fields: if any PUR field is present, all must be present (b8 binding
    # intent-handshake: a partial PUR means the codomain proof is incomplete)
    pur_present = [k for k in PUR_REQUIRED if rec.get(k) is not None]
    if pur_present and len(pur_present) < len(PUR_REQUIRED):
        missing_pur = [k for k in PUR_REQUIRED if rec.get(k) is None]
        errors.append(ValidationError(
            "pur/shape", "partial-pur",
            f"PUR fields present but missing: {missing_pur} "
            f"(b8: partial PUR = incomplete codomain proof)", rid))

    # If PUR fields present, validate non-empty
    for field in PUR_REQUIRED:
        val = rec.get(field)
        if val is not None and isinstance(val, str) and val.strip() == "":
            errors.append(ValidationError(
                "pur/shape", "empty-field",
                f"Required field :{field} is empty", rid))

    return errors


def validate_determination(rec):
    """Validate a determination record. Returns list of errors."""
    errors = []
    rid = rec.get("determination/id", "???")

    for field in DETERMINATION_REQUIRED:
        val = rec.get(field)
        if val is None:
            errors.append(ValidationError(
                "det/shape", "missing-field",
                f"Required field :{field} is absent", rid))
        elif isinstance(val, str) and val.strip() == "":
            errors.append(ValidationError(
                "det/shape", "empty-field",
                f"Required field :{field} is empty", rid))

    classification = rec.get("determination/classification")
    if classification is not None and classification not in VALID_CLASSIFICATIONS:
        errors.append(ValidationError(
            "det/shape", "invalid-classification",
            f":determination/classification '{classification}' "
            f"not in {sorted(VALID_CLASSIFICATIONS)}", rid))

    at = rec.get("determination/at")
    if at is not None and not validate_iso(at):
        errors.append(ValidationError(
            "det/shape", "invalid-timestamp",
            f":determination/at '{at}' is not ISO-8601", rid))

    return errors


def validate_log(log_path):
    """Validate all records in a pxr-log file. Returns (errors, summary)."""
    psr_records, det_records = parse_log(log_path)

    all_errors = []

    for rec in psr_records:
        all_errors.extend(validate_psr(rec))

    for rec in det_records:
        all_errors.extend(validate_determination(rec))

    summary = {
        "psr-count": len(psr_records),
        "det-count": len(det_records),
        "errors": len(all_errors),
    }
    return all_errors, summary


# === Inline malformed fixtures (must be REJECTED) ===

MALFORMED_FIXTURE_1 = {
    "psr/id": "psr-malformed-001",
    "psr/at": "2026-07-10T14:51:53Z",
    # MISSING: :psr/choice-point (b3: cannot find a warrant for an unspecified fork)
    "psr/pattern-chosen": "some/pattern",
    "psr/score": 0.15,
    "psr/threshold": 0.15,
    "psr/decision": "proceed-with-PSR",
    "psr/candidates-considered": [{"pattern-id": "some/pattern", "score": 0.15}],
    "psr/rationale": "test rationale",
    # Partial PUR: has :pur/at but missing the other 3 PUR fields
    "pur/at": "2026-07-10T14:55:00Z",
}
# Expected rejections: missing :psr/choice-point, partial PUR (3 missing fields)

MALFORMED_FIXTURE_2 = {
    "psr/id": "psr-malformed-002",
    "psr/at": "not-a-timestamp",
    "psr/choice-point": "valid choice point text",
    "psr/pattern-chosen": "some/pattern",
    "psr/score": 1.5,  # OUT OF RANGE (> 1.0)
    "psr/threshold": -0.2,  # OUT OF RANGE (< 0.0)
    "psr/decision": "just-do-it",  # INVALID ENUM
    "psr/candidates-considered": [],  # EMPTY LIST
    "psr/rationale": "",  # EMPTY STRING
}
# Expected rejections: invalid timestamp, score out of range, threshold out of
# range, invalid decision, empty candidates list, empty rationale


def validate_inline(rec):
    """Validate an inline record dict (same logic as validate_psr)."""
    if any(k.startswith("psr/") for k in rec):
        return validate_psr(rec)
    elif any(k.startswith("determination/") for k in rec):
        return validate_determination(rec)
    return [ValidationError("unknown/shape", "unknown-record-type",
                            "Record has neither :psr/* nor :determination/* keys")]


# === Main ===

def main():
    args = sys.argv[1:]
    use_json = "--json" in args
    smoke = "--smoke" in args

    log_path = LOG_DEFAULT
    for i, a in enumerate(args):
        if a == "--log" and i + 1 < len(args):
            p = args[i + 1]
            log_path = p if os.path.isabs(p) else os.path.abspath(p)

    results = {}

    # 1. Validate real pxr-log.edn
    errors, summary = validate_log(log_path)
    results["real-log"] = {
        "path": log_path,
        "summary": summary,
        "errors": [e.to_dict() for e in errors],
        "status": "GREEN" if not errors else "RED",
    }

    # 2. Validate inline malformed fixtures
    fixture_results = []
    for name, fixture in [("MALFORMED-1", MALFORMED_FIXTURE_1),
                          ("MALFORMED-2", MALFORMED_FIXTURE_2)]:
        ferr = validate_inline(fixture)
        fixture_results.append({
            "name": name,
            "error-count": len(ferr),
            "errors": [e.to_dict() for e in ferr],
            "status": "REJECTED" if ferr else "WRONGLY-ACCEPTED",
        })
    results["fixtures"] = fixture_results

    # Determine overall pass/fail
    real_green = results["real-log"]["status"] == "GREEN"
    fixtures_rejected = all(f["status"] == "REJECTED" for f in fixture_results)
    overall_pass = real_green and fixtures_rejected

    # Output
    if use_json:
        results["overall"] = "PASS" if overall_pass else "FAIL"
        print(json.dumps(results, indent=2))
    else:
        print("=" * 70)
        print("CHOICE-POINT VALIDATOR — G5-style shape check")
        print("=" * 70)

        print(f"\n1. REAL LOG: {log_path}")
        s = summary
        print(f"   PSR records: {s['psr-count']}")
        print(f"   Determination records: {s['det-count']}")
        if errors:
            print(f"   ERRORS ({len(errors)}):")
            for e in errors:
                print(f"     {e}")
        else:
            print(f"   STATUS: GREEN (all records valid)")

        print(f"\n2. INLINE MALFORMED FIXTURES:")
        all_fixture_pass = True
        for fr in fixture_results:
            status = fr["status"]
            marker = "✓" if status == "REJECTED" else "✗"
            print(f"   {marker} {fr['name']}: {status} "
                  f"({fr['error-count']} errors)")
            for e in fr["errors"]:
                print(f"       {e['gate/id']} / {e['error/key']} — {e['detail']}")
            if status != "REJECTED":
                all_fixture_pass = False

        print(f"\n{'=' * 70}")
        if overall_pass:
            print("OVERALL: PASS (real log green, both fixtures rejected)")
        else:
            print("OVERALL: FAIL")

    sys.exit(0 if overall_pass else 1)


if __name__ == "__main__":
    main()
