#!/usr/bin/env python3
"""
determination_test.py — ChipWitz Corps determination classifier v0.

Given a choice point and a candidate warrant (with its PUR-recorded outcome),
classify the warrant's relationship to the choice into one of three categories:

  DETERMINED                    — the warrant DETERMINES the choice: proceeding
                                  under it yields the correct decision, and the
                                  warrant's substance (not just vocabulary) bears
                                  on the choice. The BHK arrow is constructive.

  CONSISTENT-NOT-DETERMINED     — the warrant is CONSISTENT with the choice (no
                                  contradiction) but does NOT DETERMINE it: the
                                  match is keyword-overlap or topical proximity,
                                  not semantic relevance. The right answer may
                                  fall out, but for the wrong reason — the
                                  warrant would not compel the decision. (b3's
                                  wrong-← failure mode.)

  UNDETERMINED                  — no warrant bears on the choice at all. The
                                  choice involves competing legitimate values or
                                  operator-life decisions that no standing
                                  contract resolves. (b3's wrong-象 and wrong-香
                                  failure modes, or simply no findable warrant.)

b3's three failure modes (named categories), from the deposit
ft-chipwitz-corps-013 box b3 (reverse-morphogenesis epistemology):

  WRONG-XIANG  (wrong 象)  — the choice-point is under-specified: the agent has
                             not actually named the fork. Category: UNDETERMINED.
  WRONG-XIANG-FRUIT (wrong 香) — the salience is ungrounded: the agent does not
                             know what outcome the warrant should produce.
                             Category: UNDETERMINED.
  WRONG-ARROW  (wrong ←)   — the inferred constraint's answer would not actually
                             yield the understanding: the warrant is consistent
                             with the choice but does not DETERMINE it.
                             Category: CONSISTENT-NOT-DETERMINED.

CLASSIFICATION HEURISTIC — UNCALIBRATED v0
==========================================
This is a heuristic, not a calibrated classifier. It operates on two signals
available in the PXR record:

  1. MATCH-SCORE: the TF-IDF cosine similarity from the warrant_finder. Low
     score (below finder threshold) strongly indicates UNDETERMINED — no
     findable warrant.

  2. PUR-PREDICTION-ERROR TEXT: the self-assessment recorded at cycle close.
     This text is mined for determination-language using keyword analysis:
       - "CORRECT PROCEED WITH RELEVANT WARRANT" / "semantic match" / "genuine
         semantic match" → DETERMINED signal.
       - "keyword overlap" / "not semantic" / "warrant was wrong" / "irrelevant"
         / "FALSE-POSITIVE" → CONSISTENT-NOT-DETERMINED signal.
       - "CORRECT ESCALATION" / "no findable warrant" / "genuine operator
         decision" / "competing legitimate values" → UNDETERMINED signal.

  3. DECISION OVERRIDE: when signals conflict, the prediction-error text is
     authoritative over the match score (the PUR is the post-hoc audit; the
     score is the pre-hoc estimate).

This heuristic is UNCALIBRATED because:
  - The keyword sets are hand-curated from 3 data points (n=3).
  - No threshold tuning has been done.
  - The determination boundary itself (consistent vs determined) is the very
    thing the mission is trying to formalize — we are bootstrapping.
  - Generalization to unseen forks is untested.

Usage:
  python3 determination_test.py --log pxr-log.edn
      Classify all forks in the log and emit typed records.

  python3 determination_test.py --log pxr-log.edn --fork psr-d1b8dbd4a8ab
      Classify a single fork by psr/id.

  python3 determination_test.py --classify "SCORE:0.192 TEXT:social/idempotent-handoff is a genuine semantic match, fix-don't-re-bell IS the pattern"
      Classify from inline score + prediction-error text.

  python3 determination_test.py --smoke
      Run the three known cases and verify the expected classifications.

All file paths are resolved relative to THIS SCRIPT's location (Flight 1
lesson: cwd-robust, file-relative).
"""

import sys
import os
import re
import hashlib
from datetime import datetime, timezone

# --- file-relative path resolution ---
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
LOG_DEFAULT = os.path.join(SCRIPT_DIR, "pxr-log.edn")

# --- classification categories ---
DETERMINED = "DETERMINED"
CONSISTENT_NOT_DETERMINED = "CONSISTENT-NOT-DETERMINED"
UNDETERMINED = "UNDETERMINED"

# --- b3 failure modes (named categories) ---
WRONG_XIANG = "WRONG-XIANG"          # wrong 象: choice-point under-specified
WRONG_XIANG_FRUIT = "WRONG-XIANG-FRUIT"  # wrong 香: salience ungrounded
WRONG_ARROW = "WRONG-ARROW"          # wrong ←: consistent but not determining

# --- keyword sets for heuristic (UNCALIBRATED v0) ---
# These are mined from the PUR prediction-error text.
DETERMINED_KEYWORDS = [
    "genuine semantic match",
    "semantic relevance",
    "semantic match",
    "correct proceed with relevant warrant",
    "is about this kind of move",
    "the pattern is about",
    "the pattern text contains the right vocabulary",
]

CONSISTENT_NOT_DETERMINED_KEYWORDS = [
    "keyword overlap",
    "not semantic",
    "not a semantic warrant",
    "warrant was wrong",
    "warrant was irrelevant",
    "false-positive risk",
    "false-positive",
    "topical proximity",
    "the decision was correct but the warrant was wrong",
]

UNDETERMINED_KEYWORDS = [
    "correct escalation",
    "no findable warrant",
    "genuine operator decision",
    "competing legitimate values",
    "not determined by any standing contract",
    "genuine operator question",
    "operator-life-decision",
    "operator decision",
]


def now_iso():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def count_keyword_hits(text, keywords):
    """Count how many keywords from the list appear (case-insensitive) in text."""
    text_lower = text.lower()
    return sum(1 for kw in keywords if kw.lower() in text_lower)


def classify_from_signals(match_score, finder_threshold, finder_decision,
                          prediction_error_text):
    """Classify a warrant given the available signals.

    Returns: (classification, failure_mode_or_none, rationale)

    Heuristic (UNCALIBRATED v0):
      1. Mine prediction-error text for keyword hits in each category.
      2. The category with the most hits wins (PUR text is authoritative).
      3. On ties or zero hits, fall back to the match score:
         - below threshold → UNDETERMINED
         - above threshold with no keyword signal → CONSISTENT-NOT-DETERMINED
           (conservative: a warrant above threshold without explicit determination
           language is presumed merely consistent).
    """
    det_hits = count_keyword_hits(prediction_error_text, DETERMINED_KEYWORDS)
    cnd_hits = count_keyword_hits(prediction_error_text, CONSISTENT_NOT_DETERMINED_KEYWORDS)
    und_hits = count_keyword_hits(prediction_error_text, UNDETERMINED_KEYWORDS)

    signals = {
        "determined-hits": det_hits,
        "consistent-not-determined-hits": cnd_hits,
        "undetermined-hits": und_hits,
    }

    # Step 1: keyword-driven classification (PUR text authoritative)
    max_hits = max(det_hits, cnd_hits, und_hits)
    if max_hits > 0:
        if det_hits >= cnd_hits and det_hits >= und_hits and det_hits > 0:
            rationale = (
                f"DETERMINED: prediction-error text contains {det_hits} determination "
                f"signal(s) (genuine semantic match / relevant warrant). The warrant's "
                f"substance bears on the choice — the BHK arrow is constructive. "
                f"[UNCALIBRATED v0 heuristic]"
            )
            return DETERMINED, None, rationale, signals

        if cnd_hits >= det_hits and cnd_hits >= und_hits and cnd_hits > 0:
            rationale = (
                f"CONSISTENT-NOT-DETERMINED: prediction-error text contains {cnd_hits} "
                f"consistency-without-determination signal(s) (keyword overlap / warrant "
                f"was wrong / false-positive). The warrant is consistent with the choice "
                f"but does not DETERMINE it — b3's wrong-← failure mode. "
                f"[UNCALIBRATED v0 heuristic]"
            )
            return CONSISTENT_NOT_DETERMINED, WRONG_ARROW, rationale, signals

        if und_hits > 0:
            rationale = (
                f"UNDETERMINED: prediction-error text contains {und_hits} "
                f"undetermination signal(s) (correct escalation / no findable warrant / "
                f"genuine operator decision). No warrant bears on the choice. "
                f"[UNCALIBRATED v0 heuristic]"
            )
            return UNDETERMINED, WRONG_XIANG, rationale, signals

    # Step 2: fallback to match score when no keyword signal
    if finder_decision == "escalate-to-operator" or match_score < finder_threshold:
        rationale = (
            f"UNDETERMINED (fallback): match score {match_score:.4f} below threshold "
            f"{finder_threshold} — no findable warrant. No keyword signal in PUR text "
            f"to classify further. [UNCALIBRATED v0 heuristic]"
        )
        return UNDETERMINED, WRONG_XIANG, rationale, signals

    # Score above threshold but no determination language → conservative CND
    rationale = (
        f"CONSISTENT-NOT-DETERMINED (fallback): match score {match_score:.4f} above "
        f"threshold {finder_threshold} but no explicit determination language in PUR text. "
        f"Conservative default: presume consistent-without-determining. [UNCALIBRATED v0 heuristic]"
    )
    return CONSISTENT_NOT_DETERMINED, WRONG_ARROW, rationale, signals


def parse_edn_log(log_path):
    """Parse the pxr-log.edn file to extract PSR/PUR records.

    This is a minimal EDN reader tuned to the known structure of pxr-log.edn.
    It extracts :records entries and their :psr/id, :psr/score, :psr/threshold,
    :psr/decision, :psr/choice-point, :psr/pattern-chosen, and
    :pur/prediction-error fields.
    """
    with open(log_path, 'r', encoding='utf-8') as f:
        content = f.read()

    records = []

    # Split on {:psr/id to get record blocks. Each block runs from one
    # {:psr/id to the next (or to :calibration-notes / end of file).
    parts = re.split(r'\{:psr/id\s+', content)
    # parts[0] is the preamble; parts[1:] are record blocks

    for block in parts[1:]:
        # Extract the psr/id from the start of the block (first quoted string)
        id_m = re.match(r'"([^"]+)"', block)
        if not id_m:
            continue
        psr_id = id_m.group(1)

        # Extract fields with simple regex. Fields may have the value on the
        # same line (:psr/decision "proceed-with-PSR") or on the next line
        # (:psr/choice-point\n    "Two patterns..."). We use \s+ to bridge.
        score = _extract_float(block, r':psr/score\s+([\d.]+)')
        threshold = _extract_float(block, r':psr/threshold\s+([\d.]+)')
        decision = _extract_string(block, r':psr/decision\s+"([^"]+)"')
        choice_point = _extract_string(block, r':psr/choice-point\s+"(.*?)"', dotall=True)
        pattern_chosen = _extract_string(block, r':psr/pattern-chosen\s+"([^"]+)"')
        prediction_error = _extract_string(block, r':pur/prediction-error\s+"(.*?)"', dotall=True)

        # Skip records that are already classification records (determination/*)
        if psr_id.startswith("det-"):
            continue

        if score is not None and prediction_error is not None:
            records.append({
                "psr/id": psr_id,
                "psr/score": score,
                "psr/threshold": threshold if threshold else 0.15,
                "psr/decision": decision or "",
                "psr/choice-point": choice_point or "",
                "psr/pattern-chosen": pattern_chosen or "",
                "pur/prediction-error": prediction_error,
            })

    return records


def _extract_float(text, pattern):
    m = re.search(pattern, text)
    return float(m.group(1)) if m else None


def _extract_string(text, pattern, dotall=False):
    flags = re.DOTALL if dotall else 0
    m = re.search(pattern, text, flags)
    return m.group(1) if m else None


def classify_record(record):
    """Classify a single PXR record and return a typed classification record."""
    classification, failure_mode, rationale, signals = classify_from_signals(
        record["psr/score"],
        record["psr/threshold"],
        record["psr/decision"],
        record["pur/prediction-error"],
    )

    det_id = "det-" + hashlib.sha256(
        (record["psr/id"] + "determination").encode()
    ).hexdigest()[:12]

    return {
        "determination/id": det_id,
        "determination/at": now_iso(),
        "determination/source-psr": record["psr/id"],
        "determination/choice-point": record["psr/choice-point"],
        "determination/candidate-warrant": record["psr/pattern-chosen"],
        "determination/match-score": record["psr/score"],
        "determination/classification": classification,
        "determination/failure-mode": failure_mode if failure_mode else "none (warrant determines)",
        "determination/heuristic": "UNCALIBRATED v0 — keyword-mining over PUR text + match-score fallback; n=3 calibration set",
        "determination/signals": signals,
        "determination/rationale": rationale,
    }


def format_classification_edn(rec):
    """Format a classification record as an EDN map string."""
    lines = ["   {:determination/id \"" + rec["determination/id"] + "\""]
    lines.append(f'    :determination/at "{rec["determination/at"]}"')
    lines.append(f'    :determination/source-psr "{rec["determination/source-psr"]}"')

    cp = rec["determination/choice-point"].replace('"', '\\"')
    lines.append(f'    :determination/choice-point "{cp}"')
    lines.append(f'    :determination/candidate-warrant "{rec["determination/candidate-warrant"]}"')
    lines.append(f'    :determination/match-score {rec["determination/match-score"]}')
    lines.append(f'    :determination/classification "{rec["determination/classification"]}"')
    lines.append(f'    :determination/failure-mode "{rec["determination/failure-mode"]}"')
    lines.append(f'    :determination/heuristic "{rec["determination/heuristic"]}"')

    sig = rec["determination/signals"]
    lines.append(f'    :determination/signals')
    lines.append(f'     {{:determined-hits {sig["determined-hits"]}')
    lines.append(f'      :consistent-not-determined-hits {sig["consistent-not-determined-hits"]}')
    lines.append(f'      :undetermined-hits {sig["undetermined-hits"]}}}')

    rat = rec["determination/rationale"].replace('"', '\\"')
    lines.append(f'    :determination/rationale "{rat}"')
    lines.append("   }")
    return '\n'.join(lines)


def run_smoke():
    """Smoke test: classify the three known cases and verify expected output."""
    log_path = LOG_DEFAULT
    records = parse_edn_log(log_path)

    expected = {
        "psr-d1b8dbd4a8ab": DETERMINED,       # Fork C: pins-fix
        "psr-9349c740adbe": CONSISTENT_NOT_DETERMINED,  # Fork A: satiety
        "psr-bee6548868f2": UNDETERMINED,      # Fork B: fly-vs-batch
    }

    all_pass = True
    print("SMOKE TEST — determination_test.py v0 (UNCALIBRATED)")
    print("=" * 70)

    for rec in records:
        result = classify_record(rec)
        psr_id = rec["psr/id"]
        exp = expected.get(psr_id, "???")
        got = result["determination/classification"]
        status = "PASS" if got == exp else "FAIL"
        if got != exp:
            all_pass = False

        choice_short = rec["psr/choice-point"][:60] + "..." if len(rec["psr/choice-point"]) > 60 else rec["psr/choice-point"]
        print(f"\n  [{status}] {psr_id}")
        print(f"    choice: {choice_short}")
        print(f"    warrant: {rec['psr/pattern-chosen']} (score {rec['psr/score']})")
        print(f"    expected: {exp}")
        print(f"    got:      {got}")
        print(f"    failure-mode: {result['determination/failure-mode']}")

    print("\n" + "=" * 70)
    if all_pass:
        print("ALL 3 CASES PASS")
    else:
        print("SOME CASES FAILED")
    return all_pass


def main():
    args = sys.argv[1:]

    if "--smoke" in args:
        ok = run_smoke()
        sys.exit(0 if ok else 1)

    if "--classify" in args:
        idx = args.index("--classify")
        if idx + 1 >= len(args):
            print("Error: --classify requires an argument", file=sys.stderr)
            sys.exit(1)
        raw = args[idx + 1]
        # Parse inline: "SCORE:0.192 TEXT:..."
        score_m = re.search(r'SCORE:([\d.]+)', raw)
        text_m = re.search(r'TEXT:(.*?)(?:\s+DECISION:\S+|\Z)', raw, re.DOTALL)
        decision_m = re.search(r'DECISION:(\S+)', raw)

        score = float(score_m.group(1)) if score_m else 0.0
        text = text_m.group(1).strip() if text_m else ""
        decision = decision_m.group(1) if decision_m else "proceed-with-PSR"

        classification, failure_mode, rationale, signals = classify_from_signals(
            score, 0.15, decision, text
        )
        print(json_output({
            "classification": classification,
            "failure-mode": failure_mode or "none",
            "rationale": rationale,
            "signals": signals,
        }))
        sys.exit(0)

    log_path = LOG_DEFAULT
    single_fork = None

    i = 0
    while i < len(args):
        if args[i] == "--log":
            i += 1
            # Resolve relative to cwd if not absolute, else relative to script
            p = args[i]
            if os.path.isabs(p):
                log_path = p
            else:
                log_path = os.path.abspath(p)
        elif args[i] == "--fork":
            i += 1
            single_fork = args[i]
        i += 1

    records = parse_edn_log(log_path)

    if single_fork:
        records = [r for r in records if r["psr/id"] == single_fork]
        if not records:
            print(f"Error: fork {single_fork} not found in {log_path}", file=sys.stderr)
            sys.exit(1)

    results = []
    for rec in records:
        result = classify_record(rec)
        results.append(result)
        print(format_classification_edn(result))
        print()

    # Summary
    print(f"--- {len(results)} fork(s) classified ---")
    for r in results:
        print(f"  {r['determination/source-psr']}: {r['determination/classification']}")

    sys.exit(0)


def json_output(d):
    """Simple JSON formatter (stdlib only)."""
    import json
    return json.dumps(d, indent=2)


if __name__ == "__main__":
    main()
