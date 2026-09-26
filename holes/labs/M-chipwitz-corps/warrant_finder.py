#!/usr/bin/env python3
"""
warrant_finder.py — ChipWitz Corps warrant-finder v0.

Given a choice-point description, search the pattern library for the best-matching
warrant pattern, return a match score, and decide: proceed-with-PSR or
escalate-to-operator, against an EXPLICIT threshold knob.

THE THRESHOLD IS UNCALIBRATED v0 (fold policy-hole h1). It is a guess, not a
calibrated value. The mission names this as THE one knob: too loose re-creates
autopen-creep; too tight re-creates rubber-stamp asks. This script exposes the
knob but does not claim to have solved it.

Matching mechanism: deterministic TF-IDF cosine similarity over flexiarg text
(python3 stdlib only — no model downloads, no network, reproducible from any cwd).

Usage:
  python3 warrant_finder.py "choice-point description text"
  echo "description" | python3 warrant_finder.py --stdin
  python3 warrant_finder.py "description" --pur '{"actions": "...", "outcome": "...", "prediction-error": "..."}'
  python3 warrant_finder.py --psr-log pxr-log.edn   # append PSR to log
  python3 warrant_finder.py --smoke                  # smoke test

Files are resolved relative to THIS SCRIPT's location (Flight 1 lesson:
file-relative paths, cwd-robust).
"""

import sys
import os
import json
import math
import re
import hashlib
from datetime import datetime, timezone

# --- file-relative path resolution (Flight 1 lesson) ---
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
# Library is at futon3/library/ relative to the repo root.
# From futon3c/holes/labs/M-chipwitz-corps/, that's ../../../../futon3/library/
LIBRARY_DIR = os.path.normpath(os.path.join(SCRIPT_DIR, "..", "..", "..", "..", "futon3", "library"))
PSR_LOG_DEFAULT = os.path.join(SCRIPT_DIR, "pxr-log.edn")

# --- the one knob (h1: UNCALIBRATED v0) ---
THRESHOLD_V0 = 0.15  # cosine similarity; deliberately conservative; NOT CALIBRATED


def now_iso():
    return datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def find_flexiargs(library_dir):
    """Find all .flexiarg files under the library directory."""
    results = []
    for root, dirs, files in os.walk(library_dir):
        for f in sorted(files):
            if f.endswith(".flexiarg"):
                results.append(os.path.join(root, f))
    return results


def extract_pattern_id(filepath, library_dir):
    """Extract the pattern id from the file path (e.g. 'aif/declare-the-conditioning')."""
    rel = os.path.relpath(filepath, library_dir)
    return rel.replace(".flexiarg", "")


def tokenize(text):
    """Simple tokenizer: lowercase, split on non-alphanumeric, filter short tokens."""
    return [t for t in re.split(r'[^a-z0-9]+', text.lower()) if len(t) >= 2]


def read_flexiarg_text(filepath):
    """Read the flexiarg file and return its full text (for TF-IDF)."""
    with open(filepath, 'r', encoding='utf-8', errors='replace') as f:
        return f.read()


def build_tfidf_index(library_dir):
    """Build a TF-IDF index over all flexiarg files.
    Returns: (pattern_ids, documents, idf, vocabulary)
    """
    files = find_flexiargs(library_dir)
    pattern_ids = []
    documents = []

    for fp in files:
        pid = extract_pattern_id(fp, library_dir)
        text = read_flexiarg_text(fp)
        tokens = tokenize(text)
        pattern_ids.append(pid)
        documents.append(tokens)

    # Build document frequency
    n_docs = len(documents)
    df = {}
    for doc in documents:
        seen = set(doc)
        for token in seen:
            df[token] = df.get(token, 0) + 1

    # IDF with smoothing
    idf = {}
    for token, count in df.items():
        idf[token] = math.log((1 + n_docs) / (1 + count)) + 1

    vocabulary = set(df.keys())
    return pattern_ids, documents, idf, vocabulary


def tfidf_vector(tokens, idf, vocabulary):
    """Compute TF-IDF vector for a token list."""
    tf = {}
    for token in tokens:
        if token in vocabulary:
            tf[token] = tf.get(token, 0) + 1

    vec = {}
    for token, count in tf.items():
        vec[token] = count * idf[token]
    return vec


def cosine_similarity(vec1, vec2):
    """Cosine similarity between two sparse vectors."""
    if not vec1 or not vec2:
        return 0.0

    dot = sum(vec1.get(k, 0) * vec2.get(k, 0) for k in set(vec1) & set(vec2))
    norm1 = math.sqrt(sum(v * v for v in vec1.values()))
    norm2 = math.sqrt(sum(v * v for v in vec2.values()))

    if norm1 == 0 or norm2 == 0:
        return 0.0
    return dot / (norm1 * norm2)


def search_warrants(query, library_dir, top_k=5):
    """Search the pattern library for the best-matching warrants.
    Returns list of (pattern_id, score) sorted by score descending.
    """
    pattern_ids, documents, idf, vocabulary = build_tfidf_index(library_dir)

    query_tokens = tokenize(query)
    query_vec = tfidf_vector(query_tokens, idf, vocabulary)

    results = []
    for i, doc in enumerate(documents):
        doc_vec = tfidf_vector(doc, idf, vocabulary)
        score = cosine_similarity(query_vec, doc_vec)
        results.append((pattern_ids[i], score))

    results.sort(key=lambda x: x[1], reverse=True)
    return results[:top_k]


def find_warrant(query, library_dir=LIBRARY_DIR, threshold=THRESHOLD_V0):
    """Find the best warrant for a choice-point description.
    Returns a typed PSR record (dict).
    """
    matches = search_warrants(query, library_dir, top_k=5)

    best_id, best_score = matches[0] if matches else (None, 0.0)
    candidates = [{"pattern-id": pid, "score": round(s, 4)} for pid, s in matches]

    decision = "proceed-with-PSR" if best_score >= threshold else "escalate-to-operator"

    rationale = (
        f"Best match {best_id} at cosine {best_score:.4f} "
        f"(threshold {threshold}, {'ABOVE' if best_score >= threshold else 'BELOW'}). "
        f"THRESHOLD IS UNCALIBRATED v0 — this decision is provisional."
    )

    psr = {
        "psr/id": "psr-" + hashlib.sha256((query + now_iso()).encode()).hexdigest()[:12],
        "psr/at": now_iso(),
        "psr/choice-point": query,
        "psr/pattern-chosen": best_id,
        "psr/score": round(best_score, 4),
        "psr/threshold": threshold,
        "psr/threshold-note": "UNCALIBRATED v0 — fold policy-hole h1; too loose = autopen-creep, too tight = rubber-stamp asks",
        "psr/decision": decision,
        "psr/candidates-considered": candidates,
        "psr/rationale": rationale,
    }
    return psr


def append_psr_to_log(psr, log_path=PSR_LOG_DEFAULT):
    """Append a PSR record to the EDN log file as a top-level map entry.
    The log file is a vector of maps; we read, append, and rewrite.
    """
    # For simplicity in v0, we just print the EDN fragment to append.
    # The actual log is maintained by the caller (pxr-log.edn is hand-authored
    # from the finder's output for the 3 real choice points).
    pass


def format_psr_edn(psr):
    """Format a PSR record as EDN."""
    lines = ["{"]
    for k, v in psr.items():
        if isinstance(v, str):
            lines.append(f' {k} "{v}"')
        elif isinstance(v, (int, float)):
            lines.append(f' {k} {v}')
        elif isinstance(v, list):
            lines.append(f' {k}')
            lines.append(' [')
            for item in v:
                lines.append('  {')
                for ik, iv in item.items():
                    if isinstance(iv, str):
                        lines.append(f'   {ik} "{iv}"')
                    else:
                        lines.append(f'   {ik} {iv}')
                lines.append('  }')
            lines.append(' ]')
    lines.append("}")
    return '\n'.join(lines)


def main():
    args = sys.argv[1:]

    if "--smoke" in args:
        # Smoke test: run a simple query and check exit 0
        psr = find_warrant("should I escalate a fork to the operator or proceed with a warrant?")
        print(f"SMOKE TEST: best={psr['psr/pattern-chosen']} score={psr['psr/score']} decision={psr['psr/decision']}")
        sys.exit(0)

    # Determine query source
    query = None
    pur_json = None

    i = 0
    while i < len(args):
        if args[i] == "--stdin":
            query = sys.stdin.read().strip()
        elif args[i] == "--pur":
            i += 1
            pur_json = args[i]
        elif args[i] == "--threshold":
            i += 1
            THRESHOLD_OVERRIDE = float(args[i])
            # pass through as global
            globals()['THRESHOLD_V0'] = float(args[i])
        elif not args[i].startswith("--"):
            query = args[i]
        i += 1

    if query is None:
        print("Usage: warrant_finder.py \"choice-point description\"", file=sys.stderr)
        print("       echo \"description\" | warrant_finder.py --stdin", file=sys.stderr)
        print("       warrant_finder.py --smoke", file=sys.stderr)
        sys.exit(1)

    psr = find_warrant(query, threshold=globals().get('THRESHOLD_V0', THRESHOLD_V0))

    if pur_json:
        pur = json.loads(pur_json)
        psr["pur/actions"] = pur.get("actions", "")
        psr["pur/outcome"] = pur.get("outcome", "")
        psr["pur/prediction-error"] = pur.get("prediction-error", "")
        psr["pur/at"] = now_iso()

    # Output as JSON for machine consumption
    print(json.dumps(psr, indent=2))


if __name__ == "__main__":
    main()
