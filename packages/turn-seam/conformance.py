#!/usr/bin/env python3
"""conformance.py — does a turn record conform to the seam?

  conformance.py RECORD.json [RECORD.json ...]
  conformance.py --dir DIRECTORY

Run this against records your OWN implementation produces. It is the thing
that makes the contract a contract rather than a description: two
implementations that both pass agree about what a turn is, and one that
disagrees fails here rather than silently producing misaligned annotations
that nobody notices until a reader is confused.

No dependencies beyond the standard library, and no paths belonging to
whoever wrote it. Exit 1 if any record fails.
"""
import argparse, glob, json, os, re, sys

OFFSET_UNIT = "unicode-codepoints-zero-based-end-exclusive"
REQUIRED = ["version", "source_text", "offset_unit", "sentences", "unmatched",
            "created_at", "agent_id", "session_id", "turn_id"]
STATUSES = {"unresolved", "cue-only", "analyzed"}
ANALYSIS_STATUSES = {"requested", "analyzed", "refused", "failed"}


def check(rec, name):
    bad = []

    for k in REQUIRED:
        if k not in rec:
            bad.append(f"missing required field {k}")
    if bad:
        return bad

    if rec["version"] != 1:
        bad.append(f"version is {rec['version']!r}, this checker knows version 1")
    if rec["offset_unit"] != OFFSET_UNIT:
        bad.append(f"offset_unit is {rec['offset_unit']!r}; a client counting "
                   f"UTF-16 units will disagree silently on any emoji or CJK")

    src = rec["source_text"]

    # The test that matters: every span must be the text it claims to be.
    for s in rec["sentences"]:
        for k in ("id", "start", "end", "text", "status", "cues"):
            if k not in s:
                bad.append(f"sentence missing {k}")
                break
        else:
            a, b = s["start"], s["end"]
            if not (0 <= a < b <= len(src)):
                bad.append(f"{s['id']}: span [{a} {b}] is outside source_text "
                           f"(len {len(src)})")
            elif src[a:b] != s["text"]:
                bad.append(f"{s['id']}: source_text[{a}:{b}] is "
                           f"{src[a:b][:40]!r}, the record says {s['text'][:40]!r}")
            if s["status"] not in STATUSES:
                bad.append(f"{s['id']}: status {s['status']!r} is not one of "
                           f"{sorted(STATUSES)}")
            for c in s["cues"]:
                ca, cb = c.get("start"), c.get("end")
                if not isinstance(ca, int) or not isinstance(cb, int):
                    bad.append(f"{s['id']}: a cue has non-integer offsets")
                elif not (a <= ca < cb <= b):
                    bad.append(f"{s['id']}: a cue [{ca} {cb}] falls outside its "
                               f"own sentence [{a} {b}]")
                elif src[ca:cb] != c.get("text"):
                    bad.append(f"{s['id']}: a cue's text is not what is at its "
                               f"offsets")

    # Sentences must not overlap: two readings of one word is a defect, not a
    # feature, and it is invisible unless something checks.
    spans = sorted((s["start"], s["end"], s["id"]) for s in rec["sentences"]
                   if isinstance(s.get("start"), int))
    for (a1, b1, i1), (a2, _, i2) in zip(spans, spans[1:]):
        if a2 < b1:
            bad.append(f"{i1} and {i2} overlap")

    # A quoted turn keeps both texts, because it has two readers.
    n_tokens = len(re.findall(r"\bQUOTE\b", src))
    quotes = rec.get("quotes")
    if quotes is not None and len(quotes) != n_tokens:
        bad.append(f"{len(quotes)} quotes recorded but {n_tokens} QUOTE tokens "
                   f"in source_text")
    if quotes and "original_text" not in rec:
        bad.append("quotes recorded but no original_text — the display side of "
                   "this seam has nothing to show")

    st = rec.get("analysis_status")
    if st is not None and st not in ANALYSIS_STATUSES:
        bad.append(f"analysis_status {st!r} is not one of {sorted(ANALYSIS_STATUSES)}")
    if st == "analyzed" and "analysis_file" not in rec:
        bad.append("analysis_status is analyzed but no analysis_file names it")

    return bad


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("records", nargs="*")
    ap.add_argument("--dir", help="check every turn-*.json in a directory")
    a = ap.parse_args()

    files = list(a.records)
    if a.dir:
        files += [f for f in sorted(glob.glob(os.path.join(a.dir, "turn-*.json")))
                  if not f.endswith((".analysis.json", ".candidates.json"))]
    if not files:
        sys.exit(__doc__)

    failed = 0
    for f in files:
        try:
            rec = json.load(open(f, encoding="utf-8"))
        except Exception as e:
            print(f"{os.path.basename(f)}: unreadable — {e}")
            failed += 1
            continue
        bad = check(rec, f)
        if bad:
            failed += 1
            print(f"{os.path.basename(f)}: {len(bad)} problem(s)")
            for b in bad[:6]:
                print("  FAIL " + b)
    print(f"\n{len(files)} records, {len(files) - failed} conform, {failed} do not")
    sys.exit(1 if failed else 0)


if __name__ == "__main__":
    main()
