#!/usr/bin/env python3
"""P1b data prep: exhaustive DF and pair co-occurrence for the recorded queries.

Why this exists: P1b must run on a box that cannot hold the 281 MB FTS index
(lucy-joe has ~3 GB available). This extracts the *bounded* slice the analysis
needs — every term and every within-query term pair appearing in the recorded
offered halves — so the experiment is location-independent and the extraction
is itself an auditable artifact.

BLINDNESS PROPERTY, which is the point: terms and pairs are taken EXHAUSTIVELY
from the recorded queries, never chosen. The queries were written by runners
before the answer was known, so no post-hoc selection is possible. This is the
property that fixes the post-hoc contamination in the a95J08 route measurement
(staging G6): there, term pairs were picked knowing the target lemma.

Deterministic. Read-only against a COPY of the index; the live writer is never
opened.

Usage: extract_cooccurrence.py <receipts.edn> <fts5-copy.db> --out <table.json>
"""
import json
import re
import sqlite3
import sys
import hashlib
from itertools import combinations


def offered_queries(path):
    """[(job-id, problem, [terms])] for every offered half, in file order."""
    blob = open(path, errors='replace').read()
    out = []
    for rec in re.split(r'(?=\{:evidence/body)', blob):
        if ':phase :offered' not in rec:
            continue
        job = re.search(r':job-id "([^"]+)"', rec)
        prob = re.search(r':problem "([^"]+)"', rec)
        terms = re.search(r':terms \[([^\]]*)\]', rec)
        surfaced = re.search(r'surfaced-ids \[([^\]]*)\]', rec)
        out.append({
            'job-id': job.group(1) if job else None,
            'problem': prob.group(1) if prob else None,
            'terms': re.findall(r'"([^"]+)"', terms.group(1)) if terms else [],
            'surfaced-n': len(re.findall(r'"', surfaced.group(1))) // 2 if surfaced else 0,
            'empty': not (surfaced and surfaced.group(1).strip()),
        })
    return out


def fts_count(cur, expr):
    """Documents matching an FTS5 expression. Terms are quoted, so an operator
    inside a term is data rather than syntax."""
    cur.execute("SELECT count(*) FROM ev_fts WHERE ev_fts MATCH ?", (expr,))
    return cur.fetchone()[0]


def main():
    receipts, dbpath = sys.argv[1], sys.argv[2]
    rows = offered_queries(receipts)

    terms, pairs = set(), set()
    for r in rows:
        ts = [t for t in r['terms'] if t.strip()]
        terms.update(ts)
        # EVERY within-query pair. No selection, no ranking, no truncation.
        pairs.update(tuple(sorted(p)) for p in combinations(sorted(set(ts)), 2))

    con = sqlite3.connect(f'file:{dbpath}?mode=ro', uri=True)
    cur = con.cursor()

    df = {}
    for t in sorted(terms):
        try:
            df[t] = fts_count(cur, f'"{t}"')
        except sqlite3.Error as e:
            df[t] = None
            print(f'  ! DF failed for {t!r}: {e}', file=sys.stderr)

    co = {}
    for a, b in sorted(pairs):
        try:
            co[f'{a}\t{b}'] = fts_count(cur, f'"{a}" AND "{b}"')
        except sqlite3.Error as e:
            co[f'{a}\t{b}'] = None
            print(f'  ! co-occurrence failed for {a!r},{b!r}: {e}', file=sys.stderr)
    con.close()

    total = fts_count(sqlite3.connect(f'file:{dbpath}?mode=ro', uri=True).cursor(),
                      '"the" OR "a" OR "of"') if False else None

    table = {
        'source-receipts': receipts,
        'index-copy': dbpath,
        'blindness': 'terms and pairs taken exhaustively from recorded queries; '
                     'none selected by the analyst',
        'dispatches': len(rows),
        'distinct-terms': len(df),
        'distinct-pairs': len(co),
        'document-frequency': df,
        'pair-cooccurrence': co,
        'queries': rows,
    }
    payload = json.dumps(table, indent=1, sort_keys=True)
    digest = hashlib.sha256(payload.encode()).hexdigest()
    table['self-sha256-of-payload-without-this-field'] = digest

    print(f'dispatches      {len(rows)}')
    print(f'distinct terms  {len(df)}')
    print(f'distinct pairs  {len(co)}')
    print(f'payload sha256  {digest}')

    if '--out' in sys.argv:
        out = sys.argv[sys.argv.index('--out') + 1]
        with open(out, 'w') as f:
            json.dump(table, f, indent=1, sort_keys=True)
        print(f'wrote {out}')


if __name__ == '__main__':
    main()
