"""Isolated FTS5 vocabulary probe; no live index/store changes or ranking claim.

Matches futon1b_text.clj's unicode61 tokenizer and quoted AND-default queries.
Frozen original-body.edn is pr-str of the previously captured evidence body.
"""
import json
import sqlite3
from pathlib import Path

ROOT = Path(__file__).resolve().parent


def main():
    original = (ROOT / 'original-body.edn').read_text()
    caption = json.loads((ROOT / 'caption.json').read_text())['caption']
    db = sqlite3.connect(':memory:')
    db.execute("CREATE VIRTUAL TABLE docs USING fts5(arm UNINDEXED, body, tokenize='unicode61')")
    db.executemany('INSERT INTO docs VALUES (?, ?)',
                   [('original', original), ('captioned', original + '\n' + caption)])
    cases = [('ODE uniqueness endpoint', [], ['captioned']),
             ('Gronwall', ['original', 'captioned'], ['original', 'captioned']),
             ('global existence contraction', [], []),
             ('compact operator construction', [], [])]
    rows = []
    for query, before_expected, after_expected in cases:
        match = ' '.join(t if t in ('AND', 'OR') else '"' + t.replace('"', '""') + '"'
                         for t in query.split())
        hits = [r[0] for r in db.execute('SELECT arm FROM docs WHERE docs MATCH ?', (match,))]
        before = [h for h in hits if h == 'original']
        assert set(before) == set(h for h in before_expected if h == 'original')
        assert set(hits) == set(after_expected), (query, hits)
        rows.append({'query': query, 'matches': hits})
    print(json.dumps({'scope': 'isolated lexical matching, not live recall or causal benefit',
                      'sqlite-version': sqlite3.sqlite_version, 'cases': rows}, indent=2))


if __name__ == '__main__':
    main()
