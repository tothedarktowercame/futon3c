#!/usr/bin/env python3
"""Freeze the explicitly consulted development sources, from pinned Git only."""
import hashlib
import json
from pathlib import Path
import subprocess

HERE = Path(__file__).resolve().parent
PINS = {'futon3': 'a376e4043369167ec8b491a77ddc4f9695a6d991',
        'apm-lean': 'f053ab5936725f2e41c937cfff82277f3a23c868'}
PATTERNS = ['math-informal/unfold-the-definition', 'math-informal/split-into-cases',
            'math-informal-CA/estimate-by-bounding', 'math-informal-CA/show-both-inequalities',
            'math-formalization-CA/uniform-continuity-boundedness',
            'math-strategy/plan-first-attempt', 'math-strategy/proof-architecture',
            'math-informal/local-to-global', 'math-informal-CA/optimise-a-free-parameter']


def main():
    rows = []
    for repo, paths in [('futon3', ['library/' + p + '.flexiarg' for p in PATTERNS]),
                        ('apm-lean', ['problems/a93A01/lean/Main.lean',
                                      'problems/a93A01/problem.md',
                                      'problems/a93A01/status.json'])]:
        for path in paths:
            raw = subprocess.check_output(['git', '-C', '/home/joe/code/' + repo,
                                           'show', PINS[repo] + ':' + path])
            rows.append({'repo': repo, 'pin': PINS[repo], 'path': path,
                         'sha256': hashlib.sha256(raw).hexdigest(), 'text': raw.decode()})
    (HERE / 'prelim-sources.json').write_text(json.dumps({
        'schema': 'pattern-construction-prelim-sources-v1',
        'exposure_order': 'See prelim-development.md and commits d15b2e84 and 5b53ebd2; full solution comparison occurred afterward.',
        'sources': rows}, indent=2, ensure_ascii=False) + '\n')
    print('Frozen 9 consulted patterns and 3 development problem sources.')


if __name__ == '__main__':
    main()
