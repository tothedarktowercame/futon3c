#!/usr/bin/env python3
"""V2-3: code declined-memory mentions against the pre-registered categories.

Deterministic and auditable. The runners write in a near-template, so the
stated grounds are highly stereotyped and lexical coding is defensible — but it
IS lexical, not semantic. Every mention the patterns cannot place is reported
as residue rather than forced, per the pre-registered protocol.

Categories (1-5 claude-9, pre-registered; 6 claude-9 amendment; 7 Joe):
  1 topical-mismatch          different subject entirely
  2 scope-mismatch            right area, wrong sub-object
  3 subsumption               relevant but already handled / another route won
  4 stage-mismatch            relevant to a LATER target (considered-but-declined)
  5 precondition-absent       the memory's trigger never fired
  6 relevance-no-applicability right shape, unusable form
  7 discoverability           needed something that could not be obtained

Usage: code_rejections.py <coding-sections.json> [--edn out.json]
"""
import json
import re
import sys
import collections

# Ordered: first match wins, most specific first.
RULES = [
    ('4-stage-mismatch', re.compile(
        r'(?i)(relevant only to the later|concerns the later|the later [\w.\-]*\s*target'
        r'|downstream (?:algebraic )?bookkeeping|is downstream)')),
    ('3-subsumption', re.compile(
        r'(?i)(handles .{0,40} internally|already (?:handled|committed|closed|axiom-clean|proved)'
        r'|was shorter|closed the analogous step|not needed for|already had|sibling had already)')),
    ('6-relevance-no-applicability', re.compile(
        r'(?i)(conceptually (?:adjacent|relevant|matches|related)|directionally related'
        r'|but (?:it )?does not supply|supplies no|but not applicable|lacks the)')),
    ('5-precondition-absent', re.compile(
        r'(?i)(no .{0,60}(?:arose|occurred|applies|applied|was introduced|surfaced|step|obligation'
        r'|side condition|premise|argument)|never fired|did not arise|no such)')),
    ('2-scope-mismatch', re.compile(
        r'(?i)(concerns .{0,80}, not |not (?:only )?(?:for|this) general|unlike this task'
        r'|requires general|only .{0,30}p\s*=\s*2|excludes this|infinite-space|not this )')),
    ('1-topical-mismatch', re.compile(
        r'(?i)(unrelated|different problem class|-specific\b|not part of this route'
        r'|does not affect this)')),
    ('7-discoverability', re.compile(
        r'(?i)(could not (?:import|find|obtain|reach)|not (?:importable|reachable|in scope)'
        r'|had to (?:re-?)?prove|no such (?:lemma|module|library))')),
]

# Recall that returned nothing at all: not a declined memory, a channel silence.
ERROR_RECALL_EMPTY = re.compile(
    r'(?i)error[- ]?(?:time )?(?:recall|memor\w+).{0,80}?'
    r'(?:returned no|surfaced no|produced no|no memory ID|no error-time memor)'
    r'|no error-time memor\w+ (?:surfaced|were queried)')

# A mention with a memory id/name and a verdict, one per bullet line.
MENTION = re.compile(r'^\s*[-*]\s+(.*)$', re.M)
DECLINED = re.compile(
    r'(?i)\b(ignored|not used|not directly applicable|not applicable|reviewed but'
    r'|considered but|did not use|not needed|respected|not applicable)')
USED = re.compile(r'(?i)^\s*[-*]\s*(?:\*\*)?[`\w\-]*(?:\*\*)?\s*[—:-]?\s*used\b|\bused\b[ .,:]')


def code(text):
    for name, rx in RULES:
        if rx.search(text):
            return name
    return None


def main():
    rows = json.load(open(sys.argv[1]))
    cats = collections.Counter()
    residue = []
    per_row = []
    silent_recall = 0
    for r in rows:
        sec = r['section']
        if ERROR_RECALL_EMPTY.search(sec):
            silent_recall += 1
        codes = []
        for line in MENTION.findall(sec):
            if not DECLINED.search(line):
                continue
            c = code(line)
            if c:
                cats[c] += 1
                codes.append(c)
            else:
                residue.append((r['job-id'][-12:], line.strip()[:150]))
        per_row.append({'job-id': r['job-id'], 'problem': r['problem'],
                        'codes': codes})
    total = sum(cats.values())
    print(f'rows: {len(rows)}')
    print(f'coded declined-mentions: {total}   residue: {len(residue)}')
    print(f'rows reporting error-recall returned NOTHING: {silent_recall}'
          f'  ({100*silent_recall//len(rows)}%)\n')
    for c, n in sorted(cats.items()):
        bar = '#' * (40 * n // max(cats.values()))
        print(f'  {c:<30} {n:>4}  {bar}')
    print('\n--- residue (unclassifiable, reported not forced) ---')
    for jid, line in residue[:12]:
        print(f'  [{jid}] {line}')
    if len(residue) > 12:
        print(f'  … and {len(residue)-12} more')
    if '--out' in sys.argv:
        out = sys.argv[sys.argv.index('--out') + 1]
        json.dump({'counts': dict(cats), 'total': total,
                   'residue': residue, 'per-row': per_row,
                   'rows-with-silent-error-recall': silent_recall,
                   'rows': len(rows)},
                  open(out, 'w'), indent=1)
        print(f'\nwrote {out}')


if __name__ == '__main__':
    main()
