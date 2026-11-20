#!/usr/bin/env python3
"""Offline provenance anchors. Run numbers are one-based positions in all slot runs."""
import argparse
from bisect import bisect_left
from collections import Counter, defaultdict
from datetime import datetime, timedelta
import json
from pathlib import Path
import re
import sys


def normalized(text):
    return ' '.join(text.split())


def normalize_map(text):
    """Normalized characters and their original codepoint offsets."""
    chars, offsets = [], []
    for token in re.finditer(r'\S+', text):
        if chars:
            chars.append(' ')
            offsets.append(token.start() - 1)
        chars.extend(token.group())
        offsets.extend(range(token.start(), token.end()))
    return ''.join(chars), offsets


def occurrences(text, needle, lo=0, hi=None):
    if not needle:
        return []
    hi = len(text) if hi is None else hi
    found = []
    pos = text.find(needle, lo, hi)
    while pos >= 0:
        found.append((pos, pos + len(needle)))
        pos = text.find(needle, pos + 1, hi)
    return found


def original_stream(state):
    chars, ids, counts = [], [], Counter()
    for run in state['runs']:
        site, counter = run['id']
        counts[site] += len(run['text'])
        if site == 'fable-1.restore' and run.get('attrs', {}).get('source') == 'incubator-buffer-copy':
            chars.extend(run['text'])
            ids.extend([site, counter + i] for i in range(len(run['text'])))
    if len({tuple(i) for i in ids}) != len(ids):
        raise ValueError('duplicate original character ids')
    return ''.join(chars), ids, counts


def label(op, origin):
    if op == 'kept':
        return "Joe's words, kept from the earlier answer"
    if op == 'typed':
        return 'written by Joe in his editorial pass'
    if op == 'accepted':
        if origin == 'peeragogy':
            return 'from the Peeragogy call; kept by Joe'
        if origin is None:
            return 'accepted by Joe (origin not recorded)'
        return f"written by {'Murch' if origin == 'murch' else origin}; kept by Joe"
    return op


def build(extraction, state):
    modes = [x for x in extraction if x['track'] == 'full']
    if len(modes) != 1:
        raise ValueError('expected exactly one full track')
    slots = modes[0]['slots']
    original, ids, counts = original_stream(state)
    stream, offsets = normalize_map(original)
    answers, hits = [], []
    for slot in slots:
        answer = ''.join(r['text'] for r in slot['runs'])
        norm, mapping = normalize_map(answer)
        candidates = occurrences(stream, norm)
        # A supplied heading must immediately precede the answer modulo whitespace.
        heading = slot.get('heading')
        if len(candidates) != 1 and isinstance(heading, str) and normalized(heading):
            candidates = [h for h in candidates if stream[:h[0]].rstrip().endswith(normalized(heading))]
        answers.append((answer, norm, mapping))
        hits.append(candidates)
    rows = []
    for si, slot in enumerate(slots):
        answer, norm, mapping = answers[si]
        whole = hits[si]
        bounds = {}
        cursor = 0
        for ri, run in enumerate(slot['runs']):
            text = run['text']
            if len(whole) == 1 and text.strip():
                a = cursor + len(text) - len(text.lstrip())
                b = cursor + len(text.rstrip())
                bounds[ri] = (whole[0][0] + bisect_left(mapping, a),
                              whole[0][0] + bisect_left(mapping, b))
            cursor += len(text)
        if not whole:
            lo = next((hits[j][0][1] for j in range(si - 1, -1, -1) if len(hits[j]) == 1), 0)
            hi = next((hits[j][0][0] for j in range(si + 1, len(slots)) if len(hits[j]) == 1), len(stream))
            # Only unique matches become anchors. Revisit ambiguous runs after
            # neighboring anchors narrow their region; never select among ties.
            pending = {i for i, r in enumerate(slot['runs']) if normalized(r['text'])}
            while pending:
                additions = {}
                for i in sorted(pending):
                    left = max((b[1] for j, b in bounds.items() if j < i), default=lo)
                    right = min((b[0] for j, b in bounds.items() if j > i), default=hi)
                    options = occurrences(stream, normalized(slot['runs'][i]['text']), left, right)
                    if len(options) == 1:
                        additions[i] = options[0]
                # Reject crossing anchors rather than inferring an order.
                conflicts = {i for i, a in additions.items() for j, b in additions.items()
                             if i < j and a[1] > b[0]}
                conflicts |= {j for i, a in additions.items() for j, b in additions.items()
                              if i < j and a[1] > b[0]}
                additions = {i: b for i, b in additions.items() if i not in conflicts}
                if not additions:
                    break
                bounds.update(additions)
                pending.difference_update(additions)
        for i, run in enumerate(slot['runs']):
            prov = run.get('provenance')
            if not isinstance(prov, dict) or run.get('placeholder'):
                continue
            row = dict(slot=slot['slot'], order=i + 1, text=run['text'], op=prov.get('op'),
                       provenance=prov, act=run.get('act_id'), segment=run.get('segment_index'))
            if not run['text'].strip():
                row['status'] = 'skipped-whitespace'
            elif i in bounds:
                a, b = bounds[i]
                row.update(status='resolved', start=ids[offsets[a]], end=ids[offsets[b - 1]])
            else:
                row.update(status='unresolved', reason=(f'answer occurs {len(whole)} times; no unique heading' if len(whole) > 1
                           else 'whole answer absent; run not unique between neighboring anchors'))
            rows.append(row)
    return rows, counts


def verify(rows, state):
    original, ids, _ = original_stream(state)
    index = {tuple(cid): i for i, cid in enumerate(ids)}
    for row in rows:
        if row['status'] == 'resolved':
            a, b = index[tuple(row['start'])], index[tuple(row['end'])]
            span = original[a:b + 1]
            if a > b or normalized(span) != normalized(row['text']):
                raise ValueError(f"span verification failed: {row['slot']} run {row['order']}")
            row['span_text'] = span


def utc_timestamp(value):
    """Validate an explicit ISO-8601 UTC timestamp; never consult the clock."""
    parsed = datetime.fromisoformat(value.replace("Z", "+00:00"))
    if "T" not in value or parsed.utcoffset() != timedelta(0):
        raise ValueError("derived-at must be an ISO-8601 UTC timestamp")
    return value


def acts(rows, derived_at):
    utc_timestamp(derived_at)
    result = []
    for row in rows:
        if row['status'] != 'resolved':
            continue
        node_id = f"prov.0017.{row['slot']}.{row['order']:03d}"
        if not re.fullmatch(r'[A-Za-z0-9._:-]{1,120}', node_id):
            raise ValueError(f'invalid span id: {node_id}')
        origin = row['provenance'].get('from')
        attrs = {k: row[k] for k in ('start', 'end', 'op', 'slot', 'act', 'segment')}
        attrs.update(doc='incubator-form', session='0017-f0718e48', layer=':pass3',
                     as_of='version 17 (0017-f0718e48)', sites=['fable-1.restore'],
                     derived_at=derived_at, label=label(row['op'], origin), by='fable-1 (derived from the incubator form)')
        if origin is not None:
            attrs['from'] = origin
        result.append(dict(op='add-node', id=node_id, kind='span', attrs=attrs))
    return result


def report(rows, counts, state):
    lines = ['# Incubator provenance spans', '', f"State revision: {state['rev']}", '',
             'Only restore-site characters tagged incubator-buffer-copy are matched, including deleted characters. '
             'Seed-site characters and later edits are excluded. Run orders are one-based across all runs in each slot.', '',
             'Character counts (including deleted):', '']
    lines.extend(f'- {site}: {n}' for site, n in sorted(counts.items()))
    lines.extend(['', f'Totals: {dict(Counter(r["status"] for r in rows))}', ''])
    for field in ('op', 'slot'):
        groups = defaultdict(Counter)
        for row in rows:
            groups[row[field]][row['status']] += 1
        lines.extend([f'## By {field}', '', '| Key | Resolved | Unresolved | Skipped whitespace |', '|---|---:|---:|---:|'])
        lines.extend(f"| {key} | {c['resolved']} | {c['unresolved']} | {c['skipped-whitespace']} |" for key, c in sorted(groups.items()))
        lines.append('')
    lines.extend(['## Unresolved', ''])
    for r in rows:
        if r['status'] == 'unresolved':
            lines.append(f"- {r['slot']} / {r['order']}: {r['text'][:50]!r} — {r['reason']}")
    lines.extend(['', '## Three longest verified spans', ''])
    for r in sorted((r for r in rows if r['status'] == 'resolved'), key=lambda r: len(r['span_text']), reverse=True)[:3]:
        lines.extend([f"### {r['slot']} / {r['order']}", '', 'Original span:', '', '```text', r['span_text'], '```', '',
                      'Extraction run:', '', '```text', r['text'], '```', ''])
    return '\n'.join(lines)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--extraction', required=True, type=Path)
    parser.add_argument('--state', required=True, type=Path)
    parser.add_argument('--out', required=True, type=Path)
    parser.add_argument('--derived-at', required=True, type=utc_timestamp)
    args = parser.parse_args(argv)
    try:
        extraction = json.loads(args.extraction.read_text())
        state = json.loads(args.state.read_text())
        rows, counts = build(extraction, state)
        verify(rows, state)  # All validation precedes any output writes.
        payload = acts(rows, args.derived_at)
        summary = report(rows, counts, state)
        args.out.mkdir(parents=True, exist_ok=True)
        (args.out / 'acts.json').write_text(json.dumps(payload, ensure_ascii=False, indent=2) + '\n')
        (args.out / 'report.md').write_text(summary)
        print(dict(Counter(r['status'] for r in rows)))
        return 0
    except (ValueError, KeyError, OSError) as error:
        print(str(error), file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
