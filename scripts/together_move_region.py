#!/usr/bin/env python3
"""Plan a call-notes move offline; never delete, post, or contact a service.

ops.json contains bare insert operations (wrap each as {op, parents} when later
posting). map.json maps compact JSON-encoded old IDs to new ID arrays. acts.json
contains bare supersede operations for incubator-form.notes. All validation
finishes before output files are created. Use a fresh output directory.
"""
import argparse
from collections import Counter
from copy import deepcopy
import json
from pathlib import Path
import re
import sys

SOURCE = 'incubator-form'
DESTINATION = 'incubator-call-notes'
SITE = 'fable-1.move'


def id_key(cid):
    return json.dumps(cid, ensure_ascii=False, separators=(',', ':'))


def expand(state):
    chars = []
    for run in state['runs']:
        site, n = run['id']
        for i, ch in enumerate(run['text']):
            chars.append(dict(id=[site, n + i], char=ch, attrs=deepcopy(run['attrs']),
                              deleted=bool(run.get('deleted', False))))
    if len({id_key(c['id']) for c in chars}) != len(chars):
        raise ValueError('duplicate character id')
    return chars


def current_nodes(workspace):
    replaced = set(workspace.get('derived', {}).values())
    return [(nid, node) for nid, node in workspace['nodes'].items() if nid not in replaced]


def extent(chars, index, attrs):
    a, b = index[id_key(attrs['start'])], index[id_key(attrs['end'])]
    if a > b:
        raise ValueError('reversed span')
    return [c for c in chars[a:b + 1] if not c['deleted']]


def region(chars, slots):
    index = {id_key(c['id']): i for i, c in enumerate(chars)}
    headings = [n['attrs'] for _, n in current_nodes(slots)
                if n.get('kind') == 'span' and n['attrs'].get('role') == 'heading'
                and n['attrs'].get('doc') == SOURCE]
    if not headings:
        raise ValueError('no slot headings')
    last_order = max(h['order'] for h in headings)
    last = [h for h in headings if h['order'] == last_order]
    if len(last) != 1:
        raise ValueError('ambiguous last slot')
    end = index[id_key(last[0]['end'])]
    after = [c for c in chars[end + 1:] if not c['deleted']]
    text = ''.join(c['char'] for c in after)
    first = re.search(r'\S', text)
    if first is None:
        raise ValueError('last answer has no live text')
    blank = re.search(r'\n[ \t]*\n', text[first.start():])
    if blank is None:
        raise ValueError('no blank line after last answer')
    boundary = first.start() + blank.end()
    start = re.search(r'\S', text[boundary:])
    if start is None:
        raise ValueError('empty move region')
    return after[boundary + start.start():]


def plan(state, notes, slots):
    chars = expand(state)
    index = {id_key(c['id']): i for i, c in enumerate(chars)}
    moved = region(chars, slots)
    mapping = {id_key(c['id']): [SITE, i + 1] for i, c in enumerate(moved)}
    ops = []
    offset = 0
    while offset < len(moved):
        end = offset + 1
        while end < len(moved) and moved[end]['attrs'] == moved[offset]['attrs']:
            end += 1
        attrs = deepcopy(moved[offset]['attrs'])
        attrs.update(source=SOURCE, moved_from=moved[offset]['id'])
        ops.append(dict(op='insert', id=[SITE, offset + 1],
                        after=[SITE, offset] if offset else None,
                        text=''.join(c['char'] for c in moved[offset:end]), attrs=attrs))
        offset = end
    seeded = expand({'runs': [dict(id=o['id'], text=o['text'], attrs=o['attrs']) for o in ops]})
    seed_index = {id_key(c['id']): i for i, c in enumerate(seeded)}
    if ''.join(c['char'] for c in seeded) != ''.join(c['char'] for c in moved):
        raise ValueError('seed text differs from region')
    acts, verified, untouched, straddles = [], [], [], []
    ignored = sorted(set(notes.get('derived', {}).values()) & set(notes['nodes']))
    for nid, node in current_nodes(notes):
        attrs = node.get('attrs', {})
        if node.get('kind') != 'span' or attrs.get('doc') != SOURCE:
            untouched.append(nid)
            continue
        old = extent(chars, index, attrs)
        inside = [c for c in old if id_key(c['id']) in mapping]
        if not inside:
            untouched.append(nid)
            continue
        if len(inside) != len(old):
            straddles.append(nid)
            continue
        # Trim tombstone endpoints inward, never outside the original extent.
        new_attrs = deepcopy(attrs)
        new_attrs.update(doc=DESTINATION, start=mapping[id_key(inside[0]['id'])],
                         end=mapping[id_key(inside[-1]['id'])])
        new_id = nid + '.m1'
        if not re.fullmatch(r'[A-Za-z0-9._:-]{1,120}', new_id):
            raise ValueError(f'invalid new note id: {new_id}')
        if new_id in notes['nodes'] or new_id in notes.get('edges', {}):
            raise ValueError(f'new note id already in use: {new_id}')
        before = ''.join(c['char'] for c in old)
        after = ''.join(c['char'] for c in extent(seeded, seed_index, new_attrs))
        if before != after:
            raise ValueError(f'note extent verification failed: {nid}')
        acts.append(dict(op='supersede', id=new_id, replaces=nid, kind=node['kind'], attrs=new_attrs))
        verified.append(dict(id=nid, before=before, after=after))
    text = ''.join(c['char'] for c in moved)
    authors = Counter(c['attrs'].get('author', '(not recorded)') for c in moved)
    lines = ['# Call-notes move plan', '',
             f"Input document revision: {state.get('rev')}",
             f"Region start: {moved[0]['id']}; end: {moved[-1]['id']}",
             f'Characters: {len(moved)}; insert operations: {len(ops)}',
             f'First 60: {text[:60]!r}', f'Last 60: {text[-60:]!r}', '',
             f'Characters by author: {dict(sorted(authors.items()))}', '',
             f'Notes moved: {len(acts)}; untouched: {len(untouched)}; straddling: {len(straddles)}',
             f'Moved: {[v["id"] for v in verified]}', f'Untouched: {untouched}',
             f'Straddles: {straddles}', f'Superseded nodes ignored: {ignored}', '',
             'Straddling is judged on live characters in the inclusive old span. '
             'Deleted endpoints are trimmed inward to its nearest live characters. '
             'Fully deleted spans are untouched. Missing or reversed endpoints refuse the plan.', '',
             'ops.json: bare insert operations for incubator-call-notes. '
             'acts.json: bare supersede operations for incubator-form.notes. '
             'map.json: compact JSON old-id strings as object keys, new-id arrays as values. '
             'No delete operations are produced.', '', '## Verified note extents', '']
    for v in verified:
        lines.extend([f"### {v['id']}", '', 'Before:', '```text', v['before'], '```',
                      'After:', '```text', v['after'], '```', ''])
    return ops, mapping, acts, '\n'.join(lines) + '\n'


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('state', 'notes', 'slots', 'out'):
        parser.add_argument('--' + name, required=True, type=Path)
    args = parser.parse_args(argv)
    try:
        if args.out.exists() and any(args.out.iterdir()):
            raise ValueError('output directory must be empty')
        ops, mapping, acts, report = plan(*(json.loads(p.read_text()) for p in
                                         (args.state, args.notes, args.slots)))
        args.out.mkdir(parents=True, exist_ok=True)
        for name, value in [('ops', ops), ('map', mapping), ('acts', acts)]:
            (args.out / (name + '.json')).write_text(json.dumps(value, ensure_ascii=False, indent=2) + '\n')
        (args.out / 'report.md').write_text(report)
        print(f'{len(mapping)} characters; {len(ops)} ops; {len(acts)} notes moved')
        return 0
    except (ValueError, KeyError, OSError) as error:
        print(error, file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
