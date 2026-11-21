#!/usr/bin/env python3
"""Offline slot anchors: --headings JSON --state JSON --out DIR. No hub access."""
import argparse
import json
from pathlib import Path
import re
import sys

try:
    from .together_provenance_spans import original_stream, normalize_map, normalized, occurrences
except ImportError:  # Direct script invocation.
    from together_provenance_spans import original_stream, normalize_map, normalized, occurrences


def build(headings, state):
    """Return acts, verification texts and report; refuse non-unique suffix matches."""
    original, ids, _ = original_stream(state)
    stream, offsets = normalize_map(original)
    slots = headings['slots']
    positions, errors, acts, texts = [], [], [], []
    previous = 0
    for slot in slots:
        matches = occurrences(stream, normalized(slot['heading']), previous)
        if len(matches) != 1:
            errors.append(f"{slot['slot']}: heading has {len(matches)} matches after previous heading")
            positions.append(None)
        else:
            positions.append(matches[0])
            previous = matches[0][1]
    lines = ['# Slot regions', '', f"State revision: {state['rev']}", '',
             'Original restore-site characters, including deleted ones; inclusive character IDs.', '']

    def add(slot, order, role, bounds, text):
        node_id = f"slot.0017.{slot['slot']}.{role}"
        if not re.fullmatch(r'[A-Za-z0-9._:-]{1,120}', node_id):
            raise ValueError(f'invalid id: {node_id}')
        start, end = ids[offsets[bounds[0]]], ids[offsets[bounds[1] - 1]]
        attrs = dict(doc='incubator-form', start=start, end=end, slot=slot['slot'],
                     role=role, order=order, session='0017-f0718e48', layer='pass3', by='fable-1')
        acts.append(dict(op='add-node', id=node_id, kind='span', attrs=attrs))
        texts.append(text)
        lines.append(f'- {role}: {json.dumps(start)} .. {json.dumps(end)}')

    for i, slot in enumerate(slots):
        lines.extend([f"## {i + 1}. {slot['slot']}", '', f"Heading: {slot['heading']!r}", ''])
        bounds = positions[i]
        if bounds is None:
            lines.append('Heading unplaced.')
            continue
        add(slot, i + 1, 'heading', bounds, slot['heading'])
        placeholders = slot.get('placeholders', [])
        if len(placeholders) > 1:
            errors.append(f"{slot['slot']}: multiple placeholder stretches cannot share one placeholder id")
        elif placeholders:
            limit = positions[i + 1][0] if i + 1 < len(slots) and positions[i + 1] else len(stream)
            matches = occurrences(stream, normalized(placeholders[0]), bounds[1], limit)
            if len(matches) == 1:
                add(slot, i + 1, 'placeholder', matches[0], placeholders[0])
            else:
                errors.append(f"{slot['slot']}: placeholder has {len(matches)} matches in its body")
                lines.append('Placeholder unplaced.')
        else:
            lines.append('No placeholder in extraction.')
        lines.append('')
    if len({a['id'] for a in acts}) != len(acts):
        errors.append('duplicate slot ids')
    lines.extend(['## Placement errors', ''] + (errors or ['None.']))
    return acts, texts, '\n'.join(lines) + '\n', errors


def verify(acts, texts, state):
    original, ids, _ = original_stream(state)
    index = {tuple(cid): i for i, cid in enumerate(ids)}
    if len(acts) != len(texts):
        raise ValueError('verification text count mismatch')
    for act, text in zip(acts, texts):
        attrs = act['attrs']
        a, b = index[tuple(attrs['start'])], index[tuple(attrs['end'])]
        if a > b or normalized(original[a:b + 1]) != normalized(text):
            raise ValueError(f"verification failed: {act['id']}")


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--headings', required=True, type=Path)
    parser.add_argument('--state', required=True, type=Path)
    parser.add_argument('--out', required=True, type=Path)
    args = parser.parse_args(argv)
    # Refuse reuse, so a failed run cannot leave an earlier acts file looking valid.
    if (args.out / 'acts.json').exists():
        print('output acts.json already exists; use a fresh output directory', file=sys.stderr)
        return 1
    try:
        headings = json.loads(args.headings.read_text())
        state = json.loads(args.state.read_text())
        acts, texts, report, errors = build(headings, state)
        args.out.mkdir(parents=True, exist_ok=True)
        (args.out / 'report.md').write_text(report)
        if errors:
            raise ValueError('; '.join(errors))
        verify(acts, texts, state)
        (args.out / 'acts.json').write_text(json.dumps(acts, ensure_ascii=False, indent=2) + '\n')
        print(f"Headings: {sum(a['attrs']['role'] == 'heading' for a in acts)}; "
              f"placeholders: {sum(a['attrs']['role'] == 'placeholder' for a in acts)}")
        return 0
    except (ValueError, KeyError, OSError) as error:
        print(error, file=sys.stderr)
        return 1


if __name__ == '__main__':
    sys.exit(main())
