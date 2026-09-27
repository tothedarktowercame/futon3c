#!/usr/bin/env python3
"""Validate typed-hole grades and pointers; exit 1 on failures.

Usage: python3 scripts/typed_hole_check.py HOLE.edn [HOLE.edn ...]
Pointers and :cascade resolve under /home/joe/code, including for /tmp copies.
Read notes are required, not certified truthful. Rule 6 checks the referenced
artifact for a top-level run identity (or a sequence of such maps), following
CONVERGENCE's machine-record? rule; it remains a warning, not proof of a run.
"""
import argparse
from functools import lru_cache
import json
from pathlib import Path
import re
import subprocess

ROOT = Path('/home/joe/code').resolve()
RUN_KEYS = {':run-id', ':runId', ':startedAt', ':tick-id', ':wm-run-id'}
RANK = {':none': 0, ':named': 1, ':read': 2, ':witnessed': 3}
EDN = '''(require '[clojure.edn :as e] '[clojure.walk :as w] '[cheshire.core :as j])
(let [v (e/read-string (slurp (first *command-line-args*)))]
 (print (j/generate-string (w/postwalk #(if (keyword? %) (str %) %) v))))'''


@lru_cache(maxsize=None)
def edn(path):
    result = subprocess.run(['bb', '-e', EDN, str(path)], capture_output=True,
                            text=True, timeout=30)
    if result.returncode:
        raise ValueError(f'cannot read EDN {path}: {result.stderr.strip()}')
    return json.loads(result.stdout)


def pointer(value):
    if not isinstance(value, str) or not value.strip():
        raise ValueError('pointer must be a nonempty string')
    match = re.fullmatch(r'([^:\s]+)(?::(\d+)(?:-(\d+))?)?', value.strip())
    if not match:
        raise ValueError(f'malformed pointer {value!r}')
    name, low, high = match.groups()
    path = (ROOT / name).resolve()
    if not path.is_relative_to(ROOT):
        raise ValueError(f'pointer escapes code root: {value}')
    if not path.is_file():
        raise ValueError(f'file not found: {value}')
    if low:
        lo, hi = int(low), int(high or low)
        with path.open(encoding='utf-8') as source:
            count = sum(1 for _ in source)
        if not 1 <= lo <= hi <= count:
            raise ValueError(f'line range outside 1..{count}: {value}')
    return path


def pointers(value):
    """Find explicit :pointers collections, including the wiring-point entries."""
    if isinstance(value, dict):
        for key, child in value.items():
            if key == ':pointers':
                if not isinstance(child, list):
                    yield child
                else:
                    yield from child
            else:
                yield from pointers(child)
    elif isinstance(value, list):
        for child in value:
            yield from pointers(child)


def machine_record(path):
    try:
        record = edn(path)
        records = [record] if isinstance(record, dict) else record
        return (isinstance(records, list)
                and any(isinstance(r, dict) and RUN_KEYS.intersection(r) for r in records))
    except (ValueError, OSError, subprocess.TimeoutExpired):
        return False


def check(path):
    errors, warnings = [], []

    def fail(token, rule, reason):
        errors.append(f'{token} rule {rule}: {reason}')

    def checked_pointers(owner, value):
        resolved = []
        for ref in pointers(value):
            try:
                resolved.append(pointer(ref))
            except (ValueError, OSError) as exc:
                fail(owner, 4, str(exc))
        return resolved

    try:
        doc = edn(str(Path(path).resolve()))
        fills, hole = doc[':fills'], doc[':hole']
        if not isinstance(fills, dict) or not isinstance(hole, dict):
            raise ValueError(':fills and :hole must be maps')
        for token, fill in fills.items():
            if not isinstance(fill, dict):
                fail(token, 1, 'fill must be a map')
                continue
            rung, satiety = fill.get(':rung'), fill.get(':satiety')
            if rung not in RANK:
                fail(token, 1, f'invalid or missing :rung {rung!r}')
            if satiety not in (':hungry', ':partial', ':full'):
                fail(token, 1, f'invalid or missing :satiety {satiety!r}')
            if satiety == ':partial' and RANK.get(rung, -1) < RANK[':read']:
                fail(token, 2, ':partial requires :read or :witnessed')
            if satiety == ':full' and rung != ':witnessed':
                fail(token, 2, ':full requires :witnessed')
            note = fill.get(':read-note')
            if rung in (':read', ':witnessed') and not (isinstance(note, str) and note.strip()):
                fail(token, 3, 'nonempty :read-note required')
            refs = checked_pointers(token, fill)
            if rung == ':witnessed' and not any(machine_record(p) for p in refs):
                warnings.append(f'{token} rule 6: :witnessed has no referenced machine record '
                                'with a top-level run identity')
        checked_pointers(':document', {k: v for k, v in doc.items() if k != ':fills'})
        cascade = edn(str(pointer(doc[':cascade'])))
        sets = {':fills': set(fills), ':hole/:hungry-for': set(hole[':hungry-for']),
                ':cascade/:tokens': set(cascade[':tokens'])}
        for token in sorted(set.union(*sets.values())):
            missing = [name for name, tokens in sets.items() if token not in tokens]
            if missing:
                fail(token, 5, 'missing from ' + ', '.join(missing))
        want, expected = set(hole[':want']), set(cascade[':want'])
        for token in sorted(want ^ expected):
            fail(token, 5, ':hole/:want differs from :cascade/:want')
    except (ValueError, OSError, KeyError, TypeError, subprocess.TimeoutExpired) as exc:
        fail(':document', 'schema/reference', str(exc))
    for message in errors:
        print('FAIL ' + message)
    for message in warnings:
        print('WARN ' + message)
    print(f'{path}: {"FAIL" if errors else "OK"}; {len(errors)} failures; {len(warnings)} warnings')
    return not errors


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('holes', nargs='+')
    args = parser.parse_args()
    return 0 if all([check(p) for p in args.holes]) else 1


if __name__ == '__main__':
    raise SystemExit(main())
