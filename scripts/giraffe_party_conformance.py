#!/usr/bin/env python3
"""Read-only, stdlib conformance audit. Run with python3 -I.

Checks source values, never parses displayed reasons. Missing inputs produce
explicit ABSENT results; malformed inputs FAIL. An absence is not verification.
Lean references are relative to futon5/lean-arxana. Sortedness theorems have
sorted-input hypotheses: a failed check does not refute their Lean proofs.
The two Questor statements overlap necessarily on a unique-item source.
"""
import argparse
import importlib.util
import json
from pathlib import Path
import re
import sys

CHARACTERS = ('thor', 'merlin', 'questor', 'thyra')
READY = 'ready (one small handoff)'
CHECKS = {
    'thor_monotone_unblocks': 'Circus.Characters.thor_monotone_unblocks',
    'thyra_only_harm_items': 'Circus.Characters.thyra_only_harm_items',
    'thyra_monotone': 'Circus.Characters.thyra_monotone',
    'merlin_monotone_score': 'Circus.Characters.merlin_monotone_score',
    'questor_cost_gate': 'Circus.Characters.questor_cost_gate',
    'questor_never_lists_unready': 'Circus.Characters.questor_never_lists_unready',
    'party_nodup': 'Circus.Retro.partyVote_fresh_and_nodup',
    'party_turns': 'Circus.Retro.sweep_head_contributes; partyVote',
    'party_relative_order': 'Circus.drawFresh_survivor; Circus.Retro.drawFresh',
    'merlin_nat_model': 'Circus.Characters.MerlinRow.score (Nat representation)',
}


class Missing(ValueError):
    pass


def convergence(rows):
    """Independent source graph construction: undirected, two-hop, unfinished."""
    entities, aliases = {}, {}
    for row in rows:
        entities.setdefault(row['entity'], row)
        for alias in [row.get('entity'), row.get('name'),
                      *str(row.get('aka') or '').split(',')]:
            if alias and alias.strip():
                aliases.setdefault(alias.strip().lower(), row['entity'])
    graph = {key: set() for key in entities}
    for row in rows:
        for relation in str(row.get('relations') or '').split(';'):
            match = re.match(r'\s*[a-z-]+\s+(.*)', relation)
            if match:
                for name in re.sub(r'\([^)]*\)', '', match[1]).split(','):
                    target = aliases.get(name.strip().lower())
                    if target in graph and target != row['entity']:
                        graph[target].add(row['entity'])
                        graph[row['entity']].add(target)
    unfinished = {key for key, row in entities.items()
                  if row.get('status') in ('partly', 'hypothesised')}
    scores = {}
    for key, adjacent in graph.items():
        reachable = adjacent.copy()
        for neighbor in adjacent:
            reachable.update(graph[neighbor])
        scores[key] = len((reachable & unfinished) - {key})
    # Producer's eligibility uses the last row for duplicate entity statuses.
    last_status = {row['entity']: row.get('status') for row in rows}
    return scores, {key for key, value in last_status.items()
                    if value in ('partly', 'hypothesised')}


def audit(party, pass_data=None, pending=None, lexicon=None):
    """Return one independently evaluated result per named check.

    None means an unavailable source; empty lists mean known empty sources.
    Each checker reports all detected offenses (with source values).
    """
    def character(name):
        rows = [c for c in party['characters'] if c['id'] == name]
        if len(rows) != 1:
            raise ValueError('expected exactly one character: ' + name)
        return rows[0]['top']

    def source(value, name):
        if value is None:
            raise Missing('source-unavailable: ' + name)
        return value

    def pending_map():
        rows = source(pending, 'pending index')
        result = {r['id']: r for r in rows}
        if len(result) != len(rows):
            raise ValueError('pending-item-not-unique: Lean hdet not established')
        return result

    def pass_map():
        rows = source(pass_data, 'pass.edn')['order']
        result = {r['id']: r for r in rows}
        if len(result) != len(rows):
            raise ValueError('pass-item-not-unique')
        return result

    def need(mapping, item):
        if item not in mapping:
            raise Missing('source-item-unavailable: ' + item)
        return mapping[item]

    def natural(value, label):
        if value is None:
            raise Missing('source-value-unavailable: ' + label)
        if type(value) is not int or value < 0:
            raise ValueError('not a Nat: ' + label + '=' + repr(value))
        return value

    def pairs(values):
        return [{'left': a, 'right': b} for a, b in zip(values, values[1:])
                if a['key'] > b['key']]

    def thor():
        data = pass_map()
        return pairs([{'item': r['item'], 'key': [
            -natural(need(data, r['item']).get('unblocks'), r['item'] + '.unblocks'),
            -natural(need(data, r['item']).get('cone'), r['item'] + '.cone')]}
                      for r in character('thor')])

    def harm():
        data = pending_map()
        return [{'item': r['item'], 'harm': need(data, r['item']).get('harm')}
                for r in character('thyra')
                if not str(need(data, r['item']).get('harm') or '').strip()]

    def thyra():
        data = pass_map()
        return pairs([{'item': r['item'], 'key':
                       natural(data[r['item']]['rank'], r['item'] + '.rank')
                       if r['item'] in data and data[r['item']].get('rank') is not None
                       else 10**6} for r in character('thyra')])

    def merlin_values():
        data = pending_map()
        scores, unfinished = convergence(source(lexicon, 'lexicon'))
        values = []
        for row in character('merlin'):
            item = row['item']
            p = need(data, item)
            feature = re.sub(r'^[MET]-', '', item)
            own = need(scores, feature)
            named = max((scores[e] for e in p.get('entities', [])
                         if e in scores and e in unfinished), default=0)
            values.append({'item': item, 'own': own, 'named': named,
                           'score': own + named / 2,
                           'displayed_score': row.get('score'),
                           'label': p.get('pending_label'),
                           'key': [-(2 * own + named), item]})
        return values

    def merlin():
        values = merlin_values()
        return (pairs(values) + [v for v in values
                                if v['displayed_score'] != v['score']
                                or v['label'] == 'may be no longer wanted'])

    def questor():
        data = pending_map()
        return [{'item': r['item'], 'pending_label': need(data, r['item']).get('pending_label')}
                for r in character('questor')
                if need(data, r['item']).get('pending_label') != READY]

    def chosen():
        return party['party']['top']

    def nodup():
        seen, bad = set(), []
        for row in chosen():
            if row['item'] in seen:
                bad.append({'item': row['item'], 'chosen_by': row['chosen_by']})
            seen.add(row['item'])
        return bad

    def turns():
        # Declarative fresh-head rotation, independent of producer's pop loop.
        remaining = [(name, [r['item'] for r in character(name)]) for name in CHARACTERS]
        seen, expected = set(), []
        while remaining and len(expected) < len(chosen()):
            name, items = remaining.pop(0)
            fresh = [item for item in items if item not in seen]
            if fresh:
                item = fresh[0]
                seen.add(item)
                expected.append({'item': item, 'chosen_by': name})
                remaining.append((name, fresh[1:]))
        actual = [{k: r[k] for k in ('item', 'chosen_by')} for r in chosen()]
        return [] if expected == actual else [{'expected': expected, 'actual': actual}]

    def relative():
        bad = []
        for name in CHARACTERS:
            source_items = [r['item'] for r in character(name)]
            offset = 0
            for row in chosen():
                if row['chosen_by'] == name:
                    try:
                        offset = source_items.index(row['item'], offset) + 1
                    except ValueError:
                        bad.append({'item': row['item'], 'chosen_by': name,
                                    'remaining_source_order': source_items[offset:]})
        return bad

    functions = [thor, harm, thyra, merlin, questor, questor, nodup, turns,
                 relative, lambda: [v for v in merlin_values() if v['named'] % 2]]
    results = []
    for (name, theorem), check in zip(CHECKS.items(), functions):
        try:
            offenses = check()
            status, detail = ('FAIL', offenses) if offenses else ('PASS', [])
        except Missing as exc:
            status, detail = 'ABSENT', {'kind': 'missing-source', 'reason': str(exc)}
        except (IndexError, KeyError, TypeError, ValueError) as exc:
            status, detail = 'FAIL', {'kind': 'invalid-input', 'reason': str(exc)}
        results.append({'check': name, 'theorem': theorem, 'status': status, 'detail': detail})
    return results


def load(path, edn=False):
    if not path or not Path(path).is_file():
        return None
    text = Path(path).read_text(encoding='utf-8')
    if not edn:
        return json.loads(text)
    # Reuse only the existing stdlib EDN decoder, never producer/check logic.
    spec = importlib.util.spec_from_file_location('gcc_edn', Path(__file__).with_name('gcc_preview.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module.edn_load(text)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('party', type=Path)
    parser.add_argument('--run', type=Path)
    parser.add_argument('--pending', type=Path)
    parser.add_argument('--lexicon', type=Path)
    args = parser.parse_args(argv)
    try:
        party = load(args.party)
        if party is None:
            results = [{'check': k, 'theorem': t, 'status': 'ABSENT',
                        'detail': {'kind': 'missing-party', 'path': str(args.party)}}
                       for k, t in CHECKS.items()]
        else:
            sources = party['sources']
            run = args.run or Path.home() / 'storage/giraffe' / sources['giraffe_run']
            if run.name != sources['giraffe_run']:
                raise ValueError('run-identity-mismatch: ' + str(run))
            pending = args.pending or sources.get('pending_index')
            lexicon = args.lexicon or sources.get('lexicon', '').split(' (convergence')[0]
            results = audit(party, load(run / 'pass.edn', edn=True), load(pending), load(lexicon))
            print('generated_at:', party.get('generated_at'), 'run:', run.name)
            print('sources:', json.dumps({'pending': str(pending), 'lexicon': str(lexicon)}))
            print('Scope: current source values; party.json does not retain source digests.')
        print('CHECK                        RESULT  LEAN')
        for row in results:
            print(f"{row['check']:28} {row['status']:7} {row['theorem']}")
            if row['detail']:
                print(json.dumps(row['detail'], ensure_ascii=False, sort_keys=True))
        return int(any(row['status'] == 'FAIL' for row in results))
    except (OSError, IndexError, ValueError, KeyError, TypeError) as exc:
        print('FAIL invalid-input:', str(exc))
        return 1


if __name__ == '__main__':
    sys.dont_write_bytecode = True
    raise SystemExit(main())
