#!/usr/bin/env python3
"""Read-only, stdlib conformance audit. Run with python3 -I.

Checks source values, never parses displayed reasons. Missing inputs produce
explicit ABSENT results; malformed inputs FAIL. An absence is not verification.
Lean references are relative to futon5/lean-arxana. Sortedness theorems have
sorted-input hypotheses: a failed check does not refute their Lean proofs.
The two Questor statements overlap necessarily on a unique-item source.
"""
import argparse
import hashlib
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
    'run_file_digests': 'Circus.Reproduction.Receipt.passInputDigest',
    'one_current_reading': 'Circus.Supersession.OneCurrent',
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


def audit(party, pass_data=None, pending=None, lexicon=None, run=None):
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

    # Merlin's half-integral scores are exact in the Lean model since futon5
    # afdb6eb (MerlinRow.score is the doubled score), so no representation check.
    functions = [thor, harm, thyra, merlin, questor, questor, nodup, turns,
                 relative]
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
    results.append(run_file_digests(run))
    results.append(one_current_reading(run))
    return results


def run_file_digests(run):
    """Hash only manifest-listed files contained in the resolved run directory."""
    def result(status, detail):
        return {'check': 'run_file_digests',
                'theorem': CHECKS['run_file_digests'], 'status': status, 'detail': detail}

    if run is None:
        return result('ABSENT', {'files': ['manifest.edn']})
    try:
        root = Path(run).resolve()

        def contained(name):
            if not isinstance(name, str) or not name or Path(name).is_absolute():
                raise ValueError('invalid run-relative file name')
            path = (root / name).resolve()
            if not path.is_relative_to(root):
                raise ValueError('file escapes run directory')
            return path

        manifest_path = contained('manifest.edn')
        if not manifest_path.exists():
            return result('ABSENT', {'files': ['manifest.edn']})
        manifest = load(manifest_path, edn=True, strict=True)
        if not isinstance(manifest, dict) or not isinstance(manifest.get('files'), list):
            raise ValueError('manifest files must be a list')
        entries = manifest['files']
        # Validate every path before opening any listed file, including symlinks.
        paths = []
        for entry in entries:
            if not isinstance(entry, dict):
                raise ValueError('invalid manifest file entry')
            name, recorded = entry.get('file'), entry.get('sha256')
            path = contained(name)
            if not isinstance(recorded, str) or not re.fullmatch(r'[0-9a-f]{64}', recorded):
                raise ValueError('invalid recorded SHA-256')
            paths.append((name, path, recorded))
        missing, mismatches = [], []
        for name, path, recorded in paths:
            if not path.is_file():
                missing.append(name)
                continue
            actual = hashlib.sha256(path.read_bytes()).hexdigest()
            if actual != recorded:
                mismatches.append({'file': name, 'recorded': recorded, 'actual': actual})
        if mismatches:
            return result('FAIL', {'mismatches': mismatches, 'missing': missing})
        # passInputDigest is about input.edn: an empty list, or a manifest that
        # does not list the pass input, verifies nothing about it.
        if 'input.edn' not in {name for name, _, _ in paths}:
            return result('ABSENT', {'files': ['input.edn'], 'reason': 'not listed in manifest'})
        if missing:
            return result('ABSENT', {'files': missing})
        return result('PASS', {'files': len(entries)})
    except (OSError, IndexError, KeyError, TypeError, ValueError, RuntimeError):
        # Do not echo malformed payloads or exception strings containing them.
        return result('FAIL', {'kind': 'invalid-manifest-or-file', 'file': 'manifest.edn'})


def one_current_reading(run):
    """Check only the generation pinned by manifest readings-generation.path.

    src/futon3c/giraffe/corpus_generation.clj:75-95 removes every target :id
    from batch-*.jsonl before appending its replacement. Lines 158-218 group
    ALL staged batch records by :id and refuse duplicates, without filtering
    a status field. A generation thus holds current records; superseded
    versions remain in older generations, not alongside their replacements.
    The record's id IS the item id (there is no separate reading-record id).
    No current-pointer fallback; resolve the pinned path once before scanning.
    """
    def result(status, detail):
        return {'check': 'one_current_reading',
                'theorem': CHECKS['one_current_reading'], 'status': status, 'detail': detail}

    if run is None:
        return result('ABSENT', {'missing': 'manifest.edn'})
    try:
        run = Path(run).resolve()
        manifest_path = (run / 'manifest.edn').resolve()
        if not manifest_path.is_relative_to(run):
            return result('FAIL', {'kind': 'manifest-outside-run'})
        if not manifest_path.is_file():
            return result('ABSENT', {'missing': 'manifest.edn'})
        manifest = load(manifest_path, edn=True, strict=True)
        if not isinstance(manifest, dict):
            return result('FAIL', {'kind': 'invalid-manifest'})
        pinned = manifest.get('readings-generation')
        if not isinstance(pinned, dict) or not pinned.get('path'):
            return result('ABSENT', {'missing': 'readings-generation.path'})
        path = pinned['path']
        if not isinstance(path, str) or not Path(path).is_absolute():
            return result('FAIL', {'kind': 'invalid-generation-path'})
        generation = Path(path).resolve()
        if not generation.is_dir():
            return result('ABSENT', {'missing': path})
        batches = sorted(generation.rglob('batch-*.jsonl'))
        if not batches:
            return result('ABSENT', {'missing': 'batch-*.jsonl', 'generation': path})
        grouped, errors, records = {}, [], 0
        for batch in batches:
            if not batch.resolve().is_relative_to(generation):
                return result('FAIL', {'kind': 'batch-outside-generation'})
            with batch.open(encoding='utf-8') as stream:
                for line_number, line in enumerate(stream, 1):
                    if not line.strip():
                        continue
                    records += 1
                    location = {'file': str(batch.relative_to(generation)), 'line': line_number}
                    try:
                        record = json.loads(line)
                    except ValueError:
                        errors.append(dict(location, kind='unparseable-record'))
                        continue
                    item = record.get('id') if isinstance(record, dict) else None
                    if not isinstance(item, str) or not item.strip():
                        errors.append(dict(location, kind='missing-item-id'))
                        continue
                    grouped.setdefault(item, []).append(item)
        duplicates = [{'item': item, 'record_ids': ids}
                      for item, ids in sorted(grouped.items()) if len(ids) > 1]
        if errors or duplicates:
            return result('FAIL', {'duplicates': duplicates, 'errors': errors})
        return result('PASS', {'records': records, 'items': len(grouped)})
    except FileNotFoundError:
        return result('ABSENT', {'missing': 'pinned-generation-or-file'})
    except (OSError, IndexError, KeyError, TypeError, ValueError, RuntimeError):
        return result('FAIL', {'kind': 'invalid-manifest-or-generation'})


def load(path, edn=False, strict=False):
    if not path or not Path(path).is_file():
        return None
    text = Path(path).read_text(encoding='utf-8')
    if not edn:
        return json.loads(text)
    # Reuse only the existing stdlib EDN decoder, never producer/check logic.
    spec = importlib.util.spec_from_file_location('gcc_edn', Path(__file__).with_name('gcc_preview.py'))
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    if strict:
        reader = module._EdnReader(text)
        value = reader.read()
        if reader.pos != len(reader.tokens):
            raise ValueError('trailing EDN input')
        return value
    return module.edn_load(text)


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('party', type=Path, nargs='?',
                        default=Path.home() / 'storage/gcc-preview/party.json')
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
            results = audit(party, load(run / 'pass.edn', edn=True), load(pending), load(lexicon), run=run)
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
