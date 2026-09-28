#!/usr/bin/env python3
"""Prototype source reachability over one recorded test-run file set."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess
import sys
from urllib.parse import unquote, urlparse

from warrant_index import edn

RESOLVERS = {'resolve', 'requiring-resolve', 'ns-resolve', 'find-var'}
IMPLEMENTATION_FORMS = {'extend-type', 'extend-protocol', 'defrecord', 'reify'}


def file_path(url):
    parsed = urlparse(url)
    return unquote(parsed.path) if parsed.scheme == 'file' else None


def source_rows(filename, row, end_row):
    lines = Path(filename).read_text().splitlines(keepends=True)
    return ''.join(lines[row - 1:end_row])


def digest(text):
    return hashlib.sha256(text.encode()).hexdigest()


def definition(row, synthetic=False):
    item = {'ns': row['ns'], 'name': row['name'], 'file': row['filename'],
            'row': row['row'], 'end-row': row['end-row']}
    item['sha256'] = digest(source_rows(item['file'], item['row'], item['end-row']))
    if synthetic:
        item['synthetic'] = True
    return item


def contains(row, usage):
    return (row['filename'] == usage.get('filename')
            and row['row'] <= usage.get('row', 0) <= row['end-row'])


def literal_resolutions(text, namespace):
    found = []
    qualified = re.compile(
        r'\((?:[\w.-]+/)?(?:requiring-resolve|find-var|resolve)\s+'
        r"(?:'|\(quote\s+)([\w.-]+/[\w!?*+<>=.-]+)")
    local = re.compile(
        r'\((?:[\w.-]+/)?resolve\s+(?:\'|\(quote\s+)'
        r'([\w!?*+<>=.-]+)')
    ns_resolve = re.compile(
        r'\((?:[\w.-]+/)?ns-resolve\s+(?:\'|\(quote\s+)'
        r'([\w.-]+)\)?\s+(?:\'|\(quote\s+)([\w!?*+<>=.-]+)')
    found.extend(value.split('/', 1) for value in qualified.findall(text))
    found.extend((namespace, value) for value in local.findall(text))
    found.extend(ns_resolve.findall(text))
    return [tuple(value) for value in found]


def called_keys(called_path):
    if not called_path:
        return set()
    value = edn(Path(called_path).read_text())
    rows = value.get('called-definitions', []) if isinstance(value, dict) else value
    return {(str(row['ns']), str(row['name'])) for row in rows}


def analysis_report(loaded, cache_path=None):
    reports = []
    for filename in loaded:
        cache_key = hashlib.sha256(Path(filename).read_bytes()).hexdigest()
        cached = Path(cache_path) / f'{cache_key}.json' if cache_path else None
        if cached and cached.is_file():
            report = json.loads(cached.read_text())
            for item in report.get('findings', []):
                item['filename'] = filename
            for rows in report.get('analysis', {}).values():
                if isinstance(rows, list):
                    for item in rows:
                        if isinstance(item, dict) and 'filename' in item:
                            item['filename'] = filename
            reports.append(report)
            continue
        # clj-kondo's own cache is locked per process; parallel checks collide on it.
        command = ['clj-kondo', '--cache', 'false', '--lint', filename, '--config',
                   '{:analysis {:var-definitions true :var-usages true} '
                   ':output {:format :json}}']
        process = subprocess.run(command, capture_output=True, text=True)
        try:
            report = json.loads(process.stdout)
        except json.JSONDecodeError as error:
            raise RuntimeError(process.stdout + process.stderr) from error
        if cached:
            cached.parent.mkdir(parents=True, exist_ok=True)
            cached.write_text(json.dumps(report, sort_keys=True))
        reports.append(report)
    return {
        'findings': [item for report in reports for item in report.get('findings', [])],
        'analysis': {
            'var-definitions': [item for report in reports
                                for item in report.get('analysis', {}).get('var-definitions', [])],
            'var-usages': [item for report in reports
                           for item in report.get('analysis', {}).get('var-usages', [])]}}


def analyze_files(namespace, loaded, resources, called=None, cache_path=None):
    loaded = sorted(set(loaded))
    resources = sorted(set(resources))
    report = analysis_report(loaded, cache_path)
    findings = report.get('findings', [])
    error_files = {
        finding.get('filename')
        for finding in findings
        if finding.get('level') == 'error' and finding.get('filename') in loaded
    }
    analysis = report.get('analysis', {})
    definitions = analysis.get('var-definitions')
    usages = analysis.get('var-usages')
    if not isinstance(definitions, list) or not isinstance(usages, list):
        raise RuntimeError('clj-kondo omitted var-definitions or var-usages')

    defs = {}
    rows = {}
    for row in definitions:
        if not all(key in row for key in ('ns', 'name', 'filename', 'row', 'end-row')):
            raise RuntimeError('clj-kondo definition omitted a required source field')
        key = (row['ns'], row['name'])
        defs[key] = definition(row)
        rows[key] = row

    special = []
    for usage in usages:
        if usage.get('from-var'):
            continue
        form = usage.get('name')
        if form == 'defmethod' or form in IMPLEMENTATION_FORMS:
            row = {'ns': usage['from'],
                   'name': f'<{form}:{usage["row"]}>',
                   'filename': usage['filename'], 'row': usage['row'],
                   'end-row': usage.get('end-row', usage['row']),
                   'form': form}
            key = (row['ns'], row['name'])
            defs[key] = definition(row, synthetic=True)
            rows[key] = row
            special.append((key, row))

    edges = {key: set() for key in defs}
    top_edges = {}
    # A top-level form can act on a definition of another namespace
    # (alter-var-root, a bare call). target -> {(namespace, file) of the form}.
    top_touch = {}
    resolver_calls = {}
    for usage in usages:
        source = None
        if usage.get('from-var'):
            source = (usage.get('from'), usage['from-var'])
        else:
            matching = [(key, row) for key, row in special if contains(row, usage)]
            if matching:
                source = min(matching, key=lambda pair: pair[1]['end-row'] - pair[1]['row'])[0]
        target = (usage.get('to'), usage.get('name'))
        if source in defs and target in defs:
            edges[source].add(target)
        elif source is None and usage.get('from'):
            top_edges.setdefault(usage['from'], set()).add(target)
            if target in defs and usage.get('filename'):
                top_touch.setdefault(target, set()).add(
                    (usage['from'], usage['filename']))
        if source in defs and usage.get('name') in RESOLVERS and usage.get('to') == 'clojure.core':
            resolver_calls.setdefault(source, []).append(usage)

    for key, row in special:
        form = row['form']
        inside = [usage for usage in usages if contains(row, usage)]
        if form == 'defmethod':
            for usage in inside:
                if usage.get('defmethod'):
                    target = (usage.get('to'), usage.get('name'))
                    if target in defs:
                        edges[target].add(key)
        else:
            for usage in inside:
                target = (usage.get('to'), usage.get('name'))
                target_row = rows.get(target, {})
                if target_row.get('defined-by') == 'clojure.core/defprotocol':
                    edges[target].add(key)

    dynamic = set()
    for key, calls in resolver_calls.items():
        row = rows[key]
        text = source_rows(row['filename'], row['row'], row['end-row'])
        literals = literal_resolutions(text, row['ns'])
        for target in literals:
            if target in defs:
                edges[key].add(target)
        if len(literals) < len(calls):
            dynamic.add(key)

    test_files = {path for path in loaded
                  if '/test/' in path and any(row.get('ns') == namespace
                                               and row.get('filename') == path
                                               for row in definitions)}
    roots = {key for key, row in rows.items()
             if row['filename'] in test_files or row['filename'] in error_files}
    called = called or set()
    roots.update(called & set(defs))
    reached = set(roots)
    queue = list(roots)
    unbounded = set()
    top_level_files = set()
    while queue:
        key = queue.pop()
        if key in dynamic:
            unbounded.add(key)
        candidates = set(edges.get(key, ())) | set(top_edges.get(key[0], ()))
        for touching_ns, touching_file in top_touch.get(key, ()):
            top_level_files.add(touching_file)
            candidates |= set(top_edges.get(touching_ns, ()))
        for target in candidates:
            if target in defs and target not in reached:
                reached.add(target)
                queue.append(target)

    whole_file_inputs = ([{'file': path, 'reason': 'test-file'}
                          for path in sorted(test_files)]
                         + [{'file': path, 'reason': 'resource'}
                            for path in sorted(set(resources))]
                         + [{'file': path,
                             'reason': 'clj-kondo-error-whole-file-input'}
                            for path in sorted(error_files
                                               - test_files
                                               - set(resources))])
    whole_files = sorted({entry['file'] for entry in whole_file_inputs})
    if unbounded:
        reached = set(defs)
        whole_files = sorted(set(whole_files) | set(loaded))
        whole_file_inputs = ([{'file': path, 'reason': 'unbounded'}
                              for path in whole_files])
    output = {
        'namespace': namespace,
        'status': 'unbounded' if unbounded else 'bounded',
        'unbounded-definitions': [defs[key] for key in sorted(unbounded)],
        'loaded-files': loaded,
        'resources': sorted(set(resources)),
        'test-files': sorted(test_files),
        'called-definitions': [list(key) for key in sorted(called)],
        'definition-count': len(defs),
        'all-definitions': [defs[key] for key in sorted(defs)],
        'reached-definition-count': len(reached),
        'reached-file-count': len({defs[key]['file'] for key in reached} | set(whole_files)),
        'reached-definitions': [defs[key] for key in sorted(reached)],
        'top-level-files': sorted(top_level_files),
        'whole-files': whole_files,
        'whole-file-inputs': whole_file_inputs,
        'clj-kondo-findings': findings}
    return output


def analyze(namespace, closure_path, called_path=None, cache_path=None):
    closure = edn(Path(closure_path).read_text())
    loaded = []
    resources = []
    for entry in closure:
        path = file_path(entry.get('url'))
        if not path or not Path(path).is_file():
            continue
        if entry.get('ns', '').startswith('resource:'):
            resources.append(path)
        elif path.endswith(('.clj', '.cljc')):
            loaded.append(path)
        else:
            resources.append(path)
    return analyze_files(namespace, loaded, resources, called_keys(called_path), cache_path)


def remainder_hash(filename, definitions):
    """Hash non-definition text; blank separator lines are position-insensitive."""
    lines = Path(filename).read_text().splitlines(keepends=True)
    removed = set()
    for item in definitions:
        removed.update(range(item['row'] - 1, item['end-row']))
    remainder = ''.join(line for index, line in enumerate(lines)
                        if index not in removed and line.strip())
    return digest(remainder)


def qualify_record(result):
    reached_files = ({item['file'] for item in result['reached-definitions']}
                     | set(result.get('top-level-files', ())))
    by_file = {filename: [] for filename in reached_files}
    for item in result['all-definitions']:
        if item['file'] in reached_files:
            by_file[item['file']].append(item)
    result['remainders'] = [
        {'file': filename, 'sha256': remainder_hash(filename, definitions)}
        for filename, definitions in sorted(by_file.items())]
    result['whole-file-hashes'] = [
        {'file': filename, 'sha256': hashlib.sha256(Path(filename).read_bytes()).hexdigest()}
        for filename in result['whole-files']]
    # retained to make the recorded dependency population explicit
    result['dependency-files'] = sorted(reached_files | set(result['whole-files']))
    result.pop('all-definitions', None)
    return result


def definition_identity(item):
    return item['ns'], item['name'], item['file']


def check_record(record_path, cache_path=None):
    recorded = json.loads(Path(record_path).read_text())
    missing_files = sorted(path for path in recorded['loaded-files']
                           + recorded.get('resources', []) if not Path(path).is_file())
    differences = [{'kind': 'file-missing', 'file': path} for path in missing_files]
    if missing_files:
        return differences
    if recorded['status'] == 'unbounded':
        for item in recorded.get('whole-file-hashes', []):
            current = hashlib.sha256(Path(item['file']).read_bytes()).hexdigest()
            if current != item['sha256']:
                differences.append({'kind': 'whole-file-changed', 'file': item['file']})
        return differences

    called = {tuple(item) for item in recorded.get('called-definitions', [])}
    current = qualify_record(analyze_files(recorded['namespace'],
                                           recorded['loaded-files'],
                                           recorded.get('resources', []),
                                           called, cache_path))
    old_defs = {definition_identity(item): item
                for item in recorded['reached-definitions']}
    new_defs = {definition_identity(item): item
                for item in current['reached-definitions']}
    for key in sorted(old_defs.keys() - new_defs.keys()):
        differences.append({'kind': 'definition-missing', 'definition': list(key)})
    for key in sorted(new_defs.keys() - old_defs.keys()):
        differences.append({'kind': 'definition-added', 'definition': list(key)})
    for key in sorted(old_defs.keys() & new_defs.keys()):
        if old_defs[key]['sha256'] != new_defs[key]['sha256']:
            differences.append({'kind': 'definition-changed', 'definition': list(key)})
    old_remainders = {item['file']: item['sha256'] for item in recorded['remainders']}
    new_remainders = {item['file']: item['sha256'] for item in current['remainders']}
    for filename in sorted(old_remainders.keys() & new_remainders.keys()):
        if old_remainders[filename] != new_remainders[filename]:
            differences.append({'kind': 'remainder-changed', 'file': filename})
    for filename in sorted(new_remainders.keys() - old_remainders.keys()):
        differences.append({'kind': 'remainder-added', 'file': filename})
    for item in recorded.get('whole-file-hashes', []):
        current_hash = hashlib.sha256(Path(item['file']).read_bytes()).hexdigest()
        if current_hash != item['sha256']:
            differences.append({'kind': 'whole-file-changed', 'file': item['file']})
    return differences


def main():
    parser = argparse.ArgumentParser()
    commands = parser.add_subparsers(dest='command', required=True)
    record = commands.add_parser('record')
    record.add_argument('--namespace', required=True)
    record.add_argument('--closure', required=True)
    record.add_argument('--called')
    record.add_argument('--output', required=True)
    record.add_argument('--cache')
    check = commands.add_parser('check')
    check.add_argument('--record', required=True)
    check.add_argument('--cache')
    args = parser.parse_args()
    if args.command == 'record':
        result = qualify_record(analyze(args.namespace, args.closure, args.called, args.cache))
        Path(args.output).write_text(json.dumps(result, indent=2, sort_keys=True) + '\n')
        print(json.dumps({key: result[key] for key in
                          ('namespace', 'status', 'definition-count',
                           'reached-definition-count', 'reached-file-count')}))
    else:
        differences = check_record(args.record, args.cache)
        answer = {'status': 'stale', 'differences': differences} if differences else {'status': 'current'}
        print(json.dumps(answer, sort_keys=True))
        if differences:
            sys.exit(1)


if __name__ == '__main__':
    try:
        main()
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as error:
        print(json.dumps({'status': 'refused', 'error': str(error)}), file=sys.stderr)
        sys.exit(1)
