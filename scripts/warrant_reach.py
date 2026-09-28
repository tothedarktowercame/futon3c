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


def analyze(namespace, closure_path):
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
    loaded = sorted(set(loaded))
    command = ['clj-kondo', '--lint', *loaded, '--config',
               '{:analysis {:var-definitions true :var-usages true} '
               ':output {:format :json}}']
    process = subprocess.run(command, capture_output=True, text=True)
    try:
        report = json.loads(process.stdout)
    except json.JSONDecodeError as error:
        raise RuntimeError(process.stdout + process.stderr) from error
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
    reached = set(roots)
    queue = list(roots)
    unbounded = set()
    while queue:
        key = queue.pop()
        if key in dynamic:
            unbounded.add(key)
        candidates = set(edges.get(key, ())) | set(top_edges.get(key[0], ()))
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
        'definition-count': len(defs),
        'reached-definition-count': len(reached),
        'reached-file-count': len({defs[key]['file'] for key in reached} | set(whole_files)),
        'reached-definitions': [defs[key] for key in sorted(reached)],
        'whole-files': whole_files,
        'whole-file-inputs': whole_file_inputs,
        'clj-kondo-findings': findings}
    return output


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument('--namespace', required=True)
    parser.add_argument('--closure', required=True)
    parser.add_argument('--output', required=True)
    args = parser.parse_args()
    result = analyze(args.namespace, args.closure)
    Path(args.output).write_text(json.dumps(result, indent=2, sort_keys=True) + '\n')
    print(json.dumps({key: result[key] for key in
                      ('namespace', 'status', 'definition-count',
                       'reached-definition-count', 'reached-file-count')}))


if __name__ == '__main__':
    try:
        main()
    except (OSError, ValueError, RuntimeError, subprocess.SubprocessError) as error:
        print(json.dumps({'status': 'refused', 'error': str(error)}), file=sys.stderr)
        sys.exit(1)
