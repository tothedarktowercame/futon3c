#!/usr/bin/env python3
"""Instant local test-registry checks over SQLite.

The immutable registry entries and run index are the authority. The older
`warrants` and `files` tables are retained for compatibility with the first
index version, but are never read here: rows imported from futon1b therefore
cannot become current local warrants. No command in this script uses a network
or the git-tracked namespace ledger.
"""
import argparse
from datetime import datetime, timezone
import hashlib
import json
import os
from pathlib import Path
import re
import sqlite3
import sys

ROOT = Path(__file__).resolve().parents[1]
DB = ROOT.parent / 'storage/test-registry/warrant-index.sqlite'
WIRE_LEDGER_TEST = ROOT / 'test/futon3c/diagramprover/wm_wire_ledger_test.clj'
REACH_DIR = ROOT.parent / 'storage/test-registry/reach-records'
# Tests that keep the whole-file rule on purpose. The definition rule lets a
# warrant stay current when an unreached part of a loaded file changes, even if
# the change stops the file loading; these tests load every file and are rerun
# on any change, so that break is seen by one run.
FILE_RULE_NAMESPACES = frozenset({'futon3c.diagramprover.wm-wire-everything-loads-test'})
TOKEN = re.compile(r'\s+|,[\s,]*|;[^\n]*|"(?:\\.|[^"\\])*"|#\{|[{}\[\]()]|[^\s,{}\[\]()]+')
CLASSES = ('current', 'stale', 'registration-refused', 'not-passing',
           'unverifiable', 'no-warrant')


def reach_module():
    # warrant_reach shares this module's strict EDN reader.
    sys.modules.setdefault('warrant_index', sys.modules[__name__])
    try:
        from scripts import warrant_reach as module
    except ImportError:
        import warrant_reach as module
    return module


def edn(source):
    """Strict data-only EDN reader for registry payloads."""
    tokens = [m.group() for m in TOKEN.finditer(source)
              if not m.group()[0].isspace() and m.group()[0] not in ',;']
    i = 0
    def read():
        nonlocal i
        if i == len(tokens):
            raise ValueError('incomplete EDN')
        token = tokens[i]; i += 1
        if token in ('{', '[', '(', '#{'):
            close = {'{': '}', '[': ']', '(': ')', '#{': '}'}[token]
            items = []
            while i < len(tokens) and tokens[i] != close:
                items.append(read())
            if i == len(tokens): raise ValueError('unclosed EDN collection')
            i += 1
            if token != '{': return items
            if len(items) % 2: raise ValueError('odd EDN map')
            result = dict(zip(items[::2], items[1::2]))
            if len(result) * 2 != len(items): raise ValueError('duplicate EDN key')
            return result
        if token.startswith('"'): return json.loads(token)
        if token.startswith(':') and not token.startswith('::'): return token[1:]
        if token in ('nil', 'true', 'false'):
            return {'nil': None, 'true': True, 'false': False}[token]
        if token in ('#inst', '#uuid'):
            value = read()
            if not isinstance(value, str): raise ValueError('invalid tagged scalar')
            return value
        if re.fullmatch(r'[-+]?\d+', token): return int(token)
        if re.fullmatch(r'[-+]?\d+\.\d+(?:[eE][-+]?\d+)?', token): return float(token)
        raise ValueError('unsupported EDN token: ' + token)
    result = read()
    if i != len(tokens): raise ValueError('trailing EDN')
    return result


def connect(path):
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    db = sqlite3.connect(path, timeout=30)
    db.execute('PRAGMA busy_timeout=30000')
    db.execute('PRAGMA journal_mode=WAL')
    db.execute('PRAGMA foreign_keys=ON')
    db.executescript('''
      CREATE TABLE IF NOT EXISTS registry_entries (
        id TEXT PRIMARY KEY, payload_text TEXT, payload_sha TEXT,
        envelope_edn TEXT NOT NULL, parent_id TEXT, fork_id TEXT,
        subject_edn TEXT, author TEXT, at TEXT NOT NULL, type_edn TEXT,
        claim_type_edn TEXT, session TEXT, ephemeral INTEGER NOT NULL DEFAULT 0);
      CREATE TABLE IF NOT EXISTS registry_runs (
        entry_id TEXT PRIMARY KEY, repo_root TEXT, namespace TEXT,
        command_key TEXT, ran_at TEXT, finished_at TEXT, warrant INTEGER,
        revision TEXT, ran_order TEXT NOT NULL, finished_order TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS registry_metadata (key TEXT PRIMARY KEY, value TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS warrant_refactor_requests (
        request_id INTEGER PRIMARY KEY AUTOINCREMENT,
        cause_kind TEXT NOT NULL, cause_ns TEXT, cause_name TEXT, cause_file TEXT,
        stale_count INTEGER NOT NULL, namespaces_json TEXT NOT NULL,
        limit_value INTEGER NOT NULL, requested_at TEXT NOT NULL,
        state TEXT NOT NULL CHECK (state IN ('open','dispatched','done','declined')),
        detail TEXT, finished_at TEXT);
      CREATE UNIQUE INDEX IF NOT EXISTS warrant_refactor_one_active_cause
        ON warrant_refactor_requests
          (cause_kind,COALESCE(cause_ns,''),COALESCE(cause_name,''),
           COALESCE(cause_file,''))
        WHERE state IN ('open','dispatched');
    ''')
    return db


def absolute(path, root):
    return os.path.abspath(os.path.join(root, path))


def wire_namespaces(path=WIRE_LEDGER_TEST):
    source = Path(path).read_text()
    match = re.search(r'\(def\s+wire-test-nses\s+\'\[(.*?)\]\)', source, re.S)
    if not match: raise ValueError('wire-test-nses not found in ' + str(path))
    return set(re.findall(r'\bfuton3c(?:\.[A-Za-z0-9_-]+)+\b', match.group(1)))


def local_namespaces(db):
    return {row[0] for row in db.execute(
        'SELECT DISTINCT namespace FROM registry_runs WHERE namespace IS NOT NULL')}


def latest_rows(db, namespaces, passing_only=False):
    wanted = set(namespaces)
    if not wanted: return {}
    placeholders = ','.join('?' for _ in wanted)
    warrant_clause = ' AND r.warrant=1' if passing_only else ''
    sql = f'''SELECT r.namespace,r.entry_id,r.repo_root,r.warrant,e.payload_text,e.payload_sha
              FROM registry_runs r JOIN registry_entries e ON e.id=r.entry_id
              WHERE r.namespace IN ({placeholders}){warrant_clause}
              ORDER BY r.namespace,r.ran_order DESC,r.finished_order DESC,r.entry_id DESC'''
    found = {}
    for namespace, entry, root, warrant, payload, digest in db.execute(sql, tuple(sorted(wanted))):
        found.setdefault(namespace, {'namespace': namespace, 'entry-id': entry,
                                     'repo-root': root, 'warrant': bool(warrant),
                                     'payload-text': payload, 'payload-sha': digest})
    return found


def recorded_files(run, payload, root=None):
    root = root or payload.get('repo/root') or run.get('repo-root')
    if not isinstance(root, str) or not root: raise ValueError('missing repo/root')
    closure, tests = payload.get('load-closure'), payload.get('test-files')
    if not isinstance(closure, list) or not isinstance(tests, dict):
        raise ValueError('missing load-closure or test-files')
    pairs = []
    for row in closure:
        if not isinstance(row, dict): raise ValueError('invalid load-closure row')
        pairs.append((row.get('path'), row.get('sha256')))
    pairs.extend(tests.items())
    files = {}
    for path, digest in pairs:
        if (not isinstance(path, str) or not path or not isinstance(digest, str)
                or not re.fullmatch('[0-9a-f]{64}', digest)):
            raise ValueError('invalid closure path or sha256')
        path = absolute(path, root)
        if path in files and files[path] != digest:
            raise ValueError('conflicting hashes for ' + path)
        files[path] = digest
    return files


def registration_refusal(payload):
    results = payload.get('results')
    green = (isinstance(results, dict) and results.get('exit') == 0
             and results.get('failures') == 0 and results.get('errors') == 0)
    for key in ('postcheck', 'precheck'):
        check = payload.get(key)
        if (green and isinstance(check, dict)
                and check.get('record/type') == 'test-registry/refusal'):
            return check.get('reason')
    return None


def read_reach_record(reach_dir, run):
    path = Path(reach_dir) / (run['entry-id'] + '.json')
    if not path.is_file(): return None, None
    try:
        record = json.loads(path.read_text())
        if (record.get('entry-id') != run['entry-id']
                or record.get('namespace') != run['namespace']):
            raise ValueError('identity-mismatch')
        return record, None
    except (OSError, ValueError, TypeError, json.JSONDecodeError) as error:
        return None, 'ignored: ' + str(error)


def classify(db, namespaces, root=None, reach_dir=REACH_DIR):
    latest = latest_rows(db, namespaces)
    prepared, all_paths = {}, set()
    for namespace in namespaces:
        run = latest.get(namespace)
        if not run:
            prepared[namespace] = ('no-warrant', None, {}); continue
        try:
            # The stored text must be the text the entry id was made from.
            text_sha = hashlib.sha256((run['payload-text'] or '').encode()).hexdigest()
            if text_sha != run['payload-sha'] or run['entry-id'] != 'test-registry-' + text_sha:
                raise ValueError('payload does not match its digest')
            payload = edn(run['payload-text'])
            files = recorded_files(run, payload, root)
        except (TypeError, ValueError):
            prepared[namespace] = ('unverifiable', run, {}); continue
        if not run['warrant'] or payload.get('warrant?') is not True:
            refusal = registration_refusal(payload)
            prepared[namespace] = (('registration-refused' if refusal else 'not-passing'),
                                   run, files, refusal)
            continue
        if not files:
            prepared[namespace] = ('unverifiable', run, {}); continue
        prepared[namespace] = ('passing', run, files); all_paths.update(files)
    observed = {}
    for path in all_paths:
        try:
            with open(path, 'rb') as stream:
                observed[path] = hashlib.file_digest(stream, 'sha256').hexdigest()
        except OSError: observed[path] = None
    rows = []
    for namespace in sorted(namespaces):
        prepared_row = prepared[namespace]
        state, run, files = prepared_row[:3]; changed = []
        reach_note = None
        basis = 'files'
        files_changed_unreached = []
        if state == 'passing':
            for path, digest in files.items():
                if observed[path] != digest:
                    changed.append({'path': path, 'reason': 'unreadable' if observed[path] is None
                                    else 'hash-mismatch'})
            state = 'stale' if changed else 'current'
            if changed:
                record, reach_note = read_reach_record(reach_dir, run)
                if record is not None:
                    dependency_changes = reach_module().check_record(
                        Path(reach_dir) / (run['entry-id'] + '.json'),
                        Path(reach_dir) / '.cache')
                    basis = 'definitions'
                    if dependency_changes:
                        changed = dependency_changes
                    else:
                        state = 'current'
                        files_changed_unreached = changed
                        changed = []
        rows.append({'namespace': namespace, 'class': state,
                     'entry-id': run and run['entry-id'],
                     'basis': basis,
                     'reach-record': reach_note,
                     'files-changed-unreached': files_changed_unreached,
                     'reason': prepared_row[3] if len(prepared_row) > 3 else None,
                     'changed': sorted(changed, key=lambda change:
                                       (change.get('path', ''),
                                        change.get('kind', ''),
                                        str(change.get('definition', ''))))})
    return rows


def reach_record(db, namespaces, reach_dir, root=None, file_rule=FILE_RULE_NAMESPACES):
    destination = Path(reach_dir); destination.mkdir(parents=True, exist_ok=True)
    latest = latest_rows(db, namespaces, passing_only=True)
    answers = []
    for namespace in sorted(namespaces):
        if namespace in file_rule:
            answers.append({'namespace': namespace,
                            'skipped': 'keeps the whole-file rule'})
            continue
        run = latest.get(namespace)
        if not run:
            answers.append({'namespace': namespace,
                            'skipped': 'no passing run recorded'})
            continue
        try:
            text_sha = hashlib.sha256((run['payload-text'] or '').encode()).hexdigest()
            if text_sha != run['payload-sha'] or run['entry-id'] != 'test-registry-' + text_sha:
                raise ValueError('payload does not match its digest')
            payload = edn(run['payload-text'])
            files = recorded_files(run, payload, root)
            if payload.get('warrant?') is not True or not files:
                raise ValueError('not a passing warrant')
        except (TypeError, ValueError) as error:
            answers.append({'namespace': namespace,
                            'skipped': 'entry cannot be read: ' + str(error)})
            continue
        file_current = all(Path(path).is_file()
                           and hashlib.sha256(Path(path).read_bytes()).hexdigest() == expected
                           for path, expected in files.items())
        if not file_current:
            answers.append({'namespace': namespace,
                            'skipped': 'files differ from the run'})
            continue
        path = destination / (run['entry-id'] + '.json')
        if path.exists():
            answers.append({'namespace': namespace, 'entry-id': run['entry-id'],
                            'path': str(path), 'existing': True})
            continue
        loaded = [path for path in files if path.endswith(('.clj', '.cljc'))]
        resources = [path for path in files if path not in loaded]
        reach = reach_module()
        record = reach.qualify_record(reach.analyze_files(
            namespace, loaded, resources, cache_path=destination / '.cache'))
        record['entry-id'] = run['entry-id']
        path.write_text(json.dumps(record, indent=2, sort_keys=True) + '\n')
        answers.append({'namespace': namespace, 'entry-id': run['entry-id'],
                        'path': str(path), 'existing': False})
    return answers


def put_local(db, entry_id):
    row = db.execute('SELECT r.namespace,r.entry_id FROM registry_runs r '
                     'JOIN registry_entries e ON e.id=r.entry_id WHERE r.entry_id=?',
                     (entry_id,)).fetchone()
    if not row: raise ValueError('no local run: ' + entry_id)
    return {'namespace': row[0], 'class': 'local-record', 'entry-id': row[1]}


def affected(db, paths, root=None):
    targets, found = {os.path.abspath(path) for path in paths}, []
    for namespace, run in latest_rows(db, local_namespaces(db)).items():
        try: files = recorded_files(run, edn(run['payload-text']), root)
        except (TypeError, ValueError): continue
        if targets.intersection(files): found.append(namespace)
    return sorted(found)


def change_cause(row, change):
    kind = change.get('kind')
    if kind in ('definition-changed', 'definition-missing', 'definition-added'):
        definition = change.get('definition', [])
        return ('definition', definition[0], definition[1], None)
    if kind in ('remainder-changed', 'remainder-added'):
        return ('remainder', None, None, change.get('file'))
    if kind in ('whole-file-changed', 'file-missing'):
        return ('whole-file', None, None, change.get('file'))
    return ('file', None, None, change.get('path'))


def own_test_file_change(row):
    """The path of the test's own source file when the row reports it changed."""
    suffix = '/' + row['namespace'].replace('.', '/').replace('-', '_') + '.clj'
    for change in row.get('changed', []):
        path = change.get('file') or change.get('path') or ''
        if (change.get('kind') in ('whole-file-changed', 'file-missing')
                or change.get('reason') in ('hash-mismatch', 'unreadable')) \
                and path.endswith(suffix):
            return path
    return None


def impact_rows(rows, limit=10, file_rule=FILE_RULE_NAMESPACES):
    groups = {}
    stale = set()
    for row in rows:
        namespace = row['namespace']
        if row['class'] != 'stale' or namespace in file_rule:
            continue
        stale.add(namespace)
        own = own_test_file_change(row)
        if own:
            # The test itself was edited. What it reaches now differs from what
            # was recorded, so the check also reports every definition gained
            # or lost; those follow from the edit and are not causes.
            groups.setdefault(('own-test-file', None, None, own), set()).add(namespace)
            continue
        for change in row.get('changed', []):
            key = change_cause(row, change)
            groups.setdefault(key, set()).add(namespace)
    causes = []
    for (kind, cause_ns, cause_name, cause_file), namespaces in sorted(
            groups.items(), key=lambda item: tuple(value or '' for value in item[0])):
        names = sorted(namespaces)
        cause = ({'ns': cause_ns, 'name': cause_name}
                 if kind == 'definition' else cause_file)
        causes.append({'cause': cause, 'kind': kind, 'count': len(names),
                       'namespaces': names, 'over-limit': len(names) > limit,
                       'cause-ns': cause_ns, 'cause-name': cause_name,
                       'cause-file': cause_file})
    return {'causes': causes,
            'totals': {'stale-namespaces': len(stale), 'causes': len(causes),
                       'causes-over-limit': sum(item['over-limit'] for item in causes)}}


def record_refactor_requests(db, impact, limit):
    now = datetime.now(timezone.utc).isoformat().replace('+00:00', 'Z')
    recorded = []
    db.execute('BEGIN IMMEDIATE')
    try:
        for cause in impact['causes']:
            if not cause['over-limit']:
                continue
            identity = (cause['kind'], cause['cause-ns'], cause['cause-name'],
                        cause['cause-file'])
            row = db.execute(
                '''SELECT request_id FROM warrant_refactor_requests
                   WHERE cause_kind=? AND cause_ns IS ? AND cause_name IS ?
                     AND cause_file IS ? AND state IN ('open','dispatched')
                   ORDER BY request_id LIMIT 1''', identity).fetchone()
            namespaces = json.dumps(cause['namespaces'], separators=(',', ':'))
            if row:
                db.execute('''UPDATE warrant_refactor_requests
                              SET stale_count=?,namespaces_json=?,limit_value=?
                              WHERE request_id=?''',
                           (cause['count'], namespaces, limit, row[0]))
                request_id, created = row[0], False
            else:
                cursor = db.execute(
                    '''INSERT INTO warrant_refactor_requests
                       (cause_kind,cause_ns,cause_name,cause_file,stale_count,
                        namespaces_json,limit_value,requested_at,state,detail)
                       VALUES(?,?,?,?,?,?,?,?,?,?)''',
                    (*identity, cause['count'], namespaces, limit, now, 'open',
                     json.dumps({'cause': cause['cause']}, sort_keys=True)))
                request_id, created = cursor.lastrowid, True
            recorded.append({'request-id': request_id, 'created': created,
                             'cause': cause['cause'], 'kind': cause['kind']})
        db.commit()
    except Exception:
        db.rollback(); raise
    return recorded


def refactor_requests(db, state='open'):
    rows = db.execute(
        '''SELECT request_id,cause_kind,cause_ns,cause_name,cause_file,
                  stale_count,namespaces_json,limit_value,requested_at,state,
                  detail,finished_at
           FROM warrant_refactor_requests WHERE state=?
           ORDER BY stale_count DESC,request_id''', (state,))
    return [{'request-id': row[0], 'kind': row[1], 'cause-ns': row[2],
             'cause-name': row[3], 'cause-file': row[4], 'stale-count': row[5],
             'namespaces': json.loads(row[6]), 'limit': row[7],
             'requested-at': row[8], 'state': row[9], 'detail': row[10],
             'finished-at': row[11]} for row in rows]


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--db', default=str(DB))
    parser.add_argument('--root', default=str(ROOT))
    sub = parser.add_subparsers(dest='action', required=True)
    sub.add_parser('load')
    put_parser = sub.add_parser('put'); put_parser.add_argument('entry')
    check_parser = sub.add_parser('check')
    check_parser.add_argument('--ns', nargs='+', action='extend')
    check_parser.add_argument('--prefix', default='')
    check_parser.add_argument('--wire', action='store_true')
    check_parser.add_argument('--json', action='store_true')
    check_parser.add_argument('--reach-dir', default=str(REACH_DIR))
    reach_parser = sub.add_parser('reach-record')
    reach_parser.add_argument('--ns', nargs='+', action='extend')
    reach_parser.add_argument('--wire', action='store_true')
    reach_parser.add_argument('--reach-dir', default=str(REACH_DIR))
    reach_parser.add_argument('--file-rule', nargs='+', default=[],
                              help='further namespaces that keep the whole-file rule')
    impact_parser = sub.add_parser('impact')
    impact_parser.add_argument('--ns', nargs='+', action='extend')
    impact_parser.add_argument('--prefix', default='')
    impact_parser.add_argument('--wire', action='store_true')
    impact_parser.add_argument('--limit', type=int, default=10)
    impact_parser.add_argument('--json', action='store_true')
    impact_parser.add_argument('--record', action='store_true')
    impact_parser.add_argument('--reach-dir', default=str(REACH_DIR))
    requests_parser = sub.add_parser('refactor-requests')
    requests_parser.add_argument('--state', default='open',
                                 choices=('open', 'dispatched', 'done', 'declined'))
    requests_parser.add_argument('--json', action='store_true')
    affected_parser = sub.add_parser('affected'); affected_parser.add_argument('paths', nargs='+')
    args = parser.parse_args(argv)
    with connect(args.db) as db:
        if args.action == 'put':
            print(json.dumps(put_local(db, args.entry))); return 0
        if args.action == 'load':
            print(json.dumps({'class': 'local-records', 'runs': len(local_namespaces(db))})); return 0
        if args.action == 'affected':
            print(json.dumps(affected(db, args.paths, args.root))); return 0
        if args.action == 'refactor-requests':
            requests = refactor_requests(db, args.state)
            if args.json: print(json.dumps({'requests': requests}))
            else:
                for request in requests: print(json.dumps(request, sort_keys=True))
            return 0
        selected = set(args.ns or ())
        if args.wire: selected.update(wire_namespaces())
        if args.ns is None and not args.wire: selected.update(local_namespaces(db))
        if args.action == 'reach-record':
            for row in reach_record(db, selected, args.reach_dir, args.root,
                                    FILE_RULE_NAMESPACES | set(args.file_rule)):
                print(json.dumps(row, sort_keys=True))
            return 0
        selected = {namespace for namespace in selected if namespace.startswith(args.prefix)}
        rows = classify(db, selected, args.root, args.reach_dir)
        if args.action == 'impact':
            result = impact_rows(rows, args.limit)
            if args.record:
                result['requests'] = record_refactor_requests(db, result, args.limit)
            if args.json: print(json.dumps(result))
            else:
                for cause in result['causes']: print(json.dumps(cause, sort_keys=True))
                print(json.dumps(result['totals'], sort_keys=True))
            return 3 if result['totals']['causes-over-limit'] else 0
        counts = {state: sum(row['class'] == state for row in rows) for state in CLASSES}
        if args.json: print(json.dumps({'namespaces': rows, 'counts': counts}))
        else:
            for row in rows:
                detail = ''.join(
                    (' ' + change['path'] + ' (' + change['reason'] + ')'
                     if 'path' in change and 'reason' in change
                     else ' ' + json.dumps(change, sort_keys=True))
                    for change in row['changed'])
                print(row['namespace'] + ': ' + row['class'] + detail)
            print(' '.join(f'{state}: {count}' for state, count in counts.items()))
        return int(any(row['class'] != 'current' for row in rows))


if __name__ == '__main__':
    try: sys.exit(main())
    except (ValueError, KeyError, IndexError, TypeError, OSError, sqlite3.Error) as error:
        print(json.dumps({'class': 'refused', 'error': str(error)}), file=sys.stderr)
        sys.exit(1)
