#!/usr/bin/env python3
"""Local current-file validity index; futon1b remains the permanent authority.

load chooses the greatest finished-at (entry-id breaks ties), not ledger line
position: the ledger contains out-of-order registrations. check only compares
pinned load-closure/test-files bytes; it does not certify environment or log
integrity. No store writes, registration hooks, or tests are performed.
"""
import argparse
from concurrent.futures import ThreadPoolExecutor
import datetime
import hashlib
import json
import os
from pathlib import Path
import re
import sqlite3
import sys
import urllib.parse
import urllib.request

ROOT = Path(__file__).resolve().parents[1]
DB = ROOT.parent / 'storage/test-registry/warrant-index.sqlite'
TOKEN = re.compile(r'\s+|,[\s,]*|;[^\n]*|"(?:\\.|[^"\\])*"|#\{|[{}\[\]()]|[^\s,{}\[\]()]+')


def edn(source):
    """Recursive data-only EDN reader. No evaluation or regex field extraction.
    Supports warrant/ledger maps, vectors, sets, lists and scalar data; refuses
    unknown reader forms, duplicate keys and trailing data instead of guessing.
    Keywords normalize to their names, matching the JSON transport.
    """
    tokens = [m.group() for m in TOKEN.finditer(source)
              if not m.group()[0].isspace() and m.group()[0] not in ',;']
    i = 0
    def read():
        nonlocal i
        if i == len(tokens):
            raise ValueError('incomplete EDN')
        t = tokens[i]
        i += 1
        if t in ('{', '[', '(', '#{'):
            close = {'{': '}', '[': ']', '(': ')', '#{': '}'}[t]
            items = []
            while i < len(tokens) and tokens[i] != close:
                items.append(read())
            if i == len(tokens):
                raise ValueError('unclosed EDN collection')
            i += 1
            if t != '{':
                return items
            if len(items) % 2:
                raise ValueError('odd EDN map')
            result = dict(zip(items[::2], items[1::2]))
            if len(result) * 2 != len(items):
                raise ValueError('duplicate EDN key')
            return result
        if t.startswith('"'):
            return json.loads(t)
        if t.startswith(':') and not t.startswith('::'):
            return t[1:]
        if t in ('nil', 'true', 'false'):
            return {'nil': None, 'true': True, 'false': False}[t]
        if t in ('#inst', '#uuid'):
            value = read()
            if not isinstance(value, str):
                raise ValueError('invalid tagged scalar')
            return value
        if re.fullmatch(r'[-+]?\d+', t):
            return int(t)
        if re.fullmatch(r'[-+]?\d+\.\d+(?:[eE][-+]?\d+)?', t):
            return float(t)
        raise ValueError('unsupported EDN token: ' + t)
    result = read()
    if i != len(tokens):
        raise ValueError('trailing EDN')
    return result


def stamp(value):
    # Preserve nanoseconds when ordering Java Instant strings.
    m = re.fullmatch(r'(\d{4}-\d\d-\d\dT\d\d:\d\d:\d\d)(?:\.(\d{1,9}))?Z', value or '')
    if not m:
        raise ValueError('finished-at must be an absolute UTC instant')
    datetime.datetime.fromisoformat(m[1])
    return m[1] + '.' + (m[2] or '').ljust(9, '0') + 'Z'


def ledger(path):
    namespaces, latest = set(), {}
    with open(path) as f:
        for number, line in enumerate(f, 1):
            if not line.strip() or line.lstrip().startswith(';'):
                continue
            try:
                r = edn(line)
                ns = r.get('namespace')
                if not ns:
                    continue
                namespaces.add(ns)
                if r.get('warrant?') is True:
                    key = (stamp(r['finished-at']), r['entry-id'])
                    if ns not in latest or key > latest[ns][0]:
                        latest[ns] = (key, r['entry-id'])
            except (ValueError, KeyError, TypeError) as e:
                raise ValueError(f'{path}:{number}: {e}') from e
    return namespaces, {ns: value[1] for ns, value in latest.items()}


def absolute(path, root):
    # Do not resolve symlinks: a retargeted symlink must be re-read at check time.
    return os.path.abspath(os.path.join(root, path))


def connect(path):
    Path(path).parent.mkdir(parents=True, exist_ok=True)
    db = sqlite3.connect(path, timeout=30)
    db.execute('PRAGMA busy_timeout=30000')
    db.execute('PRAGMA journal_mode=WAL')
    db.execute('PRAGMA foreign_keys=ON')
    db.executescript('''
      CREATE TABLE IF NOT EXISTS warrants (
        namespace TEXT PRIMARY KEY, entry_id TEXT NOT NULL,
        finished_at TEXT NOT NULL, revision TEXT, order_time TEXT NOT NULL);
      CREATE TABLE IF NOT EXISTS files (
        namespace TEXT NOT NULL REFERENCES warrants(namespace) ON DELETE CASCADE,
        path TEXT NOT NULL, sha256 TEXT NOT NULL,
        PRIMARY KEY(namespace, path));
      CREATE INDEX IF NOT EXISTS files_path ON files(path);
    ''')
    return db


def fetch(entry_id, base, root):
    url = base.rstrip('/') + '/api/alpha/evidence/' + urllib.parse.quote(entry_id, safe='')
    req = urllib.request.Request(url, headers={'Accept': 'application/json'})
    with urllib.request.urlopen(req, timeout=60) as response:
        row = json.load(response)
    if row.get('evidence/id') != entry_id:
        raise ValueError('entry id mismatch')
    body = row['evidence/body']
    if isinstance(body, str):
        body = edn(body)
    r = edn(body['payload-edn']) if 'payload-edn' in body else body
    results = r.get('results', {})
    if (r.get('warrant?') is not True or type(results.get('failures')) is not int
            or type(results.get('errors')) is not int or results['failures'] != 0
            or results['errors'] != 0 or results.get('exit', 0) != 0):
        raise ValueError('refused: not a passing warrant')
    command = r.get('command', [])
    ns = r.get('namespace')
    if not ns and '-n' in command:
        ns = command[command.index('-n') + 1]
    if not isinstance(ns, str) or not ns:
        raise ValueError('missing test namespace')
    finished = r['finished-at']
    order = stamp(finished)
    closure, tests = r.get('load-closure'), r.get('test-files')
    if not isinstance(closure, list) or not closure or not isinstance(tests, dict):
        raise ValueError('missing load-closure or test-files')
    files = {}
    for p, digest in [(v['path'], v['sha256']) for v in closure] + list(tests.items()):
        if not isinstance(p, str) or not p or not isinstance(digest, str) or not re.fullmatch('[0-9a-f]{64}', digest):
            raise ValueError('invalid closure path or sha256')
        path = absolute(p, root)
        if path in files and files[path] != digest:
            raise ValueError('conflicting hashes for ' + path)
        files[path] = digest
    return ns, entry_id, finished, r.get('git-head', r.get('revision')), order, files


def put(db, warrant):
    ns, entry, finished, revision, order, files = warrant
    try:
        db.execute('BEGIN IMMEDIATE')
        old = db.execute('SELECT order_time, entry_id FROM warrants WHERE namespace=?', (ns,)).fetchone()
        if old and old > (order, entry):
            db.rollback()
            return {'namespace': ns, 'class': 'older-ignored', 'entry-id': entry}
        db.execute('DELETE FROM warrants WHERE namespace=?', (ns,))
        db.execute('INSERT INTO warrants VALUES (?,?,?,?,?)', (ns, entry, finished, revision, order))
        db.executemany('INSERT INTO files VALUES (?,?,?)', ((ns, p, s) for p, s in files.items()))
        db.commit()
    except Exception:
        db.rollback()
        raise
    return {'namespace': ns, 'class': 'indexed', 'entry-id': entry}


def check(db, namespaces):
    # One read transaction prevents pairing a warrant header with another version's files.
    db.execute('BEGIN')
    headers = {r[0]: r[1] for r in db.execute('SELECT namespace,entry_id FROM warrants')}
    rows = [r for r in db.execute('SELECT namespace,path,sha256 FROM files') if r[0] in namespaces]
    db.rollback()
    hashes = {}
    for _, path, _ in rows:
        if path not in hashes:
            try:
                with open(path, 'rb') as f:
                    hashes[path] = hashlib.file_digest(f, 'sha256').hexdigest()
            except OSError:
                hashes[path] = None
    changes = {ns: [] for ns in namespaces}
    covered = {ns for ns, _, _ in rows}
    for ns, path, digest in rows:
        if hashes[path] != digest:
            changes[ns].append({'path': path, 'reason': 'unreadable' if hashes[path] is None else 'hash-mismatch'})
    # A warrant row with no file rows certifies nothing: it is never current.
    return [{'namespace': ns, 'class': 'no-warrant' if ns not in headers else 'unverifiable' if ns not in covered
             else 'stale' if changes[ns] else 'current',
             'entry-id': headers.get(ns), 'changed': sorted(changes[ns], key=lambda c: c['path'])}
            for ns in sorted(namespaces)]


def main(argv=None):
    p = argparse.ArgumentParser(description=__doc__)
    p.add_argument('--db', default=str(DB))
    p.add_argument('--root', default=str(ROOT), help='root for relative closure paths')
    p.add_argument('--ledger', default=str(ROOT / 'data/test-registry/namespace-ledger.edn'))
    p.add_argument('--store', default='http://localhost:7073')
    sub = p.add_subparsers(dest='action', required=True)
    load = sub.add_parser('load'); load.add_argument('--only')
    put_parser = sub.add_parser('put'); put_parser.add_argument('entry')
    cp = sub.add_parser('check'); cp.add_argument('--ns', nargs='+', action='extend'); cp.add_argument('--prefix', default=''); cp.add_argument('--json', action='store_true')
    ap = sub.add_parser('affected'); ap.add_argument('paths', nargs='+')
    args = p.parse_args(argv)
    with connect(args.db) as db:
        if args.action == 'put':
            print(json.dumps(put(db, fetch(args.entry, args.store, args.root))))
            return 0
        if args.action == 'affected':
            found = set()
            for path in args.paths:
                found.update(r[0] for r in db.execute('SELECT namespace FROM files WHERE path=?', (absolute(path, args.root),)))
            print(json.dumps(sorted(found)))
            return 0
        known, latest = ledger(args.ledger)
        if args.action == 'load':
            wanted = [args.only] if args.only else sorted(known)
            def get(ns):
                try:
                    if ns not in latest:
                        return ns, None, 'no passing ledger entry'
                    w = fetch(latest[ns], args.store, args.root)
                    if w[0] != ns:
                        raise ValueError('ledger/warrant namespace mismatch')
                    return ns, w, None
                except Exception as e:
                    return ns, None, str(e)
            failed = 0
            with ThreadPoolExecutor(max_workers=8) as pool:
                for ns, w, error in pool.map(get, wanted):
                    if error:
                        failed += 1
                        print(json.dumps({'namespace': ns, 'class': 'failed', 'error': error}), flush=True)
                    else:
                        print(json.dumps(put(db, w)), flush=True)
            print(json.dumps({'loaded': len(wanted) - failed, 'failed': failed}))
            return int(failed > 0)
        known.update(r[0] for r in db.execute('SELECT namespace FROM warrants'))
        selected = {ns for ns in (args.ns if args.ns is not None else known) if ns.startswith(args.prefix)}
        rows = check(db, selected)
        counts = {s: sum(r['class'] == s for r in rows) for s in ('current', 'stale', 'unverifiable', 'no-warrant')}
        if args.json:
            print(json.dumps({'namespaces': rows, 'counts': counts}))
        else:
            for r in rows:
                print(r['namespace'] + ': ' + r['class'] + ''.join(' ' + c['path'] + ' (' + c['reason'] + ')' for c in r['changed']))
            print(' '.join(f'{s}: {n}' for s, n in counts.items()))
        return int(any(r['class'] != 'current' for r in rows))


if __name__ == '__main__':
    try:
        sys.exit(main())
    except (ValueError, KeyError, IndexError, TypeError, OSError, sqlite3.Error) as error:
        print(json.dumps({'class': 'refused', 'error': str(error)}), file=sys.stderr)
        sys.exit(1)
