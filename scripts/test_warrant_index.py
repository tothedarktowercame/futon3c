#!/usr/bin/env python3
import hashlib
import json
from pathlib import Path
import sqlite3
import subprocess
import sys
import tempfile
import unittest
from scripts import warrant_index as wi

SCRIPT = str(Path(wi.__file__).resolve())

# Captured verbatim from registry_runs after local registration
# test-registry-f3e00f... (WARRANT-LOCAL-I2, 2026-09-27). Synthetic rows below
# use these exact ten columns and the same order-key representation.
REAL_RUN_ROW = (
    'test-registry-f3e00feb00a9a9d01e803eaef83f50ce5c17239c5826ad5a879e086efca17e68',
    '/home/joe/code/futon3c/../wt-warrant-5dcccab1',
    'futon3c.test-registry.sqlite-backend-test',
    '["clojure" "-M:test" "-n" "futon3c.test-registry.sqlite-backend-test"]',
    '2026-09-27T23:19:28.277119594Z', '2026-09-27T23:19:49.066460972Z', 1,
    '5dcccab10548358d2224e8e60c2073182c7da8a4',
    '00000000001790551168:277119594', '00000000001790551189:066460972')


def encode(value):
    if isinstance(value, dict):
        return '{' + ' '.join(json.dumps(key) + ' ' + encode(item)
                              for key, item in value.items()) + '}'
    if isinstance(value, list): return '[' + ' '.join(map(encode, value)) + ']'
    if value is None: return 'nil'
    if value is True: return 'true'
    if value is False: return 'false'
    return json.dumps(value)


class IndexTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(); self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.db_path = self.root / 'index.sqlite'
        self.a = self.root / 'a.clj'; self.a.write_text('original')
        self.b = self.root / 'b.clj'; self.b.write_text('test')
        with wi.connect(self.db_path): pass
        self.base = [sys.executable, SCRIPT, '--db', str(self.db_path),
                     '--root', str(self.root)]

    def insert_run(self, entry='one', namespace='test.one', when=1, passing=True,
                   files=True, payload_text=None):
        payload = {'kind': 'run', 'run/id': entry, 'repo/root': str(self.root), 'namespace': namespace,
                   'command': ['clojure', '-M:test', '-n', namespace],
                   'ran-at': f'2026-09-27T01:00:{when:02d}Z',
                   'finished-at': f'2026-09-27T01:01:{when:02d}Z',
                   'warrant?': passing,
                   'load-closure': ([{'path': self.a.name,
                                      'sha256': hashlib.sha256(self.a.read_bytes()).hexdigest()}]
                                    if files else []),
                   'test-files': ({self.b.name: hashlib.sha256(self.b.read_bytes()).hexdigest()}
                                  if files else {})}
        text = encode(payload) if payload_text is None else payload_text
        # As the registry makes them: the id is the digest of the stored text.
        digest = hashlib.sha256(text.encode()).hexdigest()
        label, entry = entry, 'test-registry-' + digest
        self.ids = getattr(self, 'ids', {}); self.ids[label] = entry
        order = f'000000000000000000{when:02d}:000000000'
        with sqlite3.connect(self.db_path) as db:
            db.execute('INSERT INTO registry_entries VALUES(?,?,?,?,?,?,?,?,?,?,?,?,?)',
                       (entry, text, digest, '{}', None, None, '{}', 'test',
                        payload['finished-at'], ':coordination', ':observation', None, 0))
            db.execute('INSERT INTO registry_runs VALUES(?,?,?,?,?,?,?,?,?,?)',
                       (entry, str(self.root), namespace, encode(payload['command']),
                        payload['ran-at'], payload['finished-at'], int(passing), 'revision',
                        order, order))
        return payload

    def runcli(self, *args, status=0):
        result = subprocess.run(self.base + list(args), capture_output=True, text=True)
        self.assertEqual(status, result.returncode, result.stdout + result.stderr)
        return result

    def check(self, state, paths=(), reason='hash-mismatch', namespace='test.one'):
        result = json.loads(self.runcli('check', '--ns', namespace, '--json',
                                        status=int(state != 'current')).stdout)
        row = result['namespaces'][0]
        self.assertEqual(state, row['class'])
        self.assertEqual([{'path': str(path), 'reason': reason} for path in paths], row['changed'])
        return row

    def test_backend_row_fixture_has_exact_schema(self):
        with sqlite3.connect(self.db_path) as db:
            columns = [row[1] for row in db.execute('PRAGMA table_info(registry_runs)')]
        self.assertEqual(['entry_id', 'repo_root', 'namespace', 'command_key', 'ran_at',
                          'finished_at', 'warrant', 'revision', 'ran_order', 'finished_order'],
                         columns)
        self.assertEqual(10, len(REAL_RUN_ROW))

    def test_pass_edit_restore(self):
        self.insert_run()
        self.check('current')
        self.a.write_text('edited'); self.check('stale', [self.a])
        self.a.write_text('original'); self.check('current')
        self.a.unlink(); self.check('stale', [self.a], 'unreadable')

    def test_newest_failure_and_later_pass(self):
        self.insert_run('pass', when=1, passing=True)
        self.insert_run('fail', when=2, passing=False)
        self.assertEqual(self.ids['fail'], self.check('not-passing')['entry-id'])
        self.insert_run('pass-again', when=3, passing=True)
        self.assertEqual(self.ids['pass-again'], self.check('current')['entry-id'])

    def test_out_of_order_append_cannot_hide_newer_failure(self):
        self.insert_run('newer-fail', when=9, passing=False)
        self.insert_run('late-old-pass', when=2, passing=True)
        self.assertEqual(self.ids['newer-fail'], self.check('not-passing')['entry-id'])

    def test_unverifiable_missing_files_and_bad_payload(self):
        self.insert_run('empty', files=False)
        self.check('unverifiable')
        self.insert_run('bad', namespace='test.bad', payload_text='{not edn', when=2)
        self.check('unverifiable', namespace='test.bad')

    def test_imported_legacy_rows_are_not_local_runs(self):
        with sqlite3.connect(self.db_path) as db:
            db.executescript('''
              CREATE TABLE warrants(namespace TEXT PRIMARY KEY, entry_id TEXT,
                finished_at TEXT, revision TEXT, order_time TEXT);
              CREATE TABLE files(namespace TEXT, path TEXT, sha256 TEXT);
            ''')
            db.execute('INSERT INTO warrants VALUES(?,?,?,?,?)',
                       ('test.imported', 'old-remote', '2026-09-27T00:00:00Z', 'old', 'old'))
            db.execute('INSERT INTO files VALUES(?,?,?)',
                       ('test.imported', str(self.a), hashlib.sha256(self.a.read_bytes()).hexdigest()))
        self.check('no-warrant', namespace='test.imported')

    def test_wire_lists_unregistered_namespace(self):
        names = wi.wire_namespaces()
        self.assertGreater(len(names), 100)
        result = json.loads(self.runcli('check', '--wire', '--json', status=1).stdout)
        missing = next(namespace for namespace in names
                       if namespace not in wi.local_namespaces(sqlite3.connect(self.db_path)))
        row = next(row for row in result['namespaces'] if row['namespace'] == missing)
        self.assertEqual('no-warrant', row['class'])

    def test_default_set_is_local_runs_and_put_load_are_local(self):
        self.insert_run('one', 'test.one'); self.insert_run('two', 'test.two', when=2)
        result = json.loads(self.runcli('check', '--json').stdout)
        self.assertEqual({'test.one', 'test.two'}, {row['namespace'] for row in result['namespaces']})
        self.assertEqual('local-record', json.loads(self.runcli('put', self.ids['one']).stdout)['class'])
        self.assertEqual(2, json.loads(self.runcli('load').stdout)['runs'])
        self.assertEqual(['test.one', 'test.two'],
                         json.loads(self.runcli('affected', str(self.a)).stdout))
        refused = self.runcli('put', 'old-remote', status=1)
        self.assertIn('no local run', json.loads(refused.stderr)['error'])

    def test_payload_altered_after_storage_is_unverifiable(self):
        self.insert_run('one')
        self.check('current')
        with sqlite3.connect(self.db_path) as db:
            text = db.execute('SELECT payload_text FROM registry_entries WHERE id=?',
                              (self.ids['one'],)).fetchone()[0]
            db.execute('UPDATE registry_entries SET payload_text=? WHERE id=?',
                       (text + ' ', self.ids['one']))
        self.check('unverifiable')

    def test_edn_refuses_ambiguous_input(self):
        self.assertEqual({'s': 'quote " inside', 'x': [True, None, 0]},
                         wi.edn('{"s" "quote \\" inside" "x" [true nil 0]}'))
        for bad in ('{"x" 1 "x" 2}', '{} {}', '#=(danger)'):
            with self.assertRaises(ValueError): wi.edn(bad)


if __name__ == '__main__': unittest.main()
