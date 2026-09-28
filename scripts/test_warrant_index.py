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
                   files=True, payload_text=None, payload_updates=None):
        payload = {'kind': 'run', 'run/id': entry, 'repo/root': str(self.root), 'namespace': namespace,
                   'command': ['clojure', '-M:test', '-n', namespace],
                   'ran-at': f'2026-09-27T01:00:{when:02d}Z',
                   'finished-at': f'2026-09-27T01:01:{when:02d}Z',
                   'warrant?': passing,
                   'load-closure': ([{'path': str(self.a.relative_to(self.root)),
                                      'sha256': hashlib.sha256(self.a.read_bytes()).hexdigest()}]
                                    if files else []),
                   'test-files': ({str(self.b.relative_to(self.root)):
                                   hashlib.sha256(self.b.read_bytes()).hexdigest()}
                                  if files else {})}
        payload.update(payload_updates or {})
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

    def check(self, state, paths=None, reason='hash-mismatch', namespace='test.one'):
        result = json.loads(self.runcli('check', '--ns', namespace, '--json',
                                        '--reach-dir', str(self.root / 'reach'),
                                        status=int(state != 'current')).stdout)
        row = result['namespaces'][0]
        self.assertEqual(state, row['class'])
        if paths is not None:
            self.assertEqual([{'path': str(path), 'reason': reason} for path in paths],
                             row['changed'])
        return row

    def dependency_run(self):
        self.a = self.root / 'src' / 'product' / 'core.clj'
        self.a.parent.mkdir(parents=True)
        self.a.write_text('(ns product.core)\n(defn reached [] 1)\n(defn spare [] 2)\n')
        self.b = self.root / 'test' / 'test_one.clj'
        self.b.parent.mkdir()
        self.b.write_text('(ns test.one (:require [product.core :as p]))\n'
                          '(defn exercise [] (p/reached))\n')
        self.insert_run()

    def write_reach_record(self):
        result = self.runcli('reach-record', '--ns', 'test.one',
                             '--reach-dir', str(self.root / 'reach'))
        row = json.loads(result.stdout)
        return Path(row['path'])

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

    def test_registration_refusal_is_distinct_from_test_failure(self):
        refusal = {'record/type': 'test-registry/refusal',
                   'reason': 'scope-not-committed'}
        green = {'results': {'exit': 0, 'failures': 0, 'errors': 0},
                 'postcheck': refusal}
        failing = {'results': {'exit': 1, 'failures': 1, 'errors': 0},
                   'postcheck': refusal}
        self.insert_run('green-refusal', 'test.green-refusal', when=1,
                        passing=False, payload_updates=green)
        row = self.check('registration-refused', namespace='test.green-refusal')
        self.assertEqual('scope-not-committed', row['reason'])
        self.insert_run('failed-refusal', 'test.failed-refusal', when=2,
                        passing=False, payload_updates=failing)
        self.check('not-passing', namespace='test.failed-refusal')
        self.insert_run('ordinary-failure', 'test.ordinary-failure', when=3,
                        passing=False,
                        payload_updates={'results': {'exit': 1, 'failures': 1, 'errors': 0}})
        self.check('not-passing', namespace='test.ordinary-failure')
        self.insert_run('warrant', 'test.warrant', when=4, passing=True,
                        payload_updates={'results': {'exit': 0, 'failures': 0, 'errors': 0}})
        self.check('current', namespace='test.warrant')
        summary = self.runcli('check', '--ns', 'test.green-refusal', status=1).stdout
        self.assertIn('test.green-refusal: registration-refused', summary)
        self.assertIn('registration-refused: 1', summary)

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

    def test_unreached_edit_uses_definition_record(self):
        self.dependency_run(); self.write_reach_record()
        self.a.write_text(self.a.read_text().replace('spare [] 2', 'spare [] 9'))
        row = self.check('current')
        self.assertEqual('definitions', row['basis'])
        self.assertEqual([{'path': str(self.a), 'reason': 'hash-mismatch'}],
                         row['files-changed-unreached'])

    def test_reached_edit_is_stale_by_definition(self):
        self.dependency_run(); self.write_reach_record()
        self.a.write_text(self.a.read_text().replace('reached [] 1', 'reached [] 9'))
        row = self.check('stale')
        self.assertEqual('definitions', row['basis'])
        self.assertIn('definition-changed', {change['kind'] for change in row['changed']})

    def test_file_stale_without_record_keeps_file_rule(self):
        self.dependency_run()
        self.a.write_text(self.a.read_text().replace('spare [] 2', 'spare [] 9'))
        row = self.check('stale', [self.a])
        self.assertEqual('files', row['basis'])

    def test_reach_record_leaves_a_whole_file_test_without_a_record(self):
        self.dependency_run()
        result = self.runcli('reach-record', '--ns', 'test.one', '--file-rule', 'test.one',
                             '--reach-dir', str(self.root / 'reach'))
        self.assertEqual('keeps the whole-file rule', json.loads(result.stdout)['skipped'])
        self.assertEqual([], list((self.root / 'reach').glob('*.json')))
    def test_reach_record_skips_stale_entry(self):
        self.dependency_run(); self.a.write_text(self.a.read_text() + '\n')
        result = self.runcli('reach-record', '--ns', 'test.one',
                             '--reach-dir', str(self.root / 'reach'))
        self.assertEqual('files differ from the run', json.loads(result.stdout)['skipped'])
        self.assertEqual([], list((self.root / 'reach').glob('*.json')))

    def test_reach_record_uses_latest_passing_entry(self):
        self.dependency_run(); passing_id = self.ids['one']
        self.insert_run('later-failure', when=2, passing=False)
        path = self.write_reach_record()
        self.assertEqual(passing_id + '.json', path.name)

    def test_wrong_identity_record_is_ignored(self):
        self.dependency_run(); path = self.write_reach_record()
        record = json.loads(path.read_text()); record['entry-id'] = 'wrong'
        path.write_text(json.dumps(record))
        self.a.write_text(self.a.read_text().replace('spare [] 2', 'spare [] 9'))
        row = self.check('stale', [self.a])
        self.assertEqual('files', row['basis'])
        self.assertIn('identity-mismatch', row['reach-record'])

    def test_invalid_json_record_is_ignored(self):
        self.dependency_run(); path = self.write_reach_record(); path.write_text('{bad')
        self.a.write_text(self.a.read_text().replace('spare [] 2', 'spare [] 9'))
        row = self.check('stale', [self.a])
        self.assertEqual('files', row['basis'])
        self.assertTrue(row['reach-record'].startswith('ignored:'))

    def test_impact_limit_and_cause_grouping(self):
        definition = {'kind': 'definition-changed',
                      'definition': ['product.core', 'shared', '/repo/core.clj']}
        rows = [{'namespace': f'test.{index:02d}', 'class': 'stale',
                 'basis': 'definitions', 'changed': [definition]}
                for index in range(11)]
        result = wi.impact_rows(rows, 10)
        self.assertEqual(
            {'cause': {'ns': 'product.core', 'name': 'shared'},
             'kind': 'definition', 'count': 11,
             'namespaces': [f'test.{index:02d}' for index in range(11)],
             'over-limit': True, 'cause-ns': 'product.core',
             'cause-name': 'shared', 'cause-file': None},
            result['causes'][0])
        self.assertEqual({'stale-namespaces': 11, 'causes': 1,
                          'causes-over-limit': 1}, result['totals'])
        self.assertFalse(wi.impact_rows(rows[:10], 10)['causes'][0]['over-limit'])

    def test_impact_attributes_an_edited_test_to_its_own_file(self):
        own = '/repo/test/wire/reader_test.clj'
        rows = [{'namespace': 'wire.reader-test', 'class': 'stale', 'basis': 'definitions',
                 'changed': [{'kind': 'definition-missing',
                              'definition': ['product.core', 'shared', '/repo/core.clj']},
                             {'kind': 'whole-file-changed', 'file': own}]},
                {'namespace': 'wire.other-test', 'class': 'stale', 'basis': 'definitions',
                 'changed': [{'kind': 'whole-file-changed',
                              'file': '/repo/resources/data.edn'}]}]
        result = wi.impact_rows(rows, 10)
        self.assertEqual([('own-test-file', own, ['wire.reader-test']),
                          ('whole-file', '/repo/resources/data.edn', ['wire.other-test'])],
                         [(c['kind'], c['cause'], c['namespaces']) for c in result['causes']])

    def test_impact_groups_two_definitions_and_file_basis(self):
        rows = []
        for index in range(11):
            changes = [{'kind': 'definition-changed',
                        'definition': ['p', 'large', '/p.clj']}]
            if index < 3:
                changes.append({'kind': 'definition-changed',
                                'definition': ['p', 'small', '/p.clj']})
            rows.append({'namespace': f'n{index:02d}', 'class': 'stale',
                         'basis': 'definitions', 'changed': changes})
        rows.append({'namespace': 'file.stale', 'class': 'stale', 'basis': 'files',
                     'changed': [{'path': '/x.clj', 'reason': 'hash-mismatch'}]})
        causes = wi.impact_rows(rows, 10)['causes']
        by_kind = {(item['kind'], str(item['cause'])): item for item in causes}
        self.assertEqual(11, by_kind[('definition', "{'ns': 'p', 'name': 'large'}")]['count'])
        self.assertEqual(3, by_kind[('definition', "{'ns': 'p', 'name': 'small'}")]['count'])
        self.assertEqual({'cause': '/x.clj', 'kind': 'file', 'count': 1,
                          'namespaces': ['file.stale'], 'over-limit': False,
                          'cause-ns': None, 'cause-name': None, 'cause-file': '/x.clj'},
                         by_kind[('file', '/x.clj')])

    def test_impact_excludes_whole_file_rule_namespace(self):
        rows = [{'namespace': next(iter(wi.FILE_RULE_NAMESPACES)), 'class': 'stale',
                 'basis': 'files',
                 'changed': [{'path': '/everything.clj', 'reason': 'hash-mismatch'}]}]
        self.assertEqual({'causes': [],
                          'totals': {'stale-namespaces': 0, 'causes': 0,
                                     'causes-over-limit': 0}},
                         wi.impact_rows(rows, 10))

    def test_refactor_request_is_idempotent_and_updates_count(self):
        cause = {'cause': {'ns': 'p', 'name': 'shared'}, 'kind': 'definition',
                 'count': 11, 'namespaces': [f'n{i:02d}' for i in range(11)],
                 'over-limit': True, 'cause-ns': 'p', 'cause-name': 'shared',
                 'cause-file': None}
        with wi.connect(self.db_path) as db:
            first = wi.record_refactor_requests(
                db, {'causes': [cause], 'totals': {}}, 10)
            cause = dict(cause, count=12, namespaces=[f'n{i:02d}' for i in range(12)])
            second = wi.record_refactor_requests(
                db, {'causes': [cause], 'totals': {}}, 10)
            requests = wi.refactor_requests(db)
        self.assertTrue(first[0]['created'])
        self.assertFalse(second[0]['created'])
        self.assertEqual(first[0]['request-id'], second[0]['request-id'])
        self.assertEqual(1, len(requests))
        self.assertEqual(12, requests[0]['stale-count'])
        self.assertEqual([f'n{i:02d}' for i in range(12)], requests[0]['namespaces'])


if __name__ == '__main__': unittest.main()
