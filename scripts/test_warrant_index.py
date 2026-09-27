import hashlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
from pathlib import Path
import subprocess
import sys
import tempfile
import threading
import unittest
from scripts import warrant_index as wi

SCRIPT = str(Path(wi.__file__).resolve())

def encode(x):
    if isinstance(x, dict):
        return '{' + ' '.join(json.dumps(k) + ' ' + encode(v) for k, v in x.items()) + '}'
    if isinstance(x, list):
        return '[' + ' '.join(map(encode, x)) + ']'
    return 'nil' if x is None else json.dumps(x)


class IndexTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.entries = {}
        class Handler(BaseHTTPRequestHandler):
            def do_GET(self):
                entry = cls.entries.get(self.path.rsplit('/', 1)[-1])
                if entry is None:
                    self.send_error(404)
                else:
                    self.send_response(200); self.end_headers()
                    self.wfile.write(json.dumps(entry).encode())
            def log_message(self, *_):
                pass
        cls.server = ThreadingHTTPServer(('127.0.0.1', 0), Handler)
        cls.thread = threading.Thread(target=lambda: cls.server.serve_forever(poll_interval=0.01), daemon=True)
        cls.thread.start()

    @classmethod
    def tearDownClass(cls):
        cls.server.shutdown(); cls.server.server_close(); cls.thread.join()

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.ledger = self.root / 'ledger.edn'
        self.ledger.write_text('')
        self.a = self.root / 'a.clj'; self.a.write_text('original')
        self.b = self.root / 'b.clj'; self.b.write_text('test')
        self.base = [sys.executable, SCRIPT, '--db', str(self.root / 'index.sqlite'),
                     '--root', str(self.root), '--ledger', str(self.ledger),
                     '--store', f'http://127.0.0.1:{self.server.server_port}']

    def entry(self, id='one', ns='test.one', paths=None, finished='2026-09-27T01:00:00Z', passing=True):
        paths = paths if paths is not None else [self.a]
        record = {'command': ['clojure', '-M:test', '-n', ns], 'finished-at': finished,
                  'git-head': 'abc', 'warrant?': passing, 'results': {'failures': 0, 'errors': 0},
                  'load-closure': [{'path': p.name, 'sha256': hashlib.sha256(p.read_bytes()).hexdigest()} for p in paths],
                  'test-files': {self.b.name: hashlib.sha256(self.b.read_bytes()).hexdigest()}}
        self.entries[id] = {'evidence/id': id, 'evidence/body': {'payload-edn': encode(record)}}
        return record

    def runcli(self, *args, status=0):
        r = subprocess.run(self.base + list(args), capture_output=True, text=True)
        self.assertEqual(status, r.returncode, r.stdout + r.stderr)
        return r

    def check(self, state, paths=(), reason='hash-mismatch', ns='test.one'):
        r = json.loads(self.runcli('check', '--ns', ns, '--json', status=int(state != 'current')).stdout)
        row = r['namespaces'][0]
        self.assertEqual(state, row['class'])
        self.assertEqual([{'path': str(p), 'reason': reason} for p in paths], row['changed'])
        return row

    def test_current_edit_restore_delete(self):
        self.entry(); self.runcli('put', 'one')
        self.check('current')
        self.a.write_text('edited'); self.check('stale', [self.a])
        self.a.write_text('original'); self.check('current')
        self.a.unlink(); self.check('stale', [self.a], 'unreadable')

    def test_refused_preserves_previous(self):
        self.entry(); self.runcli('put', 'one')
        for kind in ('flag', 'failures', 'errors'):
            record = self.entry('bad', passing=kind != 'flag')
            if kind != 'flag':
                record['results'][kind] = 1
                self.entries['bad']['evidence/body']['payload-edn'] = encode(record)
            r = self.runcli('put', 'bad', status=1)
            self.assertEqual('refused', json.loads(r.stderr)['class'])
            self.assertEqual('one', self.check('current')['entry-id'])

    def test_newer_replaces_old_paths_and_older_cannot_win(self):
        self.entry(); self.runcli('put', 'one')
        self.entry('two', paths=[self.b], finished='2026-09-27T02:00:00Z')
        self.runcli('put', 'two'); self.a.unlink()
        self.assertEqual('two', self.check('current')['entry-id'])
        self.runcli('put', 'one')
        self.assertEqual('two', self.check('current')['entry-id'])

    def test_affected_shared_and_missing(self):
        for id, ns in [('one', 'test.one'), ('two', 'test.two')]:
            self.entry(id, ns); self.runcli('put', id)
        self.assertEqual(['test.one', 'test.two'], json.loads(self.runcli('affected', str(self.a)).stdout))
        self.assertEqual([], json.loads(self.runcli('affected', 'absent').stdout))
        self.check('current'); self.check('current', ns='test.two')

    def test_concurrent_processes(self):
        self.entry('one', 'test.one'); self.entry('two', 'test.two')
        ps = [subprocess.Popen(self.base + ['put', id], stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
              for id in ('one', 'two')]
        for p in ps:
            out, err = p.communicate(timeout=15)
            self.assertEqual(0, p.returncode, out + err)
        self.check('current'); self.check('current', ns='test.two')

    def test_load_newest_and_failed_fetch_preserves_row(self):
        self.entry(); self.runcli('put', 'one')
        self.ledger.write_text('\n'.join(encode(r) for r in [
            {'namespace': 'test.one', 'entry-id': 'missing', 'warrant?': True, 'finished-at': '2026-09-27T02:00:00Z'},
            {'namespace': 'test.one', 'entry-id': 'one', 'warrant?': True, 'finished-at': '2026-09-27T01:00:00Z'},
            {'namespace': 'test.none', 'warrant?': False}]))
        r = self.runcli('load', '--only', 'test.one', status=1)
        self.assertEqual('test.one', json.loads(r.stdout.splitlines()[0])['namespace'])
        self.assertEqual('failed', json.loads(r.stdout.splitlines()[0])['class'])
        self.assertEqual('one', self.check('current')['entry-id'])
        self.check('no-warrant', ns='test.none')

    def test_load_success_selects_latest_and_test_files_are_checked(self):
        self.entry('old')
        self.entry('new', finished='2026-09-27T03:00:00Z')
        self.ledger.write_text('\n'.join(encode(r) for r in [
            {'namespace': 'test.one', 'entry-id': 'new', 'warrant?': True, 'finished-at': '2026-09-27T03:00:00Z'},
            {'namespace': 'test.one', 'entry-id': 'old', 'warrant?': True, 'finished-at': '2026-09-27T01:00:00Z'}]))
        self.runcli('load')
        self.assertEqual('new', self.check('current')['entry-id'])
        self.b.write_text('changed test file')
        self.check('stale', [self.b])

    def test_edn_and_nanosecond_order(self):
        source = '{:s ' + json.dumps('quote " inside') + ' :x [true nil 0]}'
        self.assertEqual({'s': 'quote " inside', 'x': [True, None, 0]}, wi.edn(source))
        for bad in ('{:x 1 :x 2}', '{} {}', '#=(danger)'):
            with self.assertRaises(ValueError): wi.edn(bad)
        self.assertLess(wi.stamp('2026-09-27T01:00:00Z'), wi.stamp('2026-09-27T01:00:00.000000001Z'))


if __name__ == '__main__':
    unittest.main()
