"""P0 saved LIST visibility evidence; optional CLI tests use a real live snapshot."""
import copy
import json
import os
from pathlib import Path
import subprocess
import sys
import tempfile
import unittest
from unittest.mock import patch
import urllib.error
import xiang2000_p0 as p0

FIXTURE = p0.REPO / 'holes/labs/M-象-2000/P0-retrieval-ordering-fixture.json'


class OrderingTest(unittest.TestCase):
    def setUp(self):
        self.f = json.loads(FIXTURE.read_text())

    def answer(self, probes):
        return p0.retrieval_ordering(
            {'retrieval-ordering.json': json.dumps(probes),
             'pins.json': json.dumps(self.f['pins'])},
            self.f['early'], self.f['retrieval'], self.f['commit'])

    def test_503_retry_uses_backoff(self):
        error = urllib.error.HTTPError('http://fixture', 503, 'busy', {}, None)
        with patch.object(p0.urllib.request, 'urlopen', side_effect=[error, ValueError('stop after retry')]), patch.object(p0.time_module, 'sleep') as sleep:
            with self.assertRaisesRegex(ValueError, 'stop after retry'):
                p0.capture()
            sleep.assert_called_once_with(2)

    def test_pinned_real_answer(self):
        self.assertEqual(self.f['answer'], self.answer(self.f['probes']))

    def test_swapped_brackets(self):
        p = copy.deepcopy(self.f['probes'])
        for edge in ('lo', 'hi'):
            p['retrieval'][edge]['response'], p['turn-commits'][edge]['response'] = (
                p['turn-commits'][edge]['response'], p['retrieval'][edge]['response'])
        with self.assertRaisesRegex(ValueError, 'retrieval-ordering detail'):
            self.answer(p)

    def test_ordering_is_derived_not_constant(self):
        p = copy.deepcopy(self.f['probes'])
        for edge in ('lo', 'hi'):
            p['turn-commits'][edge]['params']['system-as-of'] = p['retrieval'][edge]['params']['system-as-of']
        self.assertIn('ordering=not ordered;', self.answer(p))

    def test_absent_response_contains_record(self):
        p = copy.deepcopy(self.f['probes'])
        p['retrieval']['lo']['response']['entries'].append(self.f['retrieval'])
        with self.assertRaisesRegex(ValueError, 'retrieval-ordering detail.*lo visibility'):
            self.answer(p)

    def test_incomplete_absence_cannot_prove_bracket(self):
        p = copy.deepcopy(self.f['probes'])
        p['retrieval']['lo']['response']['incomplete'] = True
        with self.assertRaisesRegex(ValueError, 'retrieval-ordering detail.*incomplete'):
            self.answer(p)

    @unittest.skipUnless(os.environ.get('P0_ORDERING_SNAPSHOT'), 'set P0_ORDERING_SNAPSHOT for full CLI replay')
    def test_rehashed_snapshot_mutations_fail_cli_check(self):
        original = p0.read_snapshot(Path(os.environ['P0_ORDERING_SNAPSHOT']))
        for mutation in ('swap', 'false-absence'):
            files = dict(original)
            p = json.loads(files['retrieval-ordering.json'])
            if mutation == 'swap':
                for edge in ('lo', 'hi'):
                    p['retrieval'][edge]['response'], p['turn-commits'][edge]['response'] = (
                        p['turn-commits'][edge]['response'], p['retrieval'][edge]['response'])
            else:
                p['retrieval']['lo']['response']['entries'].append(self.f['retrieval'])
            files['retrieval-ordering.json'] = p0.js(p).encode()
            with tempfile.TemporaryDirectory() as d:
                snapshot = Path(d) / 'copy'
                p0.write_snapshot(snapshot, files)  # Rehash, so this is not merely integrity rejection.
                r = subprocess.run([sys.executable, str(p0.REPO / 'scripts/xiang2000_p0.py'),
                                    '--from-snapshot', str(snapshot), '--check'],
                                   capture_output=True, text=True, timeout=30)
                self.assertNotEqual(0, r.returncode)
                self.assertIn('retrieval-ordering detail', r.stderr)


if __name__ == '__main__':
    unittest.main()
