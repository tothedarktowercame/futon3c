"""Offline tests: python3 -m unittest scripts.test_together_provenance_spans."""
from collections import Counter
from contextlib import redirect_stdout, redirect_stderr
import io
import json
from pathlib import Path
import tempfile
import unittest
from unittest.mock import patch

from scripts import together_provenance_spans as spans

SITE = 'fable-1.restore'
STAMP = '2026-11-20T12:00:00Z'
REAL = Path.home() / 'storage/arxana-together-tools/provenance'


def run(text, **kw):
    return dict(text=text, provenance={'op': 'kept', 'from': None},
                act_id='act/1', segment_index=0, **kw)


def extraction(*runs):
    return [{'track': 'full', 'slots': [{'slot': 'answer', 'runs': list(runs)}]}]


def state(text, counter=1):
    return {'key': 'incubator-form', 'rev': 7, 'runs': [
        {'id': [SITE, counter], 'text': text,
         'attrs': {'source': 'incubator-buffer-copy'}, 'deleted': False}]}


class SpanTests(unittest.TestCase):
    def mapped(self, data, snapshot):
        rows, _ = spans.build(data, snapshot)
        spans.verify(rows, snapshot)
        return rows

    def test_wrapping_and_codepoints(self):
        rows = self.mapped(extraction(run('alpha\n   beta')), state('😀: alpha beta!'))
        self.assertEqual(rows[0]['start'], [SITE, 4])
        self.assertEqual(rows[0]['end'], [SITE, 13])

    def test_other_sites_and_deleted_originals(self):
        snapshot = state('ab')
        snapshot['runs'] += [
            {'id': ['joe.emacs', 10], 'text': 'LATER', 'attrs': {}, 'deleted': False},
            {'id': [SITE, 3], 'text': 'c', 'attrs': {'source': 'incubator-buffer-copy'}, 'deleted': True},
            {'id': [SITE, 4], 'text': 'd', 'attrs': {'source': 'incubator-buffer-copy'}, 'deleted': False},
            {'id': ['fable-1.seed', 1], 'text': 'abcd', 'attrs': {'source': 'incubator-buffer-copy'}, 'deleted': True}]
        rows = self.mapped(extraction(run('abcd')), snapshot)
        self.assertEqual((rows[0]['start'], rows[0]['end']), ([SITE, 1], [SITE, 4]))
        _, _, counts = spans.original_stream(snapshot)
        self.assertEqual(counts, {SITE: 4, 'joe.emacs': 5, 'fable-1.seed': 4})

    def test_duplicate_answer_is_not_guessed(self):
        rows = self.mapped(extraction(run('same')), state('same / same'))
        self.assertEqual(rows[0]['status'], 'unresolved')
        self.assertIn('2 times', rows[0]['reason'])
        self.assertEqual(spans.acts(rows, STAMP), [])

    def test_edited_answer_falls_back_between_neighbors(self):
        rows = self.mapped(extraction(run('left '), run('missing '), run('right')),
                           state('left changed right'))
        self.assertEqual([r['status'] for r in rows], ['resolved', 'unresolved', 'resolved'])
        self.assertEqual(rows[2]['start'], [SITE, 14])

    def test_repeated_run_fallback_is_ambiguous(self):
        rows = self.mapped(extraction(run('left '), run('echo '), run('absent '), run('right')),
                           state('left echo echo right'))
        self.assertEqual(rows[1]['status'], 'unresolved')
        self.assertNotIn('start', rows[1])
        self.assertIn('not unique', rows[1]['reason'])

    def test_repeated_run_whole_answer_uses_offsets(self):
        rows = self.mapped(extraction(run('left '), run('echo '), run('echo '), run('right')),
                           state('left echo echo right'))
        self.assertEqual([r['status'] for r in rows], ['resolved'] * 4)
        self.assertEqual(rows[1]['start'], [SITE, 6])
        self.assertEqual(rows[1]['end'], [SITE, 9])
        self.assertEqual(rows[2]['start'], [SITE, 11])
        self.assertEqual(rows[2]['end'], [SITE, 14])

    def test_skip_whitespace_and_ignore_nonpublishable(self):
        data = extraction(run('yes'), run(' \n '), run('placeholder', placeholder=True),
                          {'text': 'plain', 'provenance': None})
        rows = self.mapped(data, state('yes placeholderplain'))
        self.assertEqual([r['status'] for r in rows], ['resolved', 'skipped-whitespace'])

    def test_labels(self):
        cases = [
            ('kept', None, "Joe's words, kept from the earlier answer"),
            ('typed', None, 'written by Joe in his editorial pass'),
            ('accepted', 'murch', 'written by Murch; kept by Joe'),
            ('accepted', 'peeragogy', 'from the Peeragogy call; kept by Joe'),
            ('accepted', 'rob', 'written by rob; kept by Joe'),
            ('accepted', None, 'accepted by Joe (origin not recorded)'),
            ('moved', 'other-slot', 'moved')]
        for op, origin, expected in cases:
            with self.subTest(op=op, origin=origin):
                self.assertEqual(spans.label(op, origin), expected)

    def test_attrs_and_run_numbering(self):
        rows = self.mapped(extraction({'text': 'prefix ', 'provenance': None}, run('word')),
                           state('prefix word'))
        act = spans.acts(rows, STAMP)[0]
        self.assertEqual(act['id'], 'prov.0017.answer.002')
        attrs = act['attrs']
        self.assertNotIn('from', attrs)
        self.assertEqual(attrs['as_of'], 'version 17 (0017-f0718e48)')
        self.assertEqual(attrs['sites'], [SITE])
        self.assertEqual(attrs['derived_at'], STAMP)
        self.assertEqual((attrs['act'], attrs['segment']), ('act/1', 0))

    def test_timestamp_requires_utc(self):
        self.assertEqual(spans.utc_timestamp(STAMP), STAMP)
        for value in ['2026-11-20', '2026-11-20T12:00:00', '2026-11-20T12:00:00+01:00']:
            with self.subTest(value=value), self.assertRaises(ValueError):
                spans.utc_timestamp(value)

    def cli_fixture(self, directory):
        p = Path(directory)
        (p / 'extraction.json').write_text(json.dumps(extraction(run('word'))))
        (p / 'state.json').write_text(json.dumps(state('word')))
        return ['--extraction', str(p / 'extraction.json'), '--state', str(p / 'state.json'),
                '--out', str(p / 'out'), '--derived-at', STAMP]

    def test_cli_and_reproducible_outputs(self):
        with tempfile.TemporaryDirectory() as directory, redirect_stdout(io.StringIO()):
            argv = self.cli_fixture(directory)
            self.assertEqual(spans.main(argv), 0)
            p = Path(directory) / 'out'
            before = {f.name: f.read_bytes() for f in p.iterdir()}
            self.assertEqual(spans.main(argv), 0)
            self.assertEqual(before, {f.name: f.read_bytes() for f in p.iterdir()})
            self.assertEqual(len(json.loads((p / 'acts.json').read_text())), 1)

    def test_wrong_span_exits_nonzero_before_writing(self):
        real_build = spans.build

        def bad_build(*args):
            rows, counts = real_build(*args)
            rows[0]['start'] = [SITE, 2]
            return rows, counts

        with tempfile.TemporaryDirectory() as directory, patch.object(spans, 'build', bad_build):
            err = io.StringIO()
            with redirect_stderr(err):
                self.assertEqual(spans.main(self.cli_fixture(directory)), 1)
            self.assertIn('verification failed', err.getvalue())
            self.assertFalse((Path(directory) / 'out').exists())

    @unittest.skipUnless((REAL / 'extraction.json').exists() and
                         (REAL / 'incubator-form-state-rev.json').exists(), 'optional real inputs absent')
    def test_real_files(self):
        data = json.loads((REAL / 'extraction.json').read_text())
        snapshot = json.loads((REAL / 'incubator-form-state-rev.json').read_text())
        rows = self.mapped(data, snapshot)
        counts = Counter(r['status'] for r in rows)
        full = next(x for x in data if x['track'] == 'full')
        publishable = sum(isinstance(r.get('provenance'), dict) and not r.get('placeholder')
                          for s in full['slots'] for r in s['runs'])
        self.assertEqual(sum(counts.values()), publishable)
        self.assertGreaterEqual(counts['resolved'], 300)
        self.assertEqual(len(spans.acts(rows, STAMP)), counts['resolved'])
        print('Real-file counts:', dict(counts))


if __name__ == '__main__':
    unittest.main()
