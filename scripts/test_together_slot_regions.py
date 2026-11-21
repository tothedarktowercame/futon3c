"""Synthetic slot-region fixtures only."""
from contextlib import redirect_stderr
import io
import json
from pathlib import Path
import tempfile
import unittest
from scripts import together_slot_regions as slots


def state(text, deleted=False):
    return dict(rev=1, runs=[dict(id=['fable-1.restore', 1], text=text,
                                attrs={'source': 'incubator-buffer-copy'}, deleted=deleted)])


def headings(*texts):
    return {'slots': [dict(slot=f'q{i}', heading=t, placeholders=[]) for i, t in enumerate(texts)]}


class SlotTests(unittest.TestCase):
    def test_all_placed(self):
        data = headings('First question', 'Second question')
        data['slots'][0]['placeholders'] = ['No answer yet']
        snapshot = state('😀 First\nquestion\nNo  answer yet\nSecond question\nAnswer')
        acts, texts, _, errors = slots.build(data, snapshot)
        self.assertEqual(errors, [])
        self.assertEqual(len(acts), 3)
        self.assertEqual(acts[0]['attrs']['start'], ['fable-1.restore', 3])
        self.assertEqual(acts[2]['attrs']['order'], 2)
        slots.verify(acts, texts, snapshot)

    def test_order_disambiguates_earlier_occurrence(self):
        snapshot = state('Repeated title in preamble. Unique first. Repeated title')
        acts, texts, _, errors = slots.build(headings('Unique first.', 'Repeated title'), snapshot)
        self.assertEqual(errors, [])
        self.assertGreater(acts[1]['attrs']['start'][1], acts[0]['attrs']['end'][1])
        slots.verify(acts, texts, snapshot)

    def test_duplicate_remaining_occurrences_refused(self):
        _, _, _, errors = slots.build(headings('First', 'Repeated'), state('First Repeated Repeated'))
        self.assertIn('2 matches', errors[0])

    def test_missing_heading_no_acts(self):
        with tempfile.TemporaryDirectory() as directory, redirect_stderr(io.StringIO()):
            p = Path(directory)
            (p / 'headings.json').write_text(json.dumps(headings('Missing')))
            (p / 'state.json').write_text(json.dumps(state('Other')))
            self.assertEqual(slots.main(['--headings', str(p / 'headings.json'), '--state',
                                         str(p / 'state.json'), '--out', str(p / 'out')]), 1)
            self.assertFalse((p / 'out/acts.json').exists())
            self.assertIn('0 matches', (p / 'out/report.md').read_text())

    def test_out_of_order_refused(self):
        _, _, _, errors = slots.build(headings('First', 'Second'), state('Second First'))
        self.assertTrue(errors)

    def test_deleted_heading_retained(self):
        snapshot = state('Heading', deleted=True)
        acts, texts, _, errors = slots.build(headings('Heading'), snapshot)
        self.assertFalse(errors)
        slots.verify(acts, texts, snapshot)

    def test_corrupt_id_caught(self):
        snapshot = state('Heading')
        acts, texts, _, _ = slots.build(headings('Heading'), snapshot)
        acts[0]['attrs']['start'][1] = 2
        with self.assertRaisesRegex(ValueError, 'verification failed'):
            slots.verify(acts, texts, snapshot)


if __name__ == '__main__':
    unittest.main()
