"""Offline fixtures for the call-notes move planner."""
from contextlib import redirect_stderr
from copy import deepcopy
import io
import json
from pathlib import Path
import tempfile
import unittest
from scripts import together_move_region as move


def cid(n):
    return ['original', n]


def node(a, b, **attrs):
    return {'kind': 'span', 'attrs': dict(doc='incubator-form', start=cid(a), end=cid(b), **attrs)}


def fixture(text='H\nAnswer\n\nNotes 😀 end\n'):
    state = {'rev': 1, 'runs': [dict(id=cid(1), text=text, attrs={'author': 'joe', 'face': 'typed'})]}
    notes = {'nodes': {}, 'derived': {}}
    slots = {'nodes': {'heading': node(1, 1, role='heading', order=28, slot='disclosure')}}
    return state, notes, slots


class MoveTests(unittest.TestCase):
    def test_region_and_chain_attrs(self):
        state, notes, slots = fixture()
        full = state['runs'][0]['text']
        attrs = state['runs'][0]['attrs']
        state['runs'] = [dict(id=cid(1), text=full[:15], attrs=attrs),
                         dict(id=cid(16), text=full[15:17], attrs=attrs),
                         dict(id=cid(18), text=full[17:], attrs={'author': 'murch', 'mark': {'x': 1}})]
        before = deepcopy(state)
        ops, mapping, acts, _ = move.plan(state, notes, slots)
        self.assertEqual(''.join(o['text'] for o in ops), 'Notes 😀 end\n')
        self.assertEqual(len(ops), 2)  # Equal attrs across source runs coalesce.
        self.assertEqual(ops[0]['after'], None)
        self.assertEqual(ops[1]['after'], ['fable-1.move', len(ops[0]['text'])])
        self.assertEqual(ops[1]['id'][1], len(ops[0]['text']) + 1)
        self.assertEqual(ops[0]['attrs']['author'], 'joe')
        self.assertEqual(ops[1]['attrs']['author'], 'murch')
        self.assertEqual(ops[1]['attrs']['mark'], {'x': 1})
        self.assertEqual(ops[0]['attrs']['moved_from'], cid(11))
        self.assertEqual(mapping[move.id_key(cid(17))], ['fable-1.move', 7])
        self.assertEqual(acts, [])
        self.assertEqual(state, before)

    def test_inside_outside_straddling_and_superseded(self):
        state, notes, slots = fixture()
        notes['nodes'] = {'inside': node(11, 15, by='reviewer', text='comment'),
                          'outside': node(3, 8), 'crossing': node(7, 15),
                          'old': node(11, 15), 'new': node(17, 19)}
        notes['derived'] = {'new': 'old'}
        _, _, acts, report = move.plan(state, notes, slots)
        self.assertEqual([a['replaces'] for a in acts], ['inside', 'new'])
        self.assertEqual(acts[0]['attrs']['doc'], 'incubator-call-notes')
        self.assertEqual(acts[0]['attrs']['text'], 'comment')
        self.assertEqual(acts[0]['attrs']['start'], ['fable-1.move', 1])
        self.assertIn("Untouched: ['outside']", report)
        self.assertIn("Straddles: ['crossing']", report)
        self.assertIn('Before:\n```text\nNotes\n```', report)
        self.assertIn('After:\n```text\nNotes\n```', report)

    def test_deleted_endpoints_trim_inward(self):
        state, notes, slots = fixture()
        text = state['runs'][0]['text']
        state['runs'] = [dict(id=cid(i + 1), text=c, attrs={'author': 'joe'}, deleted=i in (10, 14))
                         for i, c in enumerate(text)]
        notes['nodes']['note'] = node(11, 15)
        _, mapping, acts, report = move.plan(state, notes, slots)
        self.assertEqual(acts[0]['attrs']['start'], mapping[move.id_key(cid(12))])
        self.assertEqual(acts[0]['attrs']['end'], mapping[move.id_key(cid(14))])
        self.assertIn('Before:\n```text\note\n```', report)

    def test_no_blank_or_empty_region_refuses_without_files(self):
        for text in ['H\nAnswer notes', 'H\nAnswer\n\n  \t']:
            with self.subTest(text=text), tempfile.TemporaryDirectory() as d, redirect_stderr(io.StringIO()):
                p = Path(d)
                argv = []
                for name, value in zip(('state', 'notes', 'slots'), fixture(text)):
                    path = p / (name + '.json')
                    path.write_text(json.dumps(value))
                    argv.extend(['--' + name, str(path)])
                self.assertEqual(move.main(argv + ['--out', str(p / 'out')]), 1)
                self.assertFalse((p / 'out').exists())

    def test_blank_line_before_answer_is_not_boundary(self):
        state, notes, slots = fixture('H\n\n  Answer\n \t\n  Notes')
        ops, _, _, _ = move.plan(state, notes, slots)
        self.assertEqual(''.join(o['text'] for o in ops), 'Notes')

    def test_collision_refuses(self):
        state, notes, slots = fixture()
        notes['nodes'] = {'note': node(11, 15), 'note.m1': node(3, 8)}
        with self.assertRaisesRegex(ValueError, 'already in use'):
            move.plan(state, notes, slots)


if __name__ == '__main__':
    unittest.main()
