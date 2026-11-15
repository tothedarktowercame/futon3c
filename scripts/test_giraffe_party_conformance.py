#!/usr/bin/env python3
"""Invented fixtures only. Mutations are copies; no producer/source edits.

Overlapping logical assertions deliberately report all affected checks:
Questor eligibility implies its exclusion theorem; duplicating a party entry
also violates exact round-robin replay. Hiding these failures would weaken audit.
"""
import copy
import importlib.util
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('conformance', Path(__file__).with_name('giraffe_party_conformance.py'))
c = importlib.util.module_from_spec(spec)
spec.loader.exec_module(c)


def fixture():
    # INVENTED disjoint lists make mutations' consequences explicit.
    lists = {'thor': ['T-ember', 'T-flint'], 'merlin': ['M-alpha', 'M-beta'],
             'questor': ['T-ivy', 'T-jade'], 'thyra': ['T-glow', 'T-halo']}
    party = {'characters': [{'id': name, 'top': [{'item': item, 'score': 3}
              for item in items]} for name, items in lists.items()], 'party': {'top': []}}
    rebuild(party)
    pending = [{'id': item, 'entities': ['alpha'], 'pending_label': c.READY,
                'harm': 'H2', 'pending': 1} for items in lists.values() for item in items]
    passage = {'order': [{'id': item, 'rank': i + 1, 'unblocks': 10-i, 'cone': 20-i}
                        for i, item in enumerate(sum(lists.values(), []))]}
    lexicon = [{'entity': name, 'status': 'partly', 'relations': 'uses ' + other}
               for name, other in [('alpha', 'beta'), ('beta', 'gamma'), ('gamma', 'alpha')]]
    return party, passage, pending, lexicon


def rebuild(party):
    party['party']['top'] = [{'item': ch['top'][i]['item'], 'chosen_by': ch['id']}
                            for i in range(2) for ch in party['characters']]


def failures(data):
    return {r['check'] for r in c.audit(*data) if r['status'] == 'FAIL'}


class ConformanceTests(unittest.TestCase):
    def test_conforming(self):
        self.assertEqual({'PASS'}, {r['status'] for r in c.audit(*fixture())})

    def test_thor_mutation(self):
        data = fixture()
        data[0]['characters'][0]['top'].reverse()
        rebuild(data[0])
        self.assertEqual({'thor_monotone_unblocks'}, failures(data))

    def test_thor_cone_tie(self):
        data = fixture()
        data[1]['order'][0]['unblocks'] = data[1]['order'][1]['unblocks']
        data[1]['order'][0]['cone'] = 0
        self.assertEqual({'thor_monotone_unblocks'}, failures(data))

    def test_thyra_harm_mutation(self):
        data = fixture()
        data[2][-1]['harm'] = ''
        self.assertEqual({'thyra_only_harm_items'}, failures(data))

    def test_thyra_rank_mutation(self):
        data = fixture()
        data[0]['characters'][3]['top'].reverse()
        rebuild(data[0])
        self.assertEqual({'thyra_monotone'}, failures(data))

    def test_merlin_order_mutation(self):
        data = fixture()
        data[0]['characters'][1]['top'].reverse()
        rebuild(data[0])
        self.assertEqual({'merlin_monotone_score'}, failures(data))

    def test_merlin_strict_score_mutation(self):
        data = fixture()
        data[2][2]['entities'] = []
        data[0]['characters'][1]['top'][0]['score'] = 2
        self.assertEqual({'merlin_monotone_score'}, failures(data))

    def test_merlin_score_not_trusted(self):
        data = fixture()
        data[0]['characters'][1]['top'][0]['score'] = 900
        self.assertEqual({'merlin_monotone_score'}, failures(data))

    def test_questor_cost_gate_mutation(self):
        self.assert_questor_overlap('specified, multi-day work')

    def test_questor_unready_mutation(self):
        self.assert_questor_overlap('waiting on an answer')

    def assert_questor_overlap(self, label):
        data = fixture()
        data[2][4]['pending_label'] = label
        self.assertEqual({'questor_cost_gate', 'questor_never_lists_unready'}, failures(data))

    def test_party_duplicate_mutation(self):
        data = fixture()
        data[0]['party']['top'].append(copy.deepcopy(data[0]['party']['top'][0]))
        self.assertEqual({'party_nodup', 'party_turns', 'party_relative_order'}, failures(data))

    def test_party_turn_skip_mutation(self):
        data = fixture()
        rows = data[0]['party']['top']
        rows[0], rows[1] = rows[1], rows[0]
        self.assertEqual({'party_turns'}, failures(data))

    def test_party_relative_order_mutation(self):
        data = fixture()
        rows = data[0]['party']['top']
        rows[0], rows[4] = rows[4], rows[0]
        self.assertEqual({'party_relative_order', 'party_turns'}, failures(data))

    def test_half_integral_merlin_score_conforms(self):
        # The Lean model keeps the doubled score (futon5 afdb6eb), so a
        # half-integral displayed score is not a failure.
        data = fixture()
        data[3].append({'entity': 'delta', 'status': 'partly', 'relations': 'uses alpha'})
        for row in data[0]['characters'][1]['top']:
            row['score'] = 4.5
        self.assertEqual(set(), failures(data))

    def test_missing_sources_are_declared(self):
        party, _, _, _ = fixture()
        results = c.audit(party)
        self.assertEqual(6, sum(r['status'] == 'ABSENT' for r in results))
        self.assertEqual(3, sum(r['status'] == 'PASS' for r in results))

    def test_missing_metric_is_not_zero(self):
        data = fixture()
        del data[1]['order'][0]['unblocks']
        row = c.audit(*data)[0]
        self.assertEqual('ABSENT', row['status'])
        self.assertIn('unblocks', row['detail']['reason'])

    def test_missing_character_is_invalid(self):
        data = fixture()
        data[0]['characters'].pop(0)
        self.assertIn('thor_monotone_unblocks', failures(data))

    def test_edn_and_json_read_only(self):
        with tempfile.TemporaryDirectory() as root:
            path = Path(root) / 'pass.edn'
            path.write_text('{:order [{:id "T-invented" :unblocks 2 :cone 4 :rank 1}]}')
            before = path.read_bytes()
            self.assertEqual('T-invented', c.load(path, edn=True)['order'][0]['id'])
            self.assertEqual(before, path.read_bytes())
            self.assertIsNone(c.load(Path(root) / 'missing.json'))

    def test_turn_duplicate_does_not_burn_turn(self):
        data = fixture()
        # Merlin repeats Thor's already-picked item before its own first.
        data[0]['characters'][1]['top'].insert(0, {'item': 'T-ember', 'score': 0})
        results = {r['check']: r for r in c.audit(*data)}
        for name in ('party_turns', 'party_nodup', 'party_relative_order'):
            self.assertEqual('PASS', results[name]['status'])


if __name__ == '__main__':
    unittest.main()
