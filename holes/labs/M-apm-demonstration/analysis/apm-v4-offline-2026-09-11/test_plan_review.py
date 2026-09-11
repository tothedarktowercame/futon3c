import copy
import unittest
from plan_review import assess


def fixture():
    return {'nodes': [dict(id='P', parent=None, goal='bound values',
        conditions='domain admits a finite eta-net', children=[], depends_on=[],
        methods=[dict(id='finite-net', git_pin='pin', sha256='digest')]) ]}


class ReviewControls(unittest.TestCase):
    def test_citation_and_old_status_do_not_close_obligation(self):
        p = fixture()
        p['nodes'][0]['status'] = 'paper-proof-available'
        result = assess(p, {})
        self.assertFalse(result['ready_for_ta_review'])
        self.assertEqual(len(result['open_obligations']), 2)

    def test_bounded_discrete_contrast_keeps_condition_refuted(self):
        r = assess(fixture(), {'P': {'conditions': 'refuted',
          'proof_argument': 'The proposed finite-net argument cannot apply.',
          'condition_argument': 'Infinite discrete space has no finite half-unit net.'}})
        self.assertEqual(r['open_obligations'][0]['reason'], 'conditions-refuted')

    def test_claimed_conditions_need_an_argument(self):
        r = assess(fixture(), {'P': {'conditions': 'established', 'proof_argument': 'text'}})
        self.assertIn('missing-condition-argument:P', r['structural_errors'])

    def test_complete_review_is_not_proof_verification(self):
        r = assess(fixture(), {'P': {'conditions': 'established',
             'condition_argument': 'explicit construction to be checked',
             'proof_argument': 'explicit estimate to be checked'}})
        self.assertTrue(r['ready_for_ta_review'])
        self.assertFalse(r['mathematics_verified'])

    def test_disconnected_cycle_is_not_ignored(self):
        p = fixture()
        for key, parent in [('A', 'B'), ('B', 'A')]:
            n = copy.deepcopy(p['nodes'][0])
            n.update(id=key, parent=parent, children=[parent])
            p['nodes'].append(n)
        errors = assess(p, {})['structural_errors']
        self.assertTrue(any(x.startswith('cyclic-obligation:') for x in errors))
        self.assertIn('unreachable-node:A', errors)

    def test_unknown_dependency_duplicate_and_unpinned(self):
        p = fixture()
        p['nodes'][0]['depends_on'] = ['missing']
        p['nodes'][0]['methods'][0].pop('sha256')
        p['nodes'].append(copy.deepcopy(p['nodes'][0]))
        errors = assess(p, {})['structural_errors']
        for e in ['unknown-obligation:missing', 'duplicate-node-id', 'unpinned-method:P']:
            self.assertIn(e, errors)

    def test_child_and_parent_must_agree(self):
        p = fixture()
        n = copy.deepcopy(p['nodes'][0]); n.update(id='C', parent='P')
        p['nodes'].append(n)
        self.assertIn('parent-not-reciprocal:C', assess(p, {})['structural_errors'])


if __name__ == '__main__':
    unittest.main()
