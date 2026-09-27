import copy
import json
import unittest
import xiang2000_p0 as p0

class TimelineTest(unittest.TestCase):
    def test_application_not_commit(self):
        t = {'family': p0.RULE_FAMILY, 'version': 1, 'kind': 'dispatcher-run',
             'effect': 'requisition-with-followups',
             'committed': [{'at': '2026-09-24T16:34:08Z'}],
             'live': {'at': '2026-09-24T19:04:24Z', 'status': 'applied',
                      'source': {'kind': 'execution', 'ref': 'evidence:notice'}}}
        def files(t):
            return {'rules.json': json.dumps({'hyperedges': [{'hx/props': {'rule/timeline': t}}]})}
        self.assertEqual('committed, not yet live', p0.rule_asof(files(t), '2026-09-24T17:00:00Z'))
        wrong = copy.deepcopy(t)
        wrong['live']['at'] = wrong['committed'][0]['at']
        self.assertNotEqual('committed, not yet live', p0.rule_asof(files(wrong), '2026-09-24T17:00:00Z'))
        del t['live']
        self.assertEqual('committed, not yet live', p0.rule_asof(files(t), '2026-09-25T21:00:00Z'))

if __name__ == '__main__':
    unittest.main()
