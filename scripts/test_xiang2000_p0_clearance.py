"""P14 mutations use the real clearance fixture; query ports stay offline."""
import copy
import json
import unittest
import xiang2000_p0 as p0
from test_xiang2000_p6o3 import KIMI

LAB = p0.REPO / 'holes/labs/M-象-2000'
RECORD = p0.edn((LAB / 'P14-clearance-record.edn').read_text())['record']
CONTEXT = json.loads((LAB / 'P14-validation-context.json').read_text())


def files(record):
    turns = [{'evidence/id': i, 'evidence/at': '2026-09-25T19:00:00Z',
              'evidence/author': 'joe', 'evidence/origin': {'kind': 'harness'},
              'evidence/body': {'event': 'chat-turn', 'role': 'user', 'text': KIMI}}
             for i in CONTEXT['notice-ids']]
    return {'notice-turns.jsonl': '\n'.join(map(json.dumps, turns)).encode(),
            'origin-backfills.jsonl': b'',
            'evidence.jsonl': '\n'.join(map(json.dumps, CONTEXT['evidence'])).encode(),
            'rules.json': json.dumps({'hyperedges': CONTEXT['rules']}),
            'git.json': json.dumps({'commits': {'d5e3147e': CONTEXT['resolution-commit']}}),
            'pins.json': json.dumps({'evidence-system-as-of': CONTEXT['system-as-of']}),
            'clearance.json': json.dumps({'hyperedges': [{'hx/type': 'incident/clearance', 'hx/props': record}]})}


class ClearanceTest(unittest.TestCase):
    def test_real_record(self):
        self.assertEqual('15:48 incident explained by 15:54 (d5e3147e); measures may end (permission only); 42 notices owed/unsettled',
                         p0.clearance_answer(files(RECORD)))

    def test_missing_notice_names_clearance_row(self):
        bad = copy.deepcopy(RECORD)
        bad['clearance/compensation'].pop()
        with self.assertRaisesRegex(ValueError, r'row 12 \(clearance\).*41 vs 42'):
            p0.clearance_answer(files(bad))

    def test_same_count_wrong_notice_refused(self):
        bad = copy.deepcopy(RECORD)
        bad['clearance/compensation'][0]['evidence-id'] = 'not-a-notice'
        with self.assertRaisesRegex(ValueError, 'compensation set differs'):
            p0.clearance_answer(files(bad))

    def test_broader_claim_refused(self):
        bad = copy.deepcopy(RECORD)
        bad['clearance/recognition']['claim'] = 'the rule prevented the incident'
        with self.assertRaisesRegex(ValueError, 'recognition broader'):
            p0.clearance_answer(files(bad))

if __name__ == '__main__':
    unittest.main()
