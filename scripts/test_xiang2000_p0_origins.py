"""P0's notice count uses origin evidence, with write-time stamps taking precedence."""
import json
import unittest
import xiang2000_p0 as p0
from test_xiang2000_p6o3 import KIMI


def files(kind=None, interpretations=1):
    row = {'evidence/id': 'notice', 'evidence/author': 'joe',
           'evidence/at': '2026-09-25T19:59:53Z',
           'evidence/body': {'event': 'chat-turn', 'role': 'user', 'text': KIMI}}
    if kind:
        row['evidence/origin'] = {'kind': kind}
    backfill = {'evidence/type': 'origin/backfill', 'evidence/body': {
        'source-id': 'notice', 'basis': 'backfill-inferred',
        'rule': 'kimi-notice', 'origin': 'harness'}}
    return {'notice-turns.jsonl': json.dumps(row).encode(),
            'origin-backfills.jsonl': ('\n'.join(json.dumps(backfill) for _ in range(interpretations))).encode()}


class NoticeOrigins(unittest.TestCase):
    def test_backfill_required_for_unknown_origin(self):
        self.assertEqual(1, p0.notice_count(files()))
        self.assertEqual(0, p0.notice_count(files(interpretations=0)))
        self.assertEqual(1, p0.notice_count(files(kind='unknown')))

    def test_write_time_origin_has_precedence(self):
        self.assertEqual(1, p0.notice_count(files(kind='harness', interpretations=0)))
        self.assertEqual(0, p0.notice_count(files(kind='operator')))
        self.assertEqual(0, p0.notice_count(files(kind='agent')))

    def test_count_source_turns_not_duplicate_interpretations(self):
        self.assertEqual(1, p0.notice_count(files(interpretations=2)))

    def test_wrong_rule_and_operator_discussion_excluded(self):
        f = files()
        f['origin-backfills.jsonl'] = f['origin-backfills.jsonl'].replace(b'kimi-notice', b'park-wake')
        self.assertEqual(0, p0.notice_count(f))
        f = files(kind='harness')
        r = json.loads(f['notice-turns.jsonl'])
        r['evidence/body']['text'] = 'Please fix this notice: ' + KIMI
        f['notice-turns.jsonl'] = json.dumps(r).encode()
        self.assertEqual(0, p0.notice_count(f))


if __name__ == '__main__':
    unittest.main()
