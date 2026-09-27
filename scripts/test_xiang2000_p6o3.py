"""Rules and append-only replay checks for the historical origin interpreter."""
import copy
import unittest
from urllib.parse import unquote
import xiang2000_p6o3 as m

WAKE = 'Check my job.\n\n--- resumed: parked dependencies complete (1) ---\n• invoke-test: done'
INBOX = 'inbox-zero: futon3c is carrying 2 dirty file(s) (1 untracked); 1 of them were written during your turns. Commit or delete what is yours and leave what is not. Newest first: test.py. Full list: git -C /home/joe/code/futon3c status --porcelain'
KIMI = 'You requisitioned kimi-1 for M-test while clocked on M-other. If your work has moved to M-test, clock in on it so your clock says what you are doing.'
# Real operator turn emacs-46e69c9bcb4fad45e86fd6a63e649a84, 2026-09-24 16:20.
JOE = '''So, we don't need to redirect claude-1 or claude-10 "live" but we should create an enforcement rule similar to the inbox-zero followup: message that says "You can't use Kimi without sending a work target" or similar.  Indeed, the usual use case I have in mind is that the calling agent (say, claude-1) should clock in on something and then just send that as its work target for Kimi.  This is why I'm saying that if M-autoclock-in features actually worked, we wouldn't have to think about this much.  If claude-10 stays stuck on M-futon-seams even if work drifts to another topic, the risk is that it keeps calling Kimi with M-futon-seams as the work target and never triggers a compaction.'''


class Store:
    def __init__(self):
        self.source = {'evidence/id': 'original', 'evidence/at': '2026-09-24T12:00:00Z',
                       'evidence/author': 'joe', 'evidence/body': {'event': 'chat-turn', 'role': 'user', 'text': WAKE}}
        self.entries = {}
        self.posts = []

    def rows(self, params, pin):
        return list(self.entries.values()) if 'type' in params else [self.source]

    def request(self, method, suffix='', payload=None):
        if method == 'GET':
            return 200, self.entries[unquote(suffix[1:])]
        assert method == 'POST'
        self.posts.append(copy.deepcopy(payload))
        assert payload['id'] != self.source['evidence/id']
        assert payload['id'] not in self.entries, 'duplicate write'
        self.entries[payload['id']] = {'evidence/' + k: v for k, v in payload.items()}
        return 201, {}

    def plan(self):
        r = self.source
        c = dict(m.classify(WAKE), **{'source-id': r['evidence/id'], 'source-at': r['evidence/at'], 'source-hash': m.digest(r)})
        return {'version': m.VERSION, 'window': [m.START, m.END], 'system-as-of': m.now(), 'candidates': [c]}


class Rules(unittest.TestCase):
    def test_templates(self):
        for text, rule in [(WAKE, 'park-wake'), (INBOX, 'inbox-zero'), (KIMI, 'kimi-notice')]:
            with self.subTest(rule=rule):
                self.assertEqual(rule, m.classify(text)['rule'])
        self.assertEqual('park-wake', m.classify('--- resumed: DEADLINE EXPIRED with 0 of 2 dependencies complete — the awaited work did NOT finish ---')['rule'])
        self.assertIsNone(m.classify('--- resumed: parked dependencies complete (1) ---'))

    def test_operator_quote_bad_case(self):
        self.assertIsNone(m.classify('Please fix the "resumed: parked dependencies complete" line in the UI.'))
        self.assertIsNone(m.classify(JOE))
        self.assertIsNone(m.classify('Here is a quoted example:\n```\n' + WAKE + '\n```'))
        self.assertIsNone(m.classify('Please change this notice: ' + INBOX))
        self.assertIsNone(m.classify('The agent keeps saying: ' + KIMI))

    def test_kimi_variants(self):
        for suffix in [' while not clocked in', ', which is not what your clock said']:
            text = "You requisitioned kimi-6 for E-test" + suffix + ". If your work has moved to E-test, this reminder clocks you onto it; if it hasn't, clock back onto what you are doing."
            self.assertEqual('kimi-notice', m.classify(text)['rule'])
        text = "You can't use a Kimi seat without a requisition. Put one line in the call: `Requisition: <M-*|E-*|T-*> — <one-line purpose>` — your clock says M-test; if that is what this is: `Requisition: M-test — <purpose>`. The seat keeps its conversation per target and clears it when the target changes. (Refused by kimi-2.)"
        self.assertEqual('kimi-notice', m.classify(text)['rule'])
        self.assertIsNone(m.classify(KIMI.replace('moved to M-test', 'moved to M-other')))

    def test_append_only_idempotent_and_distinct_basis(self):
        store = Store(); original = copy.deepcopy(store.source); plan = store.plan()
        self.assertEqual(1, m.write(store, plan)['written'])
        self.assertEqual({'written': 0, 'existing': 1, 'originals-modified': 0}, m.write(store, plan))
        self.assertEqual(original, store.source)
        self.assertEqual(1, len(store.posts))
        post = store.posts[0]
        self.assertEqual('backfill-inferred', post['body']['basis'])
        self.assertEqual('write-time', post['origin']['basis'])
        self.assertEqual('original', post['subject']['ref/id'])

    def test_plan_tampering_refused_before_write(self):
        for field, value in [('source-hash', 'bad'), ('origin', 'operator'), ('source-at', m.END)]:
            store = Store(); plan = store.plan(); plan['candidates'][0][field] = value
            with self.assertRaisesRegex(ValueError, 'mismatch'):
                m.write(store, plan)
            self.assertEqual([], store.posts)

    def test_existing_conflict_refused(self):
        store = Store(); plan = store.plan(); m.write(store, plan)
        next(iter(store.entries.values()))['evidence/body']['origin'] = 'operator'
        with self.assertRaisesRegex(ValueError, 'Conflicting'):
            m.write(store, plan)
        self.assertEqual(1, len(store.posts))


if __name__ == '__main__':
    unittest.main()
