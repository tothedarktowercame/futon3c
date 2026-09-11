import copy
import unittest
from library_loop import apply, digest, initial, proposal_from_obligation


class LibraryLoopTest(unittest.TestCase):
    def setUp(self):
        self.base = initial({'finite-net': {'conditions': 'bounded domain', 'construction': 'finite net then maximum'}})
        self.cascade = {'problem': 'source', 'nodes': [{'id': 'B2', 'goal': 'find finite net'}]}
        self.proposal = proposal_from_obligation(self.base, self.cascade, 'B2', author='ta',
             pattern='finite-net', category='applicability',
             patch={'conditions': 'totally bounded domain', 'failure_contrasts': ['infinite discrete bounded space']},
             evidence='test fixture diagnosis', expected_change='reject mere boundedness')
        self.pid = digest(self.proposal)
        self.proposed = apply(self.base, self.proposal)
        self.rev = self.proposed['proposals'][self.pid]['revision']
        self.review = dict(type='review', proposal_id=self.pid, revision=self.rev,
                           reviewer='reviewer', verdict='accept', evidence='test review only')

    def published(self):
        return apply(apply(self.proposed, self.review), {'type': 'publish', 'proposal_id': self.pid})

    def started(self):
        return apply(self.published(), dict(type='begin-use', proposal_id=self.pid,
                     attempt_id='a1', student='student', problem='target'))

    def observation(self, **kw):
        return dict(type='observe', proposal_id=self.pid, revision=self.rev, attempt_id='a1',
                    student='student', problem='target', evidence='test observation', retrieved=True,
                    read=True, applicable=True, proof_used=True, useful=True,
                    reviewer='independent', review_evidence='test adjudication', **kw)

    def test_publication_changes_library_but_does_not_claim_use(self):
        s = self.published()
        self.assertEqual(s['library']['finite-net']['body']['conditions'], 'totally bounded domain')
        self.assertEqual(s['proposals'][self.pid]['next_use'], 'not-yet-observed')
        self.assertEqual(self.base['library']['finite-net']['body']['conditions'], 'bounded domain')

    def test_review_binds_revision_and_author_separation(self):
        for changes in [{'reviewer': 'ta'}, {'revision': 'wrong'}]:
            with self.assertRaises(ValueError): apply(self.proposed, {**self.review, **changes})
        with self.assertRaises(ValueError): apply(self.proposed, {'type':'publish','proposal_id':self.pid})

    def test_rejected_revision_cannot_publish(self):
        s = apply(self.proposed, {**self.review, 'verdict': 'reject'})
        with self.assertRaises(ValueError): apply(s, {'type':'publish','proposal_id':self.pid})

    def test_stale_publication_cannot_overwrite_another_revision(self):
        s = apply(self.proposed, self.review)
        s['library']['finite-net']['revision'] = 'newer'
        before = copy.deepcopy(s)
        with self.assertRaises(ValueError): apply(s, {'type':'publish','proposal_id':self.pid})
        self.assertEqual(s, before)

    def test_begin_use_exports_exact_entry_and_reviewed_use_is_distinct(self):
        s = self.started()
        self.assertEqual(s['attempts']['a1']['entry'], s['library']['finite-net'])
        s = apply(s, self.observation())
        self.assertEqual(s['proposals'][self.pid]['next_use'], 'reviewed-useful-example')
        with self.assertRaises(ValueError): apply(s, self.observation())

    def test_citation_only_is_not_usefulness(self):
        o = self.observation(); o.update(read=False, applicable=False, proof_used=False, useful=False)
        s = apply(self.started(), o)
        self.assertEqual(s['proposals'][self.pid]['next_use'], 'observed-without-usefulness-witness')
        o['useful'] = True
        with self.assertRaises(ValueError): apply(self.started(), o)

    def test_wrong_problem_revision_unstarted_and_self_review_refused(self):
        for changes in [{'problem':'source'}, {'revision':'wrong'}, {'attempt_id':'unknown'}, {'reviewer':'student'}]:
            with self.assertRaises(ValueError): apply(self.started(), {**self.observation(), **changes})

    def test_category_and_real_obligation_binding(self):
        self.assertEqual(self.proposal['trigger']['cascade_digest'], digest(self.cascade))
        with self.assertRaises(ValueError): apply(self.base, {**self.proposal, 'category':'retrieval'})
        with self.assertRaises(ValueError): proposal_from_obligation(self.base, self.cascade, 'absent',
            author='ta', pattern='finite-net', category='applicability', patch={'conditions':'x'}, evidence='x', expected_change='x')

if __name__ == '__main__':
    unittest.main()
