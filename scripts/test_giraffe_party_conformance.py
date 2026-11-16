#!/usr/bin/env python3
"""Invented fixtures only. Mutations are copies; no producer/source edits.

Overlapping logical assertions deliberately report all affected checks:
Questor eligibility implies its exclusion theorem; duplicating a party entry
also violates exact round-robin replay. Hiding these failures would weaken audit.
"""
import copy
import hashlib
import json
import importlib.util
from pathlib import Path
import tempfile
import unittest
import sqlite3

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
    party['absences'] = []
    for row, entry in zip(party['characters'][0]['top'], passage['order']):
        row['reason'] = (f"unblocks {entry['unblocks']} item(s); "
                         f"cone of {entry['cone']} behind it")
    for row in party['characters'][3]['top']:
        entry = next(e for e in passage['order'] if e['id'] == row['item'])
        row['reason'] = f"on harm line H2; Giraffe pass rank {entry['rank']}"
    for row in party['characters'][2]['top']:
        row['reason'] = 'ready (one small handoff); names alpha (convergence 2)'
    for row in party['characters'][1]['top']:
        row['reason'] = (f"is {row['item'][2:]} (convergence 2, unfinished work within two hops)"
                         "; names alpha (convergence 2, halved)")
    return party, passage, pending, lexicon


def rebuild(party):
    party['party']['top'] = [{'item': ch['top'][i]['item'], 'chosen_by': ch['id']}
                            for i in range(2) for ch in party['characters']]


def failures(data):
    return {r['check'] for r in c.audit(*data) if r['status'] == 'FAIL'}


class ConformanceTests(unittest.TestCase):
    def test_conforming(self):
        results = c.audit(*fixture())
        self.assertEqual({'PASS'}, {r['status'] for r in results if r['check'] not in
                          ('run_file_digests', 'one_current_reading', 'corpus_generation_digest', 'reading_source_current')})
        self.assertEqual('ABSENT', next(r for r in results if r['check'] == 'corpus_generation_digest')['status'])

    def test_thor_mutation(self):
        data = fixture()
        data[0]['characters'][0]['top'].reverse()
        rebuild(data[0])
        self.assertEqual({'thor_monotone_unblocks'}, failures(data))

    def test_thor_cone_tie(self):
        data = fixture()
        data[1]['order'][0]['unblocks'] = data[1]['order'][1]['unblocks']
        data[1]['order'][0]['cone'] = 0
        self.assertEqual({'thor_monotone_unblocks', 'thor_missing_counts'}, failures(data))

    def test_thyra_harm_mutation(self):
        data = fixture()
        data[2][-1]['harm'] = ''
        self.assertEqual({'thyra_only_harm_items', 'thyra_reasons_faithful'}, failures(data))

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
        self.assertEqual({'merlin_monotone_score', 'merlin_reasons_faithful'}, failures(data))

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
            row['reason'] = row['reason'].replace('convergence 2', 'convergence 3')
        for row in data[0]['characters'][2]['top']:
            row['reason'] = 'ready (one small handoff); names alpha (convergence 3)'
        self.assertEqual(set(), failures(data))

    def test_missing_sources_are_declared(self):
        party, _, _, _ = fixture()
        results = c.audit(party)
        self.assertEqual(14, sum(r['status'] == 'ABSENT' for r in results))
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


class RunDigestTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.run = Path(self.temp.name) / 'invented-run'
        self.run.mkdir()
        self.input = self.run / 'input.edn'
        self.input.write_bytes(b'{:items []}')
        self.manifest = self.run / 'manifest.edn'
        self.recorded = hashlib.sha256(self.input.read_bytes()).hexdigest()
        self.write_manifest('input.edn')

    def write_manifest(self, name):
        self.manifest.write_text('{:files [{:file "' + name +
                                 '" :sha256 "' + self.recorded + '"}]}')

    def test_intact(self):
        row = c.run_file_digests(self.run)
        self.assertEqual('PASS', row['status'])
        self.assertEqual({'files': 1}, row['detail'])
        rows = c.audit(*fixture(), run=self.run)
        self.assertEqual(17, len(rows))
        self.assertEqual({'PASS'}, {r['status'] for r in rows if r['check'] not in
                          ('one_current_reading', 'corpus_generation_digest', 'reading_source_current')})
        self.assertEqual('ABSENT', next(r for r in rows if r['check'] == 'corpus_generation_digest')['status'])

    def test_changed_byte(self):
        self.input.write_bytes(b'{:items [1]}')
        row = c.run_file_digests(self.run)
        self.assertEqual('FAIL', row['status'])
        self.assertEqual([{'file': 'input.edn', 'recorded': self.recorded,
                          'actual': hashlib.sha256(self.input.read_bytes()).hexdigest()}],
                         row['detail']['mismatches'])

    def test_missing_file(self):
        self.input.unlink()
        row = c.run_file_digests(self.run)
        self.assertEqual('ABSENT', row['status'])
        self.assertEqual(['input.edn'], row['detail']['files'])

    def test_input_not_listed_is_absent(self):
        for text in ('{:files []}',
                     '{:files [{:file "other.edn" :sha256 "' + self.recorded + '"}]}'):
            with self.subTest(text=text):
                if 'other' in text:
                    (self.run / 'other.edn').write_bytes(self.input.read_bytes())
                self.manifest.write_text(text)
                row = c.run_file_digests(self.run)
                self.assertEqual('ABSENT', row['status'])
                self.assertEqual(['input.edn'], row['detail']['files'])

    def test_missing_manifest(self):
        self.manifest.unlink()
        self.assertEqual('ABSENT', c.run_file_digests(self.run)['status'])

    def test_malformed_manifest(self):
        for text in ('{:files [', '{:files []} trailing', '{}', 'nil'):
            with self.subTest(text=text):
                self.manifest.write_text(text)
                self.assertEqual('FAIL', c.run_file_digests(self.run)['status'])

    def test_escape_is_not_opened(self):
        from unittest.mock import patch
        outside = Path(self.temp.name) / 'outside.edn'
        outside.write_bytes(self.input.read_bytes())
        (self.run / 'escape.edn').symlink_to(outside)
        for name in ('../outside.edn', str(outside), 'escape.edn'):
            with self.subTest(name=name):
                self.write_manifest(name)
                with patch.object(Path, 'read_bytes', side_effect=AssertionError('opened file')):
                    self.assertEqual('FAIL', c.run_file_digests(self.run)['status'])

    def test_manifest_symlink_escape(self):
        outside = Path(self.temp.name) / 'outside-manifest.edn'
        self.manifest.rename(outside)
        self.manifest.symlink_to(outside)
        from unittest.mock import patch
        with patch.object(Path, 'read_text', side_effect=AssertionError('opened manifest')):
            self.assertEqual('FAIL', c.run_file_digests(self.run)['status'])


class CurrentReadingTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.run = self.root / 'run'
        self.run.mkdir()
        self.generation = self.root / 'generation-one'
        self.generation.mkdir()
        self.batch = self.generation / 'batch-invented.jsonl'
        self.batch.write_text('{"id":"T-ember"}\n{"id":"M-flint"}\n')
        self.manifest = self.run / 'manifest.edn'
        self.manifest.write_text('{:readings-generation {:path ' +
                                 json.dumps(str(self.generation)) + '}}')

    def test_unique(self):
        row = c.one_current_reading(self.run)
        self.assertEqual('PASS', row['status'])
        self.assertEqual({'records': 2, 'items': 2}, row['detail'])

    def test_empty_generation_is_absent(self):
        self.batch.write_text('')
        row = c.one_current_reading(self.run)
        self.assertEqual('ABSENT', row['status'])
        self.assertEqual('reading records', row['detail']['missing'])

    def test_duplicate(self):
        (self.generation / 'batch-second.jsonl').write_text('{"id":"T-ember"}\n')
        row = c.one_current_reading(self.run)
        self.assertEqual('FAIL', row['status'])
        self.assertEqual([{'item': 'T-ember', 'record_ids': ['T-ember', 'T-ember']}],
                         row['detail']['duplicates'])

    def test_history_is_in_other_generation(self):
        # Old and replacement records coexist across generations, never within
        # one: apply-replacement! removes the old :id from every staged batch.
        old = self.root / 'generation-old'
        old.mkdir()
        (old / 'batch-old.jsonl').write_text('{"id":"T-ember"}\n')
        pointer = self.root / 'current'
        pointer.symlink_to(old, target_is_directory=True)
        self.manifest.write_text('{:readings-generation {:path ' +
                                 json.dumps(str(self.generation)) + ' :pointer ' +
                                 json.dumps(str(pointer)) + '}}')
        self.assertEqual({'records': 2, 'items': 2},
                         c.one_current_reading(self.run)['detail'])
        # An invented status annotation must not conceal an actual duplicate.
        (self.generation / 'batch-old.jsonl').write_text(
            '{"id":"T-ember","status":"superseded"}\n')
        self.assertEqual('FAIL', c.one_current_reading(self.run)['status'])

    def test_missing_generation(self):
        self.batch.unlink()
        self.generation.rmdir()
        row = c.one_current_reading(self.run)
        self.assertEqual('ABSENT', row['status'])
        self.assertEqual(str(self.generation), row['detail']['missing'])

    def test_no_pointer_fallback(self):
        self.manifest.write_text('{:readings-generation {:pointer ' +
                                 json.dumps(str(self.generation)) + '}}')
        self.assertEqual('ABSENT', c.one_current_reading(self.run)['status'])

    def test_invalid_records(self):
        for text in ('not-json', '{}', '{"id":null}', '[]'):
            with self.subTest(text=text):
                self.batch.write_text(text + '\n')
                row = c.one_current_reading(self.run)
                self.assertEqual('FAIL', row['status'])
                self.assertEqual(1, row['detail']['errors'][0]['line'])

    def test_missing_manifest_and_batches(self):
        self.batch.unlink()
        self.assertEqual('ABSENT', c.one_current_reading(self.run)['status'])
        self.manifest.unlink()
        self.assertEqual('ABSENT', c.one_current_reading(self.run)['status'])


class GenerationDigestTests(unittest.TestCase):
    # SHA-256 of literal a.txt NUL A nested/z.txt NUL B, independently derived.
    EXPECTED = '451ba148c7ed949846ef77db712abec8a1d87910f0cd2b5786ad8d7a3458514f'
    SINGLE = '89d1aee8d2371b90f003b45ade4eea75238f196e5e4b0522b836c2e5254ca830'

    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.run = self.root / 'run'
        self.run.mkdir()
        self.generation = self.root / 'generation'
        self.generation.mkdir()
        (self.generation / 'a.txt').write_bytes(b'A')
        (self.generation / 'nested').mkdir()
        self.nested = self.generation / 'nested/z.txt'
        self.nested.write_bytes(b'B')
        self.manifest(self.EXPECTED)

    def manifest(self, digest):
        (self.run / 'manifest.edn').write_text(
            '{:readings-generation {:path ' + json.dumps(str(self.generation)) +
            (' :digest "' + digest + '"' if digest is not None else '') + '}}')

    def test_intact(self):
        row = c.corpus_generation_digest(self.run)
        self.assertEqual('PASS', row['status'])
        self.assertEqual({'files': 2}, row['detail'])

    def test_nested_byte_changed(self):
        self.nested.write_bytes(b'C')
        row = c.corpus_generation_digest(self.run)
        self.assertEqual('FAIL', row['status'])
        self.assertEqual(self.EXPECTED, row['detail']['recorded'])

    def test_nested_file_added(self):
        # Start with only a.txt: a top-level-only walk would miss the addition
        # and wrongly keep returning PASS against this same baseline digest.
        self.nested.unlink()
        self.manifest(self.SINGLE)
        self.assertEqual('PASS', c.corpus_generation_digest(self.run)['status'])
        self.nested.write_bytes(b'B')
        self.assertEqual('FAIL', c.corpus_generation_digest(self.run)['status'])

    def test_rename(self):
        self.nested.rename(self.nested.with_name('renamed.txt'))
        self.assertEqual('FAIL', c.corpus_generation_digest(self.run)['status'])

    def test_missing_and_empty(self):
        self.nested.unlink()
        (self.generation / 'a.txt').unlink()
        self.assertEqual('ABSENT', c.corpus_generation_digest(self.run)['status'])
        (self.generation / 'nested').rmdir()
        self.generation.rmdir()
        self.assertEqual('ABSENT', c.corpus_generation_digest(self.run)['status'])

    def test_missing_digest(self):
        self.manifest(None)
        row = c.corpus_generation_digest(self.run)
        self.assertEqual('ABSENT', row['status'])
        self.assertEqual('readings-generation.digest', row['detail']['missing'])

    def test_directory_digest(self):
        renamed = self.root / ('0' * 64)
        self.generation.rename(renamed)
        self.generation = renamed
        self.manifest(self.EXPECTED)
        row = c.corpus_generation_digest(self.run)
        self.assertEqual('FAIL', row['status'])
        self.assertEqual('0' * 64, row['detail']['directory'])
        self.generation.rename(self.root / self.EXPECTED)
        self.generation = self.root / self.EXPECTED
        self.manifest(self.EXPECTED)
        self.assertEqual('PASS', c.corpus_generation_digest(self.run)['status'])

    def test_java_utf16_sort(self):
        self.nested.unlink()
        (self.generation / 'a.txt').unlink()
        # Java sorts the surrogate pair before U+E000; Python codepoint order
        # would reverse them. Expected hashes literal UTF-8 names in Java order.
        (self.generation / '\U00010000').write_bytes(b'S')
        (self.generation / '\ue000').write_bytes(b'P')
        self.manifest('99622cbf58ea02b5a2b2bcabc9446ab130d3ce63f159ceca15e00c158819368e')
        self.assertEqual('PASS', c.corpus_generation_digest(self.run)['status'])

    def test_java_symlinks(self):
        outside = self.root / 'external'
        outside.mkdir()
        target = outside / 'payload'
        self.nested.rename(target)
        self.nested.symlink_to(target)
        (self.generation / 'linked-dir').symlink_to(outside, target_is_directory=True)
        (self.generation / 'broken').symlink_to(outside / 'missing')
        # File link contributes the original B bytes; directory link is not
        # traversed and broken link is not a regular file, exactly as Java.
        self.assertEqual('PASS', c.corpus_generation_digest(self.run)['status'])


class ThorMissingCountsTests(unittest.TestCase):
    def setUp(self):
        self.passage = {'order': [{'id': 'T-ember', 'unblocks': 2, 'cone': 0}]}
        self.party = {'characters': [{'id': 'thor', 'top': [
            {'item': 'T-ember', 'reason': 'unblocks 2 item(s); cone of 0 behind it'}]}],
            'absences': []}

    def check(self):
        return c.thor_missing_counts(self.party, self.passage)

    def missing_cone(self):
        self.passage['order'][0]['cone'] = None
        self.party['characters'][0]['top'][0]['reason'] += ' (count missing)'
        self.party['absences'] = [{'what': 'cone for T-ember', 'reason': 'invented'}]

    def test_complete(self):
        self.assertEqual({'rows': 1, 'complete_counts': 2, 'missing_counts': 0},
                         self.check()['detail'])
        self.assertEqual('PASS', self.check()['status'])

    def test_missing_correct(self):
        self.missing_cone()
        self.assertEqual('PASS', self.check()['status'])
        self.assertEqual(1, self.check()['detail']['missing_counts'])
        del self.passage['order'][0]['cone']
        self.assertEqual('PASS', self.check()['status'])

    def test_bare_zero_missing(self):
        self.passage['order'][0]['cone'] = None
        self.assertEqual('FAIL', self.check()['status'])
        self.assertIn({'item': 'T-ember', 'field': 'cone',
                       'rule': 'rendered-count-clause-mismatch'}, self.check()['detail'])

    def test_measured_zero_marked_missing(self):
        self.party['characters'][0]['top'][0]['reason'] += ' (count missing)'
        self.assertEqual('FAIL', self.check()['status'])

    def test_missing_without_absence(self):
        self.missing_cone()
        self.party['absences'] = []
        self.assertEqual('FAIL', self.check()['status'])

    def test_duplicate_absence(self):
        self.missing_cone()
        self.party['absences'] *= 2
        self.assertEqual('FAIL', self.check()['status'])

    def test_spurious_absence(self):
        self.party['absences'] = [{'what': 'unblocks for M-outside'}]
        self.assertEqual('FAIL', self.check()['status'])

    def test_measured_count_absence(self):
        self.party['absences'] = [{'what': 'cone for T-ember'}]
        self.assertEqual('FAIL', self.check()['status'])

    def test_not_in_pass(self):
        self.passage['order'] = []
        self.assertEqual('FAIL', self.check()['status'])

    def test_no_evidence(self):
        self.assertEqual('ABSENT', c.thor_missing_counts(self.party, None)['status'])
        self.party['characters'][0]['top'] = []
        self.assertEqual('ABSENT', self.check()['status'])

    def test_wrong_measured_clause(self):
        self.party['characters'][0]['top'][0]['reason'] = 'unblocks 12 item(s); cone of 0 behind it'
        self.assertEqual('FAIL', self.check()['status'])


class ThyraReasonTests(unittest.TestCase):
    def setUp(self):
        self.pending = [{'id': 'T-ember', 'harm': 'H2', 'pending': 3},
                        {'id': 'M-flint', 'harm': 'H4', 'pending': 7}]
        self.passage = {'order': [{'id': 'T-ember', 'rank': 42}]}
        self.rows = [{'item': 'T-ember', 'reason': 'on harm line H2; Giraffe pass rank 42'},
                     {'item': 'M-flint', 'reason': 'on harm line H4; not in the Giraffe pass; pending level 7'}]
        self.party = {'characters': [{'id': 'thyra', 'top': self.rows}]}

    def check(self):
        return c.thyra_reasons_faithful(self.party, self.passage, self.pending)

    def test_correct(self):
        self.assertEqual('PASS', self.check()['status'])
        self.assertEqual({'rows': 2, 'ranked': 1, 'not_in_pass': 1}, self.check()['detail'])

    def test_position_not_rank(self):
        self.rows[0]['reason'] = 'on harm line H2; Giraffe pass rank 0'
        self.assertEqual('FAIL', self.check()['status'])

    def test_wrong_harm(self):
        self.rows[0]['reason'] = 'on harm line H9; Giraffe pass rank 42'
        self.assertEqual('FAIL', self.check()['status'])

    def test_ranked_claims_not_in_pass(self):
        self.rows[0]['reason'] = 'on harm line H2; not in the Giraffe pass; pending level 3'
        self.assertEqual('FAIL', self.check()['status'])

    def test_wrong_level(self):
        self.rows[1]['reason'] = 'on harm line H4; not in the Giraffe pass; pending level 8'
        self.assertEqual('FAIL', self.check()['status'])

    def test_extra_clause(self):
        self.rows[0]['reason'] += '; unfinished work within two hops'
        self.assertEqual('FAIL', self.check()['status'])

    def test_missing_pending_or_harm(self):
        self.pending.pop()
        self.assertEqual('FAIL', self.check()['status'])
        self.rows.pop()
        self.pending[0]['harm'] = ''
        self.assertEqual('FAIL', self.check()['status'])

    def test_absent_inputs(self):
        self.assertEqual('ABSENT', c.thyra_reasons_faithful(self.party, self.passage, None)['status'])
        self.assertEqual('ABSENT', c.thyra_reasons_faithful(self.party, None, self.pending)['status'])
        self.rows.clear()
        self.assertEqual('ABSENT', self.check()['status'])

    def test_null_rank_and_measured_zero(self):
        self.passage['order'].append({'id': 'M-flint', 'rank': None})
        self.assertEqual('PASS', self.check()['status'])
        self.passage['order'][0]['rank'] = 0
        self.rows[0]['reason'] = 'on harm line H2; Giraffe pass rank 0'
        self.assertEqual('PASS', self.check()['status'])


class QuestorReasonTests(unittest.TestCase):
    def setUp(self):
        # alpha and beta tie at 1; dormant has a known score of zero.
        self.lexicon = [{'entity': 'alpha', 'status': 'partly', 'relations': 'uses beta'},
                        {'entity': 'beta', 'status': 'partly', 'relations': ''},
                        {'entity': 'dormant', 'status': 'done', 'relations': ''}]
        self.pending = [{'id': 'T-ember', 'pending_label': c.READY,
                         'entities': ['dormant', 'beta', 'alpha']},
                        {'id': 'T-flint', 'pending_label': c.READY, 'entities': ['unknown']}]
        self.rows = [{'item': 'T-ember', 'reason': c.READY + '; names beta (convergence 1)'},
                     {'item': 'T-flint', 'reason': c.READY + '; names no scored feature'}]
        self.party = {'characters': [{'id': 'questor', 'top': self.rows}]}

    def check(self):
        return c.questor_reasons_faithful(self.party, self.pending, self.lexicon)

    def test_named_and_no_feature(self):
        self.assertEqual('PASS', self.check()['status'])
        self.assertEqual({'rows': 2, 'named': 1, 'no_feature': 1}, self.check()['detail'])

    def test_wrong_convergence(self):
        self.rows[0]['reason'] = c.READY + '; names beta (convergence 2)'
        self.assertEqual('FAIL', self.check()['status'])

    def test_nonbest(self):
        self.rows[0]['reason'] = c.READY + '; names dormant (convergence 0)'
        self.assertEqual('FAIL', self.check()['status'])

    def test_scored_claims_no_feature(self):
        self.rows[0]['reason'] = c.READY + '; names no scored feature'
        self.assertEqual('FAIL', self.check()['status'])

    def test_unscored_claims_feature(self):
        self.rows[1]['reason'] = c.READY + '; names unknown (convergence 0)'
        self.assertEqual('FAIL', self.check()['status'])

    def test_extra_clause(self):
        self.rows[0]['reason'] += '; extra'
        self.assertEqual('FAIL', self.check()['status'])

    def test_first_tie_wins(self):
        self.assertEqual('PASS', self.check()['status'])
        self.rows[0]['reason'] = c.READY + '; names alpha (convergence 1)'
        self.assertEqual('FAIL', self.check()['status'])
        self.pending[0]['entities'] = ['alpha', 'beta']
        self.assertEqual('PASS', self.check()['status'])

    def test_done_feature_with_zero_is_scored(self):
        self.pending[0]['entities'] = ['dormant']
        self.rows[0]['reason'] = c.READY + '; names dormant (convergence 0)'
        self.assertEqual('PASS', self.check()['status'])

    def test_missing_pending_entry(self):
        self.pending.pop()
        self.assertEqual('FAIL', self.check()['status'])

    def test_absent_sources_and_empty_top(self):
        self.assertEqual('ABSENT', c.questor_reasons_faithful(self.party, None, self.lexicon)['status'])
        self.assertEqual('ABSENT', c.questor_reasons_faithful(self.party, self.pending, None)['status'])
        self.rows.clear()
        self.assertEqual('ABSENT', self.check()['status'])

    def test_ready_clause_required(self):
        self.rows[0]['reason'] = 'nearly ready; names beta (convergence 1)'
        self.assertEqual('FAIL', self.check()['status'])


class MerlinReasonTests(unittest.TestCase):
    def setUp(self):
        self.lexicon = [{'entity': 'alpha', 'status': 'partly', 'relations': 'uses beta'},
                        {'entity': 'beta', 'status': 'hypothesised', 'relations': ''},
                        {'entity': 'finished', 'status': 'done', 'relations': 'uses alpha'},
                        {'entity': 'quiet', 'status': 'partly', 'relations': ''}]
        # finished has convergence 2 but must be skipped; beta and alpha tie at 1.
        self.pending = [{'id': 'M-alpha', 'pending_label': 'partly',
                         'entities': ['finished', 'beta', 'alpha']},
                        {'id': 'T-quiet', 'pending_label': 'partly', 'entities': ['quiet']}]
        self.rows = [{'item': 'M-alpha', 'reason':
                      'is alpha (convergence 1, unfinished work within two hops); names beta (convergence 1, halved)'},
                     {'item': 'T-quiet', 'reason':
                      'is quiet (convergence 0, unfinished work within two hops)'}]
        self.party = {'characters': [{'id': 'merlin', 'top': self.rows}]}

    def check(self):
        return c.merlin_reasons_faithful(self.party, self.pending, self.lexicon)

    def test_correct(self):
        self.assertEqual('PASS', self.check()['status'])
        self.assertEqual({'rows': 2, 'with_named': 1, 'without_named': 1}, self.check()['detail'])

    def test_wrong_own(self):
        self.rows[0]['reason'] = self.rows[0]['reason'].replace('convergence 1, unfinished', 'convergence 2, unfinished')
        self.assertEqual('FAIL', self.check()['status'])

    def test_wrong_feature(self):
        self.rows[0]['reason'] = self.rows[0]['reason'].replace('is alpha', 'is beta')
        self.assertEqual('FAIL', self.check()['status'])

    def test_finished_named(self):
        self.rows[0]['reason'] = self.rows[0]['reason'].replace('names beta (convergence 1', 'names finished (convergence 2')
        self.assertEqual('FAIL', self.check()['status'])

    def test_zero_named_clause(self):
        self.rows[1]['reason'] += '; names quiet (convergence 0, halved)'
        self.assertEqual('FAIL', self.check()['status'])

    def test_required_named_missing(self):
        self.rows[0]['reason'] = self.rows[0]['reason'].split(';')[0]
        self.assertEqual('FAIL', self.check()['status'])

    def test_extra_clause(self):
        self.rows[0]['reason'] += '; extra'
        self.assertEqual('FAIL', self.check()['status'])

    def test_first_tie_wins(self):
        self.rows[0]['reason'] = self.rows[0]['reason'].replace('names beta', 'names alpha')
        self.assertEqual('FAIL', self.check()['status'])
        self.pending[0]['entities'] = ['alpha', 'beta']
        self.assertEqual('PASS', self.check()['status'])

    def test_excluded_rows(self):
        self.pending[0]['pending_label'] = 'may be no longer wanted'
        self.assertEqual('FAIL', self.check()['status'])
        self.pending[0]['pending_label'] = 'partly'
        self.pending[0]['id'] = self.rows[0]['item'] = 'M-absent'
        self.assertEqual('FAIL', self.check()['status'])

    def test_absent(self):
        self.assertEqual('ABSENT', c.merlin_reasons_faithful(self.party, self.pending, None)['status'])
        self.assertEqual('ABSENT', c.merlin_reasons_faithful(self.party, None, self.lexicon)['status'])
        self.rows.clear()
        self.assertEqual('ABSENT', self.check()['status'])

    def test_full_named_count_not_half(self):
        self.rows[0]['reason'] = self.rows[0]['reason'].replace('convergence 1, halved', 'convergence 0.5, halved')
        self.assertEqual('FAIL', self.check()['status'])


class ReadingSourceTests(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.addCleanup(self.temp.cleanup)
        self.root = Path(self.temp.name)
        self.run = self.root / 'run'
        self.run.mkdir()
        self.generation = self.root / 'generation'
        self.generation.mkdir()
        (self.run / 'manifest.edn').write_text(
            '{:readings-generation {:path ' + json.dumps(str(self.generation)) + '}}')
        (self.generation / 'members.json').write_text('[]')
        self.scope()
        self.doc = self.root / 'invented.md'
        self.doc.write_bytes(b'invented source\r\n')
        self.old = hashlib.sha256(self.doc.read_bytes()).hexdigest()
        self.record = {'id': 'T-invented', 'source': {'path': str(self.doc), 'sha256': self.old}}
        self.batch = self.generation / 'batch-toy.jsonl'
        self.write_record()
        self.queue = self.root / 'requests.sqlite'
        # reread_queue.clj:40-46 plus migrations :48-59, no live store access.
        with sqlite3.connect(self.queue) as db:
            db.execute("""CREATE TABLE reread_requests (
                target_id TEXT NOT NULL, source_sha TEXT NOT NULL, old_sha TEXT,
                source_path TEXT NOT NULL, state TEXT NOT NULL,
                requested_at_ms INTEGER NOT NULL, updated_at_ms INTEGER NOT NULL,
                lease_until_ms INTEGER, claimed_by TEXT, superseded_by_sha TEXT,
                superseded_reason TEXT, attempts INTEGER NOT NULL DEFAULT 0,
                last_error TEXT, landed_digest TEXT, last_answer TEXT,
                PRIMARY KEY(target_id, source_sha))""")

    def test_retired_reported_without_changing_verdict(self):
        (self.generation / 'members.json').write_text(json.dumps([
            {'id': 'M-retired', 'retired': {'reason': 'source-missing', 'at_ms': 1,
                                          'last_sha256': '0' * 64}}]))
        (self.generation / 'retired.jsonl').write_text('{"id":"M-retired"}\n')
        row = self.check()
        self.assertEqual('PASS', row['status'])
        self.assertEqual(1, row['detail']['retired'])
        self.assertEqual(['M-retired'], row['detail']['retired_ids'])
        self.assertEqual(1, row['detail']['current'])
        self.assertEqual('PASS', c.one_current_reading(self.run)['status'])
        digest = hashlib.sha256()
        for path in sorted(self.generation.rglob('*')):
            if path.is_file():
                digest.update(str(path.relative_to(self.generation)).encode())
                digest.update(b'\0')
                digest.update(path.read_bytes())
        (self.run / 'manifest.edn').write_text(
            '{:readings-generation {:path ' + json.dumps(str(self.generation)) +
            ' :digest "' + digest.hexdigest() + '"}}')
        self.assertEqual('PASS', c.corpus_generation_digest(self.run)['status'])
        self.change()
        self.assertEqual('FAIL', self.check()['status'])
        self.batch.write_text('')
        row = self.check()
        self.assertEqual('ABSENT', row['status'])
        self.assertEqual(1, row['detail']['retired'])
        self.batch.write_text('{"id":"M-retired"}\n')
        row = self.check()
        self.assertEqual('ABSENT', row['status'])
        self.assertEqual(0, row['detail']['current'])
        self.assertEqual(0, row['detail']['in_view'])

    def scope(self, inactive=()):
        (self.run / 'input.edn').write_text(
            '{:graph-coverage {:drift {:members-not-open ' + json.dumps(list(inactive)) + '}}}')

    def test_inactive_changed_not_verified(self):
        self.change()
        self.assertEqual('FAIL', self.check()['status'])
        self.scope(['T-invented'])
        row = self.check()
        self.assertEqual('ABSENT', row['status'])
        self.assertEqual(1, row['detail']['inactive_changed'])
        self.assertEqual(0, row['detail']['in_view'])
        self.assertEqual(['T-invented'], row['detail']['inactive_ids']['changed'])

    def test_inactive_missing_not_verified(self):
        self.doc.unlink()
        self.assertEqual('FAIL', self.check()['status'])
        self.scope(['T-invented'])
        row = self.check()
        self.assertEqual('ABSENT', row['status'])
        self.assertEqual(1, row['detail']['inactive_missing'])
        self.assertEqual(0, row['detail']['source_missing'])

    def test_inactive_current_not_verified(self):
        self.scope(['T-invented'])
        row = self.check()
        self.assertEqual('ABSENT', row['status'])
        self.assertEqual(1, row['detail']['inactive_current'])
        self.assertEqual(0, row['detail']['current'])

    def test_inactive_invalid_pin_is_absent_not_failure(self):
        self.scope(['T-invented'])
        del self.record['source']
        self.write_record()
        self.assertEqual('ABSENT', self.check()['status'])

    def test_scope_required(self):
        path = self.run / 'input.edn'
        path.unlink()
        self.assertEqual('ABSENT', self.check()['status'])
        for text in ['{', '{}', '{:graph-coverage {:drift {}}}',
                     '{:graph-coverage {:drift {:members-not-open nil}}}']:
            path.write_text(text)
            self.assertEqual('ABSENT', self.check()['status'])

    def test_current_plus_inactive_changed(self):
        extra = copy.deepcopy(self.record)
        extra['id'] = 'T-inactive'
        extra['source']['sha256'] = '0' * 64
        self.batch.write_text(json.dumps(self.record) + '\n' + json.dumps(extra) + '\n')
        self.scope(['T-inactive'])
        self.queue.unlink()
        row = self.check()
        self.assertEqual('PASS', row['status'])
        self.assertEqual(1, row['detail']['current'])
        self.assertEqual(1, row['detail']['in_view'])
        self.assertEqual(1, row['detail']['inactive_changed'])

    def write_record(self):
        self.batch.write_text(json.dumps(self.record) + '\n')

    def request(self, sha, state='pending'):
        with sqlite3.connect(self.queue) as db:
            db.execute('INSERT INTO reread_requests '
                       '(target_id,source_sha,source_path,state,requested_at_ms,updated_at_ms) '
                       'VALUES (?,?,?,?,0,0)', ('T-invented', sha, str(self.doc), state))

    def check(self):
        return c.reading_source_current(self.run, self.queue)

    def change(self):
        self.doc.write_bytes(b'changed source\n')
        return hashlib.sha256(self.doc.read_bytes()).hexdigest()

    def test_current_without_queue(self):
        self.queue.unlink()
        row = self.check()
        self.assertEqual('PASS', row['status'])
        self.assertEqual(1, row['detail']['current'])

    def test_changed_pending_and_claimed(self):
        sha = self.change()
        self.request(sha)
        before = self.queue.read_bytes()
        row = self.check()
        self.assertEqual('PASS', row['status'])
        self.assertEqual(1, row['detail']['pending'])
        self.assertEqual(before, self.queue.read_bytes())
        with sqlite3.connect(self.queue) as db:
            db.execute("UPDATE reread_requests SET state='claimed'")
        self.assertEqual('PASS', self.check()['status'])

    def test_changed_unqueued(self):
        self.change()
        row = self.check()
        self.assertEqual('FAIL', row['status'])
        self.assertEqual([{'item': 'T-invented', 'rule': 'source-changed'}], row['detail']['violations'])

    def test_old_sha_does_not_cover_change(self):
        self.change()
        self.request(self.old)
        self.assertEqual('FAIL', self.check()['status'])

    def test_settled_does_not_cover_change(self):
        sha = self.change()
        self.request(sha, 'landed')
        for state in ['landed', 'failed', 'superseded']:
            with sqlite3.connect(self.queue) as db:
                db.execute('UPDATE reread_requests SET state=?', (state,))
            self.assertEqual('FAIL', self.check()['status'])

    def test_deleted_requires_pending(self):
        self.doc.unlink()
        self.assertEqual('FAIL', self.check()['status'])
        self.request(self.old)
        row = self.check()
        self.assertEqual('PASS', row['status'])
        self.assertEqual(1, row['detail']['source_missing'])
        self.assertEqual(1, row['detail']['pending'])

    def test_no_pin(self):
        del self.record['source']
        self.write_record()
        self.assertEqual('no-source-pin', self.check()['detail']['violations'][0]['rule'])
        self.assertEqual('FAIL', self.check()['status'])

    def test_missing_generation(self):
        self.assertEqual('ABSENT', c.reading_source_current(None, self.queue)['status'])

    def test_empty_generation(self):
        self.batch.write_text('')
        self.assertEqual('ABSENT', self.check()['status'])

    def test_missing_queue_when_stale(self):
        self.change()
        self.queue.unlink()
        self.assertEqual('ABSENT', self.check()['status'])
        self.assertFalse(self.queue.exists())


class JsonReportTests(unittest.TestCase):
    def test_atomic_report_and_exit(self):
        from unittest.mock import patch
        import contextlib
        import io
        with tempfile.TemporaryDirectory() as root:
            root = Path(root)
            party = root / 'party.json'
            report = root / 'conformance.json'
            party.write_text(json.dumps({'generated_at': 'invented-time',
                                        'sources': {'giraffe_run': 'toy-run'}}))
            report.write_text('old report')
            result = [{'check': 'toy-check', 'theorem': 'toy', 'status': 'FAIL', 'detail': {}}]
            replace = c.os.replace
            observed = []
            def check_replace(src, dst):
                self.assertEqual('old report', report.read_text())
                self.assertEqual(report.parent, Path(src).parent)
                self.assertEqual(['toy-check'], json.loads(Path(src).read_text())['failing'])
                observed.append(True)
                replace(src, dst)
            with patch.object(c, 'audit', return_value=result), patch.object(c.os, 'replace', check_replace):
                with contextlib.redirect_stdout(io.StringIO()) as output:
                    code = c.main([str(party), '--run', str(root / 'toy-run'), '--json-out', str(report)])
            self.assertEqual(1, code)
            self.assertTrue(observed)
            self.assertIn('toy-check', output.getvalue())
            data = json.loads(report.read_text())
            self.assertEqual('invented-time', data['generated_at'])
            self.assertEqual('toy-run', data['run'])
            self.assertEqual({'PASS': 0, 'FAIL': 1, 'ABSENT': 0}, data['summary'])
            self.assertEqual(result, data['results'])
            self.assertGreater(data['checked_at_ms'], 0)


if __name__ == '__main__':
    unittest.main()
