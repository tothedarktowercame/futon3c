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
        results = c.audit(*fixture())
        self.assertEqual({'PASS'}, {r['status'] for r in results[:-3]})
        self.assertEqual('ABSENT', results[-1]['status'])

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
        self.assertEqual(9, sum(r['status'] == 'ABSENT' for r in results))
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
        self.assertEqual(12, len(rows))
        self.assertEqual({'PASS'}, {r['status'] for r in rows[:-2]})
        self.assertEqual('ABSENT', rows[-1]['status'])

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


if __name__ == '__main__':
    unittest.main()
