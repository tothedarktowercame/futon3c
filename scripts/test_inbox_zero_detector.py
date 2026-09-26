import importlib.util
from pathlib import Path
import tempfile
import unittest

spec = importlib.util.spec_from_file_location('detector', Path(__file__).with_name('inbox_zero_detector.py'))
module = importlib.util.module_from_spec(spec)
spec.loader.exec_module(module)


class DetectorTest(unittest.TestCase):
    def test_edit_then_restore_and_new_directory_are_detected(self):
        with tempfile.TemporaryDirectory() as root:
            path = Path(root, 'note.md')
            path.write_text('before')
            detector = module.Detector(root)
            try:
                self.assertEqual(0, detector.drain()['event_count'])
                path.write_text('raced')
                path.write_text('before')
                event = detector.drain()
                self.assertTrue(event['complete'])
                self.assertGreater(event['event_count'], 0)
                self.assertIn('note.md', [e['path'] for e in event['events']])
                Path(root, 'new').mkdir()
                Path(root, 'new', 'unseen.md').write_text('new')
                self.assertGreater(detector.drain()['event_count'], 0)
            finally:
                detector.close()

    def test_git_metadata_does_not_look_like_a_worktree_edit(self):
        with tempfile.TemporaryDirectory() as root:
            Path(root, '.git').mkdir()
            detector = module.Detector(root)
            try:
                Path(root, '.git', 'index.lock').write_text('git-owned')
                self.assertEqual(0, detector.drain()['event_count'])
            finally:
                detector.close()


if __name__ == '__main__':
    unittest.main()
