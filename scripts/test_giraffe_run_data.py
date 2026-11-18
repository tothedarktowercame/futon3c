"""Invented CAS fixtures; sqlite is authoritative and bytes keep their digest."""
import hashlib
import json
from pathlib import Path
import sqlite3
import tempfile
import unittest
from giraffe_run_data import load_run_file, read_object_bytes


class ObjectReader(unittest.TestCase):
    def test_database_and_legacy_and_corruption(self):
        with tempfile.TemporaryDirectory() as tmp:
            root = Path(tmp)
            raw = b'{"invented":1}'
            sha = hashlib.sha256(raw).hexdigest()
            obj = root / 'objects' / sha[:2] / (sha + '.edn')
            obj.parent.mkdir(parents=True)
            obj.write_bytes(raw)
            run = root / 'run' / 'input.edn'
            run.parent.mkdir()
            run.write_text(json.dumps({'schema': ['giraffe/run-index', 1],
                                       'content': {'giraffe/object-ref': sha}}))
            self.assertEqual({'invented': 1}, load_run_file(run, json.loads))
            with sqlite3.connect(root / 'objects.sqlite') as conn:
                conn.execute('CREATE TABLE objects (sha TEXT PRIMARY KEY, body BLOB)')
                # A file must not hide a missing row.
                with self.assertRaises(FileNotFoundError):
                    read_object_bytes(root, sha)
                conn.execute('INSERT INTO objects VALUES (?,?)', (sha, raw))
                conn.commit()
                self.assertEqual({'invented': 1}, load_run_file(run, json.loads))
                conn.execute('UPDATE objects SET body=?', (b'{}',))
                conn.commit()
                with self.assertRaisesRegex(ValueError, 'digest mismatch'):
                    load_run_file(run, json.loads)
