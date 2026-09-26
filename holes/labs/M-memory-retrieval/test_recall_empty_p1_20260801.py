#!/usr/bin/env python3

import importlib.util
import tempfile
import unittest
from pathlib import Path

MODULE_PATH = Path(__file__).with_name("recall_empty_p1_20260801.py")
SPEC = importlib.util.spec_from_file_location("recall_empty_p1_20260801", MODULE_PATH)
MODULE = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
SPEC.loader.exec_module(MODULE)


class RecallEmptyP1Test(unittest.TestCase):
    def test_match_string_quotes_each_token(self):
        self.assertEqual(MODULE.fts_match_string('alpha b"eta'), '"alpha" "b""eta"')

    def test_document_frequencies_use_read_only_copy(self):
        with tempfile.TemporaryDirectory() as directory:
            db = Path(directory) / "copy.db"
            import sqlite3

            connection = sqlite3.connect(db)
            connection.execute("CREATE VIRTUAL TABLE ev_fts USING fts5(body)")
            connection.executemany(
                "INSERT INTO ev_fts(body) VALUES (?)",
                [("rare common",), ("common",)],
            )
            connection.commit()
            connection.close()
            frequencies, indexed = MODULE.document_frequencies(db, ["rare", "common"])
            self.assertEqual(indexed, 2)
            self.assertEqual(frequencies, {"common": 2, "rare": 1})

    def test_permutation_p_has_plus_one_correction(self):
        self.assertEqual(MODULE.permutation_p(2.0, [0.0, 1.0, 2.0]), 0.5)


if __name__ == "__main__":
    unittest.main()
