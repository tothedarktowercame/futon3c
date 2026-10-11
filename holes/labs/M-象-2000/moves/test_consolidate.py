#!/usr/bin/env python3
import importlib.util
from pathlib import Path
import unittest


SPEC = importlib.util.spec_from_file_location("consolidate", Path(__file__).with_name("consolidate.py"))
MODULE = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(MODULE)
consolidate = MODULE.consolidate
content_words = MODULE.content_words
normalize_label = MODULE.normalize_label


def act(move, quote):
    return {"kind": move.split("/", 1)[0], "move": move, "quote": quote}


class ConsolidateTest(unittest.TestCase):
    def test_normalise_inflections_and_punctuation(self):
        self.assertEqual("report/test-case", normalize_label("Report/Testing Cases"))
        acts = [act("Report/Testing Cases", quote) for quote in ("alpha", "beta", "gamma")]
        _, label_map, _, _ = consolidate(acts)
        self.assertEqual(
            {"label": "report/test-case", "reason": "normalised"},
            label_map["labels"]["Report/Testing Cases"],
        )

    def test_fold_by_shared_content_words_to_most_frequent(self):
        acts = [
            act("report/stable", "kernel session transition observed"),
            act("report/stable", "session transition reaches kernel"),
            act("report/stable", "kernel records the session transition"),
            act("report/rare", "session transition was delayed"),
        ]
        _, label_map, _, _ = consolidate(acts)
        self.assertEqual("report/stable", label_map["labels"]["report/rare"]["label"])
        self.assertTrue(label_map["labels"]["report/rare"]["reason"].startswith("folded:"))

    def test_fold_without_overlap_goes_to_other(self):
        acts = [
            act("explain/common", "registry session transition"),
            act("explain/common", "session transition registry"),
            act("explain/common", "transition enters registry"),
            act("explain/rare", "purple telescope"),
        ]
        _, label_map, _, _ = consolidate(acts)
        self.assertEqual("explain/other", label_map["labels"]["explain/rare"]["label"])
        self.assertEqual("other", label_map["labels"]["explain/rare"]["reason"])

    def test_identifier_and_generic_tokens_cannot_witness_fold(self):
        acts = [
            act("report/stable", "futon7a claude-4 check plan"),
            act("report/stable", "futon7a claude-4 check plan"),
            act("report/stable", "futon7a claude-4 check plan"),
            act("report/rare", "futon7a claude-4 check plan"),
        ]
        self.assertEqual(
            set(),
            content_words("`path/file_2` futon futon7a claude-4 象-pattern check plan step"),
        )
        _, label_map, _, _ = consolidate(acts)
        self.assertEqual("report/other", label_map["labels"]["report/rare"]["label"])

    def test_label_stem_precedes_quote_overlap(self):
        acts = [
            act("propose/design-proposal", "alpha beta"),
            act("propose/design-proposal", "gamma delta"),
            act("propose/design-proposal", "epsilon zeta"),
            act("propose/design", "unrelated words"),
        ]
        _, label_map, _, _ = consolidate(acts)
        self.assertEqual(
            {"label": "propose/design-proposal", "reason": "label:design"},
            label_map["labels"]["propose/design"],
        )

    def test_label_with_three_uses_is_kept(self):
        acts = [act("approve/plain", quote) for quote in ("alpha", "beta", "gamma")]
        vocabulary, label_map, _, _ = consolidate(acts)
        self.assertEqual("approve/plain", label_map["labels"]["approve/plain"]["label"])
        self.assertEqual("kept", label_map["labels"]["approve/plain"]["reason"])
        self.assertEqual(3, vocabulary["kinds"]["approve"]["labels"][0]["count"])


if __name__ == "__main__":
    unittest.main()
