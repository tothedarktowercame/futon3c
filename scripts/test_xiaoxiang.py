"""Tests for xiaoxiang.py (小象 v0.1).  Run: python3 -m unittest scripts/test_xiaoxiang.py"""
import json
import os
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import xiaoxiang as xx  # noqa: E402


def write_turn(directory, turn, fragments, labeller="test"):
    doc = {"labeller": labeller,
           "sentences": [{"fragments": [{"text": t, "intent": i} for t, i in fragments]}]}
    with open(os.path.join(directory, f"{turn}.json.analysis.json"), "w") as fh:
        json.dump(doc, fh)


class Tokens(unittest.TestCase):
    def test_words_bigrams_cjk_no_digits(self):
        toks = xx.tokens("OK, let's go 42 象限")
        self.assertIn("ok", toks)
        self.assertIn("let's go", toks)
        self.assertIn("象", toks)
        self.assertIn("象限", toks)
        self.assertFalse(any(ch.isdigit() for t in toks for ch in t))


class Evaluation(unittest.TestCase):
    def test_folds_hold_out_whole_turns(self):
        rows = [{"turn": f"t{i}", "text": "x", "intent": "a"} for i in range(50)]
        for k in (3, 5):
            folds = {r["turn"]: xx.fold_of(r["turn"], k) for r in rows}
            self.assertEqual(folds, {r["turn"]: xx.fold_of(r["turn"], k) for r in rows})

    def test_learns_a_separable_toy_set(self):
        with tempfile.TemporaryDirectory() as d:
            for i in range(20):
                write_turn(d, f"turn-{i}", [("yes please go ahead", "approve"),
                                            ("no that is wrong", "disagree")])
            result = xx.evaluate(xx.load(d))
        self.assertEqual(1.0, result["accuracy"])
        self.assertEqual(2, result["intents"])


class Export(unittest.TestCase):
    def test_vocabulary_drops_rare_idlike_and_secret_tokens(self):
        rows = []
        for i in range(5):
            rows.append({"turn": f"t{i}", "text": "please continue", "intent": "continue"})
        rows.append({"turn": "t0", "text": "butzz", "intent": "report"})     # one turn only
        for i in range(5):
            rows.append({"turn": f"t{i}", "text": "deadbeefcafe0123", "intent": "report"})
            rows.append({"turn": f"t{i}", "text": "AKIAABCDEFGHIJKLMNOP", "intent": "report"})
        keep = xx.safe_vocab(rows)
        self.assertIn("please continue", keep)
        self.assertNotIn("butzz", keep)
        self.assertFalse(any("deadbeef" in t or "akia" in t for t in keep))

    def test_exported_model_classifies_like_the_trained_one(self):
        rows = [{"turn": f"t{i}", "text": t, "intent": c}
                for i in range(5) for t, c in [("yes go ahead", "approve"),
                                               ("that is wrong", "disagree")]]
        keep = xx.safe_vocab(rows)
        model = json.loads(json.dumps(xx.export(rows, keep)))
        nb = xx.NaiveBayes().fit(rows, keep=keep)
        for text in ("yes go", "wrong", "ahead that"):
            self.assertEqual(nb.predict(text), xx.classify(model, text)[0][0])

    def test_collisions_report_same_text_under_two_intents(self):
        rows = [{"turn": f"t{i}", "text": "ok", "intent": "approve"} for i in range(4)]
        rows.append({"turn": "t9", "text": "OK.", "intent": "continue"})
        found = xx.collisions(rows, {"ok"})
        self.assertEqual([{"text": "ok", "intents": {"approve": 4, "continue": 1}}], found)


if __name__ == "__main__":
    unittest.main()
