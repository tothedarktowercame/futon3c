"""Tests for xiaoxiang.py (小象 v0.1).  Run: python3 -m unittest scripts/test_xiaoxiang.py"""
import json
import os
import sys
import tempfile
import unittest
import unittest.mock

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


class FailClosed(unittest.TestCase):
    def test_no_scanner_means_no_vocabulary(self):
        real_import = __import__

        def no_secret_scan(name, *args, **kwargs):
            if name == "secret_scan":
                raise ImportError("absent")
            return real_import(name, *args, **kwargs)

        rows = [{"turn": f"t{i}", "text": "please continue", "intent": "continue"}
                for i in range(4)]
        saved = sys.modules.pop("secret_scan", None)
        try:
            with unittest.mock.patch("builtins.__import__", no_secret_scan):
                with self.assertRaises(RuntimeError):
                    xx.safe_vocab(rows)
        finally:
            if saved is not None:
                sys.modules["secret_scan"] = saved


class V02(unittest.TestCase):
    def test_capitalised_names_are_dropped_and_i_forms_kept(self):
        rows = [{"turn": f"t{i}", "text": f"Then Rob said yes and I'm glad {w}",
                 "intent": "report"} for i, w in enumerate(["x", "y", "z", "w"])]
        names = xx.proper_nouns(rows)
        self.assertIn("rob", names)
        self.assertNotIn("i'm", names)
        keep = xx.safe_vocab(rows, exclude=set())
        self.assertFalse(any("rob" in t.split() for t in keep))
        self.assertIn("said yes", keep)

    def test_excluded_words_and_possessives_are_dropped(self):
        rows = [{"turn": f"t{i}", "text": "ask joe and joe's team", "intent": "report"}
                for i in range(4)]
        keep = xx.safe_vocab(rows, exclude={"joe"})
        self.assertFalse(any(w in ("joe", "joe's") for t in keep for w in t.split()))

    def test_seed_cue_decides_a_rare_rejection(self):
        rows = [{"turn": f"t{i}", "text": t, "intent": c}
                for i in range(6) for t, c in [("your plan works for me", "approve"),
                                               ("i think we should add tests", "propose")]]
        self.assertNotEqual("disagree", xx.NaiveBayes().fit(rows + [
            {"turn": "t9", "text": "x", "intent": "disagree"}], seed=False)
            .predict("I reject your claim"))
        self.assertEqual("disagree", xx.NaiveBayes().fit(rows + [
            {"turn": "t9", "text": "x", "intent": "disagree"}]).predict("I reject your claim"))

    def test_only_common_words_is_no_evidence(self):
        rows = [{"turn": f"t{i}", "text": f"the {w}", "intent": "report"}
                for i, w in enumerate(["cat", "dog", "fish", "bird", "cow"] * 5)]
        model = json.loads(json.dumps(xx.export(rows, xx.safe_vocab(rows, exclude=set()))))
        self.assertEqual([], xx.evidence(model, "the the"))
        self.assertTrue(xx.evidence(model, "the reject"))


class Figures(unittest.TestCase):
    def test_sample_report_is_drawn_without_paths(self):
        report = {"turns": 3, "agent_tokens": 100, "first_turn": 0, "last_turn": 86400 * 2,
                  "gap_hours": 6, "gaps": [], "too_little_to_go_on": 0,
                  "intents": {"propose": 2, "approve": 1},
                  "files_with_secrets": {"/home/someone/.codex/x.jsonl": 3},
                  "gap_views": {"6": [{"start": 0, "end": 30000, "hours": 8.3, "tokens": 60}],
                                "0": [{"start": 0, "end": 30000, "hours": 8.3, "tokens": 60},
                                      {"start": 40000, "end": 50000, "hours": 2.8, "tokens": 30}]}}
        html = xx.figures(report)
        self.assertEqual(3, html.count("<rect"))
        self.assertIn("--gap-hours 0", html)
        self.assertIn("propose", html)
        self.assertNotIn("/home/someone", html)
        self.assertEqual("", xx.figures(None))


if __name__ == "__main__":
    unittest.main()
