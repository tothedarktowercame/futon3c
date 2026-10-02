"""Tests for xiaoxiang_preview.py's proforma-mark honouring, and for the
agreement between Python's REPLY_KEY and Clojure's proforma-marks (two
copies of one table).  Run: python3 -m unittest scripts/test_xiaoxiang_preview.py"""
import os
import re
import sys
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import xiaoxiang as xx  # noqa: E402
import xiaoxiang_preview as xp  # noqa: E402

REPO = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
TURN_RECORD = os.path.join(REPO, "src", "futon3c", "xiang", "turn_record.clj")


def toy_model():
    rows = [{"turn": f"t{i}", "text": t, "intent": c}
            for i in range(10)
            for t, c in (("yes please go ahead", "approve"),
                         ("please run the tests now", "ask-action"),
                         ("here is the status report", "report"))]
    model = xx.export(rows, keep=None)
    model.setdefault("precision", {})
    return model


MODEL = toy_model()


class DeclaredMarks(unittest.TestCase):
    def test_marked_paragraph_declares_intent(self):
        out = xp.preview("🈸 yes please do that", MODEL)
        self.assertEqual(1, len(out))
        frag = out[0]
        self.assertEqual("ask-action", frag["intent"])
        self.assertEqual("declared", frag["basis"])
        self.assertEqual("🈸", frag["mark"])
        self.assertEqual(1.0, frag["precision"])
        self.assertEqual(2, len(frag["guesses"]))  # model guesses kept

    def test_unmarked_text_is_model_basis(self):
        out = xp.preview("here is the status report", MODEL)
        self.assertTrue(out)
        for frag in out:
            self.assertEqual("model", frag["basis"])
            self.assertNotIn("mark", frag)

    def test_only_the_marked_paragraph_is_declared(self):
        text = "here is the status report\n\n🈸 please run the tests now"
        out = xp.preview(text, MODEL)
        bases = [f["basis"] for f in out]
        for frag, basis in zip(out, bases):
            if frag["start"] < text.index("\n\n"):
                self.assertEqual("model", basis, frag)
                self.assertNotIn("mark", frag)
            else:
                self.assertEqual("declared", basis, frag)
                self.assertEqual("ask-action", frag["intent"])
                self.assertEqual("🈸", frag["mark"])
                self.assertEqual(1.0, frag["precision"])


class TableAgreement(unittest.TestCase):
    def test_reply_key_matches_proforma_marks(self):
        with open(TURN_RECORD, encoding="utf-8") as fh:
            src = fh.read()
        m = re.search(r"\(def proforma-marks.*?\]\)\)", src, re.S)
        self.assertIsNotNone(m, "proforma-marks not found in turn_record.clj")
        clj = dict(re.findall(r'\["([^"]+)"\s+"([^"]+)"\s+"[^"]+"\]', m.group(0)))
        self.assertEqual(xx.REPLY_KEY, clj)


if __name__ == "__main__":
    unittest.main()
