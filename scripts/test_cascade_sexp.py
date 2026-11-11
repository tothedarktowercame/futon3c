"""Tests for cascade_sexp.py on the recorded lab cascade of 2026-11-11.

The fixture is kimi-2's mapping of a real turn (futon3c
holes/labs/M-repl-circus/cascade-2026-11-11-turn, e95aaa086).  The join is
checked with Emacs's own reader: both ends of the glue must be one object.
"""
import json
import os
import shutil
import subprocess
import tempfile
import unittest

import cascade_sexp

HERE = os.path.dirname(os.path.abspath(__file__))
LAB = os.path.join(HERE, "..", "holes", "labs", "M-repl-circus", "cascade-2026-11-11-turn")
GLUE = "voxterm-diagram-tab-start-incomplete"
RWEF = "cascade-construction/read-what-exists-first"
FMC = "forward-model/the-forward-model-is-a-pattern-cascade"

# Read the file, find the two occurrences, and compare the element under
# RWEF's BECAUSE with the one under FMC's HOWEVER.
ELISP = r"""
(let* ((c (with-temp-buffer (insert-file-contents (car command-line-args-left)) (read (current-buffer))))
       (occ (lambda (p) (seq-find (lambda (x) (and (eq (car-safe x) 'occurrence) (eq (nth 1 x) (intern p)))) c)))
       (part (lambda (o name) (nth 2 (assq name (cddr o)))))
       (a (funcall part (funcall occ "%s") 'BECAUSE))
       (b (funcall part (funcall occ "%s") 'HOWEVER)))
  (princ (format "%%s %%s %%s" (eq a b) (nth 1 a) (length (seq-filter (lambda (x) (eq (car-safe x) 'partial)) c)))))
""" % (RWEF, FMC)


def load():
    with open(os.path.join(LAB, "occurrences.json"), encoding="utf-8") as f:
        occ = json.load(f)
    with open(os.path.join(LAB, "frame.json"), encoding="utf-8") as f:
        frame = json.load(f)
    return occ, frame[0] if isinstance(frame, list) else frame


def emacs_read(text):
    with tempfile.NamedTemporaryFile("w", suffix=".sexp", delete=False, encoding="utf-8") as f:
        f.write(text)
        path = f.name
    try:
        out = subprocess.run(["emacs", "--batch", "--eval", ELISP, path],
                             capture_output=True, text=True, timeout=60)
        return out.stdout.strip(), out.stderr
    finally:
        os.unlink(path)


@unittest.skipUnless(shutil.which("emacs"), "emacs not installed")
class CascadeSexpTest(unittest.TestCase):
    def test_glue_is_one_object_in_emacs(self):
        occ, frame = load()
        text = cascade_sexp.render(occ, frame)
        out, err = emacs_read(text)
        self.assertEqual(out, "t %s 12" % GLUE, err)

    def test_counts_and_glue_line(self):
        occ, frame = load()
        text = cascade_sexp.render(occ, frame)
        self.assertIn("(reading :candidates 26 :complete 2 :partial 12 :void 12)", text)
        self.assertEqual(text.count("(occurrence "), 2)
        self.assertEqual(text.count("=(element %s" % GLUE), 1)   # named once
        self.assertRegex(text, r"\(#\d+# \(%s BECAUSE\) \(%s HOWEVER\)\)" % (RWEF, FMC))

    def test_unshared_element_has_no_label(self):
        occ, frame = load()
        text = cascade_sexp.render(occ, frame)
        self.assertIn("\n        (element legal-cascade-per-turn-criterion", text)


if __name__ == "__main__":
    unittest.main()
