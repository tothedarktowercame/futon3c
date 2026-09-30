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


class Segment(unittest.TestCase):
    """The classical segmenter: ids, offsets, protections, merging."""

    def test_offsets_exact_on_several_texts(self):
        texts = [
            "So, yes, but let's continue to build these features into xiaoxiang b/c I like the idea of releasing it as open source software; and in order to make it good, let's get back to the programme of running the backfill on old operator turns, so we can use that as a training set.",
            "First sentence. Second one; however, it continues here — and so does that. A third.",
            "We did A. Then B, because C matters, although D is unclear. e.g. this stays with its comma-side clause, but this is a new fragment.",
            "One clause only here",
        ]
        for text in texts:
            frags = xx.segment(text)
            self.assertTrue(frags)
            at = -1
            for f in frags:
                self.assertEqual(text[f["start"]:f["end"]], f["text"])
                self.assertGreaterEqual(f["start"], at)
                at = f["end"]

    def test_sentence_ids_match_turn_batch(self):
        import turn_batch
        for text in ["One. Two! Three? Four.",
                     "No punctuation at all in this one",
                     "A; b, c — d. And then e; f.",
                     ""]:
            theirs = [s["id"] for s in turn_batch.sentences_of(text)]
            mine = sorted({f["id"].split(".")[0] for f in xx.segment(text)},
                          key=lambda x: int(x[1:]))
            if text.strip():
                self.assertEqual(theirs, mine, text)
            else:
                self.assertEqual([], xx.segment(text))

    def test_empty_and_tiny(self):
        self.assertEqual([], xx.segment(""))
        self.assertEqual([], xx.segment("   "))
        one = xx.segment("word")
        self.assertEqual(1, len(one))
        self.assertEqual([{"id": "s1.0", "start": 0, "end": 4, "text": "word"}], one)

    def test_cjk_sentence(self):
        text = "象给操作员的每一句加上标注。小象做经典的切分！这是第三句？"
        frags = xx.segment(text)
        # CJK full stops are not sentence boundaries under the recorders'
        # rule, so this is one sentence, one fragment
        self.assertEqual(["s1.0"], [f["id"] for f in frags])
        self.assertEqual(text, frags[0]["text"])

    def test_no_split_inside_protected_spans(self):
        url = "see https://example.com/a/b;but?q=1 and http://x.io/y,so z"
        path = "edit scripts/turn_batch.py; however, keep tests/whole_dir/x.py; but ok"
        tick = "run `git log --oneline; and more` then, but stop"
        paren = "keep (this; however, and but) outside, but split here"
        for text in (url, path, tick, paren):
            for f in xx.segment(text):
                self.assertEqual(text[f["start"]:f["end"]], f["text"])
        # the parenthesis body survives as part of one fragment
        parenfrags = xx.segment(paren)
        inside = [f for f in parenfrags if "(this; however, and but)" in f["text"]]
        self.assertEqual(1, len(inside))
        # no fragment boundary lands inside the URL
        urlfrags = xx.segment(url)
        starts = [f["start"] for f in urlfrags]
        for m in __import__("re").finditer(r"https?://\S+", url):
            for k in range(m.start() + 1, m.end()):
                self.assertNotIn(k, starts)

    def test_short_fragment_merge(self):
        frags = xx.segment("Ok, but now the real work begins in earnest here")
        # "Ok," alone would be under 3 words: merged into the first fragment
        self.assertTrue(frags[0]["text"].startswith("Ok,"))
        self.assertLess(2, len(frags[0]["text"].split()))

    def test_deterministic(self):
        text = "A; b, but c. However, d — and then e, so f continues here."
        self.assertEqual(xx.segment(text), xx.segment(text))

    def test_lines_lists_code_and_url_punctuation(self):
        # review cases (claude-17): each failed on 43c9e029
        text = ("This is the first point\n\nHere is the second point, however it differs\n"
                "- item one is here\n- item two is here")
        got = [f["text"].strip() for f in xx.segment(text)]
        self.assertEqual(["This is the first point", "Here is the second point,", "however it differs",
                          "- item one is here", "- item two is here"], got)
        url = "Please go and see https://example.com/a.b/c?d=e.f, but do not open it today."
        self.assertEqual(2, len(xx.segment(url)))
        code = "Run this now:\n```\nx = 1; y = 2\n- not a list item\n```\nand report back."
        self.assertEqual(1, len(xx.segment(code)))
        for t in (text, url, code):
            for f in xx.segment(t):
                self.assertEqual(f["text"], t[f["start"]:f["end"]])


class ParseReply(unittest.TestCase):
    """The marked-reply parser: marks, targets, offsets, blocks."""

    def test_reply_key_matches_agents_md(self):
        # the 23 marks of AGENTS.md's key, in table order
        expected = {"㊥": "gist", "㊭": "propose", "㊣": "approve",
                    "🈚": "disagree", "㊟": "qualify", "🈖": "explain",
                    "🈯": "clarify", "㊢": "report", "㊩": "report-problem",
                    "㊬": "verify", "🈹": "retract", "🈳": "unresolved",
                    "🈲": "constrain", "🈸": "ask-action", "㊯": "delegate",
                    "㊝": "prioritize", "㊮": "collect", "🈕": "extend",
                    "🈰": "continue", "🈝": "defer", "🈘": "redirect",
                    "㊫": "explore", "🈡": "withdraw"}
        self.assertEqual(expected, xx.REPLY_KEY)

    def test_all_23_marks_parse_with_their_intents(self):
        reply = "\n\n".join(f"{mark} ({intent}-target) para {i}"
                             for i, (mark, intent) in enumerate(sorted(xx.REPLY_KEY.items()), 1))
        recs = xx.parse_reply(reply)
        self.assertEqual(23, len(recs))
        for rec, (mark, intent) in zip(recs, sorted(xx.REPLY_KEY.items())):
            self.assertEqual(mark, rec["mark"])
            self.assertEqual(intent, rec["intent"])
            self.assertEqual(f"{intent}-target", rec["target"])

    def test_offsets_exact_with_table_list_and_fence(self):
        reply = ("㊥ Gist: two findings.\n\n"
                 "㊢ Report (the segmenter): added segment().\n"
                 "| a | b |\n|---|---|\n| 1 | 2 |\n\n"
                 "㊟ (the offsets) checks pass:\n"
                 "- exact offsets\n- no overlap\n\n"
                 "㊬ Verify (the run): the block below is intact\n"
                 "```\ncode; stays (whole)\n\nstill code\n```\n\n"
                 "Unmarked closing paragraph.")
        recs = xx.parse_reply(reply)
        self.assertEqual(["a1", "a2", "a3", "a4", "a5", "a6", "a7", "a8"],
                         [r["id"] for r in recs])
        at = -1
        for r in recs:
            self.assertEqual(reply[r["start"]:r["end"]], r["text"])
            self.assertGreaterEqual(r["start"], at)
            at = r["end"]
        # the table is its own unmarked paragraph; the fenced block keeps
        # its blank line and contents
        # the table (a3) and the list (a5) are their own unmarked paragraphs
        self.assertEqual([None, None], [recs[2]["mark"], recs[2]["intent"]])
        self.assertEqual([None, None], [recs[4]["mark"], recs[4]["intent"]])
        # the fenced block (a7) keeps its blank line and contents, unmarked
        self.assertIn("code; stays (whole)\n\nstill code", recs[6]["text"])
        self.assertEqual(None, recs[6]["mark"])

    def test_target_extraction(self):
        cases = [
            ("㊟ (x) body", "x"),
            ("㊟ [x] body", "x"),
            ("**㊟ (x)** body", "x"),
            ("㊢ Report (x): body", "x"),
            ("㊟ (a (b) c) body", "a (b) c"),
            ("㊟ no target here", None),
            ("㊟ [a [b] c] body", "a [b] c"),
        ]
        for text, target in cases:
            rec = xx.parse_reply(text)[0]
            self.assertEqual(target, rec["target"], text)
            self.assertEqual(text, rec["text"])

    def test_mark_in_the_middle_does_not_mark(self):
        text = "Ordinary opening words ㊟ (x) and more"
        rec = xx.parse_reply(text)[0]
        self.assertEqual(None, rec["mark"])
        self.assertEqual(None, rec["intent"])
        self.assertEqual(None, rec["target"])

    def test_gist_first_paragraph(self):
        rec = xx.parse_reply("Gist: the short version.\n\nBody.")[0]
        self.assertEqual("gist", rec["intent"])
        self.assertEqual(None, rec["mark"])
        # Gist: only counts on the first paragraph
        recs = xx.parse_reply("㊥ x\n\nGist: not the first")
        self.assertEqual(None, recs[1]["intent"])

    def test_unmarked_paragraph_all_null(self):
        rec = xx.parse_reply("Just words.")[0]
        self.assertEqual({"id": "a1", "mark": None, "intent": None, "target": None},
                         {k: rec[k] for k in ("id", "mark", "intent", "target")})

    def test_empty_string(self):
        self.assertEqual([], xx.parse_reply(""))

    def test_deterministic(self):
        text = "㊥ Gist: one.\n\n㊟ (two) two.\n\n🈸 (three) three?"
        self.assertEqual(xx.parse_reply(text), xx.parse_reply(text))



if __name__ == "__main__":
    unittest.main()
