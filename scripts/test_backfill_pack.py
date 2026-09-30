"""Tests for backfill_pack.py.  Run: python3 scripts/test_backfill_pack.py

Hermetic: a temp block directory with two tiny request records and a temp
library with three tiny flexiargs; the BM25 index is injected, so no test
depends on the real library.
"""
import json
import os
import subprocess
import sys
import tempfile
import unittest

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import backfill_pack as bp  # noqa: E402


def make_block(root):
    block = os.path.join(root, "block")
    os.makedirs(block)
    # two turns of one session; the first is context for the second
    for tid, at, after in [("agent-1-turn-1", "2026-08-01T10:00:00Z",
                            "First turn mentions baldwin effects."),
                           ("agent-1-turn-2", "2026-08-01T10:05:00Z",
                            "Second turn. Let's test the harness work.")]:
        text = after
        n = len(text)
        doc = {"version": 1, "source_text": text, "original_text": text,
               "offset_unit": "x",
               "sentences": [{"id": "s1", "start": 0, "end": n,
                              "text": text, "status": "unresolved", "cues": []}],
               "created_at": at, "agent_id": "agent-1", "session_id": "sess-1",
               "turn_id": tid, "surface": "historical"}
        with open(os.path.join(block, tid + ".json"), "w", encoding="utf-8") as fh:
            json.dump(doc, fh)
    return block


def make_library(root):
    lib = os.path.join(root, "library")
    os.makedirs(os.path.join(lib, "baldwin"))
    os.makedirs(os.path.join(lib, "harness"))
    files = {
        "baldwin/effect": "@title The Baldwin effect\n! conclusion: effects latch\n"
                          "+ context: evolution\nIF: a skill repeats\nHOWEVER: costly\n"
                          "THEN: it sinks in\n",
        "baldwin/second": "@title Second Baldwin pattern\n! conclusion: second\n"
                          "+ context: evolution too\n",
        "harness/pack": "@title Pack the harness\n! conclusion: batch fetches\n"
                        "+ context: tooling\nIF: readers fetch one by one\nTHEN: pack\n",
    }
    for pid, body in files.items():
        with open(os.path.join(lib, pid + ".flexiarg"), "w", encoding="utf-8") as fh:
            fh.write(body)
    return lib


class FakeXlate:
    """stand-in for xlate: naive substring scoring over an injected index."""

    def __init__(self, docs):
        self.docs = docs

    def bm25(self, query, docs, n=8):
        q = query.lower()
        hits = [(pid, 1) for pid, d in docs.items()
                if any(w in q for w in d["title"].lower().split())]
        return sorted(hits, key=lambda kv: -kv[1])[:n]


DOCS = {"baldwin/effect": {"title": "The Baldwin effect"},
        "baldwin/second": {"title": "Second Baldwin pattern"},
        "harness/pack": {"title": "Pack the harness"}}


class Pack(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.block = make_block(self.tmp.name)
        self.lib = make_library(self.tmp.name)
        self.docs = {k: dict(v) for k, v in DOCS.items()}
        self.xlate = FakeXlate(self.docs)
        self.out = os.path.join(self.tmp.name, "PACK.md")

    def tearDown(self):
        self.tmp.cleanup()

    def test_pack_has_sentences_hits_appendix_and_family(self):
        stats = bp.build_pack(self.block, ["agent-1-turn-2", "agent-1-turn-1"],
                              self.out, docs=self.docs, xlate=self.xlate, lib=self.lib)
        body = open(self.out, encoding="utf-8").read()
        # every sentence id present, both hit kinds per sentence
        self.assertIn("**s1** (0..41)", body)
        self.assertIn("- MOVE:", body)
        self.assertIn("- SUBJECT:", body)
        # the family rule fires: turn 1 names baldwin, the whole family is added
        self.assertIn("### family named by this turn", body)
        self.assertIn("baldwin/effect", body)
        self.assertIn("baldwin/second", body)
        # context: turn 2's pack shows turn 1
        self.assertIn("context: previous turn `agent-1-turn-1`", body)
        # the appendix holds each distinct pattern once, with the named parts
        for pid in ("baldwin/effect", "baldwin/second", "harness/pack"):
            self.assertEqual(1, body.count(f"### {pid}\n"))
        self.assertIn("- HOWEVER: costly", body)
        self.assertIn("- THEN: it sinks in", body)
        self.assertIn("- HOWEVER: (absent)", body)  # baldwin/second lacks HOWEVER
        # instructions at the top, closed list and no-tool-call rule
        self.assertIn("## Intents: closed list", body)
        self.assertIn("more_searches", body)
        # example template of the first turn included
        self.assertIn("example: the filled template of THIS turn", body)
        self.assertEqual({"turns": 2, "patterns": 3}, {k: stats[k] for k in ("turns", "patterns")})

    def test_offsets_of_sentences_are_reported(self):
        bp.build_pack(self.block, ["agent-1-turn-1"], self.out,
                      docs=self.docs, xlate=self.xlate, lib=self.lib)
        body = open(self.out, encoding="utf-8").read()
        self.assertIn("**s1** (0..36)", body)


class Publish(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.block = make_block(self.tmp.name)
        self.answer = os.path.join(self.tmp.name, "answer.json")

    def tearDown(self):
        self.tmp.cleanup()

    def valid(self):
        return {"turn_id": "agent-1-turn-2", "labeller": "test-reader",
                "reusable_cues": [],
                "sentences": [{"id": "s1", "fragments": [
                    {"start": 0, "end": 41,
                     "text": "Second turn. Let's test the harness work.",
                     "intent": "report", "target": "the harness work",
                     "rationale": "it reports what the second turn does",
                     "relations": ["action"],
                     "pattern_refs": [],
                     "display_cues": [{"start": 0, "end": 6, "text": "Second"}],
                     "no_surface_cue": ""}],
                    "unresolved_reason": ""}]}

    def invalid(self):
        bad = self.valid()
        bad["turn_id"] = "agent-1-turn-1"
        bad["sentences"][0]["fragments"][0]["end"] = 999  # past the source
        return bad

    def run_publish(self, elements):
        with open(self.answer, "w", encoding="utf-8") as fh:
            json.dump(elements, fh, ensure_ascii=False)
        proc = subprocess.run(
            [sys.executable, os.path.join(os.path.dirname(bp.__file__),
                                          "backfill_pack.py"),
             "publish", self.block, self.answer],
            capture_output=True, text=True)
        return proc, json.loads(proc.stdout)

    def test_one_valid_one_invalid(self):
        proc, summary = self.run_publish([self.valid(), self.invalid()])
        self.assertEqual(0, proc.returncode)
        self.assertEqual(1, summary["published"])
        self.assertEqual(1, summary["refused"])
        self.assertTrue(os.path.exists(
            os.path.join(self.block, "agent-1-turn-2.json.analysis.json")))
        self.assertIn("reason", summary["turns"][1])

    def test_off_list_intent_is_refused(self):
        # review case (claude-17): the validator itself accepts any label
        bad = self.valid()
        bad["sentences"][0]["fragments"][0]["intent"] = "vibes"
        proc, summary = self.run_publish([bad])
        self.assertEqual(0, summary["published"])
        self.assertIn("closed list", summary["turns"][0]["reason"])
        self.assertFalse(os.path.exists(
            os.path.join(self.block, "agent-1-turn-2.json.analysis.json")))

    def test_offsets_are_placed_from_quoted_text(self):
        # review case (claude-17): readers quote text, the publisher counts
        good = self.valid()
        frag = good["sentences"][0]["fragments"][0]
        del frag["start"], frag["end"]
        frag["display_cues"] = [{"text": "harness work"}]
        proc, summary = self.run_publish([good])
        self.assertEqual(1, summary["published"], summary)
        wrong = self.valid()
        wrong["turn_id"] = "agent-1-turn-1"
        wrong["sentences"][0]["fragments"][0]["text"] = "words that are not there"
        proc, summary = self.run_publish([wrong])
        self.assertIn("not in sentence", summary["turns"][0]["reason"])

    def test_existing_analysis_not_overwritten(self):
        _proc, first = self.run_publish([self.valid()])
        self.assertEqual(1, first["published"])
        again, summary = self.run_publish([self.valid()])
        self.assertEqual(0, again.returncode)
        self.assertEqual(0, summary["published"])
        self.assertEqual("analysis already exists", summary["turns"][0]["reason"])


if __name__ == "__main__":
    unittest.main()
