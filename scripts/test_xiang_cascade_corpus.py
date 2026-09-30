import json
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))
import xiang_cascade_corpus as corpus


def frame(eid, at, commits=None, reply=False):
    happened = []
    if reply:
        happened.append({"at": at, "type": "coordination",
                         "summary": {"event": "chat-turn", "text": "reply"}})
    if commits is not None:
        happened.append({"at": at, "type": "coordination",
                         "summary": {"event": "turn-commits", "commits": commits}})
    return {"turn": {"evidence_id": eid, "at": at}, "happened": happened}


def turn_row(session="s", at="2026-09-30T00:01:00Z", evidence="op-2"):
    return {"base": {"session_id": session, "created_at": at,
                     "evidence_id": evidence},
            "fragments": [{"sentence": "s1", "index": 0, "intent": "correct",
                           "target": "work", "pattern_refs": []}]}


class CorpusTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        self.repo = self.root / "repo"
        self.repo.mkdir()
        subprocess.run(["git", "init", "-q", str(self.repo)], check=True)
        subprocess.run(["git", "-C", str(self.repo), "config", "user.email", "test@example.com"], check=True)
        subprocess.run(["git", "-C", str(self.repo), "config", "user.name", "Test"], check=True)
        (self.repo / "x").write_text("x")
        subprocess.run(["git", "-C", str(self.repo), "add", "x"], check=True)
        subprocess.run(["git", "-C", str(self.repo), "commit", "-qm", "fixture"], check=True)
        self.sha = subprocess.check_output(
            ["git", "-C", str(self.repo), "rev-parse", "HEAD"], text=True).strip()

    def tearDown(self):
        self.temp.cleanup()

    def test_newline_prefixed_commit_sha_resolves(self):
        facts = corpus.frame_facts(
            frame("op-1", "2026-09-30T00:00:00Z",
                  [{"repo": "repo", "sha": "\n" + self.sha}]), self.root)
        self.assertEqual(self.sha, facts["commits"][0]["sha"])
        self.assertTrue(facts["commits"][0]["resolved"])
        self.assertEqual(1, facts["commit_resolved"])

    def test_next_operator_frame_is_not_this_turns_post(self):
        own = {"repo": "repo", "sha": self.sha}
        next_only = {"repo": "repo", "sha": "deadbeef"}
        frames = [
            frame("op-1", "2026-09-30T00:00:00Z", reply=True),
            frame("op-2", "2026-09-30T00:01:00Z", [own], reply=True),
            frame("op-3", "2026-09-30T00:02:00Z", [next_only], reply=True),
        ]
        entry = corpus.build_entry("family", "turn-x", turn_row(),
                                   {"frames": frames}, self.root)
        self.assertEqual([self.sha], [c["sha"] for c in entry["post"]["commits"]])
        self.assertNotIn("deadbeef", json.dumps(entry))

    def test_session_without_frames_is_typed_missing(self):
        entry = corpus.build_entry("family", "turn-x", turn_row(), {"frames": []}, self.root)
        self.assertEqual("session-no-frames", entry["missing"])
        self.assertNotIn("pre", entry)
        self.assertNotIn("post", entry)

    def test_stable_order_and_jsonl(self):
        rows = {"b": {"turn-z": turn_row("s2")}, "a": {"turn-y": turn_row("s1")}}
        frames = {"s1": {"missing": "store-504"}, "s2": {"missing": "store-504"}}
        entries = corpus.assemble(rows, frames, self.root)
        self.assertEqual(["a", "b"], [e["family"] for e in entries])
        one, two = self.root / "one", self.root / "two"
        corpus.write_jsonl(entries, one)
        corpus.write_jsonl(entries, two)
        self.assertEqual(one.read_bytes(), two.read_bytes())


if __name__ == "__main__":
    unittest.main()
