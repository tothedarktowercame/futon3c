#!/usr/bin/env python3
import json
import importlib.util
import sys
import tempfile
import unittest
from pathlib import Path

SPEC = importlib.util.spec_from_file_location(
    "build_mission_graph", Path(__file__).with_name("build_mission_graph.py")
)
builder = importlib.util.module_from_spec(SPEC)
assert SPEC.loader is not None
sys.modules["build_mission_graph"] = builder
SPEC.loader.exec_module(builder)


class MissionGraphTest(unittest.TestCase):
    def record(self, rid, acts, closed=None, session="session-a"):
        return {"_record_id": rid, "turn_id": rid, "session_id": session,
                "agent_id": "claude-17", "created_at": "2026-10-11T00:00:00Z",
                "acts": acts, "ports_status": {"rules_version": "test-rules"},
                "ports": {"closed_this_turn": closed or []}}

    def test_live_pin_close_target(self):
        path = Path.home() / ".emacs-graph/session-turn-analysis/turn-07igHH.json"
        self.assertTrue(path.exists())
        record = json.loads(path.read_text())
        record["_record_id"] = "turn-07igHH"
        graph = builder.build_graph("M-象-2000", [record], Path(__file__).parent.parent)
        edges = [e for e in graph.edges if e.type == "port/close"]
        self.assertEqual(1, len(edges))
        self.assertEqual("claude-17-turn-48-f-s1-0", edges[0].target)

    def test_real_sidecar_edge(self):
        root = Path(__file__).parent.parent
        found = None
        for path, payload in builder._sidecars(root):
            for item in payload.get("acts") or []:
                for answer in item.get("answers") or []:
                    if answer.get("effect") in {"closes", "keeps", "unclear"}:
                        found = (path, payload, item, answer); break
                if found: break
            if found: break
        self.assertIsNotNone(found)
        path, payload, item, answer = found
        sid = str(payload["record"])
        source = str(item["act"]); target = str(answer["act"])
        record = self.record(
            sid,
            [{"id": source, "kind": item.get("kind"), "text": item.get("quote", ""), "author": "claude-17"},
             {"id": target, "kind": "ask", "text": "target", "author": "operator"}],
        )
        graph = builder.build_graph("M-象-2000", [record], root)
        matches = [e for e in graph.edges if e.source == source and e.target == target]
        self.assertTrue(matches, (path, source, target))
        self.assertEqual(f"answers/{answer['effect']}", matches[0].type)
        self.assertEqual(answer.get("quote", ""), matches[0].quote)

    def test_tarjan_cycle_marks_only_cycle_edges(self):
        nodes = {x: builder.Node(x) for x in "abcd"}
        edges = [builder.Edge("answers/closes", "a", "b", "labelled"),
                 builder.Edge("answers/closes", "b", "c", "labelled"),
                 builder.Edge("answers/closes", "c", "a", "labelled"),
                 builder.Edge("answers/closes", "c", "d", "labelled")]
        sccs = builder.tarjan(nodes, edges)
        cycle = next(s for s in sccs if len(s) == 3)
        for edge in edges:
            edge.red = edge.source in cycle and edge.target in cycle
        self.assertEqual(3, sum(e.red for e in edges))

    def test_scope_transitive_reduction_only(self):
        edges = [builder.Edge("scope/within", "a", "b", "labelled"),
                 builder.Edge("scope/within", "b", "c", "labelled"),
                 builder.Edge("scope/within", "a", "c", "labelled"),
                 builder.Edge("answers/closes", "a", "c", "labelled")]
        reduced = builder.transitive_reduce(edges)
        self.assertEqual(3, len(reduced))
        self.assertFalse(any(e.type == "scope/within" and e.source == "a" and e.target == "c" for e in reduced))
        self.assertTrue(any(e.type == "answers/closes" for e in reduced))

    def test_html_has_one_edge_row_each(self):
        record = self.record("turn-test", [
            {"id": "a", "kind": "ask", "text": "ask", "author": "operator"},
            {"id": "r", "kind": "reply", "text": "reply", "author": "claude-17"}],
            [{"act": "a", "kind": "ask", "text": "ask"}])
        graph = builder.build_graph("M-test", [record], Path(__file__).parent.parent)
        page = builder.render_html(graph)
        self.assertEqual(len(graph.edges), page.count('class="edge-row"'))


if __name__ == "__main__":
    unittest.main()
