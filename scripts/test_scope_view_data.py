#!/usr/bin/env python3
import json
import unittest
from unittest import mock

import scope_view_data as scope


class Response:
    def __init__(self, entries):
        self.payload = json.dumps({"entries": entries}).encode()

    def __enter__(self):
        return self

    def __exit__(self, *_):
        return False

    def read(self):
        return self.payload


RESULT = '{:id "pattern/one", :title "One", :score 0.8, :rank 1}'


class RetrievalJoinTest(unittest.TestCase):
    def lookup(self, body, turn="claude-8-turn-4", source="operator words continue"):
        row = {"evidence/id": "e-r", "evidence/at": "2026-11-05T00:00:00Z",
               "evidence/body": body}
        with mock.patch.object(scope.urllib.request, "urlopen",
                               return_value=Response([row])):
            return scope.retrieval_for("claude-8", "session-8", turn, source)

    def test_new_row_joins_by_turn_reference_without_query(self):
        found = self.lookup('{"event" "context-retrieval", '
                            '"turn-id" "claude-8-turn-4", '
                            '"results" [' + RESULT + ']}')
        self.assertEqual("e-r", found["evidence-id"])
        self.assertEqual("operator words continue", found["query"])
        self.assertEqual("pattern/one", found["results"][0]["id"])

    def test_legacy_row_joins_by_query_prefix(self):
        found = self.lookup('{"event" "context-retrieval", '
                            '"query" "operator words", '
                            '"results" [' + RESULT + ']}')
        self.assertEqual("e-r", found["evidence-id"])
        self.assertEqual("operator words", found["query"])


if __name__ == "__main__":
    unittest.main()
