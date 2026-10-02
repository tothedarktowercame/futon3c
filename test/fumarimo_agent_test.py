"""Contract tests for the two-event Fumarimo Matrix representation."""
import importlib.util
import json
from pathlib import Path
import unittest


SPEC = importlib.util.spec_from_file_location(
    "fumarimo_agent_test_subject",
    Path(__file__).resolve().parents[1] / "scripts/fumarimo_agent.py",
)
fumarimo = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(fumarimo)

ROOM = "!demo:matrix.paragogy.net"
REQUEST = "$request"


class FakeBot:
    channels = [ROOM]

    def __init__(self):
        self.sent = []

    @staticmethod
    def quote_room(room):
        return "%21demo%3Amatrix.paragogy.net"

    def _request(self, method, path, body=None, query=None):
        self.sent.append((method, path, json.loads(json.dumps(body))))
        return {"event_id": "$cell" if len(self.sent) == 1 else "$output"}


class FumarimoAgentTest(unittest.TestCase):
    def test_publishes_python_then_linked_image_output(self):
        bot = FakeBot()
        result = fumarimo.FumarimoPublisher(bot).publish(
            ROOM,
            REQUEST,
            "chart = make_chart()",
            "mxc://matrix.paragogy.net/chart",
            "Posts per author",
            cell_id="cell-1",
            execution_id="run-1",
        )

        self.assertEqual(("$cell", "$output"), result)
        self.assertEqual(2, len(bot.sent))
        code = bot.sent[0][2]
        output = bot.sent[1][2]
        self.assertEqual(fumarimo.PYTHON_MSGTYPE, code["msgtype"])
        self.assertEqual("python-cell", code[fumarimo.EVENT_NAMESPACE]["kind"])
        self.assertEqual(REQUEST, code["m.relates_to"]["m.in_reply_to"]["event_id"])
        self.assertEqual("cell-1", code[fumarimo.EVENT_NAMESPACE]["cell_id"])
        self.assertEqual(fumarimo.OUTPUT_MSGTYPE, output["msgtype"])
        self.assertEqual("image-output", output[fumarimo.EVENT_NAMESPACE]["kind"])
        self.assertEqual("$cell", output["m.relates_to"]["event_id"])
        self.assertNotIn("m.in_reply_to", output["m.relates_to"])
        self.assertEqual("$cell", output[fumarimo.EVENT_NAMESPACE]["cell_event_id"])
        self.assertEqual(REQUEST, output[fumarimo.EVENT_NAMESPACE]["request_event_id"])
        self.assertEqual("run-1", output[fumarimo.EVENT_NAMESPACE]["execution_id"])

    def test_rejects_non_mxc_output_before_sending_output_turn(self):
        bot = FakeBot()
        with self.assertRaisesRegex(ValueError, "MXC"):
            fumarimo.FumarimoPublisher(bot).publish(
                ROOM, REQUEST, "x = 1", "https://example.test/chart.png", "chart"
            )
        self.assertEqual(0, len(bot.sent), "invalid output must not leave an orphan code turn")

    def test_refuses_unlisted_room(self):
        with self.assertRaisesRegex(ValueError, "unlisted"):
            fumarimo.FumarimoPublisher(FakeBot()).publish(
                "!other:test", REQUEST, "x = 1", "mxc://test/chart", "chart"
            )

    def test_fixed_posts_cell_executes_to_svg_with_real_counts(self):
        source = fumarimo.posts_chart_source([
            "@joe:matrix.paragogy.net",
            "@fucodex:matrix.paragogy.net",
            "@joe:matrix.paragogy.net",
        ])
        svg = fumarimo.execute_posts_chart(source).decode()
        self.assertIn(">2</text>", svg)
        self.assertIn(">1</text>", svg)
        self.assertIn(">joe</text>", svg)
        self.assertIn(">fucodex</text>", svg)

    def test_only_addressed_posts_per_author_request_triggers(self):
        self.assertTrue(fumarimo.requests_posts_chart("@fumarimo show posts per author"))
        self.assertTrue(fumarimo.requests_posts_chart("fumarimo: show posts per author"))
        self.assertTrue(fumarimo.requests_posts_chart("show posts per author", addressed=True))
        self.assertFalse(fumarimo.requests_posts_chart("show posts per author"))
        self.assertFalse(fumarimo.requests_posts_chart("@fumarimo execute os.system('id')"))


if __name__ == "__main__":
    unittest.main()
