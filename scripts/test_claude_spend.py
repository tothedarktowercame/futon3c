import importlib.util
import json
from pathlib import Path


SCRIPT = Path(__file__).with_name("claude-spend.py")
SPEC = importlib.util.spec_from_file_location("claude_spend", SCRIPT)
claude_spend = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(claude_spend)


def test_split_message_usage_is_counted_once(tmp_path):
    path = tmp_path / "session.jsonl"
    usage = {
        "input_tokens": 100,
        "cache_read_input_tokens": 200,
        "cache_creation_input_tokens": 300,
        "output_tokens": 400,
    }
    records = [
        {"uuid": f"block-{index}",
         "message": {"id": "message-1", "usage": usage}}
        for index in range(3)
    ]
    path.write_text("".join(json.dumps(record) + "\n" for record in records))

    totals, count, contexts = claude_spend.scan(path)

    assert count == 1
    assert contexts == [600]
    expected = (
        100 * claude_spend.IN
        + 200 * claude_spend.RD
        + 300 * claude_spend.W5
        + 400 * claude_spend.OUT
    ) / 1e6
    assert abs(claude_spend.cost(totals) - expected) < 1e-12


if __name__ == "__main__":
    import sys
    import tempfile
    import traceback

    failed = 0
    for name, fn in sorted(globals().items()):
        if not (name.startswith("test_") and callable(fn)):
            continue
        try:
            if fn.__code__.co_argcount:
                with tempfile.TemporaryDirectory() as directory:
                    fn(Path(directory))
            else:
                fn()
            print(f"ok   {name}")
        except Exception:
            failed += 1
            print(f"FAIL {name}")
            traceback.print_exc()
    sys.exit(1 if failed else 0)
