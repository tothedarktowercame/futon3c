#!/usr/bin/env python3
"""Run the generation's unchanged checker once per line in one interpreter.

check_edges.py:30-35 binds HERE from __file__ and CODE from home (or the
fixture env override). main:60-157 rereads adjacent members.json, walks
CODE's canonical repositories for target ids, and reads source/evidence
bytes and quote windows. index_ids (the code-root walk, ~0.27 s of a
~0.27 s run; claude-13 profile 2026-11-16) is taken ONCE per batch: the
batch is then checked against one snapshot of which task ids exist,
which is at least as consistent as per-record runs, each of which saw the
tree at a different moment. Everything else (members.json, source and
evidence bytes) stays per call. No cwd/env changes, bytecode writes, or
cross-record hash caches.
"""
import contextlib
import importlib.util
import io
import json
from pathlib import Path
import sys
import tempfile


def main(checker, records):
    sys.dont_write_bytecode = True
    spec = importlib.util.spec_from_file_location("giraffe_generation_checker", checker)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    if callable(getattr(module, "index_ids", None)):
        ids = module.index_ids()
        module.index_ids = lambda: dict(ids)
    lines = Path(records).read_text().split("\n")
    if lines[-1] == "":
        lines.pop()
    for number, line in enumerate(lines, 1):
        with tempfile.NamedTemporaryFile(mode="w", suffix=".jsonl", encoding="utf-8") as tmp:
            tmp.write(line + "\n")
            tmp.flush()
            out, err = io.StringIO(), io.StringIO()
            with contextlib.redirect_stdout(out), contextlib.redirect_stderr(err):
                try:
                    code = module.main([tmp.name])
                except SystemExit as exc:
                    code = exc.code
                    if code is None:
                        code = 0
                    elif not isinstance(code, int):
                        print(code, file=sys.stderr)
                        code = 1
            print(json.dumps({"line": number, "exit": code or 0,
                              "out": out.getvalue().replace(tmp.name, "giraffe-reading-line"),
                              "err": err.getvalue().replace(tmp.name, "giraffe-reading-line")}))


if __name__ == "__main__":
    main(*sys.argv[1:])
