#!/usr/bin/env python3
"""Capture Lean goals and declaration reachability for a frozen proof.

Goals come from Lean's language server at manifest source positions. Semantic
reachability comes from apm-lean's `#declaration_reachability`; neither is
inferred from proof text by this script.
"""
from __future__ import annotations

import argparse
import hashlib
import json
import os
from pathlib import Path
import re
import select
import subprocess
import tempfile


class LeanServer:
    def __init__(self, root: Path):
        self.root = root
        self.proc = subprocess.Popen(
            ["lake", "env", "lean", "--server"], cwd=root,
            stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        self.next_id = 1000

    def send(self, message):
        body = json.dumps(message).encode()
        self.proc.stdin.write(f"Content-Length: {len(body)}\r\n\r\n".encode() + body)
        self.proc.stdin.flush()

    def receive(self, timeout=30):
        if not select.select([self.proc.stdout], [], [], timeout)[0]:
            raise TimeoutError("Lean language server did not respond")
        length = None
        while True:
            line = self.proc.stdout.readline().decode().strip()
            if not line:
                break
            if line.lower().startswith("content-length:"):
                length = int(line.split(":", 1)[1])
        if length is None:
            raise RuntimeError("Lean response omitted Content-Length")
        return json.loads(self.proc.stdout.read(length))

    def await_id(self, request_id):
        while True:
            message = self.receive()
            if "method" in message and "id" in message:
                self.send({"jsonrpc": "2.0", "id": message["id"], "result": None})
            if message.get("id") == request_id:
                if "error" in message:
                    raise RuntimeError(f"Lean request failed: {message['error']}")
                return message.get("result")

    def request(self, method, params):
        self.next_id += 1
        request_id = self.next_id
        self.send({"jsonrpc": "2.0", "id": request_id,
                   "method": method, "params": params})
        return self.await_id(request_id)

    def open(self, source: Path, text: str):
        self.request("initialize", {
            "processId": os.getpid(), "rootUri": self.root.resolve().as_uri(),
            "capabilities": {},
        })
        self.send({"jsonrpc": "2.0", "method": "initialized", "params": {}})
        self.send({"jsonrpc": "2.0", "method": "textDocument/didOpen", "params": {
            "textDocument": {"uri": source.resolve().as_uri(), "languageId": "lean4",
                             "version": 1, "text": text}}})
        while True:
            message = self.receive()
            if "method" in message and "id" in message:
                self.send({"jsonrpc": "2.0", "id": message["id"], "result": None})
            if (message.get("method") == "textDocument/publishDiagnostics"
                    and message.get("params", {}).get("uri") == source.resolve().as_uri()):
                return message["params"].get("diagnostics", [])

    def goals(self, source: Path, line: int, character: int):
        return self.request("$/lean/plainGoal", {
            "textDocument": {"uri": source.resolve().as_uri()},
            "position": {"line": line, "character": character},
        })

    def close(self):
        self.proc.terminate()
        try:
            self.proc.wait(timeout=5)
        except subprocess.TimeoutExpired:
            self.proc.kill()


def run(root: Path, *args: str) -> subprocess.CompletedProcess:
    return subprocess.run(args, cwd=root, text=True, stdout=subprocess.PIPE,
                          stderr=subprocess.STDOUT, check=True)


def reachability(root: Path, source: Path, target: str, module: str):
    build = root / ".lake/build/lib/lean"
    tool_olean = build / "Tools/DeclarationReachability.olean"
    tool_olean.parent.mkdir(parents=True, exist_ok=True)
    run(root, "lake", "env", "lean", "-o", str(tool_olean),
        "-i", str(tool_olean.with_suffix(".ilean")), "Tools/DeclarationReachability.lean")
    main_olean = build / Path(*module.split(".")).with_suffix(".olean")
    main_olean.parent.mkdir(parents=True, exist_ok=True)
    run(root, "lake", "env", "lean", "-o", str(main_olean),
        "-i", str(main_olean.with_suffix(".ilean")), str(source.relative_to(root)))
    with tempfile.NamedTemporaryFile("w", suffix=".lean") as report:
        report.write("import Tools.DeclarationReachability\n")
        report.write(f"import {module}\n")
        report.write(f"#declaration_reachability {target} in [{module}]\n")
        report.flush()
        output = run(root, "lake", "env", "lean", report.name).stdout
    closure = int(re.search(r"TRANSITIVE_CLOSURE_ALL_MODULES \((\d+)\)", output).group(1))
    total = int(re.search(r"MODULE .* TOTAL \((\d+)\)", output).group(1))
    reachable_block = output.split("REACHABLE (", 1)[1].split("ORPHANS (", 1)[0]
    orphan_block = output.split("ORPHANS (", 1)[1]
    names = lambda block: sorted(line.strip() for line in block.splitlines()[1:] if line.startswith("  "))
    return {"target": target, "module": module, "transitive_closure_size": closure,
            "module_declaration_count": total, "reachable": names(reachable_block),
            "orphans": names(orphan_block)}


def capture(root: Path, source: Path, regions_path: Path, target: str, module: str):
    text = source.read_text()
    raw = text.encode()
    regions = json.loads(regions_path.read_text())
    source_hash = hashlib.sha256(raw).hexdigest()
    if regions["source_sha256"] != source_hash:
        raise ValueError("region manifest does not match Lean source bytes")
    server = LeanServer(root)
    try:
        diagnostics = server.open(source, text)
        captures = []
        for region in regions["regions"]:
            line = region["start_line"] - 1
            character = len(text.splitlines()[line]) - len(text.splitlines()[line].lstrip())
            result = server.goals(source, line, character) or {}
            captures.append({"region_id": region["id"], "line": line + 1,
                             "character": character, "goals": result.get("goals", [])})
    finally:
        server.close()
    version = run(root, "lake", "env", "lean", "--version").stdout.strip()
    return {"schema": "futon3c.lean-formal-capture/v1", "source": regions["source"],
            "source_sha256": source_hash, "lean_version": version,
            "diagnostics": diagnostics, "goal_captures": captures,
            "reachability": reachability(root, source, target, module)}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument("--apm-root", type=Path, required=True)
    parser.add_argument("--source", type=Path, required=True)
    parser.add_argument("--regions", type=Path, required=True)
    parser.add_argument("--target", required=True)
    parser.add_argument("--module", required=True)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()
    source = args.source if args.source.is_absolute() else args.apm_root / args.source
    payload = capture(args.apm_root.resolve(), source.resolve(), args.regions.resolve(),
                      args.target, args.module)
    args.output.write_text(json.dumps(payload, ensure_ascii=False, sort_keys=True, indent=2) + "\n")


if __name__ == "__main__":
    main()
