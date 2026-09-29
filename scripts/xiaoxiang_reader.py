"""Read a person's own agent logs locally: which kinds of request they make, and
how many credentials sit in the logs.  Nothing leaves the machine.

Claude Code keeps sessions in ~/.claude/projects/*/*.jsonl and Codex in
~/.codex/sessions/YYYY/MM/DD/rollout-*.jsonl.  A turn counts as typed by the
person when it is a user record with text of its own: tool results, subagent
(sidechain) traffic, injected context (<environment_context>, AGENTS.md, slash
command wrappers) and compaction summaries are skipped.

Every turn is redacted with secret_scan before it is classified, and the report
carries counts, kinds and file paths, never turn text or secret values.

In the repository this imports secret_scan and xiaoxiang; `xiaoxiang.py bundle`
joins all three with the exported model into one standalone file.
"""
from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import json
import os
from pathlib import Path
import sys
import time

# --- dev imports (removed in the bundle) ---
sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from secret_scan import redact, scan  # noqa: E402
from xiaoxiang import classify, evidence  # noqa: E402
MODEL = None
# --- end dev imports ---

CLAUDE_ROOT = "~/.claude/projects"
CODEX_ROOT = "~/.codex/sessions"
_INJECTED = ("<", "# AGENTS.md", "[compacted", "Caveat:")


def _typed(text: str) -> str | None:
    text = text.strip()
    if not text or text.startswith(_INJECTED):
        return None
    # Agency-relayed turns carry a routing header; the person's words follow it.
    # Only an operator's turn was typed by a person; the rest are agents'.
    marker = "\nUser message:\n"
    if text.startswith("--- CURRENT TURN ---"):
        header, _, rest = text.partition(marker)
        if "\nOrigin: operator\n" not in header + "\n":
            return None
        text = rest.strip()
    return text or None


def claude_turns(record: dict) -> list[str]:
    if record.get("type") != "user" or record.get("isMeta") or record.get("isSidechain"):
        return []
    content = (record.get("message") or {}).get("content")
    if isinstance(content, str):
        texts = [content]
    elif isinstance(content, list):
        if any(isinstance(b, dict) and b.get("type") == "tool_result" for b in content):
            return []
        texts = [b.get("text", "") for b in content
                 if isinstance(b, dict) and b.get("type") == "text"]
    else:
        return []
    return [t for t in (_typed(x) for x in texts) if t]


def codex_turns(record: dict) -> list[str]:
    payload = record.get("payload")
    if (record.get("type") != "response_item" or not isinstance(payload, dict)
            or payload.get("type") != "message" or payload.get("role") != "user"):
        return []
    return [t for t in (_typed(b.get("text", "")) for b in payload.get("content") or []
                        if isinstance(b, dict) and b.get("type") == "input_text") if t]


def log_files(root: str, pattern: str, days: float | None) -> list[Path]:
    base = Path(os.path.expanduser(root))
    if not base.is_dir():
        return []
    cutoff = time.time() - days * 86400 if days else 0
    return sorted(p for p in base.glob(pattern) if p.stat().st_mtime >= cutoff)


def read(files: list[tuple[str, Path]], model: dict, progress=None) -> dict:
    intents: Counter = Counter()
    secret_kinds: Counter = Counter()
    secret_files: Counter = Counter()
    # Logs repeat themselves (compaction copies history), so count distinct
    # values.  Only a hash is kept, in memory, for the length of the run.
    distinct: dict[str, set] = {}
    turns = unsure = 0
    total = sum(p.stat().st_size for _, p in files) or 1
    done = 0
    for n, (source, path) in enumerate(files, 1):
        with open(path, encoding="utf-8", errors="replace") as fh:
            for line in fh:
                found = scan(line)
                for f in found:
                    secret_kinds[f.kind] += 1
                    distinct.setdefault(f.kind, set()).add(
                        hashlib.sha256(line[f.start:f.end].encode()).digest())
                if found:
                    secret_files[str(path)] += len(found)
                try:
                    record = json.loads(line)
                except ValueError:
                    continue
                if not isinstance(record, dict):
                    continue
                for text in (claude_turns if source == "claude" else codex_turns)(record):
                    clean, _ = redact(text)
                    turns += 1
                    if evidence(model, clean):
                        intents[classify(model, clean, 1)[0][0]] += 1
                    else:
                        unsure += 1
        done += path.stat().st_size
        if progress:
            progress(n, len(files), done / total)
    return {"files": len(files), "turns": turns, "intents": dict(intents.most_common()),
            "too_little_to_go_on": unsure, "secrets": sum(secret_kinds.values()),
            "distinct_secrets": sum(len(v) for v in distinct.values()),
            "secret_kinds": {k: {"distinct": len(distinct[k]), "occurrences": n}
                             for k, n in secret_kinds.most_common()},
            "files_with_secrets": dict(secret_files.most_common())}


def render(report: dict) -> str:
    out = [f"Read {report['turns']} turns you typed, in {report['files']} log files.", ""]
    if report["turns"]:
        out.append("What kinds of request you make (小象's reading, which is often wrong):")
        classified = sum(report["intents"].values()) or 1
        for intent, n in report["intents"].items():
            out.append(f"  {intent:<16} {n:>6}  {100 * n / classified:5.1f}%  "
                       + "#" * round(40 * n / classified))
        if report["too_little_to_go_on"]:
            out.append(f"  ({report['too_little_to_go_on']} turns had too little to go on)")
        out.append("")
    out.append(f"Suspected credentials in these logs: {report['distinct_secrets']} distinct "
               f"values, appearing {report['secrets']} times")
    for kind, n in report["secret_kinds"].items():
        out.append(f"  {kind:<20} {n['distinct']:>6} distinct  {n['occurrences']:>8} times")
    if report["files_with_secrets"]:
        out.append("Files holding them (values are never printed):")
        for path, n in list(report["files_with_secrets"].items())[:20]:
            out.append(f"  {n:>5}  {path}")
    return "\n".join(out)


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description="Read your own Claude Code and Codex logs locally: the kinds of "
                    "request you make, and credentials left in the logs.")
    ap.add_argument("--days", type=float, help="only logs modified in the last N days")
    ap.add_argument("--claude", default=CLAUDE_ROOT, help=f"default {CLAUDE_ROOT}")
    ap.add_argument("--codex", default=CODEX_ROOT, help=f"default {CODEX_ROOT}")
    ap.add_argument("--json", action="store_true", help="print the report as JSON")
    if MODEL is None:
        ap.add_argument("--model", required=True, help="model JSON from xiaoxiang.py export")
    a = ap.parse_args(argv)
    model = MODEL
    if model is None:
        with open(a.model, encoding="utf-8") as fh:
            model = json.load(fh)
    files = ([("claude", p) for p in log_files(a.claude, "*/*.jsonl", a.days)]
             + [("codex", p) for p in log_files(a.codex, "*/*/*/rollout-*.jsonl", a.days)])
    if not files:
        print("No Claude Code or Codex logs found.", file=sys.stderr)
        return 1
    start = time.time()

    def progress(n, total, share):
        spent = time.time() - start
        left = spent / share - spent if share else 0
        print(f"\r  {n}/{total} files, {100 * share:.0f}%, about {left / 60:.0f} min left ",
              end="", file=sys.stderr, flush=True)

    report = read(files, model, progress if sys.stderr.isatty() else None)
    if sys.stderr.isatty():
        print(file=sys.stderr)
    print(json.dumps(report, indent=1) if a.json else render(report))
    return 0


if __name__ == "__main__":
    sys.exit(main())
