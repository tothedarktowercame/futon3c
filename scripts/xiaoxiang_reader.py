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
from bisect import bisect_left, bisect_right
from collections import Counter
from datetime import datetime, timezone
import hashlib
import html
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

GAP_HOURS = 6
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


def _epoch(stamp) -> float | None:
    try:
        return datetime.fromisoformat(str(stamp).replace("Z", "+00:00")).timestamp()
    except ValueError:
        return None


def claude_tokens(record: dict) -> tuple[str, int] | None:
    """(message key, input + cache + output tokens) for an assistant reply.
    One reply is written as several lines with the same usage, hence the key."""
    if record.get("type") != "assistant":
        return None
    message = record.get("message") or {}
    usage = message.get("usage") if isinstance(message, dict) else None
    if not isinstance(usage, dict):
        return None
    n = sum(usage.get(k) or 0 for k in ("input_tokens", "cache_creation_input_tokens",
                                        "cache_read_input_tokens", "output_tokens"))
    return f"{message.get('id')}/{record.get('requestId')}", n


def codex_tokens(record: dict) -> tuple[str, int] | None:
    """(running total, input (cached included) + output tokens) for one Codex
    model call.  Repeated token_count events carry an unchanged running total."""
    payload = record.get("payload")
    if record.get("type") != "event_msg" or not isinstance(payload, dict) \
            or payload.get("type") != "token_count" or not isinstance(payload.get("info"), dict):
        return None
    info = payload["info"]
    last, total = info.get("last_token_usage") or {}, info.get("total_token_usage") or {}
    return json.dumps(total, sort_keys=True), (last.get("input_tokens") or 0) + (last.get("output_tokens") or 0)


def gaps(turn_times: list[float], events: list[tuple[float, int]],
         min_hours: float = GAP_HOURS) -> list[dict]:
    """Stretches of at least MIN_HOURS with no typed turn, and the agent tokens
    logged inside each.  Only stretches where agents logged something are kept."""
    times = sorted(set(turn_times))
    events = sorted(events)
    at = [t for t, _ in events]
    prefix = [0]
    for _, n in events:
        prefix.append(prefix[-1] + n)
    out = []
    for a, b in zip(times, times[1:]):
        if b - a < min_hours * 3600:
            continue
        i, j = bisect_right(at, a), bisect_left(at, b)
        if prefix[j] - prefix[i] > 0:
            out.append({"start": a, "end": b, "hours": (b - a) / 3600,
                        "tokens": prefix[j] - prefix[i]})
    return out


def log_files(root: str, pattern: str, days: float | None) -> list[Path]:
    base = Path(os.path.expanduser(root))
    if not base.is_dir():
        return []
    cutoff = time.time() - days * 86400 if days else 0
    return sorted(p for p in base.glob(pattern) if p.stat().st_mtime >= cutoff)


def read(files: list[tuple[str, Path]], model: dict, progress=None, since: float = 0,
         gap_hours: float = GAP_HOURS) -> dict:
    """SINCE (epoch seconds) drops turns and token events before it: a log
    file picked by --days can reach back weeks before the window."""
    intents: Counter = Counter()
    secret_kinds: Counter = Counter()
    secret_files: Counter = Counter()
    # Logs repeat themselves (compaction copies history), so count distinct
    # values.  Only a hash is kept, in memory, for the length of the run.
    distinct: dict[str, set] = {}
    turns = unsure = 0
    turn_times: list[float] = []
    token_events: list[tuple[float, int]] = []
    seen_calls: set[str] = set()
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
                call = (claude_tokens if source == "claude" else codex_tokens)(record)
                if call and call[1] > 0:
                    key = f"{source}/{path}/{call[0]}" if source == "codex" else call[0]
                    when = _epoch(record.get("timestamp"))
                    if when is not None and when >= since and key not in seen_calls:
                        seen_calls.add(key)
                        token_events.append((when, call[1]))
                for text in (claude_turns if source == "claude" else codex_turns)(record):
                    clean, _ = redact(text)
                    turns += 1
                    when = _epoch(record.get("timestamp"))
                    if when is not None and when >= since:
                        turn_times.append(when)
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
            "files_with_secrets": dict(secret_files.most_common()),
            "agent_tokens": sum(n for _, n in token_events),
            "first_turn": min(turn_times, default=None), "last_turn": max(turn_times, default=None),
            "gap_hours": gap_hours, "gaps": gaps(turn_times, token_events, gap_hours)}


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
    if report["gaps"]:
        g = report["gaps"]
        in_gaps = sum(x["tokens"] for x in g)
        h = report.get("gap_hours", GAP_HOURS)
        out.append(f"Agent work while you weren't typing: {len(g)} gap{'s' * (len(g) != 1)}"
                   + (f" of {h:g}+ hours" if h > 0 else " between typed turns") + " "
                   f"with agent activity, holding {in_gaps:,} of {report['agent_tokens']:,} "
                   f"logged tokens ({100 * in_gaps / max(1, report['agent_tokens']):.0f}%).")
        for x in sorted(g, key=lambda x: -x["tokens"])[:5]:
            out.append(f"  {_day(x['start'])} to {_day(x['end'])}  {x['hours']:5.1f} h  "
                       f"{x['tokens']:>14,} tokens")
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


def _day(t: float) -> str:
    return datetime.fromtimestamp(t, timezone.utc).strftime("%Y-%m-%d %H:%M")


def _gap_phrase(report: dict) -> str:
    h = report.get("gap_hours", GAP_HOURS)
    return (f"stretch between turns you typed" if h <= 0
            else f"gap of at least {h:g} hour{'s' * (h != 1)} between turns you typed")


def gap_svg(report: dict) -> str:
    """Mirrored bars on a date axis: width = how long the gap lasted, height =
    tokens agents logged during it."""
    W, H, left, right, mid, half = 1000, 300, 40, 20, 150, 110
    t0, t1 = report["first_turn"], report["last_turn"]
    if not report["gaps"] or t0 is None or t1 <= t0:
        return f"<p>No {_gap_phrase(report)} with agent activity was found.</p>"
    x = lambda t: left + (W - left - right) * (t - t0) / (t1 - t0)
    top = max(g["tokens"] for g in report["gaps"])
    parts = [f'<svg viewBox="0 0 {W} {H}" role="img" style="width:100%;height:auto" '
             'aria-label="Gaps in your typed activity and the tokens agents logged during each">',
             f'<line x1="{left}" y1="{mid}" x2="{W - right}" y2="{mid}" stroke="#999" stroke-width="0.5"/>']
    for g in report["gaps"]:
        h = max(1.0, 2 * half * g["tokens"] / top)
        w = max(1.5, x(g["end"]) - x(g["start"]))
        tip = html.escape(f"{_day(g['start'])} to {_day(g['end'])} UTC: {g['hours']:.1f} h, "
                          f"{g['tokens']:,} tokens")
        parts.append(f'<rect x="{x(g["start"]):.1f}" y="{mid - h / 2:.1f}" width="{w:.1f}" '
                     f'height="{h:.1f}" fill="#8a3b2e" fill-opacity="0.8"><title>{tip}</title></rect>')
    days = max(1, round((t1 - t0) / 86400))
    step = 86400 * max(1, round(days / 8))
    t = t0 - t0 % 86400 + 86400
    while t < t1:
        parts.append(f'<text x="{x(t):.1f}" y="{H - 6}" font-size="11" text-anchor="middle" '
                     f'fill="#666">{datetime.fromtimestamp(t, timezone.utc):%d %b}</text>')
        t += step
    parts.append(f'<text x="{left}" y="14" font-size="11" fill="#666">tallest bar: '
                 f'{top:,} tokens</text></svg>')
    return "".join(parts)


def render_html(report: dict) -> str:
    rows = "".join(f"<tr><td>{_day(g['start'])}</td><td>{_day(g['end'])}</td>"
                   f"<td>{g['hours']:.1f}</td><td>{g['tokens']:,}</td></tr>"
                   for g in sorted(report["gaps"], key=lambda g: -g["tokens"]))
    classified = sum(report["intents"].values()) or 1
    intents = "".join(f"<tr><td>{html.escape(k)}</td><td>{n}</td><td><span style='display:inline-block;"
                      f"height:.6em;width:{300 * n / classified:.0f}px;background:#555'></span></td></tr>"
                      for k, n in report["intents"].items())
    return f"""<!doctype html><html lang="en"><head><meta charset="utf-8">
<title>Your agent logs, read locally</title>
<style>body{{font:15px/1.5 system-ui,sans-serif;max-width:1000px;margin:2rem auto;padding:0 1rem;color:#222}}
table{{border-collapse:collapse;font-size:13px}}td,th{{padding:.15rem .7rem;border-bottom:1px solid #ddd;text-align:left}}</style>
</head><body><h1>Your agent logs, read locally</h1>
<p>{report['turns']} turns you typed, in {report['files']} log files. Generated on this machine; nothing was sent anywhere.</p>
<h2>Work that ran while you weren't typing</h2>
{gap_svg(report)}
<p><i>Each bar is a {_gap_phrase(report)}: its width is how long the gap lasted, and its height the tokens agents logged during it (input, cached input and output). A gap in typing is not proof you were away. Hover a bar for its values.</i></p>
<table><tr><th>from (UTC)</th><th>to</th><th>hours</th><th>tokens</th></tr>{rows}</table>
<h2>What kinds of request you make</h2>
<p>小象's reading, which is often wrong.</p><table>{intents}</table>
<h2>Suspected credentials</h2>
<p>{report['distinct_secrets']} distinct values, appearing {report['secrets']} times. Values are never shown; the terminal report lists the files.</p>
</body></html>"""


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description="Read your own Claude Code and Codex logs locally: the kinds of "
                    "request you make, and credentials left in the logs.")
    ap.add_argument("--days", type=float, help="only logs modified in the last N days")
    ap.add_argument("--claude", default=CLAUDE_ROOT, help=f"default {CLAUDE_ROOT}")
    ap.add_argument("--codex", default=CODEX_ROOT, help=f"default {CODEX_ROOT}")
    ap.add_argument("--json", action="store_true", help="print the report as JSON")
    ap.add_argument("--gap-hours", type=float, default=GAP_HOURS,
                    help="shortest stretch without a typed turn to count as a gap "
                         "(default %(default)g; 1 for errands, 0 for every stretch "
                         "between turns, i.e. all agent tokens laid out over time)")
    ap.add_argument("--html", default="xiaoxiang-report.html",
                    help="also write a page with the gap chart (default %(default)s; '' for none)")
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

    since = time.time() - a.days * 86400 if a.days else 0
    report = read(files, model, progress if sys.stderr.isatty() else None, since, a.gap_hours)
    if sys.stderr.isatty():
        print(file=sys.stderr)
    print(json.dumps(report, indent=1) if a.json else render(report))
    if a.html:
        with open(a.html, "w", encoding="utf-8") as fh:
            fh.write(render_html(report))
        print(f"Wrote {a.html}: open it in a browser for the gap chart.", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
