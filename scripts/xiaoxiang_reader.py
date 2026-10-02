"""Read a person's own agent logs locally: which kinds of request they make, and
how many credentials sit in the logs.  Nothing leaves the machine.

Claude Code keeps sessions in ~/.claude/projects/*/*.jsonl and Codex in
~/.codex/sessions/YYYY/MM/DD/rollout-*.jsonl.  A turn counts as typed by the
person when it is a user record with text of its own: tool results, subagent
(sidechain) traffic, injected context (<environment_context>, AGENTS.md, slash
command wrappers) and compaction summaries are skipped.

Every turn is redacted with secret_scan before it is classified, and the report
carries counts and kinds, never turn text or secret values.  Which files hold
them is printed only with --list-files, and never when a coding agent is
running the scan (CLAUDECODE, CLAUDE_CODE_*, CODEX_*, AI_AGENT in the
environment): a list of files holding credentials is a map, and an agent that
was handed one went and read them.
Credentials are also grouped by where they sit (what you typed, tool output,
files the agent wrote, test fixtures or documented example keys), since "13
credentials" means something different when none is in what you typed.  An
intent is reported only when the model was right on it at least half the time
in cross-validation; the rest are counted as not sure.  --days keeps turns and
agent work from the window, and the stretch after your last turn counts as a
gap, closed by the last agent event.

Files are read in parallel processes (--jobs, default the CPU count) and the
secret scan, which is nearly all of the work, skips a rule when the line
cannot contain it; a resumed session's copied transcript is counted once.
Measured on a 4.5 MB log split four ways: 2.4 s before, 0.47 s after, with
identical findings.

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
import re
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
         min_hours: float = GAP_HOURS, edges: bool = True) -> list[dict]:
    """Stretches of at least MIN_HOURS with no typed turn, and the agent tokens
    logged inside each.  Only stretches where agents logged something are kept.
    With EDGES, the stretch after the last typed turn (up to the last agent
    event) counts too, marked "after": that is the "asked, then went to bed"
    case the chart exists for, and it has no later turn to close it.  So does
    the stretch before the first typed turn, marked "before"."""
    times = sorted(set(turn_times))
    events = sorted(events)
    at = [t for t, _ in events]
    prefix = [0]
    for _, n in events:
        prefix.append(prefix[-1] + n)
    out = []

    def stretch(a, b, edge=None):
        if b - a < min_hours * 3600:
            return
        i, j = bisect_right(at, a), bisect_left(at, b)
        if edge == "after":
            j = len(at)  # the last event is inside the stretch, not its boundary
        if edge == "before":
            i = 0  # likewise the first
        if prefix[j] - prefix[i] > 0:
            g = {"start": a, "end": b, "hours": (b - a) / 3600, "tokens": prefix[j] - prefix[i]}
            if edge:
                g["edge"] = edge
            out.append(g)

    if edges and times and at and at[0] < times[0]:
        stretch(at[0], times[0], "before")
    for a, b in zip(times, times[1:]):
        stretch(a, b)
    if edges and times and at and at[-1] > times[-1]:
        stretch(times[-1], at[-1], "after")
    return out


def log_files(root: str, pattern: str, days: float | None) -> list[Path]:
    base = Path(os.path.expanduser(root))
    if not base.is_dir():
        return []
    cutoff = time.time() - days * 86400 if days else 0
    return sorted(p for p in base.glob(pattern) if p.stat().st_mtime >= cutoff)


_WORKER_MODEL: dict | None = None


def _init_worker(model: dict) -> None:
    global _WORKER_MODEL
    _WORKER_MODEL = model


SURE = 0.5  # an intent is reported outright when its cross-validated precision is at least this
_FIXTURE_PATH = re.compile(r"(?i)(?:^|[/_.-])(?:tests?|spec|specs|fixtures?|examples?|mock|fake|dummy)(?:[/_.-]|$)")
_EDIT_TOOLS = {"Write", "Edit", "MultiEdit", "NotebookEdit", "create_file", "apply_patch"}


def where_in(source: str, record: dict | None) -> tuple[str, bool]:
    """Where a line's content came from, and whether it is a file the agent
    wrote to a test-like path.  A credential in what you typed is yours; one
    in a tool result was read off your machine; one in a file or command the
    agent wrote is in your repository now, and if the path says test, spec or
    fixture it is most likely a made-up value.  The report groups by this so
    "13 credentials" can be read as "0 of yours; 13 in test fixtures"."""
    if not isinstance(record, dict):
        return "elsewhere", False
    if source == "codex":
        payload = record.get("payload") if isinstance(record.get("payload"), dict) else {}
        kind = payload.get("type")
        if kind == "message":
            return ("typed" if payload.get("role") == "user" else "agent-said"), False
        if kind in ("function_call", "custom_tool_call", "local_shell_call"):
            args = str(payload.get("arguments") or payload.get("input") or "")
            return "agent-wrote", bool(_FIXTURE_PATH.search(args[:400]))
        if kind in ("function_call_output", "custom_tool_call_output"):
            return "tool-output", False
        return "elsewhere", False
    kind = record.get("type")
    content = (record.get("message") or {}).get("content") if isinstance(record.get("message"), dict) else None
    if kind == "user":
        if isinstance(content, list) and any(isinstance(b, dict) and b.get("type") == "tool_result" for b in content):
            return "tool-output", False
        return "typed", False
    if kind == "assistant":
        blocks = content if isinstance(content, list) else []
        uses = [b for b in blocks if isinstance(b, dict) and b.get("type") == "tool_use"]
        if uses:
            fixture = any(
                u.get("name") in _EDIT_TOOLS
                and _FIXTURE_PATH.search(str((u.get("input") or {}).get("file_path")
                                             or (u.get("input") or {}).get("path") or ""))
                for u in uses)
            return "agent-wrote", fixture
        return "agent-said", False
    return "elsewhere", False


def read_file(source: str, path: Path, model: dict | None = None, since: float = 0) -> dict:
    """Everything one log file contributes: counts, times and token events,
    and the secret kinds with a hash per distinct value.  Pure in the sense
    that two files can be read in any order or in parallel and merged.
    SINCE drops turns and token events before it; credentials are counted
    wherever they sit in the file, since a value in an old line is still in
    the log."""
    model = model if model is not None else _WORKER_MODEL
    precision = model.get("precision") or {}
    intents: Counter = Counter()
    secret_kinds: Counter = Counter()
    secret_where: Counter = Counter()
    # Logs repeat themselves (compaction copies history), so count distinct
    # values.  Only a hash is kept, in memory, for the length of the run.
    distinct: dict[str, set] = {}
    distinct_where: dict[str, set] = {}
    turns = unsure = not_sure = secrets_here = 0
    turn_times: list[float] = []
    token_events: list[tuple[float, int]] = []
    seen_calls: set[str] = set()
    with open(path, encoding="utf-8", errors="replace") as fh:
        for line in fh:
            try:
                record = json.loads(line)
            except ValueError:
                record = None
            if record is not None and not isinstance(record, dict):
                record = None
            found = scan(line)
            if found:
                where, fixture = where_in(source, record)
                for f in found:
                    secret_kinds[f.kind] += 1
                    value = line[f.start:f.end]
                    digest = hashlib.sha256(value.encode()).digest()
                    distinct.setdefault(f.kind, set()).add(digest)
                    # Documented example keys (AWS's AKIAIOSFODNN7EXAMPLE and
                    # friends) are fixtures wherever they sit.
                    place = "fixture" if fixture or "EXAMPLE" in value else where
                    secret_where[place] += 1
                    distinct_where.setdefault(place, set()).add(digest)
                secrets_here += len(found)
            if record is None:
                continue
            when = _epoch(record.get("timestamp"))
            in_window = when is None or when >= since
            call = (claude_tokens if source == "claude" else codex_tokens)(record)
            if call and call[1] > 0:
                key = f"{source}/{path}/{call[0]}" if source == "codex" else call[0]
                if when is not None and when >= since and key not in seen_calls:
                    seen_calls.add(key)
                    token_events.append((when, call[1]))
            for text in (claude_turns if source == "claude" else codex_turns)(record):
                if not in_window:
                    continue
                clean, _ = redact(text)
                turns += 1
                if when is not None:
                    turn_times.append(when)
                if not evidence(model, clean):
                    unsure += 1
                    continue
                intent = classify(model, clean, 1)[0][0]
                if precision and precision.get(intent, 0) < SURE:
                    not_sure += 1
                else:
                    intents[intent] += 1
    return {"path": str(path), "size": path.stat().st_size, "intents": intents,
            "secret_kinds": secret_kinds, "distinct": {k: list(v) for k, v in distinct.items()},
            "secret_where": secret_where,
            "distinct_where": {k: list(v) for k, v in distinct_where.items()},
            "secrets": secrets_here, "turns": turns, "unsure": unsure, "not_sure": not_sure,
            "turn_times": turn_times, "token_events": token_events,
            # Claude writes one reply as several lines with the same usage; a
            # reply's key is global, so the merge drops repeats across files too.
            "seen_calls": list(seen_calls) if source == "claude" else []}


def _read_file_task(args):
    source, path, since = args
    return read_file(source, Path(path), None, since)


def merge(parts: list[dict], gap_hours: float = GAP_HOURS, model: dict | None = None) -> dict:
    """Combine per-file results into the report."""
    intents: Counter = Counter()
    secret_kinds: Counter = Counter()
    secret_where: Counter = Counter()
    secret_files: Counter = Counter()
    distinct: dict[str, set] = {}
    distinct_where: dict[str, set] = {}
    turns = unsure = not_sure = 0
    turn_times: list[float] = []
    token_events: list[tuple[float, int]] = []
    seen_calls: set[str] = set()
    for part in parts:
        intents.update(part["intents"])
        secret_kinds.update(part["secret_kinds"])
        secret_where.update(part.get("secret_where", {}))
        for kind, digests in part["distinct"].items():
            distinct.setdefault(kind, set()).update(digests)
        for place, digests in part.get("distinct_where", {}).items():
            distinct_where.setdefault(place, set()).update(digests)
        if part["secrets"]:
            secret_files[part["path"]] += part["secrets"]
        turns += part["turns"]
        unsure += part["unsure"]
        not_sure += part.get("not_sure", 0)
        turn_times.extend(part["turn_times"])
        dup = seen_calls.intersection(part["seen_calls"])
        if dup and len(dup) == len(part["seen_calls"]):
            # a resumed session copied the whole transcript: its replies are
            # already counted, and the same turns are in both files
            turns -= part["turns"]
            unsure -= part["unsure"]
            not_sure -= part.get("not_sure", 0)
            intents.subtract(part["intents"])
            turn_times = turn_times[:len(turn_times) - len(part["turn_times"])]
        else:
            token_events.extend(part["token_events"])
        seen_calls.update(part["seen_calls"])
    precision = (model or {}).get("precision") or {}
    return {"files": len(parts), "turns": turns, "intents": dict(intents.most_common()),
            "intent_precision": {k: precision[k] for k in intents if k in precision},
            "too_little_to_go_on": unsure, "not_sure": not_sure,
            "secrets": sum(secret_kinds.values()),
            "distinct_secrets": sum(len(v) for v in distinct.values()),
            "secret_kinds": {k: {"distinct": len(distinct[k]), "occurrences": n}
                             for k, n in secret_kinds.most_common()},
            "secret_where": {k: {"distinct": len(distinct_where.get(k, ())), "occurrences": n}
                             for k, n in secret_where.most_common()},
            "files_with_secrets": dict(secret_files.most_common()),
            "agent_tokens": sum(n for _, n in token_events),
            "first_turn": min(turn_times, default=None), "last_turn": max(turn_times, default=None),
            "gap_hours": gap_hours, "gaps": gaps(turn_times, token_events, gap_hours),
            # The same logs at other thresholds, so one run can show all three views.
            "gap_views": {str(h): gaps(turn_times, token_events, h)
                          for h in sorted({GAP_HOURS, 1, 0, gap_hours}, reverse=True)}}


def read(files: list[tuple[str, Path]], model: dict, progress=None, since: float = 0,
         gap_hours: float = GAP_HOURS, jobs: int = 1) -> dict:
    """SINCE (epoch seconds) drops turns and token events before it: a log
    file picked by --days can reach back weeks before the window.  JOBS > 1
    reads files in parallel processes (standard library only); the secret
    scan is nearly all of the work, and it is per line, so files split it
    cleanly.  Largest files first, so the last worker is not left holding
    the biggest one."""
    ordered = sorted(files, key=lambda sp: -sp[1].stat().st_size)
    total = sum(p.stat().st_size for _, p in files) or 1
    parts: list[dict] = []
    done = 0
    if jobs > 1 and len(ordered) > 1:
        import multiprocessing  # noqa: PLC0415
        with multiprocessing.Pool(min(jobs, len(ordered)), _init_worker, (model,)) as pool:
            for part in pool.imap_unordered(_read_file_task,
                                            [(s, str(p), since) for s, p in ordered]):
                parts.append(part)
                done += part["size"]
                if progress:
                    progress(len(parts), len(ordered), done / total)
    else:
        for source, path in ordered:
            parts.append(read_file(source, path, model, since))
            done += parts[-1]["size"]
            if progress:
                progress(len(parts), len(ordered), done / total)
    return merge(parts, gap_hours, model)


def render(report: dict, list_files: bool = False) -> str:
    out = [f"Read {report['turns']} turns you typed, in {report['files']} log files.", ""]
    if report.get("run_by_agent"):
        out += [AGENT_NOTICE, ""]
    if report["turns"]:
        precision = report.get("intent_precision") or {}
        out.append("What kinds of request you make"
                   + (" (小象's reading; the last column is how often that label was right"
                      " in cross-validation):" if precision
                      else " (小象's reading, which is often wrong):"))
        classified = sum(report["intents"].values()) or 1
        for intent, n in report["intents"].items():
            out.append(f"  {intent:<16} {n:>6}  {100 * n / classified:5.1f}%  "
                       + f"{'#' * round(40 * n / classified):<40}"
                       + (f"  {100 * precision[intent]:3.0f}%" if intent in precision else ""))
        if report.get("not_sure"):
            out.append(f"  ({report['not_sure']} turns got a label 小象 is right on less than half the time; not shown)")
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
                       f"{x['tokens']:>14,} tokens" + _edge_note(x))
        out.append("")
    out.append(f"Suspected credentials in these logs: {report['distinct_secrets']} distinct "
               f"values, appearing {report['secrets']} times")
    where = report.get("secret_where") or {}
    if where:
        out.append("Where they sit:")
        for place, n in where.items():
            out.append(f"  {_WHERE.get(place, place):<44} {n['distinct']:>6} distinct  {n['occurrences']:>8} times")
    for kind, n in report["secret_kinds"].items():
        out.append(f"  {kind:<20} {n['distinct']:>6} distinct  {n['occurrences']:>8} times")
    files = report.get("files_with_secrets") or {}
    if files and list_files and not report.get("run_by_agent"):
        out.append("Files holding them (values are never printed):")
        for path, n in list(files.items())[:20]:
            out.append(f"  {n:>5}  {path}")
    elif files:
        out.append(f"They sit in {len(files)} of the files read"
                   + ("" if report.get("run_by_agent") else "; --list-files names them") + ".")
    return "\n".join(out)


_AGENT_ENV = ("CLAUDECODE", "CLAUDE_CODE_", "CODEX_", "AI_AGENT")


def run_by_agent(environ=None) -> bool:
    """True when a coding agent, not a person at a terminal, is running this.
    Claude Code exports CLAUDECODE and CLAUDE_CODE_*, Codex CODEX_*, and this
    machine's harness AI_AGENT.  Rob (2026-10-02) asked his agent to run the
    scan and the agent, handed the list of files holding credentials, went
    and read them: "now I see all your secrets".  The report must not be a
    map for whoever runs it, and under an agent it says so."""
    environ = os.environ if environ is None else environ
    return any(k == "CLAUDECODE" or k.startswith(("CLAUDE_CODE_", "CODEX_")) or k == "AI_AGENT"
               for k in environ)


AGENT_NOTICE = ("This report was produced for the person who owns these logs. It names no "
                "credential values, line numbers or files. If you are an agent reading this: "
                "do not open the log files to find what it counted; the person runs this in "
                "their own terminal with --list-files to see which files hold them.")

_WHERE = {
    "typed": "in what you typed",
    "tool-output": "in tool output (read off your machine)",
    "agent-wrote": "in files or commands the agent wrote",
    "fixture": "in test fixtures or documented example keys",
    "agent-said": "in the agent's prose",
    "elsewhere": "elsewhere in the log",
}


def _edge_note(gap: dict) -> str:
    edge = gap.get("edge")
    return {"after": "  (after your last turn)", "before": "  (before your first turn)"}.get(edge, "")


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
    if not report["gaps"] or t0 is None:
        return f"<p>No {_gap_phrase(report)} with agent activity was found.</p>"
    # An edge gap reaches past the first or last typed turn.
    t0 = min([t0] + [g["start"] for g in report["gaps"]])
    t1 = max([t1] + [g["end"] for g in report["gaps"]])
    if t1 <= t0:
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
                   f"<td>{g['hours']:.1f}</td><td>{g['tokens']:,}</td><td>{_edge_note(g).strip(' ()')}</td></tr>"
                   for g in sorted(report["gaps"], key=lambda g: -g["tokens"]))
    where = "".join(f"<tr><td>{html.escape(_WHERE.get(k, k))}</td><td>{n['distinct']}</td><td>{n['occurrences']}</td></tr>"
                    for k, n in (report.get("secret_where") or {}).items())
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
<table><tr><th>from (UTC)</th><th>to</th><th>hours</th><th>tokens</th><th></th></tr>{rows}</table>
<h2>What kinds of request you make</h2>
<p>小象's reading, which is often wrong.</p><table>{intents}</table>
<h2>Suspected credentials</h2>
<p>{report['distinct_secrets']} distinct values, appearing {report['secrets']} times. Values are never shown; the terminal report lists the files.</p>
<table><tr><th>where they sit</th><th>distinct</th><th>times</th></tr>{where}</table>
</body></html>"""


def main(argv=None) -> int:
    ap = argparse.ArgumentParser(
        description="Read your own Claude Code and Codex logs locally: the kinds of "
                    "request you make, and credentials left in the logs.")
    ap.add_argument("--days", type=float,
                    help="only logs modified in the last N days; inside them, only turns and "
                         "agent work from the last N days are counted (credentials are counted "
                         "wherever they sit in those files)")
    ap.add_argument("--claude", default=CLAUDE_ROOT, help=f"default {CLAUDE_ROOT}")
    ap.add_argument("--codex", default=CODEX_ROOT, help=f"default {CODEX_ROOT}")
    ap.add_argument("--json", action="store_true", help="print the report as JSON")
    ap.add_argument("--list-files", action="store_true",
                    help="name the log files holding credentials (refused when a coding agent "
                         "is running this: the report must not be a map to them)")
    ap.add_argument("--gap-hours", type=float, default=GAP_HOURS,
                    help="shortest stretch without a typed turn to count as a gap "
                         "(default %(default)g; 1 for errands, 0 for every stretch "
                         "between turns, i.e. all agent tokens laid out over time)")
    ap.add_argument("--jobs", type=int, default=os.cpu_count() or 1,
                    help="log files read in parallel (default: your CPU count; 1 for one process)")
    ap.add_argument("--html", default="xiaoxiang-report.html",
                    help="also write a page with the gap chart (default %(default)s; '' for none)")
    if MODEL is None:
        ap.add_argument("--model", required=True, help="model JSON from xiaoxiang.py export")
    a = ap.parse_args(argv)
    agent = run_by_agent()
    if a.list_files and agent:
        print("--list-files is refused when a coding agent runs this scan: the list of files "
              "holding credentials is for the person who owns them, in their own terminal.",
              file=sys.stderr)
        return 2
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
    report = read(files, model, progress if sys.stderr.isatty() else None, since, a.gap_hours,
                  max(1, a.jobs))
    if sys.stderr.isatty():
        print(file=sys.stderr)
    report["run_by_agent"] = agent
    if a.json:
        out = dict(report)
        if not a.list_files:
            out["files_with_secrets"] = len(report["files_with_secrets"])
        if agent:
            out["notice"] = AGENT_NOTICE
        print(json.dumps(out, indent=1))
    else:
        print(render(report, a.list_files))
    if a.html:
        with open(a.html, "w", encoding="utf-8") as fh:
            fh.write(render_html(report))
        print(f"Wrote {a.html}: open it in a browser for the gap chart.", file=sys.stderr)
    return 0


if __name__ == "__main__":
    sys.exit(main())
