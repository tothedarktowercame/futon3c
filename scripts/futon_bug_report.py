#!/usr/bin/env python3
"""Collect a self-contained futon bug report, secrets scrubbed.

Called by `report-futon-bug' (emacs/report-futon-bug.el); usable alone:

    futon_bug_report.py --summary "what went wrong" [--emacs-context FILE]
                        [--out-dir DIR] [--minutes 30]

Prints the path of the report it wrote.  The report is meant to be read by an
agent on another host, so it carries everything inline: the operator's
summary, Emacs context, service state and journal excerpts, health checks,
and the HEAD of each futon repo, so the reader can check out the same code.

Origin: futon0/holes/missions/M-landscape-positioning.md §5 (alternative
backlog, "just bell them out"), Joe 2026-10-10.
"""
import argparse
import datetime as dt
import json
import os
import re
import socket
import subprocess
import urllib.request

HOME = os.path.expanduser("~")
CODE = os.path.join(HOME, "code")
REPOS = ["futon0", "futon1b", "futon2", "futon3", "futon3a", "futon3b",
         "futon3c", "futon4", "futon5", "futon6", "futon7"]
UNITS = ["futon3c-zone", "futon1b-zone", "emacs-graph"]
HEALTH = [("Agency :7070", "http://127.0.0.1:7070/health"),
          ("futon1b :7072", "http://127.0.0.1:7072/health"),
          ("futon1b :7073", "http://127.0.0.1:7073/health")]

# Secrets that turn up in logs, environments and buffers.  Each pattern keeps
# the key and replaces the value, so the reader still sees what was there.
SCRUB = [
    (re.compile(r"(?i)\b(bearer)\s+[A-Za-z0-9._~+/=-]{12,}"), r"\1 [REDACTED]"),
    (re.compile(r"(?i)((?:[A-Z0-9_]*(?:TOKEN|SECRET|PASSWORD|PASSWD|API_?KEY|"
                r"PRIVATE_?KEY|ACCESS_?KEY|CREDENTIAL)[A-Z0-9_]*)\s*[=:]\s*)"
                r"[\"']?[^\s\"',;]{6,}"), r"\1[REDACTED]"),
    (re.compile(r"(?i)(\"(?:access_token|token|password|secret|api_key)\"\s*:\s*\")"
                r"[^\"]{6,}"), r"\1[REDACTED]"),
    (re.compile(r"\b(?:ghp|gho|ghs|ghu|github_pat)_[A-Za-z0-9_]{20,}"), "[REDACTED-GITHUB]"),
    (re.compile(r"\bsk-[A-Za-z0-9_-]{20,}"), "[REDACTED-APIKEY]"),
    (re.compile(r"\bsyt_[A-Za-z0-9_=-]{20,}"), "[REDACTED-MATRIX]"),
    (re.compile(r"\bxox[abpr]-[A-Za-z0-9-]{10,}"), "[REDACTED-SLACK]"),
    (re.compile(r"-----BEGIN [A-Z ]*PRIVATE KEY-----.*?-----END [A-Z ]*PRIVATE KEY-----",
                re.S), "[REDACTED-PRIVATE-KEY]"),
]


def scrub(text):
    """Return TEXT with secrets replaced, and how many replacements were made."""
    n = 0
    for pat, rep in SCRUB:
        text, k = pat.subn(rep, text)
        n += k
    return text, n


def run(cmd, timeout=20):
    try:
        p = subprocess.run(cmd, capture_output=True, text=True, timeout=timeout)
        return (p.stdout + p.stderr).rstrip()
    except subprocess.TimeoutExpired:
        return f"(timed out after {timeout}s: {' '.join(cmd)})"
    except OSError as e:
        return f"(could not run {cmd[0]}: {e})"


def health():
    rows = []
    for name, url in HEALTH:
        try:
            with urllib.request.urlopen(url, timeout=4) as r:
                body = r.read(300).decode("utf-8", "replace").strip()
                rows.append(f"- {name}: HTTP {r.status} {body[:120]}")
        except Exception as e:  # report every failure mode as text
            rows.append(f"- {name}: FAILED ({e})")
    return "\n".join(rows)


def repos():
    rows = ["| repo | branch | HEAD | dirty files |", "|---|---|---|---|"]
    for r in REPOS:
        d = os.path.join(CODE, r)
        if not os.path.isdir(os.path.join(d, ".git")):
            continue
        head = run(["git", "-C", d, "log", "-1", "--format=%h %cs %s"], 5)
        branch = run(["git", "-C", d, "rev-parse", "--abbrev-ref", "HEAD"], 5)
        dirty = run(["git", "-C", d, "status", "--porcelain"], 10)
        n = len([l for l in dirty.splitlines() if l.strip()])
        rows.append(f"| {r} | {branch} | {head[:70]} | {n} |")
    return "\n".join(rows)


def services(minutes):
    out = []
    for u in UNITS:
        state = run(["systemctl", "--user", "is-active", u], 5)
        out.append(f"### {u}: {state}\n")
        log = run(["journalctl", "--user", "-u", u, "--since", f"-{minutes}min",
                   "-n", "40", "--no-pager", "-o", "short-iso"], 15)
        out.append("```\n" + (log or "(no journal lines)") + "\n```\n")
    return "\n".join(out)


def zone_health():
    script = os.path.join(CODE, "futon0", "scripts", "zone-health.bb")
    if not os.path.exists(script):
        return "(zone-health.bb not found)"
    out = run(["bb", script], 90)
    lines = [l for l in out.splitlines() if l.startswith(("PASS", "FAIL", "WARN"))]
    return "\n".join(lines) or out[-2000:]


def slug(s):
    s = re.sub(r"[^a-z0-9]+", "-", s.lower()).strip("-")
    return s[:48] or "bug"


def main():
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    ap.add_argument("--summary", required=True)
    ap.add_argument("--emacs-context", help="file of Emacs-side context (markdown)")
    ap.add_argument("--out-dir", default=os.path.join(HOME, "notes", "futon-bugs"))
    ap.add_argument("--minutes", type=int, default=30, help="journal window")
    ap.add_argument("--no-zone-health", action="store_true")
    a = ap.parse_args()

    now = dt.datetime.now(dt.timezone.utc)
    stamp = now.strftime("%Y%m%dT%H%M%SZ")
    emacs = ""
    if a.emacs_context and os.path.exists(a.emacs_context):
        with open(a.emacs_context, encoding="utf-8", errors="replace") as f:
            emacs = f.read()

    parts = [
        f"# Futon bug report {stamp}",
        "",
        f"**Summary (operator):** {a.summary}",
        "",
        f"- host: {socket.gethostname()}",
        f"- time: {now.isoformat(timespec='seconds')}",
        f"- uptime/load: {run(['uptime'], 5)}",
        "",
        "## What the reader is asked to do",
        "",
        "Triage only, unless told otherwise: reproduce if you can, locate the "
        "cause in code at the HEADs below, propose a fix, and report back what "
        "you checked. This report is self-contained; the host it came from may "
        "not be reachable from where you are.",
        "",
        "## Emacs context",
        "",
        emacs or "(none supplied)",
        "",
        "## Health",
        "",
        health(),
        "",
        "## Repos (HEAD at report time)",
        "",
        repos(),
        "",
        f"## Services and journal (last {a.minutes} min)",
        "",
        services(a.minutes),
    ]
    if not a.no_zone_health:
        parts += ["## zone-health", "", "```", zone_health(), "```", ""]

    text, n = scrub("\n".join(parts))
    text += f"\n---\nSecrets scrubbed: {n} replacement(s). Collector: futon3c/scripts/futon_bug_report.py\n"

    os.makedirs(a.out_dir, exist_ok=True)
    path = os.path.join(a.out_dir, f"BUG-{stamp}-{slug(a.summary)}.md")
    with open(path, "w", encoding="utf-8") as f:
        f.write(text)
    print(path)


if __name__ == "__main__":
    main()
