#!/usr/bin/env python3
"""Reap stale agents from Agency: deregister on the JVM and kill their Emacs buffers.

An agent's live footprint is in two places:
  - the Agency registry (JVM, port 7070) — cleared via DELETE /api/alpha/agents/:id
  - the Emacs `server` (its `*<type>-repl:<id>*` REPL buffer AND paired
    `*invoke: <id>*` buffer) — killed via emacsclient

This finds agents idle for >= THRESHOLD hours and cleans up both. Agents currently
`invoking`, and this session itself (--self), are never reaped.

Usage:
  reap_idle_agents.py                 # dry-run: list what WOULD be reaped
  reap_idle_agents.py --reap          # actually reap
  reap_idle_agents.py --hours 24      # different idle threshold (default 15)
  reap_idle_agents.py --self claude-9 # protect a specific id (defaults to $FUTON_AGENT_ID)
"""
import argparse
import datetime
import json
import os
import subprocess
import sys
import urllib.request

API = os.environ.get("FUTON3C_API_URL", "http://localhost:7070")


def now_utc():
    return datetime.datetime.now(datetime.timezone.utc)


def fetch_roster():
    with urllib.request.urlopen(f"{API}/api/alpha/agents", timeout=5) as r:
        return json.load(r)["agents"]


def idle_hours(agent, ref):
    la = agent.get("last-active")
    if not la:
        return None
    t = datetime.datetime.fromisoformat(la.replace("Z", "+00:00"))
    return (ref - t).total_seconds() / 3600.0


def emacs_socket(agent):
    return (agent.get("metadata") or {}).get("emacs-socket")


def kill_emacs_buffers(agent_id, socket, agent_type):
    """Kill the REPL buffer AND its paired *invoke: ...* buffer(s) in SOCKET's Emacs.

    Mirrors futon0/contrib/repl-reaper.el: a REPL buffer *<type>-repl:<id>* has a
    companion *invoke: <id>* (or *invoke: <type>-repl:<id>* for codex lanes). We
    kill the invoke buffers first so the count is accurate even if the REPL
    buffer's own kill-buffer-hook (when repl-reaper.el is loaded) races to do it.
    Returns a list of status strings.
    """
    repl = f"*{agent_type}-repl:{agent_id}*"
    invokes = [f"*invoke: {agent_id}*", f"*invoke: {agent_type}-repl:{agent_id}*"]
    invoke_list = " ".join(f'"{n}"' for n in invokes)
    elisp = (
        "(let ((kill-buffer-query-functions nil) (ik 0) (rk nil))"
        f'  (dolist (n (list {invoke_list}))'
        "    (when (get-buffer n) (kill-buffer n) (setq ik (1+ ik))))"
        f'  (when (get-buffer "{repl}") (kill-buffer "{repl}") (setq rk t))'
        '  (format "repl=%s invoke=%d" (if rk "killed" "absent") ik))'
    )
    try:
        out = subprocess.run(
            ["emacsclient", "-s", socket, "--eval", elisp],
            capture_output=True, text=True, timeout=10,
        )
    except (subprocess.TimeoutExpired, FileNotFoundError) as e:
        return [f"emacs-error({e.__class__.__name__})"]
    if out.returncode != 0:
        return [f"emacs-error({out.stderr.strip()[:60]})"]
    # emacsclient prints the returned string wrapped in quotes, e.g. "repl=killed invoke=1"
    body = out.stdout.strip().strip('"')
    results = []
    if "repl=killed" in body:
        results.append("buffer-killed")
    elif "repl=absent" in body:
        results.append("buffer-absent")
    else:
        return [f"emacs-unexpected({body[:40]})"]
    n_invoke = int(body.rsplit("invoke=", 1)[-1] or 0)
    if n_invoke:
        results.append(f"invoke-killed({n_invoke})")
    return results


def deregister(agent_id):
    req = urllib.request.Request(
        f"{API}/api/alpha/agents/{agent_id}", method="DELETE")
    try:
        with urllib.request.urlopen(req, timeout=5) as r:
            body = json.load(r)
            return "deregistered" if body.get("ok") else f"jvm-fail({body})"
    except urllib.error.HTTPError as e:
        return f"jvm-http-{e.code}"
    except Exception as e:  # noqa: BLE001
        return f"jvm-error({e})"


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--hours", type=float, default=15.0,
                    help="idle threshold in hours (default 15)")
    ap.add_argument("--reap", action="store_true",
                    help="actually reap (default is dry-run)")
    ap.add_argument("--self", default=os.environ.get("FUTON_AGENT_ID"),
                    help="agent id to protect from reaping (this session)")
    args = ap.parse_args()

    ref = now_utc()
    roster = fetch_roster()
    candidates = []
    for aid, a in roster.items():
        hrs = idle_hours(a, ref)
        status = a.get("status")
        if aid == args.self:
            continue
        if status == "invoking":
            continue
        if hrs is not None and hrs >= args.hours:
            candidates.append((hrs, aid, a))
    candidates.sort(reverse=True)

    if not candidates:
        print(f"No agents idle >= {args.hours}h. Nothing to reap.")
        return

    mode = "REAPING" if args.reap else "DRY-RUN (pass --reap to execute)"
    print(f"{mode} — threshold {args.hours}h, {len(candidates)} candidate(s):\n")
    for hrs, aid, a in candidates:
        sock = emacs_socket(a)
        atype = a.get("type") or "claude"
        line = f"  {aid:14} idle {hrs:5.1f}h  type={atype}  emacs={sock or '-'}"
        if not args.reap:
            print(line)
            continue
        results = []
        if sock:
            results.extend(kill_emacs_buffers(aid, sock, atype))
        results.append(deregister(aid))
        print(f"{line}  ->  {', '.join(results)}")


if __name__ == "__main__":
    main()
