#!/usr/bin/env python3
"""Reap stale agents from Agency: deregister on the JVM and kill their Emacs buffers.

An agent's live footprint is in two places:
  - the Agency registry (JVM, port 7070) — cleared via DELETE /api/alpha/agents/:id
  - the Emacs `server` (its `*<type>-repl:<id>*` REPL buffer AND paired
    `*invoke: <id>*` buffer) — killed via emacsclient

This finds agents idle for >= THRESHOLD hours and cleans up both. Agents currently
`invoking`, federation proxies, restored entries, and this session itself
(--self), are never reaped.

Usage:
  reap_idle_agents.py                 # dry-run: list what WOULD be reaped
  reap_idle_agents.py --reap          # actually reap
  reap_idle_agents.py --hours 48      # different idle threshold (default 24)
  reap_idle_agents.py --self claude-9 # protect a specific id (defaults to $FUTON_AGENT_ID)
  reap_idle_agents.py --json          # emit the same decisions as JSON
  reap_idle_agents.py --self-test     # run deterministic decision tests
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


def is_proxy(agent):
    """Return true only for Agency's explicit federation-proxy marker."""
    return (agent.get("metadata") or {}).get("proxy?") is True


def classify_agents(roster, ref, threshold, self_id=None):
    """Return one decision record per roster entry.

    Protection checks precede the idle threshold.  The summary counts proxy and
    restored properties independently, so an entry carrying both is visible in
    both census counts even though its per-entry reason is the first applicable
    protection.
    """
    decisions = []
    for aid, agent in roster.items():
        hrs = idle_hours(agent, ref)
        status = agent.get("status")
        if aid == self_id:
            decision, reason = "skip", "self"
        elif is_proxy(agent):
            decision, reason = "skip", "proxy"
        elif status == "restored":
            decision, reason = "skip", "restored"
        elif status == "invoking":
            decision, reason = "skip", "invoking"
        elif hrs is None:
            decision, reason = "keep", "last-active-unavailable"
        elif hrs >= threshold:
            decision, reason = "reap", "idle-threshold"
        else:
            decision, reason = "keep", "below-threshold"
        decisions.append({
            "id": aid,
            "idle_hours": None if hrs is None else round(hrs, 6),
            "decision": decision,
            "reason": reason,
        })
    return sorted(decisions, key=lambda row: row["id"])


def decision_summary(roster, decisions):
    return {
        "agents": len(decisions),
        "reap": sum(row["decision"] == "reap" for row in decisions),
        "proxies": sum(is_proxy(agent) for agent in roster.values()),
        "restored": sum(agent.get("status") == "restored"
                        for agent in roster.values()),
        "invoking": sum(agent.get("status") == "invoking"
                        for agent in roster.values()),
        "self": sum(row["reason"] == "self" for row in decisions),
    }


def json_report(roster, decisions, threshold, reap):
    return json.dumps({
        "mode": "reap" if reap else "dry-run",
        "threshold_hours": threshold,
        "summary": decision_summary(roster, decisions),
        "decisions": decisions,
    }, sort_keys=True)


def human_report(roster, decisions, threshold, reap):
    summary = decision_summary(roster, decisions)
    mode = "REAPING" if reap else "DRY-RUN (pass --reap to execute)"
    lines = [
        f"{mode} — threshold {threshold}h, {summary['reap']} candidate(s):",
        (f"Skipped protections: {summary['proxies']} proxies, "
         f"{summary['restored']} restored, {summary['invoking']} invoking, "
         f"{summary['self']} self."),
        "",
    ]
    by_id = {row["id"]: row for row in decisions}
    candidates = []
    for aid, agent in roster.items():
        row = by_id[aid]
        if row["decision"] == "reap":
            candidates.append((row["idle_hours"], aid, agent))
    candidates.sort(reverse=True)
    for hrs, aid, agent in candidates:
        socket = emacs_socket(agent)
        agent_type = agent.get("type") or "claude"
        lines.append(
            f"  {aid:14} idle {hrs:5.1f}h  type={agent_type}  "
            f"emacs={socket or '-'}  decision=reap reason=idle-threshold")
    if not candidates:
        lines.append("No agents meet the reap threshold.")
    return "\n".join(lines)


def self_test():
    ref = datetime.datetime(2026, 9, 1, tzinfo=datetime.timezone.utc)

    def agent(hours, status="idle", proxy=False):
        return {
            "last-active": (ref - datetime.timedelta(hours=hours)).isoformat(),
            "status": status,
            "metadata": {"proxy?": proxy},
        }

    roster = {
        "proxy-old": agent(300, proxy=True),
        "restored-old": agent(300, status="restored"),
        "invoking-old": agent(300, status="invoking"),
        "local-old": agent(25),
        "local-recent": agent(23),
        "self-old": agent(300),
    }
    decisions = classify_agents(roster, ref, 24.0, "self-old")
    by_id = {row["id"]: row for row in decisions}
    expected = {
        "proxy-old": ("skip", "proxy"),
        "restored-old": ("skip", "restored"),
        "invoking-old": ("skip", "invoking"),
        "local-old": ("reap", "idle-threshold"),
        "local-recent": ("keep", "below-threshold"),
        "self-old": ("skip", "self"),
    }
    for aid, pair in expected.items():
        actual = (by_id[aid]["decision"], by_id[aid]["reason"])
        assert actual == pair, (aid, actual, pair)
    encoded = json_report(roster, decisions, 24.0, False)
    decoded = json.loads(encoded)
    assert decoded["decisions"] == decisions
    human = human_report(roster, decisions, 24.0, False)
    for row in decisions:
        if row["decision"] == "reap":
            assert row["id"] in human
    print("self-test: PASS (proxy, restored, invoking, threshold, self, JSON)")


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
    ap.add_argument("--hours", type=float, default=24.0,
                    help="idle threshold in hours (default 24)")
    ap.add_argument("--reap", action="store_true",
                    help="actually reap (default is dry-run)")
    ap.add_argument("--self", default=os.environ.get("FUTON_AGENT_ID"),
                    help="agent id to protect from reaping (this session)")
    ap.add_argument("--json", action="store_true",
                    help="emit all decisions as machine-readable JSON")
    ap.add_argument("--self-test", action="store_true",
                    help="run deterministic decision tests without Agency")
    args = ap.parse_args()

    if args.self_test:
        self_test()
        return

    ref = now_utc()
    roster = fetch_roster()
    decisions = classify_agents(roster, ref, args.hours, args.self)
    if args.json:
        print(json_report(roster, decisions, args.hours, args.reap))
    else:
        print(human_report(roster, decisions, args.hours, args.reap))

    if not args.reap:
        return

    by_id = {row["id"]: row for row in decisions}
    candidates = [(row["idle_hours"], aid, agent)
                  for aid, agent in roster.items()
                  if (row := by_id[aid])["decision"] == "reap"]
    candidates.sort(reverse=True)
    for hrs, aid, a in candidates:
        sock = emacs_socket(a)
        atype = a.get("type") or "claude"
        line = f"  {aid:14} idle {hrs:5.1f}h  type={atype}  emacs={sock or '-'}"
        results = []
        if sock:
            results.extend(kill_emacs_buffers(aid, sock, atype))
        results.append(deregister(aid))
        print(f"{line}  ->  {', '.join(results)}")


if __name__ == "__main__":
    main()
