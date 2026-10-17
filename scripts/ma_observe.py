#!/usr/bin/env python3
"""馬: Mastra's Observer over each finished turn, written back as evidence.

The counterpart of 象-sonnet: 象 reads the operator's turn; 馬 reads the
whole exchange that followed it (the turn, the agent's narration segments,
its commits and memory calls, its final reply) and writes dated, prioritised
observations (🔴 🟡 🟢, ✅ for finished work) in Mastra's Observational
Memory format, using Mastra's own Observer prompt
(scripts/ma/observer-system-prompt.txt) and its message layout
(buildObserverPrompt).  Each observation is one evidence row
(event "observation", tag ma-observation) keyed to the operator turn's
evidence id; turn_frames.py puts it on that turn's frame and the stepper
shows it as OBSERVED.

  ma_observe.py once SESSION [--dry-run]   observe that session's finished,
                                            unobserved turns (newest MAX_BACKLOG)
  ma_observe.py watch                      loop over the agents listed in
                                            ~/code/data/minus40/ma-enabled.txt

The previous observations of the session are passed back in (Mastra's
"Previous Observations"), so each pass adds only what is new.  Experiment
(E-prove-memory-useful, Joe 2026-10-17): opt-in per agent; the aim is a
classical pass once enough observations show what to look for.
"""
import argparse
import datetime
import json
import os
import re
import subprocess
import sys
import time
import urllib.parse
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)
import turn_frames as tf  # noqa: E402

BASE = os.environ.get("FUTON3C_BASE", "http://127.0.0.1:7070")
ENABLED = os.path.expanduser(os.environ.get("MA_ENABLED_FILE", "~/code/data/minus40/ma-enabled.txt"))
PROMPT_FILE = os.path.join(HERE, "ma", "observer-system-prompt.txt")
PROMPT_VERSION = "mastra-memory-1.38.0"
ZAI_URL = "https://api.z.ai/api/coding/paas/v4/chat/completions"
MODEL = os.environ.get("MA_MODEL", "glm-5.3")
MAX_BACKLOG = 2          # a new or restarted watcher does not backfill history
WINDOW_CHARS = 40000     # cap on one turn's window sent to the model
PREVIOUS_CHARS = 12000   # cap on the previous observations passed back in
POLL_S = 20


def system_prompt():
    text = open(PROMPT_FILE, encoding="utf-8").read()
    return text.split("\n\n", 1)[1] if text.startswith("# ") else text


def is_observation(row):
    return tf.body_of(row).get("event") == "observation"


def turn_text(row):
    b = tf.body_of(row)
    return (b.get("text") or b.get("prompt") or "").strip()


def window_messages(rows, start, end):
    """The exchange after one operator turn, as (role, at, text) messages."""
    out = []
    for r in rows:
        at = tf.at_key(r)
        if at <= start or (end is not None and at >= end):
            continue
        b = tf.body_of(r)
        ev = b.get("event")
        if b.get("role") == "assistant" and (b.get("unified-text") or b.get("text")):
            out.append(("assistant", r.get("evidence/at"), (b.get("unified-text") or b.get("text")).strip()))
        elif ev == "turn-round" and (b.get("text") or b.get("calls")):
            parts = [b.get("text") or ""]
            for c in b.get("calls") or []:
                parts.append("[tool %s] %s" % (c.get("tool"), json.dumps(c.get("args"), ensure_ascii=False)[:600]))
            out.append(("assistant", r.get("evidence/at"), "\n".join(p for p in parts if p).strip()))
        elif ev == "turn-commits":
            commits = b.get("commits") or []
            lines = ["%s %s" % (c.get("sha", "")[:9], c.get("subject", "")) if isinstance(c, dict) else str(c)
                     for c in commits]
            out.append(("assistant", r.get("evidence/at"), "[commits] " + "; ".join(lines)))
        elif ev == "memory-call":
            out.append(("assistant", r.get("evidence/at"),
                        "[memory %s] %s" % (b.get("tool"), json.dumps(b.get("args"), ensure_ascii=False)[:300])))
    return out


def clock(at):
    t = datetime.datetime.fromisoformat(at.replace("Z", "+00:00")[:26] + ("+00:00" if "+" not in at[19:] and "Z" in at else ""))
    return t


def format_history(messages):
    """Mastra's layout (buildObserverPrompt): a date line, then
    'User (8:00 PM): ...' / 'Assistant (8:01 PM): ...'."""
    lines, day = [], None
    for role, at, text in messages:
        t = parse_at(at)
        d = t.strftime("%b %-d %Y")
        if d != day:
            lines.append(("\n" if day else "") + d + ":")
            day = d
        lines.append("%s (%s): %s" % ("User" if role == "user" else "Assistant",
                                      t.strftime("%-I:%M %p"), text))
    return "\n".join(lines)


def parse_at(at):
    s = (at or "").replace("Z", "")
    s = re.sub(r"(\.\d{6})\d+", r"\1", s)
    try:
        return datetime.datetime.fromisoformat(s)
    except ValueError:
        return datetime.datetime.utcnow()


def cap_window(messages):
    total = sum(len(m[2]) for m in messages)
    if total <= WINDOW_CHARS:
        return messages
    # keep the turn, the final reply and as much of the middle as fits
    out = list(messages)
    budget = WINDOW_CHARS // max(1, len(out))
    return [(r, a, t if len(t) <= budget or i in (0, len(out) - 1) else t[:budget] + " …[cut]")
            for i, (r, a, t) in enumerate(out)]


def user_prompt(history, previous):
    parts = ["## New Message History to Observe\n\n" + history + "\n\n---\n"]
    if previous:
        parts.append("## Previous Observations\n\n" + previous + "\n\n---\n\n"
                     "Do not repeat these existing observations. Your new observations will be appended to the existing observations.\n")
    parts.append("## Your Task\n\nExtract new observations from the message history above. "
                 "Do not repeat observations that are already in the previous observations. "
                 "Add your new observations in the format specified in your instructions.")
    return "\n".join(parts)


def tag(text, name):
    m = re.search(r"<%s>\s*(.*?)\s*</%s>" % (name, name), text, re.S)
    return m.group(1).strip() if m else None


def call_model(system, user):
    key = open(os.path.expanduser("~/.zai-key"), encoding="utf-8").read().strip()
    req = urllib.request.Request(ZAI_URL, method="POST", headers={
        "Content-Type": "application/json", "Authorization": "Bearer " + key},
        data=json.dumps({"model": MODEL, "temperature": 0.3, "messages": [
            {"role": "system", "content": system}, {"role": "user", "content": user}]}).encode())
    with urllib.request.urlopen(req, timeout=180) as resp:
        j = json.load(resp)
    return j["choices"][0]["message"]["content"], j.get("usage")


def post_evidence(row):
    req = urllib.request.Request(BASE + "/api/alpha/evidence", method="POST",
                                 data=json.dumps(row, ensure_ascii=False).encode("utf-8"),
                                 headers={"content-type": "application/json"})
    with urllib.request.urlopen(req, timeout=10) as resp:
        return json.load(resp)


def observation_row(session, agent, turn_row, raw, usage, seconds, n_msgs):
    now = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    return {
        "subject": {"ref/type": "session", "ref/id": session},
        "type": "coordination", "claim-type": "observation", "author": "馬",
        "origin": {"kind": "agent", "actor": "馬", "writer": "futon3c/scripts/ma_observe.py",
                   "attributed-author": "馬", "authorization": "unknown",
                   "basis": "write-time", "recorded-at": now},
        "session-id": session,
        "body": {"event": "observation", "observed-agent": agent,
                 "turn-evidence-id": turn_row.get("evidence/id"),
                 "observations": tag(raw, "observations") or raw.strip(),
                 "current-task": tag(raw, "current-task"),
                 "suggested-response": tag(raw, "suggested-response"),
                 "model": MODEL, "prompt": PROMPT_VERSION, "usage": usage,
                 "seconds": round(seconds, 1), "messages": n_msgs, "at": now},
        "tags": ["ma-observation"],
    }


def refresh_frames(session):
    """Ask the live frames cache to rebuild SESSION (no route exists yet)."""
    form = ('(futon3c.xiang.frames-cache/invalidate! "%s" '
            '(#\'futon3c.transport.http/frames-cache-opts))' % session)
    subprocess.run([os.path.join(HERE, "proof-eval.sh"), form], capture_output=True, timeout=30)


def observe_session(session, agent="", dry_run=False, log=print):
    rows = sorted(tf.fetch_evidence(session, base=BASE), key=tf.at_key)
    turns = [r for r in rows if tf.is_operator_turn(r) or tf.is_harness_turn_start(r)]
    done = {tf.body_of(r).get("turn-evidence-id") for r in rows if is_observation(r)}
    previous = "\n".join(tf.body_of(r).get("observations") or "" for r in rows if is_observation(r))
    todo = []
    for i, t in enumerate(turns):
        start = tf.at_key(t)
        end = tf.at_key(turns[i + 1]) if i + 1 < len(turns) else None
        if t.get("evidence/id") in done:
            continue
        if not any(tf.is_final_reply(r) or tf.body_of(r).get("event") == "turn-round"
                   for r in rows if tf.at_key(r) > start and (end is None or tf.at_key(r) < end)):
            continue                                   # no reply yet
        if end is None and not any(tf.is_final_reply(r) for r in rows if tf.at_key(r) > start):
            continue                                   # harness turn still running
        todo.append((t, start, end))
    written = 0
    for t, start, end in todo[-MAX_BACKLOG:]:
        msgs = [("user", t.get("evidence/at"), turn_text(t))] + window_messages(rows, start, end)
        msgs = cap_window(msgs)
        user = user_prompt(format_history(msgs), previous[-PREVIOUS_CHARS:])
        if dry_run:
            log(user[:3000])
            continue
        t0 = time.time()
        raw, usage = call_model(system_prompt(), user)
        row = observation_row(session, agent, t, raw, usage, time.time() - t0, len(msgs))
        post_evidence(row)
        previous += "\n" + row["body"]["observations"]
        written += 1
        log("馬 %s %s: %d messages, %.0fs, %s tokens" % (
            agent or session[:8], t.get("evidence/id"), len(msgs), time.time() - t0,
            (usage or {}).get("total_tokens")))
    if written:
        refresh_frames(session)
    return written


def enabled_agents():
    try:
        return [l.strip() for l in open(ENABLED, encoding="utf-8") if l.strip() and not l.startswith("#")]
    except OSError:
        return []


def session_of(agent):
    with urllib.request.urlopen(BASE + "/api/alpha/agents/" + urllib.parse.quote(agent), timeout=10) as r:
        a = json.load(r)
    a = a.get("agent", a)
    return a.get("session-id")


def watch():
    while True:
        for agent in enabled_agents():
            try:
                sid = session_of(agent)
                if sid:
                    observe_session(sid, agent)
            except Exception as exc:  # one agent's failure never stops the loop
                print("馬 %s: %s: %s" % (agent, type(exc).__name__, exc), file=sys.stderr, flush=True)
        sys.stdout.flush()
        time.sleep(POLL_S)


def main(argv=None):
    ap = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    sub = ap.add_subparsers(dest="cmd", required=True)
    o = sub.add_parser("once"); o.add_argument("session"); o.add_argument("--agent", default="")
    o.add_argument("--dry-run", action="store_true")
    sub.add_parser("watch")
    args = ap.parse_args(argv)
    if args.cmd == "once":
        print(observe_session(args.session, args.agent, args.dry_run))
    else:
        watch()
    return 0


if __name__ == "__main__":
    sys.exit(main())
