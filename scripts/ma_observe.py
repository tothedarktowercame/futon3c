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
MAX_BACKLOG = 2          # a session with no observation yet starts at its newest turns
WINDOW_CHARS = 40000     # cap on one turn's window sent to the model
PREVIOUS_CHARS = 12000   # cap on the previous observations passed back in
POLL_S = 20


CARD_INSTRUCTION = """Also maintain the AGENT CARD: the agent's current state, rewritten each time, not appended to.
After </observations>, output <agent-card>{json}</agent-card> with exactly these keys:
  "purpose": one sentence, what the agent is trying to achieve now, in its own terms;
  "packet_vs_purpose": "" when the agent is doing what it was asked, else one sentence on the difference;
  "open_threads": list of short strings, things started and not finished (asks awaiting the user, bells awaiting replies, claims not yet checked);
  "repair": list of short strings, recent failures, mistakes or degraded capability, with what was done about each;
  "ramifications": list of short strings, consequences of this work for other missions, agents or parts of the codebase.
You will be given the previous card. Copy every item that still holds VERBATIM (exact same words), drop items that no longer hold, add new ones. At most 6 items per list."""


HARNESS_INSTRUCTION = """The first User message in the history is a TASK PACKET that the assistant will receive again, word for word, on every attempt. Do NOT restate it: no rules, file paths, commands, tool hints, checker conditions or reply format, and no "User assigned task ..." observation beyond one short line naming the task. Observe only what the assistant itself derived, tried and learned in this turn, how far it got, and exactly what remains. Keep mathematical and code content precise, with the formulas: definitions and maps it constructed, identities it verified, lemma names that closed goals, routes that dead-ended and why."""


def system_prompt(harness=False):
    with open(PROMPT_FILE, encoding="utf-8") as fh:
        text = fh.read()
    text = text.split("\n\n", 1)[1] if text.startswith("# ") else text
    # buildObserverSystemPrompt(false, instruction): Mastra's own slot
    extra = CARD_INSTRUCTION + ("\n\n" + HARNESS_INSTRUCTION if harness else "")
    return text + "\n\n=== CUSTOM INSTRUCTIONS ===\n\n" + extra


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


def user_prompt(history, previous, previous_card=None):
    parts = ["## New Message History to Observe\n\n" + history + "\n\n---\n"]
    if previous_card is not None:
        parts.append("## Previous Agent Card\n\n" + json.dumps(previous_card, ensure_ascii=False, indent=1) + "\n\n---\n")
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


LLM_KEYS = ("purpose", "packet_vs_purpose", "open_threads", "repair", "ramifications")


def parse_card(raw):
    text = tag(raw, "agent-card")
    if not text:
        return None
    text = re.sub(r"^```(?:json)?|```$", "", text.strip()).strip()
    try:
        card = json.loads(text)
    except ValueError:
        return None
    return {k: card.get(k) for k in LLM_KEYS} if isinstance(card, dict) else None


def get_json(path, timeout=10):
    with urllib.request.urlopen(BASE + path, timeout=timeout) as r:
        return json.load(r)


def clock_of(agent):
    """The work the agent is clocked on; the roster's mission-id fills in
    the mission when the clock names only an excursion."""
    out = {}
    try:
        c = get_json("/api/alpha/agent-clock?agent-id=" + urllib.parse.quote(agent))
        out = {k: c.get(k) for k in ("campaign-id", "mission-id", "excursion-id", "ticket-id") if c.get(k)}
    except Exception:
        pass
    if "mission-id" not in out:
        try:
            a = get_json("/api/alpha/agents/" + urllib.parse.quote(agent))
            mid = ((a.get("agent", a).get("metadata") or {}).get("mission-id"))
            if mid:
                out["mission-id (roster)"] = mid
        except Exception:
            pass
    return out


def mission_patterns(clock):
    """Patterns substrate-2 links to the engaged mission (Cascade Live's
    mission-scope/pattern edges); excursions inherit nothing yet."""
    try:
        edges = (get_json("/api/alpha/cascade-real/graph", 30).get("patterns") or {}).get("edges") or []
    except Exception:
        return []
    ids = {re.sub(r"^[cmet]-", "", str(v).lower()) for v in clock.values()}
    return sorted({e["pattern"] for e in edges
                   if e.get("state") == "linked" and e.get("mission", "").rsplit("/", 1)[-1] in ids})


def xiang_patterns(session):
    """Patterns 象 matched in this session's turns, with counts."""
    try:
        frames = get_json("/api/alpha/xiang/frames?session=%s&limit=400" % session, 60).get("frames") or []
    except Exception:
        return {}
    counts = {}
    for f in frames:
        for m in (f.get("patterns") or {}).get("matched") or []:
            pid = m if isinstance(m, str) else (m.get("id") or m.get("pattern"))
            if pid:
                counts[pid] = counts.get(pid, 0) + 1
    return counts


def done_lines(observations):
    """Observations whose marker is ✅ (a line that only mentions ✅ is not)."""
    out = []
    for l in observations.splitlines():
        item = l.strip().lstrip("*-> ").strip()
        if item.startswith("✅"):
            out.append(item)
    return out


def held(items, previous):
    """Each item with how many passes it has been carried verbatim."""
    before = {i["text"]: i["held"] for i in (previous or [])}
    return [{"text": t, "held": before.get(t, 0) + 1} for t in (items or [])]


def build_card(agent, session, rows, new_obs, model_card, previous):
    """Tiers by settledness: SETTLED (recorded facts), STANDING (judgements
    carried across passes), FRESH (this pass)."""
    prev = previous or {}
    commits = [c for r in rows if tf.body_of(r).get("event") == "turn-commits"
               for c in (tf.body_of(r).get("commits") or [])]
    earlier = "\n".join(tf.body_of(r).get("observations") or "" for r in rows if is_observation(r))
    xp = xiang_patterns(session)
    clock = clock_of(agent) if agent else {}
    mc = model_card or {k: (prev.get("standing", {}).get(k) or {}) for k in LLM_KEYS}
    lists = {k: held(mc.get(k) if model_card else [i["text"] for i in prev.get("standing", {}).get(k) or []],
                     prev.get("standing", {}).get(k)) for k in ("open_threads", "repair", "ramifications")}
    purpose = (mc.get("purpose") if model_card else (prev.get("standing", {}).get("purpose") or {}).get("text")) or ""
    prev_purpose = prev.get("standing", {}).get("purpose") or {}
    return {
        "settled": {
            "engaged": clock,
            "done": done_lines(earlier)[-12:],
            "commits": {"count": len(commits),
                        "last": [("%s %s" % (c.get("sha", "")[:9], c.get("subject", ""))).strip()
                                 if isinstance(c, dict) else str(c) for c in commits[-5:]]},
            "mission_patterns": mission_patterns(clock) if clock else [],
        },
        "standing": {
            "purpose": {"text": purpose,
                        "held": prev_purpose.get("held", 0) + 1 if prev_purpose.get("text") == purpose else 1},
            "packet_vs_purpose": mc.get("packet_vs_purpose") if model_card else prev.get("standing", {}).get("packet_vs_purpose"),
            "xiang_patterns": sorted(([p, n] for p, n in xp.items() if n >= 2), key=lambda x: -x[1])[:8],
            **lists,
        },
        "fresh": {
            "done": done_lines(new_obs),
            "problems": [l.strip().lstrip("*- ").strip() for l in new_obs.splitlines() if "🔴" in l][:6],
        },
        "institutions": None,  # no such entity in substrate-2 yet (asked Joe, 2026-10-17)
    }


def card_row(session, agent, turn_row, card, now):
    return {
        "subject": {"ref/type": "session", "ref/id": session},
        "type": "coordination", "claim-type": "observation", "author": "馬",
        "origin": {"kind": "agent", "actor": "馬", "writer": "futon3c/scripts/ma_observe.py",
                   "attributed-author": "馬", "authorization": "unknown",
                   "basis": "write-time", "recorded-at": now},
        "session-id": session,
        "body": {"event": "agent-card", "observed-agent": agent,
                 "turn-evidence-id": turn_row.get("evidence/id"), "card": card, "at": now},
        "tags": ["ma-card"],
    }


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
    cards = [tf.body_of(r).get("card") for r in rows if tf.body_of(r).get("event") == "agent-card"]
    previous_card = cards[-1] if cards else None
    # Only turns after the newest observed one: a watcher never walks back
    # through history (2026-10-17: the per-pass cap alone let each pass take
    # the next two older turns, 9 old turns before it was stopped).
    observed_at = [tf.at_key(t) for t in turns if t.get("evidence/id") in done]
    floor = max(observed_at) if observed_at else None
    todo = []
    for i, t in enumerate(turns):
        if floor is not None and tf.at_key(t) <= floor:
            continue
        start = tf.at_key(t)
        end = tf.at_key(turns[i + 1]) if i + 1 < len(turns) else None
        if t.get("evidence/id") in done:
            continue
        if tf.is_harness_turn_start(t):
            # a Zai/Kimi turn is finished when its job is: there is no
            # final-reply row, only rounds
            if job_of(tf.body_of(t).get("dispatch-id") or "").get("state") not in ("done", "failed", "timed-out"):
                continue
        elif not any(tf.is_final_reply(r)
                     for r in rows if tf.at_key(r) > start and (end is None or tf.at_key(r) < end)):
            continue                                   # no reply yet
        todo.append((t, start, end))
    written = 0
    for t, start, end in todo[-MAX_BACKLOG:]:
        msgs = [("user", t.get("evidence/at"), turn_text(t))] + window_messages(rows, start, end)
        harness = tf.is_harness_turn_start(t)
        if harness:
            # the seat's final text is in the job record; add it unless the
            # last round already carries it
            job = job_of(tf.body_of(t).get("dispatch-id") or "")
            final = job_text(job).strip()
            if final and not any(final[-300:] in m[2] for m in msgs[1:]):
                msgs.append(("assistant", job.get("finished-at") or msgs[-1][1], final[-6000:]))
        msgs = cap_window(msgs)
        model_view = None
        if previous_card:
            st = previous_card.get("standing", {})
            model_view = {"purpose": (st.get("purpose") or {}).get("text"),
                          "packet_vs_purpose": st.get("packet_vs_purpose"),
                          **{k: [i["text"] for i in st.get(k) or []] for k in ("open_threads", "repair", "ramifications")}}
        user = user_prompt(format_history(msgs), previous[-PREVIOUS_CHARS:], model_view)
        if dry_run:
            log(user[:3000])
            continue
        t0 = time.time()
        raw, usage = call_model(system_prompt(harness), user)
        row = observation_row(session, agent, t, raw, usage, time.time() - t0, len(msgs))
        post_evidence(row)
        card = build_card(agent, session, rows, row["body"]["observations"], parse_card(raw), previous_card)
        post_evidence(card_row(session, agent, t, card, row["body"]["at"]))
        previous_card = card
        previous += "\n" + row["body"]["observations"]
        if harness:
            try:
                after_harness_turn(agent, session, t, row["body"]["observations"], rows, log)
            except Exception as exc:  # promotion never blocks observation
                log("馬 after-turn %s: %s: %s" % (agent, type(exc).__name__, exc))
        written += 1
        log("馬 %s %s: %d messages, %.0fs, %s tokens" % (
            agent or session[:8], t.get("evidence/id"), len(msgs), time.time() - t0,
            (usage or {}).get("total_tokens")))
    if written:
        refresh_frames(session)
    return written


# ---------------------------------------------------------------------------
# Harness seats (Zai, Kimi): progress promotion and auto-continue
# (zai-5's t95J03 experiment, Joe's go 2026-10-17).  A long leaf's route was
# derived four times and lost four times: it lived only in truncated
# episodes' narration.  Now a turn that ends without PROVED has 馬's
# observation asserted as a minus40 memory named for the lemma, whose first
# line carries the lemma name (inject.py's exact-name slots deliver it first
# on any packet naming the lemma); a turn that ends with no verdict at all
# is continued with the same packet, at most MAX_CONTINUE times per chain.

CONTINUE_FILE = os.path.expanduser(os.environ.get("MA_CONTINUE_FILE", "~/code/data/minus40/ma-continue.txt"))
M40_MCP = os.path.expanduser("~/code/minus40/scripts/m40_mcp.py")
AGENCY_SEND = os.path.join(HERE, "agency_send.py")
MEMORY_PROJECT = "ma"           # ~/.claude/projects/ma/memory: 馬's own memories
MAX_CONTINUE = 3
CONTINUE_WITHIN_S = 1800        # never continue a turn that finished longer ago
PROMOTE_CHARS = 1300
ENVELOPE = re.compile(r"\A\s*--- CURRENT TURN ---\n.*?^---[ \t]*$\n?", re.S | re.M)
CONT_HEAD = re.compile(r"\A\s*CONTINUATION \(auto, 馬\): episode (\d+) of (\S+?)\.(?:\s|$)")


def packet_body(prompt):
    return ENVELOPE.sub("", prompt or "", count=1).strip()


def leaf_of(packet):
    """(problem, lemma) from a leaf packet's 'P = ..., NAME = ...' line."""
    name = re.search(r"\bNAME\s*=\s*([A-Za-z][A-Za-z0-9_']*)", packet or "")
    prob = re.search(r"\bP\s*=\s*([A-Za-z0-9]+)", packet or "")
    return (prob.group(1) if prob else None, name.group(1) if name else None)


def verdict(reply):
    """'proved', 'partial' or None (the episode ended without a verdict).
    Only the end of the reply counts: narration says 'once PROVED' too."""
    tail = (reply or "")[-2000:]
    if re.search(r"\b(not|n't|never)\s+(yet\s+)?PROVED\b", tail, re.I):
        proved = False
    else:
        proved = bool(re.search(r"\bPROVED\b", tail))
    if proved:
        return "proved"
    if re.search(r"\bPARTIAL\b", tail):
        return "partial"
    return None


def job_of(dispatch_id):
    try:
        return get_json("/api/alpha/invoke/jobs/" + urllib.parse.quote(dispatch_id)).get("job") or {}
    except Exception:
        return {}


def job_text(job):
    r = job.get("result")
    return r if isinstance(r, str) else (r or {}).get("text") or ""


def progress_lines(problem, lemma, agent, dispatch, ended, observations, now):
    """A head line carrying the lemma name, then one line per observation
    (the task restatement dropped: the packet carries it)."""
    items = [re.sub(r"^\s*[*>-]+\s*", "", l).strip() for l in (observations or "").splitlines()]
    items = [i for i in items if i and not i.startswith("Date:")
             and not re.match(r"^\W*(\([^)]*\))?\s*Task\b", i)]
    head = "%s leaf (%s) progress %s (馬 from %s %s; episode ended %s):" % (
        problem or "?", lemma, now, agent, dispatch, ended)
    return [head] + [i[:PROMOTE_CHARS].replace("\n", " ") for i in items]


def memory_name(lemma):
    return re.sub(r"[^a-z0-9]+", "-", lemma.lower()).strip("-") + "-progress"


def assert_memory(name, line, hook):
    args = {"project": MEMORY_PROJECT, "name": name, "line": line, "hook": hook}
    r = subprocess.run(["python3", "-I", M40_MCP, "--call", "memory_assert", json.dumps(args, ensure_ascii=False)],
                       capture_output=True, text=True, timeout=120)
    if r.returncode:
        raise RuntimeError("memory_assert: %s" % (r.stdout or r.stderr).strip()[:300])
    return r.stdout.strip()


def continue_agents():
    try:
        return {l.strip() for l in open(CONTINUE_FILE, encoding="utf-8") if l.strip() and not l.startswith("#")}
    except OSError:
        return set()


def continuation_packet(packet, root, episode, memory):
    return ("CONTINUATION (auto, 馬): episode %d of %s. The previous episode ended without "
            "PROVED or PARTIAL. Your progress so far was banked as the minus40 memory %s, "
            "injected above: start from it, do not re-derive what it records.\n\n%s"
            % (episode, root, memory, packet))


def send_bell(agent, text):
    r = subprocess.run(["python3", AGENCY_SEND, "--from", "ma-continue", "--to", agent,
                        "--kind", "bell", "--mode", "work"], input=text, capture_output=True,
                       text=True, timeout=60)
    last = (r.stdout or "").strip().splitlines()[-1:] or ["{}"]
    return json.loads(last[0]).get("job-id")


def after_harness_turn(agent, session, turn_row, observation, rows, log=print, dry_run=False):
    """Promote and, when the episode ended with no verdict, continue."""
    b = tf.body_of(turn_row)
    dispatch = b.get("dispatch-id")
    packet = packet_body(b.get("prompt"))
    m = CONT_HEAD.match(packet)
    root, episode = (m.group(2), int(m.group(1))) if m else (dispatch, 1)
    inner = packet.split("\n\n", 1)[1] if m else packet
    problem, lemma = leaf_of(inner)
    job = job_of(dispatch) if dispatch else {}
    ended = verdict(job_text(job))
    out = {"lemma": lemma, "verdict": ended, "promoted": None, "continued": None}
    if not lemma or ended == "proved":
        return out
    now = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%MZ")
    name = memory_name(lemma)
    lines = progress_lines(problem, lemma, agent, dispatch, ended or "without a verdict", observation, now)
    if not dry_run:
        for line in lines:
            assert_memory(name, line, ("%s progress (馬, auto)" % lemma)[:79])
    out["promoted"] = "cc:%s/%s" % (MEMORY_PROJECT, name)
    log("馬 promoted %s (%s)" % (out["promoted"], ended or "no verdict"))
    if ended is None and agent in continue_agents() and episode < 1 + MAX_CONTINUE:
        finished = parse_at(job.get("finished-at") or job.get("completed-at") or turn_row.get("evidence/at"))
        latest = max((tf.at_key(r) for r in rows if tf.is_harness_turn_start(r)), default="")
        fresh = (datetime.datetime.utcnow() - finished).total_seconds() < CONTINUE_WITHIN_S
        if tf.at_key(turn_row) == latest and fresh:
            text = continuation_packet(inner, root, episode + 1, name)
            new_job = None if dry_run else send_bell(agent, text)
            out["continued"] = {"root": root, "from": dispatch, "to": new_job, "episode": episode + 1}
            if not dry_run:
                post_evidence(continue_row(session, agent, out["continued"], out["promoted"]))
            log("馬 continued %s: episode %d of %s -> %s" % (agent, episode + 1, root, new_job))
    return out


def continue_row(session, agent, cont, memory):
    now = datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
    return {"subject": {"ref/type": "session", "ref/id": session},
            "type": "coordination", "claim-type": "observation", "author": "馬",
            "origin": {"kind": "agent", "actor": "馬", "writer": "futon3c/scripts/ma_observe.py",
                       "attributed-author": "馬", "authorization": "unknown",
                       "basis": "write-time", "recorded-at": now},
            "session-id": session,
            "body": {"event": "auto-continue", "agent": agent, "root-dispatch": cont["root"],
                     "from-dispatch": cont["from"], "to-dispatch": cont["to"],
                     "episode": cont["episode"], "progress-memory": memory, "at": now},
            "tags": ["ma-continue"]}


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
