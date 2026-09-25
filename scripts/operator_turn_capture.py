#!/usr/bin/env python3
"""Record operator turns to the named agents that this machine's Emacs missed.

Joe's turns reach an agent from whichever Emacs he is typing in, and every
one lands in futon1b's evidence store (author joe, :event "chat-turn"). Only
turns typed in the Emacs that runs session-mode get a record under
~/.emacs-graph/session-turn-analysis/, which is what the turn feed shows. This
reads the evidence store and hands each new turn to that Emacs
(session-mode-record-external-turn), so it is structured and sent for
interpretation by the same code, then sets the record's created_at to the
turn's own time.

A turn is skipped when a record with its turn_id already exists, and when it
is a park wake (its text carries "--- resumed:"), which is the harness
talking in Joe's name rather than Joe.

  operator_turn_capture.py --agents claude-12 [--since 2026-09-25T19:00:00Z] [--loop 30]
"""
import argparse, glob, json, os, re, subprocess, sys, time, urllib.request

F1B = "http://localhost:7073"
RECORDS = os.path.expanduser("~/.emacs-graph/session-turn-analysis")
STR = r'"((?:[^"\\]|\\.)*)"'
ESC = {'"': '"', "\\": "\\", "n": "\n", "t": "\t", "r": "\r"}


def unescape(s):
    out, i = [], 0
    while i < len(s):
        if s[i] == "\\" and i + 1 < len(s):
            out.append(ESC.get(s[i + 1], s[i + 1])); i += 2
        else:
            out.append(s[i]); i += 1
    return "".join(out)


def field(entry, key):
    m = re.search(r":%s\s+%s" % (re.escape(key), STR), entry)
    return unescape(m.group(1)) if m else None


def turns(limit):
    with urllib.request.urlopen(f"{F1B}/api/alpha/evidence?author=joe&limit={limit}",
                                timeout=120) as r:
        page = r.read().decode()
    for entry in re.split(r"(?=\{:evidence/)", page):
        if '"chat-turn"' not in entry or ':role "user"' not in entry:
            continue
        yield {"turn_id": field(entry, "turn-id"), "text": field(entry, "text"),
               "session": field(entry, "evidence/session-id"),
               "at": field(entry, "evidence/at")}


def known_turn_ids():
    ids = set()
    for p in glob.glob(f"{RECORDS}/turn-*.json"):
        if p.endswith((".analysis.json", ".candidates.json")):
            continue
        try:
            ids.add(json.load(open(p)).get("turn_id"))
        except (OSError, ValueError):
            pass
    return ids


def elisp_str(s):
    return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'


def capture(agents, since, limit):
    known = known_turn_ids()
    new = []
    for t in turns(limit):
        tid, text = t["turn_id"], t["text"]
        if not (tid and text and t["at"]):
            continue
        agent = tid.rsplit("-turn-", 1)[0]
        if agent not in agents or tid in known or (since and t["at"] < since):
            continue
        if "--- resumed:" in text:
            continue
        new.append((t, agent))
    for t, agent in sorted(new, key=lambda x: x[0]["at"]):
        form = "(session-mode-record-external-turn %s %s %s %s)" % (
            elisp_str(t["text"]), elisp_str(agent), elisp_str(t["session"] or ""),
            elisp_str(t["turn_id"]))
        out = subprocess.run(["emacsclient", "-e", form], capture_output=True, text=True)
        path = out.stdout.strip().strip('"')
        if out.returncode or not os.path.exists(path):
            print(f"failed {t['turn_id']}: {out.stderr.strip() or out.stdout.strip()}", file=sys.stderr)
            continue
        rec = json.load(open(path))
        rec["created_at"] = t["at"][:19] + "Z"
        rec["captured_by"] = "operator_turn_capture.py (futon1b evidence)"
        json.dump(rec, open(path, "w"), ensure_ascii=False)
        print(f"{t['turn_id']} -> {os.path.basename(path)}")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--agents", required=True, help="comma-separated agent ids")
    ap.add_argument("--since", help="ISO time; older turns are left alone")
    ap.add_argument("--limit", type=int, default=200)
    ap.add_argument("--loop", type=int, metavar="SECONDS")
    a = ap.parse_args()
    agents = set(a.agents.split(","))
    while True:
        try:
            capture(agents, a.since, a.limit)
        except Exception as e:  # a failed poll is reported and retried, not fatal
            print(f"poll failed: {e}", file=sys.stderr)
        if not a.loop:
            break
        time.sleep(a.loop)


if __name__ == "__main__":
    main()
