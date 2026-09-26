#!/usr/bin/env python3
"""Harvest the operator-turn back catalogue from the futon1b evidence API.

The API caps a page at 1000 entries, so we walk backwards with `before=`,
taking the oldest :evidence/at of each page as the next cursor.  Output is
one JSON object per line, newest first, written under storage/ -- run data,
not repo data.

  python3 harvest_operator_turns.py [OUT.jsonl]
"""
import json, re, sys, urllib.request

F1B = "http://localhost:7073"
OUT = sys.argv[1] if len(sys.argv) > 1 else \
    "/home/joe/code/storage/operator-turns/operator-turns.jsonl"

def get(url):
    with urllib.request.urlopen(url, timeout=180) as r:
        return r.read().decode()

# An EDN string may contain escaped quotes; consume \" as one unit.
STR = r'"((?:[^"\\]|\\.)*)"'
FIELD = lambda k: re.compile(r':%s\s+%s' % (k, STR))
AT, TEXT = FIELD("evidence/at"), FIELD("text")
TURN, SESS = FIELD("turn-id"), FIELD("evidence/session-id")
TRANSPORT, MISSION = FIELD("transport"), FIELD("clocked-mission")
ROLE, EVID = FIELD("role"), FIELD("evidence/id")

ESCAPES = {'"': '"', "\\": "\\", "n": "\n", "t": "\t", "r": "\r"}


def unescape(s):
    """Undo EDN string escapes without touching UTF-8.

    unicode_escape would round-trip the bytes through latin-1 and mojibake
    every em dash in the corpus, so the escapes are expanded by hand.
    """
    out, i = [], 0
    while i < len(s):
        c = s[i]
        if c == "\\" and i + 1 < len(s):
            nxt = s[i + 1]
            out.append(ESCAPES.get(nxt, nxt))
            i += 2
        else:
            out.append(c)
            i += 1
    return "".join(out)

def parse(page):
    for e in re.split(r"(?=\{:evidence/)", page):
        if ':event "chat-turn"' not in e:
            continue
        g = lambda rx: (rx.search(e).group(1) if rx.search(e) else None)
        yield {"id": g(EVID), "turn_id": g(TURN), "at": g(AT),
               "session": g(SESS), "transport": g(TRANSPORT),
               "role": g(ROLE), "mission": g(MISSION),
               "text": unescape(g(TEXT) or "")}

def main():
    seen, rows, cursor = set(), [], None
    while True:
        u = f"{F1B}/api/alpha/evidence?author=joe&limit=1000"
        if cursor:
            u += f"&before={cursor}"
        page = get(u)
        ats = re.findall(r':evidence/at\s+"([^"]+)"', page)
        if not ats:
            break
        new = [r for r in parse(page) if r["id"] not in seen]
        for r in new:
            seen.add(r["id"])
        rows.extend(new)
        oldest = min(ats)
        print(f"page: {len(ats)} entries, {len(new)} new turns, "
              f"oldest {oldest}, total {len(rows)}", flush=True)
        if oldest == cursor:            # no progress; the window is exhausted
            break
        cursor = oldest
    rows.sort(key=lambda r: r["at"] or "", reverse=True)
    with open(OUT, "w") as f:
        for r in rows:
            f.write(json.dumps(r) + "\n")
    print(f"wrote {len(rows)} operator turns to {OUT}")

if __name__ == "__main__":
    main()
