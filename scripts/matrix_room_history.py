#!/usr/bin/env python3
"""Print a Matrix room's message history as JSON lines, oldest first.

Reader for the futon/mfuton outer loop (holes/E-futon-mfuton-successor-requirements.md):
it reads what the homeserver holds, not what a client buffer happens to show.
Run it where a bridge bot's token lives (the bridge host), e.g.

    ssh lucy-joe python3 - ROOM_ID fuclaude [AFTER_TS_MS] < scripts/matrix_room_history.py

Only events with origin_server_ts > AFTER_TS_MS are printed, so a pass can
start from the previous pass's cursor. Reads MATRIX_HOMESERVER_URL (default
http://127.0.0.1:8008) and MATRIX_TOKEN_DIR (default ~/.config/futon3c/matrix),
token file <nick>.token. Read-only; the token never reaches argv or output.
"""
import json
import os
from pathlib import Path
import sys
import urllib.parse
import urllib.request

MAX_PAGES = 50


def main():
    room, nick = sys.argv[1], sys.argv[2]
    after = int(sys.argv[3]) if len(sys.argv) > 3 else 0
    base = os.environ.get("MATRIX_HOMESERVER_URL", "http://127.0.0.1:8008").rstrip("/")
    token_dir = Path(os.environ.get("MATRIX_TOKEN_DIR", Path.home() / ".config/futon3c/matrix"))
    token = (token_dir / f"{nick}.token").read_text().strip()

    def page(start):
        query = {"dir": "b", "limit": 100}
        if start:
            query["from"] = start
        url = (f"{base}/_matrix/client/v3/rooms/{urllib.parse.quote(room, safe='')}"
               f"/messages?{urllib.parse.urlencode(query)}")
        request = urllib.request.Request(url, headers={"Authorization": "Bearer " + token})
        with urllib.request.urlopen(request, timeout=20) as response:
            return json.load(response)

    events, start = [], None
    for _ in range(MAX_PAGES):
        result = page(start)
        chunk = result.get("chunk", [])
        events += chunk
        start = result.get("end")
        # Paging runs backwards; stop once a page reaches the cursor.
        if not chunk or not start or min(e.get("origin_server_ts", 0) for e in chunk) <= after:
            break
    for event in reversed(events):
        if event.get("type") == "m.room.message" and event.get("origin_server_ts", 0) > after:
            print(json.dumps({"ts": event["origin_server_ts"],
                              "id": event.get("event_id"),
                              "sender": event.get("sender"),
                              "body": event.get("content", {}).get("body", "")}))


if __name__ == "__main__":
    main()
