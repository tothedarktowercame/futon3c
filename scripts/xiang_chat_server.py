#!/usr/bin/env python3
"""Authenticated read proxy for the zone-local Matrix/象 chat demo.

The browser presents its Matrix access token.  This proxy asks the configured
homeserver whether that token is joined to the configured room before exposing
the read-only 象 routes.  Tokens are never logged or forwarded to futon3c.
"""
from __future__ import annotations

from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import base64
from http.cookies import SimpleCookie
import hashlib
import hmac
import json
import os
import time
import urllib.error
import urllib.parse
import urllib.request

HOMESERVER = os.environ.get("MATRIX_HOMESERVER_URL", "https://matrix.paragogy.net").rstrip("/")
DEFAULT_ROOM_ID = "!_qvu9Pec8-hw1-nsN18SA8uIChKlJPmS4f4ji3zajRw"
ROOM_IDS = frozenset(filter(None, os.environ.get(
    "MATRIX_ROOM_IDS", os.environ.get("MATRIX_ROOM_ID", DEFAULT_ROOM_ID)
).split(",")))
FUTON3C = os.environ.get("FUTON3C_BASE", "http://127.0.0.1:7070").rstrip("/")
LISTEN = os.environ.get("XIANG_CHAT_LISTEN", "127.0.0.1")
PORT = int(os.environ.get("XIANG_CHAT_PORT", "8131"))
SESSION_SECRET = os.environ.get("MARIMO_SESSION_SECRET", "").encode()
MARIMO_COOKIE = "futon_marimo_session"
MARIMO_SESSION_SECONDS = 60 * 60


def matrix_joined(token: str, room_id: str) -> bool:
    req = urllib.request.Request(
        HOMESERVER + "/_matrix/client/v3/joined_rooms",
        headers={"Authorization": "Bearer " + token, "Accept": "application/json"},
    )
    try:
        with urllib.request.urlopen(req, timeout=8) as response:
            return room_id in json.load(response).get("joined_rooms", [])
    except Exception:
        return False


def mint_marimo_session(room_id: str, now: int | None = None) -> str:
    if not SESSION_SECRET:
        raise RuntimeError("MARIMO_SESSION_SECRET is required")
    expires = (now if now is not None else int(time.time())) + MARIMO_SESSION_SECONDS
    payload = base64.urlsafe_b64encode(f"{room_id}\n{expires}".encode()).decode().rstrip("=")
    signature = hmac.new(SESSION_SECRET, payload.encode(), hashlib.sha256).hexdigest()
    return f"{payload}.{signature}"


def valid_marimo_session(cookie_header: str, now: int | None = None) -> bool:
    if not SESSION_SECRET:
        return False
    cookies = SimpleCookie()
    try:
        cookies.load(cookie_header)
        value = cookies[MARIMO_COOKIE].value
        payload, signature = value.rsplit(".", 1)
        expected = hmac.new(SESSION_SECRET, payload.encode(), hashlib.sha256).hexdigest()
        if not hmac.compare_digest(signature, expected):
            return False
        padded = payload + "=" * (-len(payload) % 4)
        room_id, expires_text = base64.urlsafe_b64decode(padded).decode().split("\n", 1)
        current = now if now is not None else int(time.time())
        return room_id in ROOM_IDS and int(expires_text) >= current
    except (KeyError, ValueError, UnicodeError):
        return False


def upstream(path: str) -> tuple[int, bytes, str]:
    req = urllib.request.Request(FUTON3C + path, headers={"Accept": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=10) as response:
            return response.status, response.read(), response.headers.get_content_type()
    except urllib.error.HTTPError as exc:
        return exc.code, exc.read(), exc.headers.get_content_type()


class Handler(BaseHTTPRequestHandler):
    server_version = "xiang-chat/1"

    def log_message(self, fmt, *args):
        # Method/path/status only; Authorization never enters the format.
        super().log_message(fmt, *args)

    def reply(self, status: int, body: bytes, content_type: str = "application/json") -> None:
        self.send_response(status)
        self.send_header("Content-Type", content_type)
        self.send_header("Cache-Control", "private, no-store")
        self.send_header("X-Content-Type-Options", "nosniff")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self):
        parsed = urllib.parse.urlsplit(self.path)
        if parsed.path != "/api/marimo/session":
            self.reply(404, b'{"ok":false,"reason":"not-found"}')
            return
        try:
            length = int(self.headers.get("Content-Length", "0"))
            request = json.loads(self.rfile.read(length))
        except (ValueError, json.JSONDecodeError):
            self.reply(400, b'{"ok":false,"reason":"invalid-json"}')
            return
        room_id = request.get("room_id", "")
        auth = self.headers.get("Authorization", "")
        token = auth[7:] if auth.startswith("Bearer ") else ""
        if room_id not in ROOM_IDS:
            self.reply(403, b'{"ok":false,"reason":"matrix-room-not-allowed"}')
            return
        if not token or not matrix_joined(token, room_id):
            self.reply(403, b'{"ok":false,"reason":"matrix-room-membership-required"}')
            return
        session = mint_marimo_session(room_id)
        self.send_response(204)
        self.send_header(
            "Set-Cookie",
            f"{MARIMO_COOKIE}={session}; Path=/marimo/; Max-Age={MARIMO_SESSION_SECONDS}; Secure; HttpOnly; SameSite=Strict",
        )
        self.send_header("Cache-Control", "private, no-store")
        self.end_headers()

    def do_GET(self):
        parsed = urllib.parse.urlsplit(self.path)
        if parsed.path == "/health":
            self.reply(200, b'{"ok":true}')
            return
        if parsed.path == "/api/marimo/check":
            if valid_marimo_session(self.headers.get("Cookie", "")):
                self.reply(204, b"")
            else:
                self.reply(401, b'{"ok":false,"reason":"matrix-room-session-required"}')
            return
        if not (parsed.path == "/api/xiang/turns" or parsed.path.startswith("/api/xiang/turns/")):
            self.reply(404, b'{"ok":false,"reason":"not-found"}')
            return
        auth = self.headers.get("Authorization", "")
        token = auth[7:] if auth.startswith("Bearer ") else ""
        room_id = self.headers.get("X-Matrix-Room", "")
        if room_id not in ROOM_IDS:
            self.reply(403, b'{"ok":false,"reason":"matrix-room-not-allowed"}')
            return
        if not token or not matrix_joined(token, room_id):
            self.reply(403, b'{"ok":false,"reason":"matrix-room-membership-required"}')
            return
        status, body, content_type = room_scoped(parsed.path, parsed.query, room_id)
        self.reply(status, body, content_type)


def room_surface(room_id: str) -> str:
    """The surface a bridge records for ROOM_ID (ngircd_bridge._xiang_surface)."""
    return f"matrix ({room_id})"


def room_scoped(path: str, query: str, room_id: str, fetch=None) -> tuple[int, bytes, str]:
    """Answer a 象 read for one verified room: only turns recorded in that room.

    Membership proves the caller may read ROOM_ID, not every session's turns,
    so the list is filtered to the room's surface (upstream and again here, in
    case upstream ignores the filter) and a turn from elsewhere reads as
    not found.  The list drops source-text: the client already has the
    messages, and it was most of the response's size."""
    fetch = fetch or upstream
    surface = room_surface(room_id)
    if path == "/api/xiang/turns":
        params = urllib.parse.parse_qs(query)
        limit = (params.get("limit") or ["300"])[0]
        target = "/api/alpha/xiang/turns?" + urllib.parse.urlencode({"limit": limit, "surface": surface})
        status, body, content_type = fetch(target)
        if status != 200:
            return status, body, content_type
        data = json.loads(body)
        data["turns"] = [{k: v for k, v in turn.items() if k != "source-text"}
                         for turn in data.get("turns", []) if turn.get("surface") == surface]
        return status, json.dumps(data).encode(), "application/json"
    turn_id = path.removeprefix("/api/xiang/turns/")
    status, body, content_type = fetch("/api/alpha/xiang/turns/" + urllib.parse.quote(turn_id, safe=""))
    if status == 200 and (json.loads(body).get("record") or {}).get("surface") != surface:
        return 404, b'{"ok":false,"reason":"record-not-found"}', "application/json"
    return status, body, content_type


def main() -> None:
    ThreadingHTTPServer((LISTEN, PORT), Handler).serve_forever()


if __name__ == "__main__":
    main()
