#!/usr/bin/env python3
"""Authenticated read proxy for the zone-local Matrix/象 chat demo.

The browser presents its Matrix access token.  This proxy asks the configured
homeserver whether that token is joined to the configured room before exposing
the read-only 象 routes.  Tokens are never logged or forwarded to futon3c.
"""
from __future__ import annotations

from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import os
import urllib.error
import urllib.parse
import urllib.request

HOMESERVER = os.environ.get("MATRIX_HOMESERVER_URL", "https://matrix.paragogy.net").rstrip("/")
ROOM_ID = os.environ.get("MATRIX_ROOM_ID", "!_qvu9Pec8-hw1-nsN18SA8uIChKlJPmS4f4ji3zajRw")
FUTON3C = os.environ.get("FUTON3C_BASE", "http://127.0.0.1:7070").rstrip("/")
LISTEN = os.environ.get("XIANG_CHAT_LISTEN", "127.0.0.1")
PORT = int(os.environ.get("XIANG_CHAT_PORT", "8131"))


def matrix_joined(token: str) -> bool:
    req = urllib.request.Request(
        HOMESERVER + "/_matrix/client/v3/joined_rooms",
        headers={"Authorization": "Bearer " + token, "Accept": "application/json"},
    )
    try:
        with urllib.request.urlopen(req, timeout=8) as response:
            return ROOM_ID in json.load(response).get("joined_rooms", [])
    except Exception:
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

    def do_GET(self):
        parsed = urllib.parse.urlsplit(self.path)
        if parsed.path == "/health":
            self.reply(200, b'{"ok":true}')
            return
        if not (parsed.path == "/api/xiang/turns" or parsed.path.startswith("/api/xiang/turns/")):
            self.reply(404, b'{"ok":false,"reason":"not-found"}')
            return
        auth = self.headers.get("Authorization", "")
        token = auth[7:] if auth.startswith("Bearer ") else ""
        if not token or not matrix_joined(token):
            self.reply(403, b'{"ok":false,"reason":"matrix-room-membership-required"}')
            return
        suffix = parsed.path.removeprefix("/api/xiang")
        target = "/api/alpha/xiang" + suffix
        if parsed.query:
            target += "?" + parsed.query
        status, body, content_type = upstream(target)
        self.reply(status, body, content_type)


def main() -> None:
    ThreadingHTTPServer((LISTEN, PORT), Handler).serve_forever()


if __name__ == "__main__":
    main()
