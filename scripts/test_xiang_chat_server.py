import io
import json
from unittest.mock import patch
import urllib.error

import xiang_chat_server as chat


class Response(io.BytesIO):
    status = 200
    class Headers:
        @staticmethod
        def get_content_type():
            return "application/json"
    headers = Headers()

    def __enter__(self):
        return self

    def __exit__(self, *args):
        return False


def test_membership_uses_bearer_and_exact_room(monkeypatch):
    seen = []

    def open_(request, timeout=None):
        seen.append(request)
        return Response(json.dumps({"joined_rooms": [chat.ROOM_ID]}).encode())

    monkeypatch.setattr(chat.urllib.request, "urlopen", open_)
    assert chat.matrix_joined("secret-token") is True
    assert seen[0].get_header("Authorization") == "Bearer secret-token"


def test_membership_fails_closed(monkeypatch):
    monkeypatch.setattr(chat.urllib.request, "urlopen", lambda *a, **k: (_ for _ in ()).throw(TimeoutError()))
    assert chat.matrix_joined("secret-token") is False


def test_upstream_is_read_only_internal_route(monkeypatch):
    seen = []

    def open_(request, timeout=None):
        seen.append(request.full_url)
        return Response(b'{"ok":true}')

    monkeypatch.setattr(chat.urllib.request, "urlopen", open_)
    status, body, _ = chat.upstream("/api/alpha/xiang/turns?limit=2")
    assert status == 200
    assert body == b'{"ok":true}'
    assert seen == ["http://127.0.0.1:7070/api/alpha/xiang/turns?limit=2"]
