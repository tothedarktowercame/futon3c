#!/usr/bin/env python3
"""Agency transport and Z.AI quota gate for the APM driver.

The HTTP and WebSocket connection functions are injectable.  Tests therefore
exercise request construction and lifecycle behavior without live calls.
"""

from __future__ import annotations

import base64
import datetime as dt
import hashlib
import json
import os
import socket
import ssl
import struct
import urllib.error
import urllib.parse
import urllib.request
import uuid
from pathlib import Path
from typing import Any, Callable, Mapping


AGENT_ID = "apm-driver"
DEFAULT_AGENCY_BASE = os.environ.get("APM_DRIVER_AGENCY_BASE", "http://127.0.0.1:7070").rstrip("/")
DEFAULT_WS_URL = os.environ.get("APM_DRIVER_AGENCY_WS", "ws://127.0.0.1:7070/agency/ws")
QUOTA_URL = os.environ.get(
    "APM_FORMAL_ZAI_QUOTA_URL", "https://api.z.ai/api/monitor/usage/quota/limit"
)
MIN_AVAILABLE_PERCENT = float(os.environ.get("APM_FORMAL_ZAI_MIN_AVAILABLE", "50"))
HTTP_TIMEOUT = float(os.environ.get("APM_FORMAL_ZAI_HTTP_TIMEOUT", "15"))
LOG_PATH = Path(
    os.environ.get("APM_FORMAL_ZAI_LOG", "/home/joe/code/futon2/logs/apm-formal-zai.log")
)

Fetcher = Callable[..., Mapping[str, Any]]


class AgencyError(RuntimeError):
    """Agency request, identity, or response contract failure."""


class GateClosed(RuntimeError):
    """The fail-closed Z.AI quota gate rejected a dispatch."""


def iso_now() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat()


def log(message: str, *, path: Path = LOG_PATH) -> None:
    """Append the cron-compatible timestamped usage-gate log line."""

    line = f"{iso_now()} {message}"
    print(line, flush=True)
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("a", encoding="utf-8") as stream:
        stream.write(line + "\n")


def url_fetch(
    method: str,
    url: str,
    *,
    body: Mapping[str, Any] | None = None,
    headers: Mapping[str, str] | None = None,
    timeout: float = HTTP_TIMEOUT,
) -> Mapping[str, Any]:
    """Small stdlib JSON HTTP adapter used by all production calls."""

    request_headers = {"Accept": "application/json", **dict(headers or {})}
    encoded = None
    if body is not None:
        encoded = json.dumps(dict(body)).encode("utf-8")
        request_headers.setdefault("Content-Type", "application/json")
    request = urllib.request.Request(
        url,
        data=encoded,
        headers=request_headers,
        method=method.upper(),
    )
    try:
        with urllib.request.urlopen(request, timeout=timeout) as response:
            raw = response.read().decode("utf-8")
            return {"status": response.status, "body": json.loads(raw) if raw else {}}
    except urllib.error.HTTPError as exc:
        raw = exc.read().decode("utf-8", errors="replace")
        try:
            parsed = json.loads(raw) if raw else {}
        except json.JSONDecodeError:
            parsed = {"error": raw}
        return {"status": exc.code, "body": parsed}
    except (OSError, ValueError, urllib.error.URLError) as exc:
        raise AgencyError(f"request-failed method={method} url={url} error={exc}") from exc


def _response(fetcher: Fetcher, method: str, url: str, **kwargs: Any) -> tuple[int, dict[str, Any]]:
    response = fetcher(method, url, **kwargs)
    status = response.get("status")
    body = response.get("body")
    if not isinstance(status, int) or not isinstance(body, dict):
        raise AgencyError(f"malformed HTTP adapter response: {response!r}")
    return status, body


class WebSocketConnection:
    """Minimal RFC 6455 text client sufficient for Agency readiness."""

    def __init__(self, stream: socket.socket, initial: bytes = b""):
        self._stream = stream
        self._buffer = bytearray(initial)
        self.closed = False

    @classmethod
    def connect(cls, url: str, *, timeout: float = HTTP_TIMEOUT) -> "WebSocketConnection":
        parsed = urllib.parse.urlsplit(url)
        if parsed.scheme not in {"ws", "wss"} or not parsed.hostname:
            raise AgencyError(f"invalid websocket URL: {url}")
        port = parsed.port or (443 if parsed.scheme == "wss" else 80)
        stream = socket.create_connection((parsed.hostname, port), timeout=timeout)
        if parsed.scheme == "wss":
            stream = ssl.create_default_context().wrap_socket(stream, server_hostname=parsed.hostname)
        key = base64.b64encode(os.urandom(16)).decode("ascii")
        target = parsed.path or "/"
        if parsed.query:
            target += "?" + parsed.query
        host = parsed.hostname if parsed.port is None else f"{parsed.hostname}:{parsed.port}"
        request = (
            f"GET {target} HTTP/1.1\r\n"
            f"Host: {host}\r\n"
            "Upgrade: websocket\r\n"
            "Connection: Upgrade\r\n"
            f"Sec-WebSocket-Key: {key}\r\n"
            "Sec-WebSocket-Version: 13\r\n\r\n"
        ).encode("ascii")
        stream.sendall(request)
        received = bytearray()
        while b"\r\n\r\n" not in received:
            chunk = stream.recv(4096)
            if not chunk:
                stream.close()
                raise AgencyError("websocket handshake closed before headers")
            received.extend(chunk)
            if len(received) > 65536:
                stream.close()
                raise AgencyError("websocket handshake headers too large")
        header_bytes, initial = bytes(received).split(b"\r\n\r\n", 1)
        lines = header_bytes.decode("iso-8859-1").split("\r\n")
        if " 101 " not in f" {lines[0]} ":
            stream.close()
            raise AgencyError(f"websocket upgrade rejected: {lines[0]}")
        response_headers = {}
        for line in lines[1:]:
            if ":" in line:
                name, value = line.split(":", 1)
                response_headers[name.strip().lower()] = value.strip()
        expected = base64.b64encode(
            hashlib.sha1((key + "258EAFA5-E914-47DA-95CA-C5AB0DC85B11").encode("ascii")).digest()
        ).decode("ascii")
        if response_headers.get("sec-websocket-accept") != expected:
            stream.close()
            raise AgencyError("websocket Sec-WebSocket-Accept mismatch")
        return cls(stream, initial)

    def _read_exact(self, length: int) -> bytes:
        while len(self._buffer) < length:
            chunk = self._stream.recv(max(4096, length - len(self._buffer)))
            if not chunk:
                raise AgencyError("websocket closed while receiving a frame")
            self._buffer.extend(chunk)
        result = bytes(self._buffer[:length])
        del self._buffer[:length]
        return result

    def _send_frame(self, opcode: int, payload: bytes) -> None:
        mask = os.urandom(4)
        length = len(payload)
        header = bytearray([0x80 | opcode])
        if length < 126:
            header.append(0x80 | length)
        elif length < 65536:
            header.append(0x80 | 126)
            header.extend(struct.pack("!H", length))
        else:
            header.append(0x80 | 127)
            header.extend(struct.pack("!Q", length))
        header.extend(mask)
        masked = bytes(value ^ mask[index % 4] for index, value in enumerate(payload))
        self._stream.sendall(bytes(header) + masked)

    def send_json(self, value: Mapping[str, Any]) -> None:
        self._send_frame(0x1, json.dumps(dict(value), separators=(",", ":")).encode("utf-8"))

    def receive_json(self) -> dict[str, Any]:
        while True:
            first, second = self._read_exact(2)
            opcode = first & 0x0F
            masked = bool(second & 0x80)
            length = second & 0x7F
            if length == 126:
                length = struct.unpack("!H", self._read_exact(2))[0]
            elif length == 127:
                length = struct.unpack("!Q", self._read_exact(8))[0]
            mask = self._read_exact(4) if masked else b""
            payload = self._read_exact(length)
            if masked:
                payload = bytes(value ^ mask[index % 4] for index, value in enumerate(payload))
            if opcode == 0x9:
                self._send_frame(0xA, payload)
                continue
            if opcode == 0x8:
                raise AgencyError("websocket closed before readiness acknowledgement")
            if opcode != 0x1:
                continue
            parsed = json.loads(payload.decode("utf-8"))
            if not isinstance(parsed, dict):
                raise AgencyError("websocket text frame was not a JSON object")
            return parsed

    def close(self) -> None:
        if self.closed:
            return
        try:
            self._send_frame(0x8, struct.pack("!H", 1000))
        except OSError:
            pass
        finally:
            self.closed = True
            self._stream.close()


def connect_identity(
    ws_url: str,
    agent_id: str,
    session_id: str,
    timeout: float = HTTP_TIMEOUT,
) -> WebSocketConnection:
    """Connect and complete Agency's typed WebSocket readiness handshake."""

    parsed = urllib.parse.urlsplit(ws_url)
    query = urllib.parse.parse_qsl(parsed.query, keep_blank_values=True)
    query.extend([("agent-id", agent_id), ("session-id", session_id)])
    target = urllib.parse.urlunsplit(parsed._replace(query=urllib.parse.urlencode(query)))
    connection = WebSocketConnection.connect(target, timeout=timeout)
    connection.send_json(
        {"type": "ready", "agent_id": agent_id, "session_id": session_id}
    )
    acknowledgement = connection.receive_json()
    if acknowledgement.get("type") not in {"ready_ack", "ready-ack"}:
        connection.close()
        raise AgencyError(f"unexpected websocket readiness response: {acknowledgement!r}")
    return connection


class AgencyIdentity:
    """Own the exact ``apm-driver`` registry and WebSocket lifecycle."""

    def __init__(
        self,
        *,
        base_url: str = DEFAULT_AGENCY_BASE,
        ws_url: str = DEFAULT_WS_URL,
        fetcher: Fetcher = url_fetch,
        connection_factory: Callable[..., Any] = connect_identity,
        session_id: str | None = None,
        timeout: float = HTTP_TIMEOUT,
    ):
        self.base_url = base_url.rstrip("/")
        self.ws_url = ws_url
        self.fetcher = fetcher
        self.connection_factory = connection_factory
        self.session_id = session_id or f"apm-driver-{uuid.uuid4()}"
        self.timeout = timeout
        self.connection: Any | None = None

    def _roster(self) -> dict[str, Any]:
        status, body = _response(
            self.fetcher,
            "GET",
            f"{self.base_url}/api/alpha/agents",
            timeout=self.timeout,
        )
        if status != 200 or body.get("ok") is not True or not isinstance(body.get("agents"), dict):
            raise AgencyError(f"malformed agent roster status={status} body={body!r}")
        return body

    def start(self) -> "AgencyIdentity":
        """Register, reclaim only a stale self, connect, and verify presence."""

        roster = self._roster()
        agents = roster["agents"]
        # Liveness signal: the roster has no ws-connected field (live-smoke
        # finding 2026-08-04); a live self shows activity on its entry.
        entry = agents.get(AGENT_ID) or {}
        if AGENT_ID in agents and (
            entry.get("status") == "invoking" or entry.get("running-jobs")
        ):
            raise AgencyError("apm-driver appears live; refusing identity claim-jump")
        if AGENT_ID in agents:
            status, _body = _response(
                self.fetcher,
                "DELETE",
                f"{self.base_url}/api/alpha/agents/{urllib.parse.quote(AGENT_ID)}",
                timeout=self.timeout,
            )
            if status not in {200, 404}:
                raise AgencyError(f"could not reclaim stale apm-driver registration: HTTP {status}")

        registration = {
            "agent-id": AGENT_ID,
            "type": "peripheral",
            "ws-bridge": True,
            "capabilities": [],
        }
        status, body = _response(
            self.fetcher,
            "POST",
            f"{self.base_url}/api/alpha/agents",
            body=registration,
            timeout=self.timeout,
        )
        if status not in {200, 201} or body.get("ok") is not True:
            raise AgencyError(f"apm-driver registration failed status={status} body={body!r}")
        try:
            self.connection = self.connection_factory(
                self.ws_url, AGENT_ID, self.session_id, self.timeout
            )
            verified = self._roster()
            # WS liveness is proven by the completed readiness handshake in
            # connect_identity (it raises otherwise); the roster exposes no
            # ws-connected field, so verify registration only.
            if AGENT_ID not in verified["agents"]:
                raise AgencyError("apm-driver failed roster verification")
        except Exception:
            if self.connection is not None:
                self.connection.close()
                self.connection = None
            _response(
                self.fetcher,
                "DELETE",
                f"{self.base_url}/api/alpha/agents/{urllib.parse.quote(AGENT_ID)}",
                timeout=self.timeout,
            )
            raise
        return self

    def close(self) -> None:
        """Close WS, deregister only ``apm-driver``, and verify absence."""

        if self.connection is not None:
            self.connection.close()
            self.connection = None
        status, _body = _response(
            self.fetcher,
            "DELETE",
            f"{self.base_url}/api/alpha/agents/{urllib.parse.quote(AGENT_ID)}",
            timeout=self.timeout,
        )
        if status not in {200, 404}:
            raise AgencyError(f"apm-driver deregistration failed: HTTP {status}")
        if AGENT_ID in self._roster()["agents"]:
            raise AgencyError("apm-driver remained in roster after deregistration")

    def __enter__(self) -> "AgencyIdentity":
        return self.start()

    def __exit__(self, _type: Any, _value: Any, _traceback: Any) -> None:
        self.close()


def _argument(
    args: tuple[Any, ...], kwargs: dict[str, Any], position: int, name: str
) -> Any:
    if position < len(args):
        if name in kwargs:
            raise TypeError(f"{name} supplied both positionally and by keyword")
        return args[position]
    if name not in kwargs:
        raise TypeError(f"missing required argument: {name}")
    return kwargs.pop(name)


def dispatch_fn(*_args: Any, **_kwargs: Any) -> Mapping[str, Any]:
    """H1-compatible injection: dispatch a packet and return job metadata."""

    kwargs = dict(_kwargs)
    target_seat = _argument(_args, kwargs, 0, "target_seat")
    packet_text = _argument(_args, kwargs, 1, "packet_text")
    fetcher = kwargs.pop("fetcher", url_fetch)
    base_url = str(kwargs.pop("base_url", DEFAULT_AGENCY_BASE)).rstrip("/")
    timeout = float(kwargs.pop("timeout", HTTP_TIMEOUT))
    if len(_args) > 2 or kwargs:
        raise TypeError(f"unexpected dispatch_fn arguments: {sorted(kwargs)}")
    request = {
        "from": AGENT_ID,
        "to": str(target_seat),
        "body": str(packet_text),
        "mode": "work",
        "caller": AGENT_ID,
        "agent-id": str(target_seat),
        "prompt": str(packet_text),
        "surface": "bell",
    }
    status, response = _response(
        fetcher,
        "POST",
        f"{base_url}/api/alpha/bell",
        body=request,
        timeout=timeout,
    )
    job_id = response.get("job-id") or response.get("job_id")
    if status != 202 or not job_id:
        raise AgencyError(f"dispatch failed status={status} response={response!r}")
    return {"job-id": str(job_id), "request": request}


def _result_text(job: Mapping[str, Any]) -> str | None:
    value = job.get("result")
    if value is None:
        value = job.get("terminal-message") or job.get("result-summary") or job.get("error")
    if value is None:
        return None
    return value if isinstance(value, str) else json.dumps(value, sort_keys=True)


def poll_fn(*_args: Any, **_kwargs: Any) -> Mapping[str, Any]:
    """H1-compatible injection: poll one explicit job id, with soft overrun."""

    kwargs = dict(_kwargs)
    job_id = _argument(_args, kwargs, 0, "job_id")
    fetcher = kwargs.pop("fetcher", url_fetch)
    base_url = str(kwargs.pop("base_url", DEFAULT_AGENCY_BASE)).rstrip("/")
    timeout = float(kwargs.pop("timeout", HTTP_TIMEOUT))
    if len(_args) > 1 or kwargs:
        raise TypeError(f"unexpected poll_fn arguments: {sorted(kwargs)}")
    status, response = _response(
        fetcher,
        "GET",
        f"{base_url}/api/alpha/invoke/jobs/{urllib.parse.quote(str(job_id), safe='')}",
        timeout=timeout,
    )
    if status != 200 or not isinstance(response.get("job"), dict):
        raise AgencyError(f"poll failed status={status} response={response!r}")
    job = response["job"]
    raw_state = str(job.get("state", "error")).lower()
    mapped = {
        "queued": "queued",
        "running": "running",
        "overrun": "running",
        "done": "done",
        "completed": "done",
        "failed": "error",
        "error": "error",
        "timed-out": "error",
        "timeout": "error",
        "cancelled": "cancelled",
        "canceled": "cancelled",
    }.get(raw_state, "error")
    return {"status": mapped, "result": _result_text(job)}


def api_key() -> str:
    key = os.environ.get("ZAI_API_KEY", "").strip()
    if key:
        return key
    for path in (Path.home() / ".zaikey", Path.home() / ".zai-key"):
        try:
            key = path.read_text(encoding="utf-8").strip()
        except OSError:
            continue
        if key:
            return key
    raise GateClosed("usage-unavailable missing ZAI_API_KEY/~/.zaikey/~/.zai-key")


def quota_snapshot(obj: Mapping[str, Any]) -> list[dict[str, float | int]]:
    """Faithful normalization from ``apm_formal_zai_cron.py``."""

    if obj.get("success") is not True:
        raise GateClosed(f"usage-unavailable unsuccessful-response code={obj.get('code')}")
    limits = (obj.get("data") or {}).get("limits") or []
    token_limits = []
    for item in limits:
        if item.get("type") != "TOKENS_LIMIT":
            continue
        percentage = item.get("percentage")
        if not isinstance(percentage, (int, float)):
            raise GateClosed("usage-unavailable token-limit-without-percentage")
        token_limits.append(
            {
                "unit": int(item.get("unit", -1)),
                "number": int(item.get("number", -1)),
                "used": float(percentage),
                "available": 100.0 - float(percentage),
                "next_reset_ms": int(item.get("nextResetTime", 0)),
            }
        )
    if not token_limits:
        raise GateClosed("usage-unavailable no-token-limits")
    return token_limits


def enforce_quota(
    token_limits: list[dict[str, float | int]],
    *,
    logger: Callable[[str], None] = log,
) -> None:
    """Enforce the cron's strict >50%-available threshold and log format."""

    blocked = [
        limit
        for limit in token_limits
        if float(limit["available"]) <= MIN_AVAILABLE_PERCENT
    ]
    summary = ",".join(
        f"unit={item['unit']}/number={item['number']}/used={item['used']:g}/available={item['available']:g}"
        for item in token_limits
    )
    if blocked:
        raise GateClosed(
            f"usage-gate-closed min-available={MIN_AVAILABLE_PERCENT:g} limits={summary}"
        )
    logger(f"usage-gate-open min-available={MIN_AVAILABLE_PERCENT:g} limits={summary}")


def fetch_and_enforce_quota(
    *,
    fetcher: Fetcher = url_fetch,
    logger: Callable[[str], None] = log,
    key: str | None = None,
    url: str = QUOTA_URL,
    timeout: float = HTTP_TIMEOUT,
) -> list[dict[str, float | int]]:
    """Fetch, normalize, log, and enforce the production Z.AI quota gate."""

    try:
        status, body = _response(
            fetcher,
            "GET",
            url,
            headers={
                "Authorization": key or api_key(),
                "Accept-Language": "en-US,en",
                "Content-Type": "application/json",
            },
            timeout=timeout,
        )
    except AgencyError as exc:
        raise GateClosed(f"request-failed url={url} error={exc}") from exc
    if status != 200:
        raise GateClosed(f"request-failed url={url} status={status}")
    limits = quota_snapshot(body)
    enforce_quota(limits, logger=logger)
    return limits
