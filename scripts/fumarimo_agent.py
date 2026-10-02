#!/usr/bin/env python3
"""Matrix publisher for a Marimo code cell and its linked output.

The execution boundary is deliberately absent here: callers supply reviewed
Python source and an already-uploaded MXC image.  This module owns only the
durable Matrix representation and provenance between the request, cell, and
output events.
"""
from __future__ import annotations

import html
import json
import os
from pathlib import Path
import subprocess
import time
import urllib.parse
import urllib.request
import uuid


EVENT_NAMESPACE = "org.paragogy.marimo"
# Standard msgtypes keep the two turns useful in every Matrix client. The
# namespaced payload carries the richer notebook semantics for our Element fork.
PYTHON_MSGTYPE = "m.text"
OUTPUT_MSGTYPE = "m.image"
OUTPUT_RELATION = EVENT_NAMESPACE + ".output"


def python_cell_content(source: str, request_event_id: str, cell_id: str) -> dict:
    if not source.strip():
        raise ValueError("Python cell source must not be empty")
    if not request_event_id.startswith("$"):
        raise ValueError("request_event_id must be a Matrix event ID")
    return {
        "msgtype": PYTHON_MSGTYPE,
        "body": source,
        "format": "org.matrix.custom.html",
        "formatted_body": f"<pre><code>{html.escape(source)}</code></pre>",
        "m.relates_to": {"m.in_reply_to": {"event_id": request_event_id}},
        EVENT_NAMESPACE: {
            "kind": "python-cell",
            "language": "python",
            "cell_id": cell_id,
            "source": source,
            "request_event_id": request_event_id,
            "revision": 1,
        },
    }


def image_output_content(
    image_mxc: str,
    alt: str,
    request_event_id: str,
    cell_event_id: str,
    cell_id: str,
    execution_id: str,
    mimetype: str = "image/png",
) -> dict:
    if not image_mxc.startswith("mxc://"):
        raise ValueError("image_mxc must be an MXC URI")
    if not cell_event_id.startswith("$"):
        raise ValueError("cell_event_id must be a Matrix event ID")
    return {
        "msgtype": OUTPUT_MSGTYPE,
        "body": alt,
        "url": image_mxc,
        "info": {"mimetype": mimetype},
        "m.relates_to": {
            "rel_type": OUTPUT_RELATION,
            "event_id": cell_event_id,
        },
        EVENT_NAMESPACE: {
            "kind": "image-output",
            "cell_id": cell_id,
            "cell_event_id": cell_event_id,
            "request_event_id": request_event_id,
            "execution_id": execution_id,
            "status": "ok",
        },
    }


class FumarimoPublisher:
    """Publish an ordered code/output pair through a MatrixBot transport.

    Matrix has no multi-event transaction: validation prevents known-bad output
    from orphaning a code event; a transport failure of the second send is
    reported to the caller and requires outbox reconciliation.
    """

    def __init__(self, matrix_bot):
        self.matrix_bot = matrix_bot

    def _send(self, room_id: str, content: dict) -> str:
        if room_id not in self.matrix_bot.channels:
            raise ValueError("Matrix send to unlisted room refused")
        txn = uuid.uuid4().hex
        result = self.matrix_bot._request(
            "PUT",
            "/rooms/" + self.matrix_bot.quote_room(room_id) + "/send/m.room.message/" + txn,
            content,
        )
        event_id = result.get("event_id", "")
        if not isinstance(event_id, str) or not event_id.startswith("$"):
            raise RuntimeError("Matrix send did not return an event ID")
        return event_id

    def publish(
        self,
        room_id: str,
        request_event_id: str,
        source: str,
        image_mxc: str,
        alt: str,
        *,
        cell_id: str | None = None,
        execution_id: str | None = None,
        mimetype: str = "image/png",
    ) -> tuple[str, str]:
        cell_id = cell_id or uuid.uuid4().hex
        execution_id = execution_id or uuid.uuid4().hex
        code_content = python_cell_content(source, request_event_id, cell_id)
        if not image_mxc.startswith("mxc://"):
            raise ValueError("image_mxc must be an MXC URI")
        cell_event_id = self._send(room_id, code_content)
        output_event_id = self._send(
            room_id,
            image_output_content(
                image_mxc,
                alt,
                request_event_id,
                cell_event_id,
                cell_id,
                execution_id,
                mimetype,
            ),
        )
        return cell_event_id, output_event_id


def requests_posts_chart(body: object, addressed: bool = False) -> bool:
    if not isinstance(body, str):
        return False
    text = body.lower()
    addressed = addressed or "@fumarimo" in text or text.lstrip().startswith("fumarimo:")
    asks_count = "posts per author" in text or "number of posts per author" in text
    return addressed and asks_count


def posts_chart_source(authors: list[str]) -> str:
    encoded = json.dumps(authors, ensure_ascii=False)
    return f"""from collections import Counter
import html

authors = {encoded}
counts = Counter(authors)
width, height, margin = 720, 360, 48
rows = counts.most_common()
maximum = max((count for _, count in rows), default=1)
bar_width = max(24, (width - 2 * margin) // max(len(rows), 1) - 16)
bars = []
for index, (author, count) in enumerate(rows):
    x = margin + index * ((width - 2 * margin) / max(len(rows), 1))
    bar_height = (height - 2 * margin) * count / maximum
    y = height - margin - bar_height
    label = html.escape(author.split(":", 1)[0].lstrip("@"))
    bars.append(f'<rect x="{{x:.1f}}" y="{{y:.1f}}" width="{{bar_width}}" height="{{bar_height:.1f}}" rx="3" fill="#2a78d6"/>')
    bars.append(f'<text x="{{x + bar_width / 2:.1f}}" y="{{y - 8:.1f}}" text-anchor="middle">{{count}}</text>')
    bars.append(f'<text x="{{x + bar_width / 2:.1f}}" y="{{height - 18}}" text-anchor="middle">{{label}}</text>')
_output_svg = f'''<svg xmlns="http://www.w3.org/2000/svg" width="{{width}}" height="{{height}}" viewBox="0 0 {{width}} {{height}}"><rect width="100%" height="100%" fill="white"/><g font-family="sans-serif" font-size="14" fill="#202020">{{"".join(bars)}}</g></svg>'''
"""


def execute_posts_chart(source: str) -> bytes:
    namespace: dict[str, object] = {}
    exec(compile(source, "<fumarimo-posts-chart>", "exec"), namespace)
    svg = namespace.get("_output_svg")
    if not isinstance(svg, str) or not svg.startswith("<svg"):
        raise RuntimeError("chart cell did not produce SVG")
    return svg.encode("utf-8")


def svg_to_png(svg: bytes) -> bytes:
    result = subprocess.run(
        ["/usr/bin/convert", "svg:-", "png:-"],
        input=svg,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=True,
        timeout=10,
    )
    if not result.stdout.startswith(b"\x89PNG\r\n\x1a\n"):
        raise RuntimeError("SVG conversion did not produce PNG")
    return result.stdout


class MatrixClient:
    def __init__(self, homeserver: str, token_file: Path, rooms: list[str], state_file: Path):
        self.homeserver = homeserver.rstrip("/")
        self.token = token_file.read_text().strip()
        if not self.token or any(character.isspace() for character in self.token):
            raise ValueError("missing or malformed Matrix token")
        self.channels = rooms
        self.state_file = state_file
        self.state_file.parent.mkdir(parents=True, exist_ok=True)
        self.state = {"next_batch": "", "seen": []}
        if self.state_file.exists():
            loaded = json.loads(self.state_file.read_text())
            if (
                not isinstance(loaded, dict)
                or not isinstance(loaded.get("next_batch"), str)
                or not isinstance(loaded.get("seen"), list)
            ):
                raise ValueError("invalid Fumarimo state")
            self.state = loaded
        self.mxid = ""

    @staticmethod
    def quote_room(room: str) -> str:
        return urllib.parse.quote(room, safe="")

    def _request(self, method: str, path: str, body: dict | None = None, query: dict | None = None) -> dict:
        url = self.homeserver + "/_matrix/client/v3" + path
        if query:
            url += "?" + urllib.parse.urlencode(query)
        data = None if body is None else json.dumps(body).encode()
        request = urllib.request.Request(url, data=data, method=method, headers={
            "Authorization": "Bearer " + self.token,
            "Content-Type": "application/json",
        })
        with urllib.request.urlopen(request, timeout=40) as response:
            return json.load(response)

    def upload(self, data: bytes, mimetype: str, filename: str) -> str:
        query = urllib.parse.urlencode({"filename": filename})
        url = self.homeserver + "/_matrix/media/v3/upload?" + query
        request = urllib.request.Request(url, data=data, method="POST", headers={
            "Authorization": "Bearer " + self.token,
            "Content-Type": mimetype,
        })
        with urllib.request.urlopen(request, timeout=40) as response:
            uri = json.load(response).get("content_uri", "")
        if not isinstance(uri, str) or not uri.startswith("mxc://"):
            raise RuntimeError("Matrix upload did not return an MXC URI")
        return uri

    def connect(self) -> None:
        self.mxid = self._request("GET", "/account/whoami").get("user_id", "")
        if not self.mxid.startswith("@fumarimo:"):
            raise ValueError("Matrix token is not the fumarimo account")

    def save_state(self) -> None:
        temporary = self.state_file.with_suffix(".tmp")
        temporary.write_text(json.dumps(self.state))
        os.chmod(temporary, 0o600)
        os.replace(temporary, self.state_file)


class FumarimoAgent:
    def __init__(self, client: MatrixClient):
        self.client = client
        self.publisher = FumarimoPublisher(client)

    def room_authors(self, room_id: str) -> list[str]:
        response = self.client._request(
            "GET", f"/rooms/{self.client.quote_room(room_id)}/messages", query={"dir": "b", "limit": 1000}
        )
        return [
            event["sender"] for event in reversed(response.get("chunk", []))
            if event.get("type") == "m.room.message" and isinstance(event.get("sender"), str)
        ]

    def handle_event(self, room_id: str, event: dict) -> None:
        if event.get("type") != "m.room.message" or event.get("sender") == self.client.mxid:
            return
        body = event.get("content", {}).get("body")
        mentions = event.get("content", {}).get("m.mentions", {}).get("user_ids", [])
        event_id = event.get("event_id")
        addressed = (
            self.client.mxid in mentions
            or (isinstance(body, str) and ("@fumarimo" in body.lower() or body.lower().lstrip().startswith("fumarimo:")))
        )
        if not addressed or not isinstance(event_id, str):
            return
        if not requests_posts_chart(body, addressed=True):
            self.publisher._send(room_id, {
                "msgtype": "m.notice",
                "body": "Fumarimo currently supports: ‘show me the number of posts per author’. Other datasets need an explicit data source before I can execute them.",
                EVENT_NAMESPACE: {"kind": "unsupported-request", "request_event_id": event_id},
            })
            return
        authors = self.room_authors(room_id)
        source = posts_chart_source(authors)
        png = svg_to_png(execute_posts_chart(source))
        image_mxc = self.client.upload(png, "image/png", "posts-per-author.png")
        self.publisher.publish(
            room_id,
            event_id,
            source,
            image_mxc,
            f"Posts per author across {len(authors)} room messages",
            mimetype="image/png",
        )

    def sync_once(self) -> None:
        query = {"timeout": 30000, "filter": json.dumps({"room": {"rooms": self.client.channels}})}
        if self.client.state["next_batch"]:
            query["since"] = self.client.state["next_batch"]
        response = self.client._request("GET", "/sync", query=query)
        next_batch = response.get("next_batch", "")
        if not isinstance(next_batch, str) or not next_batch:
            raise RuntimeError("Matrix sync omitted next_batch")
        for room_id in response.get("rooms", {}).get("invite", {}):
            if room_id in self.client.channels:
                self.client._request("POST", "/join/" + self.client.quote_room(room_id), {})
        if self.client.state["next_batch"]:
            for room_id, room in response.get("rooms", {}).get("join", {}).items():
                if room_id in self.client.channels:
                    for event in room.get("timeline", {}).get("events", []):
                        event_id = event.get("event_id")
                        if not isinstance(event_id, str) or event_id in self.client.state["seen"]:
                            continue
                        self.client.state["seen"] = (self.client.state["seen"] + [event_id])[-4096:]
                        self.client.save_state()
                        self.handle_event(room_id, event)
        self.client.state["next_batch"] = next_batch
        self.client.save_state()

    def run(self) -> None:
        self.client.connect()
        while True:
            try:
                self.sync_once()
            except Exception as error:
                print("fumarimo sync paused:", type(error).__name__, flush=True)
                time.sleep(5)


def main() -> None:
    rooms = [room.strip() for room in os.environ["MATRIX_ROOMS"].split(",") if room.strip()]
    client = MatrixClient(
        os.environ["MATRIX_HOMESERVER_URL"],
        Path(os.environ["MATRIX_TOKEN_DIR"]) / "fumarimo.token",
        rooms,
        Path(os.environ.get("MATRIX_STATE_FILE", str(Path.home() / ".local/state/futon-matrix/fumarimo.cursor"))),
    )
    FumarimoAgent(client).run()


if __name__ == "__main__":
    main()
