#!/usr/bin/env python3
"""Matrix publisher for a Marimo code cell and its linked output.

The execution boundary is deliberately absent here: callers supply reviewed
Python source and an already-uploaded MXC image.  This module owns only the
durable Matrix representation and provenance between the request, cell, and
output events.
"""
from __future__ import annotations

import html
import uuid


EVENT_NAMESPACE = "org.paragogy.marimo"
PYTHON_MSGTYPE = EVENT_NAMESPACE + ".python"
OUTPUT_MSGTYPE = EVENT_NAMESPACE + ".output"
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
) -> dict:
    if not image_mxc.startswith("mxc://"):
        raise ValueError("image_mxc must be an MXC URI")
    if not cell_event_id.startswith("$"):
        raise ValueError("cell_event_id must be a Matrix event ID")
    return {
        "msgtype": OUTPUT_MSGTYPE,
        "body": alt,
        "url": image_mxc,
        "info": {"mimetype": "image/png"},
        "m.relates_to": {
            "rel_type": OUTPUT_RELATION,
            "event_id": cell_event_id,
            "m.in_reply_to": {"event_id": cell_event_id},
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
            ),
        )
        return cell_event_id, output_event_id
