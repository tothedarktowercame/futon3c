#!/usr/bin/env python3
"""Matrix transport for IRCBot; stdlib only, no encrypted-room support.

MATRIX_HOMESERVER_URL, MATRIX_ROOMS (!room:server IDs), MATRIX_TOKEN_DIR,
MATRIX_STATE_DIR (default ~/.local/state/futon-matrix), BRIDGE_BOTS and
NICK_AGENT_MAP configure the process. Agency configuration is inherited.

Event IDs are reserved durably before dispatch: at-most-once admission, not
exactly-once completion. A crash between reservation and Agency acceptance can
lose work; the inherited in-memory queue is not a durable outbox. Dedup retains
4096 event IDs; arbitrarily old replay beyond that window is not guaranteed.
One process must own each bot's state directory. Tokens never enter argv/logs.
"""
import importlib.util
import html
import json
import os
from pathlib import Path
import re
import threading
import urllib.error
import urllib.parse
import urllib.request
import uuid

# Also works when tests import this file by path without scripts on sys.path.
_spec = importlib.util.spec_from_file_location(
    "matrix_irc_transport_base", Path(__file__).with_name("ngircd_bridge.py"))
irc = importlib.util.module_from_spec(_spec)
_spec.loader.exec_module(irc)
IRCBot = irc.IRCBot

_fumarimo_spec = importlib.util.spec_from_file_location(
    "matrix_fumarimo_publisher", Path(__file__).with_name("fumarimo_agent.py"))
fumarimo = importlib.util.module_from_spec(_fumarimo_spec)
_fumarimo_spec.loader.exec_module(fumarimo)

PROFORMA_COLORS = {
    "㊩": "#2a78d6", "🈖": "#2a78d6", "㊢": "#2a78d6",
    "🈯": "#eb6834", "㊟": "#eb6834", "㊣": "#eb6834", "🈚": "#eb6834", "㊮": "#eb6834", "🈹": "#eb6834",
    "🈲": "#1baf7a", "🈕": "#1baf7a", "㊫": "#1baf7a",
    "㊭": "#eda100", "㊝": "#eda100", "🈘": "#eda100", "🈝": "#eda100", "㊯": "#eda100", "🈡": "#eda100",
    "🈸": "#e87ba4", "🈰": "#e87ba4", "㊬": "#e87ba4",
    "㊥": "#66665e", "🈳": "#66665e",
}

FUMARIMO_BRIEF = (
    "You are Fumarimo, the room's Python and Marimo notebook agent. Treat the user's message as a "
    "request to create, explain, revise, or run notebook work, not as a request for generic chat. "
    "Write correct, readable Python and preserve the user's stated data source and definitions. Never "
    "invent Matrix history, files, columns, totals, execution results, or charts. If a required input "
    "or measure is missing, ask one concise clarifying question instead of fabricating it. When the "
    "request is sufficiently specified, reply with one self-contained Python cell in exactly one "
    "fenced python block; put any short explanation outside the block. The cell should expose its final "
    "table, figure, or value as its last expression so Marimo can render it. Distinguish proposed source "
    "from observed output, and do not say the cell ran unless the prompt supplies execution evidence. "
    "For chart requests, include readable labels and the requested numeric values. Keep notebook work in "
    "the main chat; turn annotations belong to the separate annotation sidebar."
)

FENCED_PYTHON_RE = re.compile(r"```python[ \t]*\n(.*?)\n```", re.DOTALL | re.IGNORECASE)


def fumarimo_python_source(text):
    """Return the sole fenced Python cell in an LLM response, if present."""
    matches = FENCED_PYTHON_RE.findall(str(text))
    return matches[0].strip() if len(matches) == 1 and matches[0].strip() else None


def proforma_formatted_body(text):
    """Matrix-safe HTML for marked replies; plain text remains the fallback."""
    found = False
    lines = []
    glyphs = "|".join(map(re.escape, PROFORMA_COLORS))
    pattern = re.compile(rf"^(\s*)({glyphs})(?=\s)")
    for line in text.split("\n"):
        match = pattern.match(line)
        if not match:
            lines.append(html.escape(line))
            continue
        found = True
        color = PROFORMA_COLORS[match.group(2)]
        prefix = html.escape(match.group(1))
        # Ask clients to render the enclosed Unicode mark as text so its
        # foreground colour is not replaced by an emoji presentation.
        glyph = html.escape(match.group(2)) + '&#xfe0e;'
        rest = html.escape(line[match.end():])
        lines.append(f'{prefix}<span data-mx-color="{color}">{glyph}</span>{rest}')
    return "<br>".join(lines) if found else None


class MatrixBot(IRCBot):
    transport_name = "matrix"
    dedup_limit = 4096
    text_cap = 16000

    def __init__(self, nick, agent_id, rooms, homeserver, token_dir, state_dir,
                 handle_commands=False, command_owner_agent_map=None):
        if not re.fullmatch(r"[A-Za-z0-9._=-]+", nick):
            raise ValueError("Unsafe Matrix localpart for token/state filename")
        # Room version 12 IDs carry no server part: the Private Federation Proof
        # room is !_qvu9Pec8-hw1-nsN18SA8uIChKlJPmS4f4ji3zajRw (2026-09-14).
        # The "!" sigil is what separates an ID from a "#alias:server".
        if not rooms or any(not re.fullmatch(r"![^\s:]+(?::[^\s]+)?", r) for r in rooms):
            raise ValueError("MATRIX_ROOMS must contain opaque room IDs, not aliases")
        parsed = urllib.parse.urlsplit(homeserver)
        if parsed.scheme not in ("https", "http") or not parsed.netloc or parsed.username or parsed.password:
            raise ValueError("Invalid Matrix homeserver URL")
        self.homeserver = homeserver.rstrip("/")
        self._token = (Path(token_dir) / (nick + ".token")).read_text().strip()
        if not self._token or any(c.isspace() for c in self._token):
            raise ValueError("Missing or malformed Matrix token file")
        self.state_path = Path(state_dir) / (nick + ".json")
        self.state_path.parent.mkdir(parents=True, exist_ok=True)
        self._state = {"next_batch": None, "seen": []}
        if self.state_path.exists():
            self._state = json.loads(self.state_path.read_text())
            if (not isinstance(self._state, dict)
                    or not isinstance(self._state.get("seen"), list)
                    or not all(isinstance(x, str) for x in self._state["seen"])
                    or not isinstance(self._state.get("next_batch"), (str, type(None)))):
                raise ValueError("Invalid Matrix state; refusing history replay")
        self.mxid = None
        self._encrypted_rooms = set()
        super().__init__(nick, agent_id, rooms[0], parsed.hostname, parsed.port,
                         None, handle_commands, channels=list(rooms),
                         command_owner_agent_map=command_owner_agent_map)

    def _request(self, method, path, body=None, query=None):
        url = self.homeserver + "/_matrix/client/v3" + path
        if query:
            url += "?" + urllib.parse.urlencode(query)
        data = None if body is None else json.dumps(body).encode("utf-8")
        request = urllib.request.Request(url, data=data, method=method, headers={
            "Authorization": "Bearer " + self._token, "Content-Type": "application/json"})
        # Do not follow redirects with credentials to another authority.
        class NoRedirect(urllib.request.HTTPRedirectHandler):
            def redirect_request(self, req, fp, code, msg, headers, newurl):
                return None
        try:
            with urllib.request.build_opener(NoRedirect).open(request, timeout=40) as response:
                return json.load(response)
        except Exception as exc:
            # HTTP bodies/headers (including credentials) never reach bridge logs.
            raise RuntimeError("Matrix transport request failed: " + type(exc).__name__) from None

    @staticmethod
    def quote_room(room):
        return urllib.parse.quote(room, safe="")

    def _save_state(self):
        temporary = self.state_path.with_suffix(".tmp")
        with open(temporary, "w", encoding="utf-8") as stream:
            os.chmod(temporary, 0o600)
            json.dump(self._state, stream)
            stream.flush()
            os.fsync(stream.fileno())
        os.replace(temporary, self.state_path)
        directory = os.open(self.state_path.parent, os.O_RDONLY)
        try:
            os.fsync(directory)
        finally:
            os.close(directory)

    def connect(self):
        user = self._request("GET", "/account/whoami").get("user_id", "")
        if not user.startswith("@") or ":" not in user or user[1:].split(":", 1)[0] != self.nick:
            raise ValueError("Matrix token identity does not match configured nick")
        self.mxid = user
        self.connected = True

    def _surface_context(self, sender, mission_part, brief, multi_message=False, channel=None):
        context = (f"[Surface: Matrix | Room: {channel or self.channel} | Speaker: {sender}"
                f"{mission_part} | Your returned text will be posted as {self.mxid}. "
                "Do not post progress through IRC or another transport. "
                "Return a concise reply, or a concrete completion/blocker with evidence.]")
        if self.nick == "fumarimo":
            context += "\n\n" + FUMARIMO_BRIEF
        return context

    def _transport_context(self):
        return getattr(self._thread_context, "matrix_event", None)

    def _set_transport_context(self, context):
        self._thread_context.matrix_event = context

    def _start_message_handler(self, handler, sender, text, channel):
        # Process admission in sync order; invoke work still runs on inherited queue.
        handler(sender, text, channel)

    def _emit_success_reply(self, response, reply_ch, job_id, multi_message=False):
        # Transport renderer: don't run IRC's pre-send summary/line truncation.
        result = response.get("result") or "[no response]"
        context = self._transport_context()
        source = fumarimo_python_source(result) if self.nick == "fumarimo" else None
        if source and context and context["room"] == reply_ch:
            cell_id = uuid.uuid4().hex
            content = fumarimo.python_cell_content(source, context["event_id"], cell_id)
            sent = self._send_content(content, reply_ch)
            cell_event_id = sent.get("event_id") if isinstance(sent, dict) else None
            svg = fumarimo.safe_cumulative_wealth_svg(source)
            if svg is not None and cell_event_id:
                image_mxc = self._upload_media(svg, "image/svg+xml", "wealth-by-population.svg")
                self._send_content(fumarimo.image_output_content(
                    image_mxc,
                    "Cumulative U.S. household net wealth by population percentile; rendered from the cell's literal data",
                    context["event_id"],
                    cell_event_id,
                    cell_id,
                    uuid.uuid4().hex,
                    "image/svg+xml",
                ), reply_ch)
        else:
            sent = self._say(result, channel=reply_ch)
        event_id = sent.get("event_id") if isinstance(sent, dict) else None
        if event_id:
            if not hasattr(self, "_xiang_reply_events"):
                self._xiang_reply_events = {}
            self._xiang_reply_events[job_id] = event_id

    def _say(self, text, max_lines=6, channel=None):
        room = (channel or getattr(self._thread_context, "reply_channel", None)
                or self._reply_channel or self.channel)
        if room not in self.channels:
            raise ValueError("Matrix send to unlisted room refused")
        text = str(text)
        if len(text) > self.text_cap:
            text = text[:self.text_cap - 13] + "\n[truncated]"
        content = {"msgtype": "m.text", "body": text}
        formatted = proforma_formatted_body(text)
        if formatted:
            content.update({"format": "org.matrix.custom.html", "formatted_body": formatted})
        context = self._transport_context()
        if context and context["room"] == room:
            content["m.relates_to"] = {"m.in_reply_to": {"event_id": context["event_id"]}}
        return self._send_content(content, room)

    def _send_content(self, content, room):
        """Send already-shaped Matrix message content to a configured room."""
        if room not in self.channels:
            raise ValueError("Matrix send to unlisted room refused")
        txn = uuid.uuid4().hex
        path = "/rooms/" + urllib.parse.quote(room, safe="") + "/send/m.room.message/" + txn
        # Same transaction for a bounded retry after an ambiguous transport failure.
        for attempt in range(2):
            try:
                result = self._request("PUT", path, content)
                if self._handles_bare_command(room):
                    irc.post_transport_evidence("matrix", room, self.mxid, content.get("body", ""),
                                                "outbound", via_nick=self.nick)
                return result
            except RuntimeError:
                if attempt:
                    raise

    def _upload_media(self, data, mimetype, filename):
        """Upload generated bytes to Matrix without exposing the bearer token."""
        query = urllib.parse.urlencode({"filename": filename})
        url = self.homeserver + "/_matrix/media/v3/upload?" + query
        request = urllib.request.Request(url, data=data, method="POST", headers={
            "Authorization": "Bearer " + self._token,
            "Content-Type": mimetype,
        })
        try:
            with urllib.request.build_opener().open(request, timeout=40) as response:
                uri = json.load(response).get("content_uri", "")
        except Exception as exc:
            raise RuntimeError("Matrix media upload failed: " + type(exc).__name__) from None
        if not isinstance(uri, str) or not uri.startswith("mxc://"):
            raise RuntimeError("Matrix media upload did not return an MXC URI")
        return uri

    def _routable_text(self, content, text):
        """Body as the inherited IRC mention rules should see it.

        A reply's quoted fallback ("> <@rob:...> @codex hello") repeats someone
        else's mention, so it is dropped. This bot's own MXID is shortened to
        @nick, so the inherited strip leaves the prompt, not the server name.
        A same-localpart user on another server ("@codex:elsewhere") loses its
        "@": the inherited rule ends a name at ":" and would otherwise count it
        as a mention of this bot. Transcript evidence keeps the original body.
        """
        relates = content.get("m.relates_to")
        if isinstance(relates, dict) and "m.in_reply_to" in relates:
            lines = text.split("\n")
            quoted = 0
            while quoted < len(lines) and lines[quoted].startswith(">"):
                quoted += 1
            if quoted:
                text = "\n".join(lines[quoted:]).lstrip("\n")
        if self.mxid:
            text = text.replace(self.mxid, "@" + self.nick)
        return re.sub(r"@(" + re.escape(self.nick) + r":\S+)", r"\1", text,
                      flags=re.IGNORECASE)

    def process_sync(self, batch):
        token = batch.get("next_batch")
        if not isinstance(token, str) or not token:
            raise ValueError("Matrix sync missing next_batch")
        rooms = batch.get("rooms", {})
        for room in rooms.get("invite", {}):
            if room in self.channels:
                self._request("POST", "/join/" + urllib.parse.quote(room, safe=""), {})
        if self._state["next_batch"] is None:
            # Baseline only: never execute initial history, including later replay.
            self._state["seen"] = [
                e["event_id"] for r, d in rooms.get("join", {}).items()
                if r in self.channels
                for e in d.get("timeline", {}).get("events", [])
                if isinstance(e.get("event_id"), str)][-self.dedup_limit:]
            self._state["next_batch"] = token
            self._save_state()
            return
        if token == self._state["next_batch"]:
            return
        for room, data in rooms.get("join", {}).items():
            if room not in self.channels:
                continue
            for event in data.get("timeline", {}).get("events", []):
                kind = event.get("type")
                if kind == "m.room.encrypted":
                    if room not in self._encrypted_rooms:
                        irc.log(self.nick, f"Encrypted Matrix room unsupported: {room}")
                        self._encrypted_rooms.add(room)
                    continue
                if kind != "m.room.message" or event.get("sender") == self.mxid:
                    continue
                text = event.get("content", {}).get("body")
                event_id, sender = event.get("event_id"), event.get("sender")
                if not (isinstance(text, str) and isinstance(event_id, str)
                        and isinstance(sender, str) and sender.startswith("@")):
                    continue
                if event_id in self._state["seen"]:
                    continue
                self._state["seen"] = (self._state["seen"] + [event_id])[-self.dedup_limit:]
                self._save_state()  # reserve before any Agency or command side effect
                if self.handle_commands and self._handles_bare_command(room):
                    irc.post_transport_evidence("matrix", room, sender, text,
                                                "inbound", via_nick=self.nick)
                self._reply_channel = room
                self._set_transport_context({"room": room, "event_id": event_id})
                try:
                    self._dispatch_message(
                        sender, self._routable_text(event.get("content", {}), text), room)
                finally:
                    self._set_transport_context(None)
        self._state["next_batch"] = token
        self._save_state()

    def sync_once(self):
        query = {"timeout": 30000, "filter": json.dumps({"room": {"rooms": self.channels}})}
        if self._state["next_batch"]:
            query["since"] = self._state["next_batch"]
        self.process_sync(self._request("GET", "/sync", query=query))

    def run(self):
        while not self._stop_event.is_set():
            try:
                if not self.connected:
                    self.connect()
                self.sync_once()
            except Exception as exc:
                irc.log(self.nick, "Matrix loop paused: " + type(exc).__name__)
                self._stop_event.wait(5)


def main():
    rooms = [r.strip() for r in os.environ.get("MATRIX_ROOMS", "").split(",") if r.strip()]
    mapping = irc._nick_to_agent_map_from_env()
    bots = [MatrixBot(n.strip(), irc._agent_id_for_nick(n.strip(), mapping), rooms,
                      os.environ["MATRIX_HOMESERVER_URL"], os.environ["MATRIX_TOKEN_DIR"],
                      os.environ.get("MATRIX_STATE_DIR", str(Path.home() / ".local/state/futon-matrix")),
                      handle_commands=(i == 0))
            for i, n in enumerate(irc.BRIDGE_BOTS) if n.strip()]
    threads = [threading.Thread(target=b.run, daemon=True) for b in bots]
    for t in threads:
        t.start()
    try:
        for t in threads:
            t.join()
    except KeyboardInterrupt:
        for b in bots:
            b.stop()


if __name__ == "__main__":
    main()
