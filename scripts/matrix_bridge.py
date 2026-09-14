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


class MatrixBot(IRCBot):
    transport_name = "matrix"
    dedup_limit = 4096
    text_cap = 16000

    def __init__(self, nick, agent_id, rooms, homeserver, token_dir, state_dir,
                 handle_commands=False, command_owner_agent_map=None):
        if not re.fullmatch(r"[A-Za-z0-9._=-]+", nick):
            raise ValueError("Unsafe Matrix localpart for token/state filename")
        if not rooms or any(not re.fullmatch(r"![^\s:]+:[^\s]+", r) for r in rooms):
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
        return (f"[Surface: Matrix | Room: {channel or self.channel} | Speaker: {sender}"
                f"{mission_part} | Your returned text will be posted as {self.mxid}. "
                "Do not post progress through IRC or another transport. "
                "Return a concise reply, or a concrete completion/blocker with evidence.]")

    def _transport_context(self):
        return getattr(self._thread_context, "matrix_event", None)

    def _set_transport_context(self, context):
        self._thread_context.matrix_event = context

    def _start_message_handler(self, handler, sender, text, channel):
        # Process admission in sync order; invoke work still runs on inherited queue.
        handler(sender, text, channel)

    def _emit_success_reply(self, response, reply_ch, job_id, multi_message=False):
        # Transport renderer: don't run IRC's pre-send summary/line truncation.
        self._say(response.get("result") or "[no response]", channel=reply_ch)

    def _say(self, text, max_lines=6, channel=None):
        room = (channel or getattr(self._thread_context, "reply_channel", None)
                or self._reply_channel or self.channel)
        if room not in self.channels:
            raise ValueError("Matrix send to unlisted room refused")
        text = str(text)
        if len(text) > self.text_cap:
            text = text[:self.text_cap - 13] + "\n[truncated]"
        content = {"msgtype": "m.text", "body": text}
        context = self._transport_context()
        if context and context["room"] == room:
            content["m.relates_to"] = {"m.in_reply_to": {"event_id": context["event_id"]}}
        txn = uuid.uuid4().hex
        path = "/rooms/" + urllib.parse.quote(room, safe="") + "/send/m.room.message/" + txn
        # Same transaction for a bounded retry after an ambiguous transport failure.
        for attempt in range(2):
            try:
                result = self._request("PUT", path, content)
                if self._handles_bare_command(room):
                    irc.post_transport_evidence("matrix", room, self.mxid, text,
                                                "outbound", via_nick=self.nick)
                return result
            except RuntimeError:
                if attempt:
                    raise

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
