#!/usr/bin/env python3
"""
ngircd_bridge.py — Bridge between ngircd IRC and futon3c agent invoke API.

Connects to ngircd as IRC clients (claude, codex) and relays @mentions
to the futon3c invoke API on localhost:7070. Also handles ! commands
for fast mission-control and todo access.

Usage:
    python3 scripts/ngircd_bridge.py

Environment variables:
    IRC_HOST        (default: 127.0.0.1)
    IRC_PORT        (default: 6667)
    IRC_PASSWORD    (default: MonsterMountain)
    IRC_CHANNEL     (default: #futon)
    INVOKE_BASE     (default: http://127.0.0.1:7070)
    BRIDGE_BOTS     (default: claude,codex)  — comma-separated bot nicks
"""

import json
import os
import re
import socket
import sys
import threading
import time
import traceback
import urllib.request
import urllib.error

# --- Configuration ---

IRC_HOST = os.environ.get("IRC_HOST", "127.0.0.1")
IRC_PORT = int(os.environ.get("IRC_PORT", "6667"))
IRC_PASSWORD = os.environ.get("IRC_PASSWORD", "MonsterMountain")
IRC_CHANNEL = os.environ.get("IRC_CHANNEL", "#futon")
INVOKE_BASE = os.environ.get("INVOKE_BASE", "http://127.0.0.1:7070")
BRIDGE_BOTS = os.environ.get("BRIDGE_BOTS", "claude,codex").split(",")

INVOKE_URL = f"{INVOKE_BASE}/api/alpha/invoke"
MC_URL = f"{INVOKE_BASE}/api/alpha/mission-control"
TODO_URL = f"{INVOKE_BASE}/api/alpha/todo"
MAX_IRC_LINE = 400  # safe limit for PRIVMSG content (512 minus overhead)
RECONNECT_DELAY = 5
INVOKE_TIMEOUT = 600  # 10 minutes — agents can be slow
CMD_TIMEOUT = 30  # 30 seconds for ! commands

# Ungated nicks receive ALL channel messages, not just @mentions.
# Toggle with !ungate <nick> and !gate <nick>.
ungated_nicks: set[str] = set()


def log(bot_nick, msg):
    ts = time.strftime("%H:%M:%S")
    print(f"[{ts}] [{bot_nick}] {msg}", flush=True)


def api_post(url, payload, timeout=CMD_TIMEOUT):
    """POST JSON to a futon3c API endpoint."""
    body = json.dumps(payload).encode("utf-8")
    req = urllib.request.Request(
        url,
        data=body,
        headers={"Content-Type": "application/json"},
        method="POST",
    )
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return json.loads(resp.read())
    except urllib.error.HTTPError as e:
        body_text = e.read().decode("utf-8", errors="replace")[:500]
        try:
            return json.loads(body_text)
        except Exception:
            return {"ok": False, "error": f"HTTP {e.code}: {body_text[:200]}"}
    except Exception as e:
        return {"ok": False, "error": str(e)}


class IRCBot:
    """Single IRC bot that connects as a nick and relays @mentions."""

    def __init__(self, nick, agent_id, channel, host, port, password,
                 handle_commands=False):
        self.nick = nick
        self.agent_id = agent_id
        self.channel = channel
        self.host = host
        self.port = port
        self.password = password
        self.sock = None
        self.buf = ""
        self.handle_commands = handle_commands
        self.focused_mission = None
        self._invoking = threading.Lock()

    def connect(self):
        """Connect to IRC server, authenticate, join channel."""
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.settimeout(300)  # 5 min read timeout
        self.sock.connect((self.host, self.port))

        if self.password:
            self._send(f"PASS {self.password}")
        self._send(f"NICK {self.nick}")
        self._send(f"USER {self.nick} 0 * :{self.nick} agent (futon3c bridge)")

        # Wait for welcome (001) or error
        while True:
            line = self._readline()
            if line is None:
                raise ConnectionError("Connection closed during registration")
            prefix, cmd, params = self._parse(line)
            if cmd == "001":
                log(self.nick, f"Registered on {self.host}:{self.port}")
                break
            elif cmd == "433":
                # Nick in use — try with underscore
                self.nick = self.nick + "_"
                log(self.nick, f"Nick in use, trying {self.nick}")
                self._send(f"NICK {self.nick}")
            elif cmd == "ERROR":
                raise ConnectionError(f"Server error: {' '.join(params)}")

        self._send(f"JOIN {self.channel}")
        log(self.nick, f"Joined {self.channel}")

    def _send(self, line):
        """Send a raw IRC line."""
        self.sock.sendall((line + "\r\n").encode("utf-8"))

    def _readline(self):
        """Read one IRC line from the socket."""
        while "\r\n" not in self.buf:
            try:
                chunk = self.sock.recv(4096)
            except socket.timeout:
                # Send a ping to keep alive
                self._send(f"PING :keepalive")
                continue
            if not chunk:
                return None
            self.buf += chunk.decode("utf-8", errors="replace")
        line, self.buf = self.buf.split("\r\n", 1)
        return line

    def _parse(self, line):
        """Parse an IRC message into (prefix, command, params)."""
        prefix = ""
        if line.startswith(":"):
            prefix, line = line.split(" ", 1)
            prefix = prefix[1:]
        if " :" in line:
            head, trailing = line.split(" :", 1)
            parts = head.split()
            cmd = parts[0] if parts else ""
            params = parts[1:] + [trailing]
        else:
            parts = line.split()
            cmd = parts[0] if parts else ""
            params = parts[1:]
        return prefix, cmd.upper(), params

    def _sender_nick(self, prefix):
        """Extract nick from prefix like 'nick!user@host'."""
        return prefix.split("!")[0] if "!" in prefix else prefix

    def _is_mention(self, text):
        """Check if text mentions this bot (e.g., '@claude ...' or 'claude: ...')."""
        base_nick = self.nick.rstrip("_")
        patterns = [
            rf"^@{re.escape(base_nick)}\b",
            rf"^{re.escape(base_nick)}:\s",
            rf"^{re.escape(base_nick)},\s",
        ]
        for p in patterns:
            if re.match(p, text, re.IGNORECASE):
                return True
        return False

    def _strip_mention(self, text):
        """Remove the mention prefix from the text."""
        base_nick = self.nick.rstrip("_")
        text = re.sub(
            rf"^@?{re.escape(base_nick)}[,:]\s*",
            "", text, count=1, flags=re.IGNORECASE
        )
        return text.strip()

    def _invoke_agent(self, prompt, caller, mission_id=None):
        """Call the futon3c invoke API and return the result text."""
        payload = {
            "agent-id": self.agent_id,
            "prompt": prompt,
            "caller": f"irc:{caller}",
            "timeout-ms": INVOKE_TIMEOUT * 1000,
        }
        if mission_id:
            payload["mission-id"] = mission_id
        body = json.dumps(payload).encode("utf-8")

        req = urllib.request.Request(
            INVOKE_URL,
            data=body,
            headers={"Content-Type": "application/json"},
            method="POST",
        )
        try:
            with urllib.request.urlopen(req, timeout=INVOKE_TIMEOUT) as resp:
                data = json.loads(resp.read())
                if data.get("ok"):
                    return data.get("result", "")
                else:
                    return f"[invoke error: {data.get('error', 'unknown')}]"
        except urllib.error.HTTPError as e:
            body_text = e.read().decode("utf-8", errors="replace")[:200]
            return f"[HTTP {e.code}: {body_text}]"
        except Exception as e:
            return f"[invoke failed: {e}]"

    def _say(self, text):
        """Send a PRIVMSG to the channel, splitting long lines."""
        for line in text.split("\n"):
            line = line.rstrip()
            if not line:
                continue
            # Split long lines
            while len(line) > MAX_IRC_LINE:
                self._send(f"PRIVMSG {self.channel} :{line[:MAX_IRC_LINE]}")
                line = line[MAX_IRC_LINE:]
                time.sleep(0.3)  # rate limit
            self._send(f"PRIVMSG {self.channel} :{line}")
            time.sleep(0.1)  # gentle rate limit

    def _handle_mention(self, sender, text):
        """Process a mention: invoke agent, post response."""
        prompt_text = self._strip_mention(text)
        if not prompt_text:
            return

        # Surface contract: tell the agent where its output goes
        mission_part = ""
        if self.focused_mission:
            mission_part = f" | Focused Mission: {self.focused_mission}"
        surface_context = (
            f"[Surface: IRC | Channel: {self.channel} | "
            f"Speaker: {sender}{mission_part} | "
            f"Your response will be posted to {self.channel} as <{self.nick}>. "
            f"Keep responses concise for IRC.]"
        )
        full_prompt = f"{surface_context}\n\n{sender}: {prompt_text}"

        log(self.nick, f"Invoking for {sender}: {prompt_text[:80]}")

        # Serialize invocations per bot
        with self._invoking:
            result = self._invoke_agent(full_prompt, sender,
                                        mission_id=self.focused_mission)

        if result:
            log(self.nick, f"Response ({len(result)} chars)")
            self._say(result)

    def _handle_ungated(self, sender, text):
        """Process an ungated message: invoke agent with full text."""
        if not text.strip():
            return

        mission_part = ""
        if self.focused_mission:
            mission_part = f" | Focused Mission: {self.focused_mission}"
        surface_context = (
            f"[Surface: IRC | Channel: {self.channel} | "
            f"Speaker: {sender}{mission_part} | "
            f"Your response will be posted to {self.channel} as <{self.nick}>. "
            f"Keep responses concise for IRC.]"
        )
        full_prompt = f"{surface_context}\n\n{sender}: {text}"

        log(self.nick, f"Ungated invoke for {sender}: {text[:80]}")

        with self._invoking:
            result = self._invoke_agent(full_prompt, sender,
                                        mission_id=self.focused_mission)

        if result:
            log(self.nick, f"Response ({len(result)} chars)")
            self._say(result)

    # --- ! command handlers ---

    def _handle_command(self, sender, text):
        """Route ! commands to the appropriate handler."""
        parts = text.split(None, 1)
        cmd = parts[0].lower()
        args = parts[1] if len(parts) > 1 else ""

        log(self.nick, f"Command from {sender}: {text}")

        if cmd == "!help":
            self._cmd_help()
        elif cmd == "!ungate":
            self._cmd_ungate(sender, args)
        elif cmd == "!gate":
            self._cmd_gate(sender, args)
        elif cmd == "!mc":
            self._cmd_mc(sender, args)
        elif cmd == "!mission":
            self._cmd_mission(sender, args)
        elif cmd == "!reset":
            self._cmd_reset(sender, args)
        elif cmd == "!todo":
            self._cmd_todo(sender, args)
        else:
            self._say(f"Unknown command: {cmd} — try !help")

    def _cmd_help(self):
        """List available commands."""
        self._say("Commands: !ungate <nick> | !gate <nick> | "
                  "!reset <agent-id> | !mc status | !mc review | "
                  "!mc missions | !mc sessions | !mc diff | "
                  "!mission focus <id> | !mission show | "
                  "!mission clear | !todo add <text> | "
                  "!todo list | !todo done <id> | !help")

    def _cmd_ungate(self, sender, args):
        """Ungate a bot — it will respond to all messages, not just @mentions."""
        nick = args.strip().lower()
        if not nick:
            self._say("Usage: !ungate <nick> (e.g. !ungate claude)")
            return
        ungated_nicks.add(nick)
        log(self.nick, f"UNGATED: {nick} (by {sender})")
        self._say(f"{nick} is now ungated — listening to all messages")

    def _cmd_gate(self, sender, args):
        """Gate a bot — back to @mention-only mode."""
        nick = args.strip().lower()
        if not nick:
            self._say("Usage: !gate <nick> (e.g. !gate claude)")
            return
        ungated_nicks.discard(nick)
        log(self.nick, f"GATED: {nick} (by {sender})")
        self._say(f"{nick} is now gated — mention-only mode")

    def _cmd_reset(self, sender, args):
        """Reset an agent's session so the next invoke starts fresh."""
        agent_id = args.strip()
        if not agent_id:
            self._say("Usage: !reset <agent-id> (e.g. !reset claude-1)")
            return
        url = f"{INVOKE_BASE}/api/alpha/agents/{agent_id}/reset-session"
        data = api_post(url, {})
        if data.get("ok"):
            old_sid = data.get("old-session-id", "none")
            log(self.nick, f"RESET: {agent_id} (by {sender}, was {old_sid})")
            self._say(f"{agent_id} session reset (was {old_sid}) — next invoke starts fresh")
        else:
            err = data.get("error", "unknown")
            self._say(f"[reset error: {err}]")

    def _cmd_mission(self, sender, args):
        """Handle !mission subcommands: focus, clear, show."""
        parts = args.split(None, 1)
        sub = parts[0].lower() if parts else "show"
        rest = parts[1].strip() if len(parts) > 1 else ""

        if sub == "focus":
            if not rest:
                self._say("Usage: !mission focus <mission-id>")
                return
            self.focused_mission = rest
            self._say(f"Focused on {rest}")

        elif sub == "clear":
            self.focused_mission = None
            self._say("Focus cleared")

        elif sub == "show":
            if self.focused_mission:
                self._say(f"Current focus: {self.focused_mission}")
            else:
                self._say("No mission focus set")

        else:
            self._say(f"Unknown !mission subcommand: {sub} — "
                      "try: focus <id>, show, clear")

    def _cmd_mc(self, sender, args):
        """Handle !mc subcommands."""
        parts = args.split(None, 1)
        sub = parts[0].lower() if parts else "status"

        if sub == "status":
            data = api_post(MC_URL, {"action": "status"})
            if data.get("ok"):
                sc = data.get("session-count", 0)
                ac = data.get("active-count", 0)
                self._say(f"Mission control: {sc} sessions ({ac} active)")
            else:
                self._say(f"[mc error: {data.get('error', 'unknown')}]")

        elif sub == "review":
            data = api_post(MC_URL, {"action": "review", "author": sender},
                            timeout=60)
            if data.get("ok"):
                lr = data.get("last-result", {})
                result = lr.get("result", {}) if isinstance(lr, dict) else {}
                summary = result.get("portfolio/summary", "no summary")
                actionable = result.get("portfolio/actionable", [])
                self._say(f"Portfolio: {summary}")
                if actionable:
                    self._say(f"Actionable items: {len(actionable)}")
                    for item in actionable[:5]:
                        mid = item.get("mission/id", "?")
                        status = item.get("mission/status", "?")
                        self._say(f"  {mid} [{status}]")
                    if len(actionable) > 5:
                        self._say(f"  ... and {len(actionable) - 5} more")
            else:
                self._say(f"[mc error: {data.get('error', 'unknown')}]")

        elif sub == "missions":
            data = api_post(MC_URL, {"action": "review", "author": sender},
                            timeout=60)
            if data.get("ok"):
                lr = data.get("last-result", {})
                result = lr.get("result", {}) if isinstance(lr, dict) else {}
                missions = result.get("portfolio/missions", [])
                # Group by status
                by_status = {}
                for m in missions:
                    s = m.get("mission/status", "unknown")
                    by_status.setdefault(s, []).append(m)
                for status in ["in-progress", "ready", "blocked",
                               "complete", "unknown"]:
                    items = by_status.get(status, [])
                    if items:
                        names = ", ".join(m.get("mission/id", "?")
                                          for m in items[:8])
                        extra = (f" +{len(items)-8}" if len(items) > 8
                                 else "")
                        self._say(f"[{status}] ({len(items)}): {names}{extra}")
            else:
                self._say(f"[mc error: {data.get('error', 'unknown')}]")

        elif sub == "sessions":
            data = api_post(MC_URL, {"action": "sessions"})
            if data.get("ok"):
                sessions = data.get("sessions", [])
                if not sessions:
                    self._say("No mission-control sessions.")
                else:
                    for s in sessions[:5]:
                        sid = s.get("session-id", "?")[:12]
                        status = s.get("status", "?")
                        steps = s.get("step-count", 0)
                        review = s.get("latest-review", "")
                        self._say(f"  {sid}.. [{status}] {steps} steps"
                                  + (f" — {review[:120]}" if review else ""))
            else:
                self._say(f"[mc error: {data.get('error', 'unknown')}]")

        elif sub == "diff":
            data = api_post(MC_URL, {"action": "diff"})
            if data.get("ok"):
                diff = data.get("diff")
                if diff is None:
                    self._say(data.get("message",
                              "Not enough review history — run !mc review at least twice"))
                else:
                    added = diff.get("added", [])
                    removed = diff.get("removed", [])
                    changed = diff.get("changed", [])
                    if not added and not removed and not changed:
                        self._say("No changes since last review")
                    else:
                        self._say(f"Portfolio diff (since last review):")
                        for m in added:
                            mid = m.get("mission/id", "?")
                            status = m.get("mission/status", "?")
                            self._say(f"  + {mid} [{status}]")
                        for m in changed:
                            mid = m.get("mission/id", "?")
                            old_s = m.get("old-status", "?")
                            new_s = m.get("new-status", "?")
                            self._say(f"  ~ {mid} [{old_s} → {new_s}]")
                        for m in removed:
                            mid = m.get("mission/id", "?")
                            status = m.get("mission/status", "?")
                            self._say(f"  - {mid} [{status}]")
                        nc = diff.get("new-count", 0)
                        oc = diff.get("old-count", 0)
                        self._say(f"  Summary: {nc} missions (was {oc})")
            else:
                self._say(f"[mc error: {data.get('error', 'unknown')}]")

        else:
            self._say(f"Unknown !mc subcommand: {sub} — "
                      "try: status, review, missions, sessions, diff")

    def _cmd_todo(self, sender, args):
        """Handle !todo subcommands."""
        parts = args.split(None, 1)
        sub = parts[0].lower() if parts else "list"
        rest = parts[1] if len(parts) > 1 else ""

        if sub == "add":
            if not rest.strip():
                self._say("Usage: !todo add <description>")
                return
            data = api_post(TODO_URL, {
                "action": "add",
                "text": rest.strip(),
                "author": sender,
            })
            if data.get("ok"):
                tid = data.get("id", "?")
                short_id = tid.replace("todo-", "")[:8]
                self._say(f"Added: {rest.strip()} (#{short_id})")
            else:
                self._say(f"[todo error: {data.get('error', data.get('err', 'unknown'))}]")

        elif sub == "list":
            data = api_post(TODO_URL, {"action": "list"})
            if data.get("ok"):
                todos = data.get("todos", [])
                if not todos:
                    self._say("No pending todos.")
                else:
                    for i, t in enumerate(todos, 1):
                        tid = t.get("id", "").replace("todo-", "")[:8]
                        text = t.get("text", "?")
                        author = t.get("author", "?")
                        self._say(f"  {i}. {text} (#{tid}, by {author})")
            else:
                self._say(f"[todo error: {data.get('error', data.get('err', 'unknown'))}]")

        elif sub == "done":
            if not rest.strip():
                self._say("Usage: !todo done <id>")
                return
            todo_id = rest.strip()
            # Allow short IDs — prepend "todo-" if needed
            if not todo_id.startswith("todo-"):
                todo_id = f"todo-{todo_id}"
            data = api_post(TODO_URL, {
                "action": "done",
                "id": todo_id,
                "author": sender,
            })
            if data.get("ok"):
                self._say(f"Done: {todo_id}")
            else:
                self._say(f"[todo error: {data.get('error', data.get('err', 'unknown'))}]")

        else:
            self._say(f"Unknown !todo subcommand: {sub} — "
                      "try: add, list, done")

    def run(self):
        """Main loop: read IRC messages, handle PINGs, commands, mentions."""
        while True:
            try:
                self.connect()
                while True:
                    line = self._readline()
                    if line is None:
                        log(self.nick, "Connection closed by server")
                        break

                    prefix, cmd, params = self._parse(line)

                    if cmd == "PING":
                        pong_arg = params[0] if params else ""
                        self._send(f"PONG :{pong_arg}")

                    elif cmd == "PRIVMSG" and len(params) >= 2:
                        target = params[0]
                        text = params[1]
                        sender = self._sender_nick(prefix)

                        # Only respond to channel messages, not from bots
                        if (target.lower() == self.channel.lower()
                                and sender.lower() != self.nick.lower()):

                            # ! commands — only handled by first bot
                            if self.handle_commands and text.startswith("!"):
                                t = threading.Thread(
                                    target=self._handle_command,
                                    args=(sender, text),
                                    daemon=True,
                                )
                                t.start()

                            # @mentions — each bot handles its own
                            elif self._is_mention(text):
                                t = threading.Thread(
                                    target=self._handle_mention,
                                    args=(sender, text),
                                    daemon=True,
                                )
                                t.start()

                            # Ungated mode — respond to all messages
                            elif self.nick.rstrip("_").lower() in ungated_nicks:
                                t = threading.Thread(
                                    target=self._handle_ungated,
                                    args=(sender, text),
                                    daemon=True,
                                )
                                t.start()

            except Exception as e:
                log(self.nick, f"Error: {e}")
                traceback.print_exc()

            # Reconnect
            if self.sock:
                try:
                    self.sock.close()
                except Exception:
                    pass
                self.sock = None

            log(self.nick, f"Reconnecting in {RECONNECT_DELAY}s...")
            time.sleep(RECONNECT_DELAY)


def main():
    nick_to_agent = {
        "claude": "claude-1",
        "codex": "codex-1",
    }

    bots = []
    for i, nick in enumerate(BRIDGE_BOTS):
        nick = nick.strip()
        if not nick:
            continue
        agent_id = nick_to_agent.get(nick, f"{nick}-1")
        bot = IRCBot(
            nick=nick,
            agent_id=agent_id,
            channel=IRC_CHANNEL,
            host=IRC_HOST,
            port=IRC_PORT,
            password=IRC_PASSWORD,
            handle_commands=(i == 0),  # first bot handles ! commands
        )
        bots.append(bot)

    if not bots:
        print("No bots configured. Set BRIDGE_BOTS env var.", file=sys.stderr)
        sys.exit(1)

    print(f"Starting ngircd bridge: {[b.nick for b in bots]} → {INVOKE_BASE}")
    print(f"IRC: {IRC_HOST}:{IRC_PORT} | Channel: {IRC_CHANNEL}")
    print(f"Commands handled by: {bots[0].nick}")

    threads = []
    for bot in bots:
        t = threading.Thread(target=bot.run, name=f"bot-{bot.nick}", daemon=True)
        t.start()
        threads.append(t)
        time.sleep(0.5)  # stagger connections

    # Wait for all threads (they run forever with reconnect)
    try:
        while True:
            time.sleep(60)
    except KeyboardInterrupt:
        print("\nShutting down bridge...")
        sys.exit(0)


if __name__ == "__main__":
    main()
