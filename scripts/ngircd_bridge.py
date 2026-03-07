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
    INVOKE_TIMEOUT_SECONDS (default: 1800)   — invoke hard-timeout in seconds
    INVOKE_QUEUE_MAX       (default: 20)     — max queued invokes per bot
    CMD_TIMEOUT_SECONDS    (default: 30)     — !command timeout in seconds
"""

import atexit
import http.server
import json
import os
import queue
import re
import signal
import socket
import sys
import tempfile
import threading
import time
import traceback
import urllib.request
import urllib.error

try:
    import fcntl  # POSIX only
except ImportError:
    fcntl = None

try:
    import msvcrt  # Windows only
except ImportError:
    msvcrt = None

# --- Helpers ---

def env_int_seconds(name, default):
    """Read positive integer from env var (seconds), with fallback."""
    raw = os.environ.get(name)
    if raw is None:
        return default
    try:
        value = int(raw)
        return value if value > 0 else default
    except ValueError:
        return default

# --- Configuration ---


def int_env(name, default, minimum=1):
    """Parse integer env var with fallback and lower bound."""
    raw = os.environ.get(name)
    if raw is None:
        return default
    try:
        value = int(raw)
    except ValueError:
        return default
    return max(minimum, value)


def _health_ok(base_url, timeout_seconds=0.5):
    """Return True when base_url/health responds."""
    url = f"{base_url.rstrip('/')}/health"
    req = urllib.request.Request(url, method="GET")
    try:
        with urllib.request.urlopen(req, timeout=timeout_seconds) as resp:
            return 200 <= int(resp.status) < 500
    except Exception:
        return False


def resolve_invoke_base():
    """Pick futon3c HTTP base.

    Priority:
    1) explicit INVOKE_BASE
    2) FUTON3C_PORT-derived local base if healthy
    3) healthy default local ports (7070, 47070)
    4) fallback to first candidate
    """
    explicit = os.environ.get("INVOKE_BASE")
    if explicit:
        return explicit.rstrip("/"), "INVOKE_BASE"

    candidates = []
    futon3c_port = os.environ.get("FUTON3C_PORT", "").strip()
    if futon3c_port.isdigit():
        candidates.append(f"http://127.0.0.1:{int(futon3c_port)}")
    candidates.extend(["http://127.0.0.1:7070", "http://127.0.0.1:47070"])

    deduped = []
    for c in candidates:
        c = c.rstrip("/")
        if c and c not in deduped:
            deduped.append(c)

    for c in deduped:
        if _health_ok(c):
            return c, "autodetect"
    return deduped[0], "fallback"


IRC_HOST = os.environ.get("IRC_HOST", "127.0.0.1")
IRC_PORT = int_env("IRC_PORT", 6667, minimum=1)
IRC_PASSWORD = os.environ.get("IRC_PASSWORD", "MonsterMountain")
# IRC_CHANNEL is the primary channel; IRC_CHANNELS adds extras (comma-separated).
# E.g. IRC_CHANNEL=#futon IRC_CHANNELS=#math,#ops → bot joins all three.
IRC_CHANNEL = os.environ.get("IRC_CHANNEL", "#futon")
IRC_CHANNELS = [IRC_CHANNEL] + [
    ch.strip() for ch in os.environ.get("IRC_CHANNELS", "").split(",")
    if ch.strip() and ch.strip() != IRC_CHANNEL
]
INVOKE_BASE, INVOKE_BASE_SOURCE = resolve_invoke_base()
BRIDGE_BOTS = os.environ.get("BRIDGE_BOTS", "claude,claude-2,codex").split(",")

INVOKE_URL = f"{INVOKE_BASE}/api/alpha/invoke"
AGENTS_URL = f"{INVOKE_BASE}/api/alpha/agents"
MC_URL = f"{INVOKE_BASE}/api/alpha/mission-control"
TODO_URL = f"{INVOKE_BASE}/api/alpha/todo"
INVOKE_DELIVERY_URL = f"{INVOKE_BASE}/api/alpha/invoke-delivery"
CODEX_BRIDGE_SUMMARY_MODE = os.environ.get("CODEX_BRIDGE_SUMMARY_MODE", "summary").strip().lower()
CODEX_USE_RAW_OUTPUT = CODEX_BRIDGE_SUMMARY_MODE == "raw"
MAX_IRC_LINE = 400  # safe limit for PRIVMSG content (512 minus overhead)
RECONNECT_DELAY = 5
INVOKE_TIMEOUT_SECONDS = int_env(
    "INVOKE_TIMEOUT_SECONDS",
    int_env("INVOKE_TIMEOUT", 1800, minimum=60),
    minimum=60,
)  # seconds; aligns with futon3c hard invoke timeout (30 min)
INVOKE_CLIENT_TIMEOUT_SECONDS = int_env(
    "INVOKE_CLIENT_TIMEOUT_SECONDS",
    INVOKE_TIMEOUT_SECONDS + 15,
    minimum=1,
)
STATUS_TIMEOUT = int_env("AGENT_STATUS_TIMEOUT", 5, minimum=1)
INVOKE_SKIP_WHEN_BUSY = os.environ.get("INVOKE_SKIP_WHEN_BUSY", "1").lower() not in (
    "0",
    "false",
    "no",
    "off",
)
CMD_TIMEOUT = int_env("CMD_TIMEOUT_SECONDS", 30, minimum=1)  # seconds for ! commands
INVOKE_QUEUE_MAX = int_env("INVOKE_QUEUE_MAX", 20, minimum=1)

# --- Health / PID file (channel-scoped so multiple bridges can coexist) ---
_RUNTIME_DIR = os.environ.get("XDG_RUNTIME_DIR") or tempfile.gettempdir()
_CHANNEL_SLUG = IRC_CHANNEL.lstrip("#").replace("/", "_")
PIDFILE = os.path.join(_RUNTIME_DIR, f"ngircd-bridge-{_CHANNEL_SLUG}.pid")
HEALTH_FILE = os.path.join(_RUNTIME_DIR, f"ngircd-bridge-{_CHANNEL_SLUG}-health.json")
HEALTH_INTERVAL = 30  # seconds between health file writes


def _lock_pidfile(fd):
    """Acquire a non-blocking exclusive lock on a pidfile handle."""
    if fcntl is not None:
        fcntl.flock(fd, fcntl.LOCK_EX | fcntl.LOCK_NB)
        return
    if msvcrt is not None:
        # msvcrt locks byte ranges; ensure one byte exists to lock.
        fd.seek(0, os.SEEK_END)
        if fd.tell() == 0:
            fd.write("\n")
            fd.flush()
        fd.seek(0)
        msvcrt.locking(fd.fileno(), msvcrt.LK_NBLCK, 1)
        fd.seek(0)
        return
    raise RuntimeError("No supported pidfile locking mechanism for this platform")


def acquire_pidfile():
    """Acquire exclusive lock on PID file. Exit if another instance running."""
    # Open r+/create without truncating so we can read existing PID on failure
    if not os.path.exists(PIDFILE):
        open(PIDFILE, "w").close()
    fd = open(PIDFILE, "r+")
    try:
        _lock_pidfile(fd)
    except OSError:
        try:
            existing_pid = fd.read().strip() or "unknown"
        except Exception:
            existing_pid = "unknown"
        print(
            f"Another bridge instance is running (PID {existing_pid}). Exiting.",
            file=sys.stderr,
        )
        sys.exit(1)
    except RuntimeError as e:
        print(f"Cannot lock PID file: {e}", file=sys.stderr)
        sys.exit(1)
    fd.seek(0)
    fd.truncate()
    fd.write(str(os.getpid()) + "\n")
    fd.flush()
    return fd  # keep open — lock auto-releases on process death


def _cleanup():
    """Remove health and PID files on clean shutdown."""
    for path in [HEALTH_FILE, PIDFILE]:
        try:
            os.unlink(path)
        except OSError:
            pass


atexit.register(_cleanup)
signal.signal(signal.SIGTERM, lambda *_: sys.exit(0))  # triggers atexit


def sd_notify(state):
    """Send sd_notify state if running under systemd."""
    addr = os.environ.get("NOTIFY_SOCKET")
    if not addr:
        return
    sock = socket.socket(socket.AF_UNIX, socket.SOCK_DGRAM)
    try:
        if addr.startswith("@"):
            addr = "\0" + addr[1:]
        sock.sendto(state.encode(), addr)
    finally:
        sock.close()


ARTIFACT_REF_PATTERNS = [
    re.compile(r"https?://github\.com/\S+/(?:pull|issues)/\d+", re.IGNORECASE),
    re.compile(r"\bPR\s*#\d+\b", re.IGNORECASE),
    re.compile(r"\b(?:commit|sha)\s*[:#]?\s*([0-9a-f]{7,40})\b", re.IGNORECASE),
    re.compile(
        r"(?:/|\.{1,2}/|~?/)?[A-Za-z0-9._-]+(?:/[A-Za-z0-9._-]+)+\.(?:clj|cljs|cljc|el|md|txt|sh|py|js|ts|tsx|java|go|rs|tex|json|edn)\b",
        re.IGNORECASE,
    ),
]

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


def api_get(url, timeout=CMD_TIMEOUT):
    """GET JSON from a futon3c API endpoint."""
    req = urllib.request.Request(url, method="GET")
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
                 handle_commands=False, channels=None):
        self.desired_nick = nick  # the nick we want
        self.nick = nick
        self.agent_id = agent_id
        self.channel = channel  # primary channel (for compat)
        self.channels = channels or [channel]  # all channels to join
        self.host = host
        self.port = port
        self.password = password
        self.sock = None
        self.buf = ""
        self.handle_commands = handle_commands
        self.focused_mission = None
        self.connected = False
        self._reply_channel = channel  # channel of most recent inbound message
        self._invoking = threading.Lock()
        self._invoke_queue = queue.Queue(maxsize=INVOKE_QUEUE_MAX)
        self._job_seq = 0
        self._job_seq_lock = threading.Lock()
        self._send_lock = threading.Lock()
        self._worker = threading.Thread(
            target=self._invoke_worker_loop,
            name=f"{self.nick}-invoke-worker",
            daemon=True,
        )
        self._worker.start()

    def health_snapshot(self):
        """Return a dict summarising this bot's health."""
        return {
            "desired_nick": self.desired_nick,
            "current_nick": self.nick,
            "nick_ok": self.nick == self.desired_nick,
            "agent_id": self.agent_id,
            "connected": self.connected,
            "queue_depth": self._invoke_queue.qsize(),
            "queue_max": INVOKE_QUEUE_MAX,
            "handle_commands": self.handle_commands,
            "summary_mode": CODEX_BRIDGE_SUMMARY_MODE,
        }

    def _is_brief(self, text):
        """True when the message looks like casual IRC chat, not a task."""
        stripped = text.strip()
        return (len(stripped) < 100
                and "```" not in stripped
                and "\n" not in stripped)

    def _surface_context(self, sender, mission_part, brief, channel=None):
        """Build the surface contract header for an IRC invoke."""
        ch = channel or self.channel
        if brief:
            return (
                f"[Surface: IRC | Channel: {ch} | "
                f"Speaker: {sender}{mission_part} | Mode: brief | "
                f"This is casual IRC chat. Respond in 1-2 short lines. "
                f"Your reply will be posted to {ch} as <{self.nick}>.]"
            )
        return (
            f"[Surface: IRC | Channel: {ch} | "
            f"Speaker: {sender}{mission_part} | Mode: task | "
            f"Your completion update will be posted to {ch} as <{self.nick}>. "
            "Execute work asynchronously, then return a short status with artifact refs "
            "(commit/PR/issue/file path). "
            "To post progress mid-task: "
            f'curl -s -X POST {INVOKE_BASE}/api/alpha/irc/send '
            '-H "Content-Type: application/json" '
            f'-d \'{{"channel":"{ch}","from":"{self.nick}","text":"..."}}\']'
        )

    def _agent_status(self):
        """Fetch this bot agent's status from Agency."""
        data = api_get(f"{AGENTS_URL}/{self.agent_id}", timeout=STATUS_TIMEOUT)
        if not data.get("ok"):
            return None
        agent = data.get("agent", {})
        if not isinstance(agent, dict):
            return None
        return {
            "status": agent.get("status", "unknown"),
            "session_id": agent.get("session-id")
            or agent.get("session_id")
            or "unknown",
            "invoke_started_at": agent.get("invoke-started-at")
            or agent.get("invoke_started_at"),
            "invoke_activity": agent.get("invoke-activity")
            or agent.get("invoke_activity"),
        }

    def _agent_busy_summary(self, status):
        """Format a compact busy summary for IRC."""
        if not isinstance(status, dict):
            return f"{self.agent_id} appears busy"
        sid = status.get("session_id") or "unknown"
        activity = status.get("invoke_activity")
        started = status.get("invoke_started_at")
        parts = [f"{self.agent_id} is already invoking", f"session={sid}"]
        if isinstance(activity, str) and activity.strip():
            parts.append(f"activity={activity.strip()[:90]}")
        if isinstance(started, str) and started.strip():
            parts.append(f"since={started.strip()}")
        return " | ".join(parts)

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

        for ch in self.channels:
            self._send(f"JOIN {ch}")
            log(self.nick, f"Joined {ch}")
        self.connected = True

    def _send(self, line):
        """Send a raw IRC line."""
        payload = (line + "\r\n").encode("utf-8")
        # Multiple worker/command threads can write concurrently; serialize
        # socket writes so IRC commands are never interleaved mid-line.
        with self._send_lock:
            if self.sock is None:
                raise ConnectionError("IRC socket is not connected")
            self.sock.sendall(payload)

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

    def _mention_names(self):
        """Return acceptable mention forms for this bot."""
        names = []
        seen = set()
        for candidate in (self.nick, self.desired_nick, self.nick.rstrip("_")):
            cand = (candidate or "").strip()
            if not cand:
                continue
            key = cand.lower()
            if key in seen:
                continue
            seen.add(key)
            names.append(cand)
        return names

    def _is_mention(self, text):
        """Check if text mentions this bot. Matches @nick anywhere in the
        text (not just line start) so mentions like 'done.@codex review'
        still trigger. Also matches 'nick: ...' at line start.
        Uses (?!\\w|-) instead of \\b so @claude doesn't match @claude-2."""
        end = r"(?!\w|-)"  # not followed by word char or hyphen
        for name in self._mention_names():
            patterns = [
                rf"@{re.escape(name)}{end}",  # @nick anywhere
                rf"^{re.escape(name)}:\s",    # nick: at start
                rf"^{re.escape(name)},\s",    # nick, at start
            ]
            for p in patterns:
                if re.search(p, text, re.IGNORECASE):
                    return True
        return False

    def _strip_mention(self, text):
        """Remove the mention prefix from the text."""
        for name in self._mention_names():
            updated = re.sub(
                rf"^@?{re.escape(name)}[,:]\s*",
                "",
                text,
                count=1,
                flags=re.IGNORECASE,
            )
            if updated != text:
                text = updated
                break
        return text.strip()

    def _next_job_id(self):
        """Create a short monotonic job id for async invoke tracking."""
        with self._job_seq_lock:
            self._job_seq += 1
            seq = self._job_seq
        return f"{self.nick}-{int(time.time())}-{seq}"

    @staticmethod
    def _truncate(text, max_len=220):
        text = (text or "").strip()
        if len(text) <= max_len:
            return text
        return text[: max(0, max_len - 3)] + "..."

    @staticmethod
    def _extract_artifact_refs(text, max_refs=3):
        refs = []
        src = text or ""
        for pattern in ARTIFACT_REF_PATTERNS:
            for match in pattern.finditer(src):
                if match.lastindex:
                    ref = match.group(1)
                else:
                    ref = match.group(0)
                ref = (ref or "").strip()
                if not ref:
                    continue
                if ref not in refs:
                    refs.append(ref)
                if len(refs) >= max_refs:
                    return refs
        return refs

    def _summarize_invoke_result(self, result_text, clean=False):
        """Return a short IRC-safe summary with artifact refs when available.
        If clean=True, return the text without artifact ref annotations."""
        text = self._sanitize_for_irc(result_text or "")
        compact = re.sub(r"\s+", " ", text).strip()
        if clean:
            return self._truncate(compact, max_len=400) if compact else "[no response]"
        refs = self._extract_artifact_refs(text)
        if compact.startswith("{") or compact.startswith("["):
            summary = "Structured output generated."
        elif compact:
            summary = self._truncate(compact, max_len=180)
        else:
            summary = "[no textual response]"
        if refs:
            return f"{summary} refs: {', '.join(refs)}"
        return f"{summary} (no artifact refs)"

    @staticmethod
    def _execution_stats(invoke_meta):
        """Extract execution-evidence stats from invoke metadata."""
        if not isinstance(invoke_meta, dict):
            return None
        execution = invoke_meta.get("execution")
        if not isinstance(execution, dict):
            return None
        executed = bool(execution.get("executed?"))
        try:
            tool_events = int(execution.get("tool-events") or 0)
        except (TypeError, ValueError):
            tool_events = 0
        try:
            command_events = int(execution.get("command-events") or 0)
        except (TypeError, ValueError):
            command_events = 0
        return {
            "executed": executed,
            "tool_events": tool_events,
            "command_events": command_events,
        }

    @staticmethod
    def _invoke_trace_id(invoke_meta):
        """Extract invoke trace id from invoke metadata."""
        if not isinstance(invoke_meta, dict):
            return None
        for key in ("invoke-trace-id", "invoke_trace_id", "invokeTraceId"):
            value = invoke_meta.get(key)
            if isinstance(value, str) and value.strip():
                return value.strip()
        return None

    def _record_delivery_receipt(self, invoke_meta, delivered, note):
        """Record where an invoke reply was delivered for trace visibility."""
        trace_id = self._invoke_trace_id(invoke_meta)
        if not trace_id:
            return False
        payload = {
            "agent-id": self.agent_id,
            "invoke-trace-id": trace_id,
            "surface": "irc",
            "destination": f"{self._reply_channel or self.channel} as <{self.nick}>",
            "delivered": bool(delivered),
            "note": note,
        }
        result = api_post(INVOKE_DELIVERY_URL, payload, timeout=min(CMD_TIMEOUT, 5))
        if not (isinstance(result, dict) and result.get("ok")):
            log(self.nick, f"invoke-delivery record failed for {trace_id}: {result}")
            return False
        return True

    def _enqueue_invoke(self, sender, full_prompt, mission_id, reply_channel=None):
        """Queue an invoke request and return assigned job id or None when full."""
        job_id = self._next_job_id()
        task = {
            "job_id": job_id,
            "sender": sender,
            "prompt": full_prompt,
            "mission_id": mission_id,
            "reply_channel": reply_channel or self.channel,
            "queued_at": time.time(),
        }
        try:
            self._invoke_queue.put_nowait(task)
        except queue.Full:
            return None
        return job_id

    def _invoke_worker_loop(self):
        """Process queued invoke jobs serially and post completion updates."""
        while True:
            task = self._invoke_queue.get()
            if task is None:
                self._invoke_queue.task_done()
                return
            job_id = task["job_id"]
            sender = task["sender"]
            prompt = task["prompt"]
            mission_id = task.get("mission_id")
            reply_ch = task.get("reply_channel") or self.channel
            self._reply_channel = reply_ch  # for delivery receipt
            response = {"ok": False}
            invoke_meta = None
            try:
                with self._invoking:
                    response = self._invoke_agent(prompt, sender, mission_id=mission_id)
                invoke_meta = response.get("invoke_meta") if isinstance(response, dict) else None
                if response.get("ok"):
                    # Claude: clean output (no [done]/[accepted] framing)
                    # Codex: full framing with artifact refs and session IDs
                    is_claude = self.nick.startswith("claude")
                    if is_claude:
                        summary = self._summarize_invoke_result(response.get("result", ""), clean=True)
                        self._say(summary, max_lines=4, channel=reply_ch)
                    else:
                        summary = self._summarize_invoke_result(response.get("result", ""))
                        stats = self._execution_stats(response.get("invoke_meta"))
                        execution_note = ""
                        if self.agent_id.startswith("codex") and isinstance(stats, dict) and not stats["executed"]:
                            execution_note = (
                                f"[no execution evidence: tool-events={stats['tool_events']}, "
                                f"command-events={stats['command_events']}]"
                            )
                            summary = f"{summary} {execution_note}".strip()
                        sid = response.get("session_id")
                        if CODEX_USE_RAW_OUTPUT:
                            raw_text = self._sanitize_for_irc(response.get("result", "") or "").strip()
                            if raw_text:
                                refs = self._extract_artifact_refs(raw_text)
                                header_parts = [f"[done {job_id}]"]
                                if sid:
                                    header_parts.append(f"(session {sid[:8]})")
                                if refs:
                                    header_parts.append(f"refs: {', '.join(refs[:3])}")
                                if execution_note:
                                    header_parts.append(execution_note)
                                header = " ".join(part for part in header_parts if part).strip()
                                self._say(f"{header}\n{raw_text}", max_lines=6, channel=reply_ch)
                                continue
                        if sid:
                            self._say(f"[done {job_id}] {summary} (session {sid[:8]})",
                                      max_lines=2, channel=reply_ch)
                        else:
                            self._say(f"[done {job_id}] {summary}",
                                      max_lines=2, channel=reply_ch)
                    self._record_delivery_receipt(
                        invoke_meta,
                        delivered=True,
                        note=f"ngircd-bridge:{job_id}:ok",
                    )
                else:
                    err = self._truncate(response.get("error", "unknown invoke error"), max_len=220)
                    self._say(f"[failed {job_id}] {err}", max_lines=2, channel=reply_ch)
                    self._record_delivery_receipt(
                        invoke_meta,
                        delivered=True,
                        note=f"ngircd-bridge:{job_id}:invoke-failed",
                    )
            except Exception as e:
                sent = False
                try:
                    self._say(
                        f"[failed {job_id}] worker exception: {self._truncate(str(e), max_len=180)}",
                        max_lines=2, channel=reply_ch,
                    )
                    sent = True
                except Exception as send_error:
                    log(self.nick, f"Failed sending worker exception for {job_id}: {send_error}")
                self._record_delivery_receipt(
                    invoke_meta,
                    delivered=sent,
                    note=f"ngircd-bridge:{job_id}:worker-exception",
                )
                log(self.nick, f"Worker exception for {job_id}: {traceback.format_exc()}")
            finally:
                self._invoke_queue.task_done()

    def _invoke_agent(self, prompt, caller, mission_id=None):
        """Call the futon3c invoke API and return structured invoke outcome."""
        status_before = self._agent_status()
        if (
            INVOKE_SKIP_WHEN_BUSY
            and isinstance(status_before, dict)
            and status_before.get("status") == "invoking"
        ):
            return {
                "ok": False,
                "error": f"invoke skipped: {self._agent_busy_summary(status_before)}",
                "session_id": status_before.get("session_id"),
            }

        payload = {
            "agent-id": self.agent_id,
            "prompt": prompt,
            "caller": f"irc:{caller}",
            "surface": f"irc ({self.channel})",
            "timeout-ms": INVOKE_TIMEOUT_SECONDS * 1000,
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
            with urllib.request.urlopen(req, timeout=INVOKE_CLIENT_TIMEOUT_SECONDS) as resp:
                data = json.loads(resp.read())
                invoke_meta = data.get("invoke-meta") or data.get("invoke_meta")
                if data.get("ok"):
                    return {
                        "ok": True,
                        "result": data.get("result", ""),
                        "session_id": data.get("session-id") or data.get("session_id"),
                        "invoke_meta": invoke_meta,
                    }
                else:
                    return {
                        "ok": False,
                        "error": f"invoke error: {data.get('error', 'unknown')}",
                        "session_id": data.get("session-id") or data.get("session_id"),
                        "invoke_meta": invoke_meta,
                    }
        except urllib.error.HTTPError as e:
            body_text = e.read().decode("utf-8", errors="replace")
            parsed = None
            try:
                parsed = json.loads(body_text)
            except Exception:
                parsed = None
            if isinstance(parsed, dict):
                err = parsed.get("error", "unknown")
                msg = parsed.get("message", "")
                invoke_meta = parsed.get("invoke-meta") or parsed.get("invoke_meta")
                timed_out = (
                    e.code == 502
                    and isinstance(msg, str)
                    and "timeout" in msg.lower()
                )
                if timed_out:
                    status_after = self._agent_status()
                    if isinstance(status_after, dict) and status_after.get("status") == "invoking":
                        return {
                            "ok": False,
                            "error": f"invoke timeout: {self._agent_busy_summary(status_after)}",
                            "session_id": status_after.get("session_id"),
                            "invoke_meta": invoke_meta,
                        }
                if msg:
                    return {
                        "ok": False,
                        "error": f"HTTP {e.code}: {err}: {msg}",
                        "invoke_meta": invoke_meta,
                    }
                return {
                    "ok": False,
                    "error": f"HTTP {e.code}: {err}",
                    "invoke_meta": invoke_meta,
                }
            return {"ok": False, "error": f"HTTP {e.code}: {body_text[:200]}"}
        except urllib.error.URLError as e:
            reason = str(getattr(e, "reason", e))
            if "timed out" in reason.lower():
                status_after = self._agent_status()
                if isinstance(status_after, dict) and status_after.get("status") == "invoking":
                    return {
                        "ok": False,
                        "error": f"invoke timeout: {self._agent_busy_summary(status_after)}",
                        "session_id": status_after.get("session_id"),
                    }
            return {"ok": False, "error": f"invoke failed: {reason}"}
        except socket.timeout:
            status_after = self._agent_status()
            if isinstance(status_after, dict) and status_after.get("status") == "invoking":
                return {
                    "ok": False,
                    "error": f"invoke timeout: {self._agent_busy_summary(status_after)}",
                    "session_id": status_after.get("session_id"),
                }
            return {"ok": False, "error": "invoke failed: timed out"}
        except Exception as e:
            return {"ok": False, "error": f"invoke failed: {e}"}

    @staticmethod
    def _sanitize_for_irc(text):
        """Strip markdown formatting that doesn't render in IRC.
        Agents sometimes emit markdown despite surface contract instructions;
        this ensures it never reaches the wire."""
        # Strip code fences (``` ... ```)
        text = re.sub(r"```\w*\n?", "", text)
        # Strip bold **text** or __text__
        text = re.sub(r"\*\*(.+?)\*\*", r"\1", text)
        text = re.sub(r"__(.+?)__", r"\1", text)
        # Strip italic *text* or _text_ (but not underscores in identifiers)
        text = re.sub(r"(?<!\w)\*(.+?)\*(?!\w)", r"\1", text)
        # Strip headers (## Foo → Foo)
        text = re.sub(r"^#{1,6}\s+", "", text, flags=re.MULTILINE)
        # Strip bullet prefixes (- item → item)
        text = re.sub(r"^[-*]\s+", "", text, flags=re.MULTILINE)
        # Strip numbered list prefixes (1. item → item)
        text = re.sub(r"^\d+\.\s+", "", text, flags=re.MULTILINE)
        # Collapse multiple spaces
        text = re.sub(r"  +", " ", text)
        return text

    def _say(self, text, max_lines=6, channel=None):
        """Send a PRIVMSG to a channel. Caps output at max_lines to keep
        IRC readable. If the response exceeds max_lines, the tail is dropped
        and a truncation notice is appended."""
        channel = channel or self.channel
        text = self._sanitize_for_irc(text)
        lines = []
        for line in text.split("\n"):
            line = line.rstrip()
            if not line:
                continue
            # Wrap long lines at IRC limit
            while len(line) > MAX_IRC_LINE:
                lines.append(line[:MAX_IRC_LINE])
                line = line[MAX_IRC_LINE:]
            lines.append(line)

        truncated = len(lines) > max_lines
        for line in lines[:max_lines]:
            self._send(f"PRIVMSG {channel} :{line}")
            time.sleep(0.1)  # gentle rate limit
        if truncated:
            self._send(f"PRIVMSG {channel} :"
                       f"[truncated {len(lines) - max_lines} more lines — "
                       f"post details to GitHub instead]")
            time.sleep(0.1)

    def _handle_mention(self, sender, text, channel=None):
        """Process a mention: queue invoke and ack immediately."""
        channel = channel or self.channel
        prompt_text = self._strip_mention(text)
        if not prompt_text:
            return

        # Surface contract: tell the agent where its output goes
        mission_part = ""
        if self.focused_mission:
            mission_part = f" | Focused Mission: {self.focused_mission}"
        brief = self._is_brief(prompt_text)
        surface_context = self._surface_context(sender, mission_part, brief, channel=channel)
        full_prompt = f"{surface_context}\n\n{sender}: {prompt_text}"

        job_id = self._enqueue_invoke(sender, full_prompt, self.focused_mission,
                                      reply_channel=channel)
        if not job_id:
            self._say("[queue full] invoke queue is saturated; retry in a moment",
                      max_lines=1, channel=channel)
            log(self.nick, f"Queue full — dropping invocation from {sender}")
            return
        pending = self._invoke_queue.qsize()
        mode = "brief" if brief else "task"
        log(self.nick, f"Queued {job_id} ({mode}) from {sender} on {channel}: {prompt_text[:80]}")
        # Claude: no [accepted] noise — just process silently
        if not self.nick.startswith("claude"):
            self._say(
                f"[accepted {job_id}] queued ({pending} pending, timeout {INVOKE_TIMEOUT_SECONDS}s)",
                max_lines=1, channel=channel,
            )

    def _handle_ungated(self, sender, text, channel=None):
        """Process an ungated message: queue invoke and ack immediately."""
        channel = channel or self.channel
        if not text.strip():
            return

        mission_part = ""
        if self.focused_mission:
            mission_part = f" | Focused Mission: {self.focused_mission}"
        brief = self._is_brief(text)
        surface_context = self._surface_context(sender, mission_part, brief, channel=channel)
        full_prompt = f"{surface_context}\n\n{sender}: {text}"

        job_id = self._enqueue_invoke(sender, full_prompt, self.focused_mission,
                                      reply_channel=channel)
        if not job_id:
            self._say("[queue full] invoke queue is saturated; retry in a moment",
                      max_lines=1, channel=channel)
            log(self.nick, f"Queue full — dropping ungated invocation from {sender}")
            return
        pending = self._invoke_queue.qsize()
        mode = "brief" if brief else "task"
        log(self.nick, f"Queued {job_id} ({mode}/ungated) from {sender} on {channel}: {text[:80]}")
        if not self.nick.startswith("claude"):
            self._say(
                f"[accepted {job_id}] queued ({pending} pending, timeout {INVOKE_TIMEOUT_SECONDS}s)",
                max_lines=1, channel=channel,
            )

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
        elif cmd == "!agent":
            self._cmd_agent(sender, args)
        elif cmd == "!jobs":
            self._cmd_jobs(sender, args)
        else:
            self._say(f"Unknown command: {cmd} — try !help")

    def _cmd_help(self):
        """List available commands."""
        self._say("Commands: !ungate <nick> | !gate <nick> | "
                  "!reset <agent-id> | !mc status | !mc review | "
                  "!mc missions | !mc sessions | !mc diff | "
                  "!mission focus <id> | !mission show | "
                  "!mission clear | !todo add <text> | "
                  "!todo list | !todo done <id> | "
                  "!agent [agent-id] | !jobs | !help")

    def _cmd_jobs(self, _sender, _args):
        """Show invoke queue depth for this bot."""
        pending = self._invoke_queue.qsize()
        self._say(f"Invoke queue: {pending} pending for {self.agent_id}", max_lines=1)

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
                emitted = False
                for status in ["in-progress", "ready", "blocked",
                               "deferred", "nonstarter",
                               "complete", "unknown"]:
                    items = by_status.get(status, [])
                    if items:
                        emitted = True
                        names = ", ".join(m.get("mission/id", "?")
                                          for m in items[:8])
                        extra = (f" +{len(items)-8}" if len(items) > 8
                                 else "")
                        self._say(f"[{status}] ({len(items)}): {names}{extra}")
                if not emitted:
                    summary = result.get("portfolio/summary")
                    if summary:
                        self._say(f"No missions found. {summary}")
                    else:
                        self._say("No missions found.")
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

    def _cmd_agent(self, _sender, args):
        """Show live agent status from /api/alpha/agents/:id."""
        agent_id = args.strip() or self.agent_id
        data = api_get(f"{AGENTS_URL}/{agent_id}", timeout=STATUS_TIMEOUT)
        if not data.get("ok"):
            err = data.get("error", "unknown")
            self._say(f"[agent status error: {err}]")
            return
        agent = data.get("agent", {})
        if not isinstance(agent, dict):
            self._say(f"[agent status error: invalid payload for {agent_id}]")
            return
        status = agent.get("status", "unknown")
        sid = agent.get("session-id") or agent.get("session_id") or "none"
        activity = agent.get("invoke-activity") or agent.get("invoke_activity")
        started = agent.get("invoke-started-at") or agent.get("invoke_started_at")
        msg = f"{agent_id}: status={status} session={sid}"
        if isinstance(activity, str) and activity.strip():
            msg += f" activity={activity.strip()[:120]}"
        if isinstance(started, str) and started.strip():
            msg += f" since={started.strip()}"
        self._say(msg)

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
                        # Try to reclaim desired nick on each PING cycle
                        if self.nick != self.desired_nick:
                            self._send(f"NICK {self.desired_nick}")

                    elif cmd == "NICK" and prefix:
                        # Server confirmed our nick change
                        new_nick = params[0] if params else ""
                        old_nick = prefix.split("!")[0] if "!" in prefix else prefix
                        if old_nick == self.nick and new_nick:
                            log(self.nick, f"Nick changed to {new_nick}")
                            self.nick = new_nick

                    elif cmd == "QUIT" and prefix:
                        # Someone quit — if it was our desired nick, reclaim
                        quitter = prefix.split("!")[0] if "!" in prefix else prefix
                        if (quitter == self.desired_nick
                                and self.nick != self.desired_nick):
                            log(self.nick, f"{quitter} quit, reclaiming nick")
                            self._send(f"NICK {self.desired_nick}")

                    elif cmd == "PRIVMSG" and len(params) >= 2:
                        target = params[0]
                        text = params[1]
                        sender = self._sender_nick(prefix)

                        # Match any channel we've joined
                        channels_lower = {ch.lower() for ch in self.channels}
                        if (target.lower() in channels_lower
                                and sender.lower() != self.nick.lower()):
                            # Track which channel this message came from
                            self._reply_channel = target

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
                                    args=(sender, text, target),
                                    daemon=True,
                                )
                                t.start()

                            # Ungated mode — respond to all messages
                            elif self.nick.rstrip("_").lower() in ungated_nicks:
                                t = threading.Thread(
                                    target=self._handle_ungated,
                                    args=(sender, text, target),
                                    daemon=True,
                                )
                                t.start()

            except Exception as e:
                log(self.nick, f"Error: {e}")
                traceback.print_exc()

            # Reconnect
            self.connected = False
            if self.sock:
                try:
                    self.sock.close()
                except Exception:
                    pass
                self.sock = None

            log(self.nick, f"Reconnecting in {RECONNECT_DELAY}s...")
            time.sleep(RECONNECT_DELAY)


def _write_health(bots, started_at):
    """Atomically write bridge health JSON to HEALTH_FILE."""
    health = {
        "pid": os.getpid(),
        "started_at": started_at,
        "updated_at": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()),
        "invoke_base": INVOKE_BASE,
        "summary_mode": CODEX_BRIDGE_SUMMARY_MODE,
        "raw_output": CODEX_USE_RAW_OUTPUT,
        "irc_host": IRC_HOST,
        "irc_port": IRC_PORT,
        "channel": IRC_CHANNEL,
        "bots": [b.health_snapshot() for b in bots],
    }
    tmp = HEALTH_FILE + ".tmp"
    try:
        with open(tmp, "w") as f:
            json.dump(health, f)
        os.replace(tmp, HEALTH_FILE)
    except Exception as e:
        log("bridge", f"Health file write failed: {e}")


BRIDGE_HTTP_PORT = int_env("BRIDGE_HTTP_PORT", 6769, minimum=1)


def _make_say_handler(bots_by_nick):
    """Create an HTTP request handler that routes /say to the right bot."""

    class SayHandler(http.server.BaseHTTPRequestHandler):
        def do_POST(self):
            if self.path != "/say":
                self.send_error(404)
                return
            length = int(self.headers.get("Content-Length", 0))
            body = self.rfile.read(length) if length else b""
            try:
                payload = json.loads(body)
            except Exception:
                self._json(400, {"ok": False, "err": "invalid-json"})
                return
            text = payload.get("text", "")
            from_nick = payload.get("from", "claude")
            if not text.strip():
                self._json(400, {"ok": False, "err": "missing-text"})
                return
            bot = bots_by_nick.get(from_nick)
            if not bot:
                self._json(404, {"ok": False, "err": "unknown-nick",
                                 "available": list(bots_by_nick.keys())})
                return
            max_lines = payload.get("max_lines", 4)
            channel = payload.get("channel")  # optional: target specific channel
            bot._say(text, max_lines=max_lines, channel=channel)
            self._json(200, {"ok": True, "from": from_nick, "text": text,
                             "channel": channel or bot.channel})

        def _json(self, code, obj):
            body = json.dumps(obj).encode()
            self.send_response(code)
            self.send_header("Content-Type", "application/json")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)

        def log_message(self, format, *args):
            pass  # suppress default stderr logging

    return SayHandler


def _start_bridge_http(bots):
    """Start a tiny HTTP server on BRIDGE_HTTP_PORT for /say."""
    bots_by_nick = {b.nick: b for b in bots}
    handler = _make_say_handler(bots_by_nick)
    try:
        server = http.server.HTTPServer(("127.0.0.1", BRIDGE_HTTP_PORT), handler)
    except OSError as e:
        log("bridge", f"HTTP /say endpoint failed on port {BRIDGE_HTTP_PORT}: {e}")
        return None
    t = threading.Thread(target=server.serve_forever, name="bridge-http", daemon=True)
    t.start()
    log("bridge", f"HTTP /say endpoint on 127.0.0.1:{BRIDGE_HTTP_PORT}")
    return server


def main():
    _pidfile_fd = acquire_pidfile()  # noqa: F841 — must stay open

    nick_to_agent = {
        "claude": "claude-1",
        "claude-2": "claude-2",
        "codex": "codex-1",
    }
    # NICK_AGENT_MAP overrides: "zcodex:codex-1,zclaude:claude-1"
    for pair in os.environ.get("NICK_AGENT_MAP", "").split(","):
        pair = pair.strip()
        if ":" in pair:
            n, a = pair.split(":", 1)
            nick_to_agent[n.strip()] = a.strip()

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
            channels=IRC_CHANNELS,
        )
        bots.append(bot)

    if not bots:
        print("No bots configured. Set BRIDGE_BOTS env var.", file=sys.stderr)
        sys.exit(1)

    started_at = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())

    print(
        f"Starting ngircd bridge: {[b.nick for b in bots]} → {INVOKE_BASE} "
        f"(source: {INVOKE_BASE_SOURCE})"
    )
    channels_str = ", ".join(IRC_CHANNELS) if len(IRC_CHANNELS) > 1 else IRC_CHANNEL
    print(f"IRC: {IRC_HOST}:{IRC_PORT} | Channels: {channels_str}")
    print(
        f"Codex bridge summary mode: {CODEX_BRIDGE_SUMMARY_MODE} "
        f"(raw_output={'yes' if CODEX_USE_RAW_OUTPUT else 'no'})"
    )
    print(f"Commands handled by: {bots[0].nick}")

    threads = []
    for bot in bots:
        t = threading.Thread(target=bot.run, name=f"bot-{bot.nick}", daemon=True)
        t.start()
        threads.append(t)
        time.sleep(0.5)  # stagger connections

    _bridge_http = _start_bridge_http(bots)  # noqa: F841

    sd_notify("READY=1")

    # Health-write loop (replaces plain sleep)
    try:
        while True:
            _write_health(bots, started_at)
            sd_notify("WATCHDOG=1")
            time.sleep(HEALTH_INTERVAL)
    except KeyboardInterrupt:
        print("\nShutting down bridge...")
        sys.exit(0)


if __name__ == "__main__":
    main()
