#!/usr/bin/env python3
"""xiang_turns.py — the bridges' client for the 象 turn routes.

A transport (IRC, Matrix) that routes a message to an agent records it as an
operator turn, and when the reply lands tells 象 what happened and records
the reply as an agent turn, so the room's turns get the same annotations an
Emacs REPL's do (M-象-2000, P23).

The reply is recorded with dispatch "none" (analysis_status "declared"): it
is stored but never read by 象. Joe 2026-10-02: an agent's turn carries its
own proforma marks (stored as proforma_marks on agent-origin records), 象
readings of them were not wanted, and the extra jobs slowed 象's readings of
operator turns. FUTON3C_XIANG_AGENT_DISPATCH=1 restores the old dispatch
"now" for when agent-turn readings are wanted again (e.g. to measure the
marks against 象's reading).

  record_turn(text, agent_id, session_id, turn_id, operator_id=…, surface=…)
      -> record id, or None when the route refused or was unreachable
  after_reply(record_id, reply, agent_id, session_id, turn_id, surface=…)
      -> {"happened": …, "reply_record": …}

Everything is best effort and never raises into the bridge: a turn that
cannot be recorded is logged and the conversation goes on, which is the
same rule session-turn-analysis.el applies ("the record stays requested").
Kill switch: FUTON3C_XIANG_BRIDGE=0.
"""
from __future__ import annotations

import json
import os
import urllib.error
import urllib.request

TIMEOUT = float(os.environ.get("FUTON3C_XIANG_BRIDGE_TIMEOUT", "5"))


def enabled() -> bool:
    return os.environ.get("FUTON3C_XIANG_BRIDGE", "1").strip().lower() not in ("0", "false", "no", "off")


def agent_dispatch_enabled() -> bool:
    """Whether agent replies are dispatched to 象. Off by default; see the
    module docstring for why and for when to turn it back on."""
    return os.environ.get("FUTON3C_XIANG_AGENT_DISPATCH", "0").strip().lower() in ("1", "true", "yes", "on")


def post_json(url: str, payload: dict, timeout: float = TIMEOUT) -> tuple[int, dict | None]:
    """POST JSON; return (status, parsed body or None). Never raises."""
    body = json.dumps(payload).encode("utf-8")
    req = urllib.request.Request(url, data=body, method="POST",
                                 headers={"Content-Type": "application/json"})
    try:
        with urllib.request.urlopen(req, timeout=timeout) as resp:
            return resp.status, _parse(resp.read())
    except urllib.error.HTTPError as e:
        return e.code, _parse(e.read())
    except Exception as e:  # unreachable, timeout, bad URL
        return 0, {"ok": False, "error": f"{type(e).__name__}: {e}"}


def _parse(raw: bytes) -> dict | None:
    try:
        data = json.loads(raw.decode("utf-8", errors="replace"))
        return data if isinstance(data, dict) else None
    except Exception:
        return None


# Patched by tests; the bridge calls through this name.
POST = post_json


def record_turn(base: str, text: str, agent_id: str, session_id: str, turn_id: str, *,
                operator_id: str | None = None, surface: str | None = None,
                origin: str = "operator", evidence_id: str | None = None,
                dispatch: str = "later", log=None) -> str | None:
    """Record one turn; return its record id or None."""
    if not enabled() or not text or not text.strip():
        return None
    payload = {"text": text, "agent-id": agent_id, "session-id": session_id or "unknown",
               "turn-id": turn_id, "origin": origin, "dispatch": dispatch}
    if operator_id:
        payload["operator-id"] = operator_id
    if surface:
        payload["surface"] = surface
    if evidence_id:
        payload["evidence-id"] = evidence_id
    status, body = POST(f"{base.rstrip('/')}/api/alpha/xiang/turns", payload)
    rid = (body or {}).get("id") if status == 201 else None
    if not rid and log:
        log(f"象: turn {turn_id} not recorded (http {status}: {(body or {}).get('reason') or (body or {}).get('error')})")
    return rid


def happened(base: str, record_id: str, reply: str, *, log=None) -> dict | None:
    """Attach the reply to the operator turn's record and dispatch it to 象."""
    if not enabled() or not record_id:
        return None
    status, body = POST(f"{base.rstrip('/')}/api/alpha/xiang/turns/{record_id}/happened",
                        {"reply": reply or "", "commits": []})
    if status != 200 and log:
        log(f"象: happened for {record_id} refused (http {status}: {(body or {}).get('reason')})")
    return body


def after_reply(base: str, record_id: str | None, reply: str, agent_id: str, session_id: str,
                turn_id: str, *, surface: str | None = None,
                reply_evidence_id: str | None = None, log=None) -> dict:
    """Reply end: dispatch the operator turn, then record the reply as an agent turn.
    The reply is recorded with dispatch "none" (stored, never read by 象; see the
    module docstring) unless FUTON3C_XIANG_AGENT_DISPATCH restores dispatch "now"."""
    out = {"happened": None, "reply_record": None}
    if record_id:
        out["happened"] = happened(base, record_id, reply, log=log)
    out["reply_record"] = record_turn(base, reply, agent_id, session_id, f"{turn_id}:reply",
                                      origin="agent", surface=surface, evidence_id=reply_evidence_id,
                                      dispatch="now" if agent_dispatch_enabled() else "none", log=log)
    return out
