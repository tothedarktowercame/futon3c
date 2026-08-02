#!/usr/bin/env python3
"""Robust client for Agency bell / whistle — reads the prompt from STDIN.

The inline-curl form (`curl -d '{"prompt":"...don't..."}'`) breaks on apostrophes,
parens, and unicode because the shell mangles the single-quoted payload. This reads
the prompt from stdin and JSON-encodes it in-process, so a quoted heredoc is safe
for ANY characters:

    python3 futon3c/scripts/agency_send.py --to codex-3 --kind whistle <<'EOF'
    Anything goes here — apostrophes (don't), parens (f), unicode μ/κ/β/≥, newlines.
    EOF

--kind bell    -> POST /api/alpha/bell    (async, 202 + job-id)
--kind whistle -> POST /api/alpha/whistle (blocking, terminal JSON)
--type query   -> typed bell performative when FUTON3C_TYPED_BELLS is enabled
--ref ask-...  -> ArSE thread / referent for answer or routed query
--dry-run prints the payload instead of sending.
"""
import sys, json, argparse, time, urllib.request

ap = argparse.ArgumentParser()
ap.add_argument("--to", required=True, help="recipient agent-id")
ap.add_argument("--from", dest="frm", help="sender agent-id (recorded as the mesh edge's caller; "
                                           "enables mesh_trace + auto-bellback routing)")
ap.add_argument("--kind", choices=["bell", "whistle"], default="bell")
ap.add_argument("--base", default="http://localhost:7070")
ap.add_argument("--type", choices=["query", "answer", "assert", "challenge", "agree",
                                   "define", "retract", "suggest", "request"],
                help="typed-bell performative; server accepts it only when FUTON3C_TYPED_BELLS is on")
ap.add_argument("--ref", help="typed-bell referent, usually an ArSE thread id")
ap.add_argument("--mission", help="mission-id this dispatch works on; the server clocks the "
                "recipient's session to it (durable lineage, http.clj clock-dispatch!) so the "
                "agent appears on the live EFE map without a manual clock-in")
ap.add_argument("--mode", choices=["work", "brief"],
                help="explicit invoke-job mode; when omitted the server retains its legacy "
                     "prompt-text classification fallback")
ap.add_argument("--park", action="store_true",
                help="after a bell, park the sender's turn on the returned job-id")
ap.add_argument("--park-deadline", type=int, default=2700,
                help="seconds from now before the park deadline fires (default: 2700)")
ap.add_argument("--park-payload",
                help="continuation payload for /api/alpha/park")
ap.add_argument("--surface", default="emacs-repl",
                help="park surface to resume on (default: emacs-repl)")
# Bells carry substantial handoffs under the coding-handoff protocol, and the
# server's former 1800000 (30 min) cap discarded the result rather than harvesting it:
# the job goes state=failed with an empty result and the work is left uncommitted
# in the working tree, invisible unless someone goes looking. That has now cost
# four handoffs (see README-agency-cap.md). Making the generous value the DEFAULT
# rather than an opt-in flag is what closes it: a fix you must remember to pass,
# on every call, is not a fix.
#
# Whistles are synchronous and a caller is blocked on them, so they keep the
# server default.
BELL_DEFAULT_TIMEOUT_MS = 4 * 60 * 60 * 1000   # 4 hours

ap.add_argument("--timeout-ms", type=int,
                help="invoke timeout in ms. Defaults to %d (%d min) for --kind bell, "
                     "which is deliberately far above the server's 3600000 (60 min): "
                     "until the supervised-overrun fix reaches the codex relay route, "
                     "a turn hitting the cap is abandoned as state=failed and its "
                     "result is lost. Pass 0 to defer to the server default."
                     % (BELL_DEFAULT_TIMEOUT_MS, BELL_DEFAULT_TIMEOUT_MS // 60000))
ap.add_argument("--dry-run", action="store_true", help="print payload, do not send")
a = ap.parse_args()

prompt = sys.stdin.read()
if not prompt.strip():
    sys.exit("agency_send: empty prompt on stdin")

# Loud-failure for the load-bearing mesh edge (M-agency-hardening): a bell
# without --from logs as 'http-caller' with NO mesh edge, so auto-bellback has
# no recipient and the reply silently can't route back. Surface it at send time.
if a.park and a.kind != "bell":
    sys.exit("agency_send: --park is only valid with --kind bell")

if a.mode and a.kind != "bell":
    sys.exit("agency_send: --mode is only valid with --kind bell")

if a.park and not a.frm:
    sys.exit("agency_send: --park requires --from <id> so the sender's session can be parked")

if not a.frm:
    print("agency_send: WARNING — no --from <id>. This bell logs as 'http-caller' "
          "with NO mesh edge; auto-bellback cannot route a reply back to you. "
          "Pass --from <your-id>.", file=sys.stderr)

body = {"agent-id": a.to, "prompt": prompt}
if a.frm:
    body["caller"] = a.frm
if a.type:
    body["type"] = a.type
if a.ref:
    body["ref"] = a.ref
if a.mission:
    body["mission-id"] = a.mission
if a.mode:
    body["mode"] = a.mode
# Explicit --timeout-ms always wins; 0 means "defer to the server default".
if a.timeout_ms:
    body["timeout-ms"] = a.timeout_ms
elif a.timeout_ms is None and a.kind == "bell":
    body["timeout-ms"] = BELL_DEFAULT_TIMEOUT_MS
payload = json.dumps(body)


def get_json(url):
    return json.loads(urllib.request.urlopen(url).read().decode())


def post_json(url, obj):
    req = urllib.request.Request(url,
                                 data=json.dumps(obj).encode(),
                                 headers={"Content-Type": "application/json"})
    raw = urllib.request.urlopen(req).read().decode()
    return raw, json.loads(raw)


def resolve_session_id(base, agent_id):
    agents = get_json(f"{base}/api/alpha/agents")
    agent = (agents.get("agents") or {}).get(agent_id)
    if not agent:
        sys.exit(f"agency_send: --park could not find sender agent in registry: {agent_id}")
    session_id = agent.get("session-id")
    if not session_id:
        sys.exit(f"agency_send: --park sender has no session-id: {agent_id}")
    return session_id


def job_id_from_response(resp):
    job_id = resp.get("job-id") or resp.get("job_id")
    if not job_id:
        sys.exit("agency_send: --park could not parse job-id from bell response")
    return job_id


def park_body_for(job_id, session_id):
    deadline_ms = int((time.time() + a.park_deadline) * 1000)
    return {
        "agent": a.frm,
        "session": session_id,
        "surface": a.surface,
        "mode": "background",
        "awaiting": [job_id],
        "deadline-ms": deadline_ms,
        "payload": a.park_payload or
        f"review {a.to}'s reply to this dispatch per the coding-handoff protocol",
    }


if a.dry_run:
    print(payload)
    if a.park:
        session = f"<resolved-session-id-for-{a.frm}>"
        print(json.dumps(park_body_for("<bell-job-id>", session)))
    sys.exit(0)

bell_raw, bell_json = post_json(f"{a.base}/api/alpha/{a.kind}", body)
print(bell_raw)

if a.park:
    job_id = job_id_from_response(bell_json)
    session_id = resolve_session_id(a.base, a.frm)
    park_raw, _park_json = post_json(f"{a.base}/api/alpha/park",
                                     park_body_for(job_id, session_id))
    print(park_raw)
