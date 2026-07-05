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
import sys, json, argparse, urllib.request

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
ap.add_argument("--dry-run", action="store_true", help="print payload, do not send")
a = ap.parse_args()

prompt = sys.stdin.read()
if not prompt.strip():
    sys.exit("agency_send: empty prompt on stdin")

# Loud-failure for the load-bearing mesh edge (M-agency-hardening): a bell
# without --from logs as 'http-caller' with NO mesh edge, so auto-bellback has
# no recipient and the reply silently can't route back. Surface it at send time.
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
payload = json.dumps(body)
if a.dry_run:
    print(payload); sys.exit(0)

req = urllib.request.Request(f"{a.base}/api/alpha/{a.kind}",
                             data=payload.encode(),
                             headers={"Content-Type": "application/json"})
print(urllib.request.urlopen(req).read().decode())
