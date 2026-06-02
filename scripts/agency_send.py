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
--dry-run prints the payload instead of sending.
"""
import sys, json, argparse, urllib.request

ap = argparse.ArgumentParser()
ap.add_argument("--to", required=True, help="recipient agent-id")
ap.add_argument("--kind", choices=["bell", "whistle"], default="bell")
ap.add_argument("--base", default="http://localhost:7070")
ap.add_argument("--dry-run", action="store_true", help="print payload, do not send")
a = ap.parse_args()

prompt = sys.stdin.read()
if not prompt.strip():
    sys.exit("agency_send: empty prompt on stdin")

payload = json.dumps({"agent-id": a.to, "prompt": prompt})
if a.dry_run:
    print(payload); sys.exit(0)

req = urllib.request.Request(f"{a.base}/api/alpha/{a.kind}",
                             data=payload.encode(),
                             headers={"Content-Type": "application/json"})
print(urllib.request.urlopen(req).read().decode())
