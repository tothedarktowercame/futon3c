#!/usr/bin/env python3
"""pattern_cloud_server.py — serve the live pattern cloud over HTTP.

Runs on lucy inside the patterncloud venv (sentence-transformers +
matplotlib both available), listening on 127.0.0.1:7075 behind nginx:

  GET /                   -> dark HTML page, auto-refreshing the image
  GET /pattern-cloud.png  -> the cloud; regenerated at most once per
                             CACHE_SECS, from the local Evidence
                             Landscape (127.0.0.1:7070)

The MiniLM model loads once at first request and stays warm.
systemd user unit: patterncloud.service.
"""

import http.server
import os
import threading
import time

import pattern_cloud  # sibling module: embed + render stages

HERE = os.path.dirname(os.path.abspath(__file__))
PNG = os.path.join(HERE, "pattern-cloud.png")
CACHE_SECS = 60
HOURS = 8
HOST_STORE = "http://127.0.0.1:7070"
LOCK = threading.Lock()

PAGE = """<!doctype html><html><head><meta charset="utf-8">
<title>Pattern Cloud — live</title>
<style>
 body { background:#1a1a2e; color:#eaeaea; font-family:system-ui,sans-serif;
        margin:0; display:flex; flex-direction:column; align-items:center; }
 h1 { font-size:1.1rem; font-weight:600; color:#a0a0a0; margin:14px 0 6px; }
 img { max-width:98vw; height:auto; }
 p  { color:#6a6a8a; font-size:.8rem; }
</style></head><body>
<h1>Pattern Cloud — what the room's contributions activate</h1>
<img id="cloud" src="/pattern-cloud.png">
<p>regenerates from the Evidence Landscape · refreshes every 60s</p>
<script>
 setInterval(() => {
   document.getElementById('cloud').src = '/pattern-cloud.png?t=' + Date.now();
 }, 60000);
</script></body></html>"""


def regenerate_if_stale():
    with LOCK:
        fresh = (os.path.exists(PNG)
                 and time.time() - os.path.getmtime(PNG) < CACHE_SECS)
        if fresh:
            return
        try:
            pattern_cloud.stage_embed(HOST_STORE, HOURS)
            pattern_cloud.stage_render(PNG)
        except Exception as e:                      # keep serving last good PNG
            print(f"regeneration failed: {e}", flush=True)


class Handler(http.server.BaseHTTPRequestHandler):
    def do_GET(self):
        path = self.path.split("?")[0]
        if path in ("/", "/index.html"):
            body = PAGE.encode()
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.end_headers()
            self.wfile.write(body)
        elif path == "/pattern-cloud.png":
            regenerate_if_stale()
            if not os.path.exists(PNG):
                self.send_error(503, "no cloud yet")
                return
            data = open(PNG, "rb").read()
            self.send_response(200)
            self.send_header("Content-Type", "image/png")
            self.send_header("Cache-Control", "no-store")
            self.send_header("Content-Length", str(len(data)))
            self.end_headers()
            self.wfile.write(data)
        else:
            self.send_error(404)

    def log_message(self, fmt, *args):
        pass


if __name__ == "__main__":
    server = http.server.ThreadingHTTPServer(("127.0.0.1", 7075), Handler)
    print("pattern cloud server on 127.0.0.1:7075", flush=True)
    server.serve_forever()
