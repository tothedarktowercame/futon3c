#!/usr/bin/env python3
"""Corpus WS invoke bridge — retrieval endpoint for ArSE peripheral.

Connects to Agency WS from the laptop, registers as corpus-1, and handles
retrieval queries using the local BGE + FAISS pipeline. Linode agents
invoke corpus-1 via the ArSE peripheral; results flow back over WS.

Usage:
    python3 scripts/corpus_ws_bridge.py

Env:
    AGENCY_WS_URL       ws://linode:7070/agency/ws  (required)
    AGENCY_HTTP_URL     http://linode:7070           (required)
    AGENT_ID            corpus-1
    ADMIN_TOKEN         (for HTTP registration)
    CORPUS_MATH_SE      ~/code/storage/math-processed-gpu
    CORPUS_MO           ~/code/storage/mo-processed-gpu
    RETRIEVAL_SCRIPT    ~/code/futon6/scripts/retrieve-proof-context.py
"""
import asyncio
import json
import os
import subprocess
import sys
import time
from pathlib import Path

try:
    import websockets
except ImportError:
    print("pip install websockets")
    sys.exit(1)

# ---------------------------------------------------------------------------
# Config
# ---------------------------------------------------------------------------
AGENCY_WS_URL = os.environ.get("AGENCY_WS_URL", "ws://127.0.0.1:7070/agency/ws")
AGENCY_HTTP_URL = os.environ.get("AGENCY_HTTP_URL", "http://127.0.0.1:7070")
AGENT_ID = os.environ.get("AGENT_ID", "corpus-1")
ADMIN_TOKEN = os.environ.get("ADMIN_TOKEN", "")
SESSION_ID = f"corpus-{int(time.time())}"

HOME = Path.home()
CORPUS_MATH_SE = Path(os.environ.get("CORPUS_MATH_SE", HOME / "code/storage/math-processed-gpu"))
CORPUS_MO = Path(os.environ.get("CORPUS_MO", HOME / "code/storage/mo-processed-gpu"))
RETRIEVAL_SCRIPT = Path(os.environ.get("RETRIEVAL_SCRIPT",
                                        HOME / "code/futon6/scripts/retrieve-proof-context.py"))

RECONNECT_DELAY = 5  # seconds between reconnection attempts

# ---------------------------------------------------------------------------
# Corpus sources
# ---------------------------------------------------------------------------
def check_sources():
    """Check which corpus sources are available locally."""
    sources = {}
    for name, path in [("math-se", CORPUS_MATH_SE), ("mathoverflow", CORPUS_MO)]:
        faiss_path = path / "structural-similarity-index.faiss"
        embeddings_path = path / "embeddings.npy"
        sources[name] = {
            "path": str(path),
            "exists": path.exists(),
            "faiss": faiss_path.exists(),
            "embeddings": embeddings_path.exists(),
            "entities": (path / "entities.json").exists() or (path / "entities.compact.jsonl").exists(),
        }
    sources["retrieval_script"] = {
        "path": str(RETRIEVAL_SCRIPT),
        "exists": RETRIEVAL_SCRIPT.exists(),
    }
    return sources

# ---------------------------------------------------------------------------
# Retrieval
# ---------------------------------------------------------------------------
def run_retrieval(query: str, top_k: int = 5, sources: list = None) -> dict:
    """Run the retrieval pipeline for a query.

    Strategy:
    1. If retrieve-proof-context.py exists, use the full 4-stage pipeline
    2. Otherwise, do direct FAISS search via faiss_index.py
    3. Fall back to keyword search on entities.json
    """
    results = {"query": query, "top_k": top_k, "neighbors": [], "source": "corpus-1"}

    if RETRIEVAL_SCRIPT.exists():
        try:
            result = run_retrieval_script(query, top_k, sources)
            if result:
                return result
        except Exception as e:
            results["retrieval_script_error"] = str(e)

    # Fallback: direct FAISS search
    for name, path in [("mathoverflow", CORPUS_MO), ("math-se", CORPUS_MATH_SE)]:
        if sources and name not in sources:
            continue
        faiss_path = path / "structural-similarity-index.faiss"
        if faiss_path.exists():
            try:
                neighbors = faiss_search(path, query, top_k)
                results["neighbors"].extend(neighbors)
                results["source"] = name
            except Exception as e:
                results.setdefault("errors", []).append(f"{name}: {e}")

    # Sort by score, take top_k
    results["neighbors"] = sorted(
        results["neighbors"], key=lambda x: -x.get("score", 0)
    )[:top_k]

    return results


def run_retrieval_script(query: str, top_k: int = 5, sources: list = None) -> dict:
    """Run retrieve-proof-context.py as a subprocess."""
    cmd = [
        sys.executable, str(RETRIEVAL_SCRIPT),
        "--query", query,
        "--top-k", str(top_k),
        "--output-format", "json",
    ]
    if sources:
        for s in sources:
            cmd.extend(["--source", s])

    env = os.environ.copy()
    env["CORPUS_MATH_SE"] = str(CORPUS_MATH_SE)
    env["CORPUS_MO"] = str(CORPUS_MO)

    proc = subprocess.run(
        cmd, capture_output=True, text=True, timeout=60, env=env
    )
    if proc.returncode == 0 and proc.stdout.strip():
        try:
            return json.loads(proc.stdout)
        except json.JSONDecodeError:
            return None
    return None


def faiss_search(corpus_path: Path, query: str, top_k: int) -> list:
    """Direct FAISS search using faiss_index.py from futon6."""
    faiss_script = HOME / "code/futon6/src/futon6/faiss_index.py"
    if not faiss_script.exists():
        return []

    cmd = [
        sys.executable, str(faiss_script),
        "query",
        "--index", str(corpus_path / "structural-similarity-index.faiss"),
        "--ids", str(corpus_path / "structural-similarity-index.ids.json"),
        "--query", query,
        "--top-k", str(top_k),
    ]
    proc = subprocess.run(cmd, capture_output=True, text=True, timeout=30)
    if proc.returncode == 0 and proc.stdout.strip():
        try:
            return json.loads(proc.stdout)
        except json.JSONDecodeError:
            return []
    return []


def handle_invoke(prompt: str) -> str:
    """Handle an invoke request. Parse the prompt as a retrieval query."""
    try:
        # Try parsing as JSON first (structured query from ArSE peripheral)
        request = json.loads(prompt)
        query = request.get("query", "")
        top_k = request.get("top_k", 5)
        sources = request.get("sources", None)
    except (json.JSONDecodeError, TypeError):
        # Plain text query
        query = prompt.strip()
        top_k = 5
        sources = None

    if not query:
        return json.dumps({"error": "Empty query", "ok": False})

    result = run_retrieval(query, top_k, sources)
    result["ok"] = True
    return json.dumps(result, default=str)


# ---------------------------------------------------------------------------
# HTTP registration
# ---------------------------------------------------------------------------
def register_agent():
    """Register corpus-1 via HTTP POST with ws-bridge=true."""
    import urllib.request
    url = f"{AGENCY_HTTP_URL}/api/alpha/agents"
    body = json.dumps({
        "agent-id": AGENT_ID,
        "type": "corpus",
        "ws-bridge": True,
    }).encode()
    req = urllib.request.Request(url, data=body, method="POST")
    req.add_header("Content-Type", "application/json")
    if ADMIN_TOKEN:
        req.add_header("x-admin-token", ADMIN_TOKEN)
    try:
        with urllib.request.urlopen(req, timeout=10) as resp:
            result = json.loads(resp.read())
            print(f"[corpus] Registered {AGENT_ID}: {result.get('ok', False)}")
            return True
    except Exception as e:
        print(f"[corpus] Registration failed: {e}")
        return False


# ---------------------------------------------------------------------------
# WebSocket bridge
# ---------------------------------------------------------------------------
async def ws_bridge():
    """Connect to Agency WS, handle invoke frames."""
    ws_url = f"{AGENCY_WS_URL}?agent-id={AGENT_ID}&session-id={SESSION_ID}"
    print(f"[corpus] Connecting to {ws_url}")

    async with websockets.connect(ws_url, ping_interval=30, ping_timeout=10) as ws:
        # Send ready handshake
        ready = json.dumps({
            "type": "ready",
            "agent_id": AGENT_ID,
            "session_id": SESSION_ID,
        })
        await ws.send(ready)
        print(f"[corpus] Sent ready handshake as {AGENT_ID}")

        # Wait for ready-ack
        ack = await asyncio.wait_for(ws.recv(), timeout=10)
        ack_data = json.loads(ack)
        if ack_data.get("type") in ("ready-ack", "ready_ack"):
            print(f"[corpus] Connected. Waiting for invoke frames...")
        else:
            print(f"[corpus] Unexpected ack: {ack_data}")

        # Message loop
        async for message in ws:
            try:
                frame = json.loads(message)
                frame_type = frame.get("type")

                if frame_type == "invoke":
                    invoke_id = frame.get("invoke_id")
                    prompt = frame.get("prompt", "")
                    print(f"[corpus] Invoke {invoke_id}: {prompt[:80]}...")

                    # Run retrieval (in thread pool to avoid blocking)
                    loop = asyncio.get_event_loop()
                    result = await loop.run_in_executor(None, handle_invoke, prompt)

                    response = json.dumps({
                        "type": "invoke_result",
                        "invoke_id": invoke_id,
                        "result": result,
                        "session_id": SESSION_ID,
                    })
                    await ws.send(response)
                    print(f"[corpus] Replied to {invoke_id}")

                elif frame_type == "ping":
                    await ws.send(json.dumps({"type": "pong"}))

                else:
                    print(f"[corpus] Ignoring frame type: {frame_type}")

            except json.JSONDecodeError:
                print(f"[corpus] Bad JSON: {message[:100]}")
            except Exception as e:
                print(f"[corpus] Error handling frame: {e}")


async def main():
    """Register and run with auto-reconnect."""
    # Check sources
    sources = check_sources()
    print(f"[corpus] Sources:")
    for name, info in sources.items():
        status = "OK" if info.get("exists") or info.get("faiss") else "MISSING"
        print(f"  {name}: {status} ({info.get('path', '?')})")

    # Register via HTTP
    register_agent()

    # Connect WS with auto-reconnect
    while True:
        try:
            await ws_bridge()
        except websockets.ConnectionClosed:
            print(f"[corpus] Connection closed. Reconnecting in {RECONNECT_DELAY}s...")
        except ConnectionRefusedError:
            print(f"[corpus] Connection refused. Retrying in {RECONNECT_DELAY}s...")
        except Exception as e:
            print(f"[corpus] Error: {e}. Reconnecting in {RECONNECT_DELAY}s...")
        await asyncio.sleep(RECONNECT_DELAY)


if __name__ == "__main__":
    asyncio.run(main())
