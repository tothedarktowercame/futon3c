#!/usr/bin/env python3
"""Corpus WS invoke bridge — retrieval endpoint for ArSE peripheral.

Connects to Agency WS from the laptop, registers as corpus-1, and handles
retrieval queries using the local BGE + FAISS pipeline. Linode agents
invoke corpus-1 via the ArSE peripheral; results flow back over WS.

Usage:
    python3 scripts/corpus_ws_bridge.py
    python3 scripts/corpus_ws_bridge.py --semantic-rerank

Env:
    AGENCY_WS_URL       ws://linode:7070/agency/ws  (required)
    AGENCY_HTTP_URL     http://linode:7070           (required)
    AGENT_ID            corpus-1
    ADMIN_TOKEN         (for HTTP registration)
    CORPUS_MATH_SE      ~/code/storage/math-processed-gpu
    CORPUS_MO           ~/code/storage/mo-processed-gpu
    RETRIEVAL_SCRIPT    ~/code/futon6/scripts/retrieve-proof-context.py
    CORPUS_SEMANTIC_RERANK=0|1
    CORPUS_SEMANTIC_CANDIDATES=120
    CORPUS_KEYWORD_WEIGHT=0.55
"""
import argparse
import asyncio
import heapq
import json
import os
import re
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

SOURCE_CONFIGS = {
    "math-se": {"path": CORPUS_MATH_SE, "id_prefix": "se-math-"},
    "mathoverflow": {"path": CORPUS_MO, "id_prefix": "se-mo-"},
}

SOURCE_ALIASES = {
    "math-se": "math-se",
    "math": "math-se",
    "se-math": "math-se",
    "mathoverflow": "mathoverflow",
    "mo": "mathoverflow",
    "se-mo": "mathoverflow",
}

_SOURCE_CACHE = {}
_EMBEDDINGS_CACHE = {}


def parse_bool_env(name: str, default: bool = False) -> bool:
    raw = os.environ.get(name)
    if raw is None:
        return default
    return str(raw).strip().lower() not in {"0", "false", "no", "off"}


def parse_int_env(name: str, default: int, low: int, high: int) -> int:
    raw = os.environ.get(name)
    if raw is None:
        return default
    try:
        value = int(raw)
    except ValueError:
        return default
    return max(low, min(high, value))


def parse_float_env(name: str, default: float, low: float, high: float) -> float:
    raw = os.environ.get(name)
    if raw is None:
        return default
    try:
        value = float(raw)
    except ValueError:
        return default
    return max(low, min(high, value))


RETRIEVAL_OPTIONS = {
    "semantic_rerank": parse_bool_env("CORPUS_SEMANTIC_RERANK", default=False),
    "semantic_candidates": parse_int_env("CORPUS_SEMANTIC_CANDIDATES", default=120, low=20, high=1000),
    "keyword_weight": parse_float_env("CORPUS_KEYWORD_WEIGHT", default=0.55, low=0.0, high=1.0),
}

# ---------------------------------------------------------------------------
# Corpus sources
# ---------------------------------------------------------------------------
def check_sources():
    """Check which corpus sources are available locally."""
    sources = {}
    for name, path in [("math-se", CORPUS_MATH_SE), ("mathoverflow", CORPUS_MO)]:
        faiss_path = path / "structural-similarity-index.faiss"
        embeddings_path = path / "embeddings.npy"
        compact_path = path / "entities.compact.jsonl"
        sources[name] = {
            "path": str(path),
            "exists": path.exists(),
            "compact_entities": compact_path.exists(),
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
def normalize_sources(sources):
    """Normalize source names from request payload to bridge source keys."""
    if not sources:
        return ["mathoverflow", "math-se"]
    normalized = []
    for raw in sources:
        key = SOURCE_ALIASES.get(str(raw).strip().lower())
        if key and key not in normalized:
            normalized.append(key)
    return normalized or ["mathoverflow", "math-se"]


def tokenize_query(query: str):
    tokens = [t for t in re.split(r"[^a-z0-9]+", query.lower()) if len(t) >= 3]
    if len(tokens) > 16:
        return tokens[:16]
    return tokens


def parse_top_k(value, default=5):
    try:
        n = int(value)
    except (TypeError, ValueError):
        return default
    return max(1, min(n, 50))


def parse_semantic_candidates(value, default):
    try:
        n = int(value)
    except (TypeError, ValueError):
        return default
    return max(20, min(n, 1000))


def parse_keyword_weight(value, default):
    try:
        x = float(value)
    except (TypeError, ValueError):
        return default
    return max(0.0, min(x, 1.0))


def load_source_cache(source_name: str):
    """Load compact corpus entries for SOURCE-NAME once (lazy cache)."""
    if source_name in _SOURCE_CACHE:
        return _SOURCE_CACHE[source_name]

    cfg = SOURCE_CONFIGS[source_name]
    corpus_path = cfg["path"]
    compact_path = corpus_path / "entities.compact.jsonl"
    if not compact_path.exists():
        _SOURCE_CACHE[source_name] = []
        return []

    entries = []
    with compact_path.open() as f:
        for line in f:
            try:
                obj = json.loads(line)
            except json.JSONDecodeError:
                continue
            thread_id = obj.get("thread_id")
            if thread_id is None:
                continue
            try:
                thread_id = int(thread_id)
            except (TypeError, ValueError):
                continue
            title = str(obj.get("title", "")).strip()
            tags = [str(t).strip() for t in (obj.get("tags") or []) if str(t).strip()]
            if not title and not tags:
                continue
            entries.append({
                "id": f"{cfg['id_prefix']}{thread_id}",
                "thread_id": thread_id,
                "title": title,
                "title_lc": title.lower(),
                "tags": tags[:12],
                "tags_lc": [t.lower() for t in tags[:12]],
            })

    _SOURCE_CACHE[source_name] = entries
    return entries


def load_embeddings_cache(source_name: str):
    """Load source embeddings lazily (memmap) for semantic reranking."""
    if source_name in _EMBEDDINGS_CACHE:
        return _EMBEDDINGS_CACHE[source_name]
    try:
        import numpy as np
    except ImportError:
        _EMBEDDINGS_CACHE[source_name] = {"array": None, "error": "numpy-missing"}
        return _EMBEDDINGS_CACHE[source_name]

    emb_path = SOURCE_CONFIGS[source_name]["path"] / "embeddings.npy"
    if not emb_path.exists():
        _EMBEDDINGS_CACHE[source_name] = {"array": None, "error": "embeddings-missing"}
        return _EMBEDDINGS_CACHE[source_name]
    try:
        arr = np.load(emb_path, mmap_mode="r")
        _EMBEDDINGS_CACHE[source_name] = {"array": arr, "error": None}
    except Exception as e:
        _EMBEDDINGS_CACHE[source_name] = {"array": None, "error": str(e)}
    return _EMBEDDINGS_CACHE[source_name]


def keyword_candidates(source_name: str, query: str, candidate_limit: int):
    """Collect keyword-scored candidates for one source."""
    entries = load_source_cache(source_name)
    if not entries:
        return []

    tokens = tokenize_query(query)
    if not tokens:
        return []
    phrase = " ".join(tokens) if len(tokens) >= 2 else ""

    best = []  # min-heap of (score, tie_breaker, row_idx, entry)
    for row_idx, ent in enumerate(entries):
        title_lc = ent["title_lc"]
        tags_lc = ent["tags_lc"]
        score = 0.0
        if phrase and phrase in title_lc:
            score += 6.0
        for tok in tokens:
            if tok in title_lc:
                score += 3.0
            for tag in tags_lc:
                if tok == tag or tok in tag:
                    score += 2.0
                    break
        if score <= 0:
            continue
        item = (score, ent["thread_id"], row_idx, ent)
        if len(best) < candidate_limit:
            heapq.heappush(best, item)
        elif item[:2] > best[0][:2]:
            heapq.heapreplace(best, item)

    ranked = sorted(best, key=lambda x: (-x[0], -x[1]))
    out = []
    for score, _tid, row_idx, ent in ranked:
        out.append({
            "row_idx": row_idx,
            "kw_score": float(score),
            "id": ent["id"],
            "title": ent["title"],
            "source": source_name,
            "tags": ent["tags"],
            "thread_id": ent["thread_id"],
        })
    return out


def top_keyword_results(candidates, top_k):
    out = []
    for rank, c in enumerate(candidates[:top_k], start=1):
        out.append({
            "id": c["id"],
            "title": c["title"],
            "score": round(float(c["kw_score"]), 4),
            "rank": rank,
            "source": c["source"],
            "tags": c["tags"],
            "kw_score": round(float(c["kw_score"]), 4),
            "retrieval_mode": "keyword",
        })
    return out


def semantic_rerank_candidates(source_name, candidates, top_k, keyword_weight):
    """Rerank keyword candidates by embedding similarity using centroid query."""
    cache = load_embeddings_cache(source_name)
    embeddings = cache.get("array")
    if embeddings is None:
        return None, cache.get("error") or "embeddings-unavailable"

    try:
        import numpy as np
    except ImportError:
        return None, "numpy-missing"

    valid = [c for c in candidates if c["row_idx"] < len(embeddings)]
    if len(valid) < max(2, min(top_k, 3)):
        return None, "not-enough-embedding-candidates"

    idxs = np.array([c["row_idx"] for c in valid], dtype=np.int64)
    kw = np.array([max(c["kw_score"], 0.0) for c in valid], dtype=np.float32)
    kw_sum = float(kw.sum())
    if kw_sum <= 0:
        return None, "zero-keyword-score"

    cand_embs = np.asarray(embeddings[idxs], dtype=np.float32)
    norms = np.linalg.norm(cand_embs, axis=1, keepdims=True)
    norms = np.maximum(norms, 1e-8)
    cand_norm = cand_embs / norms

    weights = kw / kw_sum
    q = (cand_norm * weights[:, None]).sum(axis=0, keepdims=True)
    q_norm = np.linalg.norm(q)
    if q_norm <= 1e-8:
        return None, "degenerate-query-vector"
    q /= q_norm

    sims = (cand_norm @ q.T).flatten()
    sim01 = (sims + 1.0) / 2.0
    kw_norm = kw / max(float(kw.max()), 1e-8)
    combined = keyword_weight * kw_norm + (1.0 - keyword_weight) * sim01
    order = np.argsort(-combined)

    out = []
    for rank_pos, idx in enumerate(order[:top_k], start=1):
        c = valid[int(idx)]
        out.append({
            "id": c["id"],
            "title": c["title"],
            "score": round(float(combined[int(idx)]), 4),
            "rank": rank_pos,
            "source": c["source"],
            "tags": c["tags"],
            "kw_score": round(float(kw_norm[int(idx)]), 4),
            "semantic_similarity": round(float(sim01[int(idx)]), 4),
            "retrieval_mode": "semantic-rerank",
        })
    return out, None


def run_retrieval(query: str, top_k: int = 5, sources: list = None,
                  semantic_rerank=None, semantic_candidates=None,
                  keyword_weight=None) -> dict:
    """Run the retrieval pipeline for a query.

    Strategy:
    1. Keyword retrieval over local compact entities (title + tags)
    2. Optional semantic rerank over keyword candidates
    2. Merge source results and rank globally by score
    3. Include retrieval-script compatibility note for debugging
    """
    results = {"query": query, "top_k": top_k, "neighbors": [], "source": "corpus-1"}
    selected_sources = normalize_sources(sources)
    use_semantic = RETRIEVAL_OPTIONS["semantic_rerank"] if semantic_rerank is None else bool(semantic_rerank)
    candidate_limit = (RETRIEVAL_OPTIONS["semantic_candidates"]
                       if semantic_candidates is None
                       else parse_semantic_candidates(semantic_candidates, RETRIEVAL_OPTIONS["semantic_candidates"]))
    kw_weight = (RETRIEVAL_OPTIONS["keyword_weight"]
                 if keyword_weight is None
                 else parse_keyword_weight(keyword_weight, RETRIEVAL_OPTIONS["keyword_weight"]))
    results["semantic_rerank"] = use_semantic

    # Retrieval-script is batch-mode (proof-node precompute), not per-query.
    if RETRIEVAL_SCRIPT.exists():
        results["retrieval_script_note"] = (
            "retrieve-proof-context.py is batch-mode and does not accept "
            "--query/--output-format; using compact keyword retrieval."
        )

    for source_name in selected_sources:
        try:
            candidates = keyword_candidates(source_name, query, candidate_limit)
            if not candidates:
                continue
            if use_semantic:
                reranked, semantic_err = semantic_rerank_candidates(
                    source_name, candidates, top_k, kw_weight
                )
                if reranked is not None:
                    neighbors = reranked
                else:
                    neighbors = top_keyword_results(candidates, top_k)
                    results.setdefault("semantic_fallbacks", []).append(
                        f"{source_name}: {semantic_err}"
                    )
            else:
                neighbors = top_keyword_results(candidates, top_k)
            results["neighbors"].extend(neighbors)
        except Exception as e:
            results.setdefault("errors", []).append(f"{source_name}: {e}")

    # Sort by score, take top_k
    results["neighbors"] = sorted(
        results["neighbors"], key=lambda x: -x.get("score", 0)
    )[:top_k]

    return results


def run_retrieval_script(query: str, top_k: int = 5, sources: list = None) -> dict:
    """Legacy shim: script is batch-mode and unsupported for ad-hoc query retrieval."""
    return None


def faiss_search(corpus_path: Path, query: str, top_k: int) -> list:
    """Deprecated text->FAISS path (FAISS expects embedding vectors, not raw text)."""
    return []


def _se_url(source: str, thread_id) -> str:
    """Build a short Stack Exchange URL from source and thread_id."""
    if source == "mathoverflow":
        return f"mathoverflow.net/q/{thread_id}"
    return f"math.stackexchange.com/q/{thread_id}"


def format_for_irc(result: dict) -> str:
    """Format retrieval results as compact IRC text (one line per result)."""
    neighbors = result.get("neighbors", [])
    if not neighbors:
        return f"No results for: {result.get('query', '?')}"
    lines = [f"{len(neighbors)} results for: {result.get('query', '?')}"]
    for n in neighbors[:3]:  # top 3 only for IRC
        title = n.get("title", "untitled")[:80]
        score = n.get("score", 0)
        tid = n.get("thread_id")
        source = n.get("source", "?")
        url = _se_url(source, tid) if tid else source
        lines.append(f"{title} ({score:.2f}) — {url}")
    if len(neighbors) > 3:
        lines.append(f"(+{len(neighbors) - 3} more)")
    return "\n".join(lines)


def extract_query_from_prompt(prompt: str) -> str:
    """Extract the actual user query from an IRC surface-contract-wrapped prompt.

    The ngircd bridge wraps messages like:
        --- CURRENT TURN ---
        Surface: irc (#math)
        Caller: irc:joe
        ---
        [Surface: IRC | Channel: #math | ...]
        joe: @corpus What is known about ...
    We want just the query text after '@corpus'."""
    import re
    # Look for the @corpus mention and grab everything after it
    m = re.search(r'@corpus\s+(.*)', prompt, re.DOTALL)
    if m:
        return m.group(1).strip()
    # If no @corpus, look for last line after "---" separator block
    lines = prompt.strip().split('\n')
    # Skip surface contract framing, return last meaningful line
    for line in reversed(lines):
        line = line.strip()
        if line and not line.startswith('[') and not line.startswith('---') and ':' not in line[:20]:
            return line
    # Fallback: return the whole thing
    return prompt.strip()


def handle_invoke(prompt: str) -> str:
    """Handle an invoke request. Parse the prompt as a retrieval query.

    JSON prompt (from ArSE peripheral) → returns JSON result.
    Plain text prompt (from IRC) → returns human-readable text."""
    structured = False
    try:
        # Try parsing as JSON first (structured query from ArSE peripheral)
        request = json.loads(prompt)
        query = request.get("query", "")
        top_k = parse_top_k(request.get("top_k", 5))
        sources = request.get("sources", None)
        semantic_rerank = request.get("semantic_rerank", None)
        semantic_candidates = request.get("semantic_candidates", None)
        keyword_weight = request.get("keyword_weight", None)
        structured = True
    except (json.JSONDecodeError, TypeError):
        # Plain text query (from IRC @mention) — strip surface contract framing
        query = extract_query_from_prompt(prompt)
        top_k = 5
        sources = None
        semantic_rerank = None
        semantic_candidates = None
        keyword_weight = None

    if not query:
        return json.dumps({"error": "Empty query", "ok": False}) if structured else "Empty query — ask me a math question."

    result = run_retrieval(query, top_k, sources,
                           semantic_rerank=semantic_rerank,
                           semantic_candidates=semantic_candidates,
                           keyword_weight=keyword_weight)
    result["ok"] = True

    if structured:
        return json.dumps(result, default=str)
    else:
        return format_for_irc(result)


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


def parse_args(argv=None):
    parser = argparse.ArgumentParser(
        description="Corpus WS bridge for retrieval over Agency WS."
    )
    parser.add_argument(
        "--semantic-rerank",
        action="store_true",
        help="Enable semantic reranking over keyword candidates.",
    )
    parser.add_argument(
        "--keyword-only",
        action="store_true",
        help="Force keyword-only retrieval (disables semantic rerank).",
    )
    parser.add_argument(
        "--semantic-candidates",
        type=int,
        default=None,
        help="Candidate pool size before semantic rerank (default from env or 120).",
    )
    parser.add_argument(
        "--keyword-weight",
        type=float,
        default=None,
        help="Keyword weight in [0,1] for hybrid score (default 0.55).",
    )
    return parser.parse_args(argv)


def apply_cli_args(args):
    semantic = RETRIEVAL_OPTIONS["semantic_rerank"]
    if args.semantic_rerank:
        semantic = True
    if args.keyword_only:
        semantic = False
    RETRIEVAL_OPTIONS["semantic_rerank"] = semantic
    if args.semantic_candidates is not None:
        RETRIEVAL_OPTIONS["semantic_candidates"] = parse_semantic_candidates(
            args.semantic_candidates, RETRIEVAL_OPTIONS["semantic_candidates"]
        )
    if args.keyword_weight is not None:
        RETRIEVAL_OPTIONS["keyword_weight"] = parse_keyword_weight(
            args.keyword_weight, RETRIEVAL_OPTIONS["keyword_weight"]
        )


if __name__ == "__main__":
    cli_args = parse_args()
    apply_cli_args(cli_args)
    mode = "semantic-rerank" if RETRIEVAL_OPTIONS["semantic_rerank"] else "keyword-only"
    print(
        f"[corpus] retrieval mode={mode} "
        f"candidates={RETRIEVAL_OPTIONS['semantic_candidates']} "
        f"keyword-weight={RETRIEVAL_OPTIONS['keyword_weight']}"
    )
    asyncio.run(main())
