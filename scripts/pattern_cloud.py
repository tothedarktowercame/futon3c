#!/usr/bin/env python3
"""pattern_cloud.py — a "pattern cloud" of an IRC session, from real evidence.

Instead of a word cloud of what was SAID, this shows which PATTERNS the
room's contributions activated: each IRC message (forum-post evidence in
the Evidence Landscape) is embedded (MiniLM) and matched against the
futon pattern library (futon3a minilm_pattern_embeddings.json, 1202
patterns); the top matches above a cosine threshold accumulate weight,
and the cloud renders pattern titles sized/shaded by summed activation.

Two-stage because the embedding venv (futon3a/.venv) lacks matplotlib:
  stage 1 (embed): futon3a/.venv/bin/python pattern_cloud.py embed [--host H] [--hours N]
  stage 2 (render): python3 pattern_cloud.py render [--out F.png]
  or simply:        python3 pattern_cloud.py            (runs both via subprocess)

Provenance is embedded in the sidecar JSON; nothing is hand-typed.
"""

import argparse
import json
import math
import os
import subprocess
import sys
import time
import urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
CODE = os.path.dirname(os.path.dirname(HERE))
PATTERN_EMB = os.path.join(CODE, "futon3a/resources/notions/minilm_pattern_embeddings.json")
EMBED_PY = os.path.join(CODE, "futon3a/.venv/bin/python")
WORK_JSON = os.path.join(HERE, "pattern-cloud-work.json")
DEFAULT_HOST = "http://172-236-28-208.ip.linodeusercontent.com:7070"
NOISE_PREFIXES = ("[accepted ", "[done ", "(bridge self-test")
COS_THRESHOLD = 0.32
TOP_K = 3

# ------------------------------------------------------------- stage 1 ----

def fetch_messages(host, hours):
    url = f"{host}/api/alpha/evidence?type=forum-post&limit=500"
    with urllib.request.urlopen(url, timeout=30) as r:
        data = json.load(r)
    entries = data.get("entries", [])
    cutoff = time.time() - hours * 3600
    msgs = []
    for e in entries:
        at = e.get("evidence/at", "")
        body = e.get("evidence/body") or {}
        text = (body.get("text") or "").strip()
        author = e.get("evidence/author") or "?"
        if not text or any(text.startswith(p) for p in NOISE_PREFIXES):
            continue
        try:
            ts = time.mktime(time.strptime(at[:19], "%Y-%m-%dT%H:%M:%S"))
        except ValueError:
            ts = time.time()
        if ts >= cutoff:
            msgs.append({"author": author, "text": text, "at": at})
    return msgs


_MODEL = None

def _get_model():
    """Module-level model cache so a long-lived server loads MiniLM once."""
    global _MODEL
    if _MODEL is None:
        from sentence_transformers import SentenceTransformer
        _MODEL = SentenceTransformer("sentence-transformers/all-MiniLM-L6-v2")
    return _MODEL


def stage_embed(host, hours):
    import numpy as np

    msgs = fetch_messages(host, hours)
    if not msgs:
        print("no messages in window; nothing to embed", file=sys.stderr)
        json.dump({"provenance": {"host": host, "hours": hours, "messages": 0},
                   "weights": {}}, open(WORK_JSON, "w"))
        return

    patterns = json.load(open(PATTERN_EMB))
    pvecs = np.array([p["vector"] for p in patterns], dtype="float32")
    pvecs /= (np.linalg.norm(pvecs, axis=1, keepdims=True) + 1e-9)

    model = _get_model()
    mvecs = model.encode([m["text"] for m in msgs], normalize_embeddings=True)

    weights, hits = {}, []
    for m, v in zip(msgs, mvecs):
        cos = pvecs @ v
        top = cos.argsort()[::-1][:TOP_K]
        for i in top:
            if cos[i] < COS_THRESHOLD:
                continue
            title = patterns[i]["title"]
            weights[title] = weights.get(title, 0.0) + float(cos[i])
            hits.append({"author": m["author"], "pattern": title,
                         "cos": round(float(cos[i]), 3)})

    json.dump({"provenance": {
                   "generated-at": time.strftime("%Y-%m-%dT%H:%M:%S%z"),
                   "host": host, "hours": hours,
                   "messages": len(msgs),
                   "patterns-library": len(patterns),
                   "threshold": COS_THRESHOLD, "top-k": TOP_K,
                   "method": "MiniLM message embedding x cosine vs futon3a pattern embeddings"},
               "weights": weights, "hits": hits},
              open(WORK_JSON, "w"), indent=1)
    print(f"embedded {len(msgs)} messages -> {len(weights)} activated patterns")

# ------------------------------------------------------------- stage 2 ----

def stage_render(out_png):
    import matplotlib
    matplotlib.use("Agg")
    import matplotlib.pyplot as plt

    work = json.load(open(WORK_JSON))
    weights = work["weights"]
    prov = work["provenance"]
    if not weights:
        print("no activated patterns to render", file=sys.stderr)
        return

    # Evidence-viewer dark theme, light-blue magnitude ramp
    BG = "#1a1a2e"
    RAMP = ["#5598e7", "#6da7ec", "#86b6ef", "#9ec5f4", "#b7d3f6", "#cde2fb"]
    INK3 = "#6a6a8a"

    def clean(t):
        s = ''.join(c if c.isascii() and c.isprintable() else ' ' for c in t)
        return ' '.join(s.split())
    weights = {clean(k): v for k, v in weights.items() if clean(k)}
    ranked = sorted(weights.items(), key=lambda kv: -kv[1])[:32]
    wmax = ranked[0][1]
    wmin = ranked[-1][1]

    fig, ax = plt.subplots(figsize=(12.8, 7.2), dpi=150)
    fig.patch.set_facecolor(BG)
    ax.set_facecolor(BG)
    ax.set_xlim(0, 100); ax.set_ylim(0, 100); ax.axis("off")

    placed = []  # (x, y, w, h)
    def collides(x, y, w, h):
        for (px, py, pw, ph) in placed:
            if abs(x - px) < (w + pw) / 2 and abs(y - py) < (h + ph) / 2:
                return True
        return False

    golden = math.pi * (3 - math.sqrt(5))
    for rank, (title, wt) in enumerate(ranked):
        t = 0 if wmax == wmin else (wt - wmin) / (wmax - wmin)
        size = 11 + 26 * t
        color = RAMP[min(len(RAMP) - 1, int(t * len(RAMP)))]
        # extents in axes units: fig 12.8x7.2in @150dpi, axes 100x100
        est_w = 0.062 * size * len(title) + 2
        est_h = 0.30 * size + 1
        k = 0
        pos = None
        while k <= 12000:
            r = 0.55 * math.sqrt(k)
            ang = k * golden
            x = 50 + r * math.cos(ang) * 1.75
            y = 50 + r * math.sin(ang)
            if (est_w / 2 + 1 < x < 99 - est_w / 2
                    and est_h / 2 + 3 < y < 99 - est_h / 2
                    and not collides(x, y, est_w, est_h)):
                pos = (x, y)
                break
            k += 3
        if pos is None:
            continue
        x, y = pos
        placed.append((x, y, est_w, est_h))
        ax.text(x, y, title, color=color, fontsize=size, ha="center",
                va="center", fontweight="bold" if t > 0.6 else "normal",
                family="sans-serif")

    ax.text(99, 1.5,
            f"pattern activations of {prov['messages']} contributions · "
            f"{prov['generated-at'][:10]} · computed from the Evidence Landscape, "
            f"zero hand-typed rows",
            color=INK3, fontsize=9, ha="right", va="bottom")
    fig.tight_layout()
    fig.savefig(out_png, facecolor=BG, bbox_inches="tight")
    print("wrote", out_png, f"({len(ranked)} patterns)")

# ---------------------------------------------------------------- main ----

if __name__ == "__main__":
    ap = argparse.ArgumentParser()
    ap.add_argument("stage", nargs="?", default="all",
                    choices=["embed", "render", "all"])
    ap.add_argument("--host", default=DEFAULT_HOST)
    ap.add_argument("--hours", type=float, default=8)
    ap.add_argument("--out", default=os.path.join(HERE, "pattern-cloud.png"))
    args = ap.parse_args()

    if args.stage == "embed":
        stage_embed(args.host, args.hours)
    elif args.stage == "render":
        stage_render(args.out)
    else:
        subprocess.run([EMBED_PY, os.path.abspath(__file__), "embed",
                        "--host", args.host, "--hours", str(args.hours)],
                       check=True)
        stage_render(args.out)
