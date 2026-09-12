#!/usr/bin/env python3
"""compact_session.py — shrink a Claude Code session transcript so a monster
becomes a warm-able joey (E-monster-to-joey, futon3c).

Two layers, safe-first:

  1. DE-BLOAT (lossless for the conversation) — drop non-conversation record
     types (ai-title / mode / last-prompt / queue-operation / attachment; these
     are local UI bookkeeping, chain-independent, never sent to the API) and
     strip the redundant `toolUseResult` / `attachment` fields. The user /
     assistant / system parentUuid chain is preserved (re-linked across dropped
     records), so `claude --resume` sees the full conversation. Verified: a
     de-bloated session resumes and recalls prior turns.

  2. TRUNCATE (lossy, only if still over target) — keep the newest conversation
     records until a context-byte budget, re-root the oldest kept record. History
     is safe in the Evidence Landscape, so dropping old turns is sanctioned; the
     agent continues from recent context + MEMORY.md.

Compacts IN PLACE under the same session-id (so the agent's clock/evidence/pouch
key are untouched), backing up the original first. Intended to run on a COLD
session (no warm process appending). Idempotent.

Usage:
  compact_session.py <session-id> [--target-mib 1.5] [--dry-run] [--no-truncate]
"""
import argparse, json, os, shutil, sys, time

DROP_TYPES = {"ai-title", "mode", "last-prompt", "queue-operation", "attachment"}
DROP_FIELDS = {"toolUseResult", "attachment"}
CONVERSATION_TYPES = {"user", "assistant", "system"}
PROJECTS = os.path.join(os.path.expanduser("~"), ".claude", "projects")


def find_transcript(session_id):
    if not os.path.isdir(PROJECTS):
        return None
    for d in os.listdir(PROJECTS):
        p = os.path.join(PROJECTS, d, f"{session_id}.jsonl")
        if os.path.isfile(p):
            return p
    return None


def content_bytes(rec):
    """Model-context bytes for a record (message.content), 0 for metadata."""
    msg = rec.get("message") or {}
    c = msg.get("content")
    if c is None:
        return 0
    return len(json.dumps(c, ensure_ascii=False))


def debloat(recs):
    """Layer 1: drop non-conversation records + redundant fields, re-link chain."""
    by_uuid = {r.get("uuid"): r for r in recs if r.get("uuid")}
    kept_uuids = {r.get("uuid") for r in recs
                  if r.get("type") not in DROP_TYPES and r.get("uuid")}

    def nearest_kept(puid):
        seen = set()
        while puid and puid not in kept_uuids and puid in by_uuid and puid not in seen:
            seen.add(puid)
            puid = by_uuid[puid].get("parentUuid")
        return puid if puid in kept_uuids else None

    out = []
    for r in recs:
        if r.get("type") in DROP_TYPES:
            continue
        r = dict(r)
        for f in DROP_FIELDS:
            r.pop(f, None)
        if "parentUuid" in r:
            r["parentUuid"] = nearest_kept(r.get("parentUuid"))
        out.append(r)
    return out


def rec_bytes(rec):
    """Full serialized file contribution of a record (what the joey-max gate sees)."""
    return len(json.dumps(rec, ensure_ascii=False).encode("utf-8")) + 1  # +newline


def _blocks(rec):
    """Content blocks of a transcript record, or [] when it has none."""
    msg = rec.get("message")
    if not isinstance(msg, dict):
        return []
    content = msg.get("content")
    return [b for b in content if isinstance(b, dict)] if isinstance(content, list) else []


def truncate(recs, target_bytes):
    """Layer 2: keep newest conversation records within a target FILE-byte budget
    (the joey-max gate measures raw file bytes), re-root the oldest kept.
    Non-conversation records (already thin post-debloat) are kept as-is and
    counted toward the budget. Returns (recs, dropped-turn-count)."""
    fixed = sum(rec_bytes(r) for r in recs
                if r.get("type") not in CONVERSATION_TYPES)
    conv_idx = [i for i, r in enumerate(recs)
                if r.get("type") in CONVERSATION_TYPES]
    total = fixed + sum(rec_bytes(recs[i]) for i in conv_idx)
    if total <= target_bytes:
        return recs, 0
    # Walk newest->oldest, accumulate FILE bytes until budget — but never stop
    # before the window contains a `user` turn (a session must start user-first
    # and be non-empty; correctness overrides the soft byte target).
    keep = set()
    acc = fixed
    has_user = False
    for i in reversed(conv_idx):
        b = rec_bytes(recs[i])
        if acc + b > target_bytes and keep and has_user:
            break
        acc += b
        keep.add(i)
        if recs[i].get("type") == "user":
            has_user = True
    # Snap the window start to a USER turn: the API requires a conversation to
    # begin user-first, so drop any leading assistant/system records in the window.
    #
    # But a tool_result IS a user turn, and the API also rejects a tool_result
    # whose tool_use is not present. Cutting between an assistant's tool_use and
    # the user's matching tool_result satisfies "begins user-first" while leaving
    # an ORPHAN, which fails on resume. Measured 2026-09-07 on claude-5's
    # session 4ea2e78a: the compacted transcript began user/[tool_result] with
    # 220 tool_use ids against 221 tool_result refs -- exactly one orphan, at
    # the re-root.
    #
    # So the window must start at a user turn that carries no unmatched
    # tool_result. Anything earlier is a turn the API will not accept.
    kept_tool_use_ids = set()
    for i in sorted(keep):
        for b in _blocks(recs[i]):
            if b.get("type") == "tool_use" and b.get("id"):
                kept_tool_use_ids.add(b["id"])

    def _orphan_free_user(i):
        r = recs[i]
        if r.get("type") != "user":
            return False
        for b in _blocks(r):
            if b.get("type") == "tool_result":
                ref = b.get("tool_use_id")
                if ref is not None and ref not in kept_tool_use_ids:
                    return False
        return True

    first_user = None
    for i in sorted(keep):
        if _orphan_free_user(i):
            first_user = i
            break
    if first_user is None:
        # Every candidate start is orphaned. Keeping a transcript the API will
        # refuse is worse than keeping less, so fall back to the plain
        # user-first rule and drop the offending tool_result blocks below.
        for i in sorted(keep):
            if recs[i].get("type") == "user":
                first_user = i
                break
    keep = {i for i in keep if first_user is not None and i >= first_user}
    dropped = len(conv_idx) - len(keep)
    out = [r for i, r in enumerate(recs)
           if (r.get("type") not in CONVERSATION_TYPES) or (i in keep)]
    # Belt and braces: strip any tool_result whose tool_use did not survive.
    # The window rule above should prevent this, but an orphan anywhere in the
    # transcript -- not just at the root -- is refused, and a dropped block
    # costs one tool echo where a refused transcript costs the whole session.
    surviving_uses = set()
    for r in out:
        for b in _blocks(r):
            if b.get("type") == "tool_use" and b.get("id"):
                surviving_uses.add(b["id"])
    for r in out:
        msg = r.get("message")
        if not isinstance(msg, dict):
            continue
        content = msg.get("content")
        if not isinstance(content, list):
            continue
        pruned = [b for b in content
                  if not (isinstance(b, dict)
                          and b.get("type") == "tool_result"
                          and b.get("tool_use_id") is not None
                          and b.get("tool_use_id") not in surviving_uses)]
        if len(pruned) != len(content):
            # Never leave an EMPTY content list; the API rejects that too.
            msg["content"] = pruned if pruned else [
                {"type": "text", "text": "[compacted: tool result dropped]"}]

    # re-root: the first surviving conversation record (now a user turn) starts the chain
    for r in out:
        if r.get("type") in CONVERSATION_TYPES:
            r["parentUuid"] = None
            break
    return out, dropped


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("session_id")
    ap.add_argument("--target-mib", type=float, default=1.7,
                    help="target FILE size (MiB) for the truncate layer; headroom "
                         "under the 2 MiB joey-max (default 1.7)")
    ap.add_argument("--dry-run", action="store_true")
    ap.add_argument("--no-truncate", action="store_true",
                    help="lossless de-bloat only; never drop conversation turns")
    args = ap.parse_args()

    path = find_transcript(args.session_id)
    if not path:
        print(f"[compact] no transcript for session {args.session_id}", file=sys.stderr)
        return 2
    before = os.path.getsize(path)
    recs = []
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            try:
                recs.append(json.loads(line))
            except json.JSONDecodeError:
                pass

    n0 = len(recs)
    recs = debloat(recs)
    n1 = len(recs)
    dropped_turns = 0
    target = int(args.target_mib * 1024 * 1024)
    if not args.no_truncate:
        recs, dropped_turns = truncate(recs, target)

    body = "".join(json.dumps(r, ensure_ascii=False) + "\n" for r in recs)
    after = len(body.encode("utf-8"))
    pct = 100 * after / before if before else 0
    print(f"[compact] {args.session_id}: {before/1048576:.2f}MB -> {after/1048576:.2f}MB "
          f"({pct:.0f}%)  records {n0}->{n1}->{len(recs)}  turns-dropped {dropped_turns}")
    if args.dry_run:
        return 0
    backup = f"{path}.pre-compact-{int(time.time())}"
    shutil.copy2(path, backup)
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(body)
    print(f"[compact] wrote {path} (backup: {os.path.basename(backup)})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
