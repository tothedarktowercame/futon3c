#!/usr/bin/env python3
"""Dispatch a durée run through the SAME path the cron uses.

WHY THIS EXISTS (2026-07-30). Durée runs 1 was hand-dispatched with
`agency_send.py`, which BYPASSES `scripts/dispatch_with_recall.clj`. That meant
no dispatch-time recall ran and no offered half was recorded, so every durée
bellback said "no dispatch-time memory IDs were supplied" — which reads as
"recall found nothing" but actually meant "recall never happened". Since the
whole point of per-run scribe + per-pass promotion is that run N+1 can consume
run N's memories, hand-dispatching silently voided the experiment it was meant
to run.

It also re-uses the cron's env fix: `dispatch_with_recall.clj` falls back to the
RETIRED :7071 store when FUTON_SUBSTRATE_URL is unset, and safe-recall records
that failure as an ordinary empty result — indistinguishable from a real one.

Usage:
  duree_dispatch.py [--row <row-id>] [--to <runner>] [--dry-run]
Default row is the highest-priority untouched row; default runner codex-7.
"""
from __future__ import annotations

import argparse
import importlib.util
import json
import os
import re
import subprocess
import sys
import urllib.request
from datetime import datetime, timezone
from pathlib import Path

FUTON3C = Path(__file__).resolve().parent.parent
sys.path.insert(0, str(FUTON3C / "scripts"))

spec = importlib.util.spec_from_file_location("cron", FUTON3C / "scripts/codex_sorry_cron.py")
cron = importlib.util.module_from_spec(spec)
spec.loader.exec_module(cron)
from edn_format import loads, Keyword as K  # noqa: E402
from refresh_statement_hints import iter_rows  # noqa: E402

QUEUE = FUTON3C / "data/codex-sorry-queue.edn"
PREAMBLE = """DURÉE RUN — hand-dispatched by claude-9 (ground control), not by the cron.

Joe asked for hand-dispatched, hand-quality-checked runs so we can confirm the
loop behaves the way the packet says it should. Two things follow.

FIRST: you have the FULL budget. Do not truncate the search to save time.

SECOND: your report is read as EVIDENCE ABOUT BEHAVIOUR, not only about the
proof, so report your search concretely enough to be audited — the actual
queries you ran and what came back, not "searched". An honest "searched, found
nothing relevant, here is what I tried" is a GOOD outcome and is recorded as
such. A vague claim of having searched is not, and it will be checked.

Blocked is an acceptable result. A false statement found and refuted is a
valuable result. Do not weaken the target to manufacture a pass.

--- standard packet follows ---

"""


def zai_busy() -> set[str]:
    """Replicate the cron's zai-busy guard: both lanes edit the same files."""
    try:
        with urllib.request.urlopen("http://localhost:7070/api/alpha/agents", timeout=20) as r:
            roster = json.load(r)
    except Exception as exc:  # noqa: BLE001
        print(f"WARNING: roster unreachable ({exc}); cannot check zai-busy", file=sys.stderr)
        return set()
    return cron.zai_busy_problem_ids(roster)


def main() -> int:
    ap = argparse.ArgumentParser()
    ap.add_argument("--row")
    ap.add_argument("--to", default="codex-7")
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    # STOP CONDITIONS for the continuous loop (Joe, 2026-07-30): run until the
    # queue is exhausted or Codex usage reaches 50%. Both are checked HERE so a
    # continuous loop cannot overrun them by forgetting; the usage gate reuses
    # the cron's own reader and threshold rather than a second implementation.
    try:
        snap = cron.newest_rate_limit()
        cron.enforce_usage(snap)
        print(f"usage gate OPEN: used={snap['used_percent']:g}% "
              f"(stops at >={cron.MAX_USED_PERCENT:g}%)")
    except Exception as exc:  # GateClosed or unavailable signal
        print(f"STOP: {exc}", file=sys.stderr)
        return 10

    rows = loads(QUEUE.read_text(encoding="utf-8"))
    untouched = [r for r in rows if cron.status_name(r) == "untouched"]
    if not untouched:
        print("STOP: queue exhausted - no untouched rows remain", file=sys.stderr)
        return 11
    print(f"queue: {len(untouched)} untouched rows remain")
    busy = zai_busy()
    if args.row:
        cands = [r for r in rows if str(r.get(K("id"))) == args.row]
        if not cands:
            print(f"no such row: {args.row}", file=sys.stderr)
            return 2
        row = cands[0]
    else:
        _, row = cron.choose_row(rows, busy)

    rid = str(row.get(K("id")))
    pid = cron.row_problem_id(row)
    if pid and pid in busy:
        print(f"REFUSING: zai lane is invoking on {pid}; both lanes edit the same file", file=sys.stderr)
        return 3
    if cron.status_name(row) != "untouched":
        print(f"REFUSING: row status is {cron.status_name(row)}, not untouched", file=sys.stderr)
        return 4

    template = (FUTON3C / "data/codex-sorry-packet-template.txt").read_text(encoding="utf-8")
    packet = PREAMBLE + cron.instantiate_packet(row, template)
    assert "@@" not in packet, "unreplaced marker in packet"

    command = [
        "clojure", "-M", "scripts/dispatch_with_recall.clj",
        "--problem", rid, "--to", args.to, "--from", "claude-9",
        "--mission", "M-codex-sorry-loop",
    ]
    for s in cron.subjects_for(row):
        command.extend(["--subject", s])

    if args.dry_run:
        print("DRY RUN")
        print("  row:     ", rid)
        print("  runner:  ", args.to)
        print("  subjects:", " ".join(cron.subjects_for(row)))
        print("  packet:  ", len(packet), "chars")
        print("  command: ", " ".join(command[:8]), "...")
        return 0

    env = dict(os.environ)
    env.setdefault("FUTON_SUBSTRATE_URL", cron.SUBSTRATE_URL)
    env.setdefault("FUTON1A_URL", cron.SUBSTRATE_URL)
    res = subprocess.run(command, cwd=FUTON3C, input=packet, text=True,
                         capture_output=True, env=env, timeout=300)
    print(res.stdout.strip())
    if res.stderr.strip():
        print("stderr:", res.stderr.strip()[:600], file=sys.stderr)
    if res.returncode != 0:
        return res.returncode

    job = None
    m = re.search(r"invoke-\d+-\d+-[0-9a-f]+", res.stdout)
    if m:
        job = m.group(0)

    # Claim the row so nothing else takes it and the state is auditable.
    text = QUEUE.read_text(encoding="utf-8")          # fresh read before write
    hits = [(lo, hi) for lo, hi in iter_rows(text)
            if f'"{rid}"' in text[lo:hi] and ":status :untouched" in text[lo:hi]]
    if len(hits) == 1:
        lo, hi = hits[0]
        stamp = datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
        claim = (f':status :dispatched :dispatched-at "{stamp}" :dispatch-mode :duree '
                 f':dispatched-by "claude-9" :runner "{args.to}"'
                 + (f' :job-id "{job}"' if job else ""))
        QUEUE.write_text(text[:lo] + text[lo:hi].replace(":status :untouched", claim, 1) + text[hi:],
                         encoding="utf-8")
        print(f"row claimed: {rid} -> :dispatched")
    else:
        print(f"WARNING: could not claim row ({len(hits)} matches) — claim it by hand", file=sys.stderr)

    if job:
        print()
        print("OPERATOR VISIBILITY — poll these directly:")
        print(f"  curl -s localhost:7070/api/alpha/invoke/jobs/{job} | python3 -m json.tool")
        print("  curl -s localhost:7070/api/alpha/parked | python3 -m json.tool")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
