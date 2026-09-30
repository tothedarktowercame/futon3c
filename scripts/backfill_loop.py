#!/usr/bin/env python3
"""Run the 象 backfill on several seats for a fixed time, one pack of ten per job.

Joe (2026-09-30): run a big chunk on Kimi and/or Codex, not Sonnet, for an
hour, then check quality.  Each seat loops: claim ten unread turns from the
first block that has any, build a pack (backfill_pack.py), dispatch it, wait
for the job, publish the answer (backfill_pack.py publish, the validator
decides), log one line.  No new pack is dispatched after --minutes; jobs in
flight are waited for.  A turn is tried at most twice.

  backfill_loop.py --seats kimi-3,codex-9 --minutes 60 BLOCK_DIR [BLOCK_DIR ...]

Log: /home/joe/code/storage/operator-turns/backfill-loop.jsonl (one line per pack).
Stop early: touch /tmp/claude17/backfill-loop.STOP
"""
import argparse, datetime, glob, json, os, re, subprocess, sys, threading, time, urllib.request

HERE = os.path.dirname(os.path.abspath(__file__))
WORK = "/tmp/claude17/loop"
LOG = "/home/joe/code/storage/operator-turns/backfill-loop.jsonl"
STOP = "/tmp/claude17/backfill-loop.STOP"
API = "http://localhost:7070/api/alpha"
lock = threading.Lock()
tries = {}      # turn path -> attempts
claimed = set()

COVER = """From claude-17, for Joe: annotate {n} of Joe's historical turns (the 象 backfill) from ONE prepared file, with no searches.

The file {pack} holds the instructions, the closed intent list, the ten turns with session context, pattern search hits for every sentence, and the text of every pattern hit in an appendix. Read it and follow it, including its rules on rejections, display cues (at most half of a sentence's words), candidates for every uncited fragment, and quoting text instead of giving offsets.

Write as you go. After you finish each turn, rewrite {answer} as a JSON array of every turn finished so far (each element with a "turn_id" key), and check it parses:
  python3 -c "import json;print(len(json.load(open('{answer}'))))"
Keep your reasoning short and put the work in the file. Do not publish anything, do not commit, and do not write anywhere else.

Do all {n}, one at a time. If a turn truly cannot be read from the pack, leave it out and say why.

Reply (delivered automatically): the answer path, how many turns it contains, and any turn left out with the reason.
"""


def now():
    return datetime.datetime.now(datetime.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")


def log(row):
    with lock, open(LOG, "a") as fh:
        fh.write(json.dumps(row, ensure_ascii=False) + "\n")


def claim(blocks, n=10):
    """Up to n unread, unclaimed turns from the first block that has any."""
    with lock:
        for block in blocks:
            rows = []
            for f in glob.glob(os.path.join(block, "*.json")):
                if f.count(".json") != 1 or f.endswith("MANIFEST.json"):
                    continue
                if os.path.exists(f + ".analysis.json") or f in claimed or tries.get(f, 0) >= 2:
                    continue
                created = json.load(open(f)).get("created_at")
                if created:
                    rows.append((created, f))
            rows.sort()
            if rows:
                picked = [f for _, f in rows[:n]]
                claimed.update(picked)
                for f in picked:
                    tries[f] = tries.get(f, 0) + 1
                return block, [os.path.basename(f)[:-5] for f in picked]
    return None, []


def release(block, ids):
    with lock:
        for t in ids:
            claimed.discard(os.path.join(block, t + ".json"))


def get(path):
    with urllib.request.urlopen(API + path, timeout=30) as r:
        return json.load(r)


def post(path, body=None):
    req = urllib.request.Request(API + path, data=json.dumps(body or {}).encode(),
                                 headers={"Content-Type": "application/json"}, method="POST")
    with urllib.request.urlopen(req, timeout=30) as r:
        return json.load(r)


def dispatch(seat, cover_path, purpose):
    if seat.startswith("kimi"):
        out = subprocess.run(["bash", os.path.join(HERE, "kimi-task.sh"), "--from", "claude-17",
                              "--to", seat, "--purpose", purpose, cover_path],
                             capture_output=True, text=True).stdout
        m = re.search(r"task=(\S+) .*job=(\S+)", out)
        return (m.group(2), m.group(1)) if m else (None, out[-300:])
    try:
        post(f"/agents/{seat}/reset-session")   # a fresh conversation per pack
    except Exception:
        pass
    out = subprocess.run([sys.executable, os.path.join(HERE, "agency_send.py"), "--from", "claude-17",
                          "--to", seat, "--kind", "bell", "--mode", "work"],
                         stdin=open(cover_path), capture_output=True, text=True).stdout
    m = re.search(r'"job-id":"([^"]+)"', out)
    return (m.group(1), None) if m else (None, out[-300:])


def wait(job, limit_s=45 * 60):
    t0 = time.time()
    while time.time() - t0 < limit_s:
        try:
            j = get(f"/invoke/jobs/{job}")["job"]
            if j["state"] in ("done", "failed", "cancelled"):
                return j
        except Exception:
            pass
        time.sleep(20)
    return {"state": "timeout"}


def seat_loop(seat, blocks, end_at):
    n = 0
    while time.time() < end_at and not os.path.exists(STOP):
        block, ids = claim(blocks)
        if not ids:
            return
        n += 1
        tag = f"{seat}-{int(time.time())}"
        pack, answer, cover = (f"{WORK}/{tag}.pack.md", f"{WORK}/{tag}.answer.json",
                               f"{WORK}/{tag}.cover.md")
        subprocess.run([sys.executable, os.path.join(HERE, "backfill_pack.py"), "pack", block, *ids,
                        "--out", pack], capture_output=True, text=True)
        open(cover, "w").write(COVER.format(pack=pack, answer=answer, n=len(ids)))
        started = time.time()
        job, req = dispatch(seat, cover, f"象 backfill pack {tag} ({len(ids)} turns, {os.path.basename(block)})")
        row = {"at": now(), "seat": seat, "block": os.path.basename(block), "turns": ids, "job": job}
        if not job:
            row.update({"error": "dispatch failed", "detail": req})
            log(row); release(block, ids); time.sleep(60); continue
        j = wait(job)
        row.update({"state": j.get("state"), "minutes": round((time.time() - started) / 60, 1)})
        if req:
            subprocess.run(["bash", os.path.join(HERE, "kimi-task.sh"), "--complete", req, job],
                           capture_output=True, text=True)
        if os.path.exists(answer):
            p = subprocess.run([sys.executable, os.path.join(HERE, "backfill_pack.py"), "publish",
                                block, answer, *ids], capture_output=True, text=True)
            try:
                s = json.loads(p.stdout)
                row.update({"published": s["published"], "refused": s["refused"],
                            "reasons": [(t["turn"], t.get("reason", "")[:160])
                                        for t in s["turns"] if not t["published"]]})
            except ValueError:
                row.update({"error": "publish output", "detail": (p.stdout + p.stderr)[-300:]})
        else:
            row.update({"error": "no answer file"})
        log(row)
        release(block, ids)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--seats", required=True)
    ap.add_argument("--minutes", type=float, default=60)
    ap.add_argument("blocks", nargs="+")
    a = ap.parse_args()
    os.makedirs(WORK, exist_ok=True)
    end_at = time.time() + a.minutes * 60
    log({"at": now(), "start": True, "seats": a.seats, "minutes": a.minutes, "blocks": a.blocks})
    threads = [threading.Thread(target=seat_loop, args=(s, a.blocks, end_at))
               for s in a.seats.split(",")]
    for i, t in enumerate(threads):
        t.start(); time.sleep(5)
    for t in threads:
        t.join()
    log({"at": now(), "end": True})


if __name__ == "__main__":
    main()
