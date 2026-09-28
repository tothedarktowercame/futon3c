#!/usr/bin/env python3
"""Run explicitly queued warrant registrations without touching Agency."""
import argparse
import concurrent.futures
import datetime
import os
from pathlib import Path
import sqlite3
import subprocess
import sys
import time

sys.path.insert(0, str(Path(__file__).resolve().parent))
import warrant_index

DEFAULT_DB = "/home/joe/code/storage/test-registry/warrant-index.sqlite"
DEFAULT_RUNNER = "/home/joe/code/futon2/scripts/wm/register-warrant.sh"
REPOS = {"futon2": "/home/joe/code/futon2", "futon3c": "/home/joe/code/futon3c"}
DDL = """
CREATE TABLE IF NOT EXISTS warrant_rerun_requests (
 request_id INTEGER PRIMARY KEY AUTOINCREMENT, namespace TEXT NOT NULL,
 repo TEXT NOT NULL, reason TEXT NOT NULL CHECK(reason IN ('stale','absent')),
 requested_at TEXT NOT NULL, state TEXT NOT NULL CHECK(state IN ('queued','running','done','failed')),
 entry_id TEXT, detail TEXT, finished_at TEXT);
CREATE UNIQUE INDEX IF NOT EXISTS warrant_rerun_one_active_namespace
 ON warrant_rerun_requests(namespace) WHERE state IN ('queued','running');
"""

def connect(path):
    db = sqlite3.connect(path, timeout=30)
    db.executescript(DDL)
    return db

def clean_git_env():
    env = os.environ.copy()
    for key in ("GIT_DIR", "GIT_WORK_TREE", "GIT_INDEX_FILE", "GIT_COMMON_DIR"): env.pop(key, None)
    return env

def latest(db, namespace):
    return db.execute("""SELECT r.entry_id,r.repo_root,r.warrant,r.revision,e.payload_text,
      r.ran_order FROM registry_runs r JOIN registry_entries e ON e.id=r.entry_id
      WHERE r.namespace=? ORDER BY r.ran_order DESC,r.finished_order DESC,r.entry_id DESC LIMIT 1""",
                      (namespace,)).fetchone()

def claim(db, limit, excluded=()):
    db.execute("BEGIN IMMEDIATE")
    sql = "SELECT request_id FROM warrant_rerun_requests WHERE state='queued'"
    args = []
    if excluded:
        sql += " AND request_id NOT IN (%s)" % ",".join("?" * len(excluded)); args += list(excluded)
    ids = [r[0] for r in db.execute(sql + " ORDER BY request_id LIMIT ?", (*args, limit))]
    if ids:
        db.execute("UPDATE warrant_rerun_requests SET state='running',detail=NULL WHERE request_id IN (%s)"
                   % ",".join("?" * len(ids)), ids)
    db.commit()
    return ids

def test_file(namespace):
    return "test/" + namespace.replace("-", "_").replace(".", "/") + ".clj"

def dirty_path(root, previous, namespace):
    if previous:
        payload = warrant_index.edn(previous[4])
        paths = list(warrant_index.recorded_files(
            {"repo-root": previous[1]}, payload, root).keys())
        paths = [os.path.relpath(p, root) for p in paths]
    else:
        paths = [test_file(namespace)]
    if not paths:
        return None
    result = subprocess.run(["git", "status", "--porcelain", "--", *paths], cwd=root,
                            env=clean_git_env(), text=True, capture_output=True, check=True)
    return result.stdout.splitlines()[0][3:] if result.stdout else None

def finish(db_path, request_id, state, entry=None, detail=None):
    with connect(db_path) as db:
        db.execute("UPDATE warrant_rerun_requests SET state=?,entry_id=?,detail=?,finished_at=? WHERE request_id=?",
                   (state, entry, detail, datetime.datetime.now(datetime.timezone.utc).isoformat(), request_id))

def process_one(db_path, request_id, runner, log_dir):
    with connect(db_path) as db:
        row = db.execute("SELECT namespace,repo,requested_at FROM warrant_rerun_requests WHERE request_id=?",
                         (request_id,)).fetchone()
        namespace, repo, requested = row
        root = REPOS.get(repo)
        if not root:
            finish(db_path, request_id, "failed", detail="unknown repo: " + repo); return
        previous = latest(db, namespace)
    head = subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root,
                                   env=clean_git_env(), text=True).strip()
    dirty = dirty_path(root, previous, namespace)
    if dirty:
        with connect(db_path) as db:
            db.execute("UPDATE warrant_rerun_requests SET state='queued',detail=? WHERE request_id=?",
                       ("held: dirty " + dirty, request_id))
        return
    if subprocess.check_output(["git", "rev-parse", "HEAD"], cwd=root,
                               env=clean_git_env(), text=True).strip() != head:
        with connect(db_path) as db:
            db.execute("UPDATE warrant_rerun_requests SET state='queued',detail=? WHERE request_id=?",
                       ("held: HEAD moved", request_id))
        return
    log = Path(log_dir) / f"{request_id}-{namespace}.log"; log.parent.mkdir(parents=True, exist_ok=True)
    env = clean_git_env()
    env.update(AUTHOR="warrant-rerun-worker", REGISTRY_DB=db_path,
               WARRANT_WORKTREE_SUFFIX=f"rerun-{request_id}")
    try:
        with log.open("wb") as out:
            code = subprocess.run([runner, "--pinned", head, namespace], cwd=root, env=env,
                                  stdout=out, stderr=subprocess.STDOUT, timeout=900).returncode
    except subprocess.TimeoutExpired:
        code = 124
    with connect(db_path) as db: after = latest(db, namespace)
    entry = after[0] if after and after[5] > requested else None
    if entry and after[2] and after[3] == head:
        finish(db_path, request_id, "done", entry=entry)
    else:
        finish(db_path, request_id, "failed", entry=entry, detail=f"runner exit {code}")

def run_pass(args):
    seen = set()
    while True:
        with connect(args.db) as db: ids = claim(db, args.parallel, seen)
        if not ids: return
        seen.update(ids)
        with concurrent.futures.ThreadPoolExecutor(max_workers=args.parallel) as pool:
            list(pool.map(lambda i: process_one(args.db, i, args.runner, args.log_dir), ids))

def main(argv=None):
    p = argparse.ArgumentParser(); p.add_argument("--db", default=DEFAULT_DB)
    p.add_argument("--parallel", type=int, default=1); p.add_argument("--runner", default=DEFAULT_RUNNER)
    p.add_argument("--log-dir", default="/tmp/warrant-rerun-logs")
    mode = p.add_mutually_exclusive_group(required=True); mode.add_argument("--once", action="store_true")
    mode.add_argument("--watch", type=float)
    args = p.parse_args(argv)
    while True:
        run_pass(args)
        if args.once: return 0
        time.sleep(args.watch)

if __name__ == "__main__": raise SystemExit(main())
